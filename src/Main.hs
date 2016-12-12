{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- System imports
import           Data.Monoid   ((<>))
import           Data.List (groupBy, intercalate, sortBy)
import           Data.Function (on)
import           System.FilePath (takeBaseName, takeFileName)

-- Hakyll imports
import           Hakyll

-- Our own imports
import Config
import Compiler.Org
import Compiler.Javascript
import Template.Functions (json, striptags)

-- Stuff for the testing area
import Control.Applicative (Alternative (..))
import Data.Time.Format (parseTimeM, defaultTimeLocale)
-- older TimeLocale import?
--import System.Locale (defaultTimeLocale)
import Data.Time.Clock (UTCTime)
import Control.Monad (liftM, filterM)

-- Patterns which match blog postings
-- TODO: read source patterns from a file
--
postsPattern :: Pattern
postsPattern =
  ("sites/main/_posts/*.org"
  .&&. complement "sites/main/_posts/_*.org"
  .&&. complement "sites/main/_posts/testpandoc.org"
  .&&. complement "sites/main/_posts/containertest.org")
  .||. "sites/cobra/_posts/*.org"
  .||. "sites/photo/_posts/*.org"

-- A few ad-hoc orgmode documents which should be published as pages
orgPages :: Pattern
orgPages =
  "about/*.org"
  .||. "error/*.org"
  .||. "emacs/config.org"
  
-- Main entry point
main :: IO ()
main = hakyllWith config $ do
    templateR  -- Make sure that our templates are compiled
    staticR    -- Copy static files
    aboutR     -- About pages
    postR      -- Process posts
    feedR      -- Process atom feed
    homeR      -- Homepage
    tagsR      -- List of posts tagged with certain tags
    archiveR   -- Grouped list of all posts

    match "sitemap.xml"    plainApply  -- Generate sitemap.xml
    match "feed/full.json" plainApply  -- Generate a json database which we can query client side
    
    searchR    -- Search functionality
------------------------------------------------------------------------------------

-- Copy after applying template substition, not much more
-- Typically used for non-human json/xml etc.
plainApply :: Rules ()
plainApply = do
  route idRoute
  compile $ do
    posts <- recentFirst =<< loadAllSnapshots (postsPattern .&&. hasVersion "html") "html"
    
    let ctx =
          listField "posts" postCtx (return posts) <>
          baseContext
    getResourceBody >>= applyAsTemplate ctx

-- Search results (javascript based)
searchR :: Rules ()
searchR =
  match "search/index.html" $ do
    route idRoute

    compile $ 
      getResourceBody
        >>= applyAsTemplate baseContext
        >>= loadAndApplyTemplate "_layouts/page.html" baseContext
        >>= loadAndApplyTemplate "_layouts/default.html" baseContext
        >>= relativizeUrls

-- Grouped view of tag filter post
-- FIXME: this generates contentless pages, why?
-- 
tagsR :: Rules ()
tagsR = do
  tags <- buildTags postsPattern (fromCapture "tag/*/index.html")

  -- Use the tagsRules' function to generate the pages
  -- FIXME: I don't think I understand 'pattern'  here, I could only get it to work
  --        by filtering on the tag.
  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
      
    route idRoute
    compile $ groupedPostList title (tagFilter tag) postsPattern

-- Archives are the same as the tag page, just more postings and
-- a different title
archiveR :: Rules ()
archiveR =
  create ["archive/index.html"] $ do
    let title = "Archives"
    route idRoute

    -- recentFirst is already in groupedPostList
    -- HOWTO: combine filter here?
    compile $ groupedPostList title recentFirst postsPattern


-- Homepage rules
-- TODO: take the '5' to the config
homeR :: Rules ()
homeR = 
 match "index.html" $ do
   route idRoute

   compile $ do
     posts <- fmap (take 5) . recentFirst
             =<< loadAllSnapshots (postsPattern .&&. hasVersion "html") "html"
     let indexCtx =
           listField  "posts"      postCtx (return posts) <>
           constField "title"      "Home" <>
           baseContext 
           
     getResourceBody
       >>= applyAsTemplate indexCtx
       >>= loadAndApplyTemplate "_layouts/default.html" indexCtx
       >>= relativizeUrls


-- Template rules
templateR :: Rules ()
templateR = do
  -- Templates should just be compiled; we have them in 2 locations
  match "_layouts/*"  $ compile templateCompiler
  match "_includes/*" $ compile templateCompiler


-- Static files which can just be copied
staticR :: Rules ()
staticR = do
  -- Single files that need to be copied
  mapM_ static ["robots.txt", "favicon.ico"]
  
  -- Whole directories that need to be copied
  -- TODO: move this to a config file
  mapM_ (dir static) ["assets/img/gpx",                         -- | Generic images
                      "assets/img",
                      "assets/css/images", "assets/js/images", -- | Images referred to from js or css
                      "assets/fonts",                          -- | Font resources
                      "files",".well-known","tests"]           -- | Legacy files and tests (transient I hope)

  -- Compress js files
  match "assets/js/*.js" $ do
    route $ setExtension "min.js"
    compile compressJsCompiler

  -- Compress css files
  match "assets/css/*.css" $ do
    route $ setExtension "min.css"
    compile compressCssCompiler
    
  where
    -- Make it easier to copy loads of static stuff
    static :: Pattern -> Rules ()
    static f = match f $ do
      route idRoute
      compile copyFileCompiler

    -- Treat whole directories
    dir :: (Pattern -> Rules a) -> String -> Rules a
    dir act f = act $ fromGlob $ f ++ "/**"

  
  
-- About page rules
aboutR :: Rules ()
aboutR = do
  -- Generate resolved source format
  match orgPages $ version "source" $ do
    route idRoute

    compile $ do
      let ctx = 
            constField "testing" "testing" <>
            --constField "site.posts.size" (getpostCount postsPattern) <>
            baseContext
      getResourceString
        >>= applyAsTemplate ctx
        >>= saveSnapshot "source"

  -- Generate html rendering based on source
  match orgPages $ version "html" $ do
    route $ setExtension "html"
   
    -- Compile with the src item above
    compile $ orgCompilerWith (itemFromVersion "source")
        >>= loadAndApplyTemplate "_layouts/page.html"    baseContext
        >>= loadAndApplyTemplate "_layouts/default.html" baseContext
        >>= relativizeUrls


--groupedPostList :: String -> ([Item a] -> Compiler [Item String]) -> Pattern -> Compiler (Item String)
groupedPostList title customFilter pattern = do
  posts <- fmap groupByYear $
    recentFirst
    =<< customFilter
    =<< loadAllSnapshots (pattern .&&. hasVersion "html") "html"
  let ctx =
        constField "title" title <>
        listField "years"
        (
          field "year" (return . fst . itemBody) <>
          listFieldWith "posts" postCtx (return . snd . itemBody)
        )
        (sequence $ fmap (\(y, is) -> makeItem (show y, is)) posts) <>
        baseContext
  makeItem ""
    >>= loadAndApplyTemplate "_layouts/grouped.html" ctx
    >>= loadAndApplyTemplate "_layouts/default.html" ctx
    >>= relativizeUrls

-- Shorthand to pass filters to grouped list
tagFilter :: MonadMetadata m => String -> [Item a] -> m [Item a]
tagFilter tag = filterM (fmap (elem tag) . getTags . itemIdentifier)


-- Get a previously compiled item from a version
itemFromVersion :: String -> Compiler (Item String)
itemFromVersion v = do
  id' <- setVersion (Just v) `fmap` getUnderlying
  -- Q: can we use a snapshot here? 
  body <- loadBody id'  
  makeItem body
  
-- Post Rules
-- ✓ Take file in orgmode format
-- ✓ Figure out the publish date
-- ✓ Figure out the title
-- ✓ copy to yyyy/mm/dd/title.html
-- ✓ Process tags properly
--   Skip 'published: false' posts
--   Previous/Next links?
postR :: Rules ()
postR = do
  -- Resolve variables and save as 'source' version
  match postsPattern $ version "source" $ do
    route $ postsRoute "org"
    compile $ getResourceString
      >>= applyAsTemplate postCtx
      >>= saveSnapshot "source"

  -- Generate 'html' version based on 'source' version
  match postsPattern $ version "html" $ do
    route $ postsRoute "html"

    -- Compile posts with the orgmode compiler, this generates the
    -- metadata from the header too
    compile $ orgCompilerWith (itemFromVersion "source")
      >>= loadAndApplyTemplate "_layouts/post.html"    postCtx
      >>= saveSnapshot "html"
      >>= loadAndApplyTemplate "_layouts/nav_comments.html" postCtx
      >>= loadAndApplyTemplate "_layouts/default.html" postCtx
      >>= relativizeUrls

-- Route should be: /yyyy/mm/dd/filename-without-date.html
postsRoute :: String -> Routes
postsRoute ext =
  setExtension ext `composeRoutes`
  dateFolders `composeRoutes`
  gsubRoute "sites/main/_posts/"  (const "") `composeRoutes`
  gsubRoute "sites/photo/_posts/" (const "") `composeRoutes`
  gsubRoute "sites/cobra/_posts/" (const "")
  where
    dateFolders :: Routes
    dateFolders =
      gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ replaceAll "-" (const "/")
      
postCtx :: Context String
postCtx =
    -- Construct the date components:
    -- 1. If there is a 'published' metadata, use that
    -- 2. If a date can be constructed from the filename, do that
    -- otherwise error out, because for a post, we need a date
    dateField "year" "%Y" <>
    dateField "month" "%b" <>
    dateField "day" "%d" <>
    dateField "date" "%Y-%m-%d" <>
    dateField "pubdate" "%Y-%m-%dT%X%z" <>

    listFieldWith "taglist" tagCtx getTagItems <>

    -- Our templates use reverse terminolog
    field "prevPost" (getPostUrl itemAfter) <>
    field "nextPost" (getPostUrl itemBefore) <>

    baseContext
  where
      -- Construct the 'tag' inside the list field (do we need url here too?)
      tagCtx :: Context String
      tagCtx = field "tag" $ \tagItem -> return (itemBody tagItem) 

      -- Generate the tags for this item
      getTagItems :: Item String -> Compiler [Item String]
      getTagItems postItem = do
        tags <- getTags (itemIdentifier postItem)
        mapM makeItem tags
        
-- Feed Rules
-- FIXME: My own feed templates were nicer, because they could
-- TODO: bring into own file and import here
-- TODO: bring '20' into the config
-- render better instead of dumping raw xml on a user with a browser.
feedR :: Rules ()
feedR =
  create ["feed/atom.xml"] $ do
    route idRoute
    compile $ do
      posts <- fmap (take 20) . recentFirst
              =<< loadAllSnapshots (postsPattern .&&. hasVersion "html") "html"
      renderAtom feedConfiguration feedCtx posts
   where
     feedCtx :: Context String
     feedCtx =
       bodyField "description" <>
       baseContext 

-- Jekyll variables, probably can go after a bit.
baseContext :: Context String
baseContext =
  constField "site.name"   sitename <>
  constField "site.url"    siteurl <>
  constField "site.author" author <>
  constField "year"        copyrightYear <>

  -- Expose some functions to the templates
  functionField "json"      json      <>
  functionField "striptags" striptags <>


  -- $body$, $url$, $path$ and all $foo$ from metadata are delivered
  -- by the default context
  defaultContext
  
-- Group by year, depending on filename, so FIXME later to use
-- metadata for this function
groupByYear :: [Item a] -> [(Int, [Item a])]
groupByYear = map (\pg -> ( year (head pg), pg) ) .  groupBy ((==) `on` year)
  where year :: Item a -> Int
        year = read . take 4 . takeBaseName . toFilePath . itemIdentifier

-- For later inspection
-- This give the latest commit for a file
commitField :: Context String 
commitField = field "commit" $ \item -> do 
  let fp = toFilePath $ itemIdentifier item 
  unixFilter "git" ["log", "-1", "--oneline", fp] "" 


--- TESTING TESTING ----
-- Get the number of posts that satisfy a pattern
postCount :: MonadMetadata m => Pattern -> m Int
postCount p =  length <$> getMatches p



-- Helpers for previous and next url
getPostUrl :: ([Identifier] -> Identifier -> Maybe Identifier) -> Item String -> Compiler String
getPostUrl beforeOrAfter post = do
    posts <- getMatches (postsPattern .&&. hasVersion "html")
    let ident = itemIdentifier post
        sortedPosts = sortIdentifiersByDate posts
        ident' = beforeOrAfter sortedPosts ident
    case ident' of
        Just i -> (fmap (maybe empty toUrl) . getRoute) i
        Nothing -> empty

itemAfter :: Eq a => [a] -> a -> Maybe a
itemAfter xs x =
    lookup x $ zip xs (tail xs)

itemBefore :: Eq a => [a] -> a -> Maybe a
itemBefore xs x =
    lookup x $ zip (tail xs) xs


sortIdentifiersByDate :: [Identifier] -> [Identifier]
sortIdentifiersByDate =
  sortBy (flip byDate)
  where
    byDate id1 id2 =
      let fn1 = takeFileName $ toFilePath id1
          fn2 = takeFileName $ toFilePath id2
          parseTime' fn = parseTimeM True defaultTimeLocale "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn
      in compare (parseTime' fn1 :: Maybe UTCTime) (parseTime' fn2 :: Maybe UTCTime)
 

-- Possible helpers for draft functionlity by inspecting a 'draf't metadata field
isNotDraft :: MonadMetadata m => Identifier -> m Bool
isNotDraft identifier = liftM (/= Just "true") (getMetadataField identifier "draft")

filterDrafts :: MonadMetadata m => [Identifier] -> m [Identifier]
filterDrafts  = filterM isNotDraft 

