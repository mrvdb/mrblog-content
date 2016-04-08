--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- System imports
import           Control.Monad (liftM)
import           Data.Monoid   ((<>))
import           Data.List (groupBy)
import           Data.Function (on)
import           System.FilePath (takeBaseName)

-- Hakyll imports
import           Hakyll

-- Our own imports
import Config
import Compiler.Org

sourcePosts :: Pattern
sourcePosts = "sites/main/_posts/2*.org"

-- Main entry point
main :: IO ()
main = hakyllWith config $ do
    templateR  -- Make sure that our templates are compiled
    staticR    -- Copy static files
    aboutR     -- About pages
    postR      -- Process posts
    feedR      -- Process atom feed
    homeR      -- Homepage

    tags <- buildTags sourcePosts (fromCapture "tag/*/index.html")
    tagsR tags -- List of posts tagged with certain tags

    archiveR   -- Grouped list of all posts
    sitemapR
------------------------------------------------------------------------------------

sitemapR :: Rules ()
sitemapR =
  match "sitemap.xml" $ do
    route idRoute

    compile $ do
     posts <- recentFirst
             =<< loadAllSnapshots sourcePosts "content"
     let ctx =
           listField  "posts"      postCtx (return posts) <>
           baseContext 
           
     getResourceBody
       >>= applyAsTemplate ctx
       
-- Grouped view of tag filter post
tagsR :: Tags -> Rules ()
tagsR tags = 
  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
      
    route idRoute
    compile $ groupedPostList title pattern
    
-- Archives are the same as the tag page, just more postings and
-- a different title
archiveR :: Rules ()
archiveR =
  create ["archive/index.html"] $ do
    let title = "Archives"
    route idRoute
    
    compile $ groupedPostList title sourcePosts

-- Common part of tag/<tag>/index.html pages and archive/index.html page
groupedPostList :: String -> Pattern -> Compiler (Item String)
groupedPostList title pattern = do
  posts <- fmap groupByYear $
    recentFirst
    =<< loadAllSnapshots pattern "content"
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

-- Homepage rules
homeR :: Rules ()
homeR = 
 match "index.html" $ do
   route idRoute

   compile $ do
     posts <- fmap (take 5) . recentFirst
             =<< loadAllSnapshots sourcePosts "content"
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
  mapM_ static ["robots.txt"]
  
  -- Whole directories that need to be copied
  mapM_ (dir static) ["assets","files",".well-known"]

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
aboutR = 
  match "about/*.org" $ do
    route $ setExtension "html"

    compile $ orgCompiler              -- This makes the metadata work
      >>= applyAsTemplate baseContext  -- This resolves template variables in HTML sections of org
      >>= loadAndApplyTemplate "_layouts/page.html"    baseContext
      >>= loadAndApplyTemplate "_layouts/default.html" baseContext
      >>= relativizeUrls
        
-- Post Rules
-- ✓ Take file in orgmode format
-- ✓ Figure out the publish date
-- ✓ Figure out the title
-- ✓ copy to yyyy/mm/dd/title.html
-- ✓ Process tags properly
--   Skip 'published: false' posts
--   Previous/Next links?
postR :: Rules ()
postR =
  match sourcePosts $ do
    
    -- Route should be: /yyyy/mm/dd/filename-without-date.html
    route $
      setExtension "html" `composeRoutes`
      dateFolders `composeRoutes`
      -- FIXME: this does not belong here
      gsubRoute "sites/main/_posts/" (const "") 

    -- Compile posts with the orgmode compiler
    compile $ orgCompiler 
      >>= loadAndApplyTemplate "_layouts/post.html"    postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "_layouts/default.html" postCtx
      >>= relativizeUrls
  where
    -- yyyy-mm-dd => yyyy/mm/dd
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
-- render better instead of dumping raw xml on a user with a browser.
feedR :: Rules ()
feedR =
  create ["feed/atom.xml"] $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) . recentFirst
              =<< loadAllSnapshots sourcePosts "content"
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

  -- $body$, $url$, $path$ and all $foo$ from metadata are delivered
  -- by the default context
  defaultContext
  
-- Create a location for a paginated page ("/pageN/index.html")
makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "page" ++ show pageNum ++ "/index.html"

-- Create groups of ids based on dates
grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper = liftM (paginateEvery 5) . sortRecentFirst

-- Group by year, depending on filename, so FIXME later
groupByYear :: [Item a] -> [(Int, [Item a])]
groupByYear = map (\pg -> ( year (head pg), pg) ) .
              groupBy ((==) `on` year)
   where year :: Item a -> Int
         year = read . take 4 . takeBaseName . toFilePath . itemIdentifier
