--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- System imports
import           Control.Monad (liftM)
import           Data.Monoid   ((<>))

-- Hakyll imports
import           Hakyll

-- Our own imports
import Config
import Compiler.Org


-- Main entry point
main :: IO ()
main = hakyllWith config $ do
    -- Make sure that our templates are compiled
    templateR
    
    -- Copy static files
    mapM_ static ["robots.txt"]
    -- Copy statid directories
    mapM_ (dir static) ["assets","files",".well-known"]

    -- Generate tags
    tags <- buildTags "sites/main/_posts/2*.org" (fromCapture "tag/*.html")
    
    -- Process post rules
    postR tags
    
    -- About page
    match "about/*" $ do
       route $ setExtension "html"
       compile $ orgCompiler
         >>= loadAndApplyTemplate "_layouts/page.html"    (postCtx tags)
         >>= loadAndApplyTemplate "_layouts/default.html" (postCtx tags)
         >>= relativizeUrls

    -- Process feed rules
    feedR


    -- Homepage    
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 5) . recentFirst
                    =<< loadAllSnapshots "sites/main/_posts/2*.org" "content"
            let indexCtx =
                    listField  "posts"      (postCtx tags) (return posts) <>
                    constField "title"      "Home" <>
                    constField "year"        copyrightYear <>
                    constField "site.name"   author <>
                    constField "site.url"    siteurl <>
                    constField "site.author" author <>
                    defaultContext

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


-- Post Rules
-- ✓ Take file in orgmode format
-- ✓ Figure out the publish date
-- ✓ Figure out the title
-- ✓ copy to yyyy/mm/dd/title.html
--   Process tags properly
--   Skip 'published: false' posts
--   Previous/Next links?
postR :: Tags -> Rules ()
postR tags =
  match "sites/main/_posts/2*.org" $ do
    
    -- Route should be: /yyyy/mm/dd/filename-without-date.html
    route $
      setExtension "html" `composeRoutes`
      dateFolders `composeRoutes`
      gsubRoute "sites/main/" (const "") `composeRoutes`
      gsubRoute "_posts/" (const "")

    -- Compile posts with the orgmode compiler
    compile $ orgCompiler 
      >>= loadAndApplyTemplate "_layouts/post.html"    (postCtx tags)
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "_layouts/default.html" (postCtx tags)
      >>= relativizeUrls

-- yyyy-mm-dd => yyyy/mm/dd
dateFolders :: Routes
dateFolders =
    gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ replaceAll "-" (const "/")

postCtx :: Tags -> Context String
postCtx tags =
    -- Stuff that came from jekyll, trying to use the same names
    constField "site.url" siteurl <>
    constField "site.name" sitename <>
    constField "site.author" author <>
        -- End of jekyll stuff

    -- Construct the date components:
    -- 1. If there is a 'published' metadata, use that
    -- 2. If a date can be constructed from the filename, do that
    -- otherwise error out, because for a post, we need a date
    dateField "year" "%Y" <>
    dateField "month" "%b" <>
    dateField "day" "%d" <>
    dateField "date" "%Y-%m-%d" <>

    tagsField "thetags" tags <>
    field "rtags" (\_ -> renderTagList tags) <>
    
    -- $body$, $url$, $path$ and all $foo$ from metadata are delivered
    -- by the default context
    defaultContext

                       
-- Feed Rules
feedR :: Rules ()
feedR =
  create ["feed/atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = defaultContext <>
                    bodyField "description"

      posts <- fmap (take 10) . recentFirst
              =<< loadAllSnapshots "sites/main/_posts/2*.org" "content"
      renderAtom feedConfiguration feedCtx posts

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = author
  , feedDescription = sitename
  , feedAuthorName  = author
  , feedAuthorEmail = authoremail
  , feedRoot        = siteurl
  }

  

-- Create a location for a paginated page ("/pageN/index.html")
makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "page" ++ show pageNum ++ "/index.html"

-- Create groups of ids based on dates
grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper = liftM (paginateEvery 5) . sortRecentFirst

-- Make it easier to copy loads of static stuff
static :: Pattern -> Rules ()
static f = match f $ do
  route idRoute
  compile copyFileCompiler

-- Treat whole directories
dir :: (Pattern -> Rules a) -> String -> Rules a
dir act f = act $ fromGlob $ f ++ "/**"


--------------------------------------------------------------------------------

