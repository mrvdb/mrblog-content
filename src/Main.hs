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
    templateR -- Make sure that our templates are compiled
    staticR   -- Copy static files
    aboutR    -- About pages
    postR     -- Process posts
    feedR     -- Process atom feed
    homeR     -- Homepage
    

-- Homepage rules
homeR :: Rules ()
homeR = 
 match "index.html" $ do
   route idRoute

   compile $ do
     posts <- fmap (take 5) . recentFirst
             =<< loadAllSnapshots "sites/main/_posts/2*.org" "content"
     let indexCtx =
           listField  "posts"      postCtx (return posts) <>
           constField "title"      "Home" <>
           constField "year"        copyrightYear <>
           jekyllContext <>
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
  match "about/*" $ do
    route $ setExtension "html"

    compile $ orgCompiler
      >>= loadAndApplyTemplate "_layouts/page.html"    aboutCtx
      >>= loadAndApplyTemplate "_layouts/default.html" aboutCtx
      >>= relativizeUrls

  where
    aboutCtx :: Context String
    aboutCtx =
      constField "year" copyrightYear <>
      jekyllContext <>
      defaultContext
    
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
  match "sites/main/_posts/2*.org" $ do
    
    -- Route should be: /yyyy/mm/dd/filename-without-date.html
    route $
      setExtension "html" `composeRoutes`
      dateFolders `composeRoutes`
      gsubRoute "sites/main/" (const "") `composeRoutes`
      gsubRoute "_posts/" (const "")

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
    -- $body$, $url$, $path$ and all $foo$ from metadata are delivered
    -- by the default context
    defaultContext <>
    
    -- Stuff that came from jekyll, trying to use the same names
    jekyllContext <>
    
    -- Construct the date components:
    -- 1. If there is a 'published' metadata, use that
    -- 2. If a date can be constructed from the filename, do that
    -- otherwise error out, because for a post, we need a date
    dateField "year" "%Y" <>
    dateField "month" "%b" <>
    dateField "day" "%d" <>
    dateField "date" "%Y-%m-%d" <>

    listFieldWith "taglist" tagCtx getTagItems
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
              =<< loadAllSnapshots "sites/main/_posts/2*.org" "content"
      renderAtom feedConfiguration feedCtx posts
   where
     feedCtx :: Context String
     feedCtx =
       defaultContext <>
       bodyField "description"

     feedConfiguration :: FeedConfiguration
     feedConfiguration = FeedConfiguration
       { feedTitle       = author
       , feedDescription = sitename
       , feedAuthorName  = author
       , feedAuthorEmail = authoremail
       , feedRoot        = siteurl
       }


-- Jekyll variables, probably can go after a bit.
jekyllContext :: Context String
jekyllContext =
  constField "site.name"   sitename <>
  constField "site.url"    siteurl <>
  constField "site.author" author
  
-- Create a location for a paginated page ("/pageN/index.html")
makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "page" ++ show pageNum ++ "/index.html"

-- Create groups of ids based on dates
grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper = liftM (paginateEvery 5) . sortRecentFirst

