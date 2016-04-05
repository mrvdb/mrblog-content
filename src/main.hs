--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (liftM)
import           Data.Monoid   ((<>))
import           Hakyll

sitename = "This is the name of the site, which should come from a yaml file really"

--------------------------------------------------------------------------------
-- We need an exception to the normal ignoreFile function as
-- there are files that begin with a dot (.) that I do not want to ignore
ignoreFiles :: FilePath -> Bool
ignoreFiles ".well-known" = False
ignoreFiles path = ignoreFile defaultConfiguration path

-- Make the Hakyll configuration explicit and specify overrides
config :: Configuration
config = defaultConfiguration 
  { inMemoryCache        = True           -- faster, but more memory (during build only, obviously)
  , deployCommand        = "echo TODO: implement deploy command here"
  , ignoreFile           = ignoreFiles
  , destinationDirectory = ".site"
  , storeDirectory       = ".cache"
  , tmpDirectory         = ".cache/tmp"
  }

-- Make it easier to copy loads of static stuff
static :: Pattern -> Rules ()
static f = match f $ do
  route idRoute
  compile copyFileCompiler

dir :: (Pattern -> Rules a) -> String -> Rules a
dir act f = act $ fromGlob $ f ++ "/**"

-- Main entry point
main :: IO ()
main = hakyllWith config $ do
    -- Copy static stuff
    mapM_ static ["robots.txt"]
    mapM_ (dir static) ["assets","files",".well-known"]

    -- Try tag processing
    --
    tags <- buildTags "sites/main/_posts/2*.org" (fromCapture "tag/*.html")
    
    -- Posts
    -- Take file in orgmode format
    -- Figure out the publish date
    -- Figure out the title
    -- copy to yyyy/mm/dd/title.html
    match "sites/main/_posts/2*.org" $ do
        route $
          setExtension "html" `composeRoutes`
          gsubRoute "sites/" (const "") `composeRoutes`
          gsubRoute "_posts/" (const "")
          
        compile $ pandocCompiler
          >>= loadAndApplyTemplate "_layouts/post.html"    (postCtxWithTags tags)
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "_layouts/default.html" (postCtxWithTags tags)
          >>= relativizeUrls

    -- About page
    match "about/*" $ do
       route $ setExtension "html"
       compile $ pandocCompiler
         >>= loadAndApplyTemplate "_layouts/page.html"    postCtx
         >>= loadAndApplyTemplate "_layouts/default.html" postCtx
         >>= relativizeUrls


    -- Feed
    create ["feed/atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx <>
                      bodyField "description"
           
        posts <- fmap (take 10) . recentFirst
                =<< loadAllSnapshots "sites/main/_posts/2*.org" "content"
        renderAtom feedConfiguration feedCtx posts

    -- Create pages with N posts each, the first being the index.html
    -- page (possibly) If not, start at page 2 with post N+1 and deal
    -- with index.html separately In words: pager takes a list of
    -- groups generated out of a pattern where for each match an
    -- identifier is created and puts that into the paginator
    pager <- buildPaginateWith grouper "sites/main/_posts/2*.org" makeId

    paginateRules pager $ \pageNum pattern -> do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern

        let paginateCtx = paginateContext pager pageNum
            ctx =
              constField "title" ("Blog Archive - Page " ++ (show pageNum)) <>
              listField "posts" postCtx (return posts) <>
              paginateCtx <>
              postCtx <>
              defaultContext
            
        
        makeItem ""
          >>= loadAndApplyTemplate "_layouts/paged.html" ctx
          >>= loadAndApplyTemplate "_layouts/default.html" ctx
          >>= relativizeUrls
        
    -- Homepage    
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 5) . recentFirst
                    =<< loadAllSnapshots "sites/main/_posts/2*.org" "content"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home"                <>
                    -- TODO: these do not belong here!
                    constField  "site.name" "Marcel van der Boom" <>
                    constField  "site.url" "https://mrblog.nl" <>
                    constField "date" "DATE" <>
                    constField  "site.author" "Marcel van der Boom" <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "_layouts/default.html" indexCtx
                >>= relativizeUrls


    -- Templates should just be compiled; we have them in 2 locations
    match "_layouts/*"  $ compile templateCompiler
    match "_includes/*" $ compile templateCompiler

-- Pagination related

-- Create a location for a paginated page ("/pageN/index.html")
makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "page" ++ show pageNum ++ "/index.html"

-- Create groups of ids based on dates
grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper = liftM (paginateEvery 5) . sortRecentFirst

-- Configure feed
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "Marcel van der Boom"
  , feedDescription = "Personal blog of MrB"
  , feedAuthorName = "Marcel van der Boom"
  , feedAuthorEmail = "marcel@hsdev.com"
  , feedRoot = "https://mrblog.nl"
  }
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    -- Stuff that came from jekyll, trying to use the same names
    constField "site.url" "https://mrblog.nl" <>
    constField "site.name" "Marcel van der Boom" <>
    constField "site.author" "Marcel van der Boom" <>
    constField "date" "DATE" <>
    -- End of jekyll stuff
    dateField "date" "%B %e, %Y" <>
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
