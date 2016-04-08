{-# LANGUAGE OverloadedStrings #-}
--
-- Configuration settings for mrblog
--
module Config where

import Hakyll.Core.Configuration
import Hakyll.Web.Feed

author :: String
author = "Marcel van der Boom"
authoremail :: String
authoremail = "marcel@hsdev.com"
sitename :: String
sitename = "Marcel van der Boom"
siteurl :: String
siteurl = "https://mrblog.nl"
copyrightYear :: String
copyrightYear = "2016"



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

-- Feed configuration
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = author
  , feedDescription = sitename
  , feedAuthorName  = author
  , feedAuthorEmail = authoremail
  , feedRoot        = siteurl
  }
-- We need an exception to the normal ignoreFile function as
-- there are files that begin with a dot (.) that I do not want to ignore
ignoreFiles :: FilePath -> Bool
ignoreFiles ".well-known" = False
ignoreFiles path = ignoreFile defaultConfiguration path -- Point to the original, not our config!!

--
-- Settings needed for Org compiler
postsPath :: String
postsPath = "posts"

imagesPath :: String
imagesPath = "images"
