module Template.Functions (
  json,
  striptags
  ) where

import           Data.Aeson                    (ToJSON, encode)                   
import           Codec.Binary.UTF8.Generic     (toString)

import Hakyll

-- | Produces a String that is valid JSON (can be copy-pasted into a browser and parsed).
renderToJSON :: ToJSON a => a -> String
renderToJSON = toString . encode

-- Helper function for functionField 'json'
json :: [String] -> Item String -> Compiler String
json args _ = 
  case args of
    [k]  -> return (renderToJSON k)
    _    -> fail "Template error: json function only takes a single argument"

-- Strip tags from content
striptags :: [String] -> Item String -> Compiler String
striptags args _ =
  case args of
    [k] -> return (stripTags k)
    _   -> fail "Template error: striptags function only taks a single argument"
    
