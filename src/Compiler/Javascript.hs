module Compiler.Javascript (compressJsCompiler) where

import Hakyll (Compiler, Item, itemBody, getResourceString, itemSetBody)

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Text.Jasmine


-- | Create a javascript compiler than minifies content 
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = fmap jasmin <$> getResourceString

jasmin :: String -> String
jasmin src = LB.unpack $ minify $ LB.fromChunks [(E.encodeUtf8 $ T.pack src)] 
