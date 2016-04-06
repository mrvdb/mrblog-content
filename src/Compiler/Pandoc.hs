{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.Pandoc (pandocMetadataCompilerWith) where

import BasicPrelude
import Data.Map.Lazy (foldMapWithKey)
import Hakyll
import Text.Pandoc (Pandoc(..), unMeta, MetaValue(..))
import Text.Pandoc.Builder (doc, plain, str)
import Text.Pandoc.Shared (stringify)

pandocMetadataCompilerWith :: (Pandoc -> Pandoc) -> Compiler (Item String)
pandocMetadataCompilerWith t =
  do itemPandoc <- return . fmap t =<< readPandocWith ropt =<< getResourceString
     _ <- reloadMetadata . writePandocWith wopt . fmap transformPandoc $ itemPandoc
     return . writePandocWith wopt $ itemPandoc
  where ropt = defaultHakyllReaderOptions
        wopt = defaultHakyllWriterOptions

transformPandoc :: Pandoc -> Pandoc
transformPandoc p = meta <> p
  where getMetaMap (Pandoc m _ ) = unMeta m
        meta = transformMeta . getMetaMap $ p

transformMeta :: Map String MetaValue -> Pandoc
transformMeta = doc . wrapInBlock . foldMapWithKey (\k -> toMeta k . stringify)
  where blockSep = str "---\n"
        toMeta k v = str k <> str ": " <> str v <> str "\n"
        wrapInBlock inl = plain $ blockSep <> inl <> blockSep
