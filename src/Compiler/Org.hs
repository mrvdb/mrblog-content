{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Compiler.Org (orgCompiler) where

import           Config
import           BasicPrelude
import           Compiler.Pandoc
import           Control.Monad ()
import           Data.Map.Lazy (mapWithKey)
import           Hakyll
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.Shared
import           Text.Pandoc.Walk
import qualified Text.Parsec as P
import           Text.Parsec.String (Parser)
import           Text.Read (readEither)

orgCompiler :: Compiler (Item String)
orgCompiler = pandocMetadataCompilerWith transform
  where transform = tFixLinks . tImages . tDateMeta . tTableOfContents . tHeaderIds

--------------------------------------------------------------------------------
-- Meta transformer
--------------------------------------------------------------------------------

tDateMeta :: Pandoc -> Pandoc
tDateMeta (Pandoc meta body) = Pandoc (walk modMetaDate meta) body
  where modMetaDate = Meta . mapWithKey convertDate . unMeta
        convertDate k r@(MetaInlines [Str v]) =
          if k == "date"
            then MetaInlines . toList . str . strip $ v
            else r
        convertDate _ v = v
        strip = stripLeft "<" . stripRight ">"

-- TODO: move to helpers
stripLeft :: String -> String -> String
stripLeft a b = if a `isPrefixOf` b then drop (length a) b else b

-- TODO: move to helpers
stripRight :: String -> String -> String
stripRight a b = if a `isSuffixOf` b then take (length b - length a) b else b

--------------------------------------------------------------------------------
-- Table of Contents transformer
--------------------------------------------------------------------------------

-- | replace all toc marks by table of contents
tTableOfContents :: Pandoc -> Pandoc
tTableOfContents p@(Pandoc m bs) = Pandoc m . concatMap modToc $ bs
  where modToc :: Block -> [Block]
        modToc b@(RawBlock _ s) = maybe [b] (generateToc p) . parseTOCConfig $ s
        modToc b = [b]

generateToc :: Pandoc -> TOC -> [Block]
generateToc p (TOCHeadLines level) = [tocHeader, tableOfContents level hs]
  where hs = filter (isMaxLevel level) $ getHeaders p
        isMaxLevel m (Header l _ _) = m >= l
        isMaxLevel _ _              = False
        tocHeader = Header 1 ("", [], []) . toList . str $ "Table of Contents"

tableOfContents :: Int -> [Block] -> Block
tableOfContents level headers = BulletList $ map (elementToListItem level)
  $ hierarchicalize headers

elementToListItem :: Int -> Element -> [Block]
elementToListItem level (Sec lev nums _ headerText subsecs)
  = Plain [Link nullAttr headerText ("#" ++ headerId nums, "")] :
    [ BulletList (map (elementToListItem level) subsecs) |
      not (null subsecs) && lev <= level ]
elementToListItem _ (Blk _) = []

--------------------------------------------------------------------------------
-- Table of Contents config
--------------------------------------------------------------------------------

data TOC = TOCHeadLines Int deriving Show

parseTOCConfig :: String -> Maybe TOC
parseTOCConfig = either (const Nothing) Just . P.parse tocParser ""

tocParser :: Parser TOC
tocParser =
  liftM readEither
        (P.spaces >> P.string "#+TOC:" >> P.spaces >> P.string "headlines" >>
         P.spaces >> P.many1 P.digit) >>=
  \case
    Right level -> return $ TOCHeadLines level
    Left e -> P.parserFail e

--------------------------------------------------------------------------------
-- Headers
--------------------------------------------------------------------------------

-- | add unique id to all headers
tHeaderIds :: Pandoc -> Pandoc
tHeaderIds (Pandoc m bs) = Pandoc m . convert . hierarchicalize $ bs
  where mark :: Element -> [Block]
        mark (Blk b) = [b]
        mark (Sec lvl nums (_, cs, kvps) lbl ctns) =
          Header lvl (headerId nums, cs, kvps) lbl : convert ctns
        convert = concatMap mark

headerId :: [Int] -> String
headerId = ("sec" ++) . foldMap (("-" ++) . textToString . show) . reverse

getHeaders :: Pandoc -> [Block]
getHeaders = query selectHeader
  where selectHeader h@(Header{}) = [h]
        selectHeader _ = []

--------------------------------------------------------------------------------
-- Images transform
--------------------------------------------------------------------------------

-- | Transform #+BEGIN_FIGURE blocks into special divs. First line of block must
-- contain only name of image to insert as 'figure' (this image must be in
-- imagesPath folder). All other lines of block are treated as caption
-- (description).
tImages :: Pandoc -> Pandoc
tImages = walk modImgDiv

modImgDiv :: Block -> Block
modImgDiv (Div attr@("", ["figure"], []) [Para (Str url' : descr')]) =
    Div attr [rawHtml $ "<img src=\"" ++ url ++ "\" alt=\"" ++ descr ++ "\">"
             ,rawHtml $ "<p class=\"caption\">" ++ descr ++ "</p>"]
  where rawHtml = RawBlock (Format "html")
        descr = stringify $ Para descr'
        url = convertUrl postsPath imagesPath url'
modImgDiv b = b

convertUrl :: String -> String -> String -> String
convertUrl d1 d2 f = concatify $ convert (partify d1) (partify d2) ++ [f]
  where partify = splitBy (== '/')
        concatify = intercalate "/"
        dots as = replicate (length as) ".."
        convert as bs = dots as ++ bs


--------------------------------------------------------------------------------
-- Fix links to posts transform
--------------------------------------------------------------------------------

tFixLinks :: Pandoc -> Pandoc
tFixLinks = walk modLinks

modLinks :: Inline -> Inline
modLinks (Link nullAttr content (url', title)) = Link nullAttr content (url, title)
  where url = if isPostUrl url'
                 then changeExt "html" url'
                 else url'
        isPostUrl = not . isInfixOf "/"
modLinks i = i

-- TODO: move to helpers
dropExt :: String -> String
dropExt fp = maybe fp (`take` fp) $ lastIndexOf '.' fp

-- TODO: move to helpers
changeExt :: String -> String -> String
changeExt ext fp = dropExt fp ++ "." ++ ext

-- TODO: move to helpers
indexOf :: Eq a => a -> [a] -> Maybe Int
indexOf = indexOf' 0
  where indexOf' _ _ [] = Nothing
        indexOf' i a (x:xs) =
          if a == x
             then Just i
             else indexOf' (i + 1) a xs

-- TODO: move to helpers
lastIndexOf :: Eq a => a -> [a] -> Maybe Int
lastIndexOf a as = ((length as - 1) -) <$> indexOf a (reverse as)
