module Rozbir.Format
  ( FormatOptions (..),
    defaultFormatOptions,
    format,
    formatText,
    dominantColumn,
  )
where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Rozbir.Edit (TextEdit (..), apply)
import Rozbir.Syntax
import Rozbir.Syntax.Parse (parse)
import Rozbir.Syntax.Render (render)

data FormatOptions = FormatOptions
  { foAlignTags :: !Bool,
    foTrailingWhitespace :: !Bool,
    foMaxBlankLines :: !(Maybe Int),
    foFinalNewline :: !Bool
  }
  deriving stock (Eq, Show)

defaultFormatOptions :: FormatOptions
defaultFormatOptions = FormatOptions True True (Just 2) True

formatText :: FormatOptions -> Text -> Text
formatText FormatOptions {..} src0 = foldl step src0 phases
  where
    phases =
      [alignTags | foAlignTags]
        <> [trailing | foTrailingWhitespace]
        <> [(`blankRuns` n) | Just n <- [foMaxBlankLines]]
        <> [finalNewline | foFinalNewline]
    step src phase = apply (phase (parse src)) src

format :: FormatOptions -> Tree -> [TextEdit]
format opts tree =
  let src = render tree
      new = formatText opts src
   in [TextEdit (Span startPos (advancePos startPos src)) new | new /= src]

tagPrefix :: TagLine -> Text
tagPrefix TagLine {..} = tagIndent <> tagName <> maybe "" (\q -> qualGap q <> qualRaw q) tagQualifier

preambleTags :: Tree -> [(Line, TagLine)]
preambleTags tree = go Nothing (treeLines tree)
  where
    go _ [] = []
    go sec (l : ls) = case lineBody l of
      Header h -> go (Just (secKind h)) ls
      Tag t | isPreambleLike sec -> (l, t) : go sec ls
      _ -> go sec ls

dominantColumn :: Tree -> Maybe Int
dominantColumn tree =
  case sortOn (Down . snd) (Map.toList counts) of
    (col, n) : _ | n * 2 > total -> Just col
    _ -> Nothing
  where
    aligned =
      [ T.length (tagPrefix t) + T.length (tagSep t)
      | (_, t) <- preambleTags tree,
        not (T.any (== '\t') (tagSep t)),
        T.length (T.dropWhile (/= ':') (tagSep t)) > 2
      ]
    counts = Map.fromListWith (+) [(c, 1 :: Int) | c <- aligned]
    total = length (preambleTags tree)

alignTags :: Tree -> [TextEdit]
alignTags tree = case dominantColumn tree of
  Nothing -> []
  Just col ->
    [ TextEdit (Span sepStart sepEnd) wanted
    | (l, t) <- preambleTags tree,
      not (T.any (== '\t') (tagSep t)),
      not (null (tagValue t)),
      T.null (T.takeWhile (/= ':') (tagSep t)),
      let prefix = tagPrefix t
          gap = max 1 (col - T.length prefix - 1)
          wanted = ":" <> T.replicate gap " "
          sepStart = advancePos (spanStart (lineSpan l)) prefix
          sepEnd = advancePos sepStart (tagSep t),
      wanted /= tagSep t
    ]

trailing :: Tree -> [TextEdit]
trailing tree =
  [ TextEdit (Span (advancePos s kept) (advancePos s line)) ""
  | (s, line) <- physical (render tree),
    let kept = T.dropWhileEnd (\c -> c == ' ' || c == '\t') line,
    kept /= line,
    not ("\\" `T.isSuffixOf` kept)
  ]

physical :: Text -> [(Pos, Text)]
physical src = go startPos (T.splitOn "\n" src)
  where
    go _ [] = []
    go p (l : ls) =
      let body = if "\r" `T.isSuffixOf` l then T.dropEnd 1 l else l
          next = advancePos p (l <> "\n")
       in (p, body) : go next ls

blankRuns :: Tree -> Int -> [TextEdit]
blankRuns tree limit = go [] (treeLines tree)
  where
    isBlank l = case lineBody l of
      Blank t -> T.null t
      _ -> False
    go acc = \case
      [] -> flush acc
      l : ls
        | isBlank l -> go (l : acc) ls
        | otherwise -> flush acc <> go [] ls
    flush acc
      | length acc > limit =
          let run = reverse acc
              extra = drop limit run
           in case extra of
                first : _ -> [TextEdit (Span (spanStart (lineSpan first)) (spanEnd (lineFullSpan (last extra)))) ""]
                [] -> []
      | otherwise = []

finalNewline :: Tree -> [TextEdit]
finalNewline tree =
  let src = render tree
      end = advancePos startPos src
      stripped = T.dropWhileEnd (\c -> c == '\n' || c == '\r' || c == ' ' || c == '\t') src
   in if
        | T.null src -> []
        | "\n" `T.isSuffixOf` src && not ("\n\n" `T.isSuffixOf` src) -> []
        | not ("\n" `T.isSuffixOf` src) -> [TextEdit (pointSpan end) "\n"]
        | otherwise ->
            let start = advancePos startPos stripped
             in [TextEdit (Span start end) "\n" | not ("\\" `T.isSuffixOf` stripped)]
