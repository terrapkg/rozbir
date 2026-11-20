module Rozbir.Edit
  ( TextEdit (..),
    EditError (..),
    describeEditError,
    apply,
    applyToTree,
    setTag,
    setTagLiteral,
    setMacro,
    bumpRelease,
    setVersion,
    ChangelogInput (..),
    addChangelog,
    tagValueSpan,
    defineBodySpan,
    preambleTags,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Rozbir.Changelog (renderEntry)
import Rozbir.Query (definitions)
import Rozbir.Syntax
import Rozbir.Syntax.Parse (parse)
import Rozbir.Syntax.Render (render, renderFrags)

data TextEdit = TextEdit
  { editSpan :: !Span,
    editText :: !Text
  }
  deriving stock (Eq, Show)

instance ToJSON TextEdit where
  toJSON (TextEdit sp t) = object ["span" .= sp, "text" .= t]

data EditError
  = NotFound Text
  | Ambiguous Text [Span]
  | CantEdit Text
  deriving stock (Eq, Show)

describeEditError :: EditError -> Text
describeEditError = \case
  NotFound what -> "no " <> what <> " to edit"
  Ambiguous what _ -> what <> " is set in more than one place, so there's no single line to edit"
  CantEdit why -> why

apply :: [TextEdit] -> Text -> Text
apply edits src = foldl step src (sortOn (Down . posOffset . spanStart . editSpan) edits)
  where
    step t (TextEdit (Span s e) new) = T.take (posOffset s) t <> new <> T.drop (posOffset e) t

applyToTree :: [TextEdit] -> Tree -> Tree
applyToTree edits tree = parse (apply edits (render tree))

tagValueSpan :: Line -> TagLine -> Span
tagValueSpan l TagLine {..} =
  let start = advancePos (spanStart (lineSpan l)) (tagIndent <> tagName <> maybe "" (\q -> qualGap q <> qualRaw q) tagQualifier <> tagSep)
   in Span start (advancePos start (renderFrags tagValue))

defineBodySpan :: Line -> DefineLine -> Span
defineBodySpan l DefineLine {..} =
  let prefix =
        defIndent
          <> defKeyword
          <> defGap
          <> defName
          <> maybe "" (\o -> "(" <> o <> ")") defOpts
          <> maybe "" (\m -> "<" <> m <> ">") defMods
          <> defSep
      start = advancePos (spanStart (lineSpan l)) prefix
   in Span start (advancePos start (renderFrags defBody))

preambleTags :: Tree -> [(Line, TagLine)]
preambleTags tree = go Nothing (treeLines tree)
  where
    go _ [] = []
    go sec (l : ls) = case lineBody l of
      Header h -> go (Just (secKind h)) ls
      Tag t | sec == Nothing -> (l, t) : go sec ls
      _ -> go sec ls

singleMacro :: [Frag] -> Maybe Name
singleMacro = \case
  [Macro (Bare flags n)] | T.all (== '?') flags -> Just n
  [Macro (Braced flags n Nothing)] | T.all (== '?') flags -> Just n
  _ -> Nothing

findTag :: Text -> Tree -> Either EditError (Line, TagLine)
findTag key tree = case [x | x@(_, t) <- preambleTags tree, T.toLower (tagName t) == T.toLower key] of
  [] -> Left (NotFound (key <> " tag"))
  [x] -> Right x
  xs -> Left (Ambiguous key [lineSpan l | (l, _) <- xs])

setTagLiteral :: Text -> Text -> Tree -> Either EditError [TextEdit]
setTagLiteral key value tree = do
  (l, t) <- findTag key tree
  Right [TextEdit (tagValueSpan l t) value]

setTag :: Text -> Text -> Tree -> Either EditError [TextEdit]
setTag key value tree = do
  (l, t) <- findTag key tree
  case singleMacro (tagValue t) of
    Just n | Map.member n (definitions tree) -> setMacro n value tree
    _ -> Right [TextEdit (tagValueSpan l t) value]

setMacro :: Name -> Text -> Tree -> Either EditError [TextEdit]
setMacro n value tree = go (8 :: Int) n
  where
    defs = definitions tree
    located = [(l, d) | l@Line {lineBody = Define d} <- treeLines tree, defKind d `elem` [DefGlobal, DefDefine]]
    go fuel name' = case [x | x@(_, d) <- located, defName d == name'] of
      [] -> Left (NotFound ("%" <> name' <> " definition"))
      [(l, d)] -> case singleMacro (defBody d) of
        Just inner | fuel > 0, Map.member inner defs -> go (fuel - 1) inner
        _ -> Right [TextEdit (defineBodySpan l d) value]
      xs -> Left (Ambiguous ("%" <> name') [lineSpan l | (l, _) <- xs])

releaseTarget :: Tree -> Either EditError (Span, [Frag])
releaseTarget tree = do
  (l, t) <- findTag "release" tree
  case singleMacro (tagValue t) of
    Just n | Map.member n (definitions tree) -> follow (8 :: Int) n
    _ -> Right (tagValueSpan l t, tagValue t)
  where
    located = [(l, d) | l@Line {lineBody = Define d} <- treeLines tree, defKind d `elem` [DefGlobal, DefDefine]]
    follow fuel n = case [x | x@(_, d) <- located, defName d == n] of
      [(l, d)] -> case singleMacro (defBody d) of
        Just inner | fuel > 0, Map.member inner (definitions tree) -> follow (fuel - 1) inner
        _ -> Right (defineBodySpan l d, defBody d)
      [] -> Left (NotFound ("%" <> n <> " definition"))
      xs -> Left (Ambiguous ("%" <> n) [lineSpan l | (l, _) <- xs])

bumpRelease :: Tree -> Either EditError [TextEdit]
bumpRelease = editReleaseNumber (+ 1)

editReleaseNumber :: (Integer -> Integer) -> Tree -> Either EditError [TextEdit]
editReleaseNumber f tree = do
  (sp, frags) <- releaseTarget tree
  case frags of
    Lit t : _
      | (digits, _) <- T.span isDigit t,
        not (T.null digits) ->
          let n = read (T.unpack digits) :: Integer
              start = spanStart sp
           in Right [TextEdit (Span start (advancePos start digits)) (T.pack (show (f n)))]
    _ -> Left (CantEdit "Release doesn't start with a number (it may use %autorelease), so it can't be bumped")

setVersion :: Text -> Tree -> Either EditError [TextEdit]
setVersion v tree = do
  versionEdits <- setTag "version" v tree
  let releaseEdits = either (const []) id (editReleaseNumber (const 1) tree)
  Right (versionEdits <> releaseEdits)

data ChangelogInput = ChangelogInput
  { ciDate :: !Day,
    ciAuthor :: !Text,
    ciEvr :: !(Maybe Text),
    ciItems :: ![Text]
  }
  deriving stock (Eq, Show)

addChangelog :: ChangelogInput -> Tree -> Either EditError [TextEdit]
addChangelog ChangelogInput {..} tree =
  let entry = renderEntry ciDate ciAuthor ciEvr ciItems
      headers = [l | l@Line {lineBody = Header h} <- treeLines tree, secKind h == SChangelog]
   in case headers of
        [l] ->
          let at = spanEnd (lineFullSpan l)
              rest = [x | x <- treeLines tree, posOffset (spanStart (lineSpan x)) >= posOffset at]
              separator = case rest of
                Line {lineBody = Blank _} : _ -> ""
                [] -> ""
                _ -> "\n"
              newline = if T.null (lineEol l) then "\n" else ""
           in Right [TextEdit (pointSpan at) (newline <> entry <> separator)]
        [] ->
          let src = render tree
              end = advancePos startPos src
              lead = if T.null src || "\n" `T.isSuffixOf` src then "\n" else "\n\n"
           in Right [TextEdit (pointSpan end) (lead <> "%changelog\n" <> entry)]
        many -> Left (Ambiguous "%changelog" (map lineSpan many))
