-- | small text edits to a spec that keep the rest of the file unchanged.
-- functions return edits instead of new text, so callers can show or combine
-- them before calling 'apply'.
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

-- | replace the text in a span
data TextEdit = TextEdit
  { -- | the text to replace, empty for an insert
    editSpan :: !Span,
    editText :: !Text
  }
  deriving stock (Eq, Show)

instance ToJSON TextEdit where
  toJSON (TextEdit sp t) = object ["span" .= sp, "text" .= t]

-- | why an edit couldn't be made
data EditError
  = -- | the thing to edit isn't in the spec
    NotFound Text
  | -- | it's set on more than one line, such as in both branches of an @%if@
    Ambiguous Text [Span]
  | -- | it's there but can't be edited safely, with a message
    CantEdit Text
  deriving stock (Eq, Show)

-- | a message for the user
describeEditError :: EditError -> Text
describeEditError = \case
  NotFound what -> "no " <> what <> " to edit"
  Ambiguous what _ -> what <> " is set in more than one place, so there's no single line to edit"
  CantEdit why -> why

-- | apply edits to text. edits must not overlap.
apply :: [TextEdit] -> Text -> Text
apply edits src = foldl step src (sortOn (Down . posOffset . spanStart . editSpan) edits)
  where
    step t (TextEdit (Span s e) new) = T.take (posOffset s) t <> new <> T.drop (posOffset e) t

-- | apply edits to a tree and parse the result again
applyToTree :: [TextEdit] -> Tree -> Tree
applyToTree edits tree = parse (apply edits (render tree))

-- | span of a tag line's value, after the colon and spacing
tagValueSpan :: Line -> TagLine -> Span
tagValueSpan l TagLine {..} =
  let start = advancePos (spanStart (lineSpan l)) (tagIndent <> tagName <> maybe "" (\q -> qualGap q <> qualRaw q) tagQualifier <> tagSep)
   in Span start (advancePos start (renderFrags tagValue))

-- | span of a @%global@ or @%define@ body
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

-- | tag lines before the first section header, so main package tags only
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

-- | set a main package tag's value on its own line, even if it's a macro
setTagLiteral :: Text -> Text -> Tree -> Either EditError [TextEdit]
setTagLiteral key value tree = do
  (l, t) <- findTag key tree
  Right [TextEdit (tagValueSpan l t) value]

-- | set a main package tag. if the value is just a macro the spec defines,
-- like @Version: %{ver}@, the definition is changed instead.
setTag :: Text -> Text -> Tree -> Either EditError [TextEdit]
setTag key value tree = do
  (l, t) <- findTag key tree
  case singleMacro (tagValue t) of
    Just n | Map.member n (definitions tree) -> setMacro n value tree
    _ -> Right [TextEdit (tagValueSpan l t) value]

-- | set a @%global@ or @%define@ body. if the body is just another macro
-- the spec defines, that one is changed instead.
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

-- | add 1 to the number at the start of Release, keeping the rest. fails
-- with 'CantEdit' for @%autorelease@ or other non-numeric releases.
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

-- | set Version and reset Release to 1. Release is left alone if it can't
-- be edited.
setVersion :: Text -> Tree -> Either EditError [TextEdit]
setVersion v tree = do
  versionEdits <- setTag "version" v tree
  let releaseEdits = either (const []) id (editReleaseNumber (const 1) tree)
  Right (versionEdits <> releaseEdits)

-- | a new changelog entry
data ChangelogInput = ChangelogInput
  { ciDate :: !Day,
    -- | name and email, like @Jane Doe \<jane\@example.com\>@
    ciAuthor :: !Text,
    -- | the version-release to show after the author, if any
    ciEvr :: !(Maybe Text),
    -- | the items. a leading dash is added when missing.
    ciItems :: ![Text]
  }
  deriving stock (Eq, Show)

-- | add an entry at the top of @%changelog@, creating the section at the end
-- of the file if there isn't one
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
