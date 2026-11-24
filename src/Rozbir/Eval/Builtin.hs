-- | rpm's builtin macros and the pure string helpers behind them, plus the
-- macros rozbir copies from rpm's @macros.in@. nothing here runs a shell or Lua.
module Rozbir.Eval.Builtin
  ( Builtin (..),
    builtinNamed,
    builtinNames,
    ArgStyle (..),
    builtinArgStyle,
    preludeMacros,
    closedPrefixes,
    closedNames,
    isAutoName,
    posixBasename,
    posixDirname,
    shrink,
    shescape,
    suffix,
    urlPath,
    luaLen,
    asciiUpper,
    asciiLower,
    luaRep,
    luaSub,
    luaReverse,
    splitQuoted,
    getopt,
    GetoptResult (..),
  )
where

import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower, toUpper)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Text.Read (readMaybe)

-- | a builtin macro from rpm's @builtinmacros[]@
data Builtin
  = -- | @%{P:n}@, the same as @%PATCHn@
    BP
  | -- | @%{S:n}@, the same as @%SOURCEn@
    BS
  | BBasename
  | BDefine
  | BDirname
  | -- | @%dnl@, which drops the rest of the line
    BDnl
  | -- | @%dump@, which expands to nothing
    BDump
  | -- | @%{echo:...}@, which records an info message
    BEcho
  | -- | @%{error:...}@, which records an error and gives a 'Failed' hole
    BError
  | -- | @%{exists:...}@, which reads the filesystem
    BExists
  | BExpand
  | BExprFn
  | -- | @%{getconfdir}@, always @\/usr\/lib\/rpm@
    BGetconfdir
  | -- | @%{getenv:...}@, which reads the environment
    BGetenv
  | -- | @%{getncpus}@, which depends on the build host
    BGetncpus
  | BGlobal
  | -- | @%{gsub:...}@, using Lua patterns
    BGsub
  | -- | @%{len:...}@, the length in bytes
    BLen
  | -- | @%{load:...}@, which reads the filesystem
    BLoad
  | BLower
  | -- | @%{lua:...}@, which is never run
    BLua
  | BMacrobody
  | BQuote
  | BRep
  | BReverse
  | -- | @%{rpmversion}@, which depends on the build host
    BRpmversion
  | BShrink
  | BSpan
  | BSub
  | BSuffix
  | -- | @%trace@, which expands to nothing
    BTrace
  | BU2p
  | BShescape
  | -- | @%{uncompress:...}@, which expands to the uncompress command
    BUncompress
  | BUndefine
  | BUpper
  | BUrl2path
  | BVerbose
  | -- | @%{warn:...}@, which records a warning
    BWarn
  | -- | @%{xdg:...}@, which reads the environment
    BXdg
  deriving stock (Eq, Show, Enum, Bounded)

builtinTable :: [(Text, Builtin)]
builtinTable =
  [ ("P", BP),
    ("S", BS),
    ("basename", BBasename),
    ("define", BDefine),
    ("dirname", BDirname),
    ("dnl", BDnl),
    ("dump", BDump),
    ("echo", BEcho),
    ("error", BError),
    ("exists", BExists),
    ("expand", BExpand),
    ("expr", BExprFn),
    ("getconfdir", BGetconfdir),
    ("getenv", BGetenv),
    ("getncpus", BGetncpus),
    ("global", BGlobal),
    ("gsub", BGsub),
    ("len", BLen),
    ("load", BLoad),
    ("lower", BLower),
    ("lua", BLua),
    ("macrobody", BMacrobody),
    ("quote", BQuote),
    ("rep", BRep),
    ("reverse", BReverse),
    ("rpmversion", BRpmversion),
    ("shrink", BShrink),
    ("span", BSpan),
    ("sub", BSub),
    ("suffix", BSuffix),
    ("trace", BTrace),
    ("u2p", BU2p),
    ("shescape", BShescape),
    ("uncompress", BUncompress),
    ("undefine", BUndefine),
    ("upper", BUpper),
    ("url2path", BUrl2path),
    ("verbose", BVerbose),
    ("warn", BWarn),
    ("xdg", BXdg)
  ]

-- | the builtin with this name, if there is one
builtinNamed :: Text -> Maybe Builtin
builtinNamed n = snd <$> find ((== n) . fst) builtinTable

-- | every builtin name
builtinNames :: [Text]
builtinNames = map fst builtinTable

-- | how a builtin takes its arguments
data ArgStyle
  = -- | ignores arguments
    TakesNoArgs
  | -- | takes expanded arguments
    TakesArgs
  | -- | takes expanded arguments, but works without any
    TakesOptionalArgs
  | -- | reads the unexpanded text itself, like @%define@
    ParsesRaw
  deriving stock (Eq, Show)

-- | how a builtin takes its arguments
builtinArgStyle :: Builtin -> ArgStyle
builtinArgStyle = \case
  BDefine -> ParsesRaw
  BGlobal -> ParsesRaw
  BDnl -> ParsesRaw
  BLua -> ParsesRaw
  BVerbose -> ParsesRaw
  BDump -> TakesNoArgs
  BGetconfdir -> TakesNoArgs
  BRpmversion -> TakesNoArgs
  BTrace -> TakesNoArgs
  BGetncpus -> TakesOptionalArgs
  _ -> TakesArgs

-- | macros copied from rpm's @macros.in@ that every env starts with, like
-- @%with@ and @%bcond@, as name, options and body
preludeMacros :: [(Text, Maybe Text, Text)]
preludeMacros =
  [ ("nil", Nothing, "%{!?nil}"),
    ("defined", Just "", "%{expand:%%{?%{1}:1}%%{!?%{1}:0}}"),
    ("undefined", Just "", "%{expand:%%{?%{1}:0}%%{!?%{1}:1}}"),
    ("bcond_override_default", Just "", "%{expand:%%global bcond_override_default_%{1} %{2}}"),
    ("__bcond_override_default", Just "", "%{expand:%%[\"%%{?bcond_override_default_%{1}}\"||\"%%{2}\"]}"),
    ( "bcond",
      Just "",
      "%[ %{__bcond_override_default %{1} %{2}}\n    ? \"%{expand:%%{!?_without_%{1}:%%global with_%{1} 1}}\"\n    : \"%{expand:%%{?_with_%{1}:%%global with_%{1} 1}}\"\n]"
    ),
    ("bcond_with", Just "", "%bcond %{1} 0"),
    ("bcond_without", Just "", "%bcond %{1} 1"),
    ("with", Just "", "%{expand:%%{?with_%{1}:1}%%{!?with_%{1}:0}}"),
    ("without", Just "", "%{expand:%%{?with_%{1}:0}%%{!?with_%{1}:1}}")
  ]

-- | name prefixes only a spec can define, like @with_@ and @SOURCE@. these are
-- never left as 'Rozbir.Eval.Types.Undefined' holes.
closedPrefixes :: [Text]
closedPrefixes = ["with_", "_with_", "_without_", "bcond_override_default_", "SOURCE", "PATCH"]

-- | names only a spec can define, like @name@ and @version@
closedNames :: [Text]
closedNames =
  [ "name",
    "version",
    "release",
    "epoch",
    "summary",
    "license",
    "sourcelicense",
    "url",
    "vcs",
    "buildsystem",
    "bugurl",
    "modularitylabel",
    "upstreamreleases"
  ]

-- | whether a name is a macro argument, like @1@, @*@, @#@ or @-f@
isAutoName :: Text -> Bool
isAutoName n = case T.uncons n of
  Just ('-', _) -> True
  Just (c, _) | isDigit c -> True
  _ -> n `elem` ["*", "**", "#"]

-- | the last path component, like POSIX @basename@
posixBasename :: Text -> Text
posixBasename t
  | T.null t = "."
  | T.all (== '/') t = "/"
  | otherwise = T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') t)

-- | everything but the last path component, like POSIX @dirname@
posixDirname :: Text -> Text
posixDirname t
  | T.null t = "."
  | T.all (== '/') t = "/"
  | otherwise =
      let trimmed = T.dropWhileEnd (== '/') t
          parent = T.dropWhileEnd (/= '/') trimmed
       in if
            | T.null parent -> "."
            | T.all (== '/') parent -> "/"
            | otherwise -> T.dropWhileEnd (== '/') parent

-- | trim whitespace and squeeze runs of it into single spaces
shrink :: Text -> Text
shrink = T.unwords . T.words

-- | single-quote each argument for a shell and join them with spaces
shescape :: [Text] -> Text
shescape = T.unwords . map (\a -> "'" <> T.replace "'" "'\\''" a <> "'")

-- | the text after the last dot, or empty if there's no dot
suffix :: Text -> Text
suffix t = case T.breakOnEnd "." t of
  ("", _) -> ""
  (_, s) -> s

-- | the path part of a URL, or the text itself if it isn't a URL
urlPath :: Text -> Text
urlPath t = case T.breakOn "://" t of
  (_, "") -> nonEmpty t
  (scheme, rest)
    | isUrlScheme scheme -> nonEmpty (T.dropWhile (/= '/') (T.drop 3 rest))
    | otherwise -> nonEmpty t
  where
    nonEmpty p = if T.null p then "/" else p
    isUrlScheme s = s `elem` ["ftp", "http", "https", "hkp", "file"]

-- | the length in UTF-8 bytes, like Lua's @string.len@
luaLen :: Text -> Text
luaLen = T.pack . show . BS.length . encodeUtf8

-- | Lua's @string.rep@ on the arguments. 'Nothing' for bad arguments.
luaRep :: [Text] -> Maybe Text
luaRep = \case
  [s, n] -> (\k -> T.replicate k s) <$> count n
  [s, n, sep] -> (\k -> T.intercalate sep (replicate k s)) <$> count n
  _ -> Nothing
  where
    count n = max 0 <$> readMaybe (T.unpack n)

-- | Lua's @string.sub@ on the arguments, counting UTF-8 bytes. 'Nothing' for
-- bad arguments.
luaSub :: [Text] -> Maybe Text
luaSub = \case
  [s] -> Just s
  [s, i] -> slice s <$> readMaybe (T.unpack i) <*> pure (-1)
  [s, i, j] -> slice s <$> readMaybe (T.unpack i) <*> readMaybe (T.unpack j)
  _ -> Nothing
  where
    slice s i j =
      let bytes = encodeUtf8 s
          len = BS.length bytes
          norm k = if k < 0 then max 0 (len + k + 1) else k
          start = max 1 (norm i)
          end = min len (norm j)
       in if start > end then "" else decodeUtf8Lenient (BS.take (end - start + 1) (BS.drop (start - 1) bytes))

-- | reverse the UTF-8 bytes, like Lua's @string.reverse@
luaReverse :: Text -> Text
luaReverse = decodeUtf8Lenient . BS.reverse . encodeUtf8

-- | split macro arguments on blanks, keeping @%{quote:...}@ text as one
-- argument
splitQuoted :: Text -> [Text]
splitQuoted = go False "" [] . T.unpack
  where
    go quoted cur acc = \case
      [] -> reverse (flush cur acc)
      '\x1f' : rest -> go (not quoted) cur acc rest
      c : rest
        | not quoted && (c == ' ' || c == '\t') -> go quoted "" (flush cur acc) rest
        | otherwise -> go quoted (c : cur) acc rest
    flush cur acc = if null cur then acc else T.pack (reverse cur) : acc

-- | the result of 'getopt'
data GetoptResult = GetoptResult
  { -- | options in order, with their values
    goOptions :: [(Char, Maybe Text)],
    -- | the remaining arguments
    goArgs :: [Text]
  }
  deriving stock (Eq, Show)

-- | parse macro arguments with a getopt option string, like rpm. options can
-- appear anywhere and @--@ ends them. an unknown option or a missing value
-- gives 'Left' with the option letter.
getopt :: Text -> [Text] -> Either Char GetoptResult
getopt spec args
  | spec == "-" = Right (GetoptResult [] args)
  | otherwise = go [] [] args
  where
    takesArg c = case T.breakOn (T.singleton c) (T.filter (/= '+') spec) of
      (_, rest) | Just (_, after) <- T.uncons rest -> Just ("::" `T.isPrefixOf` after, ":" `T.isPrefixOf` after)
      _ -> Nothing
    go opts pos = \case
      [] -> Right (GetoptResult (reverse opts) (reverse pos))
      "--" : rest -> Right (GetoptResult (reverse opts) (reverse pos <> rest))
      a : rest
        | Just flags <- T.stripPrefix "-" a, not (T.null flags) -> cluster opts pos (T.unpack flags) rest
        | otherwise -> go opts (a : pos) rest
    cluster opts pos [] rest = go opts pos rest
    cluster opts pos (c : cs) rest = case takesArg c of
      Nothing -> Left c
      Just (optional, required)
        | optional -> cluster ((c, if null cs then Nothing else Just (T.pack cs)) : opts) pos [] rest
        | required, not (null cs) -> go ((c, Just (T.pack cs)) : opts) pos rest
        | required -> case rest of
            v : rest' -> go ((c, Just v) : opts) pos rest'
            [] -> Left c
        | otherwise -> cluster ((c, Nothing) : opts) pos cs rest

-- | uppercase ASCII letters, leaving the rest alone
asciiUpper :: Text -> Text
asciiUpper = T.map (\c -> if isAsciiLower c then toUpper c else c)

-- | lowercase ASCII letters, leaving the rest alone
asciiLower :: Text -> Text
asciiLower = T.map (\c -> if isAsciiUpper c then toLower c else c)
