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

data Builtin
  = BP
  | BS
  | BBasename
  | BDefine
  | BDirname
  | BDnl
  | BDump
  | BEcho
  | BError
  | BExists
  | BExpand
  | BExprFn
  | BGetconfdir
  | BGetenv
  | BGetncpus
  | BGlobal
  | BGsub
  | BLen
  | BLoad
  | BLower
  | BLua
  | BMacrobody
  | BQuote
  | BRep
  | BReverse
  | BRpmversion
  | BShrink
  | BSpan
  | BSub
  | BSuffix
  | BTrace
  | BU2p
  | BShescape
  | BUncompress
  | BUndefine
  | BUpper
  | BUrl2path
  | BVerbose
  | BWarn
  | BXdg
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

builtinNamed :: Text -> Maybe Builtin
builtinNamed n = snd <$> find ((== n) . fst) builtinTable

builtinNames :: [Text]
builtinNames = map fst builtinTable

data ArgStyle = TakesNoArgs | TakesArgs | TakesOptionalArgs | ParsesRaw
  deriving stock (Eq, Show)

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

closedPrefixes :: [Text]
closedPrefixes = ["with_", "_with_", "_without_", "bcond_override_default_", "SOURCE", "PATCH"]

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

isAutoName :: Text -> Bool
isAutoName n = case T.uncons n of
  Just ('-', _) -> True
  Just (c, _) | isDigit c -> True
  _ -> n `elem` ["*", "**", "#"]

posixBasename :: Text -> Text
posixBasename t
  | T.null t = "."
  | T.all (== '/') t = "/"
  | otherwise = T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') t)

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

shrink :: Text -> Text
shrink = T.unwords . T.words

shescape :: [Text] -> Text
shescape = T.unwords . map (\a -> "'" <> T.replace "'" "'\\''" a <> "'")

suffix :: Text -> Text
suffix t = case T.breakOnEnd "." t of
  ("", _) -> ""
  (_, s) -> s

urlPath :: Text -> Text
urlPath t = case T.breakOn "://" t of
  (_, "") -> nonEmpty t
  (scheme, rest)
    | isUrlScheme scheme -> nonEmpty (T.dropWhile (/= '/') (T.drop 3 rest))
    | otherwise -> nonEmpty t
  where
    nonEmpty p = if T.null p then "/" else p
    isUrlScheme s = s `elem` ["ftp", "http", "https", "hkp", "file"]

luaLen :: Text -> Text
luaLen = T.pack . show . BS.length . encodeUtf8

luaRep :: [Text] -> Maybe Text
luaRep = \case
  [s, n] -> (\k -> T.replicate k s) <$> count n
  [s, n, sep] -> (\k -> T.intercalate sep (replicate k s)) <$> count n
  _ -> Nothing
  where
    count n = max 0 <$> readMaybe (T.unpack n)

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

luaReverse :: Text -> Text
luaReverse = decodeUtf8Lenient . BS.reverse . encodeUtf8

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

data GetoptResult = GetoptResult
  { goOptions :: [(Char, Maybe Text)],
    goArgs :: [Text]
  }
  deriving stock (Eq, Show)

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

asciiUpper :: Text -> Text
asciiUpper = T.map (\c -> if isAsciiLower c then toUpper c else c)

asciiLower :: Text -> Text
asciiLower = T.map (\c -> if isAsciiUpper c then toLower c else c)
