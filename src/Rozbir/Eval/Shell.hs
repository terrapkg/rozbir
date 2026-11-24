-- | a pure interpreter for the small subset of sh that specs use in @%(...)@:
-- variable assignments, @echo@, simple @printf@, @${v:o:l}@-style expansions,
-- here-strings, and pipes into @sed@ substitutions, @tr@ and @cut@. it never
-- runs a shell. anything outside the subset is rejected so the caller can
-- leave it opaque.
module Rozbir.Eval.Shell
  ( runShell,
    Unsupported,
    sedScript,
    trText,
    Regex,
    parseRegex,
    matchLongest,
  )
where

import Control.Monad (foldM, unless, when)
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | why a script was rejected, such as the command or syntax it doesn't support
type Unsupported = Text

data Part
  = PLit !Text Bool
  | PVar !Text !Expansion

data Expansion
  = Plain
  | Substring !Int !(Maybe Int)
  | DropPrefix !Bool !Text
  | DropSuffix !Bool !Text
  | Replace !Bool !Text !Text
  | Length

type Word' = [Part]

data Token = TWord Word' | TPipe | TSep | THere

-- | run a script and return its output with trailing newlines trimmed, like
-- @%(...)@. 'Left' names the first unsupported feature, including reading a
-- variable the script didn't set.
runShell :: Text -> Either Unsupported Text
runShell src = do
  toks <- tokenize (T.unpack src)
  stmts <- statements toks
  (_, out) <- foldM step (Map.empty, "") stmts
  pure (T.dropWhileEnd (\c -> c == '\n' || c == '\r') out)
  where
    step (vars, out) = \case
      Left (name, w) -> do
        v <- expandJoined vars w
        pure (Map.insert name v vars, out)
      Right pipeline -> do
        r <- runPipeline vars pipeline
        pure (vars, out <> r)

type Pipeline = ([[Word']], Maybe Word')

statements :: [Token] -> Either Unsupported [Either (Text, Word') Pipeline]
statements toks = mapM statement (filter (not . null) (splitOn isSep toks))
  where
    isSep = \case
      TSep -> True
      _ -> False
    statement ts = case ts of
      [TWord w] | Just a <- assignment w -> Right (Left a)
      _ -> do
        parsed <- mapM command (splitOn isPipe ts)
        let cmds = map fst parsed
            heres = map snd parsed
        when (any null cmds) (Left "empty command")
        when (any (/= Nothing) (map (fmap (const ())) (drop 1 heres))) (Left "here-strings after a pipe")
        case cmds of
          (w : _) : _ | Just _ <- assignment w -> Left "environment prefixes"
          _ -> pure ()
        pure (Right (cmds, case heres of h : _ -> h; [] -> Nothing))
    isPipe = \case
      TPipe -> True
      _ -> False
    command ts = case break isHere ts of
      (ws, []) -> (,Nothing) <$> mapM word' ws
      (ws, [THere, TWord h]) -> (,Just h) <$> mapM word' ws
      _ -> Left "unsupported here-string"
    isHere = \case
      THere -> True
      _ -> False
    word' = \case
      TWord w -> Right w
      _ -> Left "unexpected operator"

assignment :: Word' -> Maybe (Text, Word')
assignment = \case
  PLit t False : rest
    | (name, eq) <- T.break (== '=') t,
      Just value <- T.stripPrefix "=" eq,
      validName name ->
        Just (name, [PLit value False | not (T.null value)] <> rest)
  _ -> Nothing
  where
    validName n = case T.uncons n of
      Just (c, more) -> (isAlpha c || c == '_') && T.all (\x -> isAlphaNum x || x == '_') more
      Nothing -> False

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p xs = case break p xs of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitOn p rest

tokenize :: String -> Either Unsupported [Token]
tokenize = go []
  where
    go acc = \case
      [] -> Right (reverse acc)
      c : cs
        | c == '\n' || c == ';' -> go (TSep : acc) cs
        | c == '|' -> case cs of
            '|' : _ -> Left "||"
            _ -> go (TPipe : acc) cs
        | c == ' ' || c == '\t' -> go acc cs
        | c == '#' -> go acc (dropWhile (/= '\n') cs)
        | c == '<', '<' : '<' : rest <- cs -> go (THere : acc) rest
        | c `elem` ("&<>()`" :: String) -> Left (T.pack ['\'', c, '\''])
        | otherwise -> do
            (w, rest) <- word [] (c : cs)
            go (TWord (mergeParts w) : acc) rest

    word parts = \case
      [] -> Right (reverse parts, [])
      s@(c : cs)
        | c `elem` (" \t\n;|" :: String) -> Right (reverse parts, s)
        | c `elem` ("&<>()`" :: String) -> Left (T.pack ['\'', c, '\''])
        | c == '\'' -> case break (== '\'') cs of
            (inner, _ : rest) -> word (PLit (T.pack inner) True : parts) rest
            _ -> Left "unterminated quote"
        | c == '"' -> do
            (inner, rest) <- doubleQuoted [] cs
            word (reverse inner <> parts) rest
        | c == '\\' -> case cs of
            '\n' : rest -> word parts rest
            x : rest -> word (PLit (T.singleton x) True : parts) rest
            [] -> Left "trailing backslash"
        | c == '$' -> do
            (v, rest) <- dollar cs
            word (v : parts) rest
        | c `elem` ("*?[{}" :: String) -> Left "globs and braces"
        | c == '~' && null parts -> Left "tilde expansion"
        | otherwise -> word (PLit (T.singleton c) False : parts) cs

    doubleQuoted parts = \case
      [] -> Left "unterminated quote"
      '"' : rest -> Right (reverse parts, rest)
      '\\' : x : rest
        | x `elem` ("$`\"\\" :: String) -> doubleQuoted (PLit (T.singleton x) True : parts) rest
        | x == '\n' -> doubleQuoted parts rest
        | otherwise -> doubleQuoted (PLit (T.pack ['\\', x]) True : parts) rest
      '$' : rest -> do
        (v, rest') <- dollar rest
        doubleQuoted (quoteVar v : parts) rest'
      '`' : _ -> Left "command substitution"
      c : rest -> doubleQuoted (PLit (T.singleton c) True : parts) rest

    quoteVar = \case
      PVar n e -> PVar ("\"" <> n) e
      p -> p

    dollar = \case
      '{' : rest -> case break (== '}') rest of
        (inner, _ : after) -> do
          e <- braced inner
          Right (e, after)
        _ -> Left "unterminated ${"
      '(' : _ -> Left "command substitution"
      s@(c : _)
        | isAlpha c || c == '_' ->
            let (name, rest) = span (\x -> isAlphaNum x || x == '_') s
             in Right (PVar (T.pack name) Plain, rest)
        | isDigit c || c `elem` ("@*#?$!-" :: String) -> Left "special parameters"
      rest -> Right (PLit "$" False, rest)

    braced inner = case inner of
      '#' : name | validVar name -> Right (PVar (T.pack name) Length)
      _ ->
        let (name, op) = span (\x -> isAlphaNum x || x == '_') inner
         in if not (validVar name)
              then Left "unsupported ${...}"
              else PVar (T.pack name) <$> operator op
    validVar n = case n of
      c : more -> (isAlpha c || c == '_') && all (\x -> isAlphaNum x || x == '_') more
      [] -> False
    operator = \case
      [] -> Right Plain
      ':' : spec
        | (offT, rest) <- break (== ':') spec ->
            case (readMaybe offT, rest) of
              (Just o, []) | o >= 0 -> Right (Substring o Nothing)
              (Just o, ':' : lenT) | Just l <- readMaybe lenT, o >= 0, l >= 0 -> Right (Substring o (Just l))
              _ -> Left "unsupported substring"
      '#' : '#' : pat -> literal pat (DropPrefix True)
      '#' : pat -> literal pat (DropPrefix False)
      '%' : '%' : pat -> literal pat (DropSuffix True)
      '%' : pat -> literal pat (DropSuffix False)
      '/' : '/' : spec -> replace True spec
      '/' : spec -> replace False spec
      _ -> Left "unsupported ${...}"
    literal pat f
      | any (`elem` ("*?[\\$'\"" :: String)) pat = Left "glob patterns"
      | otherwise = Right (f (T.pack pat))
    replace allOcc spec = case break (== '/') spec of
      (pat, rest)
        | any (`elem` ("*?[\\$'\"#%" :: String)) pat || null pat -> Left "glob patterns"
        | otherwise ->
            let rep = drop 1 rest
             in if any (`elem` ("\\$'\"" :: String)) rep then Left "unsupported replacement" else Right (Replace allOcc (T.pack pat) (T.pack rep))

mergeParts :: [Part] -> [Part]
mergeParts = \case
  PLit a qa : PLit b qb : rest | qa == qb -> mergeParts (PLit (a <> b) qa : rest)
  x : rest -> x : mergeParts rest
  [] -> []

expandVar :: Map Text Text -> Text -> Expansion -> Either Unsupported Text
expandVar vars name0 e = do
  let name = fromMaybe name0 (T.stripPrefix "\"" name0)
  v <- maybe (Left ("$" <> name <> " comes from the environment")) Right (Map.lookup name vars)
  pure $ case e of
    Plain -> v
    Length -> T.pack (show (T.length v))
    Substring o l -> maybe id T.take l (T.drop o v)
    DropPrefix _ p -> fromMaybe v (T.stripPrefix p v)
    DropSuffix _ p -> fromMaybe v (T.stripSuffix p v)
    Replace True p r -> T.replace p r v
    Replace False p r -> case T.breakOn p v of
      (pre, post) | not (T.null post) -> pre <> r <> T.drop (T.length p) post
      _ -> v

expandJoined :: Map Text Text -> Word' -> Either Unsupported Text
expandJoined vars parts = T.concat <$> mapM one parts
  where
    one = \case
      PLit t _ -> Right t
      PVar n e -> expandVar vars n e

expandFields :: Map Text Text -> Word' -> Either Unsupported [Text]
expandFields vars parts = do
  pieces <- mapM one parts
  let fields = L.foldl' glue [] pieces
  pure (if any quotedPart parts then fields else filter (not . T.null) fields)
  where
    one = \case
      PLit t _ -> Right [Left t]
      PVar n e
        | "\"" `T.isPrefixOf` n -> (\v -> [Left v]) <$> expandVar vars n e
        | otherwise -> do
            v <- expandVar vars n e
            when (T.any (`elem` ("*?[" :: String)) v) (Left "glob characters in an expansion")
            Right [Right v]
    quotedPart = \case
      PLit _ True -> True
      PVar n _ -> "\"" `T.isPrefixOf` n
      _ -> False
    glue acc = \case
      [] -> acc
      Left t : more -> glue (appendLast acc t) more
      Right v : more ->
        let ws = T.split isIfs v
         in case ws of
              [] -> glue acc more
              first : others -> glue (appendSplit (appendLast acc first) others) more
    isIfs c = c == ' ' || c == '\t' || c == '\n'
    appendLast acc t = case acc of
      [] -> [t]
      _ -> init acc <> [last acc <> t]
    appendSplit acc others = acc <> others

runPipeline :: Map Text Text -> Pipeline -> Either Unsupported Text
runPipeline vars (cmds, here) = do
  argvs <- mapM (fmap concat . mapM (expandFields vars)) cmds
  start <- traverse (fmap (<> "\n") . expandJoined vars) here
  foldM (\input (i, argv) -> runCommand (i == (0 :: Int) && start == Nothing) input argv) (fromMaybe "" start) (zip [0 ..] argvs)

runCommand :: Bool -> Text -> [Text] -> Either Unsupported Text
runCommand first input = \case
  "echo" : args -> echo args
  "printf" : fmt : args -> printf fmt args
  "sed" : args -> needsInput >> sedArgs args >>= \(ere, scripts) -> sedRun ere scripts input
  "tr" : args -> needsInput >> trArgs args input
  "cut" : args -> needsInput >> cutArgs args input
  cmd : _ -> Left (cmd <> " isn't supported")
  [] -> Left "empty command"
  where
    needsInput = when first (Left "reads stdin")
    echo args = case args of
      "-n" : rest -> Right (T.unwords rest)
      "-E" : rest -> echo rest
      a : _ | "-" `T.isPrefixOf` a, T.length a > 1, T.all (`elem` ['n', 'e', 'E']) (T.drop 1 a) -> Left "echo options"
      _ -> Right (T.unwords args <> "\n")
    printf fmt args
      | fmt == "%s" = Right (T.concat args)
      | fmt == "%s\\n" = Right (T.concat (map (<> "\n") args))
      | not (T.any (`elem` ['%', '\\']) fmt) && null args = Right fmt
      | otherwise = Left "printf formats"

sedArgs :: [Text] -> Either Unsupported (Bool, [Text])
sedArgs = go False []
  where
    go ere scripts = \case
      [] -> if null scripts then Left "sed without a script" else Right (ere, reverse scripts)
      "-E" : rest -> go True scripts rest
      "-r" : rest -> go True scripts rest
      "-e" : s : rest -> go ere (s : scripts) rest
      a : rest
        | "-" `T.isPrefixOf` a -> Left ("sed " <> a)
        | null scripts -> go ere [a] rest
        | otherwise -> Left "sed with files"

sedRun :: Bool -> [Text] -> Text -> Either Unsupported Text
sedRun ere scripts input = do
  cmds <- concat <$> mapM (sedScript ere) scripts
  let ls = T.splitOn "\n" input
      (body, trailing) = if not (null ls) && T.null (last ls) then (init ls, True) else (ls, False)
  out <- mapM (\l -> foldM (\acc c -> c acc) l cmds) body
  pure (T.intercalate "\n" out <> if trailing then "\n" else "")

-- | parse a sed script into one function per command, each applied to a line.
-- only @s@ commands with an optional @g@ flag are supported. 'True' means
-- extended regexes, like @sed -E@.
sedScript :: Bool -> Text -> Either Unsupported [Text -> Either Unsupported Text]
sedScript ere = go . T.unpack
  where
    go = \case
      [] -> Right []
      c : rest | c == ';' || isSpace c -> go rest
      's' : d : rest | d /= '\\' && d /= '\n' -> do
        (pat, r1) <- part d rest
        (rep, r2) <- part d r1
        let (flags, r3) = span (\x -> x /= ';' && x /= '\n' && x /= '}' && not (isSpace x)) r2
        unless (all (== 'g') flags) (Left ("sed flags " <> T.pack flags))
        re <- parseRegex ere (T.pack (unescapeDelim d pat))
        repl <- replacement (unescapeDelim d rep)
        more <- go r3
        pure (substitute re repl (not (null flags)) : more)
      _ -> Left "only s commands are supported in sed"
    part d = scan []
      where
        scan acc = \case
          [] -> Left "unterminated s command"
          '\\' : x : rest -> scan (x : '\\' : acc) rest
          x : rest
            | x == d -> Right (reverse acc, rest)
            | otherwise -> scan (x : acc) rest
    unescapeDelim d = \case
      '\\' : x : rest | x == d && d /= '&' -> x : unescapeDelim d rest
      '\\' : x : rest -> '\\' : x : unescapeDelim d rest
      x : rest -> x : unescapeDelim d rest
      [] -> []

data ReplPart = RLit Text | RMatch

replacement :: String -> Either Unsupported [ReplPart]
replacement = go
  where
    go = \case
      [] -> Right []
      '&' : rest -> (RMatch :) <$> go rest
      '\\' : x : rest
        | isDigit x -> Left "back-references"
        | x == 'n' -> (RLit "\n" :) <$> go rest
        | x == 't' -> (RLit "\t" :) <$> go rest
        | otherwise -> (RLit (T.singleton x) :) <$> go rest
      x : rest -> (RLit (T.singleton x) :) <$> go rest

substitute :: Regex -> [ReplPart] -> Bool -> Text -> Either Unsupported Text
substitute re repl global line = Right (T.pack (go True (T.unpack line)))
  where
    go atStart s = case findMatch re atStart s of
      Nothing -> s
      Just (pre, matched, post) ->
        let replaced = pre <> concatMap (\case RLit t -> T.unpack t; RMatch -> matched) repl
         in if not global
              then replaced <> post
              else
                if null matched
                  then case post of
                    [] -> replaced
                    x : xs -> replaced <> [x] <> go False xs
                  else replaced <> go False post

data Atom
  = AChar Char
  | AAny
  | ASet Bool [Char -> Bool]

data Piece = Piece Atom Int (Maybe Int)

-- | a parsed sed regex. only single characters, @.@, bracket expressions,
-- quantifiers and anchors are supported, without groups, alternation,
-- intervals or back-references.
data Regex = Regex Bool [Piece] Bool

-- | parse a basic regex, or an extended one when the flag is 'True'
parseRegex :: Bool -> Text -> Either Unsupported Regex
parseRegex ere t0 = do
  let s0 = T.unpack t0
      (anchoredStart, s1) = case s0 of
        '^' : rest -> (True, rest)
        _ -> (False, s0)
  (pieces, anchoredEnd) <- go [] s1
  pure (Regex anchoredStart (reverse pieces) anchoredEnd)
  where
    go acc = \case
      [] -> Right (acc, False)
      ['$'] -> Right (acc, True)
      '\\' : x : rest
        | not ere && x == '+' -> quantify acc 1 Nothing rest
        | not ere && x == '?' -> quantify acc 0 (Just 1) rest
        | x `elem` ("(){}|<>bBwWsS'`" :: String) || isDigit x -> Left "regex groups, intervals and classes"
        | x == 'n' -> go (Piece (AChar '\n') 1 (Just 1) : acc) rest
        | x == 't' -> go (Piece (AChar '\t') 1 (Just 1) : acc) rest
        | otherwise -> go (Piece (AChar x) 1 (Just 1) : acc) rest
      '*' : rest
        | null acc -> go (Piece (AChar '*') 1 (Just 1) : acc) rest
        | otherwise -> quantify acc 0 Nothing rest
      '+' : rest | ere -> if null acc then Left "dangling +" else quantify acc 1 Nothing rest
      '?' : rest | ere -> if null acc then Left "dangling ?" else quantify acc 0 (Just 1) rest
      c : _ | ere && c `elem` ("(){}|" :: String) -> Left "regex groups and alternation"
      '.' : rest -> go (Piece AAny 1 (Just 1) : acc) rest
      '[' : rest -> do
        (atom, rest') <- bracket rest
        go (Piece atom 1 (Just 1) : acc) rest'
      '^' : _ -> Left "^ in the middle"
      '$' : rest -> go (Piece (AChar '$') 1 (Just 1) : acc) rest
      c : rest -> go (Piece (AChar c) 1 (Just 1) : acc) rest
    quantify acc lo hi rest = case acc of
      Piece a 1 (Just 1) : more -> go (Piece a lo hi : more) rest
      []
        | not ere, lo == 1 -> go [Piece (AChar '+') 1 (Just 1)] rest
        | not ere -> go [Piece (AChar '?') 1 (Just 1)] rest
      _ -> Left "stacked quantifiers"
    bracket s = do
      let (negated, s1) = case s of
            '^' : r -> (True, r)
            _ -> (False, s)
          (firstLit, s2) = case s1 of
            ']' : r -> ([(== ']')], r)
            _ -> ([], s1)
      (preds, rest) <- members firstLit s2
      Right (ASet negated preds, rest)
    members acc = \case
      [] -> Left "unterminated ["
      ']' : rest -> Right (acc, rest)
      '[' : ':' : rest -> case break (== ':') rest of
        (cls, ':' : ']' : after) -> do
          p <- charClass cls
          members (p : acc) after
        _ -> Left "unsupported bracket expression"
      '[' : x : _ | x == '=' || x == '.' -> Left "unsupported bracket expression"
      a : '-' : b : rest | b /= ']' -> members ((\c -> c >= a && c <= b) : acc) rest
      c : rest -> members ((== c) : acc) rest
    charClass = \case
      "alpha" -> Right isAlpha
      "digit" -> Right isDigit
      "alnum" -> Right isAlphaNum
      "space" -> Right isSpace
      "upper" -> Right isUpper
      "lower" -> Right isLower
      "punct" -> Right (\c -> c `elem` ("!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~" :: String))
      other -> Left ("[:" <> T.pack other <> ":]")

atomMatches :: Atom -> Char -> Bool
atomMatches a c = case a of
  AChar x -> x == c
  AAny -> c /= '\n'
  ASet neg ps -> (any ($ c) ps) /= neg

matchEnds :: [Piece] -> Bool -> String -> [Int]
matchEnds pieces anchoredEnd = go pieces 0
  where
    go [] n s = [n | not anchoredEnd || null s]
    go (Piece a lo hi : rest) n s =
      let available = length (takeWhile (atomMatches a) (maybe id take hi s))
          counts = [k | k <- [available, available - 1 .. lo], k >= lo]
       in concat [go rest (n + k) (drop k s) | k <- counts]

-- | the length of the longest match at the start of the string, or 'Nothing'.
-- this ignores a @^@ anchor.
matchLongest :: Regex -> String -> Maybe Int
matchLongest (Regex _ pieces anchoredEnd) s = case matchEnds pieces anchoredEnd s of
  [] -> Nothing
  ends -> Just (maximum ends)

findMatch :: Regex -> Bool -> String -> Maybe (String, String, String)
findMatch re@(Regex anchoredStart _ _) atStart = go []
  where
    go pre s
      | anchoredStart && not (atStart && null pre) = Nothing
      | Just n <- matchLongest re s = Just (reverse pre, take n s, drop n s)
      | otherwise = case s of
          [] -> Nothing
          x : xs -> go (x : pre) xs

trArgs :: [Text] -> Text -> Either Unsupported Text
trArgs args input = case args of
  ["-d", set] -> do
    del <- trSet set
    Right (T.filter (`notElem` del) input)
  [a, b] | not ("-" `T.isPrefixOf` a) -> trText a b input
  _ -> Left "tr options"

-- | @tr a b@ on some input. like GNU tr, a short second set repeats its last
-- character.
trText :: Text -> Text -> Text -> Either Unsupported Text
trText a b input = do
  from <- trSet a
  to <- trSet b
  when (null to && not (null from)) (Left "tr with an empty second set")
  let padded = to <> repeat (last to)
      table = Map.fromList (reverse (zip from padded))
  Right (T.map (\c -> Map.findWithDefault c c table) input)

trSet :: Text -> Either Unsupported [Char]
trSet = go . T.unpack
  where
    go = \case
      [] -> Right []
      '[' : ':' : rest | (cls, ':' : ']' : after) <- break (== ':') rest -> do
        cs <- case cls of
          "digit" -> Right ['0' .. '9']
          "lower" -> Right ['a' .. 'z']
          "upper" -> Right ['A' .. 'Z']
          "space" -> Right " \t\n\r\f\v"
          _ -> Left "tr classes"
        (cs <>) <$> go after
      '\\' : x : rest -> (escape x :) <$> go rest
      a : '-' : b : rest | b /= '\\' -> ([a .. b] <>) <$> go rest
      c : rest -> (c :) <$> go rest
    escape = \case
      'n' -> '\n'
      't' -> '\t'
      'r' -> '\r'
      x -> x

cutArgs :: [Text] -> Text -> Either Unsupported Text
cutArgs args input = do
  (delim, list, fields) <- parse' args Nothing Nothing False
  ranges <- mapM range (T.splitOn "," list)
  let ls = T.splitOn "\n" input
      (body, trailing) = if not (null ls) && T.null (last ls) then (init ls, True) else (ls, False)
      pick n = any (\(lo, hi) -> n >= lo && maybe True (n <=) hi) ranges
      d = fromMaybe "\t" delim
      perLine l
        | not fields = T.pack [c | (i, c) <- zip [1 :: Int ..] (T.unpack l), pick i]
        | not (T.isInfixOf d l) = l
        | otherwise = T.intercalate d [f | (i, f) <- zip [1 ..] (T.splitOn d l), pick i]
  Right (T.intercalate "\n" (map perLine body) <> if trailing then "\n" else "")
  where
    parse' as delim list fields = case as of
      [] -> maybe (Left "cut without a list") (\l -> Right (delim, l, fields)) list
      "-d" : d : rest | T.length d == 1 -> parse' rest (Just d) list fields
      "-c" : l : rest -> parse' rest delim (Just l) False
      "-f" : l : rest -> parse' rest delim (Just l) True
      a : rest
        | Just d <- T.stripPrefix "-d" a, T.length d == 1 -> parse' rest (Just d) list fields
        | Just l <- T.stripPrefix "-c" a, not (T.null l) -> parse' rest delim (Just l) False
        | Just l <- T.stripPrefix "-f" a, not (T.null l) -> parse' rest delim (Just l) True
        | otherwise -> Left "cut options"
    range r = case T.splitOn "-" r of
      [n] | Just k <- readMaybe (T.unpack n) -> Right (k, Just k)
      ["", m] | Just k <- readMaybe (T.unpack m) -> Right (1, Just k)
      [n, ""] | Just k <- readMaybe (T.unpack n) -> Right (k, Nothing)
      [n, m] | Just a <- readMaybe (T.unpack n), Just b <- readMaybe (T.unpack m) -> Right (a, Just b)
      _ -> Left "cut ranges"
