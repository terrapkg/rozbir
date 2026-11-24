-- | a pure port of Lua 5.4's pattern matching, just enough for @%{gsub:...}@.
-- captures, @%b@, @%f@ and back-references aren't supported, and no Lua is ever
-- run.
module Rozbir.Eval.LuaPattern
  ( luaGsub,
  )
where

import Data.Char (isAlpha, isAlphaNum, isControl, isDigit, isHexDigit, isLower, isPunctuation, isSpace, isSymbol, isUpper)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data Class
  = CAny
  | CChar Char
  | CPred (Char -> Bool)

data Item = Item Class Char

data Pattern = Pattern Bool [Item] Bool

parsePattern :: String -> Either Text Pattern
parsePattern s0 =
  let (anchored, s1) = case s0 of
        '^' : rest -> (True, rest)
        _ -> (False, s0)
   in go anchored [] s1
  where
    go anchored acc = \case
      [] -> Right (Pattern anchored (reverse acc) False)
      ['$'] -> Right (Pattern anchored (reverse acc) True)
      '(' : _ -> Left "Lua captures"
      ')' : _ -> Left "Lua captures"
      '%' : 'b' : _ -> Left "%b"
      '%' : 'f' : _ -> Left "%f"
      '%' : d : _ | isDigit d -> Left "Lua back-references"
      s -> do
        (cls, rest) <- single s
        case rest of
          q : rest' | q `elem` ("*+-?" :: String) -> go anchored (Item cls q : acc) rest'
          _ -> go anchored (Item cls '1' : acc) rest
    single = \case
      '.' : rest -> Right (CAny, rest)
      '%' : c : rest -> (,rest) <$> escape c
      '[' : rest -> set rest
      c : rest -> Right (CChar c, rest)
      [] -> Left "empty pattern item"
    escape c = case luaClass c of
      Just p -> Right (CPred p)
      Nothing
        | isAlphaNum c -> Left "unknown Lua class"
        | otherwise -> Right (CChar c)
    set s = do
      let (neg, s1) = case s of
            '^' : r -> (True, r)
            _ -> (False, s)
      (preds, rest) <- members [] True s1
      let p c = any ($ c) preds
      Right (CPred (if neg then not . p else p), rest)
    members acc first = \case
      [] -> Left "unterminated ["
      ']' : rest | not first -> Right (acc, rest)
      '%' : c : rest -> do
        p <- either (const (Right (== c))) Right (maybe (Left ()) Right (luaClass c))
        members (p : acc) False rest
      a : '-' : b : rest | b /= ']' -> members ((\c -> c >= a && c <= b) : acc) False rest
      c : rest -> members ((== c) : acc) False rest

luaClass :: Char -> Maybe (Char -> Bool)
luaClass c = case c of
  'a' -> Just isAlpha
  'd' -> Just isDigit
  'l' -> Just isLower
  's' -> Just isSpace
  'u' -> Just isUpper
  'w' -> Just isAlphaNum
  'x' -> Just isHexDigit
  'p' -> Just (\x -> isPunctuation x || isSymbol x)
  'c' -> Just isControl
  _
    | isUpper c, Just p <- luaClass (toLowerAscii c) -> Just (not . p)
    | otherwise -> Nothing
  where
    toLowerAscii x = toEnum (fromEnum x + 32)

matchesClass :: Class -> Char -> Bool
matchesClass cls c = case cls of
  CAny -> True
  CChar x -> x == c
  CPred p -> p c

matchHere :: [Item] -> Bool -> String -> Maybe String
matchHere items anchoredEnd s = case items of
  [] -> if anchoredEnd && not (null s) then Nothing else Just s
  Item cls q : rest -> case q of
    '1' -> case s of
      c : cs | matchesClass cls c -> matchHere rest anchoredEnd cs
      _ -> Nothing
    '?' -> case s of
      c : cs | matchesClass cls c, Just r <- matchHere rest anchoredEnd cs -> Just r
      _ -> matchHere rest anchoredEnd s
    '-' -> lazy s
    _ ->
      let n = length (takeWhile (matchesClass cls) s)
          minimum' = if q == '+' then 1 else 0
       in firstJust [matchHere rest anchoredEnd (drop k s) | k <- [n, n - 1 .. minimum']]
    where
      lazy str = case matchHere rest anchoredEnd str of
        Just r -> Just r
        Nothing -> case str of
          c : cs | matchesClass cls c -> lazy cs
          _ -> Nothing
  where
    firstJust = \case
      [] -> Nothing
      Just x : _ -> Just x
      Nothing : xs -> firstJust xs

-- | Lua's @string.gsub@ on @[string, pattern, replacement]@ with an optional
-- count, returning the new string. 'Left' names the unsupported feature or bad
-- argument.
luaGsub :: [Text] -> Either Text Text
luaGsub = \case
  [s, p, r] -> run s p r Nothing
  [s, p, r, n] -> maybe (Left "bad count") (run s p r . Just) (readMaybe (T.unpack n))
  _ -> Left "gsub takes 3 or 4 arguments"
  where
    run s p r limit = do
      Pattern anchored items anchoredEnd <- parsePattern (T.unpack p)
      repl <- replacement (T.unpack r)
      Right (T.pack (loop anchored items anchoredEnd repl limit (0 :: Int) Nothing (T.unpack s)))
    loop anchored items anchoredEnd repl limit n lastEnd src
      | maybe False (n >=) limit = src
      | otherwise = case matchHere items anchoredEnd src of
          Just rest
            | Just (length rest) /= lastEnd ->
                repl (take (length src - length rest) src)
                  <> if anchored then rest else loop anchored items anchoredEnd repl limit (n + 1) (Just (length rest)) rest
          _ -> case src of
            c : cs
              | anchored -> src
              | otherwise -> c : loop anchored items anchoredEnd repl limit n lastEnd cs
            [] -> []
    replacement = go []
      where
        go acc = \case
          [] -> Right (\m -> concatMap ($ m) (reverse acc))
          '%' : c : rest
            | c == '0' || c == '1' -> go (id : acc) rest
            | isDigit c -> Left "Lua captures"
            | otherwise -> go (const [c] : acc) rest
          ['%'] -> Left "trailing %"
          c : rest -> go (const [c] : acc) rest
