-- | rpm's expression language, used by @%if@, @%[...]@ and @%{expr:...}@.
-- values that aren't known make the result unknown, unless the rest of the
-- expression decides it anyway, like @0 && x@.
module Rozbir.Syntax.Expr
  ( Item (..),
    ExprValue (..),
    itemsFromValue,
    itemsFromFrags,
    evalExpr,
    boolify,
    renderExprValue,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict (StateT, evalStateT, get, gets, lift, modify', put)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Rozbir.Eval.Types
import Rozbir.Syntax (Frag (..), MacroRef)
import Rozbir.Version (compareVersionLiteral, parseVersionLiteral)
import Text.Read (readMaybe)

-- | one unit of expression input
data Item
  = IChar !Char
  | -- | a part of the input that isn't known
    IOpaque !Reason
  | -- | a macro that hasn't been expanded yet
    IMacro !MacroRef

-- | input from an expanded value
itemsFromValue :: Value -> [Item]
itemsFromValue (Value ps) = concatMap piece ps
  where
    piece = \case
      Known t -> map IChar (T.unpack t)
      Opaque r _ -> [IOpaque r]

-- | input from unexpanded text, with macros kept for later
itemsFromFrags :: [Frag] -> [Item]
itemsFromFrags = concatMap $ \case
  Lit t -> map IChar (T.unpack t)
  Escape -> [IChar '%', IChar '%']
  Macro m -> [IMacro m]

-- | the result of an expression
data ExprValue
  = EInt !Integer
  | -- | a string from @\"...\"@
    EStr !Text
  | -- | a version from @v\"...\"@
    EVer !Text
  | -- | a value that isn't known, with why
    EUnknown ![Reason]
  deriving stock (Eq, Show)

-- | turn a result back into text, the way rpm prints it
renderExprValue :: ExprValue -> Value
renderExprValue = \case
  EInt i -> known (T.pack (show i))
  EStr s -> known s
  EVer v -> known v
  EUnknown rs -> Value [Opaque r Nothing | r <- take 1 (rs <> [Failed "unknown"])]

-- | whether a result counts as true. zero and empty strings are false.
boolify :: ExprValue -> Tri
boolify = \case
  EInt i -> if i /= 0 then Yes else No
  EStr s -> if T.null s then No else Yes
  EVer v -> if T.null v then No else Yes
  EUnknown rs -> Unknown rs

data Tok m
  = TInt (m Value)
  | TStr Bool (m Value)
  | TUnknown [Reason]
  | TFunc Text
  | TOp Text
  | TEOF

tokenize :: (Monad m) => Bool -> (MacroRef -> m Value) -> [Item] -> Either Text [Tok m]
tokenize expand expandRef = go
  where
    go items = case dropWhile isSpaceItem items of
      [] -> Right [TEOF]
      IOpaque r : rest -> let (more, rest') = span intItem rest in (TUnknown (r : opaqueIn more) :) <$> go rest'
      IChar 'v' : IChar '"' : rest -> string True rest
      IChar '"' : rest -> string False rest
      items'@(i : _) | startsInt i -> let (tok, rest) = span intItem items' in (TInt (valueOf tok) :) <$> go rest
      IMacro _ : _ -> Left "macros can't be expanded here"
      IChar c : rest
        | isAlpha c -> case span funcChar (IChar c : rest) of
            (name, IChar '(' : rest') -> (TFunc (T.pack [x | IChar x <- name]) :) <$> go rest'
            _ -> Left "bare words are no longer supported, please use \"...\""
        | otherwise -> operator c rest

    isSpaceItem = \case
      IChar c -> isSpace c
      _ -> False
    startsInt = \case
      IChar c -> isDigit c
      IMacro _ -> expand
      _ -> False
    intItem = \case
      IChar c -> isDigit c
      IMacro _ -> expand
      IOpaque _ -> True
    funcChar = \case
      IChar c -> isAlphaNum c || c == '_' || c == '.' || c == ':'
      _ -> False
    opaqueIn xs = [r | IOpaque r <- xs]

    valueOf items = mconcat <$> mapM itemValue items
    itemValue = \case
      IChar c -> pure (known (T.singleton c))
      IOpaque r -> pure (opaque r)
      IMacro m -> expandRef m

    string isVersion rest = case break isQuote rest of
      (inner, _ : rest') -> (TStr isVersion (valueOf inner) :) <$> go rest'
      (_, []) -> Left "unterminated string in expression"
    isQuote = \case
      IChar '"' -> True
      _ -> False

    operator c rest = case (c, rest) of
      ('=', IChar '=' : r) -> (TOp "==" :) <$> go r
      ('!', IChar '=' : r) -> (TOp "!=" :) <$> go r
      ('<', IChar '=' : r) -> (TOp "<=" :) <$> go r
      ('>', IChar '=' : r) -> (TOp ">=" :) <$> go r
      ('&', IChar '&' : r) -> (TOp "&&" :) <$> go r
      ('|', IChar '|' : r) -> (TOp "||" :) <$> go r
      _
        | c `elem` ("+-*/()<>!?:," :: String) -> (TOp (T.singleton c) :) <$> go rest
        | otherwise -> Left ("parse error in expression at '" <> T.singleton c <> "'")

type P m = StateT ([Tok m], Bool) (ExceptT Text m)

-- | evaluate an expression. 'Left' is a syntax or type error, with rpm's message.
-- when the flag is set, macros are expanded with the function only if they are needed, like rpm does for @%[...]@. otherwise a macro in the input is an error.
evalExpr :: (Monad m) => Bool -> (MacroRef -> m Value) -> [Item] -> m (Either Text ExprValue)
evalExpr expand expandRef items = case tokenize expand expandRef items of
  Left e -> pure (Left e)
  Right toks -> runExceptT (evalStateT top (toks, False))
  where
    top = do
      v <- ternary
      peek >>= \case
        TEOF -> pure v
        _ -> throwError "syntax error in expression"

peek :: (Monad m) => P m (Tok m)
peek =
  gets fst >>= \case
    t : _ -> pure t
    [] -> pure TEOF

advance :: (Monad m) => P m ()
advance = modify' (\(ts, d) -> (drop 1 ts, d))

isOp :: Text -> Tok m -> Bool
isOp o = \case
  TOp o' -> o == o'
  _ -> False

discarding :: (Monad m) => Bool -> P m a -> P m a
discarding on act
  | not on = act
  | otherwise = do
      (ts, d) <- get
      put (ts, True)
      r <- act
      modify' (\(ts', _) -> (ts', d))
      pure r

ternary :: (Monad m) => P m ExprValue
ternary = do
  v1 <- logical
  t <- peek
  if not (isOp "?" t)
    then pure v1
    else do
      advance
      let cond = boolify v1
      a <- discarding (cond == No) ternary
      t' <- peek
      unless (isOp ":" t') (throwError "syntax error in expression")
      advance
      b <- discarding (cond == Yes) ternary
      checkTypes a b
      pure $ case cond of
        Yes -> a
        No -> b
        Unknown rs -> if a == b then a else EUnknown rs

logical :: (Monad m) => P m ExprValue
logical = relational >>= loop
  where
    loop v1 = do
      t <- peek
      if
        | isOp "&&" t -> step v1 True
        | isOp "||" t -> step v1 False
        | otherwise -> pure v1
    step v1 isAnd = do
      advance
      let b1 = boolify v1
          short = if isAnd then b1 == No else b1 == Yes
      v2 <- discarding short relational
      checkTypes v1 v2
      loop $ case (b1, isAnd) of
        (No, True) -> v1
        (Yes, True) -> v2
        (Yes, False) -> v1
        (No, False) -> v2
        (Unknown rs, True) -> if boolify v2 == No then v2 else EUnknown rs
        (Unknown rs, False) -> if boolify v2 == Yes then v2 else EUnknown rs

relational :: (Monad m) => P m ExprValue
relational = addSub >>= loop
  where
    ops = ["==", "!=", "<", "<=", ">", ">="]
    loop v1 =
      peek >>= \case
        TOp o | o `elem` ops -> do
          advance
          v2 <- addSub
          checkTypes v1 v2
          loop (compareWith o v1 v2)
        _ -> pure v1
    compareWith o v1 v2 = case ordering v1 v2 of
      Left rs -> EUnknown rs
      Right ord -> EInt (if test o ord then 1 else 0)
    ordering a b = case (a, b) of
      (EUnknown r1, EUnknown r2) -> Left (r1 <> r2)
      (EUnknown r, _) -> Left r
      (_, EUnknown r) -> Left r
      (EInt x, EInt y) -> Right (compare x y)
      (EStr x, EStr y) -> Right (compare x y)
      (EVer x, EVer y) -> case (parseVersionLiteral x, parseVersionLiteral y) of
        (Just vx, Just vy) -> Right (compareVersionLiteral vx vy)
        _ -> Left [Failed "invalid version"]
      _ -> Left [Failed "types must match"]
    test o ord = case o of
      "==" -> ord == EQ
      "!=" -> ord /= EQ
      "<" -> ord == LT
      "<=" -> ord /= GT
      ">" -> ord == GT
      _ -> ord /= LT

addSub :: (Monad m) => P m ExprValue
addSub = mulDiv >>= loop
  where
    loop v1 =
      peek >>= \case
        TOp o | o == "+" || o == "-" -> do
          advance
          v2 <- mulDiv
          checkTypes v1 v2
          r <- case (v1, v2) of
            (EUnknown a, EUnknown b) -> pure (EUnknown (a <> b))
            (EUnknown a, _) -> pure (EUnknown a)
            (_, EUnknown b) -> pure (EUnknown b)
            (EInt x, EInt y) -> pure (EInt (if o == "+" then x + y else x - y))
            (EStr x, EStr y) | o == "+" -> pure (EStr (x <> y))
            (EStr _, _) -> throwError "- not supported for strings"
            _ -> throwError "+ and - not supported for versions"
          loop r
        _ -> pure v1

mulDiv :: (Monad m) => P m ExprValue
mulDiv = primary >>= loop
  where
    loop v1 =
      peek >>= \case
        TOp o | o == "*" || o == "/" -> do
          advance
          v2 <- primary
          checkTypes v1 v2
          d <- gets snd
          r <- case (v1, v2) of
            (EUnknown a, _) -> pure (EUnknown a)
            (_, EUnknown b) -> pure (EUnknown b)
            (EInt x, EInt y)
              | d -> pure v1
              | o == "*" -> pure (EInt (x * y))
              | y == 0 -> throwError "division by zero"
              | otherwise -> pure (EInt (x `quot` y))
            (EVer _, _) -> throwError "* and / not supported for versions"
            _ -> throwError "* and / not supported for strings"
          loop r
        _ -> pure v1

primary :: (Monad m) => P m ExprValue
primary =
  peek >>= \case
    TFunc name -> do
      advance
      args <- funcArgs
      if "lua:" `T.isPrefixOf` name
        then pure (EUnknown (RunsLua : concat [rs | EUnknown rs <- args]))
        else throwError "unsupported function"
    TOp "(" -> do
      advance
      v <- ternary
      t <- peek
      unless (isOp ")" t) (throwError "unmatched (")
      advance
      pure v
    TInt act -> do
      advance
      value act >>= \case
        Left rs -> pure (EUnknown rs)
        Right t -> case wellFormed t of
          Just i -> pure (EInt i)
          Nothing
            | Just (c, _) <- T.uncons t, isAlpha c -> throwError "macro expansion returned a bare word, please use \"...\""
            | otherwise -> throwError "macro expansion did not return an integer"
    TStr isVersion act -> do
      advance
      value act >>= \case
        Left rs -> pure (EUnknown rs)
        Right t
          | isVersion -> do
              d <- gets snd
              when (not d && parseVersionLiteral t == Nothing) (throwError "invalid version")
              pure (EVer t)
          | otherwise -> pure (EStr t)
    TUnknown rs -> advance >> pure (EUnknown rs)
    TOp "-" -> do
      advance
      primary >>= \case
        EInt i -> pure (EInt (negate i))
        EUnknown rs -> pure (EUnknown rs)
        _ -> throwError "- only on numbers"
    TOp "!" -> do
      advance
      v <- primary
      pure $ case boolify v of
        Yes -> EInt 0
        No -> EInt 1
        Unknown rs -> EUnknown rs
    TEOF -> throwError "unexpected end of expression"
    _ -> throwError "syntax error in expression"
  where
    value act = do
      d <- gets snd
      if d
        then pure (Right "0")
        else do
          v <- lift (lift act)
          pure (maybe (Left (reasons v)) Right (knownText (stripQuoteMarks v)))
    wellFormed t
      | T.null t = Just 0
      | Just digits <- T.stripPrefix "-" t, not (T.null digits), T.all isDigit digits = negate <$> readMaybe (T.unpack digits)
      | T.all isDigit t = readMaybe (T.unpack t)
      | otherwise = Nothing
    funcArgs = do
      t <- peek
      if isOp ")" t
        then advance >> pure []
        else collect []
    collect acc = do
      v <- ternary
      t <- peek
      if
        | isOp ")" t -> advance >> pure (reverse (v : acc))
        | isOp "," t -> advance >> collect (v : acc)
        | otherwise -> throwError "syntax error in expression"

checkTypes :: (Monad m) => ExprValue -> ExprValue -> P m ()
checkTypes a b = case (a, b) of
  (EUnknown _, _) -> pure ()
  (_, EUnknown _) -> pure ()
  (EInt _, EInt _) -> pure ()
  (EStr _, EStr _) -> pure ()
  (EVer _, EVer _) -> pure ()
  _ -> throwError "types must match"
