-- | partial evaluation of rpm macros and conditionals, following rpm's own
-- rules. it never runs a shell or Lua. anything that would need to becomes an
-- 'Opaque' hole, and conditions that can't be decided run every branch and
-- merge the results.
module Rozbir.Eval
  ( module Rozbir.Eval.Types,
    Eval,
    EvalState (..),
    Lookup (..),
    runEval,
    initialState,
    newEnv,
    withTarget,
    withMacroFile,
    expandText,
    expandFrags,
    expandValue,
    expandAt,
    evalCond,
    lookupMacro,
    isDefined,
    pushMacro,
    popMacro,
    defineAuto,
    message,
    atSpan,
    currentSpan,
    getEnv,
    putEnv,
    mergeEnvs,
    maxDepth,
    freeField,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.State.Strict (State, get, gets, modify', runState)
import Data.Char (isSpace)
import Data.List qualified as L
import Data.Map.Merge.Strict qualified as Merge
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Rozbir.Eval.Builtin
import Rozbir.Eval.LuaPattern (luaGsub)
import Rozbir.Eval.Shell (runShell)
import Rozbir.Eval.Types
import Rozbir.Syntax
import Rozbir.Syntax.Expr (boolify, evalExpr, itemsFromFrags, itemsFromValue, renderExprValue)
import Rozbir.Syntax.Parse (isBlankChar, isNameChar, matchClose, parseFrags)
import Rozbir.Syntax.Render (renderFrags, renderMacro)

-- | evaluator state
data EvalState = EvalState
  { esEnv :: !Env,
    esLevel :: !Int,
    -- | lookups left before giving up with 'DepthLimit'
    esFuel :: !Int,
    -- | messages so far, newest first
    esMessages :: ![EvalMessage],
    esSpan :: !(Maybe Span),
    -- | undefined macros used in a closed env, newest first
    esUndefined :: ![(Name, Span)]
  }
  deriving stock (Show)

-- | the evaluation monad. it's pure, with no IO.
type Eval = State EvalState

-- | the result of looking up a macro name
data Lookup
  = Found MacroDef
  | -- | a builtin, which wins over definitions
    IsBuiltin Builtin
  | -- | definitely not defined
    NotFound
  | -- | might be defined
    Unsure Reason
  deriving stock (Show)

-- | the nesting limit, the same as rpm's
maxDepth :: Int
maxDepth = 64

-- | a fresh state for an environment
initialState :: Env -> EvalState
initialState env = EvalState env 0 2000000 [] Nothing []

-- | run an evaluation
runEval :: EvalState -> Eval a -> (a, EvalState)
runEval st act = runState act st

-- | an environment with only rpm's built-in macros. 'True' makes it closed,
-- meaning system macros are already loaded.
newEnv :: Bool -> Env
newEnv closed = L.foldl' build (L.foldl' add (Env Map.empty closed) preludeMacros) buildTimeMacros
  where
    add env (name, opts, body) = pushDef name (MacroDef opts (BodyRaw body) OriginPrelude 0 False) env
    build env (name, body) = pushDef name (MacroDef Nothing body OriginPrelude 0 False) env

buildTimeMacros :: [(Name, MacroBody)]
buildTimeMacros =
  [ ("buildroot", BodyOpaque (HostDependent "the buildroot")),
    ("builddir", BodyOpaque (HostDependent "the build directory")),
    ("buildsubdir", BodyOpaque (HostDependent "the directory %setup made")),
    ("specpartsdir", BodyOpaque (HostDependent "the build directory")),
    ("_top_builddir", BodyOpaque (HostDependent "the build directory")),
    ("_docdir", BodyRaw "%{_defaultdocdir}"),
    ("_licensedir", BodyRaw "%{_defaultlicensedir}")
  ]

pushDef :: Name -> MacroDef -> Env -> Env
pushDef name def env = env {envMacros = Map.insertWith (<>) name [def] (envMacros env)}

-- | apply a target. its macros replace any existing definitions.
withTarget :: Target -> Env -> Env
withTarget Target {..} env0 = L.foldl' apply env0 (archMacros <> osMacros <> targetMacros)
  where
    archMacros = maybe [] (\a -> [("_target_cpu", Just a), ("_arch", Just a), ("_build_cpu", Just a)]) targetArch
    osMacros = maybe [] (\o -> [("_target_os", Just o), ("_os", Just o), ("_build_os", Just o)]) targetOs
    apply env (name, value) = env {envMacros = Map.insert name [MacroDef Nothing (maybe BodyUndefined BodyRaw value) OriginTarget 0 False] (envMacros env)}

-- | add definitions from a macro file, as name, options, body, file and line
withMacroFile :: [(Name, Maybe Text, Text, FilePath, Int)] -> Env -> Env
withMacroFile defs env0 = L.foldl' (\env (n, o, b, f, l) -> pushDef n (MacroDef o (BodyRaw (macroFileBody b)) (OriginFile f l) 0 False) env) env0 defs

macroFileBody :: Text -> Text
macroFileBody b = case T.uncons b of
  Just ('{', _) | Just (inner, _) <- matchClose '{' '}' b -> inner
  _ -> fst (freeField b)

-- | the current environment
getEnv :: Eval Env
getEnv = gets esEnv

-- | replace the environment
putEnv :: Env -> Eval ()
putEnv env = modify' (\s -> s {esEnv = env})

-- | the source location being evaluated
currentSpan :: Eval (Maybe Span)
currentSpan = gets esSpan

-- | run with a source location, restoring the old one after
atSpan :: Span -> Eval a -> Eval a
atSpan sp act = do
  old <- gets esSpan
  modify' (\s -> s {esSpan = Just sp})
  r <- act
  modify' (\s -> s {esSpan = old})
  pure r

-- | record a message at the current location
message :: Severity -> Text -> Eval ()
message sev txt = do
  sp <- gets esSpan
  modify' (\s -> s {esMessages = EvalMessage sev sp txt : esMessages s})

-- | look up a name. in an open env, a missing name is 'Unsure' unless only the
-- spec could define it, like tags and macro arguments.
lookupMacro :: Name -> Eval Lookup
lookupMacro name
  | Just b <- builtinNamed name = pure (IsBuiltin b)
  | otherwise = do
      EvalState {esEnv = Env macros closed, esLevel = lvl} <- get
      pure $ case Map.lookup name macros of
        Just (m : _)
          | macroAuto m && lvl > macroLevel m -> NotFound
          | otherwise -> case macroBody m of
              BodyUndefined -> NotFound
              BodyMaybe r -> Unsure r
              _ -> Found m
        _
          | closed || isAutoName name || name `elem` closedNames || isKeyword name || any (`T.isPrefixOf` name) closedPrefixes -> NotFound
          | otherwise -> Unsure (Undefined name)

isKeyword :: Name -> Bool
isKeyword n =
  T.toLower n `elem` map fst sectionKinds
    || n `elem` ["if", "ifarch", "ifnarch", "ifos", "ifnos", "elif", "elifarch", "elifos", "else", "endif", "include"]
    || n `elem` ["doc", "docdir", "dir", "ghost", "config", "lang", "attr", "defattr", "exclude", "caps", "verify", "readme", "artifact", "pubkey", "missingok"]

-- | whether a name is defined
isDefined :: Name -> Eval Tri
isDefined name =
  lookupMacro name >>= \case
    Found _ -> pure Yes
    IsBuiltin _ -> pure Yes
    NotFound -> pure No
    Unsure r -> pure (Unknown [r])

-- | add a definition, like @%define@
pushMacro :: Name -> MacroDef -> Eval ()
pushMacro name def = modify' (\s -> s {esEnv = pushDef name def (esEnv s)})

-- | remove the newest definition
popMacro :: Name -> Eval ()
popMacro name = modify' $ \s ->
  let env = esEnv s
   in s {esEnv = env {envMacros = Map.update (\case _ : rest | not (null rest) -> Just rest; _ -> Nothing) name (envMacros env)}}

-- | define a macro with known text at the current depth
defineAuto :: Name -> Text -> Eval ()
defineAuto name body = do
  lvl <- gets esLevel
  sp <- gets esSpan
  pushMacro name (MacroDef Nothing (BodyValue (known body)) (maybe OriginAuto OriginTag sp) lvl False)

-- | merge the environments of branches that couldn't be decided. a macro they
-- disagree on becomes 'Ambiguous', and one only some define becomes maybe
-- defined.
mergeEnvs :: Env -> Env -> Env
mergeEnvs a b =
  Env
    ( Merge.merge
        (Merge.mapMissing (\k s -> conflict k s []))
        (Merge.mapMissing (\k s -> conflict k [] s))
        (Merge.zipWithMatched (\k s1 s2 -> if s1 == s2 then s1 else conflict k s1 s2))
        (envMacros a)
        (envMacros b)
    )
    (envClosed a && envClosed b)
  where
    conflict k s1 s2 =
      let top1 = defined s1
          top2 = defined s2
          rest = drop 1 (if length s1 >= length s2 then s1 else s2)
          template = case s1 <> s2 of
            t : _ -> t
            [] -> MacroDef Nothing BodyUndefined OriginMerged 0 False
       in case (top1, top2) of
            (Just m1, Just m2)
              | macroOpts m1 == macroOpts m2 -> m1 {macroBody = BodyOpaque (Ambiguous k), macroOrigin = OriginMerged} : rest
            _ -> template {macroBody = BodyMaybe (Ambiguous k), macroOrigin = OriginMerged} : rest
    defined = \case
      m : _ | not (isUndefined (macroBody m)) -> Just m
      _ -> Nothing
    isUndefined = \case
      BodyUndefined -> True
      BodyMaybe _ -> True
      _ -> False

luaDefines :: Text -> Eval ()
luaDefines src = mapM_ maybeDefined (luaDefinedNames src)
  where
    maybeDefined n = do
      lvl <- gets esLevel
      pushMacro n (MacroDef Nothing (BodyMaybe RunsLua) OriginAuto lvl False)

luaDefinedNames :: Text -> [Name]
luaDefinedNames src = filter (not . T.null) (defines <> assigns <> indexed)
  where
    defines = [T.takeWhile isNameChar (T.dropWhile (`elem` ['"', '\'', ' ', '(']) (T.drop 10 rest)) | (_, rest) <- T.breakOnAll "rpm.define" src]
    assigns = [n | (_, rest) <- T.breakOnAll "macros." src, let n = T.takeWhile isNameChar (T.drop 7 rest), "=" `T.isPrefixOf` T.stripStart (T.drop (7 + T.length n) rest), not ("==" `T.isPrefixOf` T.stripStart (T.drop (7 + T.length n) rest))]
    indexed = [T.takeWhile isNameChar (T.drop 1 q) | (_, rest) <- T.breakOnAll "macros[" src, let q = T.dropWhile (== ' ') (T.drop 7 rest), Just (c, _) <- [T.uncons q], c == '"' || c == '\'']

noteUndefined :: Int -> Name -> Eval ()
noteUndefined d name = do
  st <- get
  case esSpan st of
    Just sp
      | d == 0,
        envClosed (esEnv st),
        not (isKeyword name),
        not (isAutoName name),
        T.length name > 1,
        name `notElem` ["setup", "patch", "license"] ->
          modify' (\s -> s {esUndefined = (name, sp) : esUndefined s})
    _ -> pure ()

uncertain :: [Reason] -> Eval Value -> Eval Value
uncertain rs act = do
  before <- gets esEnv
  v <- act
  after <- gets esEnv
  putEnv (mergeEnvs before after)
  pure $
    if valueNull v
      then mempty
      else Value [Opaque r Nothing | r <- take 1 (rs <> [Failed "unknown"])]

spend :: Eval Bool
spend = do
  fuel <- gets esFuel
  if fuel <= 0
    then pure False
    else modify' (\s -> s {esFuel = fuel - 1}) >> pure True

-- | expand text, like @rpm --eval@
expandText :: Text -> Eval Value
expandText t = expandAt 0 (parseFrags t)

-- | expand fragments at a depth, attaching the current location to holes
expandAt :: Int -> [Frag] -> Eval Value
expandAt d fs = do
  sp <- gets esSpan
  v <- expandFrags d fs
  pure (attachSpan sp (stripQuoteMarks v))

attachSpan :: Maybe Span -> Value -> Value
attachSpan sp (Value ps) = Value [case p of Opaque r Nothing -> Opaque r sp; _ -> p | p <- ps]

-- | expand fragments at a depth. an undefined macro in a closed env stays as
-- literal text, like rpm.
expandFrags :: Int -> [Frag] -> Eval Value
expandFrags d = go
  where
    go = \case
      [] -> pure mempty
      Lit t : rest -> (known t <>) <$> go rest
      Escape : rest -> (known "%" <>) <$> go rest
      Macro m : rest -> do
        (v, rest') <- expandMacro d m rest
        (v <>) <$> go rest'

-- | expand a value's known text again, like @%{expand:...}@. if a hole might
-- split a macro, the result is just that hole.
expandValue :: Int -> Value -> Eval Value
expandValue d v@(Value ps)
  | not (isKnown v) && any splitsMacro ps = pure (Value [o | o@(Opaque {}) <- take 1 [x | x@(Opaque {}) <- ps]])
  | otherwise = mconcat <$> mapM piece ps
  where
    piece = \case
      Known t -> expandFrags d (parseFrags t)
      o -> pure (Value [o])
    splitsMacro = \case
      Known t -> any unterminated (parseFrags t)
      _ -> False
    unterminated = \case
      Macro (Unterminated _ _) -> True
      _ -> False

expandMacro :: Int -> MacroRef -> [Frag] -> Eval (Value, [Frag])
expandMacro d m rest
  | d > maxDepth = pure (opaque DepthLimit, rest)
  | otherwise = case m of
      Shell fs -> do
        script <- expandFrags (d + 1) fs
        pure $ case knownText (stripQuoteMarks script) >>= either (const Nothing) Just . runShell of
          Just out -> (known out, rest)
          Nothing -> (opaque RunsShell, rest)
      Lua src -> do
        luaDefines src
        pure (opaque RunsLua, rest)
      Unterminated c _ -> do
        message Error ("unterminated %" <> T.singleton c)
        pure (known (renderMacro m), rest)
      Expr fs -> (,rest) <$> expression d True fs
      Bare flags name -> reference d m flags name Nothing True rest
      Braced flags name arg -> reference d m flags name arg False rest

expression :: Int -> Bool -> [Frag] -> Eval Value
expression d expandMode fs = do
  r <- evalExpr expandMode (\ref -> fst <$> expandMacro (d + 1) ref []) (itemsFromFrags fs)
  case r of
    Left e -> do
      message Error ("bad expression: " <> e)
      pure (opaque (Failed e))
    Right v -> pure (renderExprValue v)

reference :: Int -> MacroRef -> Text -> Name -> Maybe MacroArg -> Bool -> [Frag] -> Eval (Value, [Frag])
reference d m flags name arg isBare rest
  | T.null name = pure (known (renderMacro m), rest)
  | otherwise = do
      ok <- spend
      if not ok
        then pure (opaque DepthLimit, rest)
        else do
          look <- lookupMacro name
          let negated = odd (T.count "!" flags)
              checks = T.any (== '?') flags || "-" `T.isPrefixOf` name
          if checks
            then do
              let exists = case look of
                    Found _ -> Yes
                    IsBuiltin _ -> Yes
                    NotFound -> No
                    Unsure r -> Unknown [r]
                  wanted = if negated then triNot exists else exists
                  action = case (arg, look) of
                    (Just (ColonArg fs), _) -> expandFrags (d + 1) fs
                    (_, Found def) -> callMacro d name def Nothing
                    (_, IsBuiltin b) -> fst <$> builtin d b NoArgs []
                    (_, Unsure r) | not negated -> pure (opaque r)
                    _ -> pure mempty
              v <- case wanted of
                No -> pure mempty
                Yes -> action
                Unknown rs -> uncertain rs action
              pure (v, rest)
            else case look of
              NotFound -> do
                noteUndefined d name
                pure (known (renderMacro m), rest)
              Unsure r -> pure (opaque r, rest)
              Found def
                | macroOpts def == Nothing -> (,rest) <$> callMacro d name def Nothing
                | otherwise -> do
                    (args, rest') <- userArgs
                    v <- callMacro d name def (Just args)
                    pure (v, rest')
              IsBuiltin b -> builtinCall b
  where
    userArgs = case arg of
      Just (ColonArg fs) -> (\v -> ([v], rest)) <$> expandFrags (d + 1) fs
      Just (SpaceArg fs) -> (,rest) . splitArgs <$> expandFrags (d + 1) fs
      Nothing
        | isBare,
          followedByBlank rest -> do
            let (lineFrags, rest') = takeLine rest
            v <- expandFrags (d + 1) lineFrags
            pure (splitArgs v, rest')
        | otherwise -> pure ([], rest)

    builtinCall b = case builtinArgStyle b of
      ParsesRaw -> case arg of
        Just (ColonArg fs) -> builtin d b (RawWhole (renderFrags fs)) rest
        Just (SpaceArg fs) -> builtin d b (RawWhole (T.drop 1 (renderFrags fs))) rest
        Nothing
          | isBare -> builtin d b (RawRest (renderFrags rest)) []
          | otherwise -> builtin d b NoArgs rest
      TakesNoArgs -> builtin d b NoArgs rest
      _ -> do
        (args, rest') <- case arg of
          Just (ColonArg fs) -> (\v -> (Args [v], rest)) <$> expandFrags (d + 1) fs
          Just (SpaceArg fs) -> (,rest) . Args . splitArgs <$> expandFrags (d + 1) fs
          Nothing
            | isBare,
              followedByBlank rest -> do
                let (lineFrags, rest') = takeLine rest
                v <- expandFrags (d + 1) lineFrags
                pure (Args (splitArgs v), rest')
            | otherwise -> pure (NoArgs, rest)
        builtin d b args rest'

followedByBlank :: [Frag] -> Bool
followedByBlank = \case
  Lit t : _ | Just (c, _) <- T.uncons t -> isBlankChar c
  _ -> False

takeLine :: [Frag] -> ([Frag], [Frag])
takeLine = go []
  where
    go acc = \case
      [] -> (reverse acc, [])
      Lit t : rest
        | (pre, post) <- T.break (== '\n') t,
          not (T.null post) ->
            let continued = "\\" `T.isSuffixOf` pre
                remaining = if continued then T.drop 1 post else post
             in (reverse (Lit pre : acc), [Lit remaining | not (T.null remaining)] <> rest)
      f : rest -> go (f : acc) rest

splitArgs :: Value -> [Value]
splitArgs v = case knownText v of
  Just t -> map known (splitQuoted t)
  Nothing -> valueWords (stripQuoteMarks v)

callMacro :: Int -> Name -> MacroDef -> Maybe [Value] -> Eval Value
callMacro d name def margs = case macroBody def of
  BodyLiteral t -> pure (known t)
  BodyOpaque r -> pure (opaque r)
  BodyMaybe r -> pure (opaque r)
  BodyUndefined -> pure (known ("%" <> name))
  BodyRaw t -> withArgs (expandFrags (d + 1) (parseFrags t))
  BodyValue v -> withArgs (expandValue (d + 1) v)
  where
    withArgs body = case margs of
      Nothing -> body
      Just args -> do
        lvl <- (+ 1) <$> gets esLevel
        modify' (\s -> s {esLevel = lvl})
        let auto n b = pushMacro n (MacroDef Nothing b OriginAuto lvl True)
            bodyOf v = maybe (BodyOpaque (headReason v)) BodyLiteral (knownText v)
            placeholders = zip [0 :: Int ..] args
            asText (i, v) = fromMaybe ("\0arg" <> T.pack (show i)) (knownText v)
            textArgs = map asText placeholders
            back t = maybe (known t) snd (lookup t (zip textArgs placeholders))
        auto "0" (BodyLiteral name)
        auto "**" (bodyOf (joinValues args))
        result <- case getopt (fromMaybe "" (macroOpts def)) textArgs of
          Left c -> do
            message Error ("unknown option -" <> T.singleton c <> " for %" <> name)
            pure (opaque (Failed ("unknown option -" <> T.singleton c <> " for %" <> name)))
          Right (GetoptResult opts positional) -> do
            forM_ opts $ \(c, optArg) -> do
              let flag = "-" <> T.singleton c
              auto flag (BodyLiteral (maybe flag (\a -> flag <> " " <> a) optArg))
              forM_ optArg $ \a -> auto (flag <> "*") (bodyOf (back a))
            auto "#" (BodyLiteral (T.pack (show (length positional))))
            forM_ (zip [1 :: Int ..] positional) $ \(i, a) -> auto (T.pack (show i)) (bodyOf (back a))
            auto "*" (bodyOf (joinValues (map back positional)))
            body
        freeLevel lvl
        modify' (\s -> s {esLevel = lvl - 1})
        pure result
    headReason v = case reasons v of
      r : _ -> r
      [] -> Failed "unknown argument"

joinValues :: [Value] -> Value
joinValues = \case
  [] -> mempty
  v : vs -> v <> mconcat (map (known " " <>) vs)

freeLevel :: Int -> Eval ()
freeLevel lvl = modify' $ \s ->
  let env = esEnv s
      prune = dropWhile ((>= lvl) . macroLevel)
      macros' = Map.mapMaybe (\stack -> case prune stack of [] -> Nothing; st -> Just st) (envMacros env)
   in s {esEnv = env {envMacros = macros'}}

data BArgs
  = NoArgs
  | Args [Value]
  | RawWhole Text
  | RawRest Text

builtin :: Int -> Builtin -> BArgs -> [Frag] -> Eval (Value, [Frag])
builtin d b args rest = case b of
  BDefine -> raw (defineRaw False True)
  BGlobal -> raw (defineRaw True False)
  BDnl -> case args of
    RawRest t -> pure (mempty, parseFrags (T.drop 1 (T.dropWhile (/= '\n') t)))
    _ -> done mempty
  BLua -> case args of
    RawRest t -> do
      let (script, after) = T.break (== '\n') t
      luaDefines script
      pure (opaque RunsLua, parseFrags after)
    RawWhole t -> luaDefines t >> done (opaque RunsLua)
    _ -> done (opaque RunsLua)
  BVerbose -> case args of
    RawRest t -> pure (known "0", parseFrags t)
    _ -> done mempty
  BUndefine -> do
    forM_ (take 1 argList) $ \v -> forM_ (knownText v) undefine
    done mempty
  BP -> sourceRef "%PATCH"
  BS -> sourceRef "%SOURCE"
  BBasename -> unary posixBasename
  BDirname -> unary posixDirname
  BSuffix -> unary suffix
  BShrink -> unary shrink
  BU2p -> unary urlPath
  BUrl2path -> unary urlPath
  BSpan -> done (firstArg)
  BQuote -> done (mapKnown (\t -> "\x1f" <> t <> "\x1f") (joinValues argList))
  BShescape -> strings (Just . shescape)
  BExpand -> expandValue (d + 1) firstArg >>= done
  BExprFn -> do
    r <- evalExpr False (\_ -> pure mempty) (itemsFromValue firstArg)
    case r of
      Left e -> message Error ("bad %{expr}: " <> e) >> done (opaque (Failed e))
      Right v -> done (renderExprValue v)
  BGetenv -> done (opaque ReadsEnv)
  BXdg -> done (opaque ReadsEnv)
  BExists -> done (opaque ReadsFilesystem)
  BLoad -> done (opaque ReadsFilesystem)
  BGetncpus -> done (opaque (HostDependent "cpu count"))
  BRpmversion -> done (opaque (HostDependent "rpm version"))
  BGetconfdir -> done (known "/usr/lib/rpm")
  BDump -> done mempty
  BTrace -> done mempty
  BEcho -> say Info >> done mempty
  BWarn -> say Warn >> done mempty
  BError -> do
    say Error
    done (opaque (Failed ("%{error:" <> showValue firstArg <> "}")))
  BLen -> strings (\case a : _ -> Just (luaLen a); [] -> Nothing)
  BUpper -> strings (\case a : _ -> Just (asciiUpper a); [] -> Nothing)
  BLower -> strings (\case a : _ -> Just (asciiLower a); [] -> Nothing)
  BReverse -> strings (\case a : _ -> Just (luaReverse a); [] -> Nothing)
  BRep -> luaStrings luaRep
  BSub -> luaStrings luaSub
  BGsub -> case traverse knownText argList of
    Just ts -> done (either (const (opaque RunsLua)) known (luaGsub ts))
    Nothing -> done (opaque (headReason (mconcat argList)))
  BMacrobody -> case knownText firstArg of
    Just n -> do
      look <- lookupMacro n
      case look of
        Found def -> done $ case macroBody def of
          BodyRaw t -> known t
          BodyValue v -> v
          BodyLiteral t -> known t
          BodyOpaque r -> opaque r
          BodyMaybe r -> opaque r
          BodyUndefined -> mempty
        Unsure r -> done (opaque r)
        _ -> message Error ("no such macro: " <> n) >> done (opaque (Failed ("no such macro: " <> n)))
    Nothing -> done (opaque (headReason firstArg))
  BUncompress -> do
    cmd <- expandText "%__rpmuncompress "
    done (cmd <> firstArg)
  where
    done v = pure (v, rest)
    argList = case args of
      Args vs -> vs
      _ -> []
    firstArg = case argList of
      v : _ -> v
      [] -> mempty
    headReason v = case reasons v of
      r : _ -> r
      [] -> Failed "unknown"
    unary f = done (maybe (opaque (headReason firstArg)) (known . f) (knownText firstArg))
    strings f = case traverse knownText argList of
      Just ts -> done (maybe mempty known (f ts))
      Nothing -> done (opaque (headReason (mconcat argList)))
    luaStrings f = case traverse knownText argList of
      Just ts -> done (maybe (opaque RunsLua) known (f ts))
      Nothing -> done (opaque (headReason (mconcat argList)))
    say sev = message sev (showValue (joinValues argList))
    sourceRef prefix = case knownText firstArg of
      Just n -> (,rest) <$> expandFrags (d + 1) (parseFrags (prefix <> n))
      Nothing -> done (opaque (headReason firstArg))
    raw f = case args of
      RawRest t -> do
        remaining <- f False t
        pure (mempty, parseFrags remaining)
      RawWhole t -> f True t >> done mempty
      _ -> done mempty
    defineRaw isGlobal handleOpts whole t = define d isGlobal handleOpts whole t

undefine :: Name -> Eval ()
undefine name = do
  look <- gets (Map.lookup name . envMacros . esEnv)
  closed <- gets (envClosed . esEnv)
  case look of
    Just [_] | not closed -> popMacro name >> pushMacro name (MacroDef Nothing BodyUndefined OriginAuto 0 False)
    Just (_ : _) -> popMacro name
    _ -> when (not closed) (pushMacro name (MacroDef Nothing BodyUndefined OriginAuto 0 False))

define :: Int -> Bool -> Bool -> Bool -> Text -> Eval Text
define d isGlobal handleOpts whole t0 = do
  let (flagExpand, flagGlobal, t1) = if handleOpts then options False False t0 else (False, False, t0)
      expandBody = isGlobal || flagExpand
      t2 = T.dropWhile isBlankChar t1
      (name, t3) = T.span isNameChar t2
      (opts, t4) = delimited '(' ')' t3
      (mods, t5) = delimited '<' '>' t4
      afterWs = T.dropWhile isBlankChar t5
      (bodyText, remaining)
        | whole = (afterWs, "")
        | Just ('{', _) <- T.uncons afterWs, Just (inner, after) <- matchClose '{' '}' afterWs = (inner, after)
        | otherwise = freeField afterWs
      remaining' = T.dropWhile (\c -> c == '\n' || c == '\r') remaining
      literal = maybe False (T.elem 'l') mods
      validName = case T.uncons name of
        Just (c, more) -> (c /= '_' && not (isNameDigit c)) || (c == '_' && not (T.null more))
        Nothing -> False
  if
    | not validName -> message Error ("macro %" <> name <> " has an illegal name") >> pure remaining'
    | isJust (builtinNamed name) -> message Error ("macro %" <> name <> " is a built-in") >> pure remaining'
    | T.null bodyText -> message Error ("macro %" <> name <> " has an empty body") >> pure remaining'
    | otherwise -> do
        lvl <- gets esLevel
        sp <- gets esSpan
        let origin = maybe OriginAuto OriginSpec sp
            level = if isGlobal || flagGlobal then 0 else lvl
        body <-
          if
            | literal -> pure (BodyLiteral bodyText)
            | expandBody -> BodyValue . stripQuoteMarks <$> expandFrags (d + 1) (parseFrags bodyText)
            | otherwise -> pure (BodyRaw bodyText)
        pushMacro name (MacroDef opts body origin level False)
        pure remaining'
  where
    isNameDigit c = c >= '0' && c <= '9'
    options e g t =
      let t' = T.dropWhile isBlankChar t
       in case T.uncons t' of
            Just ('-', more) ->
              let (letters, after) = T.span (not . isSpace) more
               in if T.null letters then (e, g, t') else options (e || T.elem 'e' letters) (g || T.elem 'g' letters) after
            _ -> (e, g, t')
    delimited open close t = case T.uncons t of
      Just (c, more) | c == open, (inner, after) <- T.break (== close) more, not (T.null after) -> (Just inner, T.drop 1 after)
      _ -> (Nothing, t)

-- | split a free-form macro body from the rest, like rpm. backslashes escape
-- the next character and are dropped.
freeField :: Text -> (Text, Text)
freeField t = go (0 :: Int) (0 :: Int) (0 :: Int) [] (T.unpack t)
  where
    finish acc rest = (T.dropWhileEnd (\c -> isBlankChar c || c == '\n' || c == '\r') (T.pack (reverse acc)), T.pack rest)
    go bc pc xc acc = \case
      [] -> finish acc []
      s@(c : cs)
        | c == '\n' && bc == 0 && pc == 0 -> finish acc s
        | c == '\\' -> case cs of
            [] -> finish (c : acc) []
            x : cs' -> go bc pc xc (x : acc) cs'
        | c == '%' -> case cs of
            '{' : cs' -> go (bc + 1) pc xc ('{' : '%' : acc) cs'
            '(' : cs' -> go bc (pc + 1) xc ('(' : '%' : acc) cs'
            '[' : cs' -> go bc pc (xc + 1) ('[' : '%' : acc) cs'
            '%' : cs' -> go bc pc xc ('%' : '%' : acc) cs'
            _ -> go bc pc xc (c : acc) cs
        | c == '{' && bc > 0 -> go (bc + 1) pc xc (c : acc) cs
        | c == '}' && bc > 0 -> go (bc - 1) pc xc (c : acc) cs
        | c == '(' && pc > 0 -> go bc (pc + 1) xc (c : acc) cs
        | c == ')' && pc > 0 -> go bc (pc - 1) xc (c : acc) cs
        | c == '[' && xc > 0 -> go bc pc (xc + 1) (c : acc) cs
        | c == ']' && xc > 0 -> go bc pc (xc - 1) (c : acc) cs
        | otherwise -> go bc pc xc (c : acc) cs

-- | evaluate an @%if@, @%ifarch@ or @%ifos@ line. a bad expression records an
-- error and gives 'Unknown'.
evalCond :: CondLine -> Eval Tri
evalCond CondLine {..} = case condKind of
  CElse -> pure Yes
  CEndif -> pure Yes
  k
    | k `elem` [CIf, CElif] -> do
        v <- expandAt 0 args
        r <- evalExpr False (\_ -> pure mempty) (itemsFromValue v)
        case r of
          Left e -> do
            message Error ("bad " <> condKeyword <> " condition: " <> e)
            pure (Unknown [Failed e])
          Right ev -> pure (boolify ev)
    | k `elem` [CIfArch, CElifArch] -> matchTarget "_target_cpu"
    | k == CIfNArch -> triNot <$> matchTarget "_target_cpu"
    | k `elem` [CIfOs, CElifOs] -> matchTarget "_target_os"
    | otherwise -> triNot <$> matchTarget "_target_os"
  where
    args = map unescapeContinuation condArgs
    unescapeContinuation = \case
      Lit t -> Lit (T.replace "\\\n" "  " t)
      f -> f
    matchTarget macro = do
      target <- expandAt 0 [Macro (Braced "" macro Nothing)]
      words' <- valueWords <$> expandAt 0 args
      when (null words') (message Error (condKeyword <> " needs an argument"))
      pure $ case knownText target of
        Nothing -> Unknown (reasons target)
        Just tgt
          | any (\w -> fmap T.toLower (knownText w) == Just (T.toLower tgt)) words' -> Yes
          | any (not . isKnown) words' -> Unknown (concatMap reasons words')
          | otherwise -> No
