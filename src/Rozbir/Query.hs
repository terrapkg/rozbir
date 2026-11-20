module Rozbir.Query
  ( Resolved (..),
    resolvedToMaybe,
    tagText,
    name,
    evr,
    version,
    versionInputs,
    macroReferences,
    definitions,
    subpackages,
    sources,
    patches,
    bconds,
    buildRequires,
    dependencies,
    splitDeps,
  )
where

import Data.Char (isSpace)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Rozbir.Eval.Builtin (builtinNames, preludeMacros)
import Rozbir.Eval.Types
import Rozbir.Spec
import Rozbir.Syntax
import Rozbir.Version (EVR (..))
import Text.Read (readMaybe)

data Resolved a
  = Resolved a
  | Unresolved [Reason]
  deriving stock (Eq, Show, Functor, Generic)

instance Applicative Resolved where
  pure = Resolved
  Resolved f <*> Resolved a = Resolved (f a)
  Unresolved a <*> Unresolved b = Unresolved (a <> b)
  Unresolved a <*> _ = Unresolved a
  _ <*> Unresolved b = Unresolved b

resolvedToMaybe :: Resolved a -> Maybe a
resolvedToMaybe = \case
  Resolved a -> Just a
  Unresolved _ -> Nothing

tagText :: Text -> Package -> Resolved Text
tagText key pkg = case packageTags key pkg of
  [] -> Unresolved [Undefined key]
  entries ->
    let values = map (teValue . locValue) entries
        texts = nub [T.strip t | Just t <- map knownText values]
     in case (texts, filter (isNothing . knownText) values) of
          ([t], []) -> Resolved t
          (_, unknowns@(_ : _)) -> Unresolved (concatMap reasons unknowns)
          _ -> Unresolved [Ambiguous key]

name :: Spec -> Resolved Text
name spec = maybe (Unresolved [Undefined "name"]) (tagText "name") (mainPackage spec)

version :: Spec -> Resolved Text
version spec = maybe (Unresolved [Undefined "version"]) (tagText "version") (mainPackage spec)

evr :: Spec -> Resolved EVR
evr spec = case mainPackage spec of
  Nothing -> Unresolved [Undefined "name"]
  Just pkg ->
    EVR
      <$> epoch pkg
      <*> tagText "version" pkg
      <*> tagText "release" pkg
  where
    epoch pkg = case packageTags "epoch" pkg of
      [] -> Resolved 0
      _ -> tagText "epoch" pkg >>=. \t -> maybe (Unresolved [Failed ("bad epoch " <> t)]) Resolved (readMaybe (T.unpack t))
    r >>=. f = case r of
      Resolved a -> f a
      Unresolved rs -> Unresolved rs

macroReferences :: [Frag] -> [Name]
macroReferences = concatMap frag
  where
    frag = \case
      Macro m -> ref m
      _ -> []
    ref = \case
      Bare _ n -> [n]
      Braced _ n arg -> n : maybe [] argRefs arg
      Shell fs -> macroReferences fs
      Expr fs -> macroReferences fs
      _ -> []
    argRefs = \case
      ColonArg fs -> macroReferences fs
      SpaceArg fs -> macroReferences fs

definitions :: Tree -> Map Name [DefineLine]
definitions tree =
  Map.fromListWith
    (flip (<>))
    [ (defName d, [d])
    | Line {lineBody = Define d} <- treeLines tree,
      defKind d `elem` [DefGlobal, DefDefine]
    ]

versionInputs :: Tree -> [Name]
versionInputs tree = go [] roots
  where
    defs = definitions tree
    roots =
      concat
        [ macroReferences (tagValue t)
        | Line {lineBody = Tag t} <- treeLines tree,
          T.toLower (tagName t) == "version"
        ]
    ignored = builtinNames <> [n | (n, _, _) <- preludeMacros]
    go seen = \case
      [] -> reverse seen
      n : rest
        | n `elem` seen || n `elem` ignored || T.null n -> go seen rest
        | otherwise ->
            let more = concatMap (macroReferences . defBody) (Map.findWithDefault [] n defs)
             in go (n : seen) (rest <> more)

subpackages :: Spec -> [Package]
subpackages = filter ((/= MainKey) . pkgKey) . specPackages

sources :: Spec -> [Located SourceEntry]
sources = filter (not . srcPatch . locValue) . specSources

patches :: Spec -> [Located SourceEntry]
patches = filter (srcPatch . locValue) . specSources

bconds :: Spec -> [Located Bcond]
bconds = specBconds

dependencies :: Text -> Package -> [Located Text]
dependencies key pkg =
  [ Located dep (locSpan l) (locGuard l)
  | l <- packageTags key pkg,
    Just t <- [knownText (teValue (locValue l))],
    dep <- splitDeps t
  ]

buildRequires :: Spec -> [Located Text]
buildRequires spec = maybe [] (dependencies "buildrequires") (mainPackage spec)

splitDeps :: Text -> [Text]
splitDeps = join . tokens
  where
    tokens t = case T.uncons (T.dropWhile sepChar t) of
      Nothing -> []
      Just ('(', _) ->
        let (tok, rest) = balanced 0 0 (T.dropWhile sepChar t)
         in tok : tokens rest
      Just _ ->
        let (tok, rest) = T.break sepChar (T.dropWhile sepChar t)
         in tok : tokens rest
    sepChar c = isSpace c || c == ','
    balanced :: Int -> Int -> Text -> (Text, Text)
    balanced depth n t = case T.uncons (T.drop n t) of
      Nothing -> (t, "")
      Just (c, _)
        | c == '(' -> balanced (depth + 1) (n + 1) t
        | c == ')' && depth == 1 -> T.splitAt (n + 1) t
        | c == ')' -> balanced (depth - 1) (n + 1) t
        | otherwise -> balanced depth (n + 1) t
    operators = ["<", "<=", "=", "==", ">=", ">"]
    join = \case
      a : op : b : rest | op `elem` operators -> (a <> " " <> op <> " " <> b) : join rest
      a : rest -> a : join rest
      [] -> []
