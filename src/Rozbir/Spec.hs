-- | the semantic model of a spec for one build target. when an @%if@ can't
-- be decided, both branches end up in the model, tagged with a 'Guard'.
module Rozbir.Spec
  ( Spec (..),
    Package (..),
    PackageKey (..),
    TagEntry (..),
    SectionContent (..),
    SourceEntry (..),
    Bcond (..),
    Condition (..),
    Assumption (..),
    Guard,
    Located (..),
    ResolveOptions (..),
    defaultOptions,
    resolve,
    resolveWith,
    knownTags,
    tagDefinesMacro,
    deprecatedTags,
    mainPackage,
    packageTags,
    packageTag,
    sectionArgWords,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (unless, when)
import Control.Monad.State.Strict (StateT, execStateT, get, gets, lift, modify', put)
import Data.Char (isDigit)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Rozbir.Changelog (Entry, parseEntries)
import Rozbir.Eval
import Rozbir.Eval.Builtin (posixBasename)
import Rozbir.Syntax
import Rozbir.Syntax.Parse (parseFrags, parseLines)
import Rozbir.Syntax.Render (renderBody)
import Text.Read (readMaybe)

-- | the item is in one branch of an @%if@ that couldn't be decided
data Assumption = Assumption
  { -- | span of the @%if@ line that opens the block
    assumeIf :: !Span,
    -- | branch index, 0 for the @%if@, then one per @%elif@ or @%else@
    assumeBranch :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | the undecided @%if@ branches an item is in, innermost first. empty means
-- it always applies.
type Guard = [Assumption]

-- | a value with the source span it came from and the guard it's under
data Located a = Located
  { locValue :: !a,
    -- | span of the line it came from
    locSpan :: !Span,
    -- | the undecided branches it's in
    locGuard :: !Guard
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (NFData)

-- | identifies a package within a t'Spec'
data PackageKey
  = -- | the main package
    MainKey
  | -- | a subpackage, keyed by its full expanded name such as @foo-devel@
    SubKey !Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | a preamble tag after macro expansion
data TagEntry = TagEntry
  { -- | lowercase name without a Source or Patch number, so @Source0@ is
    -- @source@
    teKey :: !Text,
    -- | the tag name as written
    teName :: !Text,
    -- | the qualifier as written, such as @(post)@ in @Requires(post)@
    teQualifier :: !(Maybe Text),
    -- | the expanded value
    teValue :: !Value,
    -- | the original line, or Nothing when a macro expanded to this tag
    teRaw :: !(Maybe TagLine)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | the main package or a @%package@ subpackage with its preamble tags
data Package = Package
  { pkgKey :: !PackageKey,
    -- | the expanded name
    pkgName :: !Value,
    -- | span of the @%package@ line, Nothing for the main package
    pkgHeader :: !(Maybe Span),
    -- | the guard of the @%package@ line
    pkgGuard :: !Guard,
    -- | tags in source order
    pkgTags :: ![Located TagEntry],
    -- | True when a preamble line didn't fully expand, so some tags may be
    -- missing from 'pkgTags'
    pkgOpaque :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | a section such as @%prep@, @%files@ or a scriptlet, with its expanded body
data SectionContent = SectionContent
  { scKind :: !SectionKind,
    -- | the package the section belongs to
    scPackage :: !PackageKey,
    -- | the expanded name of that package
    scPackageName :: !Value,
    -- | the expanded arguments after the section keyword
    scArgs :: !Value,
    -- | span of the header line
    scHeader :: !Span,
    -- | the guard of the header line
    scGuard :: !Guard,
    -- | expanded body lines in source order
    scLines :: ![Located Value],
    -- | the CST lines behind 'scLines', in source order
    scRaw :: ![Line]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | a numbered Source or Patch tag
data SourceEntry = SourceEntry
  { -- | True for Patch, False for Source
    srcPatch :: !Bool,
    -- | the number, or the one rpm assigns when the tag has none
    srcNumber :: !Int,
    -- | whether the number was written in the tag name
    srcExplicit :: !Bool,
    -- | the expanded URL or file name
    srcValue :: !Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | a @%bcond@, @%bcond_with@ or @%bcond_without@ declaration
data Bcond = Bcond
  { -- | the name, @foo@ in @%bcond foo 1@
    bcName :: !Text,
    -- | whether the bcond is on, 'Unknown' if that depends on unknown macros
    bcEnabled :: !Tri
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | the result of evaluating one conditional line
data Condition = Condition
  { -- | span of the conditional line
    condLineSpan :: !Span,
    -- | the condition's own result, ignoring earlier branches
    condResult :: !Tri
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | everything rozbir knows about a spec for one target
data Spec = Spec
  { -- | the main package first, then subpackages in declaration order
    specPackages :: ![Package],
    -- | sections in source order
    specSections :: ![SectionContent],
    -- | Source and Patch tags in source order
    specSources :: ![Located SourceEntry],
    -- | bcond declarations in source order
    specBconds :: ![Located Bcond],
    -- | parsed @%changelog@ entries in the order written
    specChangelog :: ![Entry],
    -- | @%changelog@ lines that didn't parse, with a message
    specChangelogProblems :: ![(Span, Text)],
    -- | conditional lines that were evaluated. lines after a branch known to
    -- be taken are skipped.
    specConditions :: ![Condition],
    -- | the expanded argument of each @%include@
    specIncludes :: ![Located Value],
    -- | the macro environment at the end of the spec
    specEnv :: !Env,
    -- | evaluation warnings and errors, including @%include@ files not read
    specMessages :: ![EvalMessage],
    -- | macros nothing defined. only collected when system macros are loaded.
    specUndefined :: ![(Name, Span)]
  }
  deriving stock (Show, Generic)

-- | inputs to 'resolveWith'
data ResolveOptions = ResolveOptions
  { -- | the starting macros, from 'newEnv' or 'Rozbir.IO.loadSystemMacros'
    roEnv :: !Env,
    roTarget :: !Target,
    -- | @%include@ trees keyed by the argument as written. a missing include
    -- leaves what it defines unknown.
    roIncludes :: !(Map Text Tree)
  }

-- | an open environment without system macros, 'unknownTarget' and no includes
defaultOptions :: ResolveOptions
defaultOptions = ResolveOptions (newEnv False) unknownTarget Map.empty

-- | every preamble tag rpm accepts, as lowercase keys
knownTags :: [Text]
knownTags =
  [ "name",
    "version",
    "release",
    "epoch",
    "summary",
    "license",
    "sourcelicense",
    "distribution",
    "disturl",
    "vendor",
    "group",
    "packager",
    "url",
    "vcs",
    "source",
    "patch",
    "nosource",
    "nopatch",
    "excludearch",
    "exclusivearch",
    "excludeos",
    "exclusiveos",
    "icon",
    "provides",
    "requires",
    "recommends",
    "suggests",
    "supplements",
    "enhances",
    "prereq",
    "conflicts",
    "obsoletes",
    "prefixes",
    "prefix",
    "buildroot",
    "buildarchitectures",
    "buildarch",
    "buildconflicts",
    "buildoption",
    "buildprereq",
    "buildrequires",
    "buildsystem",
    "autoreqprov",
    "autoreq",
    "autoprov",
    "docdir",
    "disttag",
    "bugurl",
    "translationurl",
    "upstreamreleases",
    "orderwithrequires",
    "removepathpostfixes",
    "modularitylabel"
  ]

-- | whether a tag also defines a macro, like @Version:@ defines @%version@
tagDefinesMacro :: Text -> Bool
tagDefinesMacro k =
  k
    `elem` [ "name",
             "version",
             "release",
             "epoch",
             "summary",
             "license",
             "sourcelicense",
             "distribution",
             "disturl",
             "vendor",
             "group",
             "packager",
             "url",
             "vcs",
             "prefixes",
             "prefix",
             "buildsystem",
             "disttag",
             "bugurl",
             "translationurl",
             "upstreamreleases",
             "removepathpostfixes",
             "modularitylabel"
           ]

-- | tags current rpm ignores or has replaced
deprecatedTags :: [Text]
deprecatedTags = ["prereq", "buildprereq", "buildroot", "group", "packager", "vendor", "distribution"]

canonicalTag :: Text -> (Text, Maybe Int)
canonicalTag raw =
  let lower = T.toLower raw
      digits = T.takeWhileEnd isDigit lower
      base = T.dropEnd (T.length digits) lower
   in if base `elem` ["source", "patch", "nosource", "nopatch"]
        then (base, if T.null digits then Nothing else readMaybe (T.unpack digits))
        else (lower, Nothing)

-- | the main package. 'resolve' always creates one.
mainPackage :: Spec -> Maybe Package
mainPackage = L.find ((== MainKey) . pkgKey) . specPackages

-- | every tag with the given lowercase key, in source order
packageTags :: Text -> Package -> [Located TagEntry]
packageTags key = filter ((== key) . teKey . locValue) . pkgTags

-- | the first tag with the given lowercase key
packageTag :: Text -> Package -> Maybe (Located TagEntry)
packageTag key = L.find ((== key) . teKey . locValue) . pkgTags

data Current = Current
  { curKind :: !(Maybe SectionKind),
    curPackage :: !PackageKey,
    curIndex :: !(Maybe Int)
  }

data RS = RS
  { rsCurrent :: !Current,
    rsGuard :: !Guard,
    rsPackages :: ![Package],
    rsSections :: ![SectionContent],
    rsSources :: ![Located SourceEntry],
    rsBconds :: ![Located Bcond],
    rsConditions :: ![Condition],
    rsIncludes :: ![Located Value],
    rsAutoSource :: !Int,
    rsAutoPatch :: !Int,
    rsIncludeMap :: !(Map Text Tree),
    rsIncludeDepth :: !Int
  }

type R = StateT RS Eval

-- | resolve a tree for one target, without @%include@ files. it never fails.
-- unknown values come back 'Opaque'.
resolve :: Env -> Target -> Tree -> Spec
resolve env target = resolveWith (ResolveOptions env target Map.empty)

-- | like 'resolve', but takes @%include@ trees too
resolveWith :: ResolveOptions -> Tree -> Spec
resolveWith ResolveOptions {..} tree =
  let env0 = withTarget roTarget roEnv
      start =
        RS
          { rsCurrent = Current Nothing MainKey Nothing,
            rsGuard = [],
            rsPackages = [Package MainKey mempty Nothing [] [] False],
            rsSections = [],
            rsSources = [],
            rsBconds = [],
            rsConditions = [],
            rsIncludes = [],
            rsAutoSource = -1,
            rsAutoPatch = -1,
            rsIncludeMap = roIncludes,
            rsIncludeDepth = 0
          }
      (rs, st) = runEval (initialState env0) (execStateT (nodes (treeNodes tree)) start)
      sections = reverse (map finishSection (rsSections rs))
      changelogLines =
        [ (lineSpan l, renderBody (lineBody l))
        | s <- sections,
          scKind s == SChangelog,
          l <- scRaw s,
          not (macroLine l)
        ]
      macroLine l = case lineBody l of
        Body (Macro _ : _) -> True
        _ -> False
      (entries, problems) = parseEntries changelogLines
   in Spec
        { specPackages = map finishPackage (reverse (rsPackages rs)),
          specSections = sections,
          specSources = reverse (rsSources rs),
          specBconds = reverse (rsBconds rs),
          specChangelog = entries,
          specChangelogProblems = problems,
          specConditions = reverse (rsConditions rs),
          specIncludes = reverse (rsIncludes rs),
          specEnv = esEnv st,
          specMessages = reverse (esMessages st),
          specUndefined = reverse (esUndefined st)
        }
  where
    finishPackage p = p {pkgTags = reverse (pkgTags p)}
    finishSection s = s {scLines = reverse (scLines s), scRaw = reverse (scRaw s)}

nodes :: [Node] -> R ()
nodes = mapM_ node

node :: Node -> R ()
node = \case
  NLine l -> line l
  NSection s -> line (sectionLine s) >> nodes (sectionNodes s)
  NCond block -> conditional block

conditional :: CondBlock -> R ()
conditional (CondBlock branches _) = do
  env0 <- lift getEnv
  guard0 <- gets rsGuard
  let ifSpan = lineSpan (branchLine (NE.head branches))
  let go _ collected [] = finish collected True
      go prior collected ((i, Branch {..}) : more) = do
        lift (putEnv env0)
        t <- lift (atSpan (lineSpan branchLine) (evalCond branchCond))
        modify' (\rs -> rs {rsConditions = Condition (lineSpan branchLine) t : rsConditions rs})
        let active = triAnd (triNot prior) t
        case active of
          No -> go prior collected more
          Yes -> do
            nodes branchNodes
            env <- lift getEnv
            finish (env : collected) False
          Unknown _ -> do
            modify' (\rs -> rs {rsGuard = Assumption ifSpan i : guard0})
            nodes branchNodes
            modify' (\rs -> rs {rsGuard = guard0})
            env <- lift getEnv
            go (triOr prior t) (env : collected) more
      finish collected noneMayRun = do
        let candidates = if noneMayRun then env0 : collected else collected
        case candidates of
          [] -> lift (putEnv env0)
          e : es -> lift (putEnv (L.foldl' mergeEnvs e es))
  go No [] (zip [0 ..] (NE.toList branches))

line :: Line -> R ()
line l = do
  rs <- get
  let cur = rsCurrent rs
      sp = lineSpan l
  case lineBody l of
    Blank t -> record l (known t)
    Comment indent fs -> do
      v <- lift (atSpan sp (expandAt 0 fs))
      when (keepsComments (curKind cur)) (record l (known indent <> v))
    Tag t -> tag l t Nothing
    Define d -> do
      v <- lift (atSpan sp (expandAt 0 (parseFrags (renderBody (Define d)))))
      when (defKind d `elem` [DefBcond, DefBcondWith, DefBcondWithout]) $ do
        enabled <- lift (isDefined ("with_" <> defName d))
        g <- gets rsGuard
        modify' (\s -> s {rsBconds = Located (Bcond (defName d) enabled) sp g : rsBconds s})
      unless (valueNull v) (record l v)
    Cond _ -> pure ()
    Header h -> header l h
    Include _ fs -> include l fs
    Body fs -> do
      v <- lift (atSpan sp (expandAt 0 fs))
      if isPreambleLike (curKind cur) || producesHeader v
        then reclassify l v
        else record l v
    Bogus _ -> pure ()

producesHeader :: Value -> Bool
producesHeader v
  | "\n" `T.isInfixOf` t || "%" `T.isPrefixOf` t = any isHeader (fst (parseLines (Just SBuild) t))
  | otherwise = False
  where
    t = showValue v
    isHeader x = case lineBody x of
      Header _ -> True
      _ -> False

keepsComments :: Maybe SectionKind -> Bool
keepsComments = \case
  Just k -> isBuildSection k || isScriptSection k
  Nothing -> False

record :: Line -> Value -> R ()
record l v = do
  rs <- get
  case curIndex (rsCurrent rs) of
    Nothing -> pure ()
    Just _ -> case rsSections rs of
      s : rest -> put rs {rsSections = s {scLines = Located v (lineSpan l) (rsGuard rs) : scLines s, scRaw = l : scRaw s} : rest}
      [] -> pure ()

reclassify :: Line -> Value -> R ()
reclassify l v = mapM_ one (valueLines v)
  where
    one lv
      | T.all (`elem` [' ', '\t', '\r']) (showValue lv) = pure ()
      | otherwise = do
          cur <- gets rsCurrent
          when (not (isKnown lv) && isPreambleLike (curKind cur)) markOpaque
          case fst (parseLines (curKind cur) (showValue lv)) of
            x : _ -> case lineBody x of
              Tag tl -> case dropKnownPrefix (T.length (tagPrefix tl)) lv of
                Just value -> tag l tl (Just (trimValue value))
                Nothing -> markOpaque
              Header h -> header l h
              Body _ -> record l lv
              _ -> pure ()
            [] -> pure ()
    markOpaque = do
      pkgK <- gets (curPackage . rsCurrent)
      modify' (\s -> s {rsPackages = map (\p -> if pkgKey p == pkgK then p {pkgOpaque = True} else p) (rsPackages s)})
    tagPrefix TagLine {..} = tagIndent <> tagName <> maybe "" (\q -> qualGap q <> qualRaw q) tagQualifier <> tagSep
    trimValue = mapKnown T.stripEnd

dropKnownPrefix :: Int -> Value -> Maybe Value
dropKnownPrefix n (Value ps) = go n ps
  where
    go 0 rest = Just (Value rest)
    go k (Known t : rest)
      | T.length t > k = Just (Value (Known (T.drop k t) : rest))
      | otherwise = go (k - T.length t) rest
    go _ _ = Nothing

tag :: Line -> TagLine -> Maybe Value -> R ()
tag l t@TagLine {..} expandedFrom = do
  let sp = lineSpan l
      (key, number) = canonicalTag tagName
  value <- case expandedFrom of
    Just v -> pure v
    Nothing -> lift (atSpan sp (expandAt 0 tagValue))
  rs <- get
  let entry = TagEntry key tagName (qualRaw <$> tagQualifier) value (if isJust expandedFrom then Nothing else Just t)
      located = Located entry sp (rsGuard rs)
      pkgK = curPackage (rsCurrent rs)
  put rs {rsPackages = map (\p -> if pkgKey p == pkgK then p {pkgTags = located : pkgTags p} else p) (rsPackages rs)}
  when (tagDefinesMacro key) $ lift $ atSpan sp $ do
    let def = MacroDef Nothing (BodyValue value) (OriginTag sp) 0 False
    pushMacro key def
  when (key == "name" && pkgK == MainKey) $
    modify' (\s -> s {rsPackages = map (\p -> if pkgKey p == MainKey then p {pkgName = value} else p) (rsPackages s)})
  when (key `elem` ["source", "patch"]) (source sp key number value)

source :: Span -> Text -> Maybe Int -> Value -> R ()
source sp key number value = do
  rs <- get
  let isPatch = key == "patch"
      auto = if isPatch then rsAutoPatch rs else rsAutoSource rs
      n = fromMaybe (auto + 1) number
      auto' = max auto n
      prefix = if isPatch then "PATCH" else "SOURCE"
      body = case knownText value of
        Just t -> BodyRaw ("%{_sourcedir}/" <> posixBasename (T.strip t))
        Nothing -> BodyOpaque (case reasons value of r : _ -> r; [] -> Failed "unknown")
  put
    rs
      { rsSources = Located (SourceEntry isPatch n (isJust number) value) sp (rsGuard rs) : rsSources rs,
        rsAutoSource = if isPatch then rsAutoSource rs else auto',
        rsAutoPatch = if isPatch then auto' else rsAutoPatch rs
      }
  lift (pushMacro (prefix <> T.pack (show n)) (MacroDef Nothing body (OriginTag sp) 0 False))
  lift (pushMacro (prefix <> T.pack (show n) <> "_URL") (MacroDef Nothing (BodyValue value) (OriginTag sp) 0 False))

-- | the package name in a section header's arguments. the 'Bool' is True for a
-- full @-n@ name and False for a suffix to the main package's name.
sectionArgWords :: SectionKind -> [Value] -> (Maybe Value, Bool)
sectionArgWords kind = go Nothing False
  where
    withValue = case kind of
      SFiles -> ["-f"]
      SDescription -> ["-l"]
      k | isScriptSection k -> ["-p", "-P"]
      _ -> []
    is w = knownText w
    go name absolute = \case
      [] -> (name, absolute)
      w : _ | is w == Just "--" -> (name, absolute)
      w : n : rest | is w == Just "-n" -> go (Just n) True rest
      w : _ : rest | Just o <- is w, o `elem` withValue -> go name absolute rest
      w : rest | Just o <- is w, "-" `T.isPrefixOf` o -> go name absolute rest
      w : rest -> case name of
        Nothing -> go (Just w) False rest
        Just _ -> go name absolute rest

header :: Line -> SectionHeader -> R ()
header l h = do
  let sp = lineSpan l
      kind = secKind h
  expandedArgs <- lift (atSpan sp (expandAt 0 (secArgs h)))
  let (args, extraLines) = case valueLines expandedArgs of
        firstLine : more -> (firstLine, more)
        [] -> (mempty, [])
  mainName <- lift (atSpan sp (expandText "%{name}"))
  rs <- get
  let ws = valueWords args
      (named, absolute) =
        if isBuildSection kind || kind `elem` [SChangelog, SPatchlist, SSourcelist, SEnd]
          then (Nothing, False)
          else sectionArgWords kind ws
      pkgValue = case named of
        Nothing -> mainName
        Just n
          | absolute -> n
          | otherwise -> mainName <> known "-" <> n
      pkgK = case named of
        Nothing -> MainKey
        Just _
          | isKnown pkgValue && pkgValue == mainName -> MainKey
          | otherwise -> SubKey (showValue pkgValue)
      section = SectionContent kind pkgK pkgValue args sp (rsGuard rs) [] []
      exists = any ((== pkgK) . pkgKey) (rsPackages rs)
      packages
        | kind == SPackage && not exists = Package pkgK pkgValue (Just sp) (rsGuard rs) [] False : rsPackages rs
        | otherwise = rsPackages rs
  put
    rs
      { rsCurrent = Current (Just kind) pkgK (Just (length (rsSections rs))),
        rsSections = section : rsSections rs,
        rsPackages = packages
      }
  mapM_ (record l) extraLines

include :: Line -> [Frag] -> R ()
include l fs = do
  let sp = lineSpan l
  v <- lift (atSpan sp (expandAt 0 fs))
  rs <- get
  put rs {rsIncludes = Located v sp (rsGuard rs) : rsIncludes rs}
  case knownText v >>= (\k -> Map.lookup (T.strip k) (rsIncludeMap rs)) of
    Just tree | rsIncludeDepth rs < 16 -> do
      modify' (\s -> s {rsIncludeDepth = rsIncludeDepth s + 1})
      nodes (treeNodes tree)
      modify' (\s -> s {rsIncludeDepth = rsIncludeDepth s - 1})
    _ -> lift (atSpan sp (message Warn ("%include " <> showValue v <> " wasn't read, so whatever it defines is unknown")))
