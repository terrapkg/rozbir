-- | the core ruleset: syntax errors, missing or duplicate tags, bad
-- changelogs, evaluation errors and other things rpm itself trips on. on by
-- default.
module Rozbir.Lint.Core (rules) where

import Data.Char (isDigit)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Rozbir.Changelog (Entry (..), EntryHeader (..))
import Rozbir.Edit (TextEdit (..))
import Rozbir.Eval.Types
import Rozbir.Lint.Types
import Rozbir.Query (splitDeps)
import Rozbir.Spec
import Rozbir.Syntax
import Rozbir.Syntax.Render (lineFrags, macroSpans, renderFrags, renderMacro)

-- | every core rule
rules :: [Rule]
rules =
  syntaxRules
    <> [ Rule "unknown-tag" Core Error "a preamble line looks like a tag but rpm doesn't know it" unknownTag,
         Rule "duplicate-tag" Core Error "a single-valued tag is set twice for the same package" duplicateTag,
         Rule "missing-required-tag" Core Error "a package is missing Name, Version, Release, Summary or License" missingTag,
         Rule "missing-description" Core Error "a package has no %description" missingDescription,
         Rule "section-for-unknown-package" Core Error "%files, %description or a scriptlet names a package no %package declares" unknownPackage,
         Rule "duplicate-source-number" Core Error "two Source or Patch tags share a number" duplicateSource,
         Rule "macro-in-comment" Core Warn "rpm expands macros in comments, so %define in a comment still defines" macroInComment,
         Rule "needs-newer-rpm" Core Warn "uses syntax EL10's rpm 4.19 doesn't understand" newerRpm,
         Rule "patchn-obsolete" Core Error "%patchN was removed in rpm 4.20, use %patch -P N" patchN,
         Rule "changelog-date" Core Warn "a changelog entry has a bad date or the wrong weekday" changelogDate,
         Rule "changelog-order" Core Warn "changelog entries should go newest first" changelogOrder,
         Rule "eval-error" Core Error "a macro or condition fails to evaluate the way rpm would fail" evalError,
         Rule "line-outside-section" Core Error "text after %end that belongs to no section" outsideSection,
         Rule "undefined-macro" Core Warn "a macro stays as literal text because nothing defines it (needs system macros loaded)" undefinedMacro,
         Rule "bcond-unused" Core Warn "a bcond that no %{with} or %{without} ever checks" bcondUnused,
         Rule "duplicate-dependency" Core Warn "the same dependency is listed twice for one package" duplicateDependency
       ]

syntaxRules :: [Rule]
syntaxRules =
  [ syntaxRule "unclosed-if" "an %if without %endif",
    syntaxRule "stray-endif" "an %endif without %if",
    syntaxRule "stray-else" "an %else without %if",
    syntaxRule "stray-elif" "an %elif without %if",
    syntaxRule "branch-after-else" "an %elif or %else after %else",
    syntaxRule "unclosed-macro" "a %{, %( or %[ that's never closed",
    syntaxRule "trailing-backslash" "a line continuation at the end of the file",
    syntaxRule "not-a-tag" "a preamble line that isn't a tag, macro or section"
  ]
  where
    syntaxRule rid doc = Rule rid Core Error doc $ \li ->
      [diag rid (seSpan e) (seMessage e) | e <- treeErrors (liTree li), seCode e == rid]

singleValued :: [Text]
singleValued = ["name", "version", "release", "epoch", "summary", "license", "url", "buildarch", "buildarchitectures", "group", "packager", "vendor", "buildsystem", "sourcelicense"]

unknownTag :: LintInput -> [Diagnostic]
unknownTag li =
  [ diag "unknown-tag" (lineSpan l) ("rpm doesn't know the tag " <> tagName t)
  | (sec, l@Line {lineBody = Tag t}) <- linesWithSection (liTree li),
    preambleLike sec,
    let lower = T.toLower (tagName t)
        base = T.dropWhileEnd isDigit lower,
    lower `notElem` knownTags,
    not (base `elem` ["source", "patch", "nosource", "nopatch"])
  ]

overlapping :: Guard -> Guard -> Bool
overlapping a b = not (any conflicts a)
  where
    conflicts x = any (\y -> assumeIf x == assumeIf y && assumeBranch x /= assumeBranch y) b

duplicateTag :: LintInput -> [Diagnostic]
duplicateTag li =
  nub
    [ diag "duplicate-tag" (locSpan later) (teName (locValue later) <> " is already set on line " <> lineNo (locSpan earlier))
    | spec <- map snd (liSpecs li),
      pkg <- specPackages spec,
      key <- singleValued,
      let entries = sortOn (posOffset . spanStart . locSpan) [e | e <- packageTags key pkg, isRaw e],
      (i, later) <- zip [0 :: Int ..] entries,
      earlier <- take 1 [e | e <- take i entries, overlapping (locGuard e) (locGuard later)]
    ]
  where
    isRaw e = case teRaw (locValue e) of
      Just _ -> True
      Nothing -> False

lineNo :: Span -> Text
lineNo sp = T.pack (show (posLine (spanStart sp) + 1))

missingTag :: LintInput -> [Diagnostic]
missingTag li =
  nub $
    concat
      [ [ diag "missing-required-tag" (anchor pkg) (what pkg <> " has no " <> pretty key <> " tag")
        | key <- required pkg,
          null (packageTags key pkg)
        ]
      | spec <- map snd (liSpecs li),
        pkg <- specPackages spec,
        not (pkgOpaque pkg),
        not (null (pkgTags pkg)) || pkgKey pkg /= MainKey
      ]
  where
    required pkg = if pkgKey pkg == MainKey then ["name", "version", "release", "summary", "license"] else ["summary"]
    pretty k = T.toUpper (T.take 1 k) <> T.drop 1 k
    anchor pkg = maybe startSpan id (pkgHeader pkg)
    startSpan = Span startPos startPos
    what pkg = if pkgKey pkg == MainKey then "the main package" else "%package " <> showValue (pkgName pkg)

missingDescription :: LintInput -> [Diagnostic]
missingDescription li =
  nub
    [ diag "missing-description" (maybe (Span startPos startPos) id (pkgHeader pkg)) (label pkg <> " has no %description")
    | spec <- map snd (liSpecs li),
      not (null (specSections spec)) || not (null (concatMap pkgTags (specPackages spec))),
      pkg <- specPackages spec,
      pkgKey pkg /= MainKey || not (null (pkgTags pkg)),
      not (pkgOpaque pkg),
      not (any (\s -> scKind s == SDescription && scPackage s == pkgKey pkg) (specSections spec))
    ]
  where
    label pkg = if pkgKey pkg == MainKey then "the main package" else "%package " <> showValue (pkgName pkg)

unknownPackage :: LintInput -> [Diagnostic]
unknownPackage li =
  nub
    [ diag "section-for-unknown-package" (scHeader s) ("%" <> sectionName (scKind s) <> " is for " <> showValue (scPackageName s) <> ", but no %package declares it")
    | spec <- map snd (liSpecs li),
      s <- specSections spec,
      scKind s /= SPackage,
      scPackage s /= MainKey,
      not (any ((== scPackage s) . pkgKey) (specPackages spec)),
      isKnown (scPackageName s)
    ]

duplicateSource :: LintInput -> [Diagnostic]
duplicateSource li =
  nub
    [ diag "duplicate-source-number" (locSpan b) (kind b <> show' (srcNumber (locValue b)) <> " is already defined on line " <> lineNo (locSpan a))
    | spec <- map snd (liSpecs li),
      let srcs = specSources spec,
      (i, b) <- zip [0 :: Int ..] srcs,
      a <- take 1 [a | a <- take i srcs, srcPatch (locValue a) == srcPatch (locValue b), srcNumber (locValue a) == srcNumber (locValue b), overlapping (locGuard a) (locGuard b)]
    ]
  where
    kind x = if srcPatch (locValue x) then "Patch" else "Source"
    show' = T.pack . show

macroInComment :: LintInput -> [Diagnostic]
macroInComment li =
  [ (diag "macro-in-comment" sp ("rpm expands " <> renderMacro m <> " even though it's in a comment"))
      { diagFixes = [Fix "escape the macros as %%" [TextEdit (lineSpan l) (escaped indent fs)] True]
      }
  | l@Line {lineBody = Comment indent fs} <- treeLines (liTree li),
    not (isDirective fs),
    (sp, m) <- take 1 [x | (p, fs') <- lineFrags l, x <- macroSpans p fs'],
    hasMacro fs
  ]
  where
    hasMacro = any (\case Macro _ -> True; _ -> False)
    isDirective fs = "rozbir:" `T.isInfixOf` renderFrags fs
    escaped indent fs = indent <> T.concat (map esc fs)
    esc = \case
      Macro m -> "%" <> renderMacro m
      f -> renderFrags [f]

newerRpm :: LintInput -> [Diagnostic]
newerRpm li =
  concat
    [ case lineBody l of
        Header h
          | secKind h == SConf -> [d l "%conf needs rpm 4.20"]
          | isBuildSection (secKind h), any (`elem` ["-a", "-p"]) (T.words (renderFrags (secArgs h))) -> [d l (secKeyword h <> " -a/-p needs rpm 4.20")]
        Tag t
          | preambleLike sec,
            T.toLower (tagName t) `elem` ["buildsystem", "buildoption"] ->
              [d l (tagName t <> " needs rpm 4.20")]
        _ -> []
    | (sec, l) <- linesWithSection (liTree li)
    ]
  where
    d l msg = diag "needs-newer-rpm" (lineSpan l) (msg <> ", but EL10 ships rpm 4.19")

patchN :: LintInput -> [Diagnostic]
patchN li =
  [ (diag "patchn-obsolete" sp ("%patch" <> n <> " was removed in rpm 4.20"))
      { diagFixes = [Fix ("use %patch -P " <> n) [TextEdit sp ("%patch -P " <> n)] True]
      }
  | (Just SPrep, l) <- linesWithSection (liTree li),
    (p, fs) <- lineFrags l,
    (sp, Bare "" name) <- macroSpans p fs,
    Just n <- [T.stripPrefix "patch" name],
    not (T.null n),
    T.all isDigit n
  ]

changelogDate :: LintInput -> [Diagnostic]
changelogDate li =
  nub $
    concat
      [ [diag "changelog-date" sp msg | (sp, msg) <- specChangelogProblems spec]
          <> [ diag "changelog-date" (entrySpan e) (formatDay day <> " was a " <> weekdayAbbrev day <> ", not a " <> ehWeekday (entryHeader e))
             | e <- specChangelog spec,
               Just day <- [ehDate (entryHeader e)],
               not (ehWeekdayMatches (entryHeader e))
             ]
      | spec <- firstSpec li
      ]

formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%b %-d %Y"

weekdayAbbrev :: Day -> Text
weekdayAbbrev = T.pack . formatTime defaultTimeLocale "%a"

changelogOrder :: LintInput -> [Diagnostic]
changelogOrder li =
  [ diag "changelog-order" (entrySpan b) "this entry is newer than the one before it, but entries go newest first"
  | spec <- firstSpec li,
    let dated = mapMaybe (\e -> (,e) <$> ehDate (entryHeader e)) (specChangelog spec),
    ((da, _), (db, b)) <- zip dated (drop 1 dated),
    db > da
  ]

evalError :: LintInput -> [Diagnostic]
evalError li =
  nub
    [ diag "eval-error" sp (emText m)
    | spec <- map snd (liSpecs li),
      m <- specMessages spec,
      emSeverity m == Error,
      Just sp <- [emSpan m]
    ]

outsideSection :: LintInput -> [Diagnostic]
outsideSection li =
  [ diag "line-outside-section" (lineSpan l) "this line comes after %end, so it belongs to no section"
  | (Just SEnd, l) <- linesWithSection (liTree li),
    case lineBody l of
      Blank _ -> False
      Comment _ _ -> False
      Header _ -> False
      Define _ -> False
      Cond _ -> False
      _ -> True
  ]

undefinedMacro :: LintInput -> [Diagnostic]
undefinedMacro li =
  nub
    [ d
    | spec <- map snd (liSpecs li),
      let defined = Map.keys (envMacros (specEnv spec)),
      (n, sp) <- specUndefined spec,
      d <- explain spec defined n (narrow sp n)
    ]
  where
    narrow sp n = case [s | l <- treeLines (liTree li), lineSpan l == sp, (p, fs) <- lineFrags l, (s, m) <- macroSpans p fs, macroName m == Just n] of
      s : _ -> s
      [] -> sp
    tagLines = [(T.toLower (tagName t), lineSpan l) | l@Line {lineBody = Tag t} <- treeLines (liTree li)]
    explain spec defined n sp
      | Just later <- lookup n tagLines,
        posOffset (spanStart later) > posOffset (spanStart sp) =
          [diag "undefined-macro" sp ("%" <> n <> " is used before the tag on line " <> lineNo later <> " defines it, so rpm leaves it as text")]
      | (c : _) <- [x | x <- defined, x /= n, T.toLower x == T.toLower n] =
          [suggest sp n c]
      | T.length n >= 5,
        (c : _) <- [x | x <- defined, T.length x >= 5, oneEdit n x] =
          [suggest sp n c]
      | otherwise = const [] spec
    suggest sp n c =
      (diag "undefined-macro" sp ("%" <> n <> " isn't defined, so rpm leaves it as text. did you mean %" <> c <> "?"))
        { diagFixes = [Fix ("use %" <> c) [TextEdit sp (T.replace n c (renderMacroAt sp))] False]
        }
    renderMacroAt sp = T.take (posOffset (spanEnd sp) - posOffset (spanStart sp)) (T.drop (posOffset (spanStart sp)) (liSource li))

oneEdit :: Text -> Text -> Bool
oneEdit a b
  | a == b = False
  | T.length a == T.length b = let diffs = [(x, y) | (x, y) <- T.zip a b, x /= y] in length diffs == 1 || swapped
  | abs (T.length a - T.length b) == 1 = deletion (T.unpack (longer a b)) (T.unpack (shorter a b))
  | otherwise = False
  where
    longer x y = if T.length x > T.length y then x else y
    shorter x y = if T.length x > T.length y then y else x
    swapped = case [i | (i, (x, y)) <- zip [0 :: Int ..] (T.zip a b), x /= y] of
      [i, j] -> j == i + 1 && T.index a i == T.index b j && T.index a j == T.index b i
      _ -> False
    deletion (x : xs) (y : ys)
      | x == y = deletion xs ys
      | otherwise = xs == (y : ys)
    deletion [_] [] = True
    deletion _ _ = False

bcondUnused :: LintInput -> [Diagnostic]
bcondUnused li =
  [ diag "bcond-unused" (lineSpan l) ("nothing checks %{with " <> defName d <> "}, so the bcond does nothing")
  | l@Line {lineBody = Define d} <- treeLines (liTree li),
    defKind d `elem` [DefBcond, DefBcondWith, DefBcondWithout],
    defName d `notElem` used
  ]
  where
    used = concatMap refs [m | l <- treeLines (liTree li), (p, fs) <- lineFrags l, (_, m) <- macroSpans p fs]
    refs = \case
      Braced _ n (Just (SpaceArg fs)) | n `elem` ["with", "without"] -> [T.strip (renderFrags fs)]
      Bare _ n | Just b <- T.stripPrefix "with_" n -> [b]
      Braced _ n _ | Just b <- T.stripPrefix "with_" n -> [b]
      _ -> []

duplicateDependency :: LintInput -> [Diagnostic]
duplicateDependency li =
  nub
    [ diag "duplicate-dependency" (locSpan b) (label <> " " <> locValue b <> " is already listed on line " <> lineNo (locSpan a))
    | spec <- map snd (liSpecs li),
      pkg <- specPackages spec,
      key <- ["requires", "buildrequires", "provides", "conflicts", "obsoletes", "recommends", "suggests", "buildconflicts"],
      let deps = [(qualOf e, Located dep (locSpan e) (locGuard e)) | e <- packageTags key pkg, Just t <- [knownText (teValue (locValue e))], dep <- splitDeps t],
      (i, (q, b)) <- zip [0 :: Int ..] deps,
      a <- take 1 [a | (q', a) <- take i deps, q' == q, locValue a == locValue b, locSpan a /= locSpan b, overlapping (locGuard a) (locGuard b)],
      let label = maybe (prettyKey key) (\x -> prettyKey key <> x) q
    ]
  where
    qualOf e = teQualifier (locValue e)
    prettyKey k = case k of
      "buildrequires" -> "BuildRequires"
      "buildconflicts" -> "BuildConflicts"
      _ -> T.toUpper (T.take 1 k) <> T.drop 1 k
