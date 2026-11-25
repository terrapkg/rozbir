-- | rpmlint's spec checks, run on the parsed spec so they see every @%if@
-- branch. on by default.
module Rozbir.Lint.Rpmlint (rules) where

import Data.Char (isSpace)
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Rozbir.Edit (TextEdit (..), tagValueSpan)
import Rozbir.Eval.Types
import Rozbir.Lint.Types
import Rozbir.Query (dependencies)
import Rozbir.Spec
import Rozbir.Syntax
import Rozbir.Syntax.Render (lineFrags, macroSpans, renderBody, renderFrags, renderMacro)

-- | every rpmlint rule
rules :: [Rule]
rules =
  [ Rule "deprecated-tag" Rpmlint Warn "BuildRoot, Group, PreReq and BuildPreReq do nothing or have better spellings" deprecatedTag,
    Rule "clean-section" Rpmlint Warn "%clean isn't needed, rpm cleans the buildroot itself" cleanSection,
    Rule "hardcoded-path" Rpmlint Info "a path spelled out where a directory macro exists" hardcodedPath,
    Rule "mixed-tabs-spaces" Rpmlint Warn "the spec aligns with both tabs and spaces" mixedTabs,
    Rule "mixed-buildroot" Rpmlint Warn "both $RPM_BUILD_ROOT and %{buildroot} are used" mixedBuildroot,
    Rule "rpm-buildroot-usage" Rpmlint Error "the buildroot is touched in %prep or %build" buildrootUsage,
    Rule "macro-in-changelog" Rpmlint Warn "rpm expands macros in %changelog, escape them as %%" macroInChangelog,
    Rule "patch-not-applied" Rpmlint Warn "a Patch is declared but %prep never applies it" patchNotApplied,
    Rule "setup-not-quiet" Rpmlint Info "%setup without -q lists every file it unpacks" setupNotQuiet,
    Rule "summary-ended-with-dot" Rpmlint Warn "Summary shouldn't end with a period" summaryDot,
    Rule "summary-too-long" Rpmlint Warn "Summary is longer than 79 characters" summaryLong,
    Rule "description-line-too-long" Rpmlint Warn "a %description line is longer than 79 characters" descriptionLong,
    Rule "make-check-outside-check-section" Rpmlint Warn "make check runs outside %check" makeCheck,
    Rule "use-of-RPM_SOURCE_DIR" Rpmlint Warn "use %{SOURCEn} instead of $RPM_SOURCE_DIR or %{_sourcedir}" sourceDir,
    Rule "unversioned-obsoletes" Rpmlint Warn "an Obsoletes without a version also obsoletes every future release of that package" unversionedObsoletes,
    Rule "defattr" Rpmlint Info "%defattr(-,root,root,-) is the default and can go" defattr,
    Rule "rm-buildroot" Rpmlint Info "rpm cleans the buildroot before %install, so removing it there does nothing" rmBuildroot
  ]

deprecatedTag :: LintInput -> [Diagnostic]
deprecatedTag li =
  [ (diag "deprecated-tag" (lineSpan l) msg) {diagFixes = fixes}
  | (sec, l@Line {lineBody = Tag t}) <- linesWithSection (liTree li),
    preambleLike sec,
    let key = T.toLower (tagName t)
        nameSpan = Span (advancePos (spanStart (lineSpan l)) (tagIndent t)) (advancePos (spanStart (lineSpan l)) (tagIndent t <> tagName t)),
    (msg, fixes) <- case key of
      "buildroot" -> [("BuildRoot is ignored, rpm sets the buildroot itself", [Fix "remove BuildRoot" [TextEdit (lineFullSpan l) ""] True])]
      "group" -> [("Group is ignored by current rpm and Fedora", [Fix "remove Group" [TextEdit (lineFullSpan l) ""] True])]
      "buildprereq" -> [("BuildPreReq is deprecated, use BuildRequires", [Fix "rename to BuildRequires" [TextEdit nameSpan "BuildRequires"] True])]
      "prereq" -> [("PreReq is deprecated, use Requires(pre) or Requires(post)", [Fix "rename to Requires(pre)" [TextEdit nameSpan "Requires(pre)"] False])]
      _ -> []
  ]

cleanSection :: LintInput -> [Diagnostic]
cleanSection li =
  [ diag "clean-section" (lineSpan l) "%clean isn't needed, rpm cleans the buildroot itself"
  | l@Line {lineBody = Header h} <- treeLines (liTree li),
    secKind h == SClean
  ]

pathMacros :: [(Text, Text)]
pathMacros =
  [ ("/usr/share/man", "%{_mandir}"),
    ("/usr/share/info", "%{_infodir}"),
    ("/usr/share/doc", "%{_docdir}"),
    ("/usr/share", "%{_datadir}"),
    ("/usr/bin", "%{_bindir}"),
    ("/usr/sbin", "%{_sbindir}"),
    ("/usr/lib64", "%{_libdir}"),
    ("/usr/libexec", "%{_libexecdir}"),
    ("/usr/include", "%{_includedir}"),
    ("/etc", "%{_sysconfdir}"),
    ("/var/lib", "%{_sharedstatedir}"),
    ("/var", "%{_localstatedir}")
  ]

hardcodedPath :: LintInput -> [Diagnostic]
hardcodedPath li =
  [ (diag "hardcoded-path" sp ("use " <> macro <> " instead of " <> path))
      { diagFixes = [Fix ("replace with " <> macro) [TextEdit sp macro] False]
      }
  | (sec, l) <- linesWithSection (liTree li),
    sec `elem` [Just SFiles, Just SInstall],
    (p, fs) <- lineFrags l,
    (sp, path, macro) <- take 1 (pathHits p fs)
  ]
  where
    pathHits p fs =
      [ (Span s (advancePos s path), path, macro)
      | (Span fragStart _, Lit t) <- fragsWithSpans p fs,
        (offset, path, macro) <- take 1 (findPaths t),
        let s = advancePos fragStart (T.take offset t)
      ]
    findPaths t =
      [ (i, path, macro)
      | i <- candidates t,
        (path, macro) <- take 1 [x | x@(path, _) <- pathMacros, path `T.isPrefixOf` T.drop i t, boundaryAfter (T.drop (i + T.length path) t)]
      ]
    candidates t = [i | (i, c) <- zip [0 ..] (T.unpack t), c == '/', i == 0 || boundaryBefore (T.index t (i - 1))]
    boundaryBefore c = isSpace c || c `elem` ("\"'=" :: String)
    boundaryAfter rest = case T.uncons rest of
      Nothing -> True
      Just (c, _) -> c == '/' || isSpace c || c == '"' || c == '\''

fragsWithSpans :: Pos -> [Frag] -> [(Span, Frag)]
fragsWithSpans p = \case
  [] -> []
  f : fs -> let e = advancePos p (renderFrags [f]) in (Span p e, f) : fragsWithSpans e fs

mixedTabs :: LintInput -> [Diagnostic]
mixedTabs li = case (firstWith '\t', firstWith ' ') of
  (Just (tl, _), Just (sl, _))
    | lineSpan tl == lineSpan sl ->
        [diag "mixed-tabs-spaces" (lineSpan tl) "this line lines up its value with both tabs and spaces"]
    | otherwise ->
        let (later, other, kind, otherKind) = if posOffset (spanStart (lineSpan tl)) > posOffset (spanStart (lineSpan sl)) then (tl, sl, "tabs", "spaces") else (sl, tl, "spaces", "tabs")
         in [diag "mixed-tabs-spaces" (lineSpan later) ("this line lines up its value with " <> kind <> ", but line " <> lineNo other <> " uses " <> otherKind)]
  _ -> []
  where
    alignment l = case lineBody l of
      Tag t -> [T.dropWhile (/= ':') (tagSep t)]
      Define d -> [defSep d]
      _ -> []
    firstWith c = listToMaybe [(l, ()) | l <- treeLines (liTree li), w <- alignment l, uses c w]
    uses c w
      | c == '\t' = T.any (== '\t') w
      | otherwise = "  " `T.isInfixOf` w
    lineNo l = T.pack (show (posLine (spanStart (lineSpan l)) + 1))

buildrootRefs :: Line -> ([Span], [Span])
buildrootRefs l =
  ( [sp | (p, fs) <- lineFrags l, (sp, m) <- macroSpans p fs, macroName m == Just "buildroot"],
    [ Span s (advancePos s "RPM_BUILD_ROOT")
    | (p, fs) <- lineFrags l,
      (Span fs0 _, Lit t) <- fragsWithSpans p fs,
      (pre, _) <- T.breakOnAll "RPM_BUILD_ROOT" t,
      "$" `T.isSuffixOf` pre || "${" `T.isSuffixOf` pre,
      let s = advancePos fs0 pre
    ]
  )

mixedBuildroot :: LintInput -> [Diagnostic]
mixedBuildroot li =
  let refs = map buildrootRefs (treeLines (liTree li))
      macros = concatMap fst refs
      vars = concatMap snd refs
   in if null macros || null vars
        then []
        else
          let (minority, word, usual) = if length vars <= length macros then (vars, "$RPM_BUILD_ROOT", "%{buildroot}") else (macros, "%{buildroot}", "$RPM_BUILD_ROOT")
           in [diag "mixed-buildroot" sp ("this uses " <> word <> ", but the rest of the spec uses " <> usual) | sp <- take 1 minority]

buildrootUsage :: LintInput -> [Diagnostic]
buildrootUsage li =
  [ diag "rpm-buildroot-usage" sp ("the buildroot shouldn't be touched in %" <> sectionName k)
  | (Just k, l) <- linesWithSection (liTree li),
    k `elem` [SPrep, SBuild],
    let (a, b) = buildrootRefs l,
    sp <- take 1 (a <> b)
  ]

macroInChangelog :: LintInput -> [Diagnostic]
macroInChangelog li =
  [ (diag "macro-in-changelog" sp ("rpm expands " <> renderMacro m <> " in the changelog"))
      { diagFixes = [Fix "escape as %%" [TextEdit sp ("%" <> renderMacro m)] True]
      }
  | (Just SChangelog, l) <- linesWithSection (liTree li),
    case lineBody l of
      Header _ -> False
      Body fs -> not (macroOnly fs)
      _ -> True,
    (p, fs) <- lineFrags l,
    (sp, m) <- [(s, m) | (s, Macro m) <- fragsWithSpans p fs]
  ]

macroOnly :: [Frag] -> Bool
macroOnly fs = case filter (not . blankLit) fs of
  [Macro _] -> True
  _ -> False
  where
    blankLit = \case
      Lit t -> T.all isSpace t
      _ -> False

patchNotApplied :: LintInput -> [Diagnostic]
patchNotApplied li = case firstSpec li of
  [] -> []
  spec : _ ->
    let prepText = T.unlines [renderBody (lineBody l) | (Just SPrep, l) <- linesWithSection (liTree li)]
        ws = T.words (T.map (\c -> if c == '{' || c == '}' then ' ' else c) prepText)
        appliesAll = autosetupApplies ws || any (\w -> "autopatch" `T.isInfixOf` w || "forgeautosetup" `T.isInfixOf` w) ws || "%autosetup" `T.isInfixOf` prepText && not (T.isInfixOf "-N" (autosetupLine prepText))
        applied n = any (`elem` ws) (map (<> T.pack (show n)) ["%patch", "%PATCH", "PATCH", "%P:"]) || explicit n ws || T.isInfixOf ("%{PATCH" <> T.pack (show n) <> "}") prepText
     in if appliesAll || T.null prepText
          then []
          else
            nub
              [ diag "patch-not-applied" (locSpan p) ("Patch" <> T.pack (show n) <> " is never applied in %prep")
              | p <- specSources spec,
                srcPatch (locValue p),
                let n = srcNumber (locValue p),
                not (applied n)
              ]
  where
    autosetupApplies ws = "%autosetup" `elem` ws && "-N" `notElem` ws
    autosetupLine t = T.unwords [l | l <- T.lines t, "%autosetup" `T.isInfixOf` l]
    explicit n ws = any (matches n) (zip ws (drop 1 ws)) || any (\w -> T.stripPrefix "-P" w == Just (T.pack (show n))) ws
    matches n (a, b) = (a == "-P" || a == "%patch") && b == T.pack (show n)

setupNotQuiet :: LintInput -> [Diagnostic]
setupNotQuiet li =
  [ diag "setup-not-quiet" sp "%setup without -q lists every file it unpacks"
  | (Just SPrep, l) <- linesWithSection (liTree li),
    Body fs <- [lineBody l],
    (p, _) <- lineFrags l,
    (sp, Macro (Bare "" "setup")) <- take 1 [x | x@(_, Macro _) <- fragsWithSpans p fs],
    let args = T.words (renderFrags fs),
    not (any (\w -> "-" `T.isPrefixOf` w && not ("--" `T.isPrefixOf` w) && T.any (== 'q') w) args)
  ]

summaries :: LintInput -> [(Line, TagLine, Text)]
summaries li =
  [ (l, t, v)
  | (sec, l@Line {lineBody = Tag t}) <- linesWithSection (liTree li),
    preambleLike sec,
    T.toLower (tagName t) == "summary",
    let v = renderFrags (tagValue t)
  ]

summaryDot :: LintInput -> [Diagnostic]
summaryDot li =
  [ (diag "summary-ended-with-dot" (lineSpan l) "Summary shouldn't end with a period")
      { diagFixes = [Fix "remove the period" [TextEdit (tagValueSpan l t) (T.dropEnd 1 v)] True]
      }
  | (l, t, v) <- summaries li,
    "." `T.isSuffixOf` v,
    not ("..." `T.isSuffixOf` v),
    not (lastWordAbbrev v)
  ]
  where
    lastWordAbbrev v = case reverse (T.words v) of
      w : _ -> T.length w <= 4 && T.count "." w > 1
      [] -> False

summaryLong :: LintInput -> [Diagnostic]
summaryLong li =
  nub
    [ diag "summary-too-long" (locSpan e) ("Summary is " <> T.pack (show (T.length t)) <> " characters, keep it under 80")
    | spec <- map snd (liSpecs li),
      pkg <- specPackages spec,
      e <- packageTags "summary" pkg,
      Just t <- [knownText (teValue (locValue e))],
      T.length t > 79
    ]

descriptionLong :: LintInput -> [Diagnostic]
descriptionLong li =
  nub
    [ diag "description-line-too-long" (locSpan l) ("this line is " <> T.pack (show (T.length t)) <> " characters, wrap it at 79")
    | spec <- map snd (liSpecs li),
      s <- specSections spec,
      scKind s == SDescription,
      l <- scLines s,
      Just t <- [knownText (locValue l)],
      T.length t > 79,
      length (T.words t) > 1
    ]

makeCheck :: LintInput -> [Diagnostic]
makeCheck li =
  [ diag "make-check-outside-check-section" (lineSpan l) ("make check runs in %" <> sectionName k <> ", tests belong in %check")
  | (Just k, l@Line {lineBody = Body fs}) <- linesWithSection (liTree li),
    k /= SCheck,
    isBuildSection k,
    let ws = T.words (renderFrags fs),
    any (\(a, b) -> isMake a && b == "check") (zip ws (drop 1 ws))
  ]
  where
    isMake w = w `elem` ["make", "%make_build", "%{make_build}", "%__make", "%{__make}", "%make"] || "/make" `T.isSuffixOf` w

sourceDir :: LintInput -> [Diagnostic]
sourceDir li =
  [ diag "use-of-RPM_SOURCE_DIR" (lineSpan l) "use %{SOURCEn} instead of reaching into the source directory"
  | (Just k, l) <- linesWithSection (liTree li),
    k /= SPackage,
    let raw = renderBody (lineBody l),
    "$RPM_SOURCE_DIR" `T.isInfixOf` raw || "${RPM_SOURCE_DIR}" `T.isInfixOf` raw || any (\(_, m) -> macroName m == Just "_sourcedir") [x | (p, fs) <- lineFrags l, x <- macroSpans p fs]
  ]

unversionedObsoletes :: LintInput -> [Diagnostic]
unversionedObsoletes li =
  nub
    [ diag "unversioned-obsoletes" (locSpan d) ("Obsoletes: " <> locValue d <> " has no version, so it obsoletes every future " <> locValue d <> " too")
    | spec <- map snd (liSpecs li),
      pkg <- specPackages spec,
      d <- dependencies "obsoletes" pkg,
      not (T.any (`elem` ['<', '>', '=']) (locValue d)),
      not ("(" `T.isPrefixOf` locValue d)
    ]

deleteLine :: Text -> Line -> Fix
deleteLine title l = Fix title [TextEdit (lineFullSpan l) ""] True

defattr :: LintInput -> [Diagnostic]
defattr li =
  [ (diag "defattr" (lineSpan l) "%defattr(-,root,root,-) is already the default") {diagFixes = [deleteLine "remove %defattr" l]}
  | (Just SFiles, l@Line {lineBody = Body fs}) <- linesWithSection (liTree li),
    let t = T.filter (not . isSpace) (renderFrags fs),
    t `elem` ["%defattr(-,root,root,-)", "%defattr(-,root,root)", "%defattr(-,root,root,0755)"]
  ]

rmBuildroot :: LintInput -> [Diagnostic]
rmBuildroot li =
  [ (diag "rm-buildroot" (lineSpan l) "rpm already cleans the buildroot before %install") {diagFixes = [deleteLine "remove the line" l]}
  | (Just SInstall, l@Line {lineBody = Body fs}) <- linesWithSection (liTree li),
    let ws = T.words (T.replace "\"" "" (renderFrags fs)),
    case ws of
      ["rm", flags, target] -> T.all (`elem` ['-', 'r', 'f', 'R']) flags && T.dropWhileEnd (== '/') target `elem` ["%{buildroot}", "%buildroot", "$RPM_BUILD_ROOT", "${RPM_BUILD_ROOT}"]
      _ -> False
  ]
