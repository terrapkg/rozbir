-- | Terra's packaging conventions, such as a Packager tag and a Version that
-- @update.rhai@ can change. off unless the terra ruleset is enabled.
module Rozbir.Lint.Terra (rules) where

import Data.Char (isDigit)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Rozbir.Edit (TextEdit (..), preambleTags, tagValueSpan)
import Rozbir.Lint.Types
import Rozbir.Query (definitions, macroReferences)
import Rozbir.Syntax
import Rozbir.Syntax.Render (lineFrags, macroSpans, renderFrags)

-- | every Terra rule
rules :: [Rule]
rules =
  [ Rule "terra-packager" Terra Info "Terra credits whoever maintains a package with a Packager tag" packager,
    Rule "terra-dist-release" Terra Warn "Release should end in %{?dist} so builds for different releases don't collide" distRelease,
    Rule "terra-define-global" Terra Info "prefer %global to %define outside parametric macros, like Fedora" defineGlobal,
    Rule "terra-version-editable" Terra Warn "update.rhai can only set Version through literal text or a %global in the spec" versionEditable,
    Rule "terra-py3-build" Terra Warn "%py3_build and %py3_install are deprecated, use the pyproject macros" py3Build,
    Rule "terra-insecure-url" Terra Info "a URL or Source uses http:// where https:// probably works" insecureUrl
  ]

packager :: LintInput -> [Diagnostic]
packager li =
  [ diag "terra-packager" sp "the main package has no Packager tag"
  | let tags = preambleTags (liTree li),
    not (null tags),
    not (any (\(_, t) -> T.toLower (tagName t) == "packager") tags),
    (l, _) <- take 1 [x | x@(_, t) <- tags, T.toLower (tagName t) == "name"],
    let sp = lineSpan l
  ]

distRelease :: LintInput -> [Diagnostic]
distRelease li =
  [ (diag "terra-dist-release" (lineSpan l) "Release doesn't include %{?dist}")
      { diagFixes = [Fix "append %{?dist}" [TextEdit (tagValueSpan l t) (v <> "%{?dist}")] True]
      }
  | (l, t) <- preambleTags (liTree li),
    T.toLower (tagName t) == "release",
    let refs = macroReferences (tagValue t)
        v = renderFrags (tagValue t),
    "dist" `notElem` refs,
    "autorelease" `notElem` refs,
    not (any (`Map.member` definitions (liTree li)) refs)
  ]

defineGlobal :: LintInput -> [Diagnostic]
defineGlobal li =
  [ (diag "terra-define-global" (lineSpan l) ("%" <> defName d <> " is defined with %define, use %global"))
      { diagFixes = [Fix "use %global" [TextEdit (Span s (advancePos s (defKeyword d))) "%global"] False]
      }
  | l@Line {lineBody = Define d} <- treeLines (liTree li),
    defKind d == DefDefine,
    defOpts d == Nothing,
    T.null (T.strip (defGap d)),
    let s = advancePos (spanStart (lineSpan l)) (defIndent d)
  ]

versionEditable :: LintInput -> [Diagnostic]
versionEditable li =
  nub
    [ diag "terra-version-editable" (lineSpan l) "Version only uses macros the spec doesn't define with %global, so update.rhai can't change it"
    | (l, t) <- preambleTags (liTree li),
      T.toLower (tagName t) == "version",
      let refs = macroReferences (tagValue t)
          defs = definitions (liTree li)
          hasLiteral = any (\case Lit x -> T.any (`notElem` (" \t" :: String)) x; _ -> False) (tagValue t),
      not hasLiteral,
      not (null refs),
      not (any (\r -> any ((== DefGlobal) . defKind) (Map.findWithDefault [] r defs)) refs)
    ]

py3Build :: LintInput -> [Diagnostic]
py3Build li =
  [ diag "terra-py3-build" sp ("%" <> n <> " is deprecated, use %pyproject_wheel and %pyproject_install")
  | (Just k, l) <- linesWithSection (liTree li),
    k `elem` [SBuild, SInstall],
    (p, fs) <- lineFrags l,
    (sp, m) <- macroSpans p fs,
    Just n <- [macroName m],
    n `elem` ["py3_build", "py3_install", "py_build", "py_install", "py3_build_wheel", "py3_build_egg"]
  ]

insecureUrl :: LintInput -> [Diagnostic]
insecureUrl li =
  [ (diag "terra-insecure-url" sp (tagName t <> " uses http://"))
      { diagFixes = [Fix "use https://" [TextEdit (Span (spanStart sp) (advancePos (spanStart sp) "http")) "https"] False]
      }
  | (sec, l@Line {lineBody = Tag t}) <- linesWithSection (liTree li),
    isPreambleLike sec,
    let key = T.toLower (T.dropWhileEnd isDigit (tagName t)),
    key `elem` ["url", "source", "patch", "bugurl", "vcs"],
    let sp = tagValueSpan l t,
    "http://" `T.isPrefixOf` renderFrags (tagValue t)
  ]
