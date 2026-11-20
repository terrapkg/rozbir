module LintSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Rozbir.Edit (apply)
import Rozbir.Eval (newEnv, unknownTarget)
import Rozbir.Lint
import Rozbir.Syntax.Parse (parse)
import Test.Hspec

everything :: LintConfig
everything = defaultConfig {lcRuleSets = Set.fromList [minBound .. maxBound]}

lintWith :: LintConfig -> Text -> [Diagnostic]
lintWith cfg src = lintTree cfg (newEnv False) [unknownTarget] Map.empty (parse src)

rulesOf :: Text -> [RuleId]
rulesOf = map diagRule . lintWith everything

base :: [Text]
base = ["Name: x", "Version: 1", "Release: 1%{?dist}", "Summary: s", "License: MIT", "Packager: p", "", "%description", "d", ""]

withBase :: [Text] -> Text
withBase extra = T.unlines (base <> extra)

withPreamble :: [Text] -> Text
withPreamble extra = T.unlines (take 6 base <> extra <> drop 6 base)

catches :: RuleId -> Text -> Spec
catches rid src = it ("catches " <> T.unpack rid) (rulesOf src `shouldContain` [rid])

passes :: RuleId -> Text -> Spec
passes rid src = it ("lets through what " <> T.unpack rid <> " allows") (rulesOf src `shouldNotContain` [rid])

fixes :: RuleId -> Text -> Text -> Spec
fixes rid src expected = it ("fixes " <> T.unpack rid) $
  case [f | d <- lintWith everything src, diagRule d == rid, f <- diagFixes d] of
    f : _ -> apply (fixEdits f) src `shouldBe` expected
    [] -> expectationFailure "no fix offered"

spec :: Spec
spec = describe "Rozbir.Lint" $ do
  it "has no findings on a clean spec" $ rulesOf (withBase ["%files", "", "%changelog"]) `shouldBe` []
  catches "unknown-tag" (withPreamble ["Nmae: y"])
  passes "unknown-tag" (withPreamble ["Source12: y", "BuildRequires: z"])
  catches "duplicate-tag" (withPreamble ["Version: 2"])
  passes "duplicate-tag" "%if 0%{?rhel}\nVersion: 1\n%else\nVersion: 2\n%endif\n"
  catches "missing-required-tag" "Name: x\n%description\nd\n"
  catches "missing-description" (withBase ["%package devel", "Summary: y"])
  catches "section-for-unknown-package" (withBase ["%files devel"])
  passes "section-for-unknown-package" (withBase ["%package devel", "Summary: y", "%description devel", "y", "%files devel"])
  catches "duplicate-source-number" (withPreamble ["Source1: a", "Source1: b"])
  catches "macro-in-comment" (withBase ["# %global foo 1"])
  fixes "macro-in-comment" "# %{x} and %y\n" "# %%{x} and %%y\n"
  catches "needs-newer-rpm" (withBase ["%conf"])
  catches "patchn-obsolete" (withBase ["%prep", "%patch0 -p1"])
  fixes "patchn-obsolete" "%prep\n%patch0 -p1\n" "%prep\n%patch -P 0 -p1\n"
  catches "changelog-date" (withBase ["%changelog", "* Mon Nov 20 2026 A - 1-1", "- x"])
  catches "changelog-order" (withBase ["%changelog", "* Mon Jan 05 2026 A", "- x", "* Fri Nov 20 2026 A", "- y"])
  catches "unclosed-if" (withBase ["%if 1"])
  catches "eval-error" (withBase ["%if 1 +", "%endif"])
  catches "deprecated-tag" (withPreamble ["BuildRoot: /tmp"])
  fixes "deprecated-tag" "BuildPreReq: x\n" "BuildRequires: x\n"
  catches "clean-section" (withBase ["%clean"])
  catches "hardcoded-path" (withBase ["%files", "/usr/bin/x"])
  passes "hardcoded-path" (withBase ["%files", "%{_bindir}/x"])
  catches "mixed-buildroot" (withBase ["%install", "rm -rf $RPM_BUILD_ROOT", "mkdir %{buildroot}", "mkdir %{buildroot}/a"])
  catches "rpm-buildroot-usage" (withBase ["%build", "ls %{buildroot}"])
  catches "macro-in-changelog" (withBase ["%changelog", "* Fri Nov 20 2026 A", "- use %{name}"])
  passes "macro-in-changelog" (withBase ["%changelog", "%autochangelog"])
  catches "patch-not-applied" (withPreamble ["Patch1: a.patch"] <> "%prep\n%setup -q\n")
  passes "patch-not-applied" (withPreamble ["Patch1: a.patch"] <> "%prep\n%autosetup -p1\n")
  catches "setup-not-quiet" (withBase ["%prep", "%setup -n x"])
  catches "summary-ended-with-dot" "Summary: A thing.\n"
  fixes "summary-ended-with-dot" "Summary: A thing.\n" "Summary: A thing\n"
  catches "make-check-outside-check-section" (withBase ["%build", "make check"])
  catches "mixed-tabs-spaces" "Name:\tx\nVersion:  1\n"
  catches "terra-packager" "Name: x\n"
  catches "terra-dist-release" "Release: 1\n"
  fixes "terra-dist-release" "Release: 1\n" "Release: 1%{?dist}\n"
  passes "terra-dist-release" "Release: %autorelease\n"
  catches "terra-define-global" "%define foo 1\n"
  catches "terra-version-editable" "Version: %{pyver}\n"
  catches "bcond-unused" (withPreamble ["%bcond docs 1"])
  passes "bcond-unused" (withPreamble ["%bcond docs 1", "%if %{with docs}", "BuildRequires: d", "%endif"])
  catches "duplicate-dependency" (withPreamble ["Requires: a", "Requires: b, a"])
  passes "duplicate-dependency" (withPreamble ["Requires: a", "Requires(post): a"])
  catches "unversioned-obsoletes" (withPreamble ["Obsoletes: old"])
  passes "unversioned-obsoletes" (withPreamble ["Obsoletes: old < 2"])
  catches "defattr" (withBase ["%files", "%defattr(-,root,root,-)"])
  catches "rm-buildroot" (withBase ["%install", "rm -rf %{buildroot}"])
  catches "terra-py3-build" (withBase ["%build", "%py3_build"])
  catches "terra-insecure-url" (withPreamble ["URL: http://example.com"])
  describe "undefined-macro" $ do
    let closedRules src = map diagRule (lintTree everything (newEnv True) [unknownTarget] Map.empty (parse src))
    it "catches a case typo" $ closedRules "URL: https://x\nSource0: %{URL}/a.tar.gz\n" `shouldContain` ["undefined-macro"]
    it "catches a tag macro used too early" $ closedRules "Summary: %{name}\nName: n\n" `shouldContain` ["undefined-macro"]
    it "ignores %files directives" $ closedRules "%files\n%doc README\n%dir /x\n" `shouldNotContain` ["undefined-macro"]
    it "ignores rpmbuild's own macros" $ closedRules "%install\nmkdir %{buildroot}\n" `shouldNotContain` ["undefined-macro"]
    it "stays quiet in an open environment" $ rulesOf "URL: x\nSource0: %{URL}/a\n" `shouldNotContain` ["undefined-macro"]
  describe "directives" $ do
    it "suppresses the next line" $
      rulesOf (withPreamble ["# rozbir: ignore[unknown-tag] it's fine", "Nmae: y"]) `shouldNotContain` ["unknown-tag"]
    it "suppresses a whole file" $
      rulesOf (T.unlines ["# rozbir: ignore-file[unknown-tag]"] <> withPreamble ["Nmae: y"]) `shouldNotContain` ["unknown-tag"]
    it "reports a directive that suppresses nothing" $
      rulesOf (withPreamble ["# rozbir: ignore[unknown-tag]", "Version: x"]) `shouldContain` ["unused-ignore"]
    it "reports an unknown rule in a directive" $
      rulesOf (withBase ["# rozbir: ignore[no-such-rule]"]) `shouldContain` ["unused-ignore"]
  describe "config" $ do
    it "leaves the terra rules off by default" $
      map diagRule (lintWith defaultConfig "Release: 1\n") `shouldNotContain` ["terra-dist-release"]
    it "disables a rule" $
      map diagRule (lintWith everything {lcOverrides = Map.fromList [("terra-dist-release", Nothing)]} "Release: 1\n") `shouldNotContain` ["terra-dist-release"]
    it "changes a severity" $
      [diagSeverity d | d <- lintWith everything {lcOverrides = Map.fromList [("terra-dist-release", Just Error)]} "Release: 1\n", diagRule d == "terra-dist-release"] `shouldBe` [Error]
