module QuerySpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Rozbir
import Rozbir.Eval (Reason (..), Tri (..), showValue)
import Rozbir.Spec
import Test.Hspec hiding (Spec)
import Test.Hspec qualified as H

sample :: Text
sample =
  T.unlines
    [ "%global major 1",
      "%global minor 2",
      "%global ver %{major}.%{minor}",
      "%bcond docs 1",
      "",
      "Name:           demo",
      "Version:        %{ver}",
      "Release:        3%{?dist}",
      "Epoch:          2",
      "Summary:        A demo",
      "License:        MIT",
      "Source0:        https://example.com/demo-%{version}.tar.gz",
      "Source:         extra.txt",
      "Patch:          fix.patch",
      "BuildRequires:  gcc, pkgconfig(foo) >= 1.0 (a or b)",
      "%if 0%{?fedora}",
      "BuildRequires:  fedora-only",
      "%endif",
      "",
      "%description",
      "Demo.",
      "",
      "%package devel",
      "Summary:        Headers",
      "",
      "%description devel",
      "Headers.",
      "",
      "%files devel",
      "",
      "%changelog",
      "* Thu Nov 19 2026 Someone <a@b.c> - 2:1.2-3",
      "- first"
    ]

f44 :: Target
f44 = Target (Just "x86_64") (Just "linux") [("dist", Just ".fc44"), ("fedora", Just "44")]

spec :: H.Spec
spec = describe "Rozbir.Query" $ do
  let tree = parse sample
      open = resolve (newEnv False) unknownTarget tree
      known' = resolve (newEnv False) f44 tree
  it "reads the name" $ name open `shouldBe` Resolved "demo"
  it "can't finish the EVR without a dist" $ evr open `shouldBe` Unresolved [Undefined "dist"]
  it "resolves the EVR for a target" $ evr known' `shouldBe` Resolved (EVR 2 "1.2" "3.fc44")
  it "follows %global chains for versionInputs" $ versionInputs tree `shouldBe` ["ver", "major", "minor"]
  it "numbers sources" $
    [(srcNumber (locValue s), showValue (srcValue (locValue s))) | s <- sources open]
      `shouldBe` [(0, "https://example.com/demo-1.2.tar.gz"), (1, "extra.txt")]
  it "numbers patches" $ map (srcNumber . locValue) (patches open) `shouldBe` [0]
  it "lists subpackages" $ map (showValue . pkgName) (subpackages open) `shouldBe` ["demo-devel"]
  it "reads bconds" $ [(bcName b, bcEnabled b) | b <- map locValue (bconds open)] `shouldBe` [("docs", Yes)]
  it "splits dependencies" $
    map locValue (buildRequires known') `shouldBe` ["gcc", "pkgconfig(foo) >= 1.0", "(a or b)", "fedora-only"]
  it "guards dependencies behind unknown conditions" $
    [locGuard d /= [] | d <- buildRequires open] `shouldBe` [False, False, False, True]
  it "parses the changelog" $ length (specChangelog open) `shouldBe` 1
  it "finds macro definitions" $ Map.keys (definitions tree) `shouldBe` ["major", "minor", "ver"]
  it "keeps both branches of an unknown %if" $ do
    let t = parse "Name: x\n%if 0%{?rhel}\nVersion: 1\n%else\nVersion: 2\n%endif\nRelease: 1\n"
    version (resolve (newEnv False) unknownTarget t) `shouldBe` Unresolved [Ambiguous "version"]
    version (resolve (newEnv False) (Target Nothing Nothing [("rhel", Just "10")]) t) `shouldBe` Resolved "1"
    version (resolve (newEnv False) (Target Nothing Nothing [("rhel", Nothing)]) t) `shouldBe` Resolved "2"
  it "merges macros defined in unknown branches" $ do
    let t = parse "%if 0%{?rhel}\n%global v 1\n%else\n%global v 2\n%endif\nName: x\nVersion: %{v}\n"
    version (resolve (newEnv False) unknownTarget t) `shouldBe` Unresolved [Ambiguous "v"]
