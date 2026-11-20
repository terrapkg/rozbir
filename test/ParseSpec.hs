module ParseSpec (spec) where

import Arbitrary
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Rozbir.Syntax
import Rozbir.Syntax.Parse
import Rozbir.Syntax.Render
import Test.Hspec
import Test.QuickCheck

bodies :: Text -> [LineBody]
bodies = map lineBody . treeLines . parse

spec :: Spec
spec = describe "Rozbir.Syntax.Parse" $ do
  describe "round-trips" $ do
    it "spec-like text" $ property $ \(SpecText t) -> render (parse t) === t
    it "arbitrary text" $ property $ \(AnyText t) -> render (parse t) === t
    it "frags of arbitrary text" $ property $ \(AnyText t) -> renderFrags (parseFrags t) === t

  describe "lines" $ do
    it "classifies a tag" $
      bodies "Name:  foo  \n" `shouldBe` [Tag (TagLine "" "Name" Nothing ":  " [Lit "foo"] "  ")]
    it "keeps a qualifier" $ case bodies "Requires(post): bar" of
      [Tag t] -> (qualRaw <$> tagQualifier t) `shouldBe` Just "(post)"
      other -> expectationFailure (show other)
    it "doesn't read tags inside %description" $
      bodies "%description\nName: x\n" `shouldBe` [Header (SectionHeader SDescription "%description" []), Body [Lit "Name: x"]]
    it "reads tags inside %package" $ case bodies "%package devel\nSummary: x\n" of
      [Header _, Tag _] -> pure ()
      other -> expectationFailure (show other)
    it "only treats column 0 as a section header" $ case bodies "  %files\n" of
      [Body _] -> pure ()
      other -> expectationFailure (show other)
    it "matches section names case-insensitively" $ case bodies "%Description\n" of
      [Header h] -> secKind h `shouldBe` SDescription
      other -> expectationFailure (show other)
    it "parses a %global with options and body" $ case bodies "%define foo(a:b) %{-a*} \n" of
      [Define d] -> (defKind d, defName d, defOpts d, renderFrags (defBody d), defTrailing d) `shouldBe` (DefDefine, "foo", Just "a:b", "%{-a*}", " ")
      other -> expectationFailure (show other)
    it "joins a %global continued with a backslash" $ case bodies "%global foo a \\\n  b\nName: x\n" of
      [Define d, Tag _] -> renderFrags (defBody d) `shouldBe` "a \\\n  b"
      other -> expectationFailure (show other)
    it "doesn't join a shell line continued with a backslash" $
      length (bodies "%build\nmake \\\n%if 1\n  X=1\n%endif\n") `shouldBe` 5
    it "joins lines while a macro is open" $
      length (bodies "%global x %{lua:\nprint(1)\n}\nName: y\n") `shouldBe` 2
    it "reports an unclosed macro without swallowing the file" $ do
      let tree = parse "%global x %{lua:\nName: y\n"
      map seCode (treeErrors tree) `shouldBe` ["unclosed-macro"]
      length (treeLines tree) `shouldBe` 2
    it "marks stray text in the preamble" $
      map seCode (treeErrors (parse "hello there\n")) `shouldBe` ["not-a-tag"]

  describe "conditionals" $ do
    it "nests %if blocks" $ case treeNodes (parse "%if 1\n%if 0\nA: b\n%endif\n%else\nC: d\n%endif\n") of
      [NCond c] -> do
        NE.length (condBranches c) `shouldBe` 2
        case branchNodes (NE.head (condBranches c)) of
          [NCond inner] -> NE.length (condBranches inner) `shouldBe` 1
          other -> expectationFailure (show other)
      other -> expectationFailure (show other)
    it "reports an unclosed %if" $
      map seCode (treeErrors (parse "%if 1\nName: x\n")) `shouldBe` ["unclosed-if"]
    it "reports a stray %endif" $
      map seCode (treeErrors (parse "%endif\n")) `shouldBe` ["stray-endif"]
    it "reports %else after %else" $
      map seCode (treeErrors (parse "%if 1\n%else\n%else\n%endif\n")) `shouldBe` ["branch-after-else"]
    it "doesn't take %ifarch for %if" $ case bodies "%ifarch x86_64\n%endif\n" of
      [Cond c, Cond _] -> condKind c `shouldBe` CIfArch
      other -> expectationFailure (show other)
    it "lets %if wrap sections" $ case treeNodes (parse "%if 1\n%package doc\nSummary: x\n%description doc\nd\n%endif\n") of
      [NCond c] -> length [() | NSection _ <- branchNodes (NE.head (condBranches c))] `shouldBe` 2
      other -> expectationFailure (show other)

  describe "frags" $ do
    it "parses bare and braced macros" $
      parseFrags "a %name %{?b:c} %%" `shouldBe` [Lit "a ", Macro (Bare "" "name"), Lit " ", Macro (Braced "?" "b" (Just (ColonArg [Lit "c"]))), Lit " ", Escape]
    it "parses argument macros" $
      parseFrags "%1%*%**%#%{-f*}" `shouldBe` [Macro (Bare "" "1"), Macro (Bare "" "*"), Macro (Bare "" "**"), Macro (Bare "" "#"), Macro (Braced "" "-f*" Nothing)]
    it "keeps Lua raw" $
      parseFrags "%{lua: print(\"%{x}\")}" `shouldBe` [Macro (Lua " print(\"%{x}\")")]
    it "nests braces" $
      parseFrags "%{?a:%{b}}" `shouldBe` [Macro (Braced "?" "a" (Just (ColonArg [Macro (Braced "" "b" Nothing)])))]
    it "parses space arguments" $
      parseFrags "%{foo a b}" `shouldBe` [Macro (Braced "" "foo" (Just (SpaceArg [Lit " a b"])))]
    it "treats a lone % as text" $
      parseFrags "100% sure" `shouldBe` [Lit "100% sure"]
    it "keeps unterminated macros" $
      parseFrags "x %{abc" `shouldBe` [Lit "x ", Macro (Unterminated '{' "abc")]

  describe "spans" $ do
    it "tracks lines and columns" $ do
      let ls = treeLines (parse "a: b\r\n\nName: c")
      map (posLine . spanStart . lineSpan) ls `shouldBe` [0, 1, 2]
      map lineEol ls `shouldBe` ["\r\n", "\n", ""]
    it "covers the whole text" $ property $ \(SpecText t) ->
      let ls = treeLines (parse t)
       in sum (map (\l -> let Span s e = lineFullSpan l in posOffset e - posOffset s) ls) === T.length t
