module ShellSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Rozbir.Eval
import Rozbir.Eval.Shell (runShell)
import Test.Hspec

shell :: Text -> Either Text Text
shell = runShell

spec :: Spec
spec = describe "Rozbir.Eval.Shell" $ do
  it "slices a variable" $ shell "c=0123456789; echo ${c:0:7}" `shouldBe` Right "0123456"
  it "strips prefixes and suffixes" $ shell "v=v1.2.tar; echo ${v#v} ${v%.tar}" `shouldBe` Right "1.2.tar v1.2"
  it "replaces in a variable" $ shell "v=1-2-3; echo ${v//-/.} ${v/-/.}" `shouldBe` Right "1.2.3 1.2-3"
  it "measures a variable" $ shell "v=abcd; echo ${#v}" `shouldBe` Right "4"
  it "runs sed s commands" $ shell "echo v1.2-rc1 | sed 's/^v//;s/-/~/g'" `shouldBe` Right "1.2~rc1"
  it "runs several sed scripts" $ shell "echo a-b | sed -e 's/a/x/' -e 's/b/y/'" `shouldBe` Right "x-y"
  it "honors ERE quantifiers" $ shell "echo aaab | sed -E 's/a+/x/'" `shouldBe` Right "xb"
  it "keeps + literal in BRE" $ shell "echo a+b | sed 's/a+/x/'" `shouldBe` Right "xb"
  it "matches the longest leftmost" $ shell "echo xaaay | sed 's/a*y/Z/'" `shouldBe` Right "xZ"
  it "replaces with &" $ shell "echo 1.2 | sed 's/[0-9]/<&>/g'" `shouldBe` Right "<1>.<2>"
  it "handles empty matches globally like GNU sed" $ shell "echo abc | sed 's/x*/-/g'" `shouldBe` Right "-a-b-c-"
  it "translates with tr" $ shell "echo 1~2 | tr '~' '-'" `shouldBe` Right "1-2"
  it "deletes with tr" $ shell "echo v1v | tr -d v" `shouldBe` Right "1"
  it "cuts fields" $ shell "echo 1.2.3 | cut -d. -f1,3" `shouldBe` Right "1.3"
  it "cuts characters" $ shell "echo abcdef | cut -c2-4" `shouldBe` Right "bcd"
  it "splits unquoted words" $ shell "v='a  b'; echo $v \"$v\"" `shouldBe` Right "a b a  b"
  it "drops trailing newlines like rpm" $ shell "echo x; echo" `shouldBe` Right "x"
  describe "refuses" $ do
    it "the environment" $ shell "echo $HOME" `shouldSatisfy` isLeft
    it "other commands" $ shell "date +%Y%m%d" `shouldSatisfy` isLeft
    it "command substitution" $ shell "echo $(date)" `shouldSatisfy` isLeft
    it "redirections" $ shell "echo x > /tmp/y" `shouldSatisfy` isLeft
    it "here-strings after a pipe" $ shell "echo x | sed 's/a/b/' <<< x" `shouldSatisfy` isLeft
    it "globs" $ shell "echo *" `shouldSatisfy` isLeft
    it "back-references" $ shell "echo ab | sed 's/\\(a\\)/\\1/'" `shouldSatisfy` isLeft
    it "sed reading stdin" $ shell "sed 's/a/b/'" `shouldSatisfy` isLeft
    it "&&" $ shell "true && echo x" `shouldSatisfy` isLeft
  describe "Lua patterns in %{gsub}, checked against rpm --eval" $ do
    let gsub ws = run ["%{gsub " <> ws <> "}"]
        run ts = fst (runEval (initialState (newEnv True)) (last <$> mapM expandText ts))
    mapM_
      (\(args, out) -> it (show args) (gsub args `shouldBe` known out))
      [ ("1.2-3 - ~", "1.2~3"),
        ("a-b-c - _ 2", "a_b_c"),
        ("hello o* x", "xhxexlxlx"),
        ("hello %w+ X", "X"),
        ("1.2.3 %. -", "1-2-3"),
        ("abc .- X", "XaXbXcX"),
        ("aaa a- X", "XaXaXaX"),
        ("1.2.3rc1 %d+ <%0>", "<1>.<2>.<3>rc<1>"),
        ("foo_bar [_-] .", "foo.bar"),
        ("CAPS %u .", "...."),
        ("tag-v1.0 ^.*%-v %0", "tag-v1.0"),
        ("a.b.c [^.]+ w", "w.w.w")
      ]
    it "is opaque about captures" $ gsub "ab (a)b %1" `shouldBe` opaque RunsLua
  it "reads here-strings" $ shell "v=1-2; sed 's/-/~/g' <<< $v" `shouldBe` Right "1~2"
  it "treats a leading \\+ in BRE as a literal" $ shell "echo 1.0+abc | sed 's/\\+.*//g'" `shouldBe` Right "1.0"
  describe "in macros" $ do
    let run ts = fst (runEval (initialState (newEnv False)) (last <$> mapM expandText ts))
    it "expands the classic shortcommit" $
      run ["%global commit 0123456789abcdef", "%global shortcommit %(c=%{commit}; echo ${c:0:7})", "%{shortcommit}"] `shouldBe` known "0123456"
    it "stays opaque for real commands" $
      run ["%(git rev-parse HEAD)"] `shouldBe` opaque RunsShell
