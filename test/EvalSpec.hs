module EvalSpec (spec) where

import Data.Text (Text)
import Rozbir.Eval
import Rozbir.Syntax
import Rozbir.Syntax.Parse (parse)
import Test.Hspec

expandIn :: Env -> [Text] -> Value
expandIn env ts = fst (runEval (initialState env) (last <$> mapM expandText ts))

open, closed :: [Text] -> Value
open = expandIn (newEnv False)
closed = expandIn (newEnv True)

spec :: Spec
spec = describe "Rozbir.Eval" $ do
  describe "definitions" $ do
    it "expands %global eagerly" $
      open ["%global a 1", "%global b %{a}", "%global a 2", "%{b}"] `shouldBe` known "1"
    it "expands %define lazily" $
      open ["%global a 1", "%define b %{a}", "%global a 2", "%{b}"] `shouldBe` known "2"
    it "pops with %undefine" $
      closed ["%global a 1", "%global a 2", "%undefine a", "%{a}"] `shouldBe` known "1"
    it "groups a braced body" $
      open ["%global a {x y}", "%{a}"] `shouldBe` known "x y"
    it "keeps continuation lines" $
      open ["%global a x \\\ny", "%{a}"] `shouldBe` known "x \ny"
    it "unescapes the body like rpm" $
      open ["%global a 1\\\\2 \\\\\\\n3", "%{a}"] `shouldBe` known "1\\2 \\\n3"
    it "defines from inside a conditional macro" $
      closed ["%{!?a:%global a 3}", "%{a}"] `shouldBe` known "3"
    it "can't tell whether a conditional definition ran in an open environment" $
      open ["%{!?a:%global a 3}", "%{a}"] `shouldBe` opaque (Ambiguous "a")
  describe "references" $ do
    it "leaves undefined macros alone in a closed environment" $
      closed ["%{nope} %nope"] `shouldBe` known "%{nope} %nope"
    it "is opaque about unknown macros in an open one" $
      open ["x%{?dist}"] `shouldBe` (known "x" <> opaque (Undefined "dist"))
    it "knows %{?with_x} is off without a bcond" $
      open ["a%{?with_x:b}"] `shouldBe` known "a"
    it "handles %{!?x:...}" $
      closed ["%{!?x:none}"] `shouldBe` known "none"
    it "escapes %%" $
      open ["100%%"] `shouldBe` known "100%"
    it "is opaque about shell and Lua" $ do
      open ["%(date)"] `shouldBe` opaque RunsShell
      open ["%{lua: print(1)}"] `shouldBe` opaque RunsLua
    it "stops runaway recursion" $
      open ["%define a %{a}", "%{a}"] `shouldBe` opaque DepthLimit
  describe "parametric macros" $ do
    it "passes positional arguments" $
      open ["%define p() [%1|%2|%#|%*]", "%p a b"] `shouldBe` known "[a|b|2|a b]"
    it "parses options" $
      open ["%define p(n:v) %{-n*}%{?-v:V}", "%p -n x -v"] `shouldBe` known "xV"
    it "takes arguments in braces" $
      open ["%define p() <%1>", "%{p z}"] `shouldBe` known "<z>"
    it "stops arguments at the end of the line" $
      open ["%define p() <%*>", "%p a\nb"] `shouldBe` known "<a>\nb"
    it "scopes %define to the call" $
      closed ["%define p() %{expand:%%define inner 1}", "%p", "%{inner}"] `shouldBe` known "%{inner}"
  describe "builtins" $ do
    it "computes basename and dirname" $
      open ["%{basename:/a/b.tar.gz} %{dirname:/a/b} %{suffix:x.tar.gz}"] `shouldBe` known "b.tar.gz /a gz"
    it "expands twice with %{expand}" $
      open ["%global a 1", "%{expand:%%{a}}"] `shouldBe` known "1"
    it "evaluates %[...]" $
      open ["%[1 + 1]"] `shouldBe` known "2"
    it "evaluates %[...] with macros" $
      open ["%global a 3", "%[%{a} * 2]"] `shouldBe` known "6"
    it "does string functions" $
      open ["%{upper:abc}%{len:abc}%{reverse:ab}%{rep a 3}"] `shouldBe` known "ABC3baaaa"
    it "is opaque about the environment" $
      open ["%{getenv:HOME}"] `shouldBe` opaque ReadsEnv
    it "reports %{shrink}" $
      open ["%{shrink:  a   b }"] `shouldBe` known "a b"
  describe "bconds" $ do
    it "turns on %bcond_without" $ open ["%bcond_without docs", "%{with docs}"] `shouldBe` known "1"
    it "leaves %bcond_with off" $ open ["%bcond_with docs", "%{with docs}"] `shouldBe` known "0"
    it "reads a %bcond default" $ open ["%bcond tests 1", "%{with tests}%{without tests}"] `shouldBe` known "10"
  describe "conditions" $ do
    let cond env t = case map lineBody (treeLines (parse t)) of
          [Cond c] -> fst (runEval (initialState env) (evalCond c))
          other -> error (show other)
        x86 = withTarget (Target (Just "x86_64") (Just "linux") [("fedora", Just "44")]) (newEnv False)
    it "is three-valued for unknown macros" $
      cond (newEnv False) "%if 0%{?fedora} >= 40" `shouldBe` Unknown [Undefined "fedora"]
    it "decides with a target" $
      cond x86 "%if 0%{?fedora} >= 40" `shouldBe` Yes
    it "matches %ifarch against the target" $ do
      cond x86 "%ifarch aarch64 x86_64" `shouldBe` Yes
      cond x86 "%ifnarch x86_64" `shouldBe` No
    it "doesn't know %ifarch without an arch" $
      cond (newEnv False) "%ifarch x86_64" `shouldBe` Unknown [Undefined "_target_cpu"]
    it "joins a continued condition" $
      cond x86 "%if 1 && \\\n    0\n" `shouldBe` No
    it "uses target overrides" $
      expandIn (withTarget (Target (Just "aarch64") Nothing [("fedora", Just "44")]) (newEnv False)) ["%{fedora} %{_target_cpu}"] `shouldBe` known "44 aarch64"
