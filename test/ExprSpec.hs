module ExprSpec (spec) where

import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import Rozbir.Eval.Types
import Rozbir.Syntax.Expr
import Test.Hspec

run :: Text -> Either Text ExprValue
run t = runIdentity (evalExpr False (\_ -> pure mempty) (itemsFromValue (known t)))

runWith :: Value -> Either Text ExprValue
runWith v = runIdentity (evalExpr False (\_ -> pure mempty) (itemsFromValue v))

spec :: Spec
spec = describe "Rozbir.Syntax.Expr" $ do
  it "does arithmetic" $ run "1 + 2 * 3" `shouldBe` Right (EInt 7)
  it "divides like C" $ run "-7 / 2" `shouldBe` Right (EInt (-3))
  it "compares integers" $ run "044 >= 40" `shouldBe` Right (EInt 1)
  it "compares strings" $ run "\"a\" < \"b\"" `shouldBe` Right (EInt 1)
  it "compares versions with rpmvercmp" $ run "v\"1.10\" > v\"1.9\"" `shouldBe` Right (EInt 1)
  it "treats && and || alike, left to right" $ run "1 || 0 && 0" `shouldBe` Right (EInt 0)
  it "returns an operand from &&" $ run "2 && 3" `shouldBe` Right (EInt 3)
  it "supports the ternary operator" $ run "0 ? 1 : 2" `shouldBe` Right (EInt 2)
  it "negates" $ run "!0" `shouldBe` Right (EInt 1)
  it "rejects bare words" $ run "foo" `shouldSatisfy` either (const True) (const False)
  it "rejects mixed types" $ run "1 == \"1\"" `shouldBe` Left "types must match"
  it "rejects an empty expression" $ run "" `shouldBe` Left "unexpected end of expression"
  it "rejects trailing junk" $ run "1 2" `shouldBe` Left "syntax error in expression"
  it "rejects division by zero" $ run "1 / 0" `shouldBe` Left "division by zero"
  describe "with unknown values" $ do
    let fedora = known "0" <> opaque (Undefined "fedora")
    it "is unknown when it depends on one" $
      fmap boolify (runWith (fedora <> known " >= 40")) `shouldBe` Right (Unknown [Undefined "fedora"])
    it "short-circuits &&" $
      fmap boolify (runWith (known "0 && " <> fedora)) `shouldBe` Right No
    it "short-circuits ||" $
      fmap boolify (runWith (known "1 || " <> fedora)) `shouldBe` Right Yes
    it "decides && when the other side is false" $
      fmap boolify (runWith (fedora <> known " && 0")) `shouldBe` Right No
