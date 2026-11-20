module EditSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (fromGregorian)
import Rozbir.Edit
import Rozbir.Syntax.Parse (parse)
import Test.Hspec

applyEdit :: Text -> Either EditError [TextEdit] -> Either EditError Text
applyEdit src = fmap (`apply` src)

spec :: Spec
spec = describe "Rozbir.Edit" $ do
  let src = T.unlines ["%global ver 1.2.3", "Name:    foo", "Version:\t%{ver}", "Release: 4%{?dist}", "", "%changelog", "* Mon Jan 05 2026 A <a@b> - 1.2.3-4", "- x"]
      tree = parse src
  it "follows Version to its %global" $
    applyEdit src (setTag "version" "2.0" tree) `shouldBe` Right (T.replace "%global ver 1.2.3" "%global ver 2.0" src)
  it "sets a tag literally when asked" $
    applyEdit src (setTagLiteral "Version" "2.0" tree) `shouldBe` Right (T.replace "Version:\t%{ver}" "Version:\t2.0" src)
  it "keeps the alignment of a plain tag" $
    applyEdit src (setTag "name" "bar" tree) `shouldBe` Right (T.replace "Name:    foo" "Name:    bar" src)
  it "bumps Release" $
    applyEdit src (bumpRelease tree) `shouldBe` Right (T.replace "Release: 4" "Release: 5" src)
  it "resets Release when setting Version" $
    applyEdit src (setVersion "9" tree) `shouldBe` Right (T.replace "Release: 4" "Release: 1" (T.replace "ver 1.2.3" "ver 9" src))
  it "refuses %autorelease" $
    bumpRelease (parse "Release: %autorelease\n") `shouldSatisfy` either (const True) (const False)
  it "refuses a value set in two branches" $
    case setTag "version" "2" (parse "%if 1\nVersion: 1\n%else\nVersion: 2\n%endif\n") of
      Left (Ambiguous _ spans) -> length spans `shouldBe` 2
      other -> expectationFailure (show other)
  it "adds a changelog entry on top" $
    applyEdit src (addChangelog (ChangelogInput (fromGregorian 2026 11 20) "B <b@c>" (Just "2.0-1") ["new"]) tree)
      `shouldBe` Right (T.replace "%changelog\n" "%changelog\n* Fri Nov 20 2026 B <b@c> - 2.0-1\n- new\n\n" src)
  it "creates %changelog when missing" $
    applyEdit "Name: x\n" (addChangelog (ChangelogInput (fromGregorian 2026 11 20) "B" Nothing ["y"]) (parse "Name: x\n"))
      `shouldBe` Right "Name: x\n\n%changelog\n* Fri Nov 20 2026 B\n- y\n"
  it "keeps %define when editing a macro" $
    applyEdit "%define v 1\nVersion: %v\n" (setTag "version" "2" (parse "%define v 1\nVersion: %v\n")) `shouldBe` Right "%define v 2\nVersion: %v\n"
