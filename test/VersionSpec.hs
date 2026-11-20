module VersionSpec (spec) where

import Rozbir.Version
import Test.Hspec

spec :: Spec
spec = describe "Rozbir.Version" $ do
  it "parses version-release" $ parseEVR "1.2.3-1.fc44" `shouldBe` Just (EVR 0 "1.2.3" "1.fc44")
  it "parses an epoch" $ parseEVR "2:1.0-3" `shouldBe` Just (EVR 2 "1.0" "3")
  it "rejects a missing release" $ parseEVR "1.0" `shouldBe` Nothing
  describe "rpmvercmp" $
    mapM_
      (\(a, b, o) -> it (show a <> " vs " <> show b) (rpmvercmp a b `shouldBe` o))
      [ ("1.0", "1.0", EQ),
        ("1.0", "2.0", LT),
        ("2.0.1", "2.0", GT),
        ("1.05", "1.5", EQ),
        ("5.5p10", "5.5p2", GT),
        ("1.0~rc1", "1.0", LT),
        ("1.0^git1", "1.0", GT),
        ("1.0^git1", "1.0.1", LT),
        ("a", "1", LT),
        ("1.0a", "1.0", GT)
      ]
  it "compares literals without a release" $
    (compareVersionLiteral <$> parseVersionLiteral "1.2" <*> parseVersionLiteral "1.2-3") `shouldBe` Just EQ
