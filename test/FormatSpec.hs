module FormatSpec (spec) where

import Arbitrary
import Rozbir.Format
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Rozbir.Format" $ do
  let fmt = formatText defaultFormatOptions
  it "aligns tags to the column most of them use" $
    fmt "Name:           a\nVersion:        1\nRelease: 1\n" `shouldBe` "Name:           a\nVersion:        1\nRelease:        1\n"
  it "leaves a spec with no dominant column alone" $
    fmt "Name: a\nVersion:   1\n" `shouldBe` "Name: a\nVersion:   1\n"
  it "doesn't touch tab-aligned tags" $
    fmt "Name:\t\ta\nVersion:\t1\nRelease: 1\n" `shouldBe` "Name:\t\ta\nVersion:\t1\nRelease: 1\n"
  it "strips trailing whitespace" $
    fmt "%build\nmake  \n" `shouldBe` "%build\nmake\n"
  it "keeps whitespace after a continuation backslash" $
    fmt "%build\nmake \\ \n" `shouldBe` "%build\nmake \\ \n"
  it "squashes long runs of blank lines" $
    fmt "Name: a\n\n\n\n\n%description\n" `shouldBe` "Name: a\n\n\n%description\n"
  it "ends with one newline" $ do
    fmt "Name: a" `shouldBe` "Name: a\n"
    fmt "Name: a\n\n\n" `shouldBe` "Name: a\n"
  it "is idempotent" $ property $ \(SpecText t) -> fmt (fmt t) === fmt t
