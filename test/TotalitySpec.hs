module TotalitySpec (spec) where

import Arbitrary
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Rozbir.Edit
import Rozbir.Eval (Target (..), newEnv, unknownTarget)
import Rozbir.Lint
import Rozbir.Query
import Rozbir.Spec hiding (Spec)
import Rozbir.Syntax.Parse (parse)
import Rozbir.Syntax.Render (render)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

everything :: LintConfig
everything = defaultConfig {lcRuleSets = Set.fromList [minBound .. maxBound]}

targets :: [Target]
targets = [unknownTarget, Target (Just "x86_64") (Just "linux") [("fedora", Just "44"), ("dist", Just ".fc44")]]

survives :: Text -> IO ()
survives t = do
  let tree = parse t
  _ <- evaluate (length (show (lintTree everything (newEnv False) targets Map.empty tree)))
  _ <- evaluate (length (show (lintTree everything (newEnv True) targets Map.empty tree)))
  let s = resolve (newEnv False) unknownTarget tree
  _ <- evaluate (force (specPackages s, specSections s, specSources s))
  _ <- evaluate (length (show (evr s, versionInputs tree, buildRequires s)))
  mapM_
    (\r -> either (const (pure ())) (\es -> () <$ evaluate (render (parse (apply es t)))) r)
    [ setTag "version" "9.9" tree,
      setTag "release" "2" tree,
      bumpRelease tree,
      setVersion "1.0" tree,
      setMacro "ver" "2" tree,
      addChangelog (ChangelogInput (fromGregorian 2026 11 20) "A <a@b>" (Just "1-1") ["x"]) tree
    ]
  mapM_ (\d -> mapM_ (\f -> () <$ evaluate (render (parse (apply (fixEdits f) t)))) (diagFixes d)) (lintTree everything (newEnv False) targets Map.empty tree)

spec :: Spec
spec = describe "totality" $ modifyMaxSuccess (max 600) $ do
  it "never crashes on spec-like text" $ property $ \(SpecText t) -> ioProperty (survives t >> pure True)
  it "never crashes on arbitrary text" $ property $ \(AnyText t) -> ioProperty (survives t >> pure True)
  it "keeps edits on value spans" $ property $ \(SpecText t) ->
    case setTagLiteral "version" "X" (parse t) of
      Right es -> render (parse (apply es t)) === apply es t
      Left _ -> property True
