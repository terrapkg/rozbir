module Rozbir.Lint.Types
  ( RuleSet (..),
    ruleSetName,
    parseRuleSet,
    RuleId,
    Rule (..),
    Diagnostic (..),
    Fix (..),
    LintInput (..),
    LintConfig (..),
    defaultConfig,
    Severity (..),
    parseSeverity,
    diag,
    linesWithSection,
    preambleLike,
    firstSpec,
    mainPackages,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Rozbir.Edit (TextEdit)
import Rozbir.Eval.Types (Severity (..), Target)
import Rozbir.Spec (Package, Spec, mainPackage)
import Rozbir.Syntax

data RuleSet = Core | Rpmlint | Terra
  deriving stock (Eq, Ord, Show, Enum, Bounded)

ruleSetName :: RuleSet -> Text
ruleSetName = \case
  Core -> "core"
  Rpmlint -> "rpmlint"
  Terra -> "terra"

parseRuleSet :: Text -> Maybe RuleSet
parseRuleSet t = lookup (T.toLower (T.strip t)) [(ruleSetName r, r) | r <- [minBound .. maxBound]]

parseSeverity :: Text -> Maybe Severity
parseSeverity t = case T.toLower (T.strip t) of
  "info" -> Just Info
  "warn" -> Just Warn
  "warning" -> Just Warn
  "error" -> Just Error
  _ -> Nothing

type RuleId = Text

data Fix = Fix
  { fixTitle :: !Text,
    fixEdits :: ![TextEdit],
    fixSafe :: !Bool
  }
  deriving stock (Eq, Show)

instance ToJSON Fix where
  toJSON Fix {..} = object ["title" .= fixTitle, "edits" .= fixEdits, "safe" .= fixSafe]

data Diagnostic = Diagnostic
  { diagRule :: !RuleId,
    diagSeverity :: !Severity,
    diagSpan :: !Span,
    diagMessage :: !Text,
    diagFixes :: ![Fix]
  }
  deriving stock (Eq, Show)

instance ToJSON Diagnostic where
  toJSON Diagnostic {..} =
    object
      [ "rule" .= diagRule,
        "severity" .= diagSeverity,
        "span" .= diagSpan,
        "message" .= diagMessage,
        "fixes" .= diagFixes
      ]

data LintInput = LintInput
  { liSource :: !Text,
    liTree :: !Tree,
    liSpecs :: ![(Target, Spec)]
  }

data Rule = Rule
  { ruleId :: !RuleId,
    ruleSet :: !RuleSet,
    ruleSeverity :: !Severity,
    ruleDoc :: !Text,
    ruleCheck :: LintInput -> [Diagnostic]
  }

data LintConfig = LintConfig
  { lcRuleSets :: !(Set RuleSet),
    lcOverrides :: !(Map RuleId (Maybe Severity))
  }
  deriving stock (Eq, Show)

defaultConfig :: LintConfig
defaultConfig = LintConfig (Set.fromList [Core, Rpmlint]) Map.empty

diag :: RuleId -> Span -> Text -> Diagnostic
diag r sp msg = Diagnostic r Warn sp msg []

linesWithSection :: Tree -> [(Maybe SectionKind, Line)]
linesWithSection tree = go Nothing (treeLines tree)
  where
    go _ [] = []
    go sec (l : ls) = case lineBody l of
      Header h -> (Just (secKind h), l) : go (Just (secKind h)) ls
      _ -> (sec, l) : go sec ls

preambleLike :: Maybe SectionKind -> Bool
preambleLike = isPreambleLike

firstSpec :: LintInput -> [Spec]
firstSpec li = take 1 (map snd (liSpecs li))

mainPackages :: LintInput -> [Package]
mainPackages li = [p | (_, s) <- liSpecs li, Just p <- [mainPackage s]]
