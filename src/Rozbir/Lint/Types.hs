-- | types shared by the lint rulesets: rules, diagnostics, fixes and config
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

-- | a group of rules turned on together
data RuleSet
  = -- | broken specs and problems rpm itself trips on, on by default
    Core
  | -- | rpmlint's spec checks, on by default
    Rpmlint
  | -- | Terra's packaging conventions, off by default
    Terra
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | the name used in config, like @rpmlint@
ruleSetName :: RuleSet -> Text
ruleSetName = \case
  Core -> "core"
  Rpmlint -> "rpmlint"
  Terra -> "terra"

-- | parse a ruleset name, ignoring case and surrounding spaces
parseRuleSet :: Text -> Maybe RuleSet
parseRuleSet t = lookup (T.toLower (T.strip t)) [(ruleSetName r, r) | r <- [minBound .. maxBound]]

-- | parse @info@, @warn@, @warning@ or @error@, ignoring case
parseSeverity :: Text -> Maybe Severity
parseSeverity t = case T.toLower (T.strip t) of
  "info" -> Just Info
  "warn" -> Just Warn
  "warning" -> Just Warn
  "error" -> Just Error
  _ -> Nothing

-- | a rule's id, like @hardcoded-path@
type RuleId = Text

-- | a suggested change that resolves a diagnostic
data Fix = Fix
  { -- | a short label, like @use %global@
    fixTitle :: !Text,
    -- | edits to the original source
    fixEdits :: ![TextEdit],
    -- | True if it's fine to apply without review
    fixSafe :: !Bool
  }
  deriving stock (Eq, Show)

instance ToJSON Fix where
  toJSON Fix {..} = object ["title" .= fixTitle, "edits" .= fixEdits, "safe" .= fixSafe]

-- | one finding
data Diagnostic = Diagnostic
  { diagRule :: !RuleId,
    -- | how serious it is, after config overrides
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

-- | what rules look at
data LintInput = LintInput
  { liSource :: !Text,
    liTree :: !Tree,
    -- | the spec resolved for each target
    liSpecs :: ![(Target, Spec)]
  }

-- | a lint check
data Rule = Rule
  { -- | unique id, used in config and ignore comments
    ruleId :: !RuleId,
    ruleSet :: !RuleSet,
    -- | severity unless config overrides it
    ruleSeverity :: !Severity,
    ruleDoc :: !Text,
    -- | find problems. the severity it sets is replaced by the configured one.
    ruleCheck :: LintInput -> [Diagnostic]
  }

-- | which rules run and how serious they are
data LintConfig = LintConfig
  { lcRuleSets :: !(Set RuleSet),
    -- | per-rule severity. Nothing turns a rule off, and a severity turns it
    -- on even if its ruleset isn't enabled.
    lcOverrides :: !(Map RuleId (Maybe Severity))
  }
  deriving stock (Eq, Show)

-- | core and rpmlint rules, no overrides
defaultConfig :: LintConfig
defaultConfig = LintConfig (Set.fromList [Core, Rpmlint]) Map.empty

-- | a diagnostic without fixes. its severity is replaced by the configured one.
diag :: RuleId -> Span -> Text -> Diagnostic
diag r sp msg = Diagnostic r Warn sp msg []

-- | every line with the section it's in. Nothing is the main preamble, and a
-- header line belongs to its own section.
linesWithSection :: Tree -> [(Maybe SectionKind, Line)]
linesWithSection tree = go Nothing (treeLines tree)
  where
    go _ [] = []
    go sec (l : ls) = case lineBody l of
      Header h -> (Just (secKind h), l) : go (Just (secKind h)) ls
      _ -> (sec, l) : go sec ls

-- | True for the main preamble and @%package@ sections
preambleLike :: Maybe SectionKind -> Bool
preambleLike = isPreambleLike

-- | the spec for the first target, if there is one
firstSpec :: LintInput -> [Spec]
firstSpec li = take 1 (map snd (liSpecs li))

-- | the main package of each target's spec
mainPackages :: LintInput -> [Package]
mainPackages li = [p | (_, s) <- liSpecs li, Just p <- [mainPackage s]]
