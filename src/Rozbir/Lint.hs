-- | runs lint rules on a spec and drops findings silenced by ignore comments
-- like @# rozbir: ignore[rule]@. the core and rpmlint rulesets are on by
-- default, terra is opt-in.
module Rozbir.Lint
  ( module Rozbir.Lint.Types,
    allRules,
    rulesFor,
    findRule,
    runLint,
    lintTree,
    Directive (..),
    directives,
    applyFix,
  )
where

import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Rozbir.Edit (apply)
import Rozbir.Eval (Env, Target)
import Rozbir.Lint.Core qualified as Core
import Rozbir.Lint.Rpmlint qualified as Rpmlint
import Rozbir.Lint.Terra qualified as Terra
import Rozbir.Lint.Types
import Rozbir.Spec (ResolveOptions (..), resolveWith)
import Rozbir.Syntax
import Rozbir.Syntax.Render (render, renderFrags)

-- | every rule in every ruleset
allRules :: [Rule]
allRules = Core.rules <> Rpmlint.rules <> Terra.rules <> [unusedIgnoreRule]

unusedIgnoreRule :: Rule
unusedIgnoreRule = Rule "unused-ignore" Core Warn "a rozbir: ignore directive that no longer suppresses anything" (const [])

-- | the rules a config turns on: its rulesets, plus rules given a severity in
-- 'lcOverrides', minus rules overridden to Nothing
rulesFor :: LintConfig -> [Rule]
rulesFor LintConfig {..} =
  [ r
  | r <- allRules,
    ruleSet r `Set.member` lcRuleSets || Map.lookup (ruleId r) lcOverrides `notElem` [Nothing, Just Nothing],
    Map.lookup (ruleId r) lcOverrides /= Just Nothing
  ]

-- | look up a rule by id
findRule :: RuleId -> Maybe Rule
findRule rid = case filter ((== rid) . ruleId) allRules of
  r : _ -> Just r
  [] -> Nothing

-- | a @# rozbir: ignore[...]@ or @# rozbir: ignore-file[...]@ comment
data Directive = Directive
  { dirSpan :: !Span,
    -- | True for @ignore-file@, which covers the whole spec
    dirFile :: !Bool,
    -- | the rule ids it names
    dirRules :: ![RuleId],
    -- | line numbers it covers, its own and the next line's
    dirCovers :: ![Int]
  }
  deriving stock (Eq, Show)

-- | every ignore comment in a tree
directives :: Tree -> [Directive]
directives tree = mapMaybe directive (zip ls (drop 1 (map Just ls) <> [Nothing]))
  where
    ls = treeLines tree
    directive (l, next) = case lineBody l of
      Comment _ fs -> do
        let text = T.strip (T.drop 1 (T.stripStart (renderFrags fs)))
        rest <- T.stripPrefix "rozbir:" text
        let body = T.stripStart rest
            (kw, afterKw) = T.span (\c -> c == '-' || c >= 'a' && c <= 'z') body
        inner <- T.stripPrefix "[" afterKw
        let (names, closing) = T.break (== ']') inner
        _ <- T.stripPrefix "]" closing
        let ruleIds = filter (not . T.null) (map T.strip (T.splitOn "," names))
            here = posLine (spanStart (lineSpan l))
            covered = here : maybe [] (\n -> [posLine (spanStart (lineSpan n)) .. posLine (spanEnd (lineSpan n))]) next
        case kw of
          "ignore" -> Just (Directive (lineSpan l) False ruleIds covered)
          "ignore-file" -> Just (Directive (lineSpan l) True ruleIds [])
          _ -> Nothing
      _ -> Nothing

-- | run the enabled rules and return findings sorted by position. ignore
-- comments that silence nothing or name an unknown rule are reported as
-- @unused-ignore@.
runLint :: LintConfig -> LintInput -> [Diagnostic]
runLint cfg li =
  let active = rulesFor cfg
      raw =
        nub
          [ d {diagSeverity = severityOf r}
          | r <- active,
            d <- ruleCheck r li
          ]
      severityOf r = case Map.lookup (ruleId r) (lcOverrides cfg) of
        Just (Just s) -> s
        _ -> ruleSeverity r
      dirs = directives (liTree li)
      suppressedBy d dir =
        diagRule d `elem` dirRules dir
          && (dirFile dir || posLine (spanStart (diagSpan d)) `elem` dirCovers dir)
      kept = [d | d <- raw, not (any (suppressedBy d) dirs)]
      unusedOn = any ((== "unused-ignore") . ruleId) active
      unused =
        [ diag "unused-ignore" (dirSpan dir) ("this directive doesn't suppress " <> rid <> " anywhere")
        | unusedOn,
          dir <- dirs,
          rid <- dirRules dir,
          ruleIsActive rid,
          not (any (\d -> diagRule d == rid && suppressedBy d dir) raw)
        ]
      ruleIsActive rid = any ((== rid) . ruleId) active
      unknownRule =
        [ diag "unused-ignore" (dirSpan dir) (rid <> " isn't a rozbir rule")
        | unusedOn,
          dir <- dirs,
          rid <- dirRules dir,
          null (findRule rid)
        ]
   in sortOn (\d -> (posOffset (spanStart (diagSpan d)), diagRule d)) (kept <> map (\d -> d {diagSeverity = severityOfId "unused-ignore"}) (unused <> unknownRule))
  where
    severityOfId rid = case Map.lookup rid (lcOverrides cfg) of
      Just (Just s) -> s
      _ -> Warn

-- | resolve a tree for each target and lint it. with no targets, rules that
-- need a resolved spec find nothing.
lintTree :: LintConfig -> Env -> [Target] -> Map.Map Text Tree -> Tree -> [Diagnostic]
lintTree cfg env targets includes tree =
  runLint cfg (LintInput (render tree) tree [(t, resolveWith (ResolveOptions env t includes) tree) | t <- targets])

-- | apply a fix to the source text it was made for
applyFix :: Fix -> Text -> Text
applyFix f = apply (fixEdits f)
