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

allRules :: [Rule]
allRules = Core.rules <> Rpmlint.rules <> Terra.rules <> [unusedIgnoreRule]

unusedIgnoreRule :: Rule
unusedIgnoreRule = Rule "unused-ignore" Core Warn "a rozbir: ignore directive that no longer suppresses anything" (const [])

rulesFor :: LintConfig -> [Rule]
rulesFor LintConfig {..} =
  [ r
  | r <- allRules,
    ruleSet r `Set.member` lcRuleSets || Map.lookup (ruleId r) lcOverrides `notElem` [Nothing, Just Nothing],
    Map.lookup (ruleId r) lcOverrides /= Just Nothing
  ]

findRule :: RuleId -> Maybe Rule
findRule rid = case filter ((== rid) . ruleId) allRules of
  r : _ -> Just r
  [] -> Nothing

data Directive = Directive
  { dirSpan :: !Span,
    dirFile :: !Bool,
    dirRules :: ![RuleId],
    dirCovers :: ![Int]
  }
  deriving stock (Eq, Show)

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

lintTree :: LintConfig -> Env -> [Target] -> Map.Map Text Tree -> Tree -> [Diagnostic]
lintTree cfg env targets includes tree =
  runLint cfg (LintInput (render tree) tree [(t, resolveWith (ResolveOptions env t includes) tree) | t <- targets])

applyFix :: Fix -> Text -> Text
applyFix f = apply (fixEdits f)
