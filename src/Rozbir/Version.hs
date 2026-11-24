-- | rpm versions: epoch, version and release, and rpm's rules for comparing them
module Rozbir.Version
  ( EVR (..),
    parseEVR,
    renderEVR,
    rpmvercmp,
    compareEVR,
    VersionLiteral (..),
    parseVersionLiteral,
    compareVersionLiteral,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Read (readMaybe)

-- | an epoch, version and release. 'compare' uses rpm's rules, while '==' compares the text.
data EVR = EVR
  { -- | the epoch, 0 when there is none
    evrEpoch :: !Int,
    evrVersion :: !Text,
    evrRelease :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

instance Ord EVR where
  compare = compareEVR

-- | parse @[epoch:]version-release@. an epoch of @(none)@ counts as 0.
-- 'Nothing' when the epoch isn't a number or the version or release is missing.
parseEVR :: Text -> Maybe EVR
parseEVR raw = do
  let (epochPart, rest) = case T.breakOn ":" raw of
        (vr, "") -> ("0", vr)
        (e, r) -> (e, T.drop 1 r)
  epoch <- if epochPart == "(none)" then Just 0 else readMaybe (T.unpack epochPart)
  let (verDash, rel) = T.breakOnEnd "-" rest
  ver <- T.stripSuffix "-" verDash
  if T.null ver || T.null rel then Nothing else Just (EVR epoch ver rel)

-- | print an t'EVR', leaving out an epoch of 0
renderEVR :: EVR -> Text
renderEVR (EVR 0 v r) = v <> "-" <> r
renderEVR (EVR e v r) = T.pack (show e) <> ":" <> v <> "-" <> r

-- | compare by epoch, then version, then release
compareEVR :: EVR -> EVR -> Ordering
compareEVR (EVR e1 v1 r1) (EVR e2 v2 r2) =
  compare e1 e2 <> rpmvercmp v1 v2 <> rpmvercmp r1 r2

-- | a port of rpm's @rpmvercmp@. @~@ sorts before anything, even the end of the string, and @^@ sorts after the end but before anything else.
rpmvercmp :: Text -> Text -> Ordering
rpmvercmp a b
  | a == b = EQ
  | otherwise = go a b
  where
    go one0 two0
      | T.null one0 && T.null two0 = EQ
      | otherwise =
          let one = T.dropWhile isSep one0
              two = T.dropWhile isSep two0
           in case (T.uncons one, T.uncons two) of
                (Just ('~', one'), Just ('~', two')) -> go one' two'
                (Just ('~', _), _) -> LT
                (_, Just ('~', _)) -> GT
                (Just ('^', one'), Just ('^', two')) -> go one' two'
                (Nothing, Just ('^', _)) -> LT
                (Just ('^', _), Nothing) -> GT
                (Just ('^', _), Just _) -> LT
                (Just _, Just ('^', _)) -> GT
                (Nothing, Nothing) -> EQ
                (Just _, Nothing) -> GT
                (Nothing, Just _) -> LT
                (Just (c, _), Just _)
                  | isDigit c -> segment True (T.span isDigit one) (T.span isDigit two)
                  | otherwise -> segment False (T.span isAlpha one) (T.span isAlpha two)

    segment isNum (seg1, rest1) (seg2, rest2)
      | T.null seg2 = if isNum then GT else LT
      | isNum =
          let n1 = T.dropWhile (== '0') seg1
              n2 = T.dropWhile (== '0') seg2
           in compare (T.length n1) (T.length n2) <> compare n1 n2 <> go rest1 rest2
      | otherwise = compare seg1 seg2 <> go rest1 rest2

    isAlpha c = isAsciiLower c || isAsciiUpper c
    isSep c = not (isDigit c || isAlpha c || c == '~' || c == '^')

-- | a version from a @v\"...\"@ literal in an expression
data VersionLiteral = VersionLiteral
  { -- | the epoch, if one was given
    vlEpoch :: !(Maybe Integer),
    vlVersion :: !Text,
    -- | the release, if one was given
    vlRelease :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | parse @[epoch:]version[-release]@. 'Nothing' when the version is empty.
parseVersionLiteral :: Text -> Maybe VersionLiteral
parseVersionLiteral raw = do
  let (digits, afterDigits) = T.span isDigit raw
      (epoch, rest) = case T.uncons afterDigits of
        Just (':', more) | not (T.null digits) -> (readMaybe (T.unpack digits), more)
        _ -> (Nothing, raw)
      (version, release) = case T.breakOnEnd "-" rest of
        ("", _) -> (rest, Nothing)
        (verDash, rel) -> (T.dropEnd 1 verDash, Just rel)
  if T.null version then Nothing else Just (VersionLiteral epoch version release)

-- | compare like rpm does in expressions. a missing epoch counts as 0, and releases are only compared when both sides have one.
compareVersionLiteral :: VersionLiteral -> VersionLiteral -> Ordering
compareVersionLiteral a b =
  compare (maybe 0 id (vlEpoch a)) (maybe 0 id (vlEpoch b))
    <> rpmvercmp (vlVersion a) (vlVersion b)
    <> case (vlRelease a, vlRelease b) of
      (Just r1, Just r2) -> rpmvercmp r1 r2
      _ -> EQ
