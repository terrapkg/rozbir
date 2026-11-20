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

data EVR = EVR
  { evrEpoch :: !Int,
    evrVersion :: !Text,
    evrRelease :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

instance Ord EVR where
  compare = compareEVR

parseEVR :: Text -> Maybe EVR
parseEVR raw = do
  let (epochPart, rest) = case T.breakOn ":" raw of
        (vr, "") -> ("0", vr)
        (e, r) -> (e, T.drop 1 r)
  epoch <- if epochPart == "(none)" then Just 0 else readMaybe (T.unpack epochPart)
  let (verDash, rel) = T.breakOnEnd "-" rest
  ver <- T.stripSuffix "-" verDash
  if T.null ver || T.null rel then Nothing else Just (EVR epoch ver rel)

renderEVR :: EVR -> Text
renderEVR (EVR 0 v r) = v <> "-" <> r
renderEVR (EVR e v r) = T.pack (show e) <> ":" <> v <> "-" <> r

compareEVR :: EVR -> EVR -> Ordering
compareEVR (EVR e1 v1 r1) (EVR e2 v2 r2) =
  compare e1 e2 <> rpmvercmp v1 v2 <> rpmvercmp r1 r2

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

data VersionLiteral = VersionLiteral
  { vlEpoch :: !(Maybe Integer),
    vlVersion :: !Text,
    vlRelease :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

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

compareVersionLiteral :: VersionLiteral -> VersionLiteral -> Ordering
compareVersionLiteral a b =
  compare (maybe 0 id (vlEpoch a)) (maybe 0 id (vlEpoch b))
    <> rpmvercmp (vlVersion a) (vlVersion b)
    <> case (vlRelease a, vlRelease b) of
      (Just r1, Just r2) -> rpmvercmp r1 r2
      _ -> EQ
