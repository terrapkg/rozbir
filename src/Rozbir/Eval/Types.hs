module Rozbir.Eval.Types
  ( Reason (..),
    describeReason,
    Piece (..),
    Value (..),
    known,
    opaque,
    opaqueAt,
    knownText,
    isKnown,
    reasons,
    showValue,
    stripQuoteMarks,
    valueNull,
    mapKnown,
    valueLines,
    valueWords,
    Tri (..),
    triNot,
    triAnd,
    triOr,
    MacroDef (..),
    MacroBody (..),
    Origin (..),
    Env (..),
    Target (..),
    unknownTarget,
    Severity (..),
    EvalMessage (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Rozbir.Syntax (Name, Span)

data Reason
  = RunsShell
  | RunsLua
  | ReadsEnv
  | ReadsFilesystem
  | HostDependent Text
  | Undefined Name
  | Ambiguous Name
  | DepthLimit
  | Unsupported Text
  | Failed Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

describeReason :: Reason -> Text
describeReason = \case
  RunsShell -> "runs a shell command"
  RunsLua -> "runs Lua"
  ReadsEnv -> "reads the environment"
  ReadsFilesystem -> "reads the filesystem"
  HostDependent what -> "depends on the build host (" <> what <> ")"
  Undefined n -> "%" <> n <> " isn't defined here"
  Ambiguous n -> "%" <> n <> " depends on a condition that can't be decided"
  DepthLimit -> "expands too deeply, probably recursion"
  Unsupported what -> what <> " isn't supported"
  Failed why -> why

instance ToJSON Reason where
  toJSON r = toJSON (describeReason r)

data Piece
  = Known !Text
  | Opaque !Reason !(Maybe Span)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

newtype Value = Value [Piece]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Semigroup Value where
  Value a <> Value b = Value (normalize (a <> b))

instance Monoid Value where
  mempty = Value []

instance ToJSON Value where
  toJSON v = case knownText v of
    Just t -> object ["known" .= True, "text" .= t]
    Nothing -> object ["known" .= False, "text" .= showValue v, "reasons" .= reasons v]

normalize :: [Piece] -> [Piece]
normalize = \case
  Known "" : rest -> normalize rest
  Known a : Known b : rest -> normalize (Known (a <> b) : rest)
  x : rest -> x : normalize rest
  [] -> []

known :: Text -> Value
known t = Value (normalize [Known t])

opaque :: Reason -> Value
opaque r = Value [Opaque r Nothing]

opaqueAt :: Reason -> Span -> Value
opaqueAt r s = Value [Opaque r (Just s)]

knownText :: Value -> Maybe Text
knownText (Value ps) = T.concat <$> traverse (\case Known t -> Just t; Opaque {} -> Nothing) ps

isKnown :: Value -> Bool
isKnown (Value ps) = all (\case Known _ -> True; _ -> False) ps

reasons :: Value -> [Reason]
reasons (Value ps) = [r | Opaque r _ <- ps]

valueNull :: Value -> Bool
valueNull (Value ps) = null ps

showValue :: Value -> Text
showValue (Value ps) = T.concat (map piece ps)
  where
    piece = \case
      Known t -> t
      Opaque r _ -> "\x2039" <> short r <> "\x203a"
    short = \case
      Undefined n -> "%" <> n
      Ambiguous n -> "%" <> n <> "?"
      RunsShell -> "%(...)"
      RunsLua -> "lua"
      ReadsEnv -> "env"
      ReadsFilesystem -> "fs"
      HostDependent w -> w
      DepthLimit -> "recursion"
      Unsupported w -> w
      Failed _ -> "error"

stripQuoteMarks :: Value -> Value
stripQuoteMarks = mapKnown (T.filter (/= '\x1f'))

mapKnown :: (Text -> Text) -> Value -> Value
mapKnown f (Value ps) = Value (normalize (map (\case Known t -> Known (f t); o -> o) ps))

valueLines :: Value -> [Value]
valueLines (Value ps) = map (Value . normalize) (go [] ps)
  where
    go acc = \case
      [] -> [reverse acc]
      Known t : rest -> case T.splitOn "\n" t of
        [single] -> go (Known single : acc) rest
        first : more ->
          let (middle, lastOne) = (init more, last more)
           in reverse (Known first : acc) : map (pure . Known) middle <> go [Known lastOne] rest
        [] -> go acc rest
      o : rest -> go (o : acc) rest

valueWords :: Value -> [Value]
valueWords (Value ps) = filter (not . valueNull) (map (Value . normalize) (go [] ps))
  where
    go acc = \case
      [] -> [reverse acc]
      Known t : rest -> case T.split isSpace t of
        [single] -> go (Known single : acc) rest
        first : more ->
          let (middle, lastOne) = (init more, last more)
           in reverse (Known first : acc) : map (pure . Known) middle <> go [Known lastOne] rest
        [] -> go acc rest
      o : rest -> go (o : acc) rest

data Tri = Yes | No | Unknown [Reason]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

triNot :: Tri -> Tri
triNot = \case
  Yes -> No
  No -> Yes
  u -> u

triAnd :: Tri -> Tri -> Tri
triAnd No _ = No
triAnd _ No = No
triAnd Yes b = b
triAnd a Yes = a
triAnd (Unknown a) (Unknown b) = Unknown (a <> b)

triOr :: Tri -> Tri -> Tri
triOr Yes _ = Yes
triOr _ Yes = Yes
triOr No b = b
triOr a No = a
triOr (Unknown a) (Unknown b) = Unknown (a <> b)

data MacroBody
  = BodyRaw !Text
  | BodyValue !Value
  | BodyLiteral !Text
  | BodyOpaque !Reason
  | BodyMaybe !Reason
  | BodyUndefined
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Origin
  = OriginSpec !Span
  | OriginFile !FilePath !Int
  | OriginTarget
  | OriginTag !Span
  | OriginAuto
  | OriginPrelude
  | OriginMerged
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data MacroDef = MacroDef
  { macroOpts :: !(Maybe Text),
    macroBody :: !MacroBody,
    macroOrigin :: !Origin,
    macroLevel :: !Int,
    macroAuto :: !Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

instance Eq MacroDef where
  a == b =
    macroOpts a == macroOpts b
      && macroBody a == macroBody b
      && macroLevel a == macroLevel b
      && macroAuto a == macroAuto b

data Env = Env
  { envMacros :: !(Map Name [MacroDef]),
    envClosed :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Target = Target
  { targetArch :: !(Maybe Text),
    targetOs :: !(Maybe Text),
    targetMacros :: ![(Name, Maybe Text)]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

unknownTarget :: Target
unknownTarget = Target Nothing (Just "linux") []

data Severity = Info | Warn | Error
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

instance ToJSON Severity where
  toJSON = \case
    Info -> "info"
    Warn -> "warning"
    Error -> "error"

data EvalMessage = EvalMessage
  { emSeverity :: !Severity,
    emSpan :: !(Maybe Span),
    emText :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)
