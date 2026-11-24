-- | values, environments and targets for partial evaluation. a t'Value' is known
-- text mixed with 'Opaque' holes for parts that can't be expanded without
-- running something, and conditions are three-valued ('Tri').
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

-- | why part of a value couldn't be expanded
data Reason
  = -- | a @%(...)@ the pure shell subset can't handle
    RunsShell
  | -- | Lua code, which is never run
    RunsLua
  | -- | reads environment variables
    ReadsEnv
  | -- | reads the filesystem
    ReadsFilesystem
  | -- | depends on the build host, like the buildroot
    HostDependent Text
  | -- | not defined here, but system macros might define it
    Undefined Name
  | -- | defined differently depending on a condition that can't be decided
    Ambiguous Name
  | -- | nested too deeply or ran out of fuel, usually recursion
    DepthLimit
  | -- | something rozbir doesn't model
    Unsupported Text
  | -- | an error, like a bad expression or @%{error:...}@
    Failed Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | a short human-readable explanation
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

-- | one part of a t'Value'
data Piece
  = -- | fully expanded text
    Known !Text
  | -- | a hole, with its source location when known
    Opaque !Reason !(Maybe Span)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | an expansion result. '<>' joins adjacent known text and drops empty pieces.
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

-- | a fully known value
known :: Text -> Value
known t = Value (normalize [Known t])

-- | a single hole without a location
opaque :: Reason -> Value
opaque r = Value [Opaque r Nothing]

-- | a single hole at a source location
opaqueAt :: Reason -> Span -> Value
opaqueAt r s = Value [Opaque r (Just s)]

-- | the text, if there are no holes
knownText :: Value -> Maybe Text
knownText (Value ps) = T.concat <$> traverse (\case Known t -> Just t; Opaque {} -> Nothing) ps

-- | whether there are no holes
isKnown :: Value -> Bool
isKnown (Value ps) = all (\case Known _ -> True; _ -> False) ps

-- | the reason for each hole, in order
reasons :: Value -> [Reason]
reasons (Value ps) = [r | Opaque r _ <- ps]

-- | whether the value is empty
valueNull :: Value -> Bool
valueNull (Value ps) = null ps

-- | render for people, with holes as short tags like @‹%version›@
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

-- | remove the markers @%{quote:...}@ adds to keep text as one argument
stripQuoteMarks :: Value -> Value
stripQuoteMarks = mapKnown (T.filter (/= '\x1f'))

-- | apply a function to the known text, leaving holes alone
mapKnown :: (Text -> Text) -> Value -> Value
mapKnown f (Value ps) = Value (normalize (map (\case Known t -> Known (f t); o -> o) ps))

-- | split on newlines. empty lines are kept.
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

-- | split on whitespace, dropping empty words. a hole stays with the word it
-- touches.
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

-- | a yes, no or unknown answer. 'Unknown' says why it couldn't be decided.
data Tri
  = Yes
  | No
  | -- | can't be decided, with the reasons why
    Unknown [Reason]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | negate. 'Unknown' stays 'Unknown'.
triNot :: Tri -> Tri
triNot = \case
  Yes -> No
  No -> Yes
  u -> u

-- | and. 'No' takes precedence over 'Unknown'.
triAnd :: Tri -> Tri -> Tri
triAnd No _ = No
triAnd _ No = No
triAnd Yes b = b
triAnd a Yes = a
triAnd (Unknown a) (Unknown b) = Unknown (a <> b)

-- | or. 'Yes' takes precedence over 'Unknown'.
triOr :: Tri -> Tri -> Tri
triOr Yes _ = Yes
triOr _ Yes = Yes
triOr No b = b
triOr a No = a
triOr (Unknown a) (Unknown b) = Unknown (a <> b)

-- | what a macro expands to
data MacroBody
  = -- | text expanded each time it's used, like @%define@
    BodyRaw !Text
  | -- | an already expanded value, like @%global@. rpm still expands it again on use.
    BodyValue !Value
  | -- | text used as is, without expanding
    BodyLiteral !Text
  | -- | defined, but the body can't be known
    BodyOpaque !Reason
  | -- | might or might not be defined
    BodyMaybe !Reason
  | -- | explicitly undefined
    BodyUndefined
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | where a definition came from
data Origin
  = -- | a @%define@ or @%global@ in the spec
    OriginSpec !Span
  | -- | a macro file and line
    OriginFile !FilePath !Int
  | -- | the t'Target'
    OriginTarget
  | -- | a tag, like @Version:@ defining @%version@
    OriginTag !Span
  | -- | the evaluator, like macro arguments
    OriginAuto
  | -- | rozbir's copy of rpm's built-in macros
    OriginPrelude
  | -- | merged from branches that couldn't be decided
    OriginMerged
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | one macro definition. equality ignores the origin.
data MacroDef = MacroDef
  { -- | getopt options for a macro that takes arguments
    macroOpts :: !(Maybe Text),
    macroBody :: !MacroBody,
    macroOrigin :: !Origin,
    -- | the call depth it was defined at. 0 is global, others go away when that call returns.
    macroLevel :: !Int,
    -- | a macro argument like @%1@ or @%*@, hidden from nested calls
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

-- | the macro environment
data Env = Env
  { -- | a stack of definitions per name, newest first
    envMacros :: !(Map Name [MacroDef]),
    -- | whether system macros are loaded. if not, unknown names become 'Undefined' holes instead of literal text.
    envClosed :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | what to evaluate for. 'Nothing' means unknown.
data Target = Target
  { -- | sets @%_arch@ and @%_target_cpu@
    targetArch :: !(Maybe Text),
    -- | sets @%_os@ and @%_target_os@
    targetOs :: !(Maybe Text),
    -- | macro overrides. 'Nothing' undefines.
    targetMacros :: ![(Name, Maybe Text)]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Linux with an unknown arch
unknownTarget :: Target
unknownTarget = Target Nothing (Just "linux") []

-- | how serious a message is
data Severity
  = -- | informational, like @%{echo:...}@
    Info
  | -- | a warning, like @%{warn:...}@
    Warn
  | -- | an error, like @%{error:...}@ or a bad expression
    Error
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

instance ToJSON Severity where
  toJSON = \case
    Info -> "info"
    Warn -> "warning"
    Error -> "error"

-- | a message from evaluation, like a bad expression or @%{warn:...}@
data EvalMessage = EvalMessage
  { emSeverity :: !Severity,
    emSpan :: !(Maybe Span),
    emText :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)
