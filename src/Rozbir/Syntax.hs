module Rozbir.Syntax
  ( Name,
    Pos (..),
    Span (..),
    startPos,
    advancePos,
    pointSpan,
    spanContains,
    Line (..),
    LineBody (..),
    Frag (..),
    MacroRef (..),
    MacroArg (..),
    TagLine (..),
    Qualifier (..),
    DefineLine (..),
    DefineKind (..),
    CondLine (..),
    CondKind (..),
    isIfKind,
    isElifKind,
    SectionHeader (..),
    SectionKind (..),
    sectionKinds,
    sectionName,
    isScriptSection,
    isBuildSection,
    isPreambleLike,
    Node (..),
    Section (..),
    CondBlock (..),
    Branch (..),
    Tree (..),
    SyntaxError (..),
    treeLines,
    nodeLines,
    linesOfNodes,
    lineText,
    lineFullSpan,
    macroName,
    macroFlags,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

type Name = Text

data Pos = Pos
  { posOffset :: !Int,
    posLine :: !Int,
    posCol :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data Span = Span
  { spanStart :: !Pos,
    spanEnd :: !Pos
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON Pos where
  toJSON (Pos o l c) = object ["offset" .= o, "line" .= l, "column" .= c]

instance ToJSON Span where
  toJSON (Span s e) = object ["start" .= s, "end" .= e]

startPos :: Pos
startPos = Pos 0 0 0

advancePos :: Pos -> Text -> Pos
advancePos = T.foldl' step
  where
    step (Pos o l c) = \case
      '\n' -> Pos (o + 1) (l + 1) 0
      _ -> Pos (o + 1) l (c + 1)

pointSpan :: Pos -> Span
pointSpan p = Span p p

spanContains :: Span -> Pos -> Bool
spanContains (Span s e) p = posOffset s <= posOffset p && posOffset p <= posOffset e

data Frag
  = Lit !Text
  | Escape
  | Macro !MacroRef
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data MacroRef
  = Bare !Text !Name
  | Braced !Text !Name !(Maybe MacroArg)
  | Shell ![Frag]
  | Expr ![Frag]
  | Lua !Text
  | Unterminated !Char !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data MacroArg
  = ColonArg ![Frag]
  | SpaceArg ![Frag]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

macroName :: MacroRef -> Maybe Name
macroName = \case
  Bare _ n -> Just n
  Braced _ n _ -> Just n
  Lua _ -> Just "lua"
  _ -> Nothing

macroFlags :: MacroRef -> Text
macroFlags = \case
  Bare f _ -> f
  Braced f _ _ -> f
  _ -> ""

data Line = Line
  { lineBody :: !LineBody,
    lineEol :: !Text,
    lineSpan :: !Span
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data LineBody
  = Blank !Text
  | Comment !Text ![Frag]
  | Tag !TagLine
  | Define !DefineLine
  | Cond !CondLine
  | Header !SectionHeader
  | Include !Text ![Frag]
  | Body ![Frag]
  | Bogus !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data TagLine = TagLine
  { tagIndent :: !Text,
    tagName :: !Text,
    tagQualifier :: !(Maybe Qualifier),
    tagSep :: !Text,
    tagValue :: ![Frag],
    tagTrailing :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Qualifier = Qualifier
  { qualGap :: !Text,
    qualRaw :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data DefineKind
  = DefGlobal
  | DefDefine
  | DefUndefine
  | DefBcond
  | DefBcondWith
  | DefBcondWithout
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data DefineLine = DefineLine
  { defIndent :: !Text,
    defKind :: !DefineKind,
    defKeyword :: !Text,
    defGap :: !Text,
    defName :: !Name,
    defOpts :: !(Maybe Text),
    defMods :: !(Maybe Text),
    defSep :: !Text,
    defBody :: ![Frag],
    defTrailing :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data CondKind
  = CIf
  | CIfArch
  | CIfNArch
  | CIfOs
  | CIfNOs
  | CElif
  | CElifArch
  | CElifOs
  | CElse
  | CEndif
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

isIfKind :: CondKind -> Bool
isIfKind k = k `elem` [CIf, CIfArch, CIfNArch, CIfOs, CIfNOs]

isElifKind :: CondKind -> Bool
isElifKind k = k `elem` [CElif, CElifArch, CElifOs]

data CondLine = CondLine
  { condIndent :: !Text,
    condKind :: !CondKind,
    condKeyword :: !Text,
    condArgs :: ![Frag]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data SectionKind
  = SPackage
  | SDescription
  | SPrep
  | SConf
  | SGenerateBuildRequires
  | SBuild
  | SInstall
  | SCheck
  | SClean
  | SFiles
  | SChangelog
  | SPre
  | SPost
  | SPreun
  | SPostun
  | SPretrans
  | SPosttrans
  | SPreuntrans
  | SPostuntrans
  | SVerifyscript
  | STriggerprein
  | STriggerin
  | STriggerun
  | STriggerpostun
  | SFiletriggerin
  | SFiletriggerun
  | SFiletriggerpostun
  | STransfiletriggerin
  | STransfiletriggerun
  | STransfiletriggerpostun
  | SSepolicy
  | SPatchlist
  | SSourcelist
  | SEnd
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

sectionKinds :: [(Text, SectionKind)]
sectionKinds =
  [ ("package", SPackage),
    ("description", SDescription),
    ("prep", SPrep),
    ("conf", SConf),
    ("generate_buildrequires", SGenerateBuildRequires),
    ("build", SBuild),
    ("install", SInstall),
    ("check", SCheck),
    ("clean", SClean),
    ("files", SFiles),
    ("changelog", SChangelog),
    ("pre", SPre),
    ("post", SPost),
    ("preun", SPreun),
    ("postun", SPostun),
    ("pretrans", SPretrans),
    ("posttrans", SPosttrans),
    ("preuntrans", SPreuntrans),
    ("postuntrans", SPostuntrans),
    ("verifyscript", SVerifyscript),
    ("triggerprein", STriggerprein),
    ("triggerin", STriggerin),
    ("trigger", STriggerin),
    ("triggerun", STriggerun),
    ("triggerpostun", STriggerpostun),
    ("filetriggerin", SFiletriggerin),
    ("filetrigger", SFiletriggerin),
    ("filetriggerun", SFiletriggerun),
    ("filetriggerpostun", SFiletriggerpostun),
    ("transfiletriggerin", STransfiletriggerin),
    ("transfiletrigger", STransfiletriggerin),
    ("transfiletriggerun", STransfiletriggerun),
    ("transfiletriggerpostun", STransfiletriggerpostun),
    ("sepolicy", SSepolicy),
    ("patchlist", SPatchlist),
    ("sourcelist", SSourcelist),
    ("end", SEnd)
  ]

sectionName :: SectionKind -> Text
sectionName k = maybe "" fst (lookupFirst k sectionKinds)
  where
    lookupFirst x = \case
      [] -> Nothing
      (n, y) : rest
        | y == x -> Just (n, y)
        | otherwise -> lookupFirst x rest

isScriptSection :: SectionKind -> Bool
isScriptSection k = k >= SPre && k <= STransfiletriggerpostun

isBuildSection :: SectionKind -> Bool
isBuildSection k = k `elem` [SPrep, SConf, SGenerateBuildRequires, SBuild, SInstall, SCheck, SClean]

isPreambleLike :: Maybe SectionKind -> Bool
isPreambleLike = \case
  Nothing -> True
  Just SPackage -> True
  Just _ -> False

data SectionHeader = SectionHeader
  { secKind :: !SectionKind,
    secKeyword :: !Text,
    secArgs :: ![Frag]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Node
  = NLine !Line
  | NCond !CondBlock
  | NSection !Section
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Section = Section
  { sectionLine :: !Line,
    sectionHeader :: !SectionHeader,
    sectionNodes :: ![Node]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data CondBlock = CondBlock
  { condBranches :: !(NonEmpty Branch),
    condEnd :: !(Maybe Line)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Branch = Branch
  { branchLine :: !Line,
    branchCond :: !CondLine,
    branchNodes :: ![Node]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data SyntaxError = SyntaxError
  { seCode :: !Text,
    seSpan :: !Span,
    seMessage :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Tree = Tree
  { treeNodes :: ![Node],
    treeErrors :: ![SyntaxError]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

treeLines :: Tree -> [Line]
treeLines = linesOfNodes . treeNodes

linesOfNodes :: [Node] -> [Line]
linesOfNodes = concatMap nodeLines

nodeLines :: Node -> [Line]
nodeLines = \case
  NLine l -> [l]
  NSection s -> sectionLine s : linesOfNodes (sectionNodes s)
  NCond c -> concatMap (\b -> branchLine b : linesOfNodes (branchNodes b)) (foldr (:) [] (condBranches c)) <> maybe [] pure (condEnd c)

lineText :: Text -> Line -> Text
lineText src l =
  let Span s e = lineSpan l
   in T.take (posOffset e - posOffset s) (T.drop (posOffset s) src)

lineFullSpan :: Line -> Span
lineFullSpan l = let Span s e = lineSpan l in Span s (advancePos e (lineEol l))
