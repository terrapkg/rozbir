-- | the lossless syntax tree for spec files. every character of the input,
-- whitespace and line endings included, lives somewhere in a t'Tree', so
-- rendering it gives back the exact source.
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

-- | a macro or tag name as written in the spec
type Name = Text

-- | a position in the source
data Pos = Pos
  { -- | offset from the start of the text, in characters
    posOffset :: !Int,
    -- | line number, starting at 0
    posLine :: !Int,
    -- | column in characters, starting at 0
    posCol :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | a range of source text
data Span = Span
  { -- | the first character
    spanStart :: !Pos,
    -- | just past the last character
    spanEnd :: !Pos
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON Pos where
  toJSON (Pos o l c) = object ["offset" .= o, "line" .= l, "column" .= c]

instance ToJSON Span where
  toJSON (Span s e) = object ["start" .= s, "end" .= e]

-- | the start of the text
startPos :: Pos
startPos = Pos 0 0 0

-- | move a position past some text
advancePos :: Pos -> Text -> Pos
advancePos = T.foldl' step
  where
    step (Pos o l c) = \case
      '\n' -> Pos (o + 1) (l + 1) 0
      _ -> Pos (o + 1) l (c + 1)

-- | an empty span at a position
pointSpan :: Pos -> Span
pointSpan p = Span p p

-- | whether a position is inside a span. both ends count, so a cursor right after the span is inside.
spanContains :: Span -> Pos -> Bool
spanContains (Span s e) p = posOffset s <= posOffset p && posOffset p <= posOffset e

-- | a piece of text that may contain macros
data Frag
  = Lit !Text
  | -- | @%%@, a literal percent sign
    Escape
  | Macro !MacroRef
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | a macro reference, kept exactly as written
data MacroRef
  = -- | @%name@ with its @?@ and @!@ flags. also covers arguments like @%1@, @%*@, @%#@ and @%-f@.
    Bare !Text !Name
  | -- | @%{name}@ with its flags and an optional argument
    Braced !Text !Name !(Maybe MacroArg)
  | -- | @%(...)@, a shell command
    Shell ![Frag]
  | -- | @%[...]@, an expression
    Expr ![Frag]
  | -- | @%{lua:...}@ with the raw Lua source
    Lua !Text
  | -- | a @%{@, @%(@ or @%[@ that is never closed, with the opening character and the rest of the text
    Unterminated !Char !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | the argument of a braced macro
data MacroArg
  = -- | the text after the colon in @%{name:arg}@
    ColonArg ![Frag]
  | -- | the text after the name in @%{name arg}@, leading space included
    SpaceArg ![Frag]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | the macro's name, @lua@ for Lua blocks. 'Nothing' for shell, expression and unterminated macros.
macroName :: MacroRef -> Maybe Name
macroName = \case
  Bare _ n -> Just n
  Braced _ n _ -> Just n
  Lua _ -> Just "lua"
  _ -> Nothing

-- | the @?@ and @!@ flags, empty when there are none
macroFlags :: MacroRef -> Text
macroFlags = \case
  Bare f _ -> f
  Braced f _ _ -> f
  _ -> ""

-- | one logical line. continuations and unclosed macros can make it span several physical lines.
data Line = Line
  { lineBody :: !LineBody,
    -- | the line ending, empty on a last line without one
    lineEol :: !Text,
    -- | where the body is, without the line ending
    lineSpan :: !Span
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | the kind of a line, with all of its text
data LineBody
  = -- | an empty or whitespace-only line
    Blank !Text
  | -- | a @#@ comment and its indentation. rpm expands macros in comments, so they are parsed.
    Comment !Text ![Frag]
  | -- | a preamble tag such as @Name:@
    Tag !TagLine
  | -- | @%global@, @%define@, @%undefine@ or a bcond
    Define !DefineLine
  | -- | a conditional such as @%if@ or @%endif@
    Cond !CondLine
  | -- | a section header such as @%build@
    Header !SectionHeader
  | -- | @%include@, with the indentation and keyword first
    Include !Text ![Frag]
  | -- | any other line, such as shell, a file list entry or a macro call in the preamble
    Body ![Frag]
  | -- | a preamble line that is none of the above. it also gets a @not-a-tag@ error.
    Bogus !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | a tag line, split up so it can be rendered back exactly
data TagLine = TagLine
  { -- | whitespace before the name
    tagIndent :: !Text,
    -- | the name as written. rpm ignores case.
    tagName :: !Text,
    -- | a qualifier such as @(post)@
    tagQualifier :: !(Maybe Qualifier),
    -- | the colon and the whitespace around it
    tagSep :: !Text,
    tagValue :: ![Frag],
    -- | trailing whitespace
    tagTrailing :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | a tag qualifier such as @Requires(post)@
data Qualifier = Qualifier
  { -- | whitespace between the tag name and the parenthesis
    qualGap :: !Text,
    -- | the qualifier with both parentheses
    qualRaw :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | which keyword a define line uses
data DefineKind
  = DefGlobal
  | DefDefine
  | DefUndefine
  | DefBcond
  | DefBcondWith
  | DefBcondWithout
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | a define line, split up so it can be rendered back exactly
data DefineLine = DefineLine
  { -- | whitespace before the keyword
    defIndent :: !Text,
    defKind :: !DefineKind,
    -- | the keyword as written, with the @%@
    defKeyword :: !Text,
    -- | whitespace before the name. for @%define@ it also holds dash options.
    defGap :: !Text,
    -- | the macro or bcond name
    defName :: !Name,
    -- | options of a parametric macro, without the parentheses. only for @%global@ and @%define@.
    defOpts :: !(Maybe Text),
    -- | the text in angle brackets after the name, without the brackets. only for @%global@ and @%define@.
    defMods :: !(Maybe Text),
    -- | whitespace between the name and the body
    defSep :: !Text,
    -- | the body, unexpanded
    defBody :: ![Frag],
    -- | trailing whitespace
    defTrailing :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | which conditional keyword a line uses
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

-- | whether the keyword opens a new block
isIfKind :: CondKind -> Bool
isIfKind k = k `elem` [CIf, CIfArch, CIfNArch, CIfOs, CIfNOs]

-- | whether the keyword is one of the @%elif@ forms
isElifKind :: CondKind -> Bool
isElifKind k = k `elem` [CElif, CElifArch, CElifOs]

-- | a conditional line
data CondLine = CondLine
  { -- | whitespace before the keyword
    condIndent :: !Text,
    condKind :: !CondKind,
    -- | the keyword as written, with the @%@
    condKeyword :: !Text,
    -- | everything after the keyword, leading whitespace included
    condArgs :: ![Frag]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | the sections rpm knows
data SectionKind
  = SPackage
  | SDescription
  | SPrep
  | -- | @%conf@, new in rpm 4.18
    SConf
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
  | -- | @%triggerin@, also written @%trigger@
    STriggerin
  | STriggerun
  | STriggerpostun
  | -- | @%filetriggerin@, also written @%filetrigger@
    SFiletriggerin
  | SFiletriggerun
  | SFiletriggerpostun
  | -- | @%transfiletriggerin@, also written @%transfiletrigger@
    STransfiletriggerin
  | STransfiletriggerun
  | STransfiletriggerpostun
  | SSepolicy
  | SPatchlist
  | SSourcelist
  | SEnd
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | section keywords without the @%@, aliases included. the parser matches them ignoring case.
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

-- | the main keyword for a section, without the @%@
sectionName :: SectionKind -> Text
sectionName k = maybe "" fst (lookupFirst k sectionKinds)
  where
    lookupFirst x = \case
      [] -> Nothing
      (n, y) : rest
        | y == x -> Just (n, y)
        | otherwise -> lookupFirst x rest

-- | whether the section is a scriptlet or trigger, from @%pre@ to @%transfiletriggerpostun@
isScriptSection :: SectionKind -> Bool
isScriptSection k = k >= SPre && k <= STransfiletriggerpostun

-- | whether the section is a build step, from @%prep@ to @%clean@
isBuildSection :: SectionKind -> Bool
isBuildSection k = k `elem` [SPrep, SConf, SGenerateBuildRequires, SBuild, SInstall, SCheck, SClean]

-- | whether tags are allowed here, which is before any section or inside @%package@
isPreambleLike :: Maybe SectionKind -> Bool
isPreambleLike = \case
  Nothing -> True
  Just SPackage -> True
  Just _ -> False

-- | a section header line
data SectionHeader = SectionHeader
  { secKind :: !SectionKind,
    -- | the keyword as written, with the @%@
    secKeyword :: !Text,
    -- | everything after the keyword, such as @ -n foo@
    secArgs :: ![Frag]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | a node of the tree
data Node
  = NLine !Line
  | -- | an @%if@ block. it can wrap whole sections or sit inside one.
    NCond !CondBlock
  | NSection !Section
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | a section, which runs until the next header at the same nesting level
data Section = Section
  { sectionLine :: !Line,
    -- | the parsed header, same as the one in 'sectionLine'
    sectionHeader :: !SectionHeader,
    sectionNodes :: ![Node]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | an @%if@ block with its @%elif@ and @%else@ branches
data CondBlock = CondBlock
  { -- | the branches in order, starting with the @%if@
    condBranches :: !(NonEmpty Branch),
    -- | the @%endif@ line. 'Nothing' when the block is never closed.
    condEnd :: !(Maybe Line)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | one branch of a conditional block
data Branch = Branch
  { -- | the @%if@, @%elif@ or @%else@ line
    branchLine :: !Line,
    -- | the parsed condition, same as the one in 'branchLine'
    branchCond :: !CondLine,
    branchNodes :: ![Node]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | a problem found while parsing. parsing still succeeds.
data SyntaxError = SyntaxError
  { -- | a short code such as @unclosed-if@, which lint uses as the rule id
    seCode :: !Text,
    -- | where the problem is
    seSpan :: !Span,
    seMessage :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | a parsed spec file
data Tree = Tree
  { treeNodes :: ![Node],
    -- | syntax errors, in source order
    treeErrors :: ![SyntaxError]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | every line of the tree in source order
treeLines :: Tree -> [Line]
treeLines = linesOfNodes . treeNodes

-- | every line of some nodes in source order
linesOfNodes :: [Node] -> [Line]
linesOfNodes = concatMap nodeLines

-- | every line of a node in source order
nodeLines :: Node -> [Line]
nodeLines = \case
  NLine l -> [l]
  NSection s -> sectionLine s : linesOfNodes (sectionNodes s)
  NCond c -> concatMap (\b -> branchLine b : linesOfNodes (branchNodes b)) (foldr (:) [] (condBranches c)) <> maybe [] pure (condEnd c)

-- | the source text of a line without its ending. the text must be the one the line was parsed from.
lineText :: Text -> Line -> Text
lineText src l =
  let Span s e = lineSpan l
   in T.take (posOffset e - posOffset s) (T.drop (posOffset s) src)

-- | the span of a line including its ending
lineFullSpan :: Line -> Span
lineFullSpan l = let Span s e = lineSpan l in Span s (advancePos e (lineEol l))
