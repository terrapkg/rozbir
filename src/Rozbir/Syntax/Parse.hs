-- | the spec parser. it never fails and loses nothing, so @render (parse s) == s@
-- for any text. problems end up in 'treeErrors' instead.
module Rozbir.Syntax.Parse
  ( parse,
    parseLines,
    parseFrags,
    classifyLine,
    matchClose,
    isNameChar,
    isHSpace,
    isBlankChar,
    splitTrailing,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Rozbir.Syntax

-- | parse a whole spec into a tree
parse :: Text -> Tree
parse src =
  let (ls, lineErrs) = parseLines Nothing src
      (nodes, treeErrs) = buildTree ls
   in Tree (groupSections nodes) (lineErrs <> treeErrs)

-- | split text into lines without building the tree. the section decides whether lines can be tags, and positions start at 'startPos'.
parseLines :: Maybe SectionKind -> Text -> ([Line], [SyntaxError])
parseLines sec0 src = go sec0 startPos (logicalLines src)
  where
    go _ _ [] = ([], [])
    go sec pos (LogicalLine {..} : rest) =
      let body = classifyLine sec llBody
          end = advancePos pos llBody
          sp = Span pos end
          next = advancePos end llEol
          sec' = case body of
            Header h -> Just (secKind h)
            _ -> sec
          errs =
            [SyntaxError code sp msg | Just (code, msg) <- [llProblem]]
              <> [SyntaxError "not-a-tag" sp "this line isn't a tag, a macro or a section header" | Bogus _ <- [body]]
          (ls, es) = go sec' next rest
       in (Line body llEol sp : ls, errs <> es)

data LogicalLine = LogicalLine
  { llBody :: Text,
    llEol :: Text,
    llProblem :: Maybe (Text, Text)
  }

data Scan = Scan !Int !Int !Int !Bool

logicalLines :: Text -> [LogicalLine]
logicalLines = assemble . physicalLines
  where
    assemble [] = []
    assemble phys@(first : _) = case collect (continuesWithBackslash first) (Scan 0 0 0 False) [] phys of
      Right (taken, rest) -> finish (T.concat taken) Nothing : assemble rest
      Left problem -> finish first (Just problem) : assemble (drop 1 phys)

    collect joinable st acc = \case
      [] -> Left (unclosed st)
      p : ps ->
        let st'@(Scan pc bc xc nc) = scanText st p
            acc' = p : acc
         in if pc == 0 && bc == 0 && xc == 0 && not (joinable && nc)
              then Right (reverse acc', ps)
              else collect joinable st' acc' ps

    unclosed (Scan pc bc xc _)
      | pc + bc + xc > 0 = ("unclosed-macro", "this macro is never closed, so rpm reads the rest of the file into it")
      | otherwise = ("trailing-backslash", "the last line ends in a backslash with nothing to continue onto")

    finish t problem
      | Just b <- T.stripSuffix "\r\n" t = LogicalLine b "\r\n" problem
      | Just b <- T.stripSuffix "\n" t = LogicalLine b "\n" problem
      | otherwise = LogicalLine t "" problem

continuesWithBackslash :: Text -> Bool
continuesWithBackslash line =
  let rest = T.dropWhile isHSpace line
   in isDefine rest || isJust (condLine "" rest) || isJust (keywordWithArg "%include" rest)
  where
    isDefine t = case T.stripPrefix "%" t of
      Just after ->
        let (word, more) = T.span isNameChar after
         in isJust (lookup word defineKeywords) && maybe False (isBlankChar . fst) (T.uncons more)
      Nothing -> False

physicalLines :: Text -> [Text]
physicalLines t
  | T.null t = []
  | otherwise = case T.break (== '\n') t of
      (pre, rest)
        | T.null rest -> [pre]
        | otherwise -> (pre <> "\n") : physicalLines (T.drop 1 rest)

data Mode = Normal | AfterBackslash | AfterPercent

scanText :: Scan -> Text -> Scan
scanText st0 = fst . T.foldl' step (st0, Normal)
  where
    step (s@(Scan pc bc xc nc), mode) c = case mode of
      AfterBackslash
        | c == '\n' -> (Scan pc bc xc True, Normal)
        | otherwise -> (s, Normal)
      AfterPercent -> case c of
        '{' -> (Scan pc (bc + 1) xc nc, Normal)
        '(' -> (Scan (pc + 1) bc xc nc, Normal)
        '[' -> (Scan pc bc (xc + 1) nc, Normal)
        '%' -> (s, Normal)
        _ -> step (s, Normal) c
      Normal -> case c of
        '\\' -> (s, AfterBackslash)
        '\n' -> (Scan pc bc xc False, Normal)
        '%' -> (s, AfterPercent)
        '{' | bc > 0 -> (Scan pc (bc + 1) xc nc, Normal)
        '}' | bc > 0 -> (Scan pc (bc - 1) xc nc, Normal)
        '(' | pc > 0 -> (Scan (pc + 1) bc xc nc, Normal)
        ')' | pc > 0 -> (Scan (pc - 1) bc xc nc, Normal)
        '[' | xc > 0 -> (Scan pc bc (xc + 1) nc, Normal)
        ']' | xc > 0 -> (Scan pc bc (xc - 1) nc, Normal)
        _ -> (s, Normal)

-- | whitespace other than a newline
isHSpace :: Char -> Bool
isHSpace c = c /= '\n' && isSpace c

-- | a space or a tab
isBlankChar :: Char -> Bool
isBlankChar c = c == ' ' || c == '\t'

-- | a character allowed in a macro name, which is an ASCII letter, digit or underscore
isNameChar :: Char -> Bool
isNameChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiLower c || isAsciiUpper c

-- | split off trailing whitespace
splitTrailing :: Text -> (Text, Text)
splitTrailing t = let tr = T.takeWhileEnd isHSpace t in (T.dropEnd (T.length tr) t, tr)

-- | work out what one line is, given the section it is in. the text must not include the line ending.
classifyLine :: Maybe SectionKind -> Text -> LineBody
classifyLine sec body
  | T.null rest = Blank body
  | "#" `T.isPrefixOf` rest = Comment indent (parseFrags rest)
  | Just c <- condLine indent rest = Cond c
  | T.null indent, Just h <- headerLine rest = Header h
  | Just d <- defineLine indent rest = Define d
  | Just after <- keywordWithArg "%include" rest = Include (indent <> "%include") (parseFrags after)
  | isPreambleLike sec, Just t <- tagLine indent rest = Tag t
  | isPreambleLike sec, not ("%" `T.isPrefixOf` rest) = Bogus body
  | otherwise = Body (parseFrags body)
  where
    (indent, rest) = T.span isHSpace body

keywordWithArg :: Text -> Text -> Maybe Text
keywordWithArg kw t = do
  after <- T.stripPrefix kw t
  case T.uncons after of
    Nothing -> Just after
    Just (c, _) | isBlankChar c -> Just after
    _ -> Nothing

keywordAlone :: Text -> Text -> Maybe Text
keywordAlone kw t = do
  after <- T.stripPrefix kw t
  case T.uncons after of
    Just (c, _) | isAsciiAlpha c -> Nothing
    _ -> Just after

condLine :: Text -> Text -> Maybe CondLine
condLine indent rest = firstJust candidates
  where
    candidates =
      [ (CEndif, "%endif", keywordAlone),
        (CElse, "%else", keywordAlone),
        (CIf, "%if", keywordWithArg),
        (CIfArch, "%ifarch", keywordWithArg),
        (CIfNArch, "%ifnarch", keywordWithArg),
        (CIfOs, "%ifos", keywordWithArg),
        (CIfNOs, "%ifnos", keywordWithArg),
        (CElifArch, "%elifarch", keywordWithArg),
        (CElifOs, "%elifos", keywordWithArg),
        (CElif, "%elif", keywordWithArg)
      ]
    firstJust = \case
      [] -> Nothing
      (kind, kw, match) : more -> case match kw rest of
        Just after -> Just (CondLine indent kind kw (parseFrags after))
        Nothing -> firstJust more

headerLine :: Text -> Maybe SectionHeader
headerLine t = do
  after <- T.stripPrefix "%" t
  let (word, more) = T.span isNameChar after
  kind <- lookup (T.toLower word) sectionKinds
  case T.uncons more of
    Just (c, _) | not (isSpace c) -> Nothing
    _ -> Just (SectionHeader kind ("%" <> word) (parseFrags more))

defineKeywords :: [(Text, DefineKind)]
defineKeywords =
  [ ("global", DefGlobal),
    ("define", DefDefine),
    ("undefine", DefUndefine),
    ("bcond", DefBcond),
    ("bcond_with", DefBcondWith),
    ("bcond_without", DefBcondWithout)
  ]

defineLine :: Text -> Text -> Maybe DefineLine
defineLine indent t = do
  after <- T.stripPrefix "%" t
  let (word, more) = T.span isNameChar after
  kind <- lookup word defineKeywords
  (c, _) <- T.uncons more
  if not (isBlankChar c)
    then Nothing
    else do
      let (gap, afterGap) = defineGap kind more
          (name, afterName) = T.span isNameChar afterGap
      if T.null name
        then Nothing
        else do
          (opts, afterOpts) <- delimitedOpt '(' ')' afterName kind
          (mods, afterMods) <- delimitedOpt '<' '>' afterOpts kind
          let (sep, bodyText) = T.span isHSpace afterMods
              (bodyCore, trailing) = splitTrailing bodyText
          Just
            DefineLine
              { defIndent = indent,
                defKind = kind,
                defKeyword = "%" <> word,
                defGap = gap,
                defName = name,
                defOpts = opts,
                defMods = mods,
                defSep = sep,
                defBody = parseFrags bodyCore,
                defTrailing = trailing
              }
  where
    delimitedOpt open close s kind
      | kind `notElem` [DefGlobal, DefDefine] = Just (Nothing, s)
      | Just inner <- T.stripPrefix (T.singleton open) s = case T.break (== close) inner of
          (o, rest) | not (T.null rest) -> Just (Just o, T.drop 1 rest)
          _ -> Nothing
      | otherwise = Just (Nothing, s)

defineGap :: DefineKind -> Text -> (Text, Text)
defineGap kind s0
  | kind == DefDefine = options s0
  | otherwise = T.span isBlankChar s0
  where
    options s =
      let (ws, rest) = T.span isBlankChar s
       in case T.uncons rest of
            Just ('-', more) ->
              let (letters, rest') = T.span (not . isSpace) more
                  (ws2, rest'') = options rest'
               in if T.null letters then (ws, rest) else (ws <> "-" <> letters <> ws2, rest'')
            _ -> (ws, rest)

tagLine :: Text -> Text -> Maybe TagLine
tagLine indent t = do
  (c, _) <- T.uncons t
  if not (isAsciiAlpha c)
    then Nothing
    else do
      let (name, afterName) = T.span isNameChar t
          (gap, afterGap) = T.span isHSpace afterName
      (qualifier, afterQual) <- case T.uncons afterGap of
        Just ('(', _) -> case T.break (== ')') afterGap of
          (q, rest) | not (T.null rest) -> Just (Just (Qualifier gap (q <> ")")), T.drop 1 rest)
          _ -> Nothing
        _ -> Just (Nothing, afterName)
      let (preColon, afterPre) = T.span isHSpace afterQual
      afterColon <- T.stripPrefix ":" afterPre
      let (postColon, valueText) = T.span isHSpace afterColon
          (valueCore, trailing) = splitTrailing valueText
      Just
        TagLine
          { tagIndent = indent,
            tagName = name,
            tagQualifier = qualifier,
            tagSep = preColon <> ":" <> postColon,
            tagValue = parseFrags valueCore,
            tagTrailing = trailing
          }

-- | split text into literal text and macros. an unclosed macro takes the rest of the text.
parseFrags :: Text -> [Frag]
parseFrags = mergeLits . go
  where
    go t = case T.break (== '%') t of
      (pre, rest)
        | T.null rest -> lit pre []
        | otherwise -> lit pre (percent (T.drop 1 rest))
    lit p xs = if T.null p then xs else Lit p : xs
    percent r = case T.uncons r of
      Nothing -> [Lit "%"]
      Just ('%', r') -> Escape : go r'
      Just ('(', _) -> delimited '(' ')' Shell r
      Just ('[', _) -> delimited '[' ']' Expr r
      Just ('{', _) -> braced r
      _ -> bare r
    delimited open close ctor r = case matchClose open close r of
      Just (inner, after) -> Macro (ctor (go inner)) : go after
      Nothing -> [Macro (Unterminated open (T.drop 1 r))]
    braced r = case matchClose '{' '}' r of
      Nothing -> [Macro (Unterminated '{' (T.drop 1 r))]
      Just (inner, after) -> Macro (bracedRef inner) : go after
    bracedRef inner =
      let (flags, i1) = T.span (`elem` ['?', '!']) inner
          (name, i2) = T.break (`elem` [' ', ':', '}']) i1
       in case T.uncons i2 of
            Nothing -> Braced flags name Nothing
            Just (':', a)
              | T.null flags && name == "lua" -> Lua a
              | otherwise -> Braced flags name (Just (ColonArg (mergeLits (go a))))
            Just (' ', _) -> Braced flags name (Just (SpaceArg (mergeLits (go i2))))
            Just _ -> Braced flags i1 Nothing
    bare r =
      let (flags, r1) = T.span (`elem` ['?', '!']) r
          (dash, r2) = maybe ("", r1) ("-",) (T.stripPrefix "-" r1)
          (nm, r3) = T.span isNameChar r2
          (suffix, r4)
            | "**" `T.isPrefixOf` r3 = ("**", T.drop 2 r3)
            | Just (c, more) <- T.uncons r3, c == '*' || c == '#' = (T.singleton c, more)
            | otherwise = ("", r3)
          name = dash <> nm <> suffix
       in if T.null name && T.null flags
            then Lit "%" : go r
            else Macro (Bare flags name) : go r4

mergeLits :: [Frag] -> [Frag]
mergeLits = \case
  Lit a : Lit b : rest -> mergeLits (Lit (a <> b) : rest)
  Macro m : rest -> Macro (inner m) : mergeLits rest
  x : rest -> x : mergeLits rest
  [] -> []
  where
    inner = \case
      Braced f n (Just (ColonArg fs)) -> Braced f n (Just (ColonArg (mergeLits fs)))
      Braced f n (Just (SpaceArg fs)) -> Braced f n (Just (SpaceArg (mergeLits fs)))
      Shell fs -> Shell (mergeLits fs)
      Expr fs -> Expr (mergeLits fs)
      m -> m

-- | for text starting with an opening character, return what is inside the matching close and what comes after it.
-- nesting and backslash escapes are respected. 'Nothing' when it is never closed.
matchClose :: Char -> Char -> Text -> Maybe (Text, Text)
matchClose open close t = go (0 :: Int) 0 (T.unpack t)
  where
    go _ _ [] = Nothing
    go lvl i (c : cs)
      | c == '\\' = case cs of
          [] -> Nothing
          _ : cs' -> go lvl (i + 2) cs'
      | c == close =
          if lvl - 1 <= 0
            then Just (T.take (i - 1) (T.drop 1 t), T.drop (i + 1) t)
            else go (lvl - 1) (i + 1) cs
      | c == open = go (lvl + 1) (i + 1) cs
      | otherwise = go lvl (i + 1) cs

data Frame = Frame
  { frDone :: [Branch],
    frLine :: Line,
    frCond :: CondLine,
    frNodes :: [Node]
  }

buildTree :: [Line] -> ([Node], [SyntaxError])
buildTree = go [] [] []
  where
    go :: [Frame] -> [Node] -> [SyntaxError] -> [Line] -> ([Node], [SyntaxError])
    go stack top errs [] = closeAll stack top errs
    go stack top errs (l : ls) = case lineBody l of
      Cond c
        | isIfKind (condKind c) -> go (Frame [] l c [] : stack) top errs ls
        | otherwise -> case stack of
            [] -> go stack (NLine l : top) (stray c l : errs) ls
            fr : frs
              | condKind c == CEndif ->
                  let block = NCond (CondBlock (branchesOf fr) (Just l))
                   in push frs top errs block ls
              | otherwise ->
                  let errs' = [afterElse c l | condKind (frCond fr) == CElse] <> errs
                   in go (Frame (closeBranch fr : frDone fr) l c [] : frs) top errs' ls
      _ -> push stack top errs (NLine l) ls

    push [] top errs n ls = go [] (n : top) errs ls
    push (fr : frs) top errs n ls = go (fr {frNodes = n : frNodes fr} : frs) top errs ls

    closeBranch fr = Branch (frLine fr) (frCond fr) (reverse (frNodes fr))
    branchesOf fr = NE.fromList (reverse (closeBranch fr : frDone fr))

    closeAll [] top errs = (reverse top, reverse errs)
    closeAll (fr : frs) top errs =
      let block = NCond (CondBlock (branchesOf fr) Nothing)
          err = SyntaxError "unclosed-if" (lineSpan (firstLine fr)) (condKeyword (firstCond fr) <> " is never closed with %endif")
       in case frs of
            [] -> closeAll [] (block : top) (err : errs)
            parent : rest -> closeAll (parent {frNodes = block : frNodes parent} : rest) top (err : errs)

    firstBranch fr = NE.head (branchesOf fr)
    firstLine = branchLine . firstBranch
    firstCond = branchCond . firstBranch

    stray c l = SyntaxError (strayCode c) (lineSpan l) (condKeyword c <> " has no %if to belong to")
    strayCode c = case condKind c of
      CEndif -> "stray-endif"
      CElse -> "stray-else"
      _ -> "stray-elif"
    afterElse c l = SyntaxError "branch-after-else" (lineSpan l) (condKeyword c <> " comes after %else, so it can never be reached")

groupSections :: [Node] -> [Node]
groupSections = \case
  [] -> []
  NLine l@(Line (Header h) _ _) : rest ->
    let (body, rest') = break isHeader rest
     in NSection (Section l h (map deep body)) : groupSections rest'
  n : rest -> deep n : groupSections rest
  where
    isHeader = \case
      NLine (Line (Header _) _ _) -> True
      _ -> False
    deep = \case
      NCond (CondBlock bs end) -> NCond (CondBlock (fmap (\b -> b {branchNodes = groupSections (branchNodes b)}) bs) end)
      n -> n
