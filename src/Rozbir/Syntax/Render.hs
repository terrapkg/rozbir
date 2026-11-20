module Rozbir.Syntax.Render
  ( render,
    renderNodes,
    renderLine,
    renderBody,
    renderFrags,
    renderFrag,
    renderMacro,
    renderTagValue,
    renderDefineBody,
    renderCondArgs,
    renderSectionArgs,
    fragSpans,
    macroSpans,
    lineFrags,
  )
where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)
import Rozbir.Syntax

render :: Tree -> Text
render = renderNodes . treeNodes

renderNodes :: [Node] -> Text
renderNodes = TL.toStrict . toLazyText . foldMap lineB . linesOfNodes

lineB :: Line -> Builder
lineB l = bodyB (lineBody l) <> fromText (lineEol l)

renderLine :: Line -> Text
renderLine = TL.toStrict . toLazyText . lineB

renderBody :: LineBody -> Text
renderBody = TL.toStrict . toLazyText . bodyB

renderFrags :: [Frag] -> Text
renderFrags = TL.toStrict . toLazyText . fragsB

renderFrag :: Frag -> Text
renderFrag = TL.toStrict . toLazyText . fragB

renderMacro :: MacroRef -> Text
renderMacro = TL.toStrict . toLazyText . macroB

renderTagValue :: TagLine -> Text
renderTagValue = renderFrags . tagValue

renderDefineBody :: DefineLine -> Text
renderDefineBody = renderFrags . defBody

renderCondArgs :: CondLine -> Text
renderCondArgs = renderFrags . condArgs

renderSectionArgs :: SectionHeader -> Text
renderSectionArgs = renderFrags . secArgs

bodyB :: LineBody -> Builder
bodyB = \case
  Blank t -> fromText t
  Comment indent fs -> fromText indent <> fragsB fs
  Tag TagLine {..} ->
    fromText tagIndent
      <> fromText tagName
      <> maybe mempty (\q -> fromText (qualGap q) <> fromText (qualRaw q)) tagQualifier
      <> fromText tagSep
      <> fragsB tagValue
      <> fromText tagTrailing
  Define DefineLine {..} ->
    fromText defIndent
      <> fromText defKeyword
      <> fromText defGap
      <> fromText defName
      <> maybe mempty (\o -> singleton '(' <> fromText o <> singleton ')') defOpts
      <> maybe mempty (\m -> singleton '<' <> fromText m <> singleton '>') defMods
      <> fromText defSep
      <> fragsB defBody
      <> fromText defTrailing
  Cond CondLine {..} -> fromText condIndent <> fromText condKeyword <> fragsB condArgs
  Header SectionHeader {..} -> fromText secKeyword <> fragsB secArgs
  Include prefix fs -> fromText prefix <> fragsB fs
  Body fs -> fragsB fs
  Bogus t -> fromText t

fragsB :: [Frag] -> Builder
fragsB = foldMap fragB

fragB :: Frag -> Builder
fragB = \case
  Lit t -> fromText t
  Escape -> fromText "%%"
  Macro m -> macroB m

macroB :: MacroRef -> Builder
macroB = \case
  Bare flags name -> singleton '%' <> fromText flags <> fromText name
  Braced flags name arg ->
    fromText "%{"
      <> fromText flags
      <> fromText name
      <> maybe mempty argB arg
      <> singleton '}'
  Shell fs -> fromText "%(" <> fragsB fs <> singleton ')'
  Expr fs -> fromText "%[" <> fragsB fs <> singleton ']'
  Lua t -> fromText "%{lua:" <> fromText t <> singleton '}'
  Unterminated c t -> singleton '%' <> singleton c <> fromText t
  where
    argB = \case
      ColonArg fs -> singleton ':' <> fragsB fs
      SpaceArg fs -> fragsB fs

fragSpans :: Pos -> [Frag] -> [(Span, Frag)]
fragSpans p = \case
  [] -> []
  f : fs -> let e = advancePos p (renderFrag f) in (Span p e, f) : fragSpans e fs

macroSpans :: Pos -> [Frag] -> [(Span, MacroRef)]
macroSpans p fs = concat [(sp, m) : inner sp m | (sp, Macro m) <- fragSpans p fs]
  where
    inner (Span s _) = \case
      Braced fl n (Just a) ->
        let s' = advancePos s ("%{" <> fl <> n)
         in case a of
              ColonArg xs -> macroSpans (advancePos s' ":") xs
              SpaceArg xs -> macroSpans s' xs
      Shell xs -> macroSpans (advancePos s "%(") xs
      Expr xs -> macroSpans (advancePos s "%[") xs
      _ -> []

lineFrags :: Line -> [(Pos, [Frag])]
lineFrags l = case lineBody l of
  Comment indent fs -> [(at indent, fs)]
  Tag TagLine {..} -> [(at (tagIndent <> tagName <> maybe "" (\q -> qualGap q <> qualRaw q) tagQualifier <> tagSep), tagValue)]
  Define DefineLine {..} ->
    [ ( at
          ( defIndent
              <> defKeyword
              <> defGap
              <> defName
              <> maybe "" (\o -> "(" <> o <> ")") defOpts
              <> maybe "" (\m -> "<" <> m <> ">") defMods
              <> defSep
          ),
        defBody
      )
    ]
  Cond CondLine {..} -> [(at (condIndent <> condKeyword), condArgs)]
  Header SectionHeader {..} -> [(at secKeyword, secArgs)]
  Include prefix fs -> [(at prefix, fs)]
  Body fs -> [(at "", fs)]
  _ -> []
  where
    at prefix = advancePos (spanStart (lineSpan l)) prefix
