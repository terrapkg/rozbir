module Rozbir.MacroFile
  ( MacroFileEntry (..),
    parseMacroFile,
    toDefinitions,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Rozbir.Syntax (Name)
import Rozbir.Syntax.Parse (isBlankChar, isNameChar)

data MacroFileEntry = MacroFileEntry
  { mfName :: !Name,
    mfOpts :: !(Maybe Text),
    mfBody :: !Text,
    mfLine :: !Int
  }
  deriving stock (Eq, Show)

parseMacroFile :: Text -> [MacroFileEntry]
parseMacroFile = go 1 . T.splitOn "\n"
  where
    go _ [] = []
    go n (l : ls) =
      let stripped = T.dropWhile isBlankChar l
       in case T.uncons stripped of
            Just ('%', _) ->
              let (taken, rest) = continuation (scan (0, 0, 0) (dropCr l)) [dropCr l] ls
                  joined = T.intercalate "\n" taken
               in maybe id (:) (entry n (T.drop 1 (T.dropWhile isBlankChar joined))) (go (n + length taken) rest)
            _ -> go (n + 1) ls
    dropCr t = maybe t id (T.stripSuffix "\r" t)
    continuation st acc ls
      | done st (last acc) = (acc, ls)
      | otherwise = case ls of
          [] -> (acc, [])
          x : xs -> let x' = dropCr x in continuation (scan st x') (acc <> [x']) xs
    done (pc, bc, xc) lastLine = pc == 0 && bc == 0 && xc == 0 && not ("\\" `T.isSuffixOf` lastLine)
    scan st = fst . T.foldl' step (st, False)
      where
        step ((pc, bc, xc), pending) c
          | pending = case c of
              '{' -> ((pc, bc + 1, xc), False)
              '(' -> ((pc + 1, bc, xc), False)
              '[' -> ((pc, bc, xc + 1), False)
              _ -> step ((pc, bc, xc), False) c
          | otherwise = case c of
              '%' -> ((pc, bc, xc), True)
              '{' | bc > 0 -> ((pc, bc + 1, xc), False)
              '}' | bc > 0 -> ((pc, bc - 1 :: Int, xc), False)
              '(' | pc > 0 -> ((pc + 1, bc, xc), False)
              ')' | pc > 0 -> ((pc - 1 :: Int, bc, xc), False)
              '[' | xc > 0 -> ((pc, bc, xc + 1), False)
              ']' | xc > 0 -> ((pc, bc, xc - 1 :: Int), False)
              _ -> ((pc, bc, xc), False)
    entry n t =
      let (name, afterName) = T.span isNameChar t
          (opts, afterOpts) = case T.stripPrefix "(" afterName of
            Just inner | (o, rest) <- T.break (== ')') inner, not (T.null rest) -> (Just o, T.drop 1 rest)
            _ -> (Nothing, afterName)
          body = T.dropWhile isBlankChar afterOpts
       in if T.null name || T.null body then Nothing else Just (MacroFileEntry name opts body n)

toDefinitions :: FilePath -> [MacroFileEntry] -> [(Name, Maybe Text, Text, FilePath, Int)]
toDefinitions path = map (\MacroFileEntry {..} -> (mfName, mfOpts, mfBody, path, mfLine))
