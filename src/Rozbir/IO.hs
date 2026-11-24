-- | reading specs and rpm macro files from disk. this is the only module in
-- rozbir that does IO.
module Rozbir.IO
  ( readSpec,
    readSpecText,
    resolveIncludes,
    macroPath,
    loadMacroFiles,
    loadSystemMacros,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Rozbir.Eval (Env, newEnv, withMacroFile)
import Rozbir.MacroFile (parseMacroFile, toDefinitions)
import Rozbir.Syntax
import Rozbir.Syntax.Parse (parse)
import Rozbir.Syntax.Render (renderFrags)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (isAbsolute, takeDirectory, takeFileName, (</>))

-- | read a file as UTF-8, replacing invalid bytes
readSpecText :: FilePath -> IO Text
readSpecText path = decodeUtf8Lenient <$> BS.readFile path

-- | read and parse a spec
readSpec :: FilePath -> IO Tree
readSpec path = parse <$> readSpecText path

-- | read the @%include@ files a spec uses, relative to the spec's directory,
-- for 'Rozbir.Spec.roIncludes'. files that are missing, unreadable or named
-- with a macro are left out.
resolveIncludes :: FilePath -> Tree -> IO (Map Text Tree)
resolveIncludes specPath = go (0 :: Int) Map.empty
  where
    dir = takeDirectory specPath
    go depth acc tree
      | depth > 16 = pure acc
      | otherwise = foldM (visit depth) acc (includeArgs tree)
    visit depth acc arg
      | Map.member arg acc || T.any (== '%') arg || T.null arg = pure acc
      | otherwise = do
          let path = T.unpack arg
              full = if isAbsolute path then path else dir </> path
          exists <- doesFileExist full
          if not exists
            then pure acc
            else do
              r <- try (readSpec full) :: IO (Either IOException Tree)
              case r of
                Left _ -> pure acc
                Right tree -> go (depth + 1) (Map.insert arg tree acc) tree
    includeArgs tree = [T.strip (renderFrags fs) | Line {lineBody = Include _ fs} <- treeLines tree]

-- | rpm's macro file search path for a platform like @x86_64-linux@. entries
-- can end in a @*@ glob.
macroPath :: Text -> [FilePath]
macroPath target =
  let t = T.unpack target
   in [ "/usr/lib/rpm/macros",
        "/usr/lib/rpm/macros.d/macros.*",
        "/usr/lib/rpm/platform/" <> t <> "/macros",
        "/usr/lib/rpm/redhat/macros",
        "/etc/rpm/macros.*",
        "/etc/rpm/macros",
        "/etc/rpm/" <> t <> "/macros"
      ]

-- | load macro files on top of an environment, in order. later files win, and
-- missing or unreadable files are skipped.
loadMacroFiles :: [FilePath] -> Env -> IO Env
loadMacroFiles patterns env0 = do
  files <- concat <$> mapM expandGlob patterns
  foldM load env0 files
  where
    load env file = do
      r <- try (BS.readFile file) :: IO (Either IOException BS.ByteString)
      pure $ case r of
        Left _ -> env
        Right bytes -> withMacroFile (toDefinitions file (parseMacroFile (decodeUtf8Lenient bytes))) env

expandGlob :: FilePath -> IO [FilePath]
expandGlob pat
  | '*' `notElem` name = do
      exists <- doesFileExist pat
      pure [pat | exists]
  | otherwise = do
      exists <- doesDirectoryExist dir
      if not exists
        then pure []
        else do
          entries <- sort <$> listDirectory dir
          let (prefix, rest) = break (== '*') name
              suffixPart = drop 1 rest
          pure [dir </> e | e <- entries, prefix `isPrefixOf` e, suffixPart `isSuffixOf` e, not ("~" `isSuffixOf` e), not (".rpmnew" `isSuffixOf` e), not (".rpmsave" `isSuffixOf` e)]
  where
    dir = takeDirectory pat
    name = takeFileName pat

-- | load this system's rpm macros for an arch (default @x86_64@) into a
-- closed environment, where undefined macros stay as text like in rpm
loadSystemMacros :: Maybe Text -> IO Env
loadSystemMacros arch = loadMacroFiles (macroPath (fromMaybe "x86_64" arch <> "-linux")) (newEnv True)
