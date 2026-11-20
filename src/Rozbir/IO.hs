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

readSpecText :: FilePath -> IO Text
readSpecText path = decodeUtf8Lenient <$> BS.readFile path

readSpec :: FilePath -> IO Tree
readSpec path = parse <$> readSpecText path

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

loadSystemMacros :: Maybe Text -> IO Env
loadSystemMacros arch = loadMacroFiles (macroPath (fromMaybe "x86_64" arch <> "-linux")) (newEnv True)
