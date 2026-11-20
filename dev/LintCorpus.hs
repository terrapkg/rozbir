{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Rozbir.Eval (Target (..), newEnv)
import Rozbir.IO (loadSystemMacros, readSpec, resolveIncludes)
import Rozbir.Lint
import Rozbir.Syntax (Pos (..), Span (..))
import System.Environment (getArgs)

main :: IO ()
main = do
  list : rule : flags <- getArgs
  env <- if "system" `elem` flags then loadSystemMacros (Just "x86_64") else pure (newEnv False)
  files <- lines <$> readFile list
  let cfg = defaultConfig {lcRuleSets = Set.fromList [minBound .. maxBound]}
      targets = [Target (Just "x86_64") (Just "linux") [("dist", Just ".fc44"), ("fedora", Just "44")]]
  diags <- fmap concat $ forM files $ \f -> do
    tree <- readSpec f
    includes <- resolveIncludes f tree
    pure [(f, d) | d <- lintTree cfg env targets includes tree]
  let counts = sortOn (Down . snd) (Map.toList (Map.fromListWith (+) [(diagRule d, 1 :: Int) | (_, d) <- diags]))
  mapM_ (\(r, n) -> putStrLn (show n <> "\t" <> T.unpack r)) counts
  mapM_
    (\(f, d) -> putStrLn (f <> ":" <> show (posLine (spanStart (diagSpan d)) + 1) <> ": " <> T.unpack (diagRule d) <> ": " <> T.unpack (diagMessage d)))
    (take (if "all" `elem` flags then maxBound else 40) [x | x@(_, d) <- diags, diagRule d == T.pack rule])
