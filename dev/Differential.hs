{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, forM_)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Rozbir.Eval (Target (..), describeReason)
import Rozbir.IO (loadSystemMacros, readSpec, resolveIncludes)
import Rozbir.Query (Resolved (..), evr)
import Rozbir.Spec (ResolveOptions (..), resolveWith)
import Rozbir.Version (EVR (..))
import System.Environment (getArgs)

main :: IO ()
main = do
  [expected, dist] <- getArgs
  rows <- map (T.splitOn "\t") . T.lines <$> TIO.readFile expected
  env <- loadSystemMacros (Just "x86_64")
  let target = Target (Just "x86_64") (Just "linux") [("dist", Just (T.pack dist))]
  results <- forM [(f, t) | [f, t] <- rows] $ \(f, theirs) -> do
    tree <- readSpec (T.unpack f)
    includes <- resolveIncludes (T.unpack f) tree
    pure (f, evr (resolveWith (ResolveOptions env target includes) tree), theirs)
  let full (EVR e v r) = T.pack (show e) <> ":" <> v <> "-" <> r
      known = [(f, full e, t) | (f, Resolved e, t) <- results, t /= "FAILED"]
      unresolved = [(f, rs) | (f, Unresolved rs, t) <- results, t /= "FAILED"]
      mismatches = [x | x@(_, o, t) <- known, o /= t]
      reasonCounts = sortDesc (Map.toList (Map.fromListWith (+) [(describeReason r, 1 :: Int) | (_, r : _) <- unresolved]))
  forM_ mismatches $ \(f, o, t) -> TIO.putStrLn ("MISMATCH " <> f <> ": rozbir " <> o <> ", rpmspec " <> t)
  forM_ (take 25 reasonCounts) $ \(r, n) -> TIO.putStrLn (T.pack (show n) <> "\t" <> r)
  TIO.putStrLn $
    T.pack (show (length known))
      <> " known, "
      <> T.pack (show (length mismatches))
      <> " mismatches, "
      <> T.pack (show (length unresolved))
      <> " unresolved, of "
      <> T.pack (show (length [() | (_, _, t) <- results, t /= "FAILED"]))
      <> " specs rpmspec could read"
  where
    sortDesc = sortOn (Down . snd)
