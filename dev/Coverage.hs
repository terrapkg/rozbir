{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Rozbir.Eval (Piece (..), Target (..), Value (..), describeReason)
import Rozbir.IO (loadSystemMacros, readSpecText, resolveIncludes)
import Rozbir.Spec
import Rozbir.Syntax (Pos (..), Span (..))
import Rozbir.Syntax.Parse (parse)
import System.Environment (getArgs)

main :: IO ()
main = do
  [list] <- getArgs
  files <- lines <$> readFile list
  env <- loadSystemMacros (Just "x86_64")
  let target = Target (Just "x86_64") (Just "linux") [("dist", Just ".fc44")]
  found <- fmap concat $ forM files $ \f -> do
    src <- readSpecText f
    let tree = parse src
    includes <- resolveIncludes f tree
    let s = resolveWith (ResolveOptions env target includes) tree
        ls = T.lines src
    pure
      [ (describeReason r, T.strip (T.take 90 (ls !! posLine (spanStart sp))))
      | Just p <- [mainPackage s],
        key <- ["version", "release"],
        e <- take 1 (packageTags key p),
        let Value ps = teValue (locValue e),
        Opaque r (Just sp) <- take 1 [x | x@(Opaque _ _) <- ps]
      ]
  let byReason = sortOn (Down . snd) (Map.toList (Map.fromListWith (+) [(r, 1 :: Int) | (r, _) <- found]))
      bySrc = sortOn (Down . snd) (Map.toList (Map.fromListWith (+) [(T.unwords (take 4 (T.words l)), 1 :: Int) | (_, l) <- found]))
  mapM_ (\(r, n) -> TIO.putStrLn (T.pack (show n) <> "\t" <> r)) (take 15 byReason)
  putStrLn "---"
  mapM_ (\(r, n) -> TIO.putStrLn (T.pack (show n) <> "\t" <> r)) (take 40 bySrc)
