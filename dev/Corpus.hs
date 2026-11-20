{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, unless)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Rozbir.Syntax
import Rozbir.Syntax.Parse (parse)
import Rozbir.Syntax.Render (render)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  files <-
    lines
      <$> ( getArgs >>= \case
              [list] -> readFile list
              _ -> getContents
          )
  results <- forM files $ \f -> do
    src <- decodeUtf8Lenient <$> BS.readFile f
    let tree = parse src
        roundTrips = render tree == src
        errs = treeErrors tree
    unless roundTrips (putStrLn ("ROUND-TRIP FAILED: " <> f))
    mapM_ (\e -> putStrLn (f <> ":" <> show (posLine (spanStart (seSpan e)) + 1) <> ": " <> T.unpack (seCode e) <> ": " <> T.unpack (seMessage e))) errs
    pure (roundTrips, null errs)
  let failed = length (filter (not . fst) results)
      withErrs = length (filter (not . snd) results)
  putStrLn (show (length results) <> " specs, " <> show failed <> " round-trip failures, " <> show withErrs <> " with syntax errors")
  unless (failed == 0) exitFailure
