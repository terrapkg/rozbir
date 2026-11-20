{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, unless)
import Rozbir.Eval (Target (..), newEnv, showValue)
import Rozbir.Format (defaultFormatOptions, formatText)
import Rozbir.IO (readSpecText)
import Rozbir.Spec
import Rozbir.Syntax.Parse (parse)
import System.Environment (getArgs)

main :: IO ()
main = do
  [list] <- getArgs
  files <- lines <$> readFile list
  results <- forM files $ \f -> do
    src <- readSpecText f
    let out = formatText defaultFormatOptions src
        again = formatText defaultFormatOptions out
        target = Target (Just "x86_64") (Just "linux") [("dist", Just ".fc44"), ("fedora", Just "44")]
        summary s = [(map (showValue . teValue . locValue) (pkgTags p)) | p <- specPackages (resolve (newEnv False) target (parse s))]
        sameMeaning = summary src == summary out
    unless (again == out) (putStrLn ("NOT IDEMPOTENT " <> f))
    unless sameMeaning (putStrLn ("CHANGED MEANING " <> f))
    pure (out /= src, again == out && sameMeaning)
  putStrLn (show (length (filter fst results)) <> " of " <> show (length results) <> " specs would change, " <> show (length (filter (not . snd) results)) <> " problems")
