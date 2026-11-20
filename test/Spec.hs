module Main (main) where

import EditSpec qualified
import EvalSpec qualified
import ExprSpec qualified
import FormatSpec qualified
import LintSpec qualified
import ParseSpec qualified
import QuerySpec qualified
import ShellSpec qualified
import Test.Hspec (hspec)
import TotalitySpec qualified
import VersionSpec qualified

main :: IO ()
main = hspec $ do
  ParseSpec.spec
  ExprSpec.spec
  EvalSpec.spec
  ShellSpec.spec
  QuerySpec.spec
  EditSpec.spec
  FormatSpec.spec
  LintSpec.spec
  VersionSpec.spec
  TotalitySpec.spec
