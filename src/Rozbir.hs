-- | the main entry point. it re-exports what most callers need: the syntax
-- tree, parsing, evaluation and queries.
--
-- > import Data.Text.IO qualified as T
-- > import Rozbir
-- >
-- > main :: IO ()
-- > main = do
-- >   tree <- parse <$> T.readFile "foo.spec"
-- >   let target = Target (Just "x86_64") (Just "linux") [("dist", Just ".fc44"), ("fedora", Just "44")]
-- >       spec = resolve (newEnv False) target tree
-- >   case evr spec of
-- >     Resolved v -> T.putStrLn (renderEVR v)
-- >     Unresolved reasons -> print reasons
-- >   print (versionInputs tree)
module Rozbir
  ( -- * Queries
    module Rozbir.Query,

    -- * Versions
    module Rozbir.Version,

    -- * Syntax tree
    module Rozbir.Syntax,

    -- * Parsing and rendering
    parse,
    render,

    -- * Evaluation
    resolve,
    resolveWith,
    ResolveOptions (..),
    defaultOptions,
    Spec,
    Target (..),
    unknownTarget,
    newEnv,
  )
where

import Rozbir.Eval (Target (..), newEnv, unknownTarget)
import Rozbir.Query
import Rozbir.Spec (ResolveOptions (..), Spec, defaultOptions, resolve, resolveWith)
import Rozbir.Syntax
import Rozbir.Syntax.Parse (parse)
import Rozbir.Syntax.Render (render)
import Rozbir.Version
