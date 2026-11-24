# rozbir, an RPM spec library for Haskell

розбір is Ukrainian for "parsing."

rozbir parses, evaluates, edits and lints RPM spec files without running
anything: no `rpmspec`, no shell, no Lua. it was written for
[oglyad](https://github.com/terrapkg/oglyad), and
[korektor](https://github.com/terrapkg/korektor) builds a CLI and language
server on it.

```haskell
import Rozbir
import Rozbir.IO (readSpec)

main :: IO ()
main = do
  tree <- readSpec "foo.spec"
  let target = Target (Just "x86_64") (Just "linux") [("dist", Just ".fc44"), ("fedora", Just "44")]
  print (evr (resolve (newEnv False) target tree))
  print (versionInputs tree)
```

- `Rozbir.Syntax.Parse.parse` is total and lossless: `render (parse s) == s`
- `Rozbir.Spec.resolve` evaluates macros and conditionals. a small pure
  subset of shell (`echo`, `${c:0:7}`, `sed s///`, `tr`, `cut`) is
  interpreted, anything else that would run code comes back `Opaque`, and
  conditions it can't decide keep both branches. over terrapkg/packages and
  tuatara it agrees with `rpmspec` on every spec it can finish, which is 98%
  of them
- `Rozbir.Query` answers the usual questions: name, EVR, sources, patches,
  bconds, BuildRequires, and which macros feed `Version:`
- `Rozbir.Edit` sets tags and macros, bumps Release and adds changelog entries
  as minimal text edits
- `Rozbir.Lint` runs the core, rpmlint and (opt-in) terra rulesets. a comment
  like `# rozbir: ignore[hardcoded-path] reason` suppresses a finding on the
  next line, and `# rozbir: ignore-file[rule]` for the whole spec

## development

```sh
cabal build all --enable-tests
cabal test --test-show-details=direct
ormolu --mode inplace $(git ls-files '*.hs')
dev/differential.sh <(find ../packages -name '*.spec' -printf '%p\n' | xargs realpath)
```

## license

this project is licensed under GPL-3.0-or-later. see [LICENSE.md](LICENSE.md).
