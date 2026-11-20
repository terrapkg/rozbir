# rozbir architecture

status: draft, nothing here is built yet.

rozbir is a pure Haskell library that parses, evaluates, edits and lints RPM
spec files. it never runs anything: no `rpmspec`, no shell, no Lua. the CLI,
language server and editor plugins live in [terralint](../terralint) and
depend on rozbir like any other consumer, oglyad included.

## goals

- **lossless**: `render (parse s) == s` for every input, including broken
  specs. the parser is total, and anything it doesn't understand becomes a
  `Bogus` node with a diagnostic, never a parse failure
- **sandboxed partial evaluation**: expand `%global`, `%define`, `%if`,
  bconds and rpm's builtins natively. `%(...)`, `%{lua:...}`, `%{getenv:...}`
  and friends come back `Opaque`, so a caller like oglyad falls back to
  `rpmspec` only for the specs that actually need it
- **minimal-diff edits**: set Version, a `%global`, bump Release or add a
  changelog entry while keeping the spec's own formatting
- **lint every branch**: conditions that depend on unknown macros (`%fedora`
  with no target set) are three-valued, so rules see both sides of an `%if`.
  rpmlint only sees the branch one build took
- **match oglyad's packaging**: GHC2021, same warnings and extensions, lower
  bounds at what EPEL 10 ships, only deps Fedora packages. it ships as
  `ghc-rozbir` in Terra

## non-goals

- replacing `rpmspec` completely. no Lua interpreter, no shell
- building or reading binary rpms ([codec-rpm](https://hackage.haskell.org/package/codec-rpm) does that)
- formatting (maybe later, through `Edit`)

## layers

```
              ┌─────────────────────────── pure ───────────────────────────────┐
 Text ──► Syntax.Lex/Parse ──► CST ──► render ≡ id
                                 │
                                 ├──► Eval (Env, Target) ──► Spec model (per target)
                                 │       Value = Known | Opaque Reason Span   │
                                 │                                            ├──► Query
                                 ├──► Edit ◄──────────────────────────────────┤
                                 │     [TextEdit], reparse after             └──► Lint ──► [Diagnostic]
                                 │                                                  (fixes are Edits)
 Version (EVR, rpmvercmp)        │
              └──────────────────┴─────────────────────────────────────────────┘
 Rozbir.IO: read a spec, resolve %include, load macro files. the only IO module
```

### `Rozbir.Syntax`: the CST

rpm parses line by line, so the CST does too. the source of truth is rpm's
[`build/parseSpec.c`](https://github.com/rpm-software-management/rpm/blob/master/build/parseSpec.c)
and [`rpmio/macro.c`](https://github.com/rpm-software-management/rpm/blob/master/rpmio/macro.c).

```haskell
data Span = Span {start :: !Pos, end :: !Pos}  -- offset, line, col

data Line = Line {body :: LineBody, eol :: Text, span :: Span}

data LineBody
  = Blank Text
  | Comment Text [Frag]          -- rpm still expands macros in comments, so keep them
  | Tag TagLine                  -- Name:, Source0:, Requires(post):, BuildSystem:
  | Define DefineLine            -- %global, %define, %undefine, %bcond*, continuations
  | Cond CondLine                -- %if/%ifarch/%ifos/%elif*/%else/%endif
  | Header SectionHeader         -- %package -n foo, %files -f x, %prep -a, %conf ...
  | Include [Frag]
  | Body [Frag]                  -- shell, %files entries, description text
  | Bogus Text Diagnostic

data Frag = Lit Text | Escape | Macro MacroRef  -- Escape is %%

data MacroRef
  = Plain Name                                   -- %name
  | Braced Test Name (Maybe [Frag]) (Maybe [Frag]) -- %{?name:...}, %{!?name}, %{name:arg}
  | Shell [Frag]                                 -- %(...)
  | Expr [Frag]                                  -- %[...]
  | Lua Text
  | Param ParamRef                               -- %1, %*, %#, %{-f}, %{-f*}
```

on top of the lines, a tree only adds the nesting that is actually in the
text:

```haskell
data Node = NLine Line | NCond CondBlock | NSection SectionHeader [Node]
data CondBlock = CondBlock {branches :: [(CondLine, [Node])], endif :: Maybe Line}
```

an `%if` can wrap whole sections (`%if %{with docs}` around `%package doc`,
its `%description` and `%files`), so `NCond` nests above `NSection` as well as
inside it. an unclosed `%if` is a `CondBlock` with `endif = Nothing` plus a
diagnostic, not an error.

parsing needs megaparsec for the inline macro grammar and `%if` expressions.
line splitting and continuations (a trailing `\` in a macro body, or an
unbalanced `%{` or `%(` that rpm keeps reading past) are handled by hand
before that. specs are small, so the LSP reparses the whole file on every
change and there's no incremental parsing.

the syntax tracks rpm ≥ 4.20 and 6.0 (`%conf`, `-a`/`-p` section appends,
`BuildSystem:`/`BuildOption`), and still reads older forms like `%patchN`.
EL10 ships rpm 4.19.1, so "needs a newer rpm than the target" is a lint
rule, not a parse error.

### `Rozbir.Eval`: partial evaluation

```haskell
data Value = Value [Piece]
data Piece = Known Text | Opaque Reason Span
data Reason = RunsShell | RunsLua | ReadsEnv | Undefined Name | DepthLimit | ...

data Target = Target
  { arch :: Maybe Text
  , macros :: [(Name, Maybe Text)]  -- same shape as oglyad's cfgMacros. Nothing undefines
  }

data Env      -- macro stack per name, since %define pushes and %undefine pops
              -- also parametric macros with getopt-style options, and bconds
```

- the `%if` grammar is rpm's: ints, strings, `v"1.2"` versions, `! && ||`,
  comparisons, arithmetic, `?:`. it evaluates to `Known Bool` or `Unknown`
- builtins follow rpm's `builtinmacros[]`, for example `expand`,
  `shrink`, `basename`, `dirname`, `suffix`, `upper`, `lower`, `len`,
  `quote`, `defined`, `undefined`, `with`, `without`, `macrobody`, `exists`
  (opaque, it touches the filesystem) and `getenv` (opaque)
- expansion is depth-limited like rpm's, and with fuel, so a recursive macro
  ends with `Opaque DepthLimit` instead of hanging
- system macros (`%_bindir`, `%cargo_build`, `%forgemeta`, ...) come from
  rpm macro files. `Rozbir.MacroFile` parses them purely, and `Rozbir.IO`
  loads `/usr/lib/rpm/macros`, `macros.d/*` and any extra files. macros
  defined in Lua (Fedora's `%dist` is one) are opaque unless the `Target`
  overrides them, as oglyad's per-release macros already do

### `Rozbir.Spec`: the semantic model

```haskell
resolve :: Env -> Target -> Tree -> (Spec, [Diagnostic])
```

`Spec` has the main package and its subpackages (tags, `%description`,
`%files`), build sections, scriptlets and triggers, numbered sources and
patches, and parsed `%changelog` entries. every field keeps the `Span` it
came from, so lint and edits can point back into the CST. an `Unknown`
condition keeps both branches and tags them with that condition, so the
model covers everything the spec could be for that target.

### `Rozbir.Query`

the small, stable API oglyad and terralint use:

- `name`, `evr :: Spec -> Resolved EVR`, with `Resolved` being either fully
  `Known` or the reasons it isn't
- `versionInputs`: which macros feed `Version:`, following `%global`
  chains. this replaces oglyad's text scan in `versionMacros`
- `subpackages`, `sources`, `patches`, `bconds`, `buildRequires`

### `Rozbir.Version`

`EVR`, `parseEVR` and the `rpmvercmp` port, moved from `Oglyad.Version`.
oglyad's looser comparison for respelled upstream tags stays in oglyad.

### `Rozbir.Edit`

```haskell
data TextEdit = TextEdit Span Text   -- maps 1:1 onto LSP TextEdit

setTag      :: TagName -> Text -> Tree -> Either EditError [TextEdit]
setMacro    :: Name -> Text -> Tree -> Either EditError [TextEdit]
bumpRelease :: Tree -> Either EditError [TextEdit]
addChangelog :: Entry -> Tree -> Either EditError [TextEdit]
apply       :: [TextEdit] -> Text -> Text
```

- `setTag Version` follows `Version: %{ver}` to the `%global ver` line, so
  the edit lands where the value lives. oglyad's `RhaiEval` can use this to
  model `rpm.version` and `rpm.global`
- the edit keeps `%global` vs `%define`, the alignment between the tag and
  its value, and the line ending
- a value defined in several `%if` branches is `Left Ambiguous`, never a
  guess

### `Rozbir.Lint`

```haskell
data RuleSet = Core | Rpmlint | Terra   -- Terra is opt-in

data Rule = Rule
  { ruleId :: RuleId          -- kebab-case, like oglyad's checks
  , ruleSet :: RuleSet
  , severity :: Severity      -- Info | Warn | Error, the default
  , doc :: Text
  , check :: LintInput -> [Diagnostic]
  }

data LintInput = LintInput {tree :: Tree, specs :: [(Target, Spec)]}

data Diagnostic = Diagnostic
  { rule :: RuleId, severity :: Severity, span :: Span
  , message :: Text, fixes :: [Fix] }

data Fix = Fix {title :: Text, edits :: [TextEdit], safe :: Bool}

data LintConfig = LintConfig
  { ruleSets :: Set RuleSet, overrides :: Map RuleId (Maybe Severity) }  -- Nothing disables it
```

rules are pure, like oglyad's checks. linting against several targets merges
findings the way oglyad merges releases.

**suppression in the spec** is the one config source rozbir parses itself,
because it's spec syntax:

```spec
# rozbir: ignore[hardcoded-path] upstream's Makefile insists
%{__install} -m755 foo /usr/bin/foo
# rozbir: ignore-file[macro-in-changelog]
```

an `ignore` covers its own line or the next one. a directive that no longer
suppresses anything is itself reported, like oglyad's `unused-ignore`.

the config file and `anda.hcl` labels are read by terralint, which turns
them into a `LintConfig`, since finding them is IO.

first rules, to be refined:

| set     | examples                                                                                                                                                                                                                                                       |
| ------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| core    | `unclosed-if`, `stray-endif`, `unknown-section`, `duplicate-tag`, `missing-required-tag`, `files-for-unknown-package`, `subpackage-without-description`, `duplicate-source-number`, `macro-in-comment`, `needs-newer-rpm`, `changelog-date`, `changelog-order` |
| rpmlint | parity with [rpmlint's spec checks](https://github.com/rpm-software-management/rpmlint/blob/main/rpmlint/checks/SpecCheck.py): deprecated tags (`BuildRoot`, `%clean`, `Group`, `PreReq`), `hardcoded-path`, mixed `$RPM_BUILD_ROOT`/`%{buildroot}`, `macro-in-changelog`, `mixed-tabs-spaces`, `patch-not-applied`, `%patchN` |
| terra   | opt-in, not decided yet. candidates: Version macros that update.rhai can edit, `%global submodule_*` shape, `%{dist}` in Release                                                                                                                              |

## module list

```
Rozbir                 re-exports Query, Version, the parse/render entry points
Rozbir.Syntax          CST types
Rozbir.Syntax.Parse    total parser
Rozbir.Syntax.Render
Rozbir.Syntax.Expr     %if expression grammar
Rozbir.MacroFile       rpm macro file parser
Rozbir.Eval
Rozbir.Eval.Builtin
Rozbir.Spec            semantic model + resolve
Rozbir.Query
Rozbir.Version
Rozbir.Edit
Rozbir.Lint            Rule, Diagnostic, LintConfig, runLint, suppression directives
Rozbir.Lint.Core
Rozbir.Lint.Rpmlint
Rozbir.Lint.Terra
Rozbir.IO              readSpec, resolveIncludes, loadSystemMacros
```

dependencies: `base`, `text`, `containers`, `megaparsec`, `deepseq`, and
`aeson` for `ToJSON` on diagnostics, which both terralint and oglyad print.
no `process`, so nothing in rozbir can run a command.

## testing

- **round-trip**: QuickCheck over generated spec-ish text and arbitrary
  text, `render . parse == id`
- **corpus**: every spec in terrapkg/packages, packages-el,
  Ultramarine-Linux/packages and tuatara has to round-trip and resolve
  without `Bogus` nodes. this is a dev script, not CI, since it needs the
  checkouts
- **differential**: compare `evr` with `rpmspec -q` over the same corpus.
  every `Known` answer has to match, and the share that's `Known` is the
  coverage number to push up. also dev-only, since it needs rpm-build
- **rules**: one small spec per rule showing what it catches and what it
  lets through, plus each fix applied and reparsed

the suite uses hspec and QuickCheck, like oglyad's.

## terralint (separate repo)

```
terralint lint [paths] [--ruleset terra] [--target f44] [--format text|json|sarif|github]
terralint lsp          # stdio
```

- **config**, from weakest to strongest: defaults, then `terralint.toml` at
  the repo root (found by walking up, parsed with `toml-parser`, which Fedora
  ships), then `anda.hcl` labels in the spec's dir
  (`terralint_ignore = "rule, rule"`, consistent with `oglyad_ignore`), then
  the in-spec directives, then CLI flags
- **exit codes** like oglyad's: 0 clean, 1 findings at or above `--fail-on`,
  2 couldn't run
- **LSP** is built on [`lsp`](https://hackage.haskell.org/package/lsp) 2.8 and
  [`lsp-types`](https://hackage.haskell.org/package/lsp-types) 2.4, which
  Fedora doesn't ship, so `ghc-lsp`, `ghc-lsp-types` and whatever else they
  pull in that Fedora lacks go into terrapkg/packages. features:
  diagnostics, code actions from `Fix`, hover that shows what a macro
  expands to per target, go to definition for macros, document symbols for
  sections and subpackages, and completion for tags, sections and macros.
  semantic tokens come from the CST
- **Zed**: our own wasm extension that launches `terralint lsp`, with
  highlighting from [cryptomilk/tree-sitter-rpmspec](https://gitlab.com/cryptomilk/tree-sitter-rpmspec).
  the existing [rpmspec extension](https://zed.dev/extensions/rpmspec)
  ([source](https://github.com/mtorromeo/zed-rpmspec-language)) wraps a
  different server and hasn't been updated since November 2024, so we don't
  build on it
- **JetBrains**: a Kotlin plugin on the platform's own
  [LSP API](https://plugins.jetbrains.com/docs/intellij/language-server-protocol.html),
  which [all unified IntelliJ-based IDEs have for free since 2025.2](https://blog.jetbrains.com/platform/2025/09/the-lsp-api-is-now-available-to-all-intellij-idea-users-and-plugin-developers/),
  with highlighting from semantic tokens. [LSP4IJ](https://plugins.jetbrains.com/plugin/23257-lsp4ij)
  covers the open-source builds if anyone asks

## oglyad integration

1. swap `Oglyad.Version` for `Rozbir.Version`
2. swap `versionMacros` for `Rozbir.Query.versionInputs`
3. shadow mode: a dev flag runs rozbir's `evr` next to `querySpec` and
   reports mismatches over a whole release
4. once it matches, `querySpec` asks rozbir first and only runs `rpmspec`
   when the answer isn't `Known`. that skips most of the cold-run cost and
   most of the "running a checkout's code" exposure
5. `RhaiEval` models `rpm.version` and `rpm.global` with `Rozbir.Edit`

oglyad stays deliberately library-less. rozbir is an ordinary external
package, so oglyad's spec just gains `BuildRequires: ghc-rozbir-devel`, and
packages-el needs it too for EL10.

## milestones

1. scaffolding: cabal layout matching oglyad, CI, ormolu
2. CST, parser and render, plus round-trip and corpus tests
3. `Version` and `versionInputs`, which oglyad adopts first
4. `Eval`, `Spec` and `Query`, plus the differential test and oglyad's shadow mode
5. `Edit`
6. lint core, rpmlint rules and directives
7. terralint CLI and config
8. `ghc-lsp` packaging, the LSP server, and the Zed and JetBrains plugins
9. the Terra ruleset

## open questions

- the name. terralint is generic and the Terra rules are opt-in
- which Terra conventions go in the terra ruleset
- should oglyad grow a `spec-lint` check that runs the core and terra
  rulesets in the nightly report, or leave linting to terralint
- should hover and diagnostics default to every maintained release as a
  target (like oglyad), or to one "unknown" target with three-valued `%if`
