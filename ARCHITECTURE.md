# rozbir architecture

status: milestones 1 to 6 are built in rozbir, 7 and 8 have a first version in
terralint. "as built" notes below mark where the code differs from the plan.

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

parsing is all hand-written, so there's no megaparsec. rpm's own scanners
(`matchchar`, `findMacroEnd`, `rdToken`) are simple enough that porting them
directly is closer to rpm than a grammar would be.

as built: rpm joins lines into one buffer for macro expansion, but splits the
expanded buffer back into lines before it looks for conditionals, tags or
section headers. so in the CST, an unbalanced `%{`, `%(` or `%[` always joins
lines, while a trailing `\` only continues `%global`/`%define`/`%undefine`/
`%bcond*` lines, conditionals and `%include`. a shell line ending in `\`
followed by `%if` stays two lines, which is what rpm does. an unclosed macro
at the end of the file becomes a one-line `unclosed-macro` error instead of
swallowing the rest of the file, which keeps the LSP usable while typing. specs are small, so the LSP reparses the whole file on every
change and there's no incremental parsing.

the syntax tracks rpm ≥ 4.20 and 6.0 (`%conf`, `-a`/`-p` section appends,
`BuildSystem:`/`BuildOption`), and still reads older forms like `%patchN`.
EL10 ships rpm 4.19.1, so "needs a newer rpm than the target" is a lint
rule, not a parse error.

### `Rozbir.Eval`: partial evaluation

```haskell
data Value = Value [Piece]
data Piece = Known Text | Opaque Reason (Maybe Span)
data Reason = RunsShell | RunsLua | ReadsEnv | ReadsFilesystem | HostDependent Text
            | Undefined Name | Ambiguous Name | DepthLimit | Unsupported Text | Failed Text

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
- as built, an `Env` is open or closed. a closed one (system macros loaded)
  leaves undefined macros as text, like rpm. an open one makes them
  `Opaque (Undefined n)`, except names nothing outside the spec can define:
  tag macros like `%version`, `%SOURCEn`, `with_*`/`_with_*`/`_without_*`,
  section keywords and argument macros. `%with`, `%bcond*`, `%defined`,
  `%undefined` and `%nil` come from a prelude copied from rpm's `macros.in`
- an `Unknown` `%if` runs every branch it might take from the same starting
  environment and merges the results. a macro the branches disagree on becomes
  `Ambiguous`, and one only some branches define is "maybe defined". the same
  happens for `%{?x:...}` when `x` itself is unknown, so
  `%{!?py3_pkgversion:%global py3_pkgversion 3}` comes out ambiguous instead of
  wrong
- as built, `%(...)` isn't always opaque. `Rozbir.Eval.Shell` interprets a
  small, pure subset of sh: assignments, `echo`, `printf %s`, `${v:o:l}`,
  `${v#p}`, `${v%p}`, `${v/p/r}`, `${#v}`, and pipes into `sed` s-commands,
  `tr` and `cut`, with a leftmost-longest regex engine for the sed subset.
  anything else (other commands, the environment, redirections, globs,
  command substitution, back-references) stays `Opaque RunsShell`. nothing is
  ever executed. here-strings (`sed ... <<< $v`) work too. this took the
  differential from 1980 to 2405 known specs, since
  `%(c=%{commit}; echo ${c:0:7})` alone is in 186 of them
- as built, `%{gsub}` uses `Rozbir.Eval.LuaPattern`, a port of Lua 5.4's
  pattern matcher and gsub loop (no captures, `%b` or `%f`), checked against
  `rpm --eval`. rozbir doesn't use `ghc-regex`: sed's BRE and Lua patterns
  both differ from POSIX ERE, and the small engines keep EPEL's dependency
  list short
- as built, free-field `%define` bodies drop the backslash before any escaped
  character, like rpm, so `\` and a line-ending `\` mean what they mean in
  `macros.cmake`. `%{lua:...}` blocks stay opaque, but names they
  `rpm.define` or assign through `macros.` become "maybe defined" instead of
  undefined. `%buildroot`, `%builddir`, `%buildsubdir` and `%_docdir` exist
  even without system macros, since rpmbuild defines them itself
- system macros (`%_bindir`, `%cargo_build`, `%forgemeta`, ...) come from
  rpm macro files. `Rozbir.MacroFile` parses them purely, and `Rozbir.IO`
  loads `/usr/lib/rpm/macros`, `macros.d/*` and any extra files. macros
  defined in Lua (Fedora's `%dist` is one) are opaque unless the `Target`
  overrides them, as oglyad's per-release macros already do

### `Rozbir.Spec`: the semantic model

```haskell
resolve :: Env -> Target -> Tree -> Spec
resolveWith :: ResolveOptions -> Tree -> Spec   -- also takes the %include trees
```

as built: messages from evaluation (bad expressions, `%{error:...}`) land in
`specMessages`, and lint turns them into diagnostics. like rpm, the model is
built from expanded lines, so `%pyproject_extras_subpkg` or a spec-local
template that expands to `%package`, tags and `%description` adds real
packages. a package whose preamble has a line that didn't expand is marked
`pkgOpaque`, so lint doesn't claim it's missing a Summary some macro adds.

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

as built:

| set     | examples                                                                                                                                                                                                                                                       |
| ------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| core    | syntax errors (`unclosed-if`, `stray-endif`, `stray-else`, `stray-elif`, `branch-after-else`, `unclosed-macro`, `trailing-backslash`, `not-a-tag`), `unknown-tag`, `duplicate-tag`, `missing-required-tag`, `missing-description`, `section-for-unknown-package`, `duplicate-source-number`, `macro-in-comment`, `needs-newer-rpm`, `patchn-obsolete`, `changelog-date`, `changelog-order`, `eval-error`, `line-outside-section`, `unused-ignore` |
| rpmlint | `deprecated-tag`, `clean-section`, `hardcoded-path`, `mixed-tabs-spaces`, `mixed-buildroot`, `rpm-buildroot-usage`, `macro-in-changelog`, `patch-not-applied`, `setup-not-quiet`, `summary-ended-with-dot`, `summary-too-long`, `description-line-too-long`, `make-check-outside-check-section`, `use-of-RPM_SOURCE_DIR` |
| terra   | opt-in: `terra-packager`, `terra-dist-release`, `terra-define-global`, `terra-version-editable`. the open question about which conventions belong here still stands |

## module list

```
Rozbir                 re-exports Query, Version, the parse/render entry points
Rozbir.Syntax          CST types
Rozbir.Syntax.Parse    total parser
Rozbir.Syntax.Render
Rozbir.Syntax.Expr     %if and %[...] expressions, three-valued
Rozbir.MacroFile       rpm macro file parser
Rozbir.Eval
Rozbir.Eval.Types      Value, Env, Target
Rozbir.Eval.Builtin
Rozbir.Changelog
Rozbir.Spec            semantic model + resolve
Rozbir.Query
Rozbir.Version
Rozbir.Edit
Rozbir.Format          tag alignment and whitespace, as one edit
Rozbir.Eval.Shell      the pure sh/sed/tr/cut subset
Rozbir.Eval.LuaPattern Lua 5.4 patterns for %{gsub}
Rozbir.Lint            runLint, suppression directives
Rozbir.Lint.Types      Rule, Diagnostic, Fix, LintConfig
Rozbir.Lint.Core
Rozbir.Lint.Rpmlint
Rozbir.Lint.Terra
Rozbir.IO              readSpec, resolveIncludes, loadSystemMacros
```

dependencies: `base`, `text`, `bytestring`, `containers`, `mtl`, `deepseq`,
`time` for changelog dates, `directory` and `filepath` for `Rozbir.IO`, and
`aeson` for `ToJSON` on diagnostics, which both terralint and oglyad print.
no `process`, so nothing in rozbir can run a command.

## testing

- **round-trip**: QuickCheck over generated spec-ish text and arbitrary
  text, `render . parse == id`
- **corpus**: every spec in terrapkg/packages, packages-el,
  Ultramarine-Linux/packages and tuatara has to round-trip and resolve
  without `Bogus` nodes. this is a dev script (`dev/Corpus.hs`), not CI,
  since it needs the checkouts. as of 2026-11-20 all 2462 specs in packages
  and tuatara round-trip with no syntax errors
- **differential**: `dev/differential.sh` compares `evr` with `rpmspec -q`
  over the same corpus. every `Known` answer has to match, and the share
  that's `Known` is the coverage number to push up. also dev-only, since it
  needs rpm-build. as of 2026-11-20, with system macros loaded: 2405 of 2461
  known, 0 mismatches. the rest run Lua (48, mostly `%autorelease`) or real
  commands like `date` (8)
- **totality**: QuickCheck runs parse, resolve, every lint rule, every fix and
  every edit over generated and arbitrary text. 60k cases pass without a crash
- **format corpus**: `dev/FormatCorpus.hs` checks the formatter is idempotent
  and leaves every evaluated tag unchanged over the whole corpus
- **lint corpus**: `dev/LintCorpus.hs` counts findings per rule over a
  checkout, which is how false positives get found
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
- as built: `terralint.toml` takes `rulesets`, `fail-on`, `system-macros`,
  `targets`, a `[rules]` table (`"off"` or a severity) and `[target.NAME]`
  tables with `arch` and `macros` (`false` undefines). `fNN`, `elNN` and
  `rawhide` are built-in targets with an optional `:arch`. with no targets it
  lints one unknown target, which answers the last open question below the
  way this doc recommended. `--fix` applies safe fixes, and `terralint rules`
  lists every rule. the LSP also offers "ignore this rule on this line" as a
  quick fix. with `ghc-lsp-types` and `ghc-regex` now in Terra, `ghc-lsp`,
  `ghc-co-log-core`, `ghc-sorted-list` and `ghc-text-rope` are the only
  missing packages, and their cabal-rpm specs are in
  `terralint/packaging/terra`
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

## formatting

as built: `Rozbir.Format` does only what can't change meaning. it aligns tag
values to the column most of the spec's tags already use (and does nothing
when there's no clear majority or the tags use tabs), strips trailing
whitespace except after a continuation backslash, squashes runs of more than
two blank lines, and ends the file with one newline. over the corpus it
changes 545 of 2468 specs, is idempotent, and leaves every evaluated tag the
same. terralint exposes it as `terralint fmt` and LSP formatting.

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
