# CBC compiler tests

This directory contains the end-to-end test suites for the CBC compiler.
Unit tests (`sbt test`) are described in the main [README.md](../README.md).

## Suites

`runner.py` drives two suites. Both compile and execute test programs against
a Cangjie toolchain (SDK distribution):

- `standalone` - the main pipeline: `cjc --emit-chir` -> `cbc-compiler` ->
  `launcher`. Each test is compiled in `default` and `O2` modes.
- `hotfix` - the hotfix/patching pipeline: `cjc` with `libhotfix-plugin.so`
  produces a throw-stub binary (must abort with `Exception: should not reach
  here`), then a patched CHIR; `patch-gen` computes the patch CHIR, which
  `cbc-compiler` compiles into `diff<name>_patched.cbc`. At run time the stub
  binary is executed with the patch loaded via `libcbc-preload.so` and
  `CBCOPT=cbc.path=...`. Each test is compiled in `default` and `O2` modes;
  both the native stub and patched CHIR use the selected optimization level.

The `cjts/` directory holds the Cangjie LLT suite; it is run by CI with its
own wrappers (`cjts/cjc-wrapper`, `cjts/launcher-wrapper`) and per-suite
exclude lists (e.g. `cjts/LLT/hotfix_testlist.exclude.known.bugs`), not by
`runner.py`.

## Prerequisites

- A Cangjie toolchain containing `envsetup.sh`, specified via `--cangjie-home`
  or the `CANGJIE_HOME` environment variable. The runner sources `envsetup.sh`
  itself (`test/tool.sh`) before every command (`cjc`, `cbc-compiler`,
  `launcher`, `chir-dis`, `cbc-asm`), so there is no need to source it manually.
- The toolchain's `cbc-compiler` wrapper (`<toolchain>/tools/bin/cbc-compiler`)
  executes `cbc-compiler.jar` from its own directory. To run the tests against
  a locally built compiler, install it into the toolchain first (see the
  "Toolchain" section in the main [README.md](../README.md)).
- For the `hotfix` suite the toolchain must in addition contain the hotfix
  components: `tools/lib/libhotfix-plugin.so`, `tools/bin/patch-gen`,
  `tools/lib/libcbc-preload.so`, the interpreter engine libraries
  (`tools/lib/libcbcengine.so`) and `tools/lib/cbc-intrinsics.cbc`. The stdx
  library path defaults to `<toolchain>/stdx/dynamic/stdx` and can be
  overridden with the `CANGJIE_STDX_PATH` environment variable.

## Running

From the repository root:

```bash
$ python3 test/runner.py test                                # standalone suite
$ python3 test/runner.py test --mode hotfix                  # hotfix suite
$ python3 test/runner.py test --filter standalone/cj/bugs    # subset by name prefix
$ python3 test/runner.py clean                               # remove generated files
```

Options:

- `--mode standalone|hotfix` - suite to run (default: `standalone`)
- `--cangjie-home <path>` - toolchain path; overrides `CANGJIE_HOME`
- `--filter <prefix>...` - run only tests whose names (paths relative to
  `test/`, without extension, e.g. `standalone/cj/bugs/bug-205/main`) start
  with one of the given prefixes
- `--filter-file <cfg>` - run exactly the tests matched by a config file in
  the tests-list format (see below)
- `-j, --parallelism <n>` - number of tests built in parallel
  (default: cpu count / 4)
- `--jc-options '<opts>'` - extra options passed to `cbc-compiler`

## Test layout

A test under `standalone/cj/` is a directory with the main source
`<name>.cj` (package `default`) plus optional side files:

- `<name>.aot.cj` - AOT part: compiled by `cjc` into `lib<name>.so`
  (`--output-type=dylib`) in the test directory and passed to
  `cbc-compiler` via `-cbcaotdeps`; it is never compiled to CHIR/CBC itself.
- `<name>.int.cj` - intermediate package: compiled to a static archive and
  passed to `cbc-compiler` as an additional CHIR input.
- `<name>.c` - native library: built with `cc -shared -fPIC` and passed via
  `-foreignlibs`.
- `<name>.expected` - golden output: the program's stdout followed by its
  exit status. For the hotfix suite, a `<name>.hotfix.expected` file
  overrides it when present. A `<name>.hotfix.O2.expected` file can record
  optimization-dependent stack traces in hotfix O2; standalone supports
  `<name>.O2.expected` in the same way.

Files generated during a run are kept next to the sources in per-mode
subdirectories (`standalone/default/<name>/`, `standalone/O2/<name>/`,
`hotfix/default/<name>/`, `hotfix/O2/<name>/`), including `*.actual`
with the observed output for diffing, and are removed by
`python3 test/runner.py clean`.

Tests in `standalone/asm/` are written in CBC assembly (`*.asm`), assembled
with `cbc-asm` and executed by `launcher` directly.

## Package hotfix tests

The package tests under `test/hotfix` use a separate runner:

```bash
python3 test/hotfix/hotfix_tests.py test --cangjie-home <toolchain>
```

It runs every enabled test and patch variant in both O0 and O2. Use
`--opt-level O0` or `--opt-level O2` to select one level, and `--tests-only`
or `--tags-only` to select tests or variants. The runner selects the fixed O0/O2
options in `[profile.customized-option]` through `cjpm run --build-args=--O0`
or `--build-args=--O2`, including local dependencies. Copied manifests are not
rewritten.

Artifacts are retained under `<test>/temp/default/` and `<test>/temp/O2/`,
with separate `release/` and variant directories. `--save-temps <path>`
selects another artifact root; an absolute root gets one subdirectory per
test. Native output is saved as `native.actual`. Each variant's `.hotfix/`
directory contains its CHIR inputs, patch CBC, `patch.actual`, and
`patch.stderr`.

A failed mode does not stop the other mode or subsequent tests. Independent
tag variants are all awaited, and the runner exits with a failure status only
after attempting the selected tests. If a mode's release build fails, its patch
variants cannot run because their native baseline is unavailable.

## Test lists

`tests-list-standalone.cfg` and `tests-list-hotfix.cfg` select the tests of
each suite with glob patterns:

```text
include standalone/cj/**/*.cj
exclude standalone/**/*.aot.cj
exclude standalone/cj/lambda-method-ref-extend/main.cj    # excluded everywhere
exclude standalone/cj/some/test.cj O2                     # excluded in O2 mode only
```

An `exclude` without a trailing mode removes the test from the suite
completely; with a mode it skips only that compilation mode. The two lists
are independent: a test excluded from the standalone list may still run in
the hotfix suite and vice versa.
