# CBC tests

`runner.py` builds and runs the registered test suites against a Cangjie
toolchain, and diffs program output against `.expected` files.

Suites (code in `suites/`, registered explicitly in `runner.py`; test
sources live under `standalone/`):
- `cj` (cjc -> cbc-compiler -> launcher, `default` + `O2` modes);
- `asm` (CBC assembly via `cbc-asm`);
- `hotfix` (cj sources rebuilt as hotfix patches, `default` + `O2` modes);
- `cjcbc` (cj sources with the std packages lowered from the toolchain
  CHIRs into the test's cbc instead of the AOT std library).

A new suite is a module in `suites/` defining a `TestSuite` subclass (from
`runner_lib`) that implements async `discover` / `build` / `run`, added to
`runner.py`'s `SUITES`; after running a test the runner writes a `run.sh`
into the test's workdir which re-runs it via
`python3 runner.py test --test <ID> --skip-build`.

Python 3.11 or newer is required to run the runner.

Native C companions use `clang`. Float16 C ABI tests require `_Float16`
support (Clang 15+ on x86_64). On x86_64, companions are compiled with `-mf16c`
and require F16C hardware support, matching the engine build.

## Running

From `test/`:

```bash
python3 runner.py test --cangjie-home /path/to/cangjie             # run everything
python3 runner.py test --cangjie-home /path/to/cangjie -j 8        # in parallel
python3 runner.py test ... --testsuite hotfix                      # one suite (repeatable)
python3 runner.py test ... --test cj::cast/cast::default           # specific test (repeatable)
python3 runner.py test ... --test cj::cast                         # ID prefix: every matching test (repeatable)
python3 runner.py test ... --tests failed.txt                      # IDs from a file
python3 runner.py test ... --skip-build                            # reuse the built workdir
python3 runner.py test-discovery                                   # list all testcase IDs
python3 runner.py clean                                            # remove the workdir
```

`--cangjie-home` (or `CANGJIE_HOME`) must point to a toolchain containing
`envsetup.sh`; the runner sources it itself before every command.

## Results

- `--test`/`--tests` entries are testcase ID prefixes: every discovered test
  whose ID starts with an entry runs (`--test cj::cast` selects all cast
  tests, all modes; a full ID like `cj::cast/cast::O2` stays exact). An entry
  matching nothing is an error.
- Excludes live in `excludes.cfg` (glob patterns over IDs, `#` comments);
  they are applied after discovery and skipped tests are reported.
- A failed test's stdout/stderr is dumped after the summary (`build.log` for
  build failures); `--no-dump-on-error` disables this.
- Failed IDs are written to `<work-dir>/failed.txt`; feed it back via
  `--tests` to re-run only failures.
- Per-test logs are in the workdir: `build.log`, `run.log`, `run.err`,
  `diff.log`. The generated `run.sh` re-runs the test through the runner and
  can be executed by hand; `--describe` prints its path.
- Exit codes: `0` all passed, `1` failures, `2` hard error.

The `cj` and `hotfix` suites omit `cangjie.runtime.cbcengine.*` stack frames
when comparing output. Expected files omit these implementation details;
raw logs and `.actual` files retain the complete trace.

Hotfix builds isolate sources and dependencies per mode. Both the native throw
stub and patched CHIR use the selected optimization level. Expected output is
selected in this order: `<name>.hotfix.<mode>.expected`,
`<name>.hotfix.expected`, `<name>.<mode>.expected`, `<name>.expected`.
The stdx library path defaults to `<toolchain>/stdx/dynamic/stdx` and can be
overridden with `CANGJIE_STDX_PATH`.

## Package hotfix tests

The package tests under `test/hotfix` use a separate runner:

```bash
python3 test/hotfix/hotfix_tests.py test --cangjie-home <toolchain>
```

It runs every enabled test and patch variant in both `default` and `O2` modes.
Use `--opt-level default` or `--opt-level O2` to select one mode, and `--tests-only`
or `--tags-only` to select tests or variants. The default mode runs `cjpm run`
without extra optimization options. O2 selects `[profile.customized-option].O2`
through `cjpm run --build-args=--O2`, including local dependencies. Copied
manifests are not rewritten.

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

Patch-gen declaration/diagnostic fixtures and their runner are described in
[patch-gen/README.md](patch-gen/README.md).
