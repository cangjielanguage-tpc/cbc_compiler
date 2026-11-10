# CBC tests — `test.py` runner

`test.py` is the async test runner. It discovers and drives pluggable test
suites and compiles + executes test programs against a Cangjie toolchain
(SDK distribution).

## Suites

Suites are discovered by recursive glob `**/*_testsuite.py` under `test/`
(the `workdir/` directory is ignored), loaded via `importlib`, and validated
against a duck-typed contract. Each suite module provides four async
functions:

- `async discover() -> list[str]` — testcase IDs.
- `async build(test_id, workdir) -> int` — copy the test's sources into
  `workdir`, build, write `build.log`. Returns `0` on success.
- `async run(test_id, workdir) -> int` — execute `run_command`, write
  `run.log`/`run.err`, compose `<name>.actual`, diff against `.expected`,
  write `diff.log` on mismatch. Returns `0` pass / `1` fail.
- `async run_command(test_id, workdir) -> str` — the shell string to run the
  test (suite responsibility, includes `LD_LIBRARY_PATH`).

Current suites:

- `standalone/cj/cj_testsuite.py` — the main pipeline: `cjc --emit-chir` ->
  `cbc-compiler` -> `launcher`. Each test runs in `default` and `O2` modes.
- `standalone/asm/asm_testsuite.py` — CBC assembly (`*.asm`), assembled with
  `cbc-asm` and executed by `launcher`.
- `hotfix/hotfix_testsuite.py` — the cj sources rebuilt as hotfix patches:
  throw-stub build (hotfix plugin), patched CHIR, `patch-gen`, and a final
  `cbc-compiler` step; single `default` mode. The run executes the stub
  binary with the patch loaded via `CBCOPT` and `LD_PRELOAD`.

`cjts/` is out of scope.

## Prerequisites

- A Cangjie toolchain containing `envsetup.sh`, specified via
  `--cangjie-home` or the `CANGJIE_HOME` environment variable. The runner
  sources `envsetup.sh` itself (`test/tool.sh`) before every command, so there
  is no need to source it manually.
- The toolchain's `cbc-compiler` wrapper (`<toolchain>/tools/bin/cbc-compiler`)
  executes the compiler from its own directory. To run the tests against a
  locally built compiler, install it into the toolchain first (see the
  "Toolchain" section in the main [README.md](../README.md)).
- The stdx library path defaults to `<toolchain>/stdx/dynamic/stdx` and can be
  overridden with the `CANGJIE_STDX_PATH` environment variable.

## Testcase IDs

IDs have the form `<suite>/<relpath>[::mode]`, where `relpath` is the path of
the test source relative to the suite directory, without extension:

- `cj/cast/cast::default`, `cj/cast/cast::O2` — the cj suite emits two IDs per
  test (one per mode).
- `asm/simple` — a suite may emit a single ID (no mode suffix).
- `hotfix/cast/cast::default` — the hotfix suite reuses the cj sources under
  its own namespace; only `default` mode.

## Running

From the `test/` directory:

```bash
$ python3 test.py test-discovery                       # list all testcase IDs
$ python3 test.py test --cangjie-home ~/tmp/cangjie     # run everything
$ python3 test.py test --cangjie-home ~/tmp/cangjie \
    --test cj/cast/cast::default --test cj/foreign-gc/main::O2   # specific tests
$ python3 test.py test --cangjie-home ~/tmp/cangjie --tests /tmp/smoke-ids.txt
$ python3 test.py test --cangjie-home ~/tmp/cangjie --testsuite hotfix
$ python3 test.py clean                                 # remove the workdir
```

### `test` options

- `--cangjie-home <path>` — toolchain path; overrides `CANGJIE_HOME`. Presence
  of `envsetup.sh` is checked.
- `--test <id>` — testcase ID to run (repeatable).
- `--tests <file>` — file with testcase IDs, one per line (`#` comments and
  blank lines are skipped).
- `--testsuite <name>` — suite to run (repeatable); when given, only the
  named suites are discovered and run.
- If any explicit IDs are given (`--test`/`--tests`), they replace the implicit
  per-suite `tests.cfg` filtering; only those IDs are run.
- `--skip-build` — skip the build and start running. Aborts with a nonzero exit
  if a testcase's artifacts are missing.
- `--describe` — for each failed testcase, print its `run_command` shell
  string and its log file paths.
- `-j, --parallelism <n>` — number of testcases processed in parallel
  (default: `cpus // 4`).
- `--work-dir <dir>` — work directory (default `./workdir`, gitignored).
- `--failed-list <file>` — file to dump failed testcase IDs to (default
  `<work-dir>/failed.txt`).
- `--jc-options '<opts>'` — extra options passed to `cbc-compiler`.

### `test-discovery` and `clean`

- `test-discovery` prints every discovered testcase ID, one per line, sorted.
- `clean` removes the work directory (`rm -rf`). Both accept `--work-dir`.

## Workdir layout

The workdir is `<work-dir>/<id with "::" replaced by "/">`. For the cj suite
that is `<work-dir>/cj/<testdir>/<name>/` with one shared source copy per test
and `default/` / `O2/` subdirectories per mode. For the asm suite it is
`<work-dir>/asm/<test>/`. The hotfix suite mirrors the cj layout under
`<work-dir>/hotfix/<testdir>/<name>/default/`.

Per testcase (per mode for cj), the runner writes:

- `build.log` — compiler output and per-step exit codes.
- `run.log` / `run.err` — the test's stdout / stderr.
- `<name>.actual` — observed output (stdout, then the exit-code line, or
  stderr text on a crash) for diffing.
- `diff.log` — unified diff of `.actual` vs `.expected` on mismatch.
- `run.sh` — a standalone script containing the `run_command`, to re-run the
  test by hand.

## Result & exit code

At the end of a run the runner prints a `[Summary] N passed, M failed` line,
lists each failed testcase, and writes the failed IDs (one per line, same
format as `--tests <file>`) to the failed-list file.

- exit `0` — all testcases passed.
- exit `1` — one or more testcases failed.
- exit `2` — hard error (e.g. missing artifacts under `--skip-build`).

## `tests.cfg` (per suite folder, exclude-only)

Each suite directory may contain a `tests.cfg` with one glob pattern per line
(`#` comments allowed). Patterns use the testcase ID format. A pattern without
`::` excludes the whole test (all modes); a pattern with `::mode` excludes only
that mode.

`standalone/cj/tests.cfg`, `standalone/asm/tests.cfg` and `hotfix/tests.cfg`
hold the current excludes.

## Typical workflow

```bash
$ python3 test.py test-discovery | grep cj/cast      # find IDs
$ python3 test.py test --cangjie-home ~/tmp/cangjie --test cj/cast/cast::default
$ python3 test.py test --cangjie-home ~/tmp/cangjie --test cj/cast/cast::default --describe
$ python3 test.py clean
```
