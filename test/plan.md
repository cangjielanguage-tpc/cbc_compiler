# test.py runner plan

New async test runner replacing `runner.py` (which stays untouched in this change).

## Driver: `test.py`

- Detects test suites by recursive glob `**/*_testsuite.py` under `test/` (workdir excluded),
  loads them via `importlib`, validates duck-typed contract.
- Subcommands:
  - `test` — build + run testcases.
  - `test-discovery` — print all testcase IDs of all suites, one per line, sorted.
  - `clean` — `rm -rf` the workdir.
- Options (on `test`):
  - `--test <id>` (repeatable) and `--tests FILE` (one ID per line, `#` comments, blank lines
    skipped). If any explicit IDs given, they replace the implicit per-suite `tests.cfg`
    filtering.
  - `--skip-build` — skip build, start running. Hard error (abort, nonzero exit) if a testcase's
    artifacts are missing (suite raises `MissingArtifacts`).
  - `--describe` — for each failed testcase print its `run_command` shell string + log file
    paths.
  - `-j/--parallelism N` — override autodetected limit.
  - `--work-dir DIR` — default `./workdir` (gitignored).
  - `--failed-list FILE` — default `<work-dir>/failed.txt`.
  - `--jc-options` — passed through to cj suite for cbc-compiler.jar.
  - `--cangjie-home PATH` — overrides `CANGJIE_HOME` env var; `envsetup.sh` presence checked.
    Driver sets `TOOLCHAIN` in os.environ (suites' `tool.sh` invocations inherit it).
- Parallelism: one asyncio task per testcase (`build` then `run`), whole task gated by a single
  `asyncio.Semaphore`. Default limit `max(1, len(os.sched_getaffinity(0)) // 4)`.
- End of run: print summary of successful/failed testcases, dump failed IDs (one per line) to
  failed-list file (format identical to `--tests FILE`), exit 0 if all pass else 1.
- Per-testcase dir: `<work-dir>/<id with "::" replaced by "/">`, e.g. `workdir/cj/foo/main/O2`.
  Driver writes `run.sh` (content of `run_command`) there.

## Suite contract (module-level, no base class)

Files: `standalone/asm/asm_testsuite.py`, `standalone/cj/cj_testsuite.py`.

- `async discover() -> list[str]`
  Testcase IDs, format `<suite>/<relpath>[::mode]`, e.g. `asm/simple`,
  `cj/arith-calls/main::default`, `cj/arith-calls/main::O2`.
  cj suites emit two IDs per test (`::default`, `::O2`). Applies the suite's `tests.cfg` excludes.
- `async build(test_id, workdir) -> int`
  Copies the test's source directory into `workdir` itself (`shutil.copytree`), builds,
  writes `build.log` in `workdir`. Returns 0 on success.
- `async run(test_id, workdir) -> int`
  Executes its own `run_command` directly (via `tool.sh`, `TOOLCHAIN` env inherited),
  writes `run.log` (stdout) and `run.err` (stderr) in `workdir`, builds `<name>.actual`
  (stdout, then exit-code line or stderr text appended — same composition rules as old runner),
  compares `.actual` with `.expected` in Python (no `diff` subprocess), writes `diff.log`
  (unified diff via `difflib`) on mismatch. Returns 0 pass / 1 fail.
  Raises `MissingArtifacts` (from `runner_tools.py`) when artifacts are absent and build was
  skipped.
- `async run_command(test_id, workdir) -> str`
  Shell string with proper `LD_LIBRARY_PATH` and run arguments (suite responsibility).

## `tests.cfg` (per suite folder, exclude-only)

One pattern per line, `#` comments allowed. Entries use the testcase ID format with glob
stars allowed. A pattern without `::` excludes the whole test (all modes); a pattern with
`::mode` excludes only that mode.

`standalone/cj/tests.cfg` (migrated from `tests-list-standalone.cfg`):
- `cj/extend/array/extend`
- `cj/multi-pkg/hashmap/main::O2`
- `cj/throwOOM/throw::O2`
- `cj/copy-generic/main`
- `cj/index-generic/main`

`standalone/asm/tests.cfg`: empty (comments only).

## Workdir layout

- `workdir/asm/<test>/` — flat, copied sources + build/run logs.
- `workdir/cj/<testdir>/<name>/` — one source copy per test, shared by modes, with `default/`
  and `O2/` subdirs (as old runner). Logs (`build.log`, `run.log`, `run.err`, `diff.log`,
  `run.sh`) live per mode inside the mode subdir.

## `.actual` / `.expected`

Whole source dir copied via `copytree`, so `.expected` travels with the test. `.actual`
composition unchanged from old runner (stdout + `res\n` or stderr text). Comparison in Python
(difflib unified diff into `diff.log`), no `diff` command.

## Out of scope

`cjts/`, `hotfix/` (incl. `hotfix_tests.py`). Toolchain resolution unchanged.
