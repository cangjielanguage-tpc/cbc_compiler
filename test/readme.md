# CBC tests

`runner.py` discovers pluggable test suites (`**/*_testsuite.py`), builds and
runs them against a Cangjie toolchain, and diffs program output against
`.expected` files.

Suites: 
- `cj` (cjc -> cbc-compiler -> launcher, `default` + `O2` modes);
- `asm` (CBC assembly via `cbc-asm`);
- `hotfix` (cj sources rebuilt as hotfix patches).

A new suite is a `<name>_testsuite.py` providing async `discover` / `build` /
`run`; after running a test the runner writes a `run.sh` into the test's
workdir which re-runs it via `python3 runner.py test --test <ID> --skip-build`.

## Running

From `test/`:

```bash
python3 runner.py test --cangjie-home ~/tmp/cangjie                # run everything
python3 runner.py test --cangjie-home ~/tmp/cangjie -j 8           # in parallel
python3 runner.py test ... --testsuite hotfix                      # one suite (repeatable)
python3 runner.py test ... --test cj::cast/cast::default           # specific test (repeatable)
python3 runner.py test ... --tests failed.txt                      # IDs from a file
python3 runner.py test ... --skip-build                            # reuse the built workdir
python3 runner.py test-discovery                                   # list all testcase IDs
python3 runner.py clean                                            # remove the workdir
```

`--cangjie-home` (or `CANGJIE_HOME`) must point to a toolchain containing
`envsetup.sh`; the runner sources it itself before every command.

## Results

- Excludes live in `excludes.cfg` (glob patterns over IDs, `#` comments);
  they are applied after discovery and skipped tests are reported.
- A failed test's stdout/stderr is dumped after the summary (`build.log` for
  build failures); `--no-dump-on-error` disables this.
- Failed IDs are written to `<work-dir>/failed.txt` — feed it back via
  `--tests` to re-run only failures.
- Per-test logs are in the workdir: `build.log`, `run.log`, `run.err`,
  `diff.log`. The generated `run.sh` re-runs the test through the runner and
  can be executed by hand; `--describe` prints its path.
- Exit codes: `0` all passed, `1` failures, `2` hard error.
