# Test suite notes for agents

Agent-oriented description of the end-to-end tests in this directory: what
each suite actually runs, what a test consists of, and where the artifacts
land. The user-facing overview (running, options, results) lives in
[README.md](README.md); unit tests (`sbt test`) are described in the main
[README.md](../README.md).

## Suites and pipelines

`runner.py` discovers every `*_testsuite.py` under `test/` and drives the
`TestSuite` subclasses they define. All suites compile and execute test
programs against a Cangjie toolchain (SDK distribution):

- `cj` (`standalone/cj/`, `cj_testsuite.py`) - the main pipeline:
  `cjc --emit-chir` -> `cbc-compiler` -> `launcher`. Each test is compiled
  and run in `default` and `O2` modes.
- `asm` (`standalone/asm/`, `asm_testsuite.py`) - tests written directly in
  CBC assembly (`*.asm`): the optional AOT part is compiled by `cjc` into a
  dylib, `cbc-asm` assembles the assembly, and `launcher` executes it.
- `hotfix` (`standalone/cj/hotfix_testsuite.py`) - reuses the `cj` sources
  for the hotfix/patching pipeline: `cjc` with `libhotfix-plugin.so` produces
  a throw-stub binary (it must abort with `Exception: should not reach
  here`), then a patched CHIR; `chir-dis` disassembles CHIRs, `patch-gen`
  computes the patch CHIR, and `cbc-compiler` compiles it into
  `diff<name>_patched.cbc`. At run time the stub binary is executed with the
  patch loaded via `libcbc-preload.so` and `CBCOPT=cbc.path=...`. Tests run
  in `default` mode only.

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

## Test layout

A test under `standalone/cj/` is a directory with the main source
`<name>.cj` (package `default`) plus optional side files:

- `<name>.aot.cj` - AOT part: compiled by `cjc` into `lib<name>.so`
  (`--output-type=dylib`; `.dylib` on macOS) in the test directory and passed
  to `cbc-compiler` via `-cbcaotdeps`; it is never compiled to CHIR/CBC itself.
- `<name>.int.cj` - intermediate package: compiled to a static archive and
  passed to `cbc-compiler` as an additional CHIR input.
- `<name>.c` - native library: built with `cc -shared -fPIC` and passed via
  `-foreignlibs`.
- `<name>.expected` - golden output: the program's stdout followed by its
  exit status. For the hotfix suite, a `<name>.hotfix.expected` file
  overrides it when present.

Tests in `standalone/asm/` are a `<name>.asm` file plus the same optional
`<name>.aot.cj` and `<name>.expected` files.

Adding a test is just creating such a directory: discovery picks it up
automatically, there is no list to register it in.

## Workdir layout

All generated files live under `test/workdir/<suite>/<test>/<mode>/`
(override with `--work-dir`); the suite copies the test's sources into the
per-mode directory before building, so modes (e.g. `default` and `O2`) never
share files and build and run fully concurrently. Per test you will find
`*.actual` with the observed output for diffing, the logs `build.log`,
`run.log`, `run.err` and `diff.log`, and a generated `run.sh` that re-runs
this single test through the runner.
`python3 test/runner.py clean` removes the whole workdir.

## Verifying compiler changes

Run a subset by ID prefix instead of the full suite:

```bash
$ python3 test/runner.py test --test cj::bugs          # 36 tests under cj/bugs
```

After a run, re-run just the failures listed in `workdir/failed.txt` with
`--tests workdir/failed.txt`. Tests can be excluded globally via
`excludes.cfg` (glob patterns over testcase IDs).
