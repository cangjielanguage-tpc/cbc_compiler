# Patch-gen functional tests

The suite checks patch generation, declaration dumps, expected diagnostics and
serialization. It contains 33 cases, each run at O0 and O1 by default.
O2 can be selected explicitly and uses level-specific expectations where present.

## Prerequisites

A Cangjie SDK with `cjc`, the hotfix plugin, `chir-dis` and the standard
`stdx/dynamic/stdx` libraries is required. Java must be available on PATH.
Build the generator and integration checker from the repository root:

```bash
sbt chirLibTest 'patchGen/assembly'
```

`chirLibTest` itself runs without the SDK. The functional runner owns all SDK
commands and file-based integration checks.

## Running

```bash
python3 test/patch-gen/run_tests.py --cangjie-home ~/Projects/software/hotfix-10.11
```

The runner builds the patchable library, compiles original and patched sources
with the hotfix plugin, generates patches and validates them with `chir-dis`.
It compares declaration dumps or diagnostics against the fixtures. Expected
errors must fail; successful cases must create a valid patch.

The `func1` and `class1` cases also invoke a model checker for guard removal,
body transfer, round-trip serialization and preserving an existing output when
validation fails. The checker uses the default CHIR schema. External tools run
in the Python runner, rather than in unit tests.

Declaration order and outer newlines are normalized. Names, unreachable markers
and diagnostic text remain significant. Work files and per-level `.actual`
outputs are stored in ignored `work/`; sources are not compiled in place.

## Options

- `--filter func1 class1`: select cases by name prefix.
- `--compile-level O0 O1 O2`: select optimization levels.
- `--patch-gen /path/to/patch-gen`: select another generator executable.
- `--native`: select a generator that prints declarations without `--test`;
  the model checker is skipped.
- `--checker-classpath <classpath>`: select prebuilt checker and generator
  artifacts from another build directory.

The default executable is `scripts/patch-gen`, backed by
`core/cbc-patchgen/target/patch-gen.jar`. The checker is packaged as
`core/chir-lib/target/chir-lib-tests.jar`. The SDK defaults to `CANGJIE_HOME`;
`CANGJIE_STDX_PATH` can override the standard stdx directory.
