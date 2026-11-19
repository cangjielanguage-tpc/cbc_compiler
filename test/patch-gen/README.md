# Patch-gen functional tests

The suite checks patch generation, declaration dumps, expected diagnostics and
serialization. Cases run at O0, O1 and O2 by default, using level-specific expectations
where present.

## Prerequisites

A Cangjie SDK with `cjc`, the hotfix plugin, `chir-dis` and the standard
`stdx/dynamic/stdx` libraries is required. Java must be available on PATH.
Build the generator and integration checker from the repository root:

```bash
sbt chirLibTest 'patchGen/assembly'
```

For a GraalVM native executable, configure `graalvm` to point to the
`native-image` executable and run `sbt patchGen/nativeImage`. The same
`scripts/patch-gen` launcher selects the native image when its matching
`patch-gen-native.info` file is beside the jar.


## Running

```bash
python3 test/patch-gen/run_tests.py --cangjie-home "$CANGJIE_HOME"
```

The runner builds the patchable library, compiles original and patched sources
with the hotfix plugin, generates patches and validates them with `chir-dis`.
It compares declaration dumps or diagnostics against the fixtures. Expected
errors must fail; successful cases must create a valid patch.

Declaration order and outer newlines are normalized. Names, unreachable markers
and diagnostic text remain significant. Work files and per-level `.actual`
outputs are stored in ignored `work/`; sources are not compiled in place.

## Options

- `--filter <prefix>...`: select cases by name prefix.
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

## Installing into a toolchain

Build with `sbt 'patchGen/assembly'`, then install the wrapper and its sibling jar:

```bash
cp scripts/patch-gen "$CANGJIE_HOME/tools/bin/patch-gen"
cp core/cbc-patchgen/target/patch-gen.jar "$CANGJIE_HOME/tools/bin/patch-gen.jar"
source "$CANGJIE_HOME/envsetup.sh"
patch-gen source.chir patched.chir
```

Preserve a copy of the existing native executable before replacing it. Java must
be available on PATH. The wrapper finds the jar beside itself when installed.

If a native image was built, also copy its binary and checksum metadata:

```bash
cp core/cbc-patchgen/target/patch-gen-native "$CANGJIE_HOME/tools/bin/"
cp core/cbc-patchgen/target/patch-gen-native.info "$CANGJIE_HOME/tools/bin/"
```

The binary, jar and metadata must come from the same build.
