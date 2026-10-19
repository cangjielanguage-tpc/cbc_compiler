# Native enum TypeInfo reconstruction

Four independent programs exercise cold native enum TypeInfos entering CBC
through `Probe.apply<T>`. `support.aot.cj` constructs all affected enums in AOT
code, avoiding the separate generic enum allocation problem tracked as #166.

| Program | Native values supplied to CBC |
| --- | --- |
| `nongeneric.cj` | Public reference enum with String, Int64 and empty constructors; CBC definition present. |
| `generic.cj` | Public generic reference enum with Payload and Int64 arguments, including nested generic arguments and every constructor. |
| `opaque.cj` | Private generic/non-generic reference enums whose definitions are absent from CBC metadata. |
| `controls.cj` | Ordinary class, value enum, reference option and primitive option. |

The interpreted callback constructs `Holder<T>`, collects with its value live,
then constructs the imported AOT `NativeBox<T>`. Native `unwrap<T>` casts that
container using a runtime-created generic TypeInfo, reads it and returns the
value. The AOT caller checks tags, numeric values, dynamically created strings
and object identity. Each affected scenario repeats three times. Separate
processes make the first TypeInfo lookup cold; subsequent calls cover cached
lookups and repeated UUID association. Only the controls deliberately warm
TypeInfos in CBC before the native callbacks.

Expected output is one `<program>-ok` line followed by the runner's zero exit
status. Normal discovery includes all four programs; no exclusions are needed.
Build with `-j 1` because all four programs share the same AOT output library.

## Running

Use an installed hotfix toolchain with its native SDK, launcher, engine helper,
CBC compiler and interpreter targeting the same architecture. The CBC compiler
build itself selects the target ABI: `sbt -Darch=arm64 jar` for aarch64 or
`sbt -Darch=amd64 jar` for x86_64. Rebuild the assembler with
`sbt 'cbcAsm/assembly'` before engine precommit checks.

From `cbc_compiler/test`, with that toolchain installed at `$CANGJIE_HOME`:

```sh
python3 runner.py test --cangjie-home "$CANGJIE_HOME" --jc-options=-CHIRVersion=13002 --filter standalone/cj/temp-enum-reconstruction -j 1
```

The runner builds both default and O2 inputs and launches each independently.
Replace `13002` with `1203` only when using a matching native compiler/runtime.

## Recorded validation: 2026-10-19

Source bases:

- Engine: `0ffdc2a091a3cdcfd006d959fcf9c7e8bea9070a`.
- Engine fix: `5abbcbe83aa6d147b1718333fabf6a94b8a4780a`.
- CBC compiler: `1e75c0e046dffebd1c2ed43bcf80efa7807f2bd2`.
- Both feature branches: `msharapov/LinkX-support/feature/8-temp-enum-fix/develop`.

Linux SDKs were downloaded from the CI `latest-aarch64` and `latest-x86_64`
artifact directories on 2026-10-19 (artifact timestamp 2026-10-16). Their
`.sdk-version.json` identifies compiler/runtime/stdlib branches as `hotfix-dev`,
compiler version `0.0.1`, and tools ref
`a635d9b132db3ec70390dea90f67d1b567aa4396`. The artifact metadata does not provide
compiler/runtime commit hashes; the exact downloaded binaries are identified
by archive SHA-256:

| SDK artifact | SHA-256 |
| --- | --- |
| [Linux aarch64](https://s3.excelsior.internal/buckets/artifacts/ci/latest-aarch64/cangjie-sdk.tar.gz) | `7e9b2a141303f4eb6b98063f85957525f9087b5cf7ccefdebfe35dbeb0fa1089` |
| [Linux x86_64](https://s3.excelsior.internal/buckets/artifacts/ci/latest-x86_64/cangjie-sdk.tar.gz) | `67019d4a73e9d0107248c53d1af2f7a66b080304be06d42e55d4fcc242eaab9f` |

CI URLs are mutable; check hashes when reproducing these results. Native
compiler, runtime, chir-diff and plugin sources were not changed.

| Check | Result |
| --- | --- |
| Unchanged engine, aarch64 Debug, CHIR 13002, default/O2 | Controls pass; generic, non-generic and opaque each fail with `type kind -124 not implemented yet` (six failures). |
| Fixed engine, aarch64 Debug/Release, CHIR 13002, default/O2 | All four programs pass: 16 executions. |
| Fixed engine, x86_64 Debug/Release, CHIR 13002, default/O2 | All four programs pass: 16 executions, under Docker emulation. |
| CHIR 1203 to CBC, aarch64/x86_64 targets, default/O2 | All four programs compile: 16 compilations. |
| CHIR 1203 execution on Linux | **Pending:** matching Linux SDK/runtime unavailable. Compilation used macOS Cangjie 1.0.0 from CJMP SDK 0.6.1.100-hotfix; it does not establish Linux execution. |
| ABI signature suite | 59 passed. |
| Engine precommit, both Linux architectures, Debug/Release | Passed in the recorded validation, with three pre-existing skips. |
| Existing enum, AOT enum, option-cast and GC regressions | All 13 programs pass in default/O2 on each architecture with Release engines: 52 executions. |

The existing regressions were selected with a filter file containing:

```text
include standalone/cj/enum/**/*.cj
include standalone/cj/option-casts-cbc/*.cj
include standalone/cj/gc-maps-frame/*.cj
include standalone/cj/mut-gc-scanning/*.cj
```

```sh
python3 runner.py test --cangjie-home "$CANGJIE_HOME" --jc-options=-CHIRVersion=13002 --filter-file /host-tmp/f08-regressions.cfg -j 2
```

Debug runs used the normal runner. To reuse already compiled inputs with a
Release engine, validation replaced only `libcbcengine.so` in the temporary
SDK and called the same runner's `run_test` for the four discovered programs:

```python
import asyncio, sys, runner
runner.toolchain_path = sys.argv[1]
async def main():
    suite = runner.StandaloneTestSuite(runner.toolchain_path)
    tests = [t for t in suite.standalone_tests
             if t.startswith("standalone/cj/temp-enum-reconstruction/")]
    assert len(tests) == 4
    for test in tests:
        await suite.run_test(test)
    assert not runner.failedTests
asyncio.run(main())
```

Engine precommit used Clang/lld 19.1.7, CMake 3.25.1 and local GoogleTest
1.12.1 in Debian bookworm containers. The existing skips were `CbcTest.Empty`,
`CbcTest.SimpleArithFloat` and `OptionsInitFromString.ValueWithSpace`.
From the engine directory inside each Linux container:

```sh
export LOCAL_GOOGLETESTS_PATH=/usr/src/googletest
python3 -c 'import argparse, build, platform; build.precommit(argparse.Namespace(target_os="linux", target_arch=platform.machine(), jobs=4), "/workspace/cbc-engine-initial-stage", "/tmp/f08-final-precommit")'
```

The build directory was placed on the container filesystem to avoid a
transient working-directory error when recreating directories on the source
bind mount. On a native checkout, use `python3 build.py precommit -j 4`.
The initial stale assembler and wrong-target CBC compiler artifact were
replaced before the recorded passing runs. No compatibility exception or
exclusion was added for those setup errors.

## Metadata and cold-lookup evidence

The Linux disassembler was run on all eight generated CBC files:

```sh
disasm standalone/cj/temp-enum-reconstruction/default/generic.cbc
```

Both modes have public `enum_support:NativeEnum` / `NativeGeneric` class
definitions with a UInt32 tag field and empty union-enum field lists. Their
signatures are references (`ref=1, rec=0`), including generic arguments.

The opaque program contains no `HiddenEnum` or `HiddenGeneric` names anywhere
in its disassembly. Both kinds therefore enter CBC only through native generic
arguments, rather than a CBC definition or direct CBC enum TypeInfo load.

Every program in both modes contains `load.ti.g` for `Holder<T>` and
`NativeBox<T>`. The callback passes its incoming generic TypeInfo into those
instructions; native enum TypeInfos must be reconstructed before those holders
can be instantiated. In the generic default-mode metadata, example terms are:

```text
@enum_support:NativeGeneric<@enum_support:Payload> - ref=1, rec=0, promoted=1
@enum_support:NativeGeneric<Int64> - ref=1, rec=0, promoted=1
@enum_support:NativeGeneric<@enum_support:NativeGeneric<@enum_support:Payload>> - ref=1, rec=0, promoted=1
```

The unused `metadata` functions mention public enum signatures without running
a TypeInfo query before the native callback. O2 may remove those functions;
the required enum definitions remain present in the inspected O2 metadata.
The reproduced cold `-124` failures on the unchanged engine independently
confirm that the affected native TypeInfos were not already in CBC's cache.

Full before/after logs and disassemblies are retained locally in the engine's
ignored `output/f08-validation/` directory. Historical malformed CBC,
reflection, Apple interpreter execution and F15 tag-offset changes remain
outside this regression.
