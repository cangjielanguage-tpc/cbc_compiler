# Native dependencies of CBC std.core

Compiling std.core CHIR does not compile its native `@C` implementations.
The SDK supplies printing, string, math and other native helpers in
`lib/libstdFFI.so`. This library exports `CJ_CORE_PrintUTF8`,
`CJ_CORE_PrintOomHint` and the other CJ_CORE native functions. It does not
require `libcangjie-std-core.so` or supply its managed std.core methods.

The compiler records `stdFFI` as an implicit foreign dependency when std.core
is compiled to CBC. AOT std.core users retain `cangjie-std-core`; bootstrap
images without std.core acquire neither dependency. Explicit `-foreignlibs`
entries remain supported and are deduplicated with the implicit entries.

The engine must find the SDK's native FFI library. Its SDK location is
`runtime/lib/<arch>/libcbcengine.so`, so its new loader-relative search path
`$ORIGIN/../../../lib` points to `<SDK>/lib`. The Apple equivalent uses
`@loader_path/../../../lib`; only Linux x86_64 has been validated here.
Custom deployments must also ship stdFFI and its native dependencies in a
loader-visible location. No copying of native implementations or fallback
to AOT std.core is involved.

## Validation (2026-11-24)

- Compiler dependency-selection unit tests: 4 passed.
- Engine release and debug precommit: passed (1974 registered tests per mode;
  three explicitly skipped tests per mode).
- Plain and spawned hello world with CBC std.core: successful in default and
  O2, with `+RejectCHIRStubs` requested and only
  `-foreignlibs=cangjie-runtime` supplied explicitly. Each prints
  `Hello, world!` and exits zero. Runner invocations bundled the strict flag
  with the filename flag; the argument-parsing defect described in
  [std-core-suite-failures.md](std-core-suite-failures.md) means those
  invocations do not independently prove strict rejection was enabled.
- `cjcbc::core-native-print/`: default and O2 passed, covering string, Bool,
  signed/unsigned integers, float, and multibyte Unicode output.
- `cjcbc::spawn-hello/`: default and O2 passed.
- `cj::core-native-print/` and `cj::fibonacci/`: default and O2 passed.
- An additional AOT-core `cj::spawn-hello/` comparison failed in both modes
  at `ResolveFutureExecute` / `unexpected method count`. This separate
  AOT/CBC Future-dispatch problem is not repaired by the native dependency fix.
- Dynamic-loader diagnostics confirm stdFFI is loaded from `<SDK>/lib` via
  the engine's RUNPATH, with no AOT std.core loaded for the CBC-core smoke test.

Local build logs, reproduction commands and test outputs are under
`/tmp/opencode/core-ffi-proof/`. Compiler and engine builds were installed
into the local SDK; their previous versions were backed up there first.

The successful spawn smoke test does not establish general Future runtime
safety. The hidden Future storage/layout concerns documented in
[future-thread-lowering.md](future-thread-lowering.md) still require a
separate audit and stress testing.
