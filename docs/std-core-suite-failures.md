# CBC std.core suite failures

Date: 2026-11-24. This records the stopped, partial `cjcbc` sweep and the
subsequent fixes. It is not a new test result or a complete suite result.

## Saved baseline

Ten completed batches contain 200 cases: 178 passed and 22 failed, across
14 source programs. All 200 compiled successfully. Batch 11 is incomplete;
its four completed passes are not included in these totals. Testing was
stopped at the user's request. The initial fixes below were not runtime-tested;
later dated sections record subsequent verification.

Logs and the exact failed IDs are preserved locally at:

- `/tmp/opencode/cjcbc-batches/failed-completed.txt`
- `/tmp/opencode/cjcbc-batches/batch-01` through `batch-10`
- `/tmp/opencode/cjcbc-batches/batch-11.log` (incomplete)

| Failure | Cases | Programs |
| --- | ---: | --- |
| Missing Object TypeInfo | 11 | `box-fields`, `bugs/bug-218-abstract-call` (default), `bugs/bug-220`, `checked-ops-2`, `extend/interf-generic-2`, `extend/tree-class-tostring` |
| Primitive interface metadata | 4 | `benches/fannkuchredux` (default), `bugs/bug-190`, `cpointer-generic` (default) |
| Mixed CBC/AOT interface dispatch | 3 | `array-22`, `enum/kind`, `extend/generics` (all default) |
| Missing at-exit output | 2 | `cbAtExit` |
| Stack-trace filename mismatch | 2 | `catch` |

Unless a mode is specified, both default and O2 failed.

## Boolean argument parsing

The runner supplied `+IgnoreSourceFileNameForCbc +RejectCHIRStubs` as one
argument. The legacy parser accepts one directive per argument and silently
discarded this bundled argument. Neither option was applied. Consequently,
the sweep's successful compilations do not establish strict stub rejection.
The independent std.core audit used separate arguments and is unaffected;
see [std-core-compilation-postfix.md](std-core-compilation-postfix.md).
(The temporary `AuditCHIRStubs`/`RejectCHIRStubs` enforcement machinery was
removed once std.core parsed without stubs: fallback bodies are no longer
produced at all, and unimplemented intrinsics fail with `notImplemented`.)

`CommandLineArguments.expand` now splits an argument containing only boolean
directives before legacy parsing. It leaves paths and equation values
containing spaces untouched. This enables both filename suppression and
stub rejection with the existing runner invocation. Source line numbers
remain intact; no golden output was changed.

Regression coverage was added in `CommandLineArgumentsSuite`. Build-only
verification of `compilerCommon/Test/compile` and `o2Lib/compile` succeeded.
The regression tests and the two failing programs have not been executed.

## Engine metadata fixes

Boxed-field metadata now resolves `std.core:Object` through the engine's term
and TypeInfo managers. This selects the loaded CBC definition when present,
instead of requiring a native Object solely to describe a reference field.
The CBC Object metadata is initialized before runtime helper preparation so
the runtime's root-object lookup can discover it.

The cache initialization in `core_typeinfo.cpp` resolves the active runtime's
exported C++ `TypeInfoManager::GetTypeInfoManager` and `InitAnyAndObjectType`
entrypoints. This is an explicit SDK ABI dependency, not yet a documented
`DYN_CJNativeInterface` operation; runtimes without those exports fail with
a diagnostic. It neither loads AOT core nor writes private runtime offsets.
The runtime operation initializes null caches only; it does not replace
previously registered roots. Initialization rejects existing native root
registrations before constructing CBC roots.

Primitive method tables now collect interface implementations from CBC
extension definitions. Primitive TypeInfos retain their original allocations
because emitted code and boxing retain those pointers; their extension
metadata is attached after CBC loading. Helper handles are registered before
this phase, since interface tables can contain abstract helper slots even
before helper bodies are prepared.

Static review found that duplicate primitive registration can share a native
method descriptor, exposing CBC receiver-indexed trampolines to native types
without CBC tail storage. Primitive enrichment therefore rejects a collision
with already registered native metadata when CBC methods or interfaces are present. Empty
CBC tables preserve the early primitive allocation without installing dispatch
metadata. This is a conservative guard, not mixed-core interoperability;
later dynamic loading and descriptor merging still require a supported bridge.

These changes target the 11 Object aborts and four primitive interface aborts.
They are implementation fixes, not evidence that the 15 cases now pass;
runtime verification remains deferred. The `cbcengine` target built
successfully, syntax-only checks passed for the added metadata regression
source, and whitespace checks passed. No test discovery or execution was
performed. The added unit sources cover empty primitive tables and helper
registration, not extension-bearing runtime registration or native collisions.

## At-exit registration

The original sweep compiled `std.core` to CBC but left `std.env` in AOT.
Native `std.env.atExit` therefore registered callbacks in native core's list,
while the generated CBC entry point drained CBC core's list. Mixing AOT and
CBC standard-library packages is not a supported configuration.

The compiler forwarding workaround was dropped. The `cjcbc` suite now includes
both `std.core` and `std.env` in its default CHIR compilation set. The existing
managed bodies of `std.env.atExit` and `std.env.exit` then call the same CBC
core implementation as the generated entry point, without synthetic wrappers
or a separate callback registry. Other referenced standard-library packages
also need CBC inputs; this change does not provide mixed-standard-library
interoperability.

`cb-order.cj` retains regression coverage for captured callbacks, LIFO ordering,
and function-value registration. The source is discovered by both `cj` and
`cjcbc`.

On 2026-11-26, after removing the workaround and rebuilding the compiler,
`fibonacci` and `spawn-hello` compiled successfully in default and O2 modes
with both `std.core` and `std.env` as CBC inputs. All four executions failed
before main with `cannot initialize CBC core type std.core:Any: native type
metadata is already registered`. Loader tracing shows native `std.fs`,
`std.io`, `std.time`, `std.collection`, `std.convert`, `std.math`, `std.binary`
and `std.sync` libraries being loaded, including native core. Adding
`std.env` alone does not close its standard-library dependency set.
Logs are under `/tmp/opencode/atexit-design/core-env-tests/`, with the summary
in `/tmp/opencode/atexit-design/core-env-tests.log` and loader tracing in
`/tmp/opencode/atexit-design/loader-debug.log`.

## Generic optional atomic references (2026-11-26)

Compiling the full native dependency closure as CBC inputs initially failed in
`std.sync.AtomicOptionReference<T>.init(Option<T>)`: `AtomicOps.irType` passed
generic `Option<T>` to `ValueType.fromSig`, which deliberately rejects that
signature. Generic field loads and option construction already represent these
values as traced references. Atomic load/store/CAS/swap now use the same IR
representation, retaining the original field signature for runtime layout.
The CBC assembler also tracks generic atomic payloads and optional payloads
as references rather than primitive registers.

`AtomicOptionReferenceSuite` reproduces the original assertion without requiring
all of `std.sync` to compile. `AtomicReferenceLivenessSuite` separately exercises
strict liveness checking for all four operations. The generic cases failed
before the fix and pass afterward; concrete reference and integer cases are
also covered. Verification passed for 16 focused assembler/liveness tests and
all 2,844 optimizer tests (two additional tests ignored), followed by `sbt jar`.

After installing that jar, Fibonacci and spawn hello-world were retried in
default and O2 modes, sequentially (`-j 1`), with this `CBC_STD_PKGS` override:

```text
std.core:std.math:std.collection:std.io:std.convert:std.binary:std.time:std.fs:std.sync:std.sort:std.env
```

All four builds now stop in `std.sync.TimerScheduler.add` at
`Intrinsic(mutexLock)` (`timer.cj:399`), reaching the explicit unsupported
synchronization branch in `CHIRParser.scala`. Mutex, monitor, waitqueue and
multicondition lowering remains unimplemented. This is the next observed
blocker, not proof that the remainder of those packages compiles. No fallback
bodies or mixed-standard-library workaround was introduced. The repository's
default CBC input list remains `std.core:std.env`.

The existing `cj::atomic` and `cj::atomic-spawn` programs also compiled in both
modes, but all four executions aborted before main with `cannot install CBC
primitive metadata for Int8: native type metadata is already registered`.
These AOT-standard-library runs do not exercise the generic CBC library body
and are not runtime verification of the optional-reference fix.

Local evidence under `/tmp/opencode/atexit-design/`:

- `atomic-ir-before.log`, `atomic-assembler-before.log`,
  `atomic-regression-after.log`: focused regression results.
- `atomic-validation-build.log`: optimizer tests and jar build.
- `atomic-fixed-transitive-tests.log`: serial Fibonacci/spawn results.
- `atomic-fixed-cj-tests.log`: existing atomic test results.

## Mixed interface ABI

The three mixed failures cross the ABI in both directions:

- Native `std.collection.ArrayList` requests `Collection<Rune>` on a CBC
  `Array<Rune>`.
- Native `std.collection.HashMap` requests `Equal<String>` on a CBC `String`.
- CBC code requests `Iterator<Object>` on a native
  `std.collection.ArrayListIterator<Object>`.

CBC generic TypeInfos currently use a full printed name and zero
`typeArgsNum`; native generic identity uses a template and type arguments.
Native extension lookup is UUID-based. In addition, CBC reconstructs method
tables from emitted declarations, while native callers use CHIR vtable slot
order. Correct interoperability requires compatible identity, slot order,
receiver/return ABI, and outer TypeInfos. A name-based fallback confined to
CBC-origin calls cannot repair the first two failures and is not a solution.

These three failures remain unresolved pending a safe ABI bridge. They have
not been excluded, and no additional standard package was compiled into CBC
to bypass them.

Static inspection also found two hazards in a naive identity change:

- The runtime can share a method descriptor through its UUID cache. Giving
  differently ordered CBC/native tables the same identity can select the wrong
  implementation, even if the CBC extension array is preserved.
- For a native generic receiver, the runtime interprets outerTI entries as
  getter functions. CBC currently stores direct TI pointers in those slots.
  Merely enabling generic `typeArgsNum` can turn a TI pointer into a call target.

A safe bridge should persist CHIR native owner/slot/ABI mappings in CBC
metadata, including the implementation's generic-context projection. A
CBC-to-native call can then translate both method and outerTI lookup to the
native owner and slot. Native-to-CBC calls additionally need supported
registration of native-layout adapter tables; an interpreter-only lookup
change cannot intercept those calls. CBC dynamic trampolines also require
the CBC-specific `dataMT` tail allocation and cannot be attached blindly to
native TypeInfos.
