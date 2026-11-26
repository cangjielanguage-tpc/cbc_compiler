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

At that point all four builds stopped in `std.sync.TimerScheduler.add` at
`Intrinsic(mutexLock)` (`timer.cj:399`), reaching the explicit unsupported
synchronization branch in `CHIRParser.scala`. Mutex, monitor, waitqueue and
multicondition lowering was still unimplemented. This was the next observed
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

## Synchronization runtime forwarders (2026-11-26)

All 13 mutex, monitor, waitqueue and multicondition intrinsics now lower to
managed `CJIntrinsic` runtime calls, following Future/thread lowering. The
signatures were checked against the SDK's std.sync LLVM and runtime exports.
Monitor initialization uses `CJ_MCC_WaitQueueForMonitorInit`, distinct from
`CJ_MCC_WaitQueueInit`. Mutex locking uses the full `CJ_MCC_MutexLock`, not
the slow-path entry that requires native inline fast-path code. Bool results
are preserved and empty runtime Unit results use Void without hidden sret.
The full mapping is in [future-thread-lowering.md](future-thread-lowering.md).

Verification passed for seven focused Future/thread and sync tests, all 2,848
optimizer tests (two additional tests ignored), and the jar build. After
installing the jar, Fibonacci and spawn hello-world were retried in default
and O2 modes with the same full `CBC_STD_PKGS` list above, sequentially with
`-j 1`. All four now stop in:

```text
std.collection:TreeMapEntryView._CNac16TreeMapEntryViewIG_G0_E5valuepsHNNat6OptionIG0_E$withoutTI(EOstd.core:Option;TC1<TC1>)U
```

The assertion reports `arg#2` of the `TreeMapEntryView.key` getter's
`InvokeSpecialTarget`: expected tag `VALUE` and type `Plain`, but received
`Param[(LongType,1)]` with tag `VALUE` and type `LongType`. The stack reaches
`CHIRParser.callMethod` and `InvokeTarget.instance`; the generic-call ABI
mismatch was the next observed compiler blocker, not fixed by the sync change.

The new `test/standalone/cj/sync-primitives/sync-primitives.cj` exercises Mutex,
Monitor and MultiConditionMonitor operations, including notifications and
nanosecond timed waits. Its source compiles in both modes (with expected
monitor deprecation warnings), but both full-closure CBC builds hit the same
TreeMapEntryView assertion. A separate pure AOT build/run in each mode
confirmed its golden output (`false`, `false`, exit status zero). This checks
the test's source semantics, not CBC runtime forwarding.

No CBC execution succeeded in these retries. The runtime's hidden sync-object
storage/pinning remains a separate unresolved issue: native std.sync uses
168-byte `CJ_MCC_NewPinnedObject` allocations for Mutex, Monitor and WaitQueue
in this SDK. The forwarders alone do not supply that allocation behavior.
There are no fallback bodies or mixed-standard-library workarounds, and the
default CBC input list remains `std.core:std.env`.

Local evidence under `/tmp/opencode/atexit-design/`:

- `std.sync.ll`, `std.sync.chirtxt`: SDK signatures and intrinsic operands.
- `sync-forwarders-build.log`: focused tests, optimizer tests and jar build.
- `sync-forwarders-tests.log`: serial Fibonacci/spawn build failures.
- `sync-primitives-tests.log`: serial synchronization regression build failures.
- `sync-primitives-aot/`: default/O2 pure AOT build logs and verified output.

## Generic mut-receiver value loads (2026-11-26)

The TreeMapEntryView failure is not a TypeInfo argument ordering problem.
Its mut value setter receives a payload address plus an owning reference.
CHIR then loads `this` and calls the non-mut key getter with that value. For
a variable-size generic struct, `CHIR.Load` previously returned the payload
address unchanged. The getter's ABI expects a boxed reference, causing the
`LongType` versus traced-reference (`Plain`) assertion at its receiver edge.

`test/standalone/cj/generic-mut-receiver/generic-mut-receiver.cj`, committed
separately as `3482e3c8`, reproduces this without any std.collection CBC input:
a generic struct's mut `replace` method calls its non-mut `get` method. Before
the fix, the default build fails with the identical receiver assertion. O2
compiles because the frontend optimizes away that call pattern. Pure AOT
builds/runs in both modes confirm the expected output `17`, `29`, status zero.

The parser now materializes a boxed value when loading an address-backed
variable-size value, using its existing `createBox`/generic load path. This
takes a value snapshot, consistent with fixed-size struct loads, rather than
casting the address to a reference or aliasing the mut receiver's owner.
Existing field-reference loads and already-boxed generic values keep their
existing handling. No call signature or TypeInfo ordering was changed.

All 2,848 optimizer tests passed (two additional tests ignored), and the jar
built successfully. With the rebuilt compiler installed, both reproducer
modes compile. Six related cases (`mut-receiver-copy`, `mut-mut-call` and
`mut-mut-call-spill`, default/O2) also compile. All eight executions abort
before main at the existing native/CBC primitive metadata collision for Int8;
they do not establish runtime correctness of this fix.

Fibonacci and spawn hello-world were rerun sequentially (`-j 1`) in default
and O2 with the full `CBC_STD_PKGS` list above. All four pass the former
TreeMapEntryView failure and now stop in `std.math.logBase(Float32, Float32)`:
`MathIntrinsic[D_LOG]` requires `DoubleType` but receives `FloatType`. This is
a separate math-intrinsic selection blocker; it is not repaired here. No CBC
program reached execution in the full-closure retry.

Local evidence:

- `/tmp/opencode/generic-call-before.log`: reproducer before the parser fix.
- `/tmp/opencode/generic-call-build.log`: optimizer tests and jar build.
- `/tmp/opencode/generic-call-after.log`: reproducer compilation/runtime retry.
- `/tmp/opencode/generic-call-related.log`: related mut-receiver retries.
- `/tmp/opencode/generic-call-full-closure.log`: serial Fibonacci/spawn retry.
- `/tmp/opencode/generic-call/aot/`: verified pure AOT build/output files.
- `/tmp/opencode/atexit-design/std.collection.chirtxt`: original CHIR load and
  key-getter call in TreeMapEntryView's mut value setter.

## Typed logarithms for logBase (2026-11-26)

CHIR's `log` intrinsic preserves its operand width. The SDK's LLVM also uses
`llvm.log.f32` for both logarithms in `logBase(Float32, Float32)`, followed by
a Float32 division; its native implementation calls `logf`. The compiler
selects `F_LOG` for Float32 and `D_LOG` for Float64. CBC emits `log.32` and
`log.64`, respectively, rather than widening the whole logBase expression.
Float16 uses the existing half-storage conversions around `F_LOG`, rounding
each logarithm to half before the subsequent division. This follows the
compiler's existing Float16-via-Float32 arithmetic model.

The new intrinsic includes constant folding, Java Math lookup exclusion,
backend code generation and cost models. The amd64 x87 path uses a Float32
result slot. For native targets requiring runtime-call lowering, the existing
Double `JR_log` entry point has explicit input/output conversions; CBC does
not take that fallback. No new runtime symbol or engine change is required.

The standalone regression at `test/standalone/cj/log-base/` is committed
separately as `d86785c9`. Its O2 build reproduces the original `D_LOG` type
assertion before the fix. Pure AOT default/O2 runs verify all six Boolean
checks and exit status zero. With the fix, default/O2 builds of this test
and the existing `math-intrinsics` test compile. Disassembly confirms
`log.32` for Float32, `log.64` for Float64, and half conversions around
`log.32` for Float16. All 2,850 optimizer tests pass (two ignored), including
Float32 constant/variable typing and typed native fallback checks. The jar
builds and is installed in the SDK.

These four standalone executions abort before main with conflicting Int8
primitive-extension methods between std.math and the engine helper, ending
at `failed to build primitive extension metadata for term kind 5`. They do
not provide CBC runtime validation.

Serial full-closure (`-j 1`) default/O2 retries for Fibonacci, spawn hello-world
and the new log-base test pass the logBase compilation failure. All six now
stop in `std.math.round(Float32)`: `MathIntrinsic[D_FLOOR]` expects DoubleType
but receives FloatType. Floor and the other hardcoded-Double math intrinsics
remain separate blockers, not repaired by the logarithm change.

Local evidence:

- `/tmp/opencode/log-base-before.log`: reproducer before the fix.
- `/tmp/opencode/log-base-build.log`: optimizer tests and jar build.
- `/tmp/opencode/log-base-lowering-final.log`: native fallback test recheck.
- `/tmp/opencode/log-base-final.log`: filtered standalone retry.
- `/tmp/opencode/log-base-full-final.log`: serial full-closure retry.
- `/tmp/opencode/log-base.disasm`: generated logarithms and half conversions.
- `/tmp/opencode/log-base/aot/`: verified pure AOT logs and output.
- `/tmp/opencode/std.math.ll`: SDK reference LLVM IR.

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
