# Future, thread and synchronization intrinsic lowering

The CHIR parser lowers these intrinsics through the existing managed `CJIntrinsic`
call path. They no longer use dummy results or the fake-intrinsic audit category.
Mutex, monitor, waitqueue and multicondition intrinsics use the same path.
The temporary fake-intrinsic and fallback-body
machinery (`fakeIntrinsicResult`, `AuditCHIRStubs`/`RejectCHIRStubs`) was
removed once std.core parsed without stubs.

## ABI evidence

The SDK's `modules/linux_x86_64_cjnative/std/libstd.core.bc`, read with its
`third_party/llvm/bin/llvm-dis`, declares and calls the following entry points.
All are present in the dynamic exports of
`runtime/lib/linux_x86_64_cjnative/libcangjie-runtime.so` (checked with `nm -D`).

| CHIR intrinsic | Runtime entry point | Runtime arguments and result |
| --- | --- | --- |
| FutureInit | CJ_MCC_FutureInit | Future reference -> empty Unit |
| FutureIsComplete | CJ_MCC_FutureIsComplete | Future reference -> Bool (i1) |
| FutureWait | CJ_MCC_FutureWait | Future reference, Int64 nanoseconds -> empty Unit |
| FutureNotifyAll | CJ_MCC_FutureNotifyAll | Future reference -> empty Unit |
| Sleep | CJ_MRT_Sleep | Int64 nanoseconds -> empty Unit |
| SetThreadObject | CJ_MCC_SetCurrentCJThreadObject | Thread reference -> empty Unit |
| IsThreadObjectInited | CJ_MCC_IsThreadObjectInited | no arguments -> Bool (i1) |
| GetThreadObject | CJ_MCC_GetCurrentCJThreadObject | no arguments -> Thread reference |
| DumpAllThreadsInfo | CJ_MCC_GetAllThreadSnapshot | RawArray<ThreadSnapshotInfo>, RawArray<StackTraceData>, RawArray<UInt8> TypeInfos -> RawArray<ThreadSnapshotInfo> reference |
| DumpCurrentThreadInfo | CJ_MCC_GetCurrentThreadSnapshot | hidden ThreadSnapshotInfo sret, RawArray<StackTraceData> and RawArray<UInt8> TypeInfos -> record through sret |

Here `Unit.Type` in the SDK LLVM IR is an empty struct. The call factories use
`SignatureType.Void` for these results, not a fabricated success value or a
hidden Unit sret argument. Future's CHIR instantiated type arguments do not add
generic metadata to these runtime signatures. The snapshot TypeInfos are real
allocation metadata, not dummy CHIR arguments.

`CangjieLLVMIRParser` has no special-case Future/thread lowering, and `RTSProc`
has no entries for these operations. The ABI declarations above come from the
SDK's already-lowered std.core LLVM IR, not guessed RTS procedure signatures.
The existing `CJIntrinsicTarget` / `CbcSymbolAdapter.adaptIntrinsic`
path emits a direct AOT-symbol reference using the managed ABI, as it already
does for `CJ_MCC_FillInStackTrace` and `CJ_MCC_DecodeStackTrace`. No new engine
symbols or bridges are required. Public runtime interface headers expose
thread creation, but not these std.core intrinsic signatures, so they must not
be inferred from `DYN_NewCJThreadFn` or replaced by calls to scheduler internals.

### Synchronization primitives

The SDK's `modules/linux_x86_64_cjnative/std/libstd.sync.bc` provides the
following signatures. All targets are exported by the same runtime library.

| CHIR intrinsic | Runtime entry point | Runtime arguments and result |
| --- | --- | --- |
| MutexInit | CJ_MCC_MutexInit | Mutex reference -> Bool (i1) |
| CjMutexLock | CJ_MCC_MutexLock | Mutex reference -> empty Unit |
| MutexTryLock | CJ_MCC_MutexTryLock | Mutex reference -> Bool (i1) |
| MutexCheckStatus | CJ_MCC_MutexCheckStatus | Mutex reference -> Bool (i1) |
| MutexUnlock | CJ_MCC_MutexUnlock | Mutex reference -> empty Unit |
| MonitorInit | CJ_MCC_WaitQueueForMonitorInit | Monitor reference -> Bool (i1) |
| WaitQueueInit | CJ_MCC_WaitQueueInit | WaitQueue reference -> Bool (i1) |
| MoitiorWait | CJ_MCC_MonitorWait | Monitor reference, Int64 nanoseconds -> Bool (i1) |
| MoitiorNotify | CJ_MCC_MonitorNotify | Monitor reference -> empty Unit |
| MoitiorNotifyAll | CJ_MCC_MonitorNotifyAll | Monitor reference -> empty Unit |
| MultiConditionWait | CJ_MCC_MultiConditionMonitorWait | Monitor reference, WaitQueue reference, Int64 nanoseconds -> Bool (i1) |
| MultiConditionNotify | CJ_MCC_MultiConditionMonitorNotify | Monitor reference, WaitQueue reference -> empty Unit |
| MultiConditionNotifyAll | CJ_MCC_MultiConditionMonitorNotifyAll | Monitor reference, WaitQueue reference -> empty Unit |

`Moitior*` retains the CHIR enum's spelling. Initialization, try-lock, status
and wait results are the actual runtime Booleans; lowering neither fabricates
success nor inverts the result. Empty Unit results use `SignatureType.Void`,
without hidden sret or TypeInfo parameters. Multicondition calls receive the
waitqueue reference extracted by the std.sync body, not its `ConditionID` record.

Native LLVM emits an inline mutex-lock fast path followed by
`CJ_MCC_MutexLockSlowPath`. CBC does not emit that fast path, so its forwarder
must use the full exported `CJ_MCC_MutexLock`. Runtime disassembly confirms
that this entry calls `MCC_MutexLock`, which includes the fast-path handling.

## Scope and known risk

Real runtime calls do not establish that CBC-compiled std.core is runnable.
Future has runtime-owned hidden synchronization storage that is not described
by its CHIR source fields. The existing Future allocation/type-info layout
issue is intentionally not repaired here. For example, the installed runtime's
`CJ_MCC_FutureInit` accesses completion/lock bytes at offsets 0x28/0x2c and
storage at 0x78; a source-fields-only allocation is not sufficient. Tests must
not interpret a successful compilation as proof of a safe Future allocation.

Synchronization objects have the same separate allocation concern. The SDK's
std.sync LLVM allocates Mutex, Monitor and WaitQueue objects through
`CJ_MCC_NewPinnedObject`, with a size of 168 bytes in this SDK. For example,
`CJ_MCC_MutexInit` accesses a status byte at 0x20 and semaphore storage starting
at 0x28. Forwarding calls does not add that hidden storage or pinned allocation
to CBC objects. This change makes no claim of runtime-safe synchronization
until allocation/layout support is implemented and tested separately.

## Validation (2026-11-26)

- `sbt -batch 'opt/testOnly *SyncIntrinsicsSuite *FutureThreadIntrinsicsSuite'
  'opt/test' jar` passed: seven focused tests and all 2,848 optimizer tests
  (two additional tests ignored), followed by a successful jar build.
- `FutureThreadIntrinsicsSuite` checks all ten Future/thread runtime targets,
  reference/timeout argument order, actual Bool/reference result signatures,
  absence of generic TI parameters, and snapshot sret.
- `SyncIntrinsicsSuite` checks all 13 synchronization runtime targets, argument
  order/types, Bool/Void results, managed calling convention and absence of
  hidden TypeInfo/sret parameters.
- `test/standalone/cj/sync-primitives/sync-primitives.cj` covers lock/unlock,
  try-lock, both monitor variants, waitqueue initialization, notifications and
  timed waits. Pure AOT builds in default and O2 modes produce the expected
  two `false` lines and exit status zero. The deprecated monitor APIs are used
  intentionally to exercise their distinct runtime forwarders.
- Serial (`-j 1`) CBC runs of this source, Fibonacci and spawn hello-world
  with the complete standard-library dependency input list stop at a generic
  call argument assertion in `std.collection.TreeMapEntryView`, rather than
  unsupported synchronization lowering. None reached execution. See
  [std-core-suite-failures.md](std-core-suite-failures.md) for the package list,
  assertion and local evidence paths.

After the generic-call blocker and hidden runtime allocation issues are
resolved separately, rerun the filtered synchronization and spawn/future
integration tests. Runtime failures must not be worked around with dummy
lowering or mixed AOT/CBC standard-library packages. No full integration suite
was run for this change.
