# Future and thread intrinsic lowering

The CHIR parser lowers these intrinsics through the existing managed `CJIntrinsic`
call path. They no longer use dummy results or the fake-intrinsic audit category.
Sync primitive intrinsics are not lowered at all: the compiler reports them as
not implemented (hard failure). The temporary fake-intrinsic and fallback-body
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

## Scope and known risk

Real runtime calls do not establish that CBC-compiled std.core is runnable.
Future has runtime-owned hidden synchronization storage that is not described
by its CHIR source fields. The existing Future allocation/type-info layout
issue is intentionally not repaired here. For example, the installed runtime's
`CJ_MCC_FutureInit` accesses completion/lock bytes at offsets 0x28/0x2c and
storage at 0x78; a source-fields-only allocation is not sufficient. Tests must
not interpret a successful compilation as proof of a safe Future allocation.
Remaining sync stubs are also independently nonfunctional.

## Focused validation for the integrating session

No build or tests were run while implementing this change.

- Run `FutureThreadIntrinsicsSuite` in the compiler opt tests. It checks all ten
  runtime targets, reference/timeout argument order, actual Bool/reference
  result signatures, absence of generic TI parameters, and snapshot sret.
- Re-run the existing std.core compilation audit with the integrated compiler.
  These ten kinds must be absent from dummy-intrinsic findings; remaining sync
  primitives must keep failing with notImplemented errors.
- Inspect the resulting std.core CBC/disassembly for the ten exported runtime
  targets, especially snapshot TypeInfos and current-snapshot sret.
- After the known Future allocation issue is resolved separately, run the
  existing filtered `standalone/cj/spawn-future` integration test. Do not use a
  runtime failure there as a reason to restore dummy lowering.

No full suite or combined standard-library build is needed for this change.
