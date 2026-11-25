# Independent std/stdx compilation audit

Date: 2026-11-24. Compiler baseline: `cea4b30b`; audit-only instrumentation added on top.

This report preserves the pre-fix baseline. For the subsequent focused std.core
result, see [std-core-compilation-postfix.md](std-core-compilation-postfix.md).
The complete 87-input matrix has not been rerun after those fixes.

## Result

- **87/87** independent CHIR inputs emit CBC with existing fallbacks enabled.
- **68/87** emit CBC with `+RejectCHIRStubs`; **19/87 fail**, with no CBC output.
- **215 distinct whole-method fallbacks**, plus **103 distinct method/intrinsic pairs** in **61 other methods**.
- Total: **276 distinct affected methods**, across **19 packages**. Whole-method and intrinsic-method sets do not overlap in this run.
- std: 47 inputs, 34 strict passes, 13 failures, 194 whole-method fallbacks. stdx directory (including flatbuffers): 40 inputs, 34 strict passes, 6 failures, 21 whole-method fallbacks.

These are local compilation results, not proof that the library is self-contained or executable. Missing reflection/runtime/interop lowering remains a compilation blocker even if correct execution is not required. Lowering to a real external runtime entry point is acceptable at this stage; substituting a constant/no-op or default-return method is not.

## Scope and interpretation

Every `*.chir` directly in SDK `tools/lib/std-packages` and `stdx-packages` was compiled separately, including `flatbuffers.chir`, `stdx.chir`, and `stdx.chir.chir`. No combined result is used here. No bytecode was executed for the audit. Optimization settings are compiler defaults, not an O2 matrix.

The compiler automatically includes cbcengine-helper CHIR. Each input's compilation-set methods are compiled, not just main-reachable methods; `+GenCbcStdLib` bypasses the main requirement. Referenced packages can remain AOT/foreign dependencies in isolated outputs. The 68 strict passes therefore mean no diagnosed stubs in those local builds, not that their dependencies (especially std.core/std.sync) are stub-free.

The audit records 86,663 distinct method parse entries summed across processes; helper methods repeat between packages, and generated wrappers/native declarations are not equivalent to CHIR bodies. This is not a count of unique standard-library source methods. `chir` in the JSON is the declaring package's reported input path, not authoritative method provenance: std.core shares package metadata with the automatically included helper. Source filenames are empty in this SDK; source names and full compiler signatures are retained.

Strict mode disables the parser catch-and-stub path and rejects explicit fake intrinsic branches. Diagnostic mode retains those paths so later methods are visited. Each whole-method record reports the first exception in that method; fixing it can expose later unsupported expressions. Intrinsic inventory is deduplicated by (method, kind), not a static call-site count. This inventories the temporary CHIR fallback mechanisms, not all potential semantic inaccuracies in the compiler.

## Whole-method blocker classification

| Category | Methods | Affected packages |
|---|---:|---|
| Missing intrinsic lowering | 123 | std.interop (5), std.reflect (95), std.runtime (23) |
| Math intrinsic operand types | 28 | std.math (27), std.unittest.prop_test (1) |
| Redundant floating conversion assertion | 15 | std.convert (2), std.core (6), std.unittest (5), std.unittest.prop_test (1), stdx.crypto.crypto (1) |
| Address of a CBC-defined C callback | 14 | std.ast (4), std.runtime (3), stdx.fuzz (1), stdx.net.tls (6) |
| Generic CPointer read/write | 12 | std.core (4), stdx.crypto.common (4), stdx.net.tls (4) |
| Atomic Option<T> representation | 9 | std.sync (9) |
| Unsupported structural/generic cast | 3 | std.deriving.impl (1), stdx.effect (1), stdx.net.http (1) |
| Generic/extension VTable resolution | 3 | std.unittest.diff (2), stdx.effect (1) |
| Generic field type substitution | 2 | std.unittest (2) |
| Record-valued generic instanceof | 2 | std.unittest.prop_test (2) |
| CString null constant | 2 | stdx.net.tls (2) |
| Mut-wrapper receiver ABI mismatch | 1 | std.collection (1) |
| Enum return IR type mismatch | 1 | std.unittest.diff (1) |

### Missing intrinsic lowering (123)

The parser rejects an intrinsic with no lowering implementation. Add a real CBC operation or an ABI-correct runtime call, not a constant/no-op result.

### Math intrinsic operand types (28)

Float32/Float16 operands are passed to DoubleType/FloatType math nodes without conversion; PowI also receives an Int32 exponent where LongType is required. Normalize operands/results and fix overload selection.

### Redundant floating conversion assertion (15)

ValueConvert(F64,F64) reaches Identities.scala:464, whose same-type identity allows signed integers/U16 only. The CHIR floating conversion path emits the redundant node. Fix the identity or avoid creating it.

### Address of a CBC-defined C callback (14)

CHIRParser.State.valueOfFunc asserts method.isCangjieForeign. These methods take/pass/store a C function value defined in CBC, rather than an imported foreign function. Lower the CBC callback address/bridge instead of requiring an imported symbol.

### Generic CPointer read/write (12)

CPointerRead/CPointerWrite handle primitive pointees or fixed records and assert pointee.isRecord for generic T. Implement dynamic size/alignment, address calculation and generic value copy/boxing.

### Atomic Option<T> representation (9)

AtomicOps.irType calls ValueType(OptionLikeEnum(...T...)), which is unsupported. Normalize nullable/generic option storage to the correct atomic representation.

### Unsupported structural/generic cast (3)

The CHIR cast matcher has no case for the observed tuple covariance or generic-reference-to-type-variable cast. Implement representation-aware casts, not a fabricated result.

### Generic/extension VTable resolution (3)

The parser cannot find the required VTable slot for pprintDifferentSizes or HandlerCase.tryHandle. Resolve extension/generic signatures and slot lookup consistently.

### Generic field type substitution (2)

Loading TestCaseResult's ArrayList<RunStepResult> field compares the instantiated signature with ArrayList<T0> retained in GetFieldSeqRef and asserts. Apply owner/type-argument substitution to the field reference.

### Record-valued generic instanceof (2)

InstanceOfGeneric expects a traceable reference but receives a record address (ShrinkKeyWrapper or Array<T>). Lower record/generic type tests with the appropriate boxing/type-info representation.

### CString null constant (2)

CHIR.Constant.intConst has no CString case. A null CString enters this matcher and throws MatchError; lower it as an address-valued zero.

### Mut-wrapper receiver ABI mismatch (1)

TreeMapEntryView.value setter's $withoutTI wrapper builds a key getter invocation whose receiver is LongType instead of a traceable reference. The reported mismatch is definite; the precise receiver-index/substitution correction needs a focused reproducer.

### Enum return IR type mismatch (1)

MultilineDiffBuilder.compareByDist returns primitive enum WeakEq, but a VerificationNode replacement is not IntType as required by Return[IntType]. This identifies the failing IR invariant; the exact producer/rewrite needs a focused investigation.

### Missing intrinsic families

Counts below are failing methods and first-rejected intrinsic kinds, not necessarily all intrinsics used by those methods.

- **std.interop**: 5 methods, 4 observed intrinsic kinds: `CREATE_EXPORT_HANDLE` (2), `CROSS_ACCESS_BARRIER` (1), `GET_EXPORTED_REF` (1), `REMOVE_EXPORTED_REF` (1).

- **std.reflect**: 95 methods, 56 observed intrinsic kinds: `APPLY_CJ_METHOD` (1), `APPLY_CJ_STATIC_METHOD` (1), `CHECK_METHOD_ACTUAL_ARGS` (2), `GET_ENUM_CONSTRUCTOR_INFO_FROM_ANY` (2), `GET_ENUM_CONSTRUCTOR_NAME` (1), `GET_FIELD_VALUE` (1), `GET_INSTANCE_FIELD_MODIFIER` (2), `GET_INSTANCE_FIELD_NAME` (2), `GET_INSTANCE_FIELD_TYPE` (2), `GET_METHOD_MODIFIER` (3), `GET_METHOD_NAME` (1), `GET_METHOD_RETURN_TYPE` (3), `GET_NUM_OF_ACTUAL_PARAMETERS` (1), `GET_NUM_OF_ENUM_CONSTRUCTOR_INFOS` (1), `GET_NUM_OF_FIELD_TYPES` (2), `GET_NUM_OF_FUNCTION_SIGNATURETYPES` (1), `GET_NUM_OF_GENERIC_PARAMETERS` (1), `GET_NUM_OF_INSTANCE_FIELD_INFOS` (1), `GET_NUM_OF_INSTANCE_METHOD_INFOS` (1), `GET_NUM_OF_INTERFACE` (1), `GET_NUM_OF_STATIC_FIELD_INFOS` (1), `GET_NUM_OF_STATIC_METHOD_INFOS` (1), `GET_OBJ_CLASS` (1), `GET_OR_CREATE_TYPEINFO_FOR_REFLECT` (1), `GET_PACKAGE_BY_QUALIFIEDNAME` (1), `GET_PACKAGE_GLOBAL_FIELD_INFO` (1), `GET_PACKAGE_GLOBAL_METHOD_INFO` (1), `GET_PACKAGE_NAME` (1), `GET_PACKAGE_NUM_OF_GLOBAL_FIELD_INFOS` (1), `GET_PACKAGE_NUM_OF_GLOBAL_METHODS` (1), `GET_PACKAGE_NUM_OF_TYPE_INFOS` (1), `GET_PACKAGE_TYPE_INFO` (1), `GET_PACKAGE_VERSION` (1), `GET_PARAMETER_INDEX` (1), `GET_PARAMETER_NAME` (1), `GET_PARAMETER_TYPE` (1), `GET_STATIC_FIELD_MODIFIER` (3), `GET_STATIC_FIELD_NAME` (3), `GET_STATIC_FIELD_TYPE` (3), `GET_STATIC_FIELD_VALUE` (2), `GET_SUB_PACKAGES` (1), `GET_SUPER_TYPE_INFO` (2), `GET_TYPETEMPLATE` (1), `GET_TYPE_BY_MANGLED_NAME` (14), `GET_TYPE_BY_QUALIFIED_NAME` (1), `GET_TYPE_FOR_ANY` (1), `GET_TYPE_INFO_MODIFIER` (1), `GET_TYPE_NAME` (1), `IS_BOX` (2), `IS_GENERIC` (2), `IS_PRIMITIVE` (1), `LOAD_PACKAGE` (1), `METHOD_ENTRYPOINT_IS_NULL` (5), `NEW_AND_INIT_OBJECT` (2), `SET_FIELD_VALUE` (1), `SET_STATIC_FIELD_VALUE` (2).

- **std.runtime**: 23 methods, 14 observed intrinsic kinds: `DUMP_CJ_HEAP_DATA` (2), `GET_ALLOCATE_HEAP_SIZE` (2), `GET_BLOCKING_THREAD_NUMBER` (2), `GET_GC_COUNT` (1), `GET_GC_FREED_SIZE` (1), `GET_GC_TIME_US` (1), `GET_MAX_HEAP_SIZE` (2), `GET_NATIVE_THREAD_NUMBER` (2), `GET_REAL_HEAP_SIZE` (1), `GET_THREAD_NUMBER` (2), `INVOKE_GC` (2), `SET_GC_THRESHOLD` (2), `START_CJ_CPU_PROFILING` (1), `STOP_CJ_CPU_PROFILING` (2).

## Explicit intrinsic stubs

These do not throw in diagnostic mode and would be invisible in the old CBC-STUBBED-METHOD log. They are separate blockers for the no-stub requirement.

- **std.core**: 10 methods, 16 method/kind pairs (Future, Thread, ThreadSnapshot, sleep).
- **std.sync**: 51 methods, 87 method/kind pairs (mutex, monitor, wait queue, condition variables and their callers).

`MutexTryLock` and `FutureIsComplete` return constant 1; other branches fabricate void/zero/null/a temporary record or ZeroValueGeneric. All 23 declared fake intrinsic kinds were observed.

| Intrinsic | Method/kind pairs |
|---|---:|
| MutexCheckStatus | 27 |
| CjMutexLock | 16 |
| MutexInit | 13 |
| MutexUnlock | 11 |
| MoitiorWait | 5 |
| MonitorInit | 4 |
| FutureIsComplete | 3 |
| Sleep | 3 |
| FutureWait | 2 |
| SetThreadObject | 2 |
| MutexTryLock | 2 |
| MoitiorNotify | 2 |
| MoitiorNotifyAll | 2 |
| WaitQueueInit | 2 |
| FutureInit | 1 |
| FutureNotifyAll | 1 |
| IsThreadObjectInited | 1 |
| GetThreadObject | 1 |
| DumpAllThreadsInfo | 1 |
| DumpCurrentThreadInfo | 1 |
| MultiConditionWait | 1 |
| MultiConditionNotify | 1 |
| MultiConditionNotifyAll | 1 |

## Per-package results

| Input basename | Diagnostic exit | Whole-method stubs | Intrinsic pairs | Strict exit | CBC bytes (diagnostic) |
|---|---:|---:|---:|---:|---:|
| std.argopt | 0 | 0 | 0 | 0 | 253596 |
| std.ast | 0 | 4 | 0 | 1 | 1619048 |
| std.binary | 0 | 0 | 0 | 0 | 123847 |
| std.collection | 0 | 1 | 0 | 1 | 552500 |
| std.collection.concurrent | 0 | 0 | 0 | 0 | 196244 |
| std.concurrent | 0 | 0 | 0 | 0 | 114623 |
| std.console | 0 | 0 | 0 | 0 | 135947 |
| std.convert | 0 | 2 | 0 | 1 | 221256 |
| std.core | 0 | 10 | 16 | 1 | 397544 |
| std.crypto | 0 | 0 | 0 | 0 | 88711 |
| std.crypto.cipher | 0 | 0 | 0 | 0 | 92591 |
| std.crypto.digest | 0 | 0 | 0 | 0 | 94007 |
| std.database | 0 | 0 | 0 | 0 | 88715 |
| std.database.sql | 0 | 0 | 0 | 0 | 279648 |
| std.deriving.api | 0 | 0 | 0 | 0 | 420092 |
| std.deriving.builtins | 0 | 0 | 0 | 0 | 490092 |
| std.deriving | 0 | 0 | 0 | 0 | 217675 |
| std.deriving.impl | 0 | 1 | 0 | 1 | 564188 |
| std.deriving.resolve | 0 | 0 | 0 | 0 | 369584 |
| std.env | 0 | 0 | 0 | 0 | 170423 |
| std.fs | 0 | 0 | 0 | 0 | 236264 |
| std.interop | 0 | 5 | 0 | 1 | 91059 |
| std.io | 0 | 0 | 0 | 0 | 143255 |
| std.math | 0 | 27 | 0 | 1 | 194896 |
| std.math.numeric | 0 | 0 | 0 | 0 | 368104 |
| std.net | 0 | 0 | 0 | 0 | 618788 |
| std.objectpool | 0 | 0 | 0 | 0 | 104471 |
| std.overflow | 0 | 0 | 0 | 0 | 219527 |
| std.posix | 0 | 0 | 0 | 0 | 126232 |
| std.process | 0 | 0 | 0 | 0 | 232636 |
| std.random | 0 | 0 | 0 | 0 | 151871 |
| std.ref | 0 | 0 | 0 | 0 | 90691 |
| std.reflect | 0 | 95 | 0 | 1 | 1944524 |
| std.regex | 0 | 0 | 0 | 0 | 214976 |
| std.runtime | 0 | 26 | 0 | 1 | 151427 |
| std.sort | 0 | 0 | 0 | 0 | 1363800 |
| std.sync | 0 | 9 | 87 | 1 | 202352 |
| std.time | 0 | 0 | 0 | 0 | 323620 |
| std.unicode | 0 | 0 | 0 | 0 | 367416 |
| std.unittest | 0 | 7 | 0 | 1 | 2702212 |
| std.unittest.common | 0 | 0 | 0 | 0 | 406660 |
| std.unittest.diff | 0 | 3 | 0 | 1 | 373820 |
| std.unittest.mock | 0 | 0 | 0 | 0 | 643824 |
| std.unittest.mock.internal | 0 | 0 | 0 | 0 | 132227 |
| std.unittest.mock.mockmacro | 0 | 0 | 0 | 0 | 261388 |
| std.unittest.prop_test | 0 | 4 | 0 | 1 | 905408 |
| std.unittest.testmacro | 0 | 0 | 0 | 0 | 1195292 |
| flatbuffers | 0 | 0 | 0 | 0 | 180472 |
| stdx.actors | 0 | 0 | 0 | 0 | 135603 |
| stdx.actors.macros | 0 | 0 | 0 | 0 | 307812 |
| stdx.aspect_cj | 0 | 0 | 0 | 0 | 129891 |
| stdx.aspect_cj.plugins.collect_aspects | 0 | 0 | 0 | 0 | 207056 |
| stdx.aspect_cj.plugins.weave_aspects | 0 | 0 | 0 | 0 | 235572 |
| stdx | 0 | 0 | 0 | 0 | 104463 |
| stdx.chir | 0 | 0 | 0 | 0 | 1948148 |
| stdx.compress | 0 | 0 | 0 | 0 | 120323 |
| stdx.compress.tar | 0 | 0 | 0 | 0 | 299928 |
| stdx.compress.zlib | 0 | 0 | 0 | 0 | 140376 |
| stdx.crypto | 0 | 0 | 0 | 0 | 88759 |
| stdx.crypto.common | 0 | 4 | 0 | 1 | 153816 |
| stdx.crypto.crypto | 0 | 1 | 0 | 1 | 211287 |
| stdx.crypto.digest | 0 | 0 | 0 | 0 | 138051 |
| stdx.crypto.keys | 0 | 0 | 0 | 0 | 285420 |
| stdx.crypto.kit | 0 | 0 | 0 | 0 | 180875 |
| stdx.crypto.x509 | 0 | 0 | 0 | 0 | 416208 |
| stdx.effect | 0 | 2 | 0 | 1 | 101459 |
| stdx.encoding.base64 | 0 | 0 | 0 | 0 | 105900 |
| stdx.encoding | 0 | 0 | 0 | 0 | 88771 |
| stdx.encoding.hex | 0 | 0 | 0 | 0 | 96831 |
| stdx.encoding.json | 0 | 0 | 0 | 0 | 249840 |
| stdx.encoding.json.stream | 0 | 0 | 0 | 0 | 322600 |
| stdx.encoding.url | 0 | 0 | 0 | 0 | 288992 |
| stdx.fuzz | 0 | 1 | 0 | 1 | 181244 |
| stdx.log | 0 | 0 | 0 | 0 | 162051 |
| stdx.logger | 0 | 0 | 0 | 0 | 245351 |
| stdx.net | 0 | 0 | 0 | 0 | 88731 |
| stdx.net.http | 0 | 1 | 0 | 1 | 1614692 |
| stdx.net.tls | 0 | 12 | 0 | 1 | 511064 |
| stdx.net.tls.common | 0 | 0 | 0 | 0 | 139123 |
| stdx.plugin | 0 | 0 | 0 | 0 | 185656 |
| stdx.plugin.manager | 0 | 0 | 0 | 0 | 141591 |
| stdx.serialization | 0 | 0 | 0 | 0 | 88807 |
| stdx.serialization.serialization | 0 | 0 | 0 | 0 | 145075 |
| stdx.string_intern | 0 | 0 | 0 | 0 | 229108 |
| stdx.syntax | 0 | 0 | 0 | 0 | 2307620 |
| stdx.unittest | 0 | 0 | 0 | 0 | 88771 |
| stdx.unittest.data | 0 | 0 | 0 | 0 | 277132 |

## Suggested implementation order

1. Fix floating identity/math operand normalization and CString null constants (45 whole-method failures).
2. Implement generic pointer access, atomic Option<T> representation and CBC C-callback addresses (35 failures).
3. Fix casts, generic dispatch, field substitutions, record type tests and the two remaining IR/ABI mismatches (12 failures).
4. Implement missing reflection/runtime/export-table intrinsic lowering (123 first-failure records), plus real Future/synchronization lowering for the 61 methods with dummy intrinsic handlers.
5. Rerun independent diagnostic and strict passes after each fix group; do not equate these first-failure counts with a guaranteed final work estimate.

## Reproduction and artifacts

Compiler jar SHA-256: `a932ab4ac2067bf2b4ccd353eed26e4956ede94f132c7bdf217097d7c3056799`.

```bash
source /workspace/toolchain/cangjie/envsetup.sh
python3 scripts/audit-stdlib.py --sdk /workspace/toolchain/cangjie \
  --jar core/compiler/target/aot/cbc-compiler.jar --output /tmp/opencode/std-compilation-audit/packages
python3 scripts/audit-stdlib.py --sdk /workspace/toolchain/cangjie \
  --jar core/compiler/target/aot/cbc-compiler.jar --output /tmp/opencode/std-compilation-audit/strict --strict
python3 scripts/summarize-stdlib-audit.py --audit /tmp/opencode/std-compilation-audit --output docs
```

Each package directory under `packages/` or `strict/` contains its exact command, full build log, and structured result. Input SHA-256 hashes are in manifest.json. Existing results are reused only if the compiler/input manifest matches.

- [Exact method inventory](std-compilation-methods.md): full signatures, source names, per-method reasons and category.
- [Machine-readable inventory](std-compilation-audit.json): all packages, 215 method failures with stack frames, and 103 intrinsic pairs.
