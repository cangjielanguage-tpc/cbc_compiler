# Independent std.core compilation after fixes

Date: 2026-11-24. Scope: only SDK `tools/lib/std-packages/std.core.chir`,
compiled independently. No combined standard-library build was performed.

## Result

| Mode | Exit | Method parse entries | Whole-method fallbacks | Fake intrinsic pairs | CBC bytes |
| --- | ---: | ---: | ---: | ---: | ---: |
| Diagnostic | 0 | 1737 | 0 | 0 | 399384 |
| Strict (`+RejectCHIRStubs`) | 0 | 1737 | 0 | 0 | 399384 |

The pre-fix std.core baseline had ten whole-method fallbacks (six floating
getters and four generic CPointer read/write overloads), plus sixteen fake
intrinsic pairs in ten additional methods. The complete pre-fix 87-input matrix
is preserved in [std-compilation-audit.md](std-compilation-audit.md). No updated
all-package pass count is claimed here.

The compiler automatically includes cbcengine-helper CHIR. Method counts are
parse entries, not a count of distinct standard-library source functions.
Referenced packages can remain AOT dependencies. Stub-free compilation does not
prove standalone execution or complete semantic correctness.

## Changes validated

- Same-type floating ValueConvert identities preserve F16/F32/F64 values and
  bits without weakening the existing integral conversion invariant.
- Pointer support was cherry-picked from `34171c32` on
  `afilatov/intrinsics-size-2` as adapted commit `9164fe53`.
  Integration corrections use the CPointer signature for generic access bases
  and query TypeInfo for record strides: record AsmTypes describe addresses,
  not the pointed-to record size. Fixed-record reads copy their values.
- All ten Future/thread intrinsics lower to actual exported runtime calls.
  See [future-thread-lowering.md](future-thread-lowering.md) for ABI evidence
  and the known Future allocation/layout restriction.

## Validation

- `sbt jar`: passed.
- Focused IdentitiesSuite, FutureThreadIntrinsicsSuite, and
  CPointerIntrinsicsSuite: 44 tests passed, zero failed.
- `cj::cpointer-generic/`, `cj::cpointer/`, `cj::cstring/`, and
  `cj::fibonacci/`: all eight tests passed across default and O2 modes.
- The emitted strict CBC was disassembled with the existing local engine
  `output/linux_x86_64/tools/dis/disasm`. All ten runtime targets have real
  direct call sites. GetAllThreadSnapshot receives three loaded TypeInfos;
  GetCurrentThreadSnapshot receives a record sret and two loaded TypeInfos.
- The SDK's bundled disassembler aborted with a decoder overflow assertion;
  the local engine disassembler succeeded. No engine source changes were made.
- Future/thread execution was deliberately not tested. Known hidden Future
  runtime fields require separate allocation/layout work before execution can
  be considered safe. Remaining std.sync fake intrinsics are outside this scope.
- No full compiler test suite was run. Nothing was pushed.

## Reproduction and artifacts

Compiler jar SHA-256:
`8336a1616f91a9da00501f05d5104d454a89ffeb12611de864220876a175879c`.
This jar was installed into the local SDK for the focused integration tests.

From the compiler repository, after `sbt jar`:

```bash
source /workspace/toolchain/cangjie/envsetup.sh
python3 scripts/audit-stdlib.py --sdk /workspace/toolchain/cangjie \
  --jar core/compiler/target/aot/cbc-compiler.jar \
  --output /tmp/opencode/std-core-postfix/final/diagnostic --filter std.core.chir
python3 scripts/audit-stdlib.py --sdk /workspace/toolchain/cangjie \
  --jar core/compiler/target/aot/cbc-compiler.jar \
  --output /tmp/opencode/std-core-postfix/final/strict --filter std.core.chir --strict
```

Use fresh output directories if the compiler jar changes. The driver's manifest
rejects reuse across different compiler or input hashes.

Local evidence is in `/tmp/opencode/std-core-postfix/`:

- `final/{diagnostic,strict}/std.core/`: commands, results, audit logs and CBC.
- `final/strict/std.core/std.core.disasm`: successful local disassembly.
- `final/integration/`: per-test outputs and logs.
- `final-integration-tests.log`: eight-test summary.
- `final/unit-tests.log`: final focused 44-test summary.
- `final/runtime-call-check.txt`: runtime target and direct-call counts.
- `build-v3.log`: final compiler build after the record-stride correction.

Compiler implementation identity is `9164fe53` plus the uncommitted audit,
floating conversion, Future/thread and pointer-integration changes. The jar hash
above identifies the tested build more precisely than HEAD alone.
