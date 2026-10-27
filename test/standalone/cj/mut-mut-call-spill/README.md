# arm64 CBC regression test for a mut-to-mut call

The arm64 CBC compiler originally failed with an assertion while compiling
`Value.unpack(reader: StringReader)` in [main.cj](main.cj). This mutable method calls
`set(reader.read())`: an interface method produces a `String`, which is passed
to another mutable method on the same receiver.

This is a standalone reproduction of the assertion observed while compiling
`protobuf.Pb2String.unpack` in the LinkX hotfix inputs. It requires no LinkX,
protobuf, hotfix plugin, or interpreter dependencies. The failure occurs during
CBC compilation, before program execution.

The fix excludes outgoing mut receiver operands from the special mut-pair
liveness rule that retains spilled call arguments. CBC copies these source
operands into ABI argument locations before the call, where the callee tracks
them. Values used after the call, including exception-handler uses, remain live
in the caller; ordinary argument uses still make both components live above
the call.

The reproduction results and investigation below describe the original failure.

## Tested environment

Verified on 2026-10-24 with:

- CBC compiler revision `d47d42b7` on
  `msharapov/feature/mut-mut-compilation-issue/develop`.
- Separate arm64 and amd64 JARs built from that revision.
- Cangjie Compiler 1.0.0 (`cjnative`), reporting target
  `aarch64-apple-darwin`, from CJMP SDK `0.6.1.100-hotfix`.
- CHIR format 1203 and OpenJDK 17.0.20.1.

Here, arm64 and amd64 identify the CBC compiler's configured target
architecture. Both JARs were run on the same macOS arm64 host.

## Reproduction

Run from the `cbc_compiler` repository root, with the SDK's `cjc` on `PATH`
and the repository's build prerequisites configured:

```bash
sbt -Dsbt.supershell=false -Darch=arm64 jar

compiler_jar="$(pwd)/core/compiler/target/aot/aot.jar"
repro_dir="$(mktemp -d)"
cp test/standalone/cj/mut-mut-call-spill/main.cj "$repro_dir/main.cj"

(
    cd "$repro_dir" || exit 1
    cjc main.cj --emit-chir
    java -jar "$compiler_jar" \
        -CHIRVersion=1203 -AllCbcAOTDeps= \
        +genlibrary -outputname=main default.chir
)
```

Use the default Cangjie optimization settings for this reproduction.
`--fno-chir-function-inlining` also reproduces the failure, but is unnecessary.
Adding `-O2` to the Cangjie command makes this small reproducer compile
successfully in the tested environment.

To collect CBC IR logs, add `+DebugIrLogs -LogOnlyClass=default:Value` to
the Java command. Logs are written under `irlogs/` in the reproduction directory.

## Expected result

Cangjie and CBC compilation should both succeed. The program should print:

```text
updated
```

Its expected exit status is 0. [main.expected](main.expected) contains the
output followed by that exit status, following the standalone test convention.
The program was not executed as part of this compilation-only verification.

## Actual result

`cjc` succeeds and emits `default.chir`. The arm64 CBC compiler exits with
status 1 and reports:

```text
New compiler failed by unknown reason for default:Value.unpack(Rdefault:StringReader;)U
  java.lang.AssertionError: assertion failed
    at scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:11)
    at com.huawei.excelsior.jet.compiler.opt.backend.codegen.MutPairsDataGenerator.calcMutPairs$$anonfun$1(MutPairsDataGenerator.scala:99)
```

The same assertion occurs when `+genlibrary` is omitted, as in the standalone
test runner's compiler invocation.

## Compilation comparisons

| CBC compiler | Cangjie options | CBC result |
| --- | --- | --- |
| Current branch, arm64 | Default | Exit 1, assertion above |
| Current branch, amd64 | Default | Exit 0 |
| Current branch, arm64 | `-O2` | Exit 0 |
| Current branch, amd64 | `-O2` | Exit 0 |
| Installed SDK `0.6.1.100-hotfix`, arm64 | Default | Exit 0 |

The existing `test/tests-list-standalone.cfg` automatically includes this
fixture. Its default mode captures the failure; its `O2` mode currently compiles.

## Cause (investigated on 2026-10-24)

Reproduced with fresh arm64 and amd64 builds of `d47d42b7`. The assertion is
caused by inconsistent liveness of the two hidden mutable-receiver parameters
when the receiver is forwarded to `Value.set`. It occurs while generating GC
metadata for the **second** call (`set`), not the earlier `reader.read()` call.

In standalone mode, `ABI.makeABISignature` represents the receiver as a pair:
the record address (`SMutRecord`) and its owning object (`SMutObject`). The
owner can be null for a stack-local receiver, but the compiled method also
supports receivers stored inside heap objects.

After arm64 register allocation, the relevant IR in `Value.unpack` is:

```text
5:  Param[(LongType,1)]()        // incoming receiver address
6:  Param[(Plain,2)]()          // incoming receiver owner
34: Copy(6)       @ IR12        // owner survives reader.read()
38: Copy(36)      @ FrameSlot#1 // address spilled before reader.read(); producer is Param 5
19: Call[VIRTUAL, StringReader.read](...)
25: Call[MUT, Value.set](..., 38, 34, ...)
11: Return[VoidType](...)
```

`MutPairsDataGenerator.calcMutValues` walks the allocated IR backwards. At a
call it adds arguments in `FrameSlot` resources to the live set, but excludes
arguments in registers because they are expected to be tracked by the callee.
It records the call's live set **before** adding the ordinary argument uses.
Neither receiver component is used after `set`, so the resulting live set at
node 25 contains only node 38: the address in `FrameSlot#1`. The owner in
`IR12` is excluded.

A temporary diagnostic on the assertion confirmed:

```text
point = 25:Call[MUT, default:Value.set(...)]
live  = (38:Copy(36), FrameSlot#1, producer=5:Param[(LongType,1)]())
call arguments include (34:Copy(6), IR12, producer=6:Param[(Plain,2)]())
```

Consequently, `calcMutPairs` finds a nonempty `derivedParams` list and a null
`baseParam`, violating its assertion at line 99. The owner is still physically
available; it is missing from the calculated live set. `GCMapsToolbox` uses the
same stack/register argument distinction, so a correction must also keep GC
root metadata consistent with any receiver pair it emits.

The locations above are the CBC call's source resources. `FrameSlot#1` is a
local spill, not evidence that the native ABI ran out of argument registers.
With `ImplicitCallRegAllocInCBC`, call arguments can remain in arbitrary
registers or spill slots and are encoded by `CodeGeneratorCBC.callArgLocations`.

### Why arm64 and optimization change the result

The arm64 CBC calling convention has only three nonvolatile integer registers
(`IR11`–`IR13`), compared with six (`IR8`–`IR13`) on amd64. The register-allocation
trace shows the address, owner and reader initially occupying the three arm64
registers. Loading the reader's type information reuses `IR11`, and preserving
the address across `read()` then spills it. On amd64 the address and owner stay
in `IR8` and `IR9`; at `set`, both are excluded from the caller's mut live set,
so no incomplete pair is formed.

The `-O2` CHIR dump shows that `unpack`, `read` and `set` are inlined into
`main`; the failing call sequence disappears. Adding
`--fno-chir-function-inlining` to `-O2` restores the assertion in `Value.unpack`.

### Additional checks and relation to the receiver fixes

| arm64 experiment | CBC compilation |
| --- | --- |
| Original generic `unpack<T>` variant | Same assertion |
| Use concrete `Reader` (direct `SPECIAL` read call) | Same assertion |
| Use `let next = reader.read(); set(next)` | Same assertion |
| Existing `mut-receiver-copy` fixture | Exit 0 |
| Existing `mut-receiver-owner` fixture | Exit 0 |
| Temporarily keep the owner live after calls with `LocalReachabilityShield` | Exit 0 |

The record-load fix in `08b07ce7` preserves value snapshots for non-mut
consumers. This reproducer forwards the original receiver address/owner pair
to another mut method, so that copying rule does not address this case.
That commit also removed the broad owner reachability shield introduced in
`8613eb98`. Temporarily restoring that behavior keeps the owner live at `set`
and makes compilation succeed while the address remains spilled. This
confirms the liveness cause; it is not a validated production fix.

All temporary compiler instrumentation and the shield experiment were removed.
The remaining issue is to handle a receiver pair consistently when only its
address is retained by the call-argument liveness rule; deleting the assertion
alone does not resolve that inconsistency.

## Gitea search

On 2026-10-24, all 196 issues in `cbc/kanban`, including closed issues and
comments in 142 issue threads, were checked. No matching report of this
`MutPairsDataGenerator` assertion was found. The issue lists in the
`cbc/cbc_compiler` and `cbc/cbc-engine` mirrors were empty.

The related open issue
[#94, Support calls with implicitly passed arguments](https://git.excelsior.internal/cbc/kanban/issues/94)
discusses outgoing stack-argument liveness tracking and the Darwin arm64 ABI.
It does not report this assertion or contain this reproducer.

The investigation above uses local compiler sources and fresh compilation
results. It does not change the earlier Gitea search findings.
