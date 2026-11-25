#!/usr/bin/env python3
"""Render independent-package audit results as a report and exact method inventory."""

import argparse
import collections
import html
import json
from pathlib import Path


CATEGORIES = {
    "missing-intrinsic": ("Missing intrinsic lowering", "The parser rejects an intrinsic with no lowering implementation. Add a real CBC operation or an ABI-correct runtime call, not a constant/no-op result."),
    "math-types": ("Math intrinsic operand types", "Float32/Float16 operands are passed to DoubleType/FloatType math nodes without conversion; PowI also receives an Int32 exponent where LongType is required. Normalize operands/results and fix overload selection."),
    "float-conversion": ("Redundant floating conversion assertion", "ValueConvert(F64,F64) reaches Identities.scala:464, whose same-type identity allows signed integers/U16 only. The CHIR floating conversion path emits the redundant node. Fix the identity or avoid creating it."),
    "callback-address": ("Address of a CBC-defined C callback", "CHIRParser.State.valueOfFunc asserts method.isCangjieForeign. These methods take/pass/store a C function value defined in CBC, rather than an imported foreign function. Lower the CBC callback address/bridge instead of requiring an imported symbol."),
    "generic-pointer": ("Generic CPointer read/write", "CPointerRead/CPointerWrite handle primitive pointees or fixed records and assert pointee.isRecord for generic T. Implement dynamic size/alignment, address calculation and generic value copy/boxing."),
    "atomic-option": ("Atomic Option<T> representation", "AtomicOps.irType calls ValueType(OptionLikeEnum(...T...)), which is unsupported. Normalize nullable/generic option storage to the correct atomic representation."),
    "cast": ("Unsupported structural/generic cast", "The CHIR cast matcher has no case for the observed tuple covariance or generic-reference-to-type-variable cast. Implement representation-aware casts, not a fabricated result."),
    "generic-dispatch": ("Generic/extension VTable resolution", "The parser cannot find the required VTable slot for pprintDifferentSizes or HandlerCase.tryHandle. Resolve extension/generic signatures and slot lookup consistently."),
    "field-instantiation": ("Generic field type substitution", "Loading TestCaseResult's ArrayList<RunStepResult> field compares the instantiated signature with ArrayList<T0> retained in GetFieldSeqRef and asserts. Apply owner/type-argument substitution to the field reference."),
    "generic-instanceof": ("Record-valued generic instanceof", "InstanceOfGeneric expects a traceable reference but receives a record address (ShrinkKeyWrapper or Array<T>). Lower record/generic type tests with the appropriate boxing/type-info representation."),
    "cstring-null": ("CString null constant", "CHIR.Constant.intConst has no CString case. A null CString enters this matcher and throws MatchError; lower it as an address-valued zero."),
    "receiver-abi": ("Mut-wrapper receiver ABI mismatch", "TreeMapEntryView.value setter's $withoutTI wrapper builds a key getter invocation whose receiver is LongType instead of a traceable reference. The reported mismatch is definite; the precise receiver-index/substitution correction needs a focused reproducer."),
    "enum-return": ("Enum return IR type mismatch", "MultilineDiffBuilder.compareByDist returns primitive enum WeakEq, but a VerificationNode replacement is not IntType as required by Return[IntType]. This identifies the failing IR invariant; the exact producer/rewrite needs a focused investigation."),
}


def classify(stub):
    reason = stub["reason"]
    stack = "\n".join(stub["stack"])
    if reason.startswith("not implemented: intrinsic "):
        return "missing-intrinsic"
    if reason == "assertion failed: F64":
        return "float-conversion"
    if "MathIntrinsic[" in reason or "PowI[" in reason:
        return "math-types"
    if "valueOfFunc" in stack:
        return "callback-address"
    if reason == "assertion failed" and stub.get("source_name") in ("read", "write", "$valueget", "$valueset", "init", "mallocCopyOf"):
        return "generic-pointer"
    if "OptionLikeEnum" in reason:
        return "atomic-option"
    if reason.startswith("CString (of class"):
        return "cstring-null"
    if "VTable slot" in reason:
        return "generic-dispatch"
    if "InstantiatedReference(std.collection:ArrayList" in reason:
        return "field-instantiation"
    if reason.startswith("not implemented: cast"):
        return "cast"
    if "InstanceOfGeneric" in reason:
        return "generic-instanceof"
    if "TreeMapEntryView" in reason:
        return "receiver-abi"
    if "Return[IntType]" in reason:
        return "enum-return"
    raise ValueError(f"Unclassified failure: {stub}")


def cell(text):
    return html.escape(str(text), quote=False).replace("|", "&#124;").replace("\n", " ")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--audit", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    results = json.loads((args.audit / "packages/results.json").read_text())
    strict = {r["package"]: r for r in json.loads((args.audit / "strict/results.json").read_text())}
    manifest = json.loads((args.audit / "packages/manifest.json").read_text())
    records = []
    packages = []
    for result in results:
        name = result["package"]
        packages.append({k: result[k] for k in ("package", "inputs", "exit", "seconds", "cbc_bytes", "parsed_methods")})
        packages[-1].update(strict_exit=strict[name]["exit"], strict_cbc_bytes=strict[name]["cbc_bytes"],
                            whole_method_stubs=len(result["stubs"]), intrinsic_pairs=len(result["intrinsic_stubs"]))
        for stub in result["stubs"]:
            records.append(dict(stub, package=name, kind="whole-method", category=classify(stub)))
        for stub in result["intrinsic_stubs"]:
            records.append(dict(stub, package=name, kind="intrinsic", category="dummy-intrinsic",
                                reason=f"Explicit dummy lowering for {stub['intrinsic']}; returns zero/null/no-op or constant success."))
    counts = collections.Counter(r["category"] for r in records)
    args.output.mkdir(parents=True, exist_ok=True)
    data = {"manifest": manifest, "scope": "Independent packages, default optimization, compilation only",
            "categories": CATEGORIES, "packages": packages, "methods": records}
    (args.output / "std-compilation-audit.json").write_text(json.dumps(data, indent=2) + "\n")
    lines = ["# Independent std/stdx compilation audit", "", "Date: 2026-11-24. Compiler baseline: `cea4b30b`; audit-only instrumentation added on top.", "",
             "## Result", "", "- **87/87** independent CHIR inputs emit CBC with existing fallbacks enabled.",
             "- **68/87** emit CBC with `+RejectCHIRStubs`; **19/87 fail**, with no CBC output.",
             "- **215 distinct whole-method fallbacks**, plus **103 distinct method/intrinsic pairs** in **61 other methods**.",
             "- Total: **276 distinct affected methods**, across **19 packages**. Whole-method and intrinsic-method sets do not overlap in this run.",
             "- std: 47 inputs, 34 strict passes, 13 failures, 194 whole-method fallbacks. stdx directory (including flatbuffers): 40 inputs, 34 strict passes, 6 failures, 21 whole-method fallbacks.", "",
             "These are local compilation results, not proof that the library is self-contained or executable. Missing reflection/runtime/interop lowering remains a compilation blocker even if correct execution is not required. Lowering to a real external runtime entry point is acceptable at this stage; substituting a constant/no-op or default-return method is not.", "",
             "## Scope and interpretation", "",
             "Every `*.chir` directly in SDK `tools/lib/std-packages` and `stdx-packages` was compiled separately, including `flatbuffers.chir`, `stdx.chir`, and `stdx.chir.chir`. No combined result is used here. No bytecode was executed for the audit. Optimization settings are compiler defaults, not an O2 matrix.", "",
             "The compiler automatically includes cbcengine-helper CHIR. Each input's compilation-set methods are compiled, not just main-reachable methods; `+GenCbcStdLib` bypasses the main requirement. Referenced packages can remain AOT/foreign dependencies in isolated outputs. The 68 strict passes therefore mean no diagnosed stubs in those local builds, not that their dependencies (especially std.core/std.sync) are stub-free.", "",
             "The audit records 86,663 distinct method parse entries summed across processes; helper methods repeat between packages, and generated wrappers/native declarations are not equivalent to CHIR bodies. This is not a count of unique standard-library source methods. `chir` in the JSON is the declaring package's reported input path, not authoritative method provenance: std.core shares package metadata with the automatically included helper. Source filenames are empty in this SDK; source names and full compiler signatures are retained.", "",
             "Strict mode disables the parser catch-and-stub path and rejects explicit fake intrinsic branches. Diagnostic mode retains those paths so later methods are visited. Each whole-method record reports the first exception in that method; fixing it can expose later unsupported expressions. Intrinsic inventory is deduplicated by (method, kind), not a static call-site count. This inventories the temporary CHIR fallback mechanisms, not all potential semantic inaccuracies in the compiler.", "",
             "## Whole-method blocker classification", "", "| Category | Methods | Affected packages |", "|---|---:|---|"]
    for category, count in sorted(counts.items(), key=lambda item: -item[1]):
        if category == "dummy-intrinsic":
            continue
        affected = collections.Counter(r["package"] for r in records if r["category"] == category)
        lines.append(f"| {CATEGORIES[category][0]} | {count} | " + ", ".join(f"{p} ({n})" for p, n in affected.items()) + " |")
    for category in CATEGORIES:
        lines.extend(["", f"### {CATEGORIES[category][0]} ({counts[category]})", "", CATEGORIES[category][1]])
    missing = collections.defaultdict(collections.Counter)
    for record in records:
        if record["category"] == "missing-intrinsic":
            missing[record["package"]][record["reason"].split("intrinsic ", 1)[1]] += 1
    lines.extend(["", "### Missing intrinsic families", "",
                  "Counts below are failing methods and first-rejected intrinsic kinds, not necessarily all intrinsics used by those methods."])
    for package, kinds in missing.items():
        lines.extend(["", f"- **{package}**: {sum(kinds.values())} methods, {len(kinds)} observed intrinsic kinds: " + ", ".join(f"`{k}` ({v})" for k, v in sorted(kinds.items())) + "."])
    lines.extend(["", "## Explicit intrinsic stubs", "",
                  "These do not throw in diagnostic mode and would be invisible in the old CBC-STUBBED-METHOD log. They are separate blockers for the no-stub requirement.", "",
                  "- **std.core**: 10 methods, 16 method/kind pairs (Future, Thread, ThreadSnapshot, sleep).",
                  "- **std.sync**: 51 methods, 87 method/kind pairs (mutex, monitor, wait queue, condition variables and their callers).", "",
                  "`MutexTryLock` and `FutureIsComplete` return constant 1; other branches fabricate void/zero/null/a temporary record or ZeroValueGeneric. All 23 declared fake intrinsic kinds were observed.", "",
                  "| Intrinsic | Method/kind pairs |", "|---|---:|"])
    for kind, n in collections.Counter(r["intrinsic"] for r in records if r["kind"] == "intrinsic").most_common():
        lines.append(f"| {kind} | {n} |")
    lines.extend(["", "## Per-package results", "", "| Input basename | Diagnostic exit | Whole-method stubs | Intrinsic pairs | Strict exit | CBC bytes (diagnostic) |", "|---|---:|---:|---:|---:|---:|"])
    for package in packages:
        lines.append("| " + " | ".join(str(package[k]) for k in ("package", "exit", "whole_method_stubs", "intrinsic_pairs", "strict_exit", "cbc_bytes")) + " |")
    lines.extend(["", "## Suggested implementation order", "",
                  "1. Fix floating identity/math operand normalization and CString null constants (45 whole-method failures).",
                  "2. Implement generic pointer access, atomic Option<T> representation and CBC C-callback addresses (35 failures).",
                  "3. Fix casts, generic dispatch, field substitutions, record type tests and the two remaining IR/ABI mismatches (12 failures).",
                  "4. Implement missing reflection/runtime/export-table intrinsic lowering (123 first-failure records), plus real Future/synchronization lowering for the 61 methods with dummy intrinsic handlers.",
                  "5. Rerun independent diagnostic and strict passes after each fix group; do not equate these first-failure counts with a guaranteed final work estimate.", "",
                  "## Reproduction and artifacts", "", f"Compiler jar SHA-256: `{manifest['jar_sha256']}`.", "",
                  "```bash", "source /workspace/toolchain/cangjie/envsetup.sh",
                  "python3 scripts/audit-stdlib.py --sdk /workspace/toolchain/cangjie \\",
                  "  --jar core/compiler/target/aot/cbc-compiler.jar --output /tmp/opencode/std-compilation-audit/packages",
                  "python3 scripts/audit-stdlib.py --sdk /workspace/toolchain/cangjie \\",
                  "  --jar core/compiler/target/aot/cbc-compiler.jar --output /tmp/opencode/std-compilation-audit/strict --strict",
                  "python3 scripts/summarize-stdlib-audit.py --audit /tmp/opencode/std-compilation-audit --output docs", "```", "",
                  "Each package directory under `packages/` or `strict/` contains its exact command, full build log, and structured result. Input SHA-256 hashes are in manifest.json. Existing results are reused only if the compiler/input manifest matches.", "",
                  "- [Exact method inventory](std-compilation-methods.md): full signatures, source names, per-method reasons and category.",
                  "- [Machine-readable inventory](std-compilation-audit.json): all packages, 215 method failures with stack frames, and 103 intrinsic pairs.", ""])
    (args.output / "std-compilation-audit.md").write_text("\n".join(lines))
    lines = ["# Exact std/stdx stub inventory", "", "See [classification and scope](std-compilation-audit.md). Full signatures uniquely identify overloads; source names alone do not.", ""]
    for package in packages:
        found = [r for r in records if r["package"] == package["package"]]
        if not found:
            continue
        lines.extend([f"## {package['package']}", "", "| Kind / category | Source name | Exact compiler method signature | Reason |", "|---|---|---|---|"])
        for record in found:
            lines.append("| " + " | ".join(cell(record[k]) for k in ("category", "source_name", "method", "reason")) + " |")
        lines.append("")
    (args.output / "std-compilation-methods.md").write_text("\n".join(lines))


if __name__ == "__main__":
    main()
