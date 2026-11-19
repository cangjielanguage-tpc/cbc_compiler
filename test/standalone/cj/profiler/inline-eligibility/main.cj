import std.runtime.*

var globalOffset: Int64 = 8

struct InlinePair {
    InlinePair(var first: Int64, var second: Int64) {}
}

@OverflowWrapping
func allowedAdd(value: Int64): Int64 {
    return value + 1
}

func allowedIdentity(value: Int64): Int64 {
    return value
}

@OverflowWrapping
func allowedArithmetic(value: Int64, other: Int64): Int64 {
    var result = value * other
    result = result ^ 85
    result = result << 1
    return result - 3
}

@OverflowWrapping
func allowedBranch(value: Int64): Int64 {
    if (value >= 0) {
        return value + 2
    }
    return value - 2
}

@OverflowWrapping
func allowedNestedBranch(value: Int64, selector: Int64): Int64 {
    if (value >= 0) {
        if (selector >= 0) {
            return value + 3
        }
        return value + 2
    }
    return value - 2
}

@OverflowWrapping
func allowedImmediateBranch(value: Int64): Int64 {
    if (value >= 0) {
        if (value < 10) {
            return value + 3
        }
        return value + 2
    }
    return value - 2
}

@OverflowWrapping
func allowedBackwardBranch(value: Int64): Int64 {
    var result = value
    while (result < 0) {
        result++
    }
    return result
}

@OverflowWrapping
func allowedInt32Branch(value: Int32): Int32 {
    if (value >= 0) {
        if (value < 10) {
            return value + 3
        }
        return value + 2
    }
    return value - 2
}

func rejectedComparison(value: Int64, other: Int64): Bool {
    return value < other
}

func rejectedImmediateComparison(value: Int64): Bool {
    return value < 10
}

func rejectedInt32Comparison(value: Int32, other: Int32): Bool {
    return value < other
}

func rejectedConversion(value: Int64): Int32 {
    return Int32(value)
}

func allowedCheckedAdd(value: Int64): Int64 {
    return value + 1
}

func allowedCheckedInt32Add(value: Int32): Int32 {
    return value + 1
}

func allowedCheckedInt8Add(value: Int8, other: Int8): Int8 {
    return value + other
}

func allowedCheckedInt16Wide(value: Int16): Int16 {
    return value + 4095
}

func allowedCheckedInt32Wide(value: Int32): Int32 {
    return value + 8000
}

func allowedCheckedInt64Add(value: Int64, other: Int64): Int64 {
    return value + other
}

@OverflowSaturating
func allowedSaturatingInt8Add(value: Int8, other: Int8): Int8 {
    return value + other
}

@OverflowSaturating
func allowedSaturatingInt64Wide(value: Int64): Int64 {
    return value + 8000
}

@OverflowSaturating
func allowedSaturatingShift(value: Int64, shift: Int64): Int64 {
    return value << shift
}

// Currently fails during runtime-bytecode preparation with "Register kind mismatch":
// the CBC call passes Float16 arguments in integer registers, while the rewriter expects float registers.
// func allowedFloat16Arithmetic(value: Float16, other: Float16): Float16 {
//     return value * other + other
// }

func allowedFloat32Arithmetic(value: Float32, other: Float32): Float32 {
    return value * other + other
}

func allowedFloat64Arithmetic(value: Float64, other: Float64): Float64 {
    return value * other + other
}

@OverflowWrapping
func rejectedLiteralLoad(value: Int64): Int64 {
    return value + globalOffset
}

@OverflowWrapping
func rejectedBranchingCaller(value: Int64): Int64 {
    if (value >= 0) {
        return allowedAdd(value)
    }
    return value - 1
}

func rejectedRecursion(value: Int64, depth: Int64): Int64 {
    if (depth == 0) {
        return value + 3
    }
    return rejectedRecursion(value, depth - 1)
}

func rejectedTooLarge(value: Int64): Int64 {
    var result = value
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    return result
}

func rejectedStackSlots(value: Int64): Int64 {
    var pair = InlinePair(value + 4, 0)
    return pair.first + pair.second
}

func rejectedLiveness(value: Int64): Int64 {
    gc()
    return value + 5
}

func rejectedExceptionRegions(value: Int64): Int64 {
    try {
        if (value < 0) {
            throw IllegalArgumentException()
        }
        return value + 6
    } catch (_: IllegalArgumentException) {
        return value
    }
}

func rejectedUnsupportedAbi<T>(value: T): T {
    return value
}

func rejectedInstruction(value: Int64): Int64 {
    return value + globalOffset
}

func hotCaller(input: Int64): Int64 {
    var value = input
    value = allowedAdd(value)
    allowedIdentity(value)
    allowedArithmetic(value, 3)
    value = allowedBranch(value)
    allowedNestedBranch(value, 1)
    allowedNestedBranch(value, -1)
    allowedNestedBranch(-value, 1)
    allowedImmediateBranch(value)
    allowedBackwardBranch(-1)
    allowedInt32Branch(1)
    allowedInt32Branch(-1)
    rejectedComparison(value, 10)
    rejectedImmediateComparison(value)
    rejectedInt32Comparison(1, 10)
    rejectedConversion(value)
    allowedCheckedAdd(value)
    allowedCheckedInt32Add(1)
    allowedCheckedInt8Add(1, 2)
    allowedCheckedInt16Wide(1)
    allowedCheckedInt32Wide(1)
    allowedCheckedInt64Add(value, 2)
    allowedSaturatingInt8Add(120, 20)
    allowedSaturatingInt64Wide(value)
    allowedSaturatingShift(value, 1)
    // allowedFloat16Arithmetic(1.5f16, 2.0f16)
    allowedFloat32Arithmetic(1.5f32, 2.0f32)
    allowedFloat64Arithmetic(1.5f64, 2.0f64)
    rejectedLiteralLoad(value)
    rejectedBranchingCaller(value)
    value = rejectedRecursion(value, 1)
    value = rejectedTooLarge(value)
    value = rejectedStackSlots(value)
    value = rejectedLiveness(value)
    value = rejectedExceptionRegions(value)
    value = rejectedUnsupportedAbi(value)
    value = rejectedInstruction(value)
    return value
}

main(): Int64 {
    var value: Int64 = 0
    for (_ in 0..50000) {
        value = hotCaller(value)
    }
    println(value)
    return 0
}
