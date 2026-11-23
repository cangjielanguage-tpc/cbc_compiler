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

func rejectedCheckedAdd(value: Int64): Int64 {
    return value + 1
}

func rejectedCheckedInt32Add(value: Int32): Int32 {
    return value + 1
}

func rejectedCheckedInt8Add(value: Int8, other: Int8): Int8 {
    return value + other
}

func rejectedCheckedInt16Wide(value: Int16): Int16 {
    return value + 4095
}

func rejectedCheckedInt32Wide(value: Int32): Int32 {
    return value + 8000
}

func rejectedCheckedInt64Add(value: Int64, other: Int64): Int64 {
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
func branchingCaller(value: Int64): Int64 {
    if (value >= 0) {
        return allowedArithmetic(value, 3)
    }
    return value - 1
}

@OverflowWrapping
func loopCaller(value: Int64): Int64 {
    var result = value
    var iteration: Int64 = 1
    while (iteration <= 3) {
        result = allowedArithmetic(result, iteration)
        iteration++
    }
    return result
}

@OverflowWrapping
func loopWithTwoCallsCaller(value: Int64): Int64 {
    var result = value
    var iteration: Int64 = 0
    while (iteration < 2) {
        result = allowedArithmetic(result, 2)
        result = allowedArithmetic(result, 3)
        iteration++
    }
    return result
}

@OverflowWrapping
func loopControlCaller(value: Int64): Int64 {
    var result = value
    var iteration: Int64 = 0
    while (iteration < 4) {
        iteration++
        if (iteration == 2) {
            continue
        }
        result = allowedArithmetic(result, iteration)
        if (result < 0) {
            break
        }
    }
    return result
}

@OverflowWrapping
func forLoopCaller(value: Int64, limit: Int64): Int64 {
    var result = value
    for (iteration in 0..limit) {
        if (iteration == 1) {
            continue
        }
        result = allowedArithmetic(result, 3)
    }
    return result
}

@OverflowWrapping
func nestedForLoopCaller(value: Int64, outerLimit: Int64, innerLimit: Int64): Int64 {
    var result = value
    for (_ in 0..outerLimit) {
        for (_ in 0..innerLimit) {
            result = allowedArithmetic(result, 3)
        }
    }
    return result
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
    rejectedCheckedAdd(value)
    rejectedCheckedInt32Add(1)
    rejectedCheckedInt8Add(1, 2)
    rejectedCheckedInt16Wide(1)
    rejectedCheckedInt32Wide(1)
    rejectedCheckedInt64Add(value, 2)
    allowedSaturatingInt8Add(120, 20)
    allowedSaturatingInt64Wide(value)
    allowedSaturatingShift(value, 1)
    // allowedFloat16Arithmetic(1.5f16, 2.0f16)
    allowedFloat32Arithmetic(1.5f32, 2.0f32)
    allowedFloat64Arithmetic(1.5f64, 2.0f64)
    rejectedLiteralLoad(value)
    branchingCaller(value)
    loopCaller(1)
    loopWithTwoCallsCaller(1)
    loopControlCaller(1)
    forLoopCaller(1, 3)
    nestedForLoopCaller(1, 2, 2)
    value = rejectedRecursion(value, 1)
    value = rejectedTooLarge(value)
    value = rejectedStackSlots(value)
    value = rejectedLiveness(value)
    value = rejectedExceptionRegions(value)
    value = rejectedUnsupportedAbi(value)
    value = rejectedInstruction(value)
    return value
}

@OverflowWrapping
func nonVolatileIntLoop(value: Int64, rounds: Int64): Int64 {
    var a = value
    var b = value + 1
    var c = value + 2
    var d = value + 3
    var e = value + 4
    var remaining = rounds
    while (remaining > 0) {
        a = a ^ b
        b = b + c
        c = c ^ d
        d = d + e
        e = e ^ a
        remaining--
    }
    return a
}

@OverflowWrapping
func nonVolatileIntCaller(value: Int64, rounds: Int64): Int64 {
    let keep = value + 2
    return nonVolatileIntLoop(keep, rounds) + keep
}

@OverflowWrapping
func nonVolatileFloatLoop(value: Float64, rounds: Int64): Float64 {
    var a = value
    var b = a + 1.0
    var c = b + 1.0
    var d = c + 1.0
    var e = d + 1.0
    var f = e + 1.0
    var g = f + 1.0
    var h = g + 1.0
    var i = h + 1.0
    var remaining = rounds
    while (remaining > 0) {
        a = a + b
        b = b + c
        c = c - d
        d = d + e
        e = e - f
        f = f + g
        g = g - h
        h = h + i
        i = i - a
        remaining--
    }
    return a
}

func nonVolatileFloatCaller(value: Float64, rounds: Int64): Float64 {
    let keep = value + 1.0
    return nonVolatileFloatLoop(keep, rounds) + keep
}

@OverflowWrapping
func typedSlotBranch(value: Int64, selector: Int64): Int64 {
    var pair: VArray<Int64, $2> = [value, value + 2]
    if (selector == 0) {
        pair[0] = pair[1] + 3
    }
    return pair[0] + pair[1]
}

@OverflowWrapping
func typedSlotCopy(value: Int64, selector: Int64): Int64 {
    var selected: VArray<Int64, $2> = [value, value + 2]
    let replacement: VArray<Int64, $2> = [value + 4, value + 6]
    if (selector != 0) {
        selected = replacement
    }
    selected[1] = selected[1] + 3
    return selected[0] + selected[1] + replacement[1]
}

@OverflowWrapping
func typedSlotPairSum(pair: VArray<Int64, $2>): Int64 {
    return pair[0] + pair[1]
}

@OverflowWrapping
func typedSlotLoopCaller(value: Int64, rounds: Int64): Int64 {
    let own: VArray<Int64, $2> = [value, value + 3]
    var result: Int64 = 0
    var iteration: Int64 = 0
    while (iteration < rounds) {
        result += typedSlotCopy(own[0], iteration)
        iteration++
    }
    return result + typedSlotPairSum(own)
}

@OverflowWrapping
func checkInlineFrameCases(seed: Int64): Int64 {
    let selector = seed & 1
    let rounds = selector + 1

    let keptInt = seed + 2
    var expectedInt = keptInt ^ (keptInt + 1)
    if (rounds == 2) {
        expectedInt = expectedInt ^ (2 * keptInt + 3)
    }
    if (nonVolatileIntCaller(seed, rounds) != expectedInt + keptInt) {
        println("nonVolatileIntCaller failed")
        return 1
    }
    if (untypedSlotIntCaller(seed, rounds) != expectedInt + keptInt) {
        println("untypedSlotIntCaller failed")
        return 1
    }

    let keptFloat = Float64(seed) + 1.0
    let expectedFloat = if (rounds == 1) { 3.0 * keptFloat + 1.0 } else { 5.0 * keptFloat + 4.0 }
    if (nonVolatileFloatCaller(Float64(seed), rounds) != expectedFloat) {
        println("nonVolatileFloatCaller failed")
        return 1
    }

    let expectedBranch = if (selector == 0) { 2 * seed + 7 } else { 2 * seed + 2 }
    if (typedSlotBranch(seed, selector) != expectedBranch) {
        println("typedSlotBranch failed")
        return 1
    }

    let expectedCopy = if (rounds == 1) { 5 * seed + 14 } else { 8 * seed + 33 }
    if (typedSlotLoopCaller(seed, rounds) != expectedCopy) {
        println("typedSlotLoopCaller failed")
        return 1
    }
    return 0
}

@OverflowWrapping
func untypedSlotIntLoop(value: Int64, rounds: Int64): Int64 {
    var a = value
    var b = value + 1
    var c = value + 2
    var d = value + 3
    var e = value + 4
    var f = value + 5
    var g = value + 6
    var h = value + 7
    var i = value + 8
    var j = value + 9
    var k = value + 10
    var remaining = rounds
    while (remaining > 0) {
        a = a ^ b
        b = b + c
        c = c ^ d
        d = d + e
        e = e ^ f
        f = f + g
        g = g ^ h
        h = h + i
        i = i ^ j
        j = j + k
        k = k ^ a
        remaining--
    }
    return a
}

@OverflowWrapping
func untypedSlotIntCaller(value: Int64, rounds: Int64): Int64 {
    let keep = value + 2
    return untypedSlotIntLoop(keep, rounds) + keep
}

main(): Int64 {
    var value: Int64 = 0
    for (iteration in 0..500000) {
        value = hotCaller(value)
        if (checkInlineFrameCases(iteration & 7) != 0) {
            return 1
        }
    }
    println(value)
    return 0
}
