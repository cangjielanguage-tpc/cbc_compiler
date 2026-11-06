import std.runtime.*

var globalOffset: Int64 = 8

struct InlinePair {
    InlinePair(var first: Int64, var second: Int64) {}
}

@OverflowWrapping
func allowedAdd(value: Int64): Int64 {
    return value + 1
}

@OverflowWrapping
func allowedBranch(value: Int64): Int64 {
    if (value >= 0) {
        return value + 2
    }
    return value - 2
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
    value = allowedBranch(value)
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
