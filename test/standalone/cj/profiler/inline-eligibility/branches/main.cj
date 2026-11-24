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

func checkBranches(value: Int64): Int64 {
    let arithmetic = (((value * 3) ^ 85) << 1) - 3
    if (allowedArithmetic(value, 3) != arithmetic || allowedBranch(value) != value + 2 ||
        allowedNestedBranch(value, 1) != value + 3 || allowedNestedBranch(value, -1) != value + 2 ||
        allowedImmediateBranch(value) != value + 3 || allowedBackwardBranch(-1) != 0 ||
        allowedInt32Branch(1) != 4 || allowedInt32Branch(-1) != -3) {
        return 1
    }
    let negative = -value - 1
    if (allowedNestedBranch(negative, 1) != negative - 2 ||
        branchingCaller(value) != arithmetic || branchingCaller(negative) != negative - 1) {
        return 1
    }
    if (loopCaller(1) != 3525 || loopWithTwoCallsCaller(1) != 28101 ||
        loopControlCaller(1) != 6943 || forLoopCaller(1, 3) != 857 ||
        nestedForLoopCaller(1, 2, 3) != 1151097) {
        return 1
    }
    return 0
}

main(): Int64 {
    for (iteration in 0..500000) {
        if (checkBranches(iteration & 7) != 0) {
            return 1
        }
    }
    println("OK")
    return 0
}
