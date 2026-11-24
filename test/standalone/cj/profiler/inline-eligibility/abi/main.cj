class Bias<T> {
    let delta: Int64

    init(delta: Int64) {
        this.delta = delta
    }

    @OverflowWrapping
    private func add(value: Int64): Int64 {
        return value + delta
    }

    @OverflowWrapping
    public func addWithTypes<U>(value: Int64): Int64 {
        return add(value) + shift<U>(value)
    }
}

@OverflowWrapping
func shift<T>(value: Int64): Int64 {
    return value + 3
}

func rejectedUnsupportedAbi<T>(value: T): T {
    return value
}

@OverflowWrapping
func rejectedStackArguments(
    a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64, i: Int64
): Int64 {
    return a + b + c + d + e + f + g + h + i
}

@OverflowWrapping
func rejectedSRet(value: Int64): VArray<Int64, $2> {
    return [value, value + 4]
}

func checkAbi(value: Int64): Int64 {
    if (rejectedUnsupportedAbi(value) != value ||
        rejectedStackArguments(value, 1, 2, 3, 4, 5, 6, 7, 8) != value + 36) {
        return 1
    }
    let pair = rejectedSRet(value)
    return if (pair[0] == value && pair[1] == value + 4) { 0 } else { 1 }
}

main(): Int64 {
    let bias = Bias<Int64>(5)
    for (iteration in 0..500000) {
        let value = iteration & 7
        if (bias.addWithTypes<Int32>(value) != 2 * value + 8 || checkAbi(value) != 0) {
            return 1
        }
    }
    println("OK")
    return 0
}
