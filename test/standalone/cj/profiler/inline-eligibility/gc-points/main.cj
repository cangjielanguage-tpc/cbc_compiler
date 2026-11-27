import std.runtime.gc

class Root {
    public var value: Int64

    public init(value: Int64) {
        this.value = value
    }
}

@OverflowWrapping
func pollGC(a: Root, b: Root, seed: Int64): Root {
    var n = seed
    var x = seed + 3
    var y = seed ^ 5
    var z = seed + 7
    var w = seed ^ 11
    while (n != 0) {
        x = (x ^ n) + y
        y = (y ^ n) + z
        z = (z ^ n) + w
        w = (w ^ n) + x
        n = n >> 1
    }
    return if (((x ^ y ^ z ^ w) & 1) == 0) { a } else { b }
}

@OverflowWrapping
func gcPointCaller(a: Root, b: Root, keep: Root, seed: Int64): Int64 {
    let result = pollGC(a, b, seed)
    return result.value + keep.value
}

@OverflowWrapping
func pollGCTyped(a: Root, b: Root, seed: Int64): Root {
    var roots = (a, b)
    if ((seed & 1) == 0) {
        roots = (b, b)
    }
    var n = seed
    var bits = 0
    while (n != 0) {
        bits ^= n
        n = n >> 1
    }
    let (first, second) = roots
    return if ((bits & 1) == 0) { first } else { second }
}

@OverflowWrapping
func pollGCWideTyped(a: Root, b: Root, seed: Int64): Root {
    var roots = (seed, a, b)
    if ((seed & 2) == 0) {
        roots = (seed, a, a)
    }
    var n = seed
    var bits = 0
    while (n != 0) {
        bits ^= n
        n = n >> 1
    }
    let (originalSeed, first, second) = roots
    return if (((bits ^ originalSeed) & 1) == 0) { second } else { first }
}

@OverflowWrapping
func gcPointTypedCaller(a: Root, b: Root, keep: Root, seed: Int64): Int64 {
    let first = pollGCTyped(a, b, seed)
    let second = pollGCWideTyped(a, b, seed)
    return first.value + second.value + keep.value
}

main(): Int64 {
    let a = Root(11)
    let b = Root(23)
    let keep = Root(37)
    let base = 0x40000000
    let expected = Array<Int64>(64, { i => pollGC(a, b, base + i).value + keep.value })
    let typedExpected = Array<Int64>(64, { i =>
        pollGCTyped(a, b, base + i).value + pollGCWideTyped(a, b, base + i).value + keep.value
    })
    for (i in 0..100000) {
        let index = i & 63
        if (gcPointCaller(a, b, keep, base + index) != expected[index] ||
            gcPointTypedCaller(a, b, keep, base + index) != typedExpected[index]) {
            return 1
        }
    }

    let collector = spawn {
        for (_ in 0..16) {
            gc()
            sleep(Duration.millisecond * 5)
        }
        0
    }
    for (i in 1..100000) {
        let index = i & 63
        if (gcPointCaller(a, b, keep, base + index) != expected[index] ||
            gcPointTypedCaller(a, b, keep, base + index) != typedExpected[index]) {
            return 1
        }
    }
    collector.get()
    println("OK")
    return 0
}
