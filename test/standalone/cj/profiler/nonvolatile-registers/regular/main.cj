import std.time.*

const calls: Int64 = 53000000

@OverflowWrapping
func mix(x: Int64, n: Int64): Int64 {
    var a = x
    var b = x + 1
    var c = x + 2
    var d = x + 3
    var e = x + 4
    var f = x + 5
    var k = n
    while (k > 0) {
        a = a ^ b
        b += c
        c = c ^ d
        d += e
        e = e ^ f
        f += a
        k--
    }
    return a ^ b ^ c ^ d ^ e ^ f
}

@OverflowWrapping
func run(n: Int64): Int64 {
    var sum: Int64 = 0
    var i: Int64 = 0
    while (i < n) {
        sum += mix(i, 1 + (i & 3))
        i++
    }
    return sum
}

@OverflowWrapping
main(): Int64 {
    if (run(200000) != 57356929984) {
        return 1
    }

    let t = MonoTime.now()
    let sum = run(calls)
    let ms = (MonoTime.now() - t).toMilliseconds()
    if (sum != 4005175569038784) {
        return 1
    }

    println(ms)
    return 0
}
