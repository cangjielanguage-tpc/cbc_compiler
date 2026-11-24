import std.time.*

const calls: Int64 = 53000000

@OverflowWrapping
func run(n: Int64): Int64 {
    var sum: Int64 = 0
    var i: Int64 = 0
    while (i < n) {
        var a = i
        var b = i + 1
        var c = i + 2
        var d = i + 3
        var e = i + 4
        var f = i + 5
        var k = 1 + (i & 3)
        while (k > 0) {
            a = a ^ b
            b += c
            c = c ^ d
            d += e
            e = e ^ f
            f += a
            k--
        }
        sum += a ^ b ^ c ^ d ^ e ^ f
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
