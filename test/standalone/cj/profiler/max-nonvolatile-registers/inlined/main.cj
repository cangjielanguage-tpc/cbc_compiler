import std.time.*

const calls: Int64 = 18000000

@OverflowWrapping
func run(n: Int64): Float64 {
    var sum = 0.0
    var i: Int64 = 0
    while (i < n) {
        var a = i + 0
        var b = i + 1
        var c = i + 2
        var d = i + 3
        var e = i + 4
        var f = i + 5
        let y = Float64(i)
        var p0 = y + 0.0
        var p1 = y + 1.0
        var p2 = y + 2.0
        var p3 = y + 3.0
        var p4 = y + 4.0
        var p5 = y + 5.0
        var p6 = y + 6.0
        var p7 = y + 7.0
        var p8 = y + 8.0
        var p9 = y + 9.0
        var p10 = y + 10.0
        var p11 = y + 11.0
        var p12 = y + 12.0
        var p13 = y + 13.0
        var p14 = y + 14.0
        var p15 = y + 15.0
        var k = 1 + (i & 3)
        while (k > 0) {
            a = a ^ b
            b += c
            c = c ^ d
            d += e
            e = e ^ f
            f += a
            p0 += p1
            p1 -= p2
            p2 += p3
            p3 -= p4
            p4 += p5
            p5 -= p6
            p6 += p7
            p7 -= p8
            p8 += p9
            p9 -= p10
            p10 += p11
            p11 -= p12
            p12 += p13
            p13 -= p14
            p14 += p15
            p15 -= p0
            k--
        }
        sum += Float64(a ^ b ^ c ^ d ^ e ^ f) +
            p0 + p1 + p2 + p3 + p4 + p5 + p6 + p7 +
            p8 + p9 + p10 + p11 + p12 + p13 + p14 + p15
        i++
    }
    return sum
}

@OverflowWrapping
main(): Int64 {
    if (run(200000) != -317673870016.0) {
        return 1
    }

    let t = MonoTime.now()
    let sum = run(calls)
    let ms = (MonoTime.now() - t).toMilliseconds()
    if (sum != -2647674124672512.0) {
        return 1
    }

    println(ms)
    return 0
}
