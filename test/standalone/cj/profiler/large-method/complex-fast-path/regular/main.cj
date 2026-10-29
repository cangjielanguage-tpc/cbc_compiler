import std.time.*

const dataSize: Int64 = 100
const iterations: Int64 = 1000000
const modulus: Int64 = 1000003

func complexFastPath(data: Array<Int64>, len: Int64, flag: Bool): Int64 {
    var result: Int64 = 0

    if (flag) {
        for (i in 0..len) {
            let x = data[i]
            let t1 = (x * 3 + 7) % modulus
            let t2 = (t1 * 5 + x + 11) % modulus
            let t3 = (t2 * 7 - t1 + modulus) % modulus
            let t4 = (t3 + result * 3 + i) % modulus
            result = (t4 * 11 + t2) % modulus
        }
    } else {
        var p0: Int64 = 1
        var p1: Int64 = 2
        var p2: Int64 = 3
        var p3: Int64 = 4
        var p4: Int64 = 5
        var p5: Int64 = 6
        var p6: Int64 = 7
        var p7: Int64 = 8

        for (i in 0..len) {
            let x = data[i]
            p0 = (p0 + x + p7) % modulus
            p1 = (p1 * 3 + x + p0) % modulus
            p2 = (p2 * 5 + x + p1) % modulus
            p3 = (p3 * 7 + x + p2) % modulus
            p4 = (p4 * 11 + x + p3) % modulus
            p5 = (p5 * 13 + x + p4) % modulus
            p6 = (p6 * 17 + x + p5) % modulus
            p7 = (p7 * 19 + x + p6) % modulus
        }

        result = (p0 + p1 + p2 + p3 + p4 + p5 + p6 + p7) % modulus
    }

    return result
}

main(): Int64 {
    let data = Array<Int64>(dataSize) { i => i * 13 + 17 }

    let started = MonoTime.now()
    for (iteration in 0..iterations) {
        data[0] = iteration % 1000
        complexFastPath(data, dataSize, true)
    }
    let elapsed = MonoTime.now() - started

    println(elapsed.toMilliseconds())
    return 0
}
