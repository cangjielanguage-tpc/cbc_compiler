import std.time.*

const dataSize: Int64 = 100
const iterations: Int64 = 1000000

func simpleFastPath(add: Int64, data: Array<Int64>, len: Int64, flag: Bool): Int64 {
    var result: Int64 = 0

    if (flag) {
        var i: Int64 = 0
        while (i < len) {
            result += data[i] + add
            i++
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
            p0 += x + p7
            p1 += x + p0
            p2 += x + p1
            p3 += x + p2
            p4 += x + p3
            p5 += x + p4
            p6 += x + p5
            p7 += x + p6
        }

        result = p0 + p1 + p2 + p3 + p4 + p5 + p6 + p7
    }

    return result
}

main(): Int64 {
    let data = Array<Int64>(dataSize) { i => i * 13 + 17 }

    let started = MonoTime.now()
    for (iteration in 0..iterations) {
        data[0] = iteration % 1000
        simpleFastPath(7, data, dataSize, true)
    }
    let elapsed = MonoTime.now() - started

    println(elapsed.toMilliseconds())
    return 0
}
