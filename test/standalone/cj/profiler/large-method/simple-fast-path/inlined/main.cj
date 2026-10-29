import std.time.*

const dataSize: Int64 = 100
const iterations: Int64 = 1000000

main(): Int64 {
    let data = Array<Int64>(dataSize) { i => i * 13 + 17 }

    let started = MonoTime.now()
    for (iteration in 0..iterations) {
        data[0] = iteration % 1000

        var result: Int64 = 0
        var i: Int64 = 0
        while (i < dataSize) {
            result += data[i] + 7
            i++
        }
    }
    let elapsed = MonoTime.now() - started

    println(elapsed.toMilliseconds())
    return 0
}
