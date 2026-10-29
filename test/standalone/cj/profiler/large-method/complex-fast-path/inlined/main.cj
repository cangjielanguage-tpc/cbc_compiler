import std.time.*

const dataSize: Int64 = 100
const iterations: Int64 = 1000000
const modulus: Int64 = 1000003

main(): Int64 {
    let data = Array<Int64>(dataSize) { i => i * 13 + 17 }
    var checksum: Int64 = 0

    let started = MonoTime.now()
    for (iteration in 0..iterations) {
        data[0] = iteration % 1000

        var result: Int64 = 0
        for (i in 0..dataSize) {
            let x = data[i]
            let t1 = (x * 3 + 7) % modulus
            let t2 = (t1 * 5 + x + 11) % modulus
            let t3 = (t2 * 7 - t1 + modulus) % modulus
            let t4 = (t3 + result * 3 + i) % modulus
            result = (t4 * 11 + t2) % modulus
        }
    }
    let elapsed = MonoTime.now() - started


    println(elapsed.toMilliseconds())
    return 0
}
