import std.time.*

const arraySize: Int64 = 100
const iterations: Int64 = 200000

main(): Int64 {
    let a = Array<Int64>(arraySize) { i => i - 50 }
    let b = Array<Int64>(arraySize) { i => i * 2 - 100 }
    let c = Array<Int64>(arraySize) { i => 200 - i * 3 }
    let d = Array<Int64>(arraySize) { i => (i % 2) * 150 - 75 }
    let out = Array<Int64>(arraySize, repeat: 0)

    let started = MonoTime.now()
    for (j in 0..iterations) {
        for (i in 0..arraySize) {
            let sum = a[i] + b[i] + c[i] + d[i]
            if (sum < 0) {
                out[i] = -sum
            } else {
                out[i] = sum
            }
        }
    }
    let elapsed = MonoTime.now() - started


    println(elapsed.toMilliseconds())
    return 0
}
