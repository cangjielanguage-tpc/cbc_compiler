import std.time.*

const arraySize: Int64 = 100
const iterations: Int64 = 200000

func my_abs(x: Int64): Int64 {
    if (x < 0) {
        return -x
    } else {
        return x
    }
}

func compute(
    a: Array<Int64>,
    b: Array<Int64>,
    c: Array<Int64>,
    d: Array<Int64>,
    out: Array<Int64>,
    n: Int64
): Unit {
    for (i in 0..n) {
        let sum = a[i] + b[i] + c[i] + d[i]
        out[i] = my_abs(sum)
    }
}

main(): Int64 {
    let a = Array<Int64>(arraySize) { i => i - 50 }
    let b = Array<Int64>(arraySize) { i => i * 2 - 100 }
    let c = Array<Int64>(arraySize) { i => 200 - i * 3 }
    let d = Array<Int64>(arraySize) { i => (i % 2) * 150 - 75 }
    let out = Array<Int64>(arraySize, repeat: 0)

    let started = MonoTime.now()
    for (i in 0..iterations) {
        compute(a, b, c, d, out, arraySize)
    }
    let elapsed = MonoTime.now() - started

    println(elapsed.toMilliseconds())
    return 0
}
