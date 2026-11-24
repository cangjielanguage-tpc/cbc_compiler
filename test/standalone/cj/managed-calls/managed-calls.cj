enum CallTag {
    First | Second | Third
}

class Payload {
    Payload(let value: Int64) {}
}

struct Pair {
    Pair(let first: Int64, let second: Int64) {}
}

func integers(a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64, i: Int64, j: Int64): Int64 {
    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j
}

func narrow(a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64, x: Int8, y: UInt8, z: Int16, w: UInt16, n: Int32, flag: Bool, marker: Int64): Bool {
    return a == 1 && b == 2 && c == 3 && d == 4 && e == 5 && f == 6 && g == 7 && h == 8 &&
        x == -128 && y == 255 && z == -30000 && w == 65000 && n == -1234567 && flag && marker == 9876543210
}

func floats(a: Float32, b: Float32, c: Float32, d: Float32, e: Float32, f: Float32, g: Float32, h: Float32, i: Float32, j: Float32): Float64 {
    return Float64(a) + 2.0*Float64(b) + 3.0*Float64(c) + 4.0*Float64(d) + 5.0*Float64(e) +
        6.0*Float64(f) + 7.0*Float64(g) + 8.0*Float64(h) + 9.0*Float64(i) + 10.0*Float64(j)
}

func halves(a: Float16, b: Float16, c: Float16, d: Float16, e: Float16, f: Float16, g: Float16, h: Float16, i: Float16, j: Float16): Float64 {
    return Float64(a) + 2.0*Float64(b) + 3.0*Float64(c) + 4.0*Float64(d) + 5.0*Float64(e) +
        6.0*Float64(f) + 7.0*Float64(g) + 8.0*Float64(h) + 9.0*Float64(i) + 10.0*Float64(j)
}

func mixed(a: Int64, p: Float64, b: Int64, q: Float64, c: Int64, r: Float64, d: Int64, s: Float64, e: Int64, t: Float64, f: Int64, u: Float64, g: Int64, v: Float64, h: Int64, w: Float64, x: Int8, y: Float32, z: Int16, marker: Float64): Float64 {
    return Float64(a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h) +
        p + 2.0*q + 3.0*r + 4.0*s + 5.0*t + 6.0*u + 7.0*v + 8.0*w +
        Float64(x) + Float64(y) + Float64(z) + marker
}

func enums(a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64, x: CallTag, y: CallTag, marker: Int32): Bool {
    let tagsMatch = match ((x, y)) {
        case (CallTag.Third, CallTag.Second) => true
        case _ => false
    }
    return a + b + c + d + e + f + g + h == 36 && tagsMatch && marker == 1234567
}

func references(a: Payload, b: Payload, c: Payload, d: Payload, e: Payload, f: Payload, g: Payload, h: Payload, i: Payload, j: Payload): Int64 {
    return a.value + 2*b.value + 3*c.value + 4*d.value + 5*e.value + 6*f.value + 7*g.value + 8*h.value + 9*i.value + 10*j.value
}

func records(a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64, x: Pair, y: Pair): Pair {
    return Pair(a + b + c + d + e + f + g + h + x.first + y.first, x.second + 2*y.second)
}

func generic<T>(a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64, value: T): T {
    return value
}

interface Operation {
    func apply(a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64, i: Int64): Int64
}

class Calculator <: Operation {
    let bias: Int64

    init(bias: Int64) {
        this.bias = bias
    }

    public func apply(a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64, i: Int64): Int64 {
        return bias + integers(a, b, c, d, e, f, g, h, i, 10)
    }
}

func dispatch(op: Operation): Int64 {
    return op.apply(1, 2, 3, 4, 5, 6, 7, 8, 9)
}

func closure(f: (Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64) -> Int64): Int64 {
    return f(1, 2, 3, 4, 5, 6, 7, 8, 9)
}

struct Counter {
    var value: Int64 = 0

    mut func add(a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64): Unit {
        value += a + b + c + d + e + f + g + h
    }
}

main(): Int64 {
    var passed = 0
    if (integers(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) == 385) {
        passed += 1
    } else {
        println("integers failed")
    }
    if (narrow(1, 2, 3, 4, 5, 6, 7, 8, -128, 255, -30000, 65000, -1234567, true, 9876543210)) {
        passed += 1
    } else {
        println("narrow failed")
    }
    if (floats(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0) == 385.0) {
        passed += 1
    } else {
        println("floats failed")
    }
    // TODO: fix float16
    /*
    if (halves(1.0f16, 2.0f16, 3.0f16, 4.0f16, 5.0f16, 6.0f16, 7.0f16, 8.0f16, 9.0f16, 10.0f16) == 385.0) {
        passed += 1
    } else {
        println("halves failed")
    }
    */
    if (mixed(1, 1.0, 2, 2.0, 3, 3.0, 4, 4.0, 5, 5.0, 6, 6.0, 7, 7.0, 8, 8.0, -100, 0.5, -30000, 1.25) == -29690.25) {
        passed += 1
    } else {
        println("mixed failed")
    }
    if (enums(1, 2, 3, 4, 5, 6, 7, 8, CallTag.Third, CallTag.Second, 1234567)) {
        passed += 1
    } else {
        println("enums failed")
    }
    if (references(Payload(1), Payload(2), Payload(3), Payload(4), Payload(5), Payload(6), Payload(7), Payload(8), Payload(9), Payload(10)) == 385) {
        passed += 1
    } else {
        println("references failed")
    }
    let pair = records(1, 2, 3, 4, 5, 6, 7, 8, Pair(10, 20), Pair(30, 40))
    if (pair.first == 76 && pair.second == 100) {
        passed += 1
    } else {
        println("records failed")
    }
    if (generic<Int64>(1, 2, 3, 4, 5, 6, 7, 8, 123456789) == 123456789) {
        passed += 1
    } else {
        println("generic integer failed")
    }
    let payload = generic<Payload>(1, 2, 3, 4, 5, 6, 7, 8, Payload(42))
    if (payload.value == 42) {
        passed += 1
    } else {
        println("generic reference failed")
    }
    let genericPair = generic<Pair>(1, 2, 3, 4, 5, 6, 7, 8, Pair(12, 34))
    if (genericPair.first == 12 && genericPair.second == 34) {
        passed += 1
    } else {
        println("generic record failed")
    }
    if (dispatch(Calculator(7)) == 392) {
        passed += 1
    } else {
        println("interface failed")
    }
    let bias: Int64 = 7
    if (closure({a, b, c, d, e, f, g, h, i => bias + integers(a, b, c, d, e, f, g, h, i, 10)}) == 392) {
        passed += 1
    } else {
        println("closure failed")
    }
    var counter = Counter()
    counter.add(1, 2, 3, 4, 5, 6, 7, 8)
    if (counter.value == 36) {
        passed += 1
    } else {
        println("mut receiver failed")
    }
    println("passed ${passed}")
    if (passed == 13) {
        return 0
    }
    return 1
}
