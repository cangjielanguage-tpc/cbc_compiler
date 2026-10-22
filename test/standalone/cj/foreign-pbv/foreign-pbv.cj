@C
struct S0 {
}

@C
struct S8i {
    var a: Int64

    init(a: Int64) {
        this.a = a
    }
}

@C
struct S8i2 {
    var a: Int32
    var b: Int32

    init(a: Int32, b: Int32) {
        this.a = a
        this.b = b
    }
}

@C
struct S8f {
    var a: Float64

    init(a: Float64) {
        this.a = a
    }
}

@C
struct S8f2 {
    var a: Float32
    var b: Float32

    init(a: Float32, b: Float32) {
        this.a = a
        this.b = b
    }
}

@C
struct S16i {
    var a: Int64
    var b: Int64

    init(a: Int64, b: Int64) {
        this.a = a
        this.b = b
    }
}

@C
struct S16f {
    var a: Float64
    var b: Float64

    init(a: Float64, b: Float64) {
        this.a = a
        this.b = b
    }
}

@C
struct S16m {
    var a: Int64
    var b: Float64

    init(a: Int64, b: Float64) {
        this.a = a
        this.b = b
    }
}

@C
struct S16m2 {
    var a: Float64
    var b: Int64

    init(a: Float64, b: Int64) {
        this.a = a
        this.b = b
    }
}

@C
struct S16f4 {
    var a: Float32
    var b: Float32
    var c: Float32
    var d: Float32

    init(a: Float32, b: Float32, c: Float32, d: Float32) {
        this.a = a
        this.b = b
        this.c = c
        this.d = d
    }
}

@C
struct S24i {
    var a: Int64
    var b: Int64
    var c: Int64

    init(a: Int64, b: Int64, c: Int64) {
        this.a = a
        this.b = b
        this.c = c
    }
}

@C
struct S24f {
    var a: Float64
    var b: Float64
    var c: Float64

    init(a: Float64, b: Float64, c: Float64) {
        this.a = a
        this.b = b
        this.c = c
    }
}

@C
struct S24m {
    var a: Int64
    var b: Float64
    var c: Float64

    init(a: Int64, b: Float64, c: Float64) {
        this.a = a
        this.b = b
        this.c = c
    }
}

@C
struct S32 {
    var a: Int64
    var b: Int64
    var c: Int64
    var d: Int64

    init(a: Int64, b: Int64, c: Int64, d: Int64) {
        this.a = a
        this.b = b
        this.c = c
        this.d = d
    }
}

@C
struct S48m {
    var a: Int64
    var b: Float64
    var c: Int64
    var d: Float64
    var e: Int64
    var f: Float64

    init(a: Int64, b: Float64, c: Int64, d: Float64, e: Int64, f: Float64) {
        this.a = a
        this.b = b
        this.c = c
        this.d = d
        this.e = e
        this.f = f
    }
}

@C
struct S64 {
    var a: Int64
    var b: Int64
    var c: Int64
    var d: Int64
    var e: Int64
    var f: Int64
    var g: Int64
    var h: Int64

    init(a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64) {
        this.a = a
        this.b = b
        this.c = c
        this.d = d
        this.e = e
        this.f = f
        this.g = g
        this.h = h
    }
}

@C
struct N16 {
    var p: S8i
    var q: S8f

    init(p: S8i, q: S8f) {
        this.p = p
        this.q = q
    }
}

@C
struct N16f {
    var p: S8f2
    var q: S8f2

    init(p: S8f2, q: S8f2) {
        this.p = p
        this.q = q
    }
}

@C
struct N24 {
    var p: S16i
    var q: S8i

    init(p: S16i, q: S8i) {
        this.p = p
        this.q = q
    }
}

@C
struct N32f {
    var p: S16f
    var q: S16f

    init(p: S16f, q: S16f) {
        this.p = p
        this.q = q
    }
}

@C
struct N40 {
    var p: S24i
    var q: S16i

    init(p: S24i, q: S16i) {
        this.p = p
        this.q = q
    }
}

foreign func pbvS0(x: Int64, s: S0, y: Int64): Int64
foreign func pbvS8i(s: S8i): Int64
foreign func pbvS8i2(s: S8i2): Int64
foreign func pbvS8f(s: S8f): Float64
foreign func pbvS8f2(s: S8f2): Float64
foreign func pbvS16i(s: S16i): Int64
foreign func pbvS16f(s: S16f): Float64
foreign func pbvS16m(s: S16m): Float64
foreign func pbvS16m2(s: S16m2): Float64
foreign func pbvS16f4(s: S16f4): Float64
foreign func pbvS24i(s: S24i): Int64
foreign func pbvS24f(s: S24f): Float64
foreign func pbvS24m(s: S24m): Float64
foreign func pbvS32(s: S32): Int64
foreign func pbvS48m(s: S48m): Float64
foreign func pbvS64(s: S64): Int64
foreign func pbvN16(n: N16): Float64
foreign func pbvN16f(n: N16f): Float64
foreign func pbvN24(n: N24): Int64
foreign func pbvN32f(n: N32f): Float64
foreign func pbvN40(n: N40): Int64
foreign func pbvMixA(x: Int64, s: S16m, d: Float64, y: Int64): Float64
foreign func pbvMixB(a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, s: S8i): Int64
foreign func pbvMixC(a: Float64, b: Float64, c: Float64, d: Float64, e: Float64, f: Float64, g: Float64, h: Float64, s: S8f): Float64
foreign func pbvMixD(a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, s: S16i): Int64
foreign func pbvMixG(a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, s: S16m): Float64

main(): Int64 {
    var passed = 0

    // --- small structs: 0/8/16/24 bytes, int-only / fp-only / mixed ---
    if (unsafe { pbvS0(11, S0(), 22) } == 55) {
        passed += 1
    } else {
        println("pbvS0 failed")
    }

    if (unsafe { pbvS8i(S8i(1234567890123)) } == 1234567890123) {
        passed += 1
    } else {
        println("pbvS8i failed")
    }

    if (unsafe { pbvS8i2(S8i2(1, 2)) } == 5) {
        passed += 1
    } else {
        println("pbvS8i2 failed")
    }

    if (unsafe { pbvS8f(S8f(3.5)) } == 3.5) {
        passed += 1
    } else {
        println("pbvS8f failed")
    }

    if (unsafe { pbvS8f2(S8f2(1.5f32, 2.5f32)) } == 6.5) {
        passed += 1
    } else {
        println("pbvS8f2 failed")
    }

    if (unsafe { pbvS16i(S16i(11, 22)) } == 55) {
        passed += 1
    } else {
        println("pbvS16i failed")
    }

    if (unsafe { pbvS16f(S16f(1.5, 2.5)) } == 6.5) {
        passed += 1
    } else {
        println("pbvS16f failed")
    }

    if (unsafe { pbvS16m(S16m(3, 0.5)) } == 4.0) {
        passed += 1
    } else {
        println("pbvS16m failed")
    }

    if (unsafe { pbvS16m2(S16m2(0.5, 3)) } == 6.5) {
        passed += 1
    } else {
        println("pbvS16m2 failed")
    }

    if (unsafe { pbvS16f4(S16f4(1.0f32, 2.0f32, 3.0f32, 4.0f32)) } == 30.0) {
        passed += 1
    } else {
        println("pbvS16f4 failed")
    }

    if (unsafe { pbvS24i(S24i(1, 2, 3)) } == 14) {
        passed += 1
    } else {
        println("pbvS24i failed")
    }

    if (unsafe { pbvS24f(S24f(1.5, 2.5, 3.5)) } == 17.0) {
        passed += 1
    } else {
        println("pbvS24f failed")
    }

    if (unsafe { pbvS24m(S24m(1, 2.5, 3.5)) } == 16.5) {
        passed += 1
    } else {
        println("pbvS24m failed")
    }

    // --- large structs ---
    if (unsafe { pbvS32(S32(1, 2, 3, 4)) } == 30) {
        passed += 1
    } else {
        println("pbvS32 failed")
    }

    if (unsafe { pbvS48m(S48m(1, 2.0, 3, 4.0, 5, 6.0)) } == 91.0) {
        passed += 1
    } else {
        println("pbvS48m failed")
    }

    if (unsafe { pbvS64(S64(1, 2, 3, 4, 5, 6, 7, 8)) } == 204) {
        passed += 1
    } else {
        println("pbvS64 failed")
    }

    // --- structs in structs ---
    if (unsafe { pbvN16(N16(S8i(5), S8f(2.5))) } == 10.0) {
        passed += 1
    } else {
        println("pbvN16 failed")
    }

    if (unsafe { pbvN16f(N16f(S8f2(1.0f32, 2.0f32), S8f2(3.0f32, 4.0f32))) } == 30.0) {
        passed += 1
    } else {
        println("pbvN16f failed")
    }

    if (unsafe { pbvN24(N24(S16i(1, 2), S8i(3))) } == 14) {
        passed += 1
    } else {
        println("pbvN24 failed")
    }

    if (unsafe { pbvN32f(N32f(S16f(1.5, 2.5), S16f(3.5, 4.5))) } == 35.0) {
        passed += 1
    } else {
        println("pbvN32f failed")
    }

    if (unsafe { pbvN40(N40(S24i(1, 2, 3), S16i(4, 5))) } == 55) {
        passed += 1
    } else {
        println("pbvN40 failed")
    }

    // --- mixed with scalars / register exhaustion (all-or-nothing) ---
    if (unsafe { pbvMixA(1, S16m(2, 3.5), 4.5, 5) } == 54871.0) {
        passed += 1
    } else {
        println("pbvMixA failed")
    }

    if (unsafe { pbvMixB(1, 2, 3, 4, 5, 6, S8i(7)) } == 140) {
        passed += 1
    } else {
        println("pbvMixB failed")
    }

    if (unsafe { pbvMixC(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, S8f(9.0)) } == 285.0) {
        passed += 1
    } else {
        println("pbvMixC failed")
    }

    if (unsafe { pbvMixD(1, 2, 3, 4, 5, S16i(6, 7)) } == 140) {
        passed += 1
    } else {
        println("pbvMixD failed")
    }

    if (unsafe { pbvMixG(1, 2, 3, 4, 5, 6, S16m(7, 8.5)) } == 208.0) {
        passed += 1
    } else {
        println("pbvMixG failed")
    }

    println("passed ${passed}")
    if (passed == 26) {
        return 0
    }
    return 1
}
