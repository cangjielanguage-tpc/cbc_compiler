@C
struct S4 {
    var a: Int32 = 0
}

@C
struct S8 {
    var a: Int64 = 0
}

@C
struct S16 {
    var a: Int64 = 0
    var b: Int64 = 0
}

@C
struct Inner {
    var x: Int64 = 0
    var y: Int64 = 0
}

@C
struct Complex {
    var c: Int8 = 0
    var d: Int32 = 0
    var big: Int64 = 0
    var inner: Inner = Inner()
    var g: VArray<Int32, $3> = [0, 0, 0]
}

// HFA of 2 doubles: XMM0/XMM1 on x86-64, v0/v1 on aarch64.
@C
struct F2 {
    var a: Float64 = 0.0
    var b: Float64 = 0.0
}

// HFA of 3 floats: 2 SSE pieces on x86-64, v0/v1/v2 on aarch64.
@C
struct F3f {
    var a: Float32 = 0.0f32
    var b: Float32 = 0.0f32
    var c: Float32 = 0.0f32
}

// HFA of 4 floats: 2 SSE pieces on x86-64, v0..v3 on aarch64.
@C
struct F4f {
    var a: Float32 = 0.0f32
    var b: Float32 = 0.0f32
    var c: Float32 = 0.0f32
    var d: Float32 = 0.0f32
}

// Mixed classes: XMM0 + RAX on x86-64, x0/x1 on aarch64 (non-HFA composites use GPRs).
@C
struct Mix {
    var a: Float64 = 0.0
    var b: Int64 = 0
}

// Diverging case: memory/sret on x86-64 (>16 bytes), HFA v0/v1/v2 on aarch64.
@C
struct F3d {
    var a: Float64 = 0.0
    var b: Float64 = 0.0
    var c: Float64 = 0.0
}

foreign func retS4(): S4
foreign func retS8(): S8
foreign func retS16(): S16
foreign func retComplex(): Complex

foreign func retF2(): F2
foreign func retF3f(a: Float32, b: Float32, c: Float32): F3f
foreign func retF4f(a: Float32, b: Float32, c: Float32, d: Float32): F4f
foreign func retMix(x: Int64, f: Float64): Mix
foreign func retF3d(x: Int64, y: Int64, z: Int64): F3d

main() {
    var passed = 0

    var s4 = unsafe { retS4() }
    println("S4: ${s4.a}")
    if (s4.a == 111) {
        passed += 1
    } else {
        println("retS4 failed")
    }

    var s8 = unsafe { retS8() }
    println("S8: ${s8.a}")
    if (s8.a == 2222222222) {
        passed += 1
    } else {
        println("retS8 failed")
    }

    var s16 = unsafe { retS16() }
    println("S16: ${s16.a} ${s16.b}")
    if (s16.a == 333 && s16.b == 444) {
        passed += 1
    } else {
        println("retS16 failed")
    }

    var cm = unsafe { retComplex() }
    println("Complex: ${cm.c} ${cm.d} ${cm.big} ${cm.inner.x} ${cm.inner.y} ${cm.g[0]} ${cm.g[1]} ${cm.g[2]}")
    if (cm.c == 1 && cm.d == 2 && cm.big == 3 && cm.inner.x == 4 && cm.inner.y == 5 &&
        cm.g[0] == 6 && cm.g[1] == 7 && cm.g[2] == 8) {
        passed += 1
    } else {
        println("retComplex failed")
    }

    var f2 = unsafe { retF2() }
    println("F2: ${f2.a} ${f2.b}")
    if (f2.a == 1.5 && f2.b == -2.5) {
        passed += 1
    } else {
        println("retF2 failed")
    }

    var f3f = unsafe { retF3f(1.25f32, 2.25f32, 3.25f32) }
    println("F3f: ${f3f.a} ${f3f.b} ${f3f.c}")
    if (f3f.a == 1.25f32 && f3f.b == 2.25f32 && f3f.c == 3.25f32) {
        passed += 1
    } else {
        println("retF3f failed")
    }

    var f4f = unsafe { retF4f(1.5f32, 2.5f32, 3.5f32, 4.5f32) }
    println("F4f: ${f4f.a} ${f4f.b} ${f4f.c} ${f4f.d}")
    if (f4f.a == 1.5f32 && f4f.b == 2.5f32 && f4f.c == 3.5f32 && f4f.d == 4.5f32) {
        passed += 1
    } else {
        println("retF4f failed")
    }

    var mix = unsafe { retMix(777, 6.5) }
    println("Mix: ${mix.a} ${mix.b}")
    if (mix.a == 6.5 && mix.b == 777) {
        passed += 1
    } else {
        println("retMix failed")
    }

    var f3d = unsafe { retF3d(10, 20, 30) }
    println("F3d: ${f3d.a} ${f3d.b} ${f3d.c}")
    if (f3d.a == 10.5 && f3d.b == 20.5 && f3d.c == 30.5) {
        passed += 1
    } else {
        println("retF3d failed")
    }

    println("passed ${passed}")
    if (passed == 9) {
        return 0
    }
    return 1
}
