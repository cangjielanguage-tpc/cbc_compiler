foreign func fcAdd2(a: Int64, b: Int64): Int64

foreign func fcSum6(a: Int32, b: Int32, c: Int32, d: Int32, e: Int32, f: Int32): Int32

foreign func fcFAdd2(a: Float64, b: Float64): Float64

foreign func fcFSum8(a: Float64, b: Float64, c: Float64, d: Float64, e: Float64, f: Float64, g: Float64, h: Float64): Float64

foreign func st10(a1: Int64, a2: Int64, a3: Int64, a4: Int64, a5: Int64, a6: Int64, a7: Int64, a8: Int64, a9: Int64, a10: Int64): Int64

foreign func sf12(a1: Float64, a2: Float64, a3: Float64, a4: Float64, a5: Float64, a6: Float64, a7: Float64, a8: Float64, a9: Float64, a10: Float64, a11: Float64, a12: Float64): Float64

foreign func stmix(x1: Int64, d1: Float64, x2: Int64, d2: Float64, x3: Int64, d3: Float64, x4: Int64, d4: Float64, x5: Int64, d5: Float64, x6: Int64, d6: Float64, x7: Int64, d7: Float64, x8: Int64, d8: Float64): Float64

foreign func fcBytes(a1: Int8, a2: Int8, a3: Int8, a4: Int8, a5: Int8, a6: Int8, a7: Int8, a8: Int8, a9: Int8, a10: Int8, a11: Int8, a12: Int8, a13: Int8, a14: Int8, a15: Int8, a16: Int8, a17: Int8, a18: Int8, a19: Int8, a20: Int8): Int64

foreign func fcNarrowMix(a1: UInt8, a2: Int16, a3: UInt8, a4: Int32, a5: Int8, a6: UInt16, a7: Int64, a8: UInt8, a9: Int16, a10: Int32, a11: Int8, a12: UInt8, a13: Int16, a14: Int64, a15: UInt8, a16: Int8, a17: Int32, a18: UInt16, a19: Int8, a20: UInt8): Int64

foreign func fcFloats32(a1: Float32, a2: Float32, a3: Float32, a4: Float32, a5: Float32, a6: Float32, a7: Float32, a8: Float32, a9: Float32, a10: Float32, a11: Float32, a12: Float32): Float64

foreign func fcMixStack(a1: Int16, b1: Float32, a2: Int16, b2: Float32, a3: Int16, b3: Float32, a4: Int16, b4: Float32, a5: Int16, b5: Float32, a6: Int16, b6: Float32, a7: Int16, b7: Float32, a8: Int16, b8: Float32, a9: Int16, b9: Float32, a10: Int16, b10: Float32, a11: Int16, b11: Float32, a12: Int16, b12: Float32): Float64

main(): Int64 {
    var passed = 0
    if (unsafe { fcAdd2(40, 2) } == 42) {
        passed += 1
    } else {
        println("fcAdd2 failed")
    }
    if (unsafe { fcSum6(1, 2, 3, 4, 5, 6) } == 21) {
        passed += 1
    } else {
        println("fcSum6 failed")
    }
    if (unsafe { fcFAdd2(1.5, 2.25) } == 3.75) {
        passed += 1
    } else {
        println("fcFAdd2 failed")
    }
    if (unsafe { fcFSum8(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0) } == 36.0) {
        passed += 1
    } else {
        println("fcFSum8 failed")
    }
    if (unsafe { st10(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) } == 385) {
        passed += 1
    } else {
        println("st10 failed")
    }
    if (unsafe { sf12(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0) } == 650.0) {
        passed += 1
    } else {
        println("sf12 failed")
    }
    if (unsafe { stmix(1, 1.0, 2, 2.0, 3, 3.0, 4, 4.0, 5, 5.0, 6, 6.0, 7, 7.0, 8, 8.0) } == 408.0) {
        passed += 1
    } else {
        println("stmix failed")
    }
    if (unsafe { fcBytes(-100, 100, -1, 1, 50, -50, 2, -2, 7, -7, 12, -12, 13, -13, 20, -20, 27, -27, 33, -33) } == -63) {
        passed += 1
    } else {
        println("fcBytes failed")
    }
    if (unsafe { fcNarrowMix(200, -30000, 255, -1000000, -128, 65000, 1234567890123, 128, -12345, 123456789, -1, 250, 32000, -5, 255, -128, -7, 60000, 100, 201) } == 8643207525492) {
        passed += 1
    } else {
        println("fcNarrowMix failed")
    }
    if (unsafe { fcFloats32(0.5, -0.25, 1.5, -2.75, 3.25, 4.5, -5.5, 6.25, -7.75, 8.5, 9.75, -10.5) } == 44.75) {
        passed += 1
    } else {
        println("fcFloats32 failed")
    }
    if (unsafe { fcMixStack(10, 0.5, -20, -0.25, 30, 1.25, -40, -2.5, 50, 3.75, -60, -4.5, 70, 5.25, -80, -6.75, 90, 7.5, -100, -8.25, 110, 9.75, -120, -10.5) } == -1631.0) {
        passed += 1
    } else {
        println("fcMixStack failed")
    }
    println("passed ${passed}")
    if (passed == 11) {
        return 0
    }
    return 1
}