foreign func fcAdd2(a: Int64, b: Int64): Int64

foreign func fcSum6(a: Int32, b: Int32, c: Int32, d: Int32, e: Int32, f: Int32): Int32

foreign func fcFAdd2(a: Float64, b: Float64): Float64

foreign func fcFSum8(a: Float64, b: Float64, c: Float64, d: Float64, e: Float64, f: Float64, g: Float64, h: Float64): Float64

foreign func st10(a1: Int64, a2: Int64, a3: Int64, a4: Int64, a5: Int64, a6: Int64, a7: Int64, a8: Int64, a9: Int64, a10: Int64): Int64

foreign func sf12(a1: Float64, a2: Float64, a3: Float64, a4: Float64, a5: Float64, a6: Float64, a7: Float64, a8: Float64, a9: Float64, a10: Float64, a11: Float64, a12: Float64): Float64

foreign func stmix(x1: Int64, d1: Float64, x2: Int64, d2: Float64, x3: Int64, d3: Float64, x4: Int64, d4: Float64, x5: Int64, d5: Float64, x6: Int64, d6: Float64, x7: Int64, d7: Float64, x8: Int64, d8: Float64): Float64

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
    println("passed ${passed}")
    if (passed == 7) {
        return 0
    }
    return 1
}