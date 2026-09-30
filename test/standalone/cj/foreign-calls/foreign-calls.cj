foreign func fcAdd2(a: Int64, b: Int64): Int64

foreign func fcSum6(a: Int32, b: Int32, c: Int32, d: Int32, e: Int32, f: Int32): Int32

foreign func fcFAdd2(a: Float64, b: Float64): Float64

foreign func fcFSum8(a: Float64, b: Float64, c: Float64, d: Float64, e: Float64, f: Float64, g: Float64, h: Float64): Float64

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
    println("passed ${passed}")
    if (passed == 4) {
        return 0
    }
    return 1
}