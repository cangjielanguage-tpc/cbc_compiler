foreign func vsum(n: Int32, ...): Float64

foreign func vmix(first: Float64, k: Int32, ...): Int64

foreign func vints6(a: Int32, b: Int32, c: Int32, d: Int32, e: Int32, ...): Int32

main(): Int64 {
    var passed = 0

    if (unsafe { vsum(3, 1.0, 2.0, 3.0) } == 6.0) {
        passed += 1
    } else {
        println("vsum3 failed")
    }

    if (unsafe { vsum(8, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0) } == 36.0) {
        passed += 1
    } else {
        println("vsum8 failed")
    }

    let i1: Int32 = 1
    let i2: Int32 = 2
    if (unsafe { vmix(1.5, 2, i1, 1.5, i2, 2.5) } == 7) {
        passed += 1
    } else {
        println("vmix failed")
    }

    let i6: Int32 = 6
    let i7: Int32 = 7
    if (unsafe { vints6(1, 2, 3, 4, 5, i6, i7) } == 28) {
        passed += 1
    } else {
        println("vints6 failed")
    }

    println("passed ${passed}")
    if (passed == 4) {
        return 0
    }
    return 1
}