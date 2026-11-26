import std.math.*

func check32(x: Float32, rounded: Float32, floored: Float32): Unit {
    println(round(x) == rounded && floor(x) == floored)
}

func check64(x: Float64, rounded: Float64, floored: Float64): Unit {
    println(round(x) == rounded && floor(x) == floored)
}

func check16(x: Float16, rounded: Float16, floored: Float16): Unit {
    println(round(x) == rounded && floor(x) == floored)
}

func check(x: Float64, rounded: Float64, floored: Float64): Unit {
    check32(Float32(x), Float32(rounded), Float32(floored))
    check64(x, rounded, floored)
    check16(Float16(x), Float16(rounded), Float16(floored))
}

main(): Int64 {
    // std.math.round uses ties to even, including negative halfway values.
    check(2.25, 2.0, 2.0)
    check(2.5, 2.0, 2.0)
    check(2.75, 3.0, 2.0)
    check(3.5, 4.0, 3.0)
    check(-2.25, -2.0, -3.0)
    check(-2.5, -2.0, -3.0)
    check(-2.75, -3.0, -3.0)
    check(-3.5, -4.0, -4.0)
    check(0.0, 0.0, 0.0)
    check(-0.25, 0.0, -1.0)
    check(10000.0, 10000.0, 10000.0)
    return 0
}
