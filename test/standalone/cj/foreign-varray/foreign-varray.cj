@C
struct CStruct {
    var c: Int8 = 1
    var g: VArray<Int32, $3> = [111, 222, 333]
}

foreign func f(s: CStruct): Int32

foreign func size_of_CStruct(): UIntNative

foreign func sumI32x3(a: VArray<Int32, $3>): Int64
foreign func sumF32x3(a: VArray<Float32, $3>): Float64
foreign func fillI32x3(a: VArray<Int32, $3>): Unit

main() {
    var s = CStruct()
    unsafe {
        f(s)
    }

    var i32x3: VArray<Int32, $3>   = [111, 222, 333]
    var f32x3: VArray<Float32, $3> = [1.0f32, 2.0f32, 3.0f32]
    var fill:  VArray<Int32, $3>   = [0, 0, 0]

    println("sumI32x3=${unsafe { sumI32x3(inout i32x3) }}")
    println("sumF32x3=${unsafe { sumF32x3(inout f32x3) }}")
    unsafe { fillI32x3(inout fill) }
    println("fill=${fill[0]} ${fill[1]} ${fill[2]}")

    if (unsafe { size_of_CStruct() } == sizeOf<CStruct>()) {
        return 0
    }
    return 1
}
