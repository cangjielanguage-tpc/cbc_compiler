@C
struct TestStruct {
    public var a: Int64 = 0
    public var b: Int64 = 0

    public init(a!: Int64, b!: Int64) {
        this.a = a
        this.b = b
    }
}

unsafe func byteStride<T>(p: CPointer<T>, n: Int64): UIntNative where T <: CType {
    return (p + n).toUIntNative() - p.toUIntNative()
}

unsafe main(): Int64 {
    let ip = LibC.malloc<Int64>(count: 4)
    println(byteStride(ip, 2))
    let bp: CPointer<UInt8> = CPointer<UInt8>(ip)
    println(byteStride(bp, 3))
    let sp = LibC.malloc<TestStruct>(count: 2)
    println(byteStride(sp, 1))
    let cp = LibC.malloc<UInt8>(count: 4)
    println(byteStride(cp, 2))
    println(byteStride(cp, 0))
    LibC.free<Int64>(ip)
    LibC.free<TestStruct>(sp)
    LibC.free<UInt8>(cp)
    return 0
}
