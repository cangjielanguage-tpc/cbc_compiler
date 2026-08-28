@C
struct TestStruct {
    public var a: Int64 = 0
    public var b: Int64 = 0

    public init(a!: Int64, b!: Int64) {
        this.a = a
        this.b = b
    }
}

unsafe main(): Int64 {
    let structPointer = LibC.malloc<TestStruct>(count: 1)
    structPointer.write(TestStruct(a: 43, b: 44))
    var value = structPointer.read()
    println(value.a)
    println(value.b)

    structPointer.write(TestStruct(a: 10, b: 11))
    value = structPointer.read()
    println(value.a)
    println(value.b)

    let primitivePointer = LibC.malloc<Int64>(count: 2)
    primitivePointer.write(42)
    primitivePointer.write(1, 84)
    println(primitivePointer.read())
    println(primitivePointer.read(1))

    primitivePointer.write(45)
    println(primitivePointer.read())
    println(primitivePointer.read(1))

    LibC.free<TestStruct>(structPointer)
    LibC.free<Int64>(primitivePointer)
    return 0
}
