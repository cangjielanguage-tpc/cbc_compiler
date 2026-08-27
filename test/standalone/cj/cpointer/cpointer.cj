@C
struct TestStruct {
    public var a: Int64 = 0
    public var b: Int64 = 0

    public init(a!: Int64, b!: Int64) {
        this.a = a
        this.b = b
    }
}

foreign {
    func createStruct(a: Int64, b: Int64): CPointer<TestStruct>
    func createPrim(value: Int64): CPointer<Int64>
}

unsafe main(): Int64 {
    let structPointer = createStruct(43, 44)
    var value = structPointer.read()
    println(value.a)
    println(value.b)

    structPointer.write(TestStruct(a: 10, b: 11))
    value = structPointer.read()
    println(value.a)
    println(value.b)

    let primitivePointer = createPrim(42)
    println(primitivePointer.read())
    println(primitivePointer.read(1))

    primitivePointer.write(45)
    println(primitivePointer.read())
    println(primitivePointer.read(1))

    LibC.free<TestStruct>(structPointer)
    LibC.free<Int64>(primitivePointer)
    return 0
}
