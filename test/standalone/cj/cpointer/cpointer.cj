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

    let snapshot = structPointer.read()
    structPointer.write(TestStruct(a: 10, b: 11))
    println(snapshot.a)
    println(snapshot.b)
    value = structPointer.read()
    println(value.a)
    println(value.b)

    let primitivePointer = LibC.malloc<Int64>(count: 2)
    primitivePointer.write(42)
    primitivePointer.write(1, 84)
    println(primitivePointer.read())
    println(primitivePointer.read(1))

    let recordPointer = LibC.malloc<TestStruct>(count: 3)
    recordPointer.write(0, TestStruct(a: 1, b: 2))
    recordPointer.write(1, TestStruct(a: 3, b: 4))
    recordPointer.write(2, TestStruct(a: 5, b: 6))
    println(recordPointer.read(0).a)
    println(recordPointer.read(1).b)
    println(recordPointer.read(2).a)

    primitivePointer.write(45)
    println(primitivePointer.read())
    println(primitivePointer.read(1))

    LibC.free<TestStruct>(structPointer)
    LibC.free<Int64>(primitivePointer)
    LibC.free(recordPointer)
    return 0
}
