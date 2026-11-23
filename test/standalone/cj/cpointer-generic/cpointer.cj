@C
struct Padded {
    public var tag: UInt8 = 0
    public var value: Int64 = 0

    public init(tag: UInt8, value: Int64) {
        this.tag = tag
        this.value = value
    }
}

@C
struct Narrow {
    public var first: Int16 = 0
    public var second: Int16 = 0
    public var last: UInt8 = 0

    public init(first: Int16, second: Int16, last: UInt8) {
        this.first = first
        this.second = second
        this.last = last
    }
}

unsafe func byteStride<T>(p: CPointer<T>, n: Int64): Int64 where T <: CType {
    return Int64((p + n).toUIntNative()) - Int64(p.toUIntNative())
}

unsafe func indexedRead<T>(p: CPointer<T>, index: Int64): T where T <: CType {
    p.read(index)
}

unsafe func indexedWrite<T>(p: CPointer<T>, index: Int64, value: T): Unit where T <: CType {
    p.write(index, value)
}

unsafe func replace<T>(p: CPointer<T>, index: Int64, value: T): T where T <: CType {
    let old = p.read(index)
    p.write(index, value)
    old
}

unsafe main(): Int64 {
    let integers = LibC.malloc<Int64>(count: 3)
    indexedWrite(integers, 0, 11)
    indexedWrite(integers, 1, 22)
    indexedWrite(integers, 2, 33)
    println(indexedRead(integers, 0))
    println(indexedRead(integers, 2))
    println(replace(integers, 1, 44))
    println(integers.read(1))
    println(byteStride(integers, 2))
    println(byteStride(integers + 2, -2))
    println(indexedRead(integers + 2, -1))
    println(byteStride(integers, 0))

    let bytes = LibC.malloc<UInt8>(count: 4)
    indexedWrite(bytes, 0, 129u8)
    indexedWrite(bytes, 3, 255u8)
    println(indexedRead(bytes, 0))
    println(indexedRead(bytes, 3))
    println(byteStride(bytes, 3))

    let floats = LibC.malloc<Float64>(count: 2)
    indexedWrite(floats, 0, 1.5)
    indexedWrite(floats, 1, -2.25)
    println(indexedRead(floats, 0))
    println(indexedRead(floats, 1))

    let padded = LibC.malloc<Padded>(count: 3)
    indexedWrite(padded, 0, Padded(1, 101))
    indexedWrite(padded, 1, Padded(2, 202))
    indexedWrite(padded, 2, Padded(3, 303))
    let previous = replace(padded, 1, Padded(4, 404))
    println(previous.tag)
    println(previous.value)
    println(indexedRead(padded, 1).value)
    println(indexedRead(padded, 2).value)
    println(byteStride(padded, 2))
    println(byteStride(padded + 2, -1))

    let narrow = LibC.malloc<Narrow>(count: 2)
    indexedWrite(narrow, 0, Narrow(10, 20, 30))
    indexedWrite(narrow, 1, Narrow(40, 50, 60))
    let item = indexedRead(narrow, 1)
    println(item.first)
    println(item.second)
    println(item.last)
    println(byteStride(narrow, 1))

    let pointers = LibC.malloc<CPointer<Int64>>(count: 2)
    indexedWrite(pointers, 0, integers)
    indexedWrite(pointers, 1, integers + 2)
    println(indexedRead(pointers, 0).read())
    println(indexedRead(pointers, 1).read())
    println(byteStride(pointers, 1))
    println(indexedRead(pointers, 1).toUIntNative() == (integers + 2).toUIntNative())
    println(CPointer<Int64>().toUIntNative())

    LibC.free(integers)
    LibC.free(bytes)
    LibC.free(floats)
    LibC.free(padded)
    LibC.free(narrow)
    LibC.free(pointers)
    return 0
}
