package default

func sizeOfGeneric<T>() : UIntNative where T <: CType {
    sizeOf<T>()
}

func alignOfGeneric<T>() : UIntNative where T <: CType {
    alignOf<T>()
}

func queryPointerLayout<T>() where T <: CType {
    // Open generic types with fixed size and alignment.
    println(sizeOf<CPointer<T>>())
    println(alignOf<CPointer<T>>())
    println(sizeOf<CPointer<CPointer<T>>>())
    println(alignOf<CPointer<CPointer<T>>>())
}

class Layout<T, U> where T <: CType, U <: CType {
    public init() {}

    public func sizeOfFirst(): UIntNative {
        sizeOf<T>()
    }

    public func alignOfSecond(): UIntNative {
        alignOf<U>()
    }

    public func mixed<V>(value: Int64): UIntNative where V <: CType {
        println(value)
        sizeOf<U>() + alignOf<V>()
    }
}

func sizeAfterCall<T, U>(a: Int64, b: Int64, c: Int64, d: Int64,
                       e: Int64, f: Int64, g: Int64, h: Int64): UIntNative
    where T <: CType, U <: CType {
    println(a + b + c + d + e + f + g + h)
    sizeOf<U>() + alignOf<T>()
}

main() {
    // Closed and generic queries through the public CType API.
    println(sizeOf<Int64>())
    println(alignOf<Int64>())
    println(sizeOf<Int32>())
    println(sizeOf<Bool>())
    println(alignOf<Float64>())
    println(sizeOf<Unit>())
    println(alignOf<Unit>())
    println(sizeOf<CPointer<Int64>>())
    // generic helpers resolve size/align of the runtime type argument
    println(sizeOfGeneric<Int64>())
    println(alignOfGeneric<Int64>())
    println(sizeOfGeneric<UInt8>())
    println(alignOfGeneric<Int32>())
    let layout = Layout<UInt8, Int64>()
    println(layout.sizeOfFirst())
    println(layout.alignOfSecond())
    println(layout.mixed<Int32>(17))
    println(sizeAfterCall<UInt8, Int64>(1, 2, 3, 4, 5, 6, 7, 8))
    queryPointerLayout<UInt8>()
    queryPointerLayout<Int64>()
    0
}
