package default

func sizeOfGeneric<T>() : UIntNative where T <: CType {
    sizeOf<T>()
}

func alignOfGeneric<T>() : UIntNative where T <: CType {
    alignOf<T>()
}

main() {
    // concrete types: emitted as size/align ops; the engine rewriter folds
    // closed layouts at image preparation
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
    // string equality: std.core implements it via the vector byte compare helper
    var s1 = "hello world, hello cangjie, long enough string!"
    var s2 = "hello world, hello cangjie, long enough string?"
    println(s1 == s1)
    println(s1 == s2)
    0
}
