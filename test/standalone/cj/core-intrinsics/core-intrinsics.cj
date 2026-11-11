package default

func sizeOfGeneric<T>() : UIntNative where T <: CType {
    sizeOf<T>()
}

func alignOfGeneric<T>() : UIntNative where T <: CType {
    alignOf<T>()
}

main() {
    // concrete types: values come from std.core (native here)
    println(sizeOf<Int64>())
    println(alignOf<Int64>())
    println(sizeOf<Int32>())
    println(sizeOf<Bool>())
    println(alignOf<Float64>())
    println(sizeOf<Unit>())
    println(sizeOf<CPointer<Int64>>())
    // generic type argument path
    println(sizeOfGeneric<Int64>())
    println(alignOfGeneric<Int64>())
    println(sizeOfGeneric<UInt8>())
    println(alignOfGeneric<Int32>())
    // string comparisons (CBC std.core implements them via the vector byte ops; native here)
    var s1 = "hello world, hello cangjie, long enough string!"
    var s2 = "hello world, hello cangjie, long enough string?"
    println(s1 == s1)
    println(s1 == s2)
    0
}
