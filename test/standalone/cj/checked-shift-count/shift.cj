@OverflowThrowing
public func left(a: Int64, count: Int32): Int64 {
    a << count
}

@OverflowThrowing
public func right(a: Int64, count: Int32): Int64 {
    a >> count
}

@OverflowThrowing
public func unsignedRight(a: UInt64, count: Int32): UInt64 {
    a >> count
}

@OverflowThrowing
public func smallLeft(a: Int8, count: Int32): Int8 {
    a << count
}

@OverflowThrowing
public func unsignedCount(a: Int64, count: UInt32): Int64 {
    a << count
}

main(): Int64 {
    println(left(12, 1))
    println(right(-12, 1))
    println(unsignedRight(UInt64.Max, 1))
    println(smallLeft(12, 1))
    try {
        unsignedRight(12, -1)
        println("fail")
    } catch (e: ArithmeticException) {
        println("negative count")
    }
    try {
        unsignedCount(12, UInt32.Max)
        println("fail")
    } catch (e: ArithmeticException) {
        println("large unsigned count")
    }
    0
}
