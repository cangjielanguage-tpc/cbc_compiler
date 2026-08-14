package aot

public func test4_helper2(x: Int64, y: Int64): Int64 {
    if (x > y + 1) {
        throw IllegalArgumentException()
    }
    return 42
}