package aot

public func reportFailure(code: Int64): Int64 {
    println("Test failed with code: ${code}")
    return code
}
