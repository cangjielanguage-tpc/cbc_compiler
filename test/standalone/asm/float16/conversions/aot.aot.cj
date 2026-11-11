package aot

public func reportFailure(number: Int64): Int64 {
    println("Failed assertion number: ${number}")
    return number
}
