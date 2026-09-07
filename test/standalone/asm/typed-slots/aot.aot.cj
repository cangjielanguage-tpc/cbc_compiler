package aot

import std.convert.*

public struct Foo {
    public var i8_1: Int8 = 0
    public var i8_2: Int8 = 0
    public var i16: Int16 = 0
    public var i32: Int32 = 0
}

public func print(num: Int64): Int64 {
    println(((num >> 56) & 0xFF).format("#X"))
    println(((num >> 48) & 0xFF).format("#X"))
    println(((num >> 32) & 0xFFFF).format("#X"))
    println((num & 0xFFFFFFFF).format("#X"))
    return 0
}