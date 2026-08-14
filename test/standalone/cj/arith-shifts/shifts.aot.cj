package aot

import std.convert.*

public func myPrintln(x: Int64) {
    println(x.format("#X"))
}

public func myPrintln(x: UInt32) {
    println(x.format("#X"))
}

public func logTest(intSize: Int32, signed: Bool) {
    let sign = if (signed) { "i" } else { "u" }
    println("test_${sign}${intSize}")
}

public func check(actual: UInt8, expected: UInt8) {
    if (actual != expected) {
        let actualHex = actual.format("#X")
        let expectedHex = expected.format("#X")
        println("check failed: ${actualHex} != ${expectedHex}")
    }
}

public func check(actual: UInt16, expected: UInt16) {
    if (actual != expected) {
        let actualHex = actual.format("#X")
        let expectedHex = expected.format("#X")
        println("check failed: ${actualHex} != ${expectedHex}")
    }
}

public func check(actual: UInt32, expected: UInt32) {
    if (actual != expected) {
        let actualHex = actual.format("#X")
        let expectedHex = expected.format("#X")
        println("check failed: ${actualHex} != ${expectedHex}")
    }
}

public func check(actual: UInt64, expected: UInt64) {
    if (actual != expected) {
        let actualHex = actual.format("#X")
        let expectedHex = expected.format("#X")
        println("check failed: ${actualHex} != ${expectedHex}")
    }
}

public func check(actual: Int8, expected: Int8) {
    if (actual != expected) {
        let actualHex = actual.format("#X")
        let expectedHex = expected.format("#X")
        println("check failed: ${actualHex} != ${expectedHex}")
    }
}

public func check(actual: Int16, expected: Int16) {
    if (actual != expected) {
        let actualHex = actual.format("#X")
        let expectedHex = expected.format("#X")
        println("check failed: ${actualHex} != ${expectedHex}")
    }
}

public func check(actual: Int32, expected: Int32) {
    if (actual != expected) {
        let actualHex = actual.format("#X")
        let expectedHex = expected.format("#X")
        println("check failed: ${actualHex} != ${expectedHex}")
    }
}

public func check(actual: Int64, expected: Int64) {
    if (actual != expected) {
        let actualHex = actual.format("#X")
        let expectedHex = expected.format("#X")
        println("check failed: ${actualHex} != ${expectedHex}")
    }
}