package aot

public var testRefVar:    ?Object = None
public var testStaticVar: Int64   = 0

public func testAssign(arg: Int64): Int64 {
    testStaticVar = testStaticVar + arg
    if (testRefVar.isSome()) {
        println("Reference is found")
    }
    println("Assigned: ${testStaticVar}")
    return 0
}
