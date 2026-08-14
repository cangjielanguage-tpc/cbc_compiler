package aot

public class Foo {
    public var x: Int64 = 0
    public var y: Int64 = 0
    public var z: Int64 = 0
    public var foo: ?Foo = None
}

public func testSum(obj: Foo): Int64 {
    if (obj.foo.isSome()) {
        println("Reference is found")
    }
    return obj.x + obj.y + obj.z
}
