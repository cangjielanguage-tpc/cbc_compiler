package aot

public class Foo<T> {
    public var y: T
    public var x: T

    init(x: T, y: T) {
        this.x = x
        this.y = y
    }
}

public func foo<T>(x: T, y: T): Foo<T> {
    Foo(x, y)
}
