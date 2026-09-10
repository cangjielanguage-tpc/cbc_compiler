package aot

public struct Foo<T> {
    public var x: T
    public var y: T

    init(x: T, y: T) {
        this.x = x
        this.y = y
    }
}

public class Bar<T> {
    public var x: T
    public var y: T

    init(x: T, y: T) {
        this.x = x
        this.y = y
    }
}

public func foo<T>(f: Foo<T>, b: Bar<T>): Int64 {
    b.x = f.x
    b.y = f.y
    0
}
