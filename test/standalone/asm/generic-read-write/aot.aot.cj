package aot

public abstract class Foo<T> {
    var field: T
    init(x: T) { this.field = x }

    public open func run(x: T): Int32 { 11 }
}

public func invoke<T>(f: Foo<T>, x: T): Int32 {
    f.run(x)
}
