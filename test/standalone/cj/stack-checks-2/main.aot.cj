package aot

public struct S {
    public var x: Object = Object()
    public var y: Int64 = 123
    public var z: Object = Object()
    public var v: Int64 = 321
}

public interface I<T> {
    func foo(): Option<T>
}

public class A<T> <: I<T> {
    public var f: T

    public init(f: T) {
        this.f = f
    }

    public func foo(): Option<T> {
        return f
    }
}

public var g_I: I<S> = A<S>(S())

public var res: S = S()

public func invokeI(): Unit {
    res = g_I.foo().getOrThrow()
    println(res.y)
    println(res.v)
    println("Invoked")
}
