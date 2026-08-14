package aot

public open class A {
    public func getA(): A {
        println("getA")
        C()
    }

    public open func foo():Int64 {
        println("a.foo")
        37
    }
}

public open class C <: A {
    public open func foo(): Int64 {
        println("c.foo")
        42
    }
}
