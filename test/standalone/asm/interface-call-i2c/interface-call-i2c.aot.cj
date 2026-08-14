package aot

public interface I {
    func foo(): Int64
}

public open class C <: I {

    public func getI(): I {
        println("getI")
        C()
    }

    public open func foo(): Int64 {
        println("c.foo")
        42
    }
}
