package aot

public open class A {
    public open func foo(): Int64 { 1 }
    public open func bar(): Int64 { 2 }
}

public open class B <: A {
    public open func foo(): Int64 { 1 }
    public open func bar(): Int64 { 2 }
}

public func qwerty(a: A): Int64 {
  println("${a.foo()} ${a.bar()}")
  0
}
