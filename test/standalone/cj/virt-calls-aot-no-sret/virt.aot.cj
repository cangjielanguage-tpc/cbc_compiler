package aot

public abstract class A {
  public open func foo(): Int64
  public open func foo2(): Int64 {
    println("A.foo2")
    2
  }
  public open func foo3(): Int64 {
    println("A.foo3")
    3
  }
}

public interface I {
  func bar(): Int64
  func bar2(): Int64 {
    println("I.bar2")
    2
  }
  func bar3(): Int64 {
    println("I.bar3")
    3
  }
}

public open class B <: A & I {
  public open func foo(): Int64 {
    println("B.foo")
    1
  }

  public open func foo3(): Int64 {
    println("B.foo3")
    3
  }

  public open func bar(): Int64 {
    println("B.bar")
    1
  }

  public open func bar3(): Int64 {
    println("B.bar3")
    3
  }
}
