package aot

public abstract class A {
  public open func foo(): Unit
  public open func foo2(): Unit {
    println("A.foo2")
  }
  public open func foo3(): Unit {
    println("A.foo3")
  }
}

public interface I {
  func bar(): Unit
  func bar2(): Unit {
    println("I.bar2")
  }
  func bar3(): Unit {
    println("I.bar3")
  }
}

public open class B <: A & I {
  public open func foo(): Unit {
    println("B.foo")
  }

  public open func foo3(): Unit {
    println("B.foo3")
  }

  public open func bar(): Unit {
    println("B.bar")
  }

  public open func bar3(): Unit {
    println("B.bar3")
  }
}
