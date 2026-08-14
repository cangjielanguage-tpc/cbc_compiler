package aot

public abstract class A<T> {
  public open func foo(x: T): T
  public open func foo2(x: T): T {
    println("A.foo2")
    x
  }
  public open func foo3(x: T): T {
    println("A.foo3")
    x
  }
}

public interface I<T> {
  func bar(x: T): T
  func bar2(x: T): T {
    println("I.bar2")
    x
  }
  func bar3(x: T): T {
    println("I.bar3")
    x
  }
}

public open class B <: A<Int64> & I<String> {
  public open func foo(x: Int64): Int64 {
    println("B.foo")
    x
  }

  public open func foo3(x: Int64): Int64 {
    println("B.foo3")
    x
  }

  public open func bar(x: String): String {
    println("B.bar")
    x
  }

  public open func bar3(x: String): String {
    println("B.bar3")
    x
  }
}
