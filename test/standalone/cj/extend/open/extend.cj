public interface I {
  func foo(): Unit {
    println("I.foo")
  }
}

public open class C <: I {}

extend C {
  public func bar(): Unit {
    println("C.bar")
    foo()
  }
}

main() {
  C().bar()
  0
}
