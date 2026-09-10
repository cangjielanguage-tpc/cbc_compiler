package b

public interface B {
  func foo() {
    println("B.foo")
    bar()
  }
  func bar() {
    println("B.bar")
  }
}

