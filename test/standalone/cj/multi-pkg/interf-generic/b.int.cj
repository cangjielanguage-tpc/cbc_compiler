package b

public interface B<T> {
  func foo() {
    println("B.foo")
    bar()
  }
  func bar() {
    println("B.bar")
  }
}

