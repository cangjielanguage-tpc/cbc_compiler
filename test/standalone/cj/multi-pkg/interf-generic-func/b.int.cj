package b

public interface B<T> {
  func foo<S>() {
    println("B.foo")
    bar<S>()
  }
  func bar<S>() {
    println("B.bar")
  }
}

