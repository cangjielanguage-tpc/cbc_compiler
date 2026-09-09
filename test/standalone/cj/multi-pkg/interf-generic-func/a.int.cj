package a

public interface A<T> {
  func foo<S>() {
    println("A.foo")
    bar<S>()
  }
  func bar<S>() {
    println("A.bar")
  }
}
