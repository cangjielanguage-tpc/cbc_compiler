package a

public interface A<T> {
  func foo() {
    println("A.foo")
    bar()
  }
  func bar() {
    println("A.bar")
  }
}
