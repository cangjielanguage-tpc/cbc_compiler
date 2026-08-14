public interface I<T> {
  func foo() {
    println("I")
  }
}

public open class C <: I<C> {

}

main() {
  C().foo()
  0
}
