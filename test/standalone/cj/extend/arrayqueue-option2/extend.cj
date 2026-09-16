import std.collection.*

extend <T> ArrayQueue<?T> {
  func foo(x: T): ??T {
    this.peek()
  }
}

func baz<T>(x: T) {
  ArrayQueue<?T>().foo(x)
}

main() {
  println(baz("foo"))
}
