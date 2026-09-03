import std.collection.*

extend <T> ArrayQueue<?T> {
  func foo(x: T): ??T {
    println("Something something")
    this.peek()
  }
}

func baz<T>(x: T) {
  ArrayQueue<?T>().foo(x)
}

main() {
  baz("foo")
  0
}
