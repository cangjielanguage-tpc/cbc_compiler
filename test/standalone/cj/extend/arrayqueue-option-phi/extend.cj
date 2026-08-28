import std.collection.*

extend <T> ArrayQueue<T> {
  func foo(x: T): ?T {
    match(this.peek()) {
      case Some(v) => v
      case None => x
    }
  }
}

func baz<T>(x: T) {
  ArrayQueue<T>().foo(x)
}

main() {
  println(baz("foo"))
  0
}
