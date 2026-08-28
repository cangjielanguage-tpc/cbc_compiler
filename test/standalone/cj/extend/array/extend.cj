extend <T> Array<T> {
  func foo(x: T) {
    this.size
  }
}

func baz<T>(x: T): Int64 {
  let arr: Array<T> = [x, x]
  arr.foo(x)
}

main() {
  println(baz("foo"))
  0
}
