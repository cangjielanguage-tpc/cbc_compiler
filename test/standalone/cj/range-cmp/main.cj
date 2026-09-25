func foo(a: Range<Int64>, b: Range<Int64>) {
  a == b
}

main() {
  println(foo((0..1), (0..1)))
  println(foo((0..2), (0..1)))
  println(foo((0..2), (0..=2)))
  println(foo((0..=2), (0..=2)))
}
