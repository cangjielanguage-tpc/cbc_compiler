import aot

func foo(i: Int64): Int64 {
  i + Int64(aot.bar(i))
}

main() {
  foo(5)
}
