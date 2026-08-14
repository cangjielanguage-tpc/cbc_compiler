func foo(i: Int64): Int64 {
  i + bar(i * 2)
}

func bar(i: Int64): Int64 {
  i + baz(i * 3)
}

func baz(i: Int64): Int64 {
  i * 4
}

main(): Int64 {
  foo(1)
}
