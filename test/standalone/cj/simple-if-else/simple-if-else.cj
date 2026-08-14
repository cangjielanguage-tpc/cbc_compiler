func foo(a: Int64): Int64 {
  if (a > 0 && a < 2) {
    a
  } else {
    -a
  }
}

main(): Int64 {
  foo(1) + foo(0) + foo(-1)
}
