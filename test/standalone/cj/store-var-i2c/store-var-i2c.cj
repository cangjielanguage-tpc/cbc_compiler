import aot

func update(a: Int64): Int64 {
  aot.i = a
  aot.i
}

main(): Int64 {
  aot.i + update(2) + update(3)
}
