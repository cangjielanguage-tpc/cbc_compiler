main(): Int64 {
  let s = foo()
  s.i + s.j
}

func foo(): S {
  S(42, 1)
}

struct S {
  S(let i: Int64, let j: Int64) {}
}
