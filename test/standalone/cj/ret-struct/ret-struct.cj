main(): Int64 {
  let s = S(42, 1)
  s.i + s.j
}

struct S {
  S(let i: Int64, let j: Int64) {}
}
