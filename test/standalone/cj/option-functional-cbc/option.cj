func apply(optional: Option<(Int64) -> Int64>, value: Int64): Int64 {
  match (optional) {
    case Some(f) => f(value)
    case None => -1
  }
}

main() {
  println(apply(Some({ value => value + 1 }), 41))
  println(apply(None, 41))
  0
}
