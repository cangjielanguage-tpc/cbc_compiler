class A {
  A(let value: Int64) { }
}

extend A <: Countable<A> {
  public func next(r: Int64): A { A(this.value + r) }
  public func position(): Int64 { this.value }
}

extend A <: Comparable<A> {
  public func compare(rhs: A): Ordering {
    match { case _ => Ordering.EQ }
  }
}

main() {
  var range: Range<A> = A(1)..A(10)
  for (i in range) {
    println(i.value)
  }
}
