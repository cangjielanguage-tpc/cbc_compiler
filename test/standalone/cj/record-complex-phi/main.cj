class C6 <: Resource {
  public func isClosed(): Bool { true }
  public func close() { () }
}

func f(): (Int64, Int64) {
  let t = (0, 0)
  try (r1 = C6()) { try (r2 = C6()) { () } }
  t
}

main() { }
