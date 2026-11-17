struct S {
}

struct B {
  public var s: S = S()
}

var b: B = B()

main() {
  b.s = S()  // <--- CRASH HERE
}