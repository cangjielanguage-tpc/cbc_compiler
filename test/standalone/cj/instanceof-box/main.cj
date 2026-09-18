open class A {
  public open func print() {
    println("A.print")
  }
}

class B <: A {
  public override func print() {
    println("B.print")
  }
}

func foo(b: Array<A>) {
  let c = b as Array<B>
  match(c) {
    case Some(c) =>
      for (x in c) {
        x.print()
      }
    case None =>
      for (x in b) {
        x.print()
      }
  }
}

main() {
  foo([B(), B()])
  foo([A(), B()])
  foo([A(), A()])
}
