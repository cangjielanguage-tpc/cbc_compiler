
interface I {
  func foo(): Unit
}

interface J {
  func foo(): Unit
}

class Impl <: I & J {
  public func foo(): Unit { // two different entries of method foo in I and J subtable.
    println("impl")
  }
}

let o = Impl()
let i = Impl()
let j = Impl()

main() {
  i.foo()
  j.foo()
}
