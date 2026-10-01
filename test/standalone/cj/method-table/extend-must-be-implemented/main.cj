
interface I {
  func foo(): Unit
}

interface J {
  func foo(): Unit {}
}

class Impl <: I {
  public func foo(): Unit { // two different entries of method foo in I and J subtable.
    println("impl")
  }
}

extend Impl <: J {}

let o = Impl()
let i = o
let j = o

main() {
  i.foo()
  j.foo()
}
