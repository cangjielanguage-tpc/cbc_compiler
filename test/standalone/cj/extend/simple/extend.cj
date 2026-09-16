
interface I {
  func foo(): Unit
}

class Bar {

}

class Baz {

}

extend Bar <: I {
  public func foo(): Unit {
    println("bar")
  }
}

extend Baz <: I {
  public func foo(): Unit {
    println("baz")
  }
}

var bar: I = Bar()
var baz: I = Baz()

main() {
  bar.foo()
  baz.foo()
}

