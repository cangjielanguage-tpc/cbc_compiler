interface A {
  func foo(): Int64 {
    println("foo")
    0
  }
}
interface B {
  func foo(): Int64
}
class S {}
extend S <: A & B {} // the default must be propagated from A to B

let b: B = S()
main(): Int64 {
  b.foo()
}

