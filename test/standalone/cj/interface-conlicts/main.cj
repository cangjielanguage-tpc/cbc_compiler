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
extend S <: A & B {}

main(): Int64 {
  let b: B = S()
  b.foo() // <--- CRASHES HERE
}

