struct S<T> {

  S(let x: T) {}

  mut func foo() {
    println("S.foo")
  }

  mut func bar() {
    foo()
  }
}

main() {
  var s1 = S(42)
  s1.bar()
  var s2 = S("test")
  s2.bar()
}
