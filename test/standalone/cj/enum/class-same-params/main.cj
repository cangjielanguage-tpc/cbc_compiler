enum E {
    A(String)
  | B(String)
}

func foo(x: E): Unit {
  match(x) {
    case A(s) => println("A(${s})")
    case B(s) => println("B(${s})")
  }
}

main() {
  foo(A("foo"))
  foo(B("bar"))
}
