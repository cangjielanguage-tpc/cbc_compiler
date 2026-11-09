package p

func foo(x: MyEnum) {
    match(x) {
      case A =>
        println("A")
      case B =>
        println("B")
      case C =>
        println("C")
    }
}

enum MyEnum {
  A | B | C
}