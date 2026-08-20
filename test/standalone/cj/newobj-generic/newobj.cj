class Foo<T> {
  Foo(let x: T) {}
}

func foo<T>(x: T): Foo<T> {
  Foo(x)
}

main(): Int64 {
  println(foo<String>("test").x)
  println(foo<Int64>(123).x)
  0
}
