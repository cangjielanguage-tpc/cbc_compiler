class Foo<T> {
  Foo(let x: T) {}
}

func foo<T>(x: T): Foo<T> {
  Foo(x)
}

main() {
  println(foo<String>("test").x)
  println(foo<Int64>(123).x)
}
