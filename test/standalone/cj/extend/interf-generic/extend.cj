interface I<T> {
  func foo(a: T): Unit {
    println("I.foo")
  }
}

class A<T> {}

extend<T> A<T> <: I<T> {}

func fooInt(a: A<Int64>) {
  a.foo(1)
}

func fooStr(a: A<String>) {
  a.foo("foo")
}

func fooAInt(a: A<A<Int64>>) {
  a.foo(A())
}

main() {
  fooInt(A<Int64>())
  fooStr(A<String>())
  fooAInt(A<A<Int64>>())
  0
}

