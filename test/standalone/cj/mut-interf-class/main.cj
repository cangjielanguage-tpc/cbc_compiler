interface I {
  mut func foo(): Unit
}

struct S <: I {
  override public mut func foo(): Unit {
    println("S.foo")
  }
}

class C<T> where T <: I {
  C(var t: T) { }
}

func baz<T>(c: C<T>) where T <: I {
  c.t.foo()
}

main() {
  baz(C<S>(S()))
}
