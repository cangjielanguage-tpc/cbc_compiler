import aot.*

public open class A {
  A(let name: String) {}
}

public open class B <: A {
  B(name: String) { super(name) }
}


main() {
  println(checkOptionalIs<A, Object>(None))
  println(checkOptionalIs<A, Object>(Some(Object())))
  println(checkOptionalIs<A, Object>(Some(A("foo"))))
  println(checkOptionalIs<A, Object>(Some(B("foo"))))

  println(checkOptionalIs<A, Any>(None))
  println(checkOptionalIs<A, Any>(Some(Object())))
  println(checkOptionalIs<A, Any>(Some(Some(A("foo")))))
  println(checkOptionalIs<A, Any>(Some(A("foo"))))
  println(checkOptionalIs<A, Any>(Some(B("foo"))))

  println(checkOptionalIs<Int64, Any>(None))
  println(checkOptionalIs<Int64, Any>(Some(Object())))
  println(checkOptionalIs<Int64, Any>(Some(A("foo"))))
  println(checkOptionalIs<Int64, Any>(Some(Some(42))))
  println(checkOptionalIs<Int64, Any>(Some(42)))

  println(checkOptionalIs<String, Any>(None))
  println(checkOptionalIs<String, Any>(Some(Object())))
  println(checkOptionalIs<String, Any>(Some(A("foo"))))
  println(checkOptionalIs<String, Any>(Some(Some("foo"))))
  println(checkOptionalIs<String, Any>(Some("foo")))
  0
}
