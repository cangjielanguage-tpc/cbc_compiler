var one = S(Object(), 1)

struct S {
  S(let x: Object, var id: Int64) {}

  mut func foo(id: Int64) {
    this.id = id
  }

  mut func bar(id: Int64) {
    foo(id)
  }
}


class C {
  C(var s: S) {}
}

main() {
  println(one.id)
  one.foo(2)
  println(one.id)

  var two = S(Object(), 3)
  println(two.id)
  two.foo(2)
  println(two.id)
  two.bar(2)
  println(two.id)

  var obj = C(S(Object(), 4))
  println(obj.s.id)
  obj.s.bar(2)
  println(obj.s.id)

  one.id + two.id + obj.s.id
}
