import aot

class C {
  C(var s: aot.S) {}
}

main() {
  println(aot.one.id)
  aot.one.foo(2)
  println(aot.one.id)

  var two = aot.S(Object(), 3)
  println(two.id)
  two.foo(2)
  println(two.id)
  two.bar(2)
  println(two.id)

  var obj = C(aot.S(Object(), 4))
  println(obj.s.id)
  obj.s.bar(2)
  println(obj.s.id)

  aot.one.id + two.id + obj.s.id
}
