import aot

main() {
  var obj1 = aot.Foo()
  var obj2 = obj1
  obj1 = aot.Foo()
  obj2.foo = Some(obj1)
  obj1 = obj2

  obj1.x = 12
  obj1.y = 13
  obj1.z = 14

  var s = obj1.x + obj1.y + obj1.z

  s + aot.testSum(obj1)
}
