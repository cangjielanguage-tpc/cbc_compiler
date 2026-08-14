import aot

main() {
  var obj = aot.C<String, Int64>("foo", 37, 42)

  println(obj.x)
  println(obj.y)
  println(obj.z)

  println(obj.getX())
  println(obj.getY())
  println(obj.getZ())
  0
}
