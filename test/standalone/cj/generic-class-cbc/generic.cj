
public open class C<T, S> {
  public C(public let x: T, public let y: Int8, public let z: S) {}

  public func getX(): T { x }
  public func getY(): Int8 { y }
  public func getZ(): S { z }
}

main() {
  var obj = C<String, Int64>("foo", 37, 42)

  println(obj.x)
  println(obj.y)
  println(obj.z)

  println(obj.getX())
  println(obj.getY())
  println(obj.getZ())
  0
}
