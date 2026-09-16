import std.collection.*

interface I<T, K> {
  func foo(): String
}

class Foo<T> {
  public let num: Int64
  public init(num: Int64) { this.num = num }
}

class Pair<T, K> {}


extend<T, K> Foo<Pair<T, Pair<T, K>>> <: I<T, K> {
  public func foo(): String {
    return "${num}"
  }
}

main() {
  let list = ArrayList<Object>()
  list.add(Foo<Pair<Int64, Pair<Int64, Int32>>>(0))
  list.add(Foo<Pair<Int64, Pair<Int32, Int32>>>(1))
  list.add(Foo<Pair<Int32, Pair<Int32, Int32>>>(2))
  list.add(Foo<Pair<Int32, Pair<Int32, Int64>>>(3))
  list.add(Foo<Pair<Int32, Pair<Int64, Int64>>>(4))
  list.add(Foo<Pair<Int64, Pair<Int64, Int64>>>(5))

  for (obj in list) {
    match (obj) {
      case x: I<Int64, Int32> => println("i64, i32 ${x.foo()}")
      case x: I<Int32, Int64> => println("i32, i64 ${x.foo()}")
      case x: I<Int64, Int64> => println("i64, i64 ${x.foo()}")
      case x: I<Int32, Int32> => println("i32, i32 ${x.foo()}")
      case _ => println("not matched")
    }
  }
}
