public struct S<T, U> {
  S(let i: T, let j: U) {}
}

public func makeS<T, U>(x: T, y: U): S<T, U> {
  S(x, y)
}

public class C {
  func print(): Unit {
    println("C")
  }
}

func test1() {
  let s = makeS<Int64, Float64>(42, 3.5)
  println(s.i)
  println(s.j)
}

func test2() {
  let s = makeS<String, Int64>("foo", 37)
  println(s.i)
  println(s.j)
}

func test3() {
  let s = makeS<C, String>(C(), "bar")
  s.i.print()
  println(s.j)
}

main(): Int64 {
  test1()
  test2()
  test3()
  0
}
