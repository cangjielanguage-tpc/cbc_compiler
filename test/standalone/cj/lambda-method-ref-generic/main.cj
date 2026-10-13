class C<M> {
}

func test2<M>(x: C<M>): Unit {
  println("test2")
}

interface I2<M> {
}

func f2() {
  test2<I2<Int64>>
}

main() {
  let ff = f2()
  ff(C<I2<Int64>>())
}
