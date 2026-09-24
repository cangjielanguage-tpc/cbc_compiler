struct Data<T> where T <: ToString {
  Data(let a: T) {}

  public func show(): Unit {
    println(a)
  }
}

main() {
  let data: Data<Int64> = Data(42)
  data.show()
  0
}
