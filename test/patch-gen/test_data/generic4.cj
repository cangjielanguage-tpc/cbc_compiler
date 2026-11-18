import hotfix.patchable

main() {
  let ints = Box<Int64>(42)
  let strings = Box<String>("class")
  ints.printValue()
  ints.printSame(43)
  strings.printValue()
  strings.printSame("same")
}

class Box<T> where T <: ToString {
  Box(let value: T) {}

  @patchable
  func printValue(): Unit {
    println(value.toString())
  }

  @patchable
  func printSame(other: T): Unit {
    println("${value.toString()} ${other.toString()}")
  }
}
