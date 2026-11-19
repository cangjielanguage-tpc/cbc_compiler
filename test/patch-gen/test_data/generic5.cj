import hotfix.patchable

main() {
  let ints = Box<Int64>(42)
  let strings = Box<String>("class")
  ints.printMixed<Int64>(43)
  ints.printMixed<String>("method")
  strings.printMixed<String>("same")
  strings.printMixed<Int64>(44)
  ints.printThree<String, Bool>("third", true)
  strings.printThree<Int64, String>(45, "last")
}

class Box<T> where T <: ToString {
  Box(let value: T) {}

  @patchable
  func printMixed<U>(other: U): Unit where U <: ToString {
    println("${value.toString()} ${other.toString()}")
  }

  @patchable
  func printThree<U, V>(other: U, last: V): Unit where U <: ToString, V <: ToString {
    println("${value.toString()} ${other.toString()} ${last.toString()}")
  }
}
