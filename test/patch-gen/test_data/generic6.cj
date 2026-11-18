import hotfix.patchable

main() {
  let ints = First<Int64>(42)
  let strings = First<String>("first")
  let mixed = Second<String, Int64>("second", 43)
  let reversed = Second<Int64, String>(44, "reversed")
  ints.printMixed<String>("method")
  strings.printMixed<Int64>(45)
  mixed.printMixed<Bool>(true)
  reversed.printMixed<String>("local")
}

class First<T> where T <: ToString {
  First(let value: T) {}

  @patchable
  func printMixed<U>(other: U): Unit where U <: ToString {
    println("${value.toString()} ${other.toString()}")
  }
}

class Second<A, B> where A <: ToString, B <: ToString {
  Second(let first: A, let second: B) {}

  @patchable
  func printMixed<T>(other: T): Unit where T <: ToString {
    println("${first.toString()} ${second.toString()} ${other.toString()}")
  }
}
