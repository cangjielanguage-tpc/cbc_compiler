import hotfix.patchable

main() {
  Foo.printPair<Int64, String>(44, "static")
  Foo.printPair<String, Int64>("static", 45)
}

class Foo {
  @patchable
  static func printPair<T, U>(first: T, second: U): Unit where T <: ToString, U <: ToString {
    println("patched ${first.toString()} ${second.toString()}")
  }
}
