import hotfix.patchable

main() {
  let foo = Foo()
  foo.printValue<Int64>(43)
  foo.printValue<String>("method")
}

class Foo {
  @patchable
  func printValue<T>(value: T): Unit where T <: ToString {
    println("patched ${value.toString()}")
  }
}
