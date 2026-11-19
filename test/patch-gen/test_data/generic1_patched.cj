import hotfix.patchable

main() {
  printValue<Int64>(42)
  printValue<String>("global")
}

@patchable
func printValue<T>(value: T): Unit where T <: ToString {
  println("patched ${value.toString()}")
}
