import hotfix.patchable

@patchable
func foo(): String {
    bar()
    "patched"
}

@patchable
func bar(): Unit {
  println("hello")
}
