import hotfix.patchable

@patchable
func foo(): String {
    bar()
    "source"
}

@patchable
func bar(): Unit {
  println("hello")
}
