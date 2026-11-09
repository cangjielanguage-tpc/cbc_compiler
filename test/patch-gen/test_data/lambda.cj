import hotfix.patchable

@patchable
func foo() {
    // TODO make lambda patchable as well, if the function is patchable?
    // TODO otherwise "non-patchable function $lambda must stay unchanged in patched version" happens
    let add = { a: Int64, b: Int64 => a + b }
    println(add(1, 2))
}

func bar() {
  let sub = { a: Int64, b: Int64 => a - b }
}
