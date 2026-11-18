import hotfix.patchable

// This function has no native body. Its guard must be removed before CBC
// merges the concrete closure with the result of a virtual guard call.
@patchable
func addedClosure(): (Int32) -> Unit {
  let closure: (Int32) -> Unit = { x: Int32 => x }
  closure
}

@patchable
func value(): Int64 {
  addedClosure()(1)
  42
}
