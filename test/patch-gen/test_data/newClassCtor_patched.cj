import hotfix.patchable

class Added {
  let result: Int64

  @patchable
  init() {
    this.result = 42
  }
}

@patchable
func value(): Int64 {
  Added().result
}
