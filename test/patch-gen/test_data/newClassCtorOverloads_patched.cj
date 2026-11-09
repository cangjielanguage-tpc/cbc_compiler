import hotfix.patchable

class Added {
  let result: Int64

  @patchable
  init(value: Int64) {
    this.result = value
  }

  @patchable
  init(left: Int64, right: Int64) {
    this.result = left + right
  }
}

@patchable
func makeValue(): Int64 {
  Added(40).result + Added(1, 1).result
}

@patchable
func value(): Int64 {
  makeValue()
}
