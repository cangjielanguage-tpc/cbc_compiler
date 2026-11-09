import hotfix.patchable

open class Super {
  let s: String

  @patchable
  init(s: String) {
    this.s = s + s
  }
}

class Foo <: Super {
  let i: Int64
  let j: Int64

  @patchable
  init(i: Int64) {
    this(i, 42)
  }

  @patchable
  init(i: Int64, j: Int64) {
    super("s=${i}{$j}")
    this.i = i
    this.j = j
  }
}
