import hotfix.patchable

// TODO support ctors for generics structs properly
struct Foo {
  let i: Int64
  let j: Int64
  let str: String

  @patchable
  init(i: Int64) {
    this(i, 42)
    foo()
  }

  @patchable
  init(i: Int64, j: Int64) {
    this(i, j, "${i}{j}")
    foo()
  }

  @patchable
  init(i: Int64, j: Int64, str: String) {
    if (str.size > 5) {
      this.i = max(i, j)
      this.j = min(i, j)
      this.str = str + str
    } else {
      this.i = min(i, j)
      this.j = max(i, j)
      this.str = ""
    }
  }

  func foo(): Foo {
    println(i)
    this
  }
}
