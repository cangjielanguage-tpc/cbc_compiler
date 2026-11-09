import hotfix.patchable

// TODO support ctors for generics structs properly
struct Foo {
  let i: Int64
  let j: Int64
  let str: String

  @patchable
  init(i: Int64) {
    this.i = i
    this.j = 0
    this.str = "${i}"
  }

  @patchable
  init(i: Int64, j: Int64) {
    this.i = i
    this.j = j
    this.str = "${i}${j}"
  }

  @patchable
  init(i: Int64, j: Int64, str: String) {
    this.i = i
    this.j = j
    this.str = str
  }

  func foo(): Foo {
    println(i)
    this
  }
}
