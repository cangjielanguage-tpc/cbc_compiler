import hotfix.patchable

@patchable
func foo1(): Int64 {
  bar1()
}

@patchable
func bar1(): Int64 {
  42
}

@patchable
func foo2(): S {
  bar2()
}

@patchable
func bar2(): S {
  S()
}

@patchable
func foo3(): C {
  bar3()
}

@patchable
func bar3(): C {
  C()
}

struct S {
}

class C {
}