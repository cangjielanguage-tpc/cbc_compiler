import hotfix.patchable

@patchable
func foo1(): Int64 {
  42
}

@patchable
func foo2(): S {
  S()
}

@patchable
func foo3(): C {
  C()
}

struct S {
}

class C {
}
