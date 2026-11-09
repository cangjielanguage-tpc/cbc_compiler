import hotfix.patchable

class FooClass {
  let a = 42
}

let x = 1

@patchable
func foo(f: FooClass): String {
    "source"
}

func bar() {}