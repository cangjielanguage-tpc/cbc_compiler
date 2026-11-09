import hotfix.patchable

class FooClass {
  let a = 42
}

let x = 1

@patchable
func foo(f: FooClass): String {
    FooClass()
    bar()
    "patched ${f.a}, ${x}"
}

func bar() {}