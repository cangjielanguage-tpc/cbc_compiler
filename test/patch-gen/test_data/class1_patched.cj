package p.p1

import hotfix.patchable

class FooClass {
    @patchable
    func foo() {
        let a = 1;
        bar()
    }

    private func bar() {
      baz()
    }

    private func baz() {
      fizz()
      buzz()
    }

    private func fizz() {
    }

    private func buzz() {
    }
}

abstract class BarClass {
  protected func baz(): Unit
}