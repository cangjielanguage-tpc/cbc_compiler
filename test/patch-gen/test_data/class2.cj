import hotfix.patchable
import std.collection.*
import std.sync.SyncCounter

class FooClass {
    @patchable
    func foo() {
        bar()
    }

    func bar() {}
}
