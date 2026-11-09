import hotfix.patchable
import std.collection.*
import std.sync.SyncCounter

let items = 100
var threads = 200

class FooClass {
    @patchable
    func foo() {
        let a = 1;
        let itemNum = items
        let threadNum = threads
        let _ = VArray<Int64, $10000>(repeat: 2)
        let st = ArrayStack<VArray<Int64, $10000>>(itemNum)
        for (_ in 0..itemNum) {
            st.add(VArray<Int64, $10000>(repeat: 1))
        }
        let sc = SyncCounter(threadNum)
        for (_ in 0..threadNum) {
            spawn { // TODO now there is a cloning issue for Spawn expression,
                    // TODO that reproduced only after Future removal optimization (-O2, RedundantFutureRemoval).
                try {
                    if (let Some(varr) <- st.remove()) {
                        var pass = true
                        for (i in 0..10000) {
                            let x = varr[i]
                            if (x != 1 && x != 0) {
                                pass = false
                                break
                            }
                        }
                    }
                } catch (e: IndexOutOfBoundsException) {
                } finally {
                    sc.dec()
                }
            }
        }
        sc.waitUntilZero()
    }

    func bar() {}
}
