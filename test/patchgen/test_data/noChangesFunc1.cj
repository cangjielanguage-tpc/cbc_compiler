import std.collection.*
import std.sync.SyncCounter

class CollectionConcurrentTest {
    func testArrayStackConcurrent() {
        let itemNum = 100
        let threadNum = 200
        let _ = VArray<Int64, $10000>(repeat: 2)
        let st = ArrayStack<VArray<Int64, $10000>>(itemNum)
        for (_ in 0..itemNum) {
            st.add(VArray<Int64, $10000>(repeat: 1))
        }
        let sc = SyncCounter(threadNum)
        for (_ in 0..threadNum) {
            spawn {
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
}

main() {
   CollectionConcurrentTest().testArrayStackConcurrent()
}
