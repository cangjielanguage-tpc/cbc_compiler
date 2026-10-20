import std.sync.SyncCounter

class CollectionConcurrentLambdaTest {
    func testArrayStackConcurrentLambda() {
        let threadNum = 200
        let _ = VArray<Int64, $10000>({ index => index * 2 })
        let sc = SyncCounter(threadNum)
    }
}

main() {
    CollectionConcurrentLambdaTest().testArrayStackConcurrentLambda()
}
