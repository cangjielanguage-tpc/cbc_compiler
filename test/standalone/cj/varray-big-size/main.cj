import std.sync.SyncCounter
class CollectionConcurrentTest {
    func testArrayStackConcurrent() {
        let threadNum = 200
        let _ = VArray<Int64, $10000>(repeat: 2)
        let sc = SyncCounter(threadNum)
    }
}

class CollectionConcurrentLambdaTest {
    func testArrayStackConcurrentLambda() {
        let threadNum = 200
        let _ = VArray<Int64, $10000>({ index => index * 2 })
        let sc = SyncCounter(threadNum)
    }
}

main() {
    CollectionConcurrentTest().testArrayStackConcurrent()
    CollectionConcurrentLambdaTest().testArrayStackConcurrentLambda()
}
