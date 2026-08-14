import std.runtime.*
import aot

struct Result {
    public var fst: Int64
    public var snd: Int64

    init(a: Int64, b: Int64) {
        fst = a
        snd = b
    }
}

struct MutPairsTester {
    public var state: Int64 = 0

    public mut func levelA(depth: Int64): Unit {
        let res = this.levelB(depth)
        state += res
    }

    public mut func levelB(depth: Int64): Int64 {
        let res = this.levelC(depth)
        return res.fst + res.snd
    }

    public mut func levelC(depth: Int64): Result {
        if (depth > 0) {
            this.levelA(depth - 1)
            return Result(0, 0)
        } else {
            aot.gcpoint()
        }
        return Result(6, 4)
    }
}

class ObjectBase {
    var x: Int64 = 0
    public var tester: MutPairsTester = MutPairsTester()
}

var globalTester: MutPairsTester = MutPairsTester()

main() {
    aot.startGarbageGenerator()

    let repeats = 10000
    let depth = 5

    let obj = ObjectBase()
    var localTester = MutPairsTester()

    gc()
    for (i in 1..repeats) {
        obj.tester.levelA(depth)
    }

    gc()
    for (i in 1..repeats) {
        localTester.levelA(depth)
    }

    gc()
    for (i in 1..repeats) {
        globalTester.levelA(depth)
    }

    gc()

    println(obj.tester.state)
    println(localTester.state)
    println(globalTester.state)
    return 0
}
