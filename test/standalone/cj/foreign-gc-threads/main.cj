import std.sync.SyncCounter
import aot.*

foreign func sleepInC(n: Int64): Int64

class Box {
    var x: Int64 = 0

    init(v: Int64) {
        x = v
    }
}

func worker(results: Array<Int64>, idx: Int64, rounds: Int64) {
    var total: Int64 = 0
    var r: Int64 = 0
    while (r < rounds) {
        let l0 = Box(0)
        let l1 = Box(1)
        let l2 = Box(2)
        let l3 = Box(3)
        let l4 = Box(4)
        let l5 = Box(5)
        let l6 = Box(6)
        let l7 = Box(7)
        let l8 = Box(8)
        let l9 = Box(9)
        let l10 = Box(10)
        let l11 = Box(11)
        let l12 = Box(12)
        let l13 = Box(13)
        let l14 = Box(14)
        let l15 = Box(15)

        let res = unsafe { sleepInC(r) }

        total += res + l0.x + l1.x + l2.x + l3.x + l4.x + l5.x + l6.x + l7.x + l8.x + l9.x + l10.x + l11.x + l12.x + l13.x + l14.x + l15.x
        r += 1
    }
    results[idx] = total
}

main(): Int64 {
    startGarbageGenerator()

    let threads = 4
    let rounds = 10000
    let results = Array<Int64>(threads, repeat: 0)
    let done = SyncCounter(threads)
    var i: Int64 = 0
    while (i < threads) {
        let idx = i
        spawn {
            =>
            worker(results, idx, rounds)
            done.dec()
        }
        i += 1
    }
    done.waitUntilZero()

    var total: Int64 = 0
    var j: Int64 = 0
    while (j < threads) {
        total += results[j]
        j += 1
    }
    println("${total}")
    return 0
}
