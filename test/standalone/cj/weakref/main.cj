import std.ref.*
import std.runtime.*

class Data {
    func ping(): Int64 {
        42
    }
}

func makeDead(): WeakRef<Data> {
    let d = Data()
    WeakRef<Data>(d, CleanupPolicy.EAGER)
}

func waitCleared(w: WeakRef<Data>): Bool {
    var i = 0
    while (i < 200) {
        if (w.value.isNone()) {
            return true
        }
        gc()
        sleep(Duration.millisecond * 10)
        i++
    }
    return false
}

main(): Int64 {
    let dead = makeDead()
    println(if (waitCleared(dead)) { "dead" } else { "still-alive" })
    println("policy=${dead.cleanupPolicy == CleanupPolicy.EAGER}")

    let live = Data()
    let kept = WeakRef<Data>(live, CleanupPolicy.EAGER)
    gc()
    println("alive=${kept.value.isSome()}")
    println("sink=${live.ping()}")
    kept.clear()
    println("cleared=${kept.value.isNone()}")

    let box = Array<WeakRef<Data>>(1, {_ => kept})
    println("array=${box[0].value.isNone()}")
    0
}
