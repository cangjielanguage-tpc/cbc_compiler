import std.sync.*

let allReady = AtomicBool(false)
func create(cnt: AtomicInt64): Unit {
  spawn {
    =>
    let x = cnt.fetchAdd(-1) - 1
    if (x == 0) {
      allReady.store(true)
    }
  }
}

main() {
  let cnt = AtomicInt64(100)
  create(cnt)
}
