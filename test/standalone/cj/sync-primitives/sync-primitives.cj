// The deprecated Monitor and MultiConditionMonitor are the only std.sync APIs
// that map 1:1 to the CHIR monitor/multicondition intrinsics, so the
// deprecation warnings are expected and the deprecated usage is intentional.
import std.sync.*

main(): Int64 {
    let mutex = Mutex()
    mutex.lock()
    try {
        if (!mutex.tryLock()) {
            return 1
        }
    } finally {
        mutex.unlock()
    }

    let monitor = Monitor()
    monitor.lock()
    try {
        monitor.notify()
        monitor.notifyAll()
        println(monitor.wait(timeout: Duration.nanosecond))
    } finally {
        monitor.unlock()
    }

    let multi = MultiConditionMonitor()
    multi.lock()
    try {
        let condition = multi.newCondition()
        multi.notify(condition)
        multi.notifyAll(condition)
        println(multi.wait(condition, timeout: Duration.nanosecond))
    } finally {
        multi.unlock()
    }
    return 0
}
