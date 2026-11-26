import std.sync.*

main(): Int64 {
    let mutex = Mutex()
    mutex.lock()
    mutex.unlock()
    if (!mutex.tryLock()) {
        return 1
    }
    mutex.unlock()

    let monitor = Monitor()
    monitor.lock()
    monitor.notify()
    monitor.notifyAll()
    println(monitor.wait(timeout: Duration.nanosecond))
    monitor.unlock()

    let multi = MultiConditionMonitor()
    multi.lock()
    let condition = multi.newCondition()
    multi.notify(condition)
    multi.notifyAll(condition)
    println(multi.wait(condition, timeout: Duration.nanosecond))
    multi.unlock()
    return 0
}
