import std.sync.*

main() {
    bool()
    s1()
    s2()
    s4()
    s8()
    u1()
    u2()
    u4()
    u8(AtomicUInt64(1))
    ref()
    optionRef()
    complex()
    0
}

func bool() {
    println("bool")
    let a = AtomicBool(false)
    println(a.load())
    a.store(true)
    println(a.load())
    println(a.compareAndSwap(true, false))
    println(a.swap(true))
    println(a.compareAndSwap(false, true))
    println(a.load())
}

func s1() {
    println("s1")
    let a = AtomicInt8(1);
    println(a.load())
    a.store(2)
    println(a.fetchAdd(3))
    println(a.fetchSub(6))
    println(a.compareAndSwap(-1, 0))
    println(a.fetchXor(1))
    println(a.fetchOr(2))
    println(a.swap(42))
    println(a.compareAndSwap(43, 44))
    println(a.load())
}

func s2() {
    println("s2")
    let a = AtomicInt16(1);
    println(a.load())
    a.store(2)
    println(a.fetchAdd(3))
    println(a.fetchSub(6))
    println(a.compareAndSwap(-1, 0))
    println(a.fetchXor(1))
    println(a.fetchOr(2))
    println(a.swap(42))
    println(a.compareAndSwap(43, 44))
    println(a.load())
}

let fs4 = AtomicInt32(1)
func s4() {
    println("s4")
    println(fs4.load())
    fs4.store(2)
    println(fs4.fetchAdd(3))
    println(fs4.fetchSub(6))
    println(fs4.compareAndSwap(-1, 0))
    println(fs4.fetchXor(1))
    println(fs4.fetchOr(2))
    println(fs4.swap(42))
    println(fs4.compareAndSwap(43, 44))
    println(fs4.load())
}

func s8() {
    println("s8")
    let a = AtomicInt64(1);
    println(a.load())
    a.store(2)
    println(a.fetchAdd(3))
    println(a.fetchSub(6))
    println(a.compareAndSwap(-1, 0))
    println(a.fetchXor(1))
    println(a.fetchOr(2))
    println(a.swap(42))
    println(a.compareAndSwap(43, 44))
    println(a.load())
}

func u1() {
    println("u1")
    let a = AtomicUInt8(1);
    println(a.load())
    a.store(2)
    println(a.fetchAdd(3))
    println(a.fetchSub(5))
    println(a.compareAndSwap(0, 1))
    println(a.fetchXor(0))
    println(a.fetchOr(2))
    println(a.swap(42))
    println(a.compareAndSwap(43, 44))
    println(a.load())
}

func u2() {
    println("u2")
    let a = AtomicUInt16(1);
    println(a.load())
    a.store(2)
    println(a.fetchAdd(3))
    println(a.fetchSub(5))
    println(a.compareAndSwap(0, 1))
    println(a.fetchXor(0))
    println(a.fetchOr(2))
    println(a.swap(42))
    println(a.compareAndSwap(43, 44))
    println(a.load())
}

func u4() {
    println("u4")
    let a = AtomicUInt32(1);
    println(a.load())
    a.store(2)
    println(a.fetchAdd(3))
    println(a.fetchSub(5))
    println(a.compareAndSwap(0, 1))
    println(a.fetchXor(0))
    println(a.fetchOr(2))
    println(a.swap(42))
    println(a.compareAndSwap(43, 44))
    println(a.load())
}

func u8(p: AtomicUInt64) {
    println("u8")
    println(p.load())
    p.store(2)
    println(p.fetchAdd(3))
    println(p.fetchSub(5))
    println(p.compareAndSwap(0, 1))
    println(p.fetchXor(0))
    println(p.fetchOr(2))
    println(p.swap(42))
    println(p.compareAndSwap(43, 44))
    println(p.load())
}

func ref(): Unit {
    println("ref")
    let a1 = Atomic(1)
    let a2 = Atomic(2)
    let a3 = Atomic(3)
    let a = AtomicReference<Atomic>(a1)
    println(a.load())
    a.store(a2)
    println(a.load())
    println(a.compareAndSwap(a2, a3))
    println(a.swap(a2))
    println(a.compareAndSwap(a1, a3))
    println(a.load())
}

func optionRef() {
    println("optionRef")
    let a1 = Atomic(1)
    let a2 = Atomic(2)
    let a = AtomicOptionReference<Atomic>(a1)
    println(a.load())
    a.store(None)
    println(a.load())
    println(a.compareAndSwap(None, a2))
    println(a.swap(a1))
    println(a.compareAndSwap(a2, None))
    println(a.load())
}

func complex() {
    // TODO multiple threads
}

class Atomic <: ToString {
    Atomic(let i: Int64) {}

    public override func toString(): String {
        "Atomic${i}"
    }
}
