class Holder<T> {
    func same(a: Ordering, b: Ordering): Bool {
        return a == b
    }
}

main(): Int64 {
    let h = Holder<Int64>()
    if (h.same(Ordering.LT, Ordering.EQ)) {
        return 1
    }
    return 0
}
