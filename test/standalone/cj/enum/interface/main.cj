enum Ord <: Comparable<Ord> {
        LT | EQ | GT

        public func compare(other: Ord): Ordering {
            return Ordering.LT
        }
}

class Holder<T> {
    func same(a: Ord, b: Ord): Bool {
        return a == b
    }
}

main(): Int64 {
    let h = Holder<Int64>()
    if (h.same(Ord.LT, Ord.EQ)) {
        return 1
    }
    return 0
}
