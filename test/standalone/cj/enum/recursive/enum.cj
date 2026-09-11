enum EE {
    | Num | Num(EE)
    public func one() {
        1
    }
}
main() {
    var b1 = EE.Num(Num)
    println(b1.one())
    0
}

