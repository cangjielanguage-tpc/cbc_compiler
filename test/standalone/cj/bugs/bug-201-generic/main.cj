// call_other_extend_interface.cj
struct A<T> {}
interface I1<S> {
    static func g(x: S) {}
}
extend <X> A<(Array<X>, X)> <: I1<X> {}
extend <X, Y> A<(X, Y)> <: I1<X> {}
interface I2 {}
extend <Y> A<(Int64, Y)> <: I2 {
    static func f(x: Int64) {
        g(x)
    }
}
main() {
}
