// mut.cj
interface I<T> {
    mut func foo<S>() {
    }
    mut func bar<S>(a: S): S
}
struct FOO<T> {}
extend<R> FOO<R> <: I<R> {
    public mut func bar<S>(a: S): S {
        a
    }
}
main() {
    let b: I<Bool> = FOO<Bool>()
    b.bar<Int32>(7)
}
