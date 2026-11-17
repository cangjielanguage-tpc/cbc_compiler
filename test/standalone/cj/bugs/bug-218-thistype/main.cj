// CJTS LLT compiler/Sema/class/ThisType/class_generic_infer_thistype_ok_4.cj
// Reproduces bug 218: "could not find VTable slot for f()TL0 in
// List(InstantiatedReference(default:Base,list(LocalTypeVariable(1))))".
// The call a.f() in g resolves through the bound Base<K>; the receiver
// type of the vtable lookup contains a local type variable (This return).

main() {
    let a = Sub<Rune>()
    let b = g(a, r'a')
    if (b is Sub<Rune>) {
        return 0
    }
    return 1
}

open class Base<T> {
    protected open func f(): This {
        this
    }
}

class Sub<T> <: Base<T> {}

func g<T, K>(a: T, b: K) where T <: Base<K> {
    a.f()
}
