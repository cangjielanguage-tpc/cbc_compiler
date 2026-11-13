// CJTS LLT compiler/CodegenCJNative/record/override_cpointer_generic_return_with_concrete_type.cj
// Reproduces bug 218: runtime "CBC internal error: call of an abstract method"
// in useAsIface(): the interface call of get() on C does not bind the
// CPointer<Float64> override.

interface I<T> where T <: CType {
    func get(): CPointer<T>
}

class C <: I<Float64> {
    public override func get(): CPointer<Float64> {
        return CPointer<Float64>()
    }
}

func makeI(): I<Float64> {
    return C()
}

func useAsIface(): CPointer<Float64> {
    return makeI().get()
}

main(): Int64 {
    useAsIface()
    return 0
}
