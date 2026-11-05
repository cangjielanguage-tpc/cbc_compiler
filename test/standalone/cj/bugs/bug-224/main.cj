// iso_g_cpointer_int64.cj
struct W<T> where T <: CType {
    var p: CPointer<T>
    public init(p: CPointer<T>) { this.p = p }
}
func make(): W<Int64> { return W<Int64>(CPointer<Int64>()) }
main(): Int64 { make(); return 0 }
