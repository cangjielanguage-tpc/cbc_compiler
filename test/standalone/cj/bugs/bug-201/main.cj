// call_other_extend_interface.cj
struct A {}
interface I1 {
    static func g() {}
}
extend A <: I1 {}
interface I2 {}
extend A <: I2 {
    static func f() {
        g()
    }
}
main() {
}
