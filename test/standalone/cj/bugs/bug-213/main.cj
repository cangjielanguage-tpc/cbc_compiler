// instanceOf03.cj
open class A1 {}
open class A2 <: A1 {}
open class A3 <: A2 {}
interface I {
    func to<T>(input: T) {
        return input
    }
}
class C <: I {
}
main() {
    let a13 = {a: A1 => A3()}
    var x: I = C()
    if (x.to<(A2) -> A2>(a13) is (A1) -> A3) {
    }
}
