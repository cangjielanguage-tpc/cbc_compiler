// class_inheritance_9.cj
interface I3<T> { func i3foo(f: Float64): T }
interface I4<T> { func i4foo(f: Float64): T }

class BBB {}

abstract class A0 <: A3 & I3<BBB> & I4<BBB> {
    public open func i3foo(f: Float64): BBB
}

abstract class A3 <: I3<BBB> { }

open class O1 <: A0 & I4<BBB> {
    public open func i4foo(f: Float64): BBB {
        BBB()
    }
    public open func i3foo(f: Float64): BBB {
        BBB()
    }
}

main(): Unit {
    let x1: A0 = O1()
    x1.i3foo(114.514000f64)
}
