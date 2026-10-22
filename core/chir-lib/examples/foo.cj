package bodymove

func foo<T>(x: T): String where T <: ToString {
    let text = x.toString()
    if (text == "") {
        return "global: empty"
    }
    return "global: " + text
}

class C {
    public func foo<T>(x: T): String where T <: ToString {
        return "member: " + x.toString()
    }
}

main() {
    let c = C()
    println(foo(1))
    println(c.foo(1))
}
