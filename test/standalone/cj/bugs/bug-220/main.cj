// recursionEnum05.cj
enum E {
    A | B(Bool, S, () -> Bool)
}
struct S {
    var x: E = E.A
    var y = "good!!!"
}
func goo() {
    true
}
main() {
    var a = E.B(true, S(), goo)
    match (a) {
        case A => println(100)
        case B(s, x, y) =>
            if (x.y == "good!!!") {
            }
    }
}
