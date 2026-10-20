// invoke_like_05.cj
interface I {
    operator func -(): Int32 {
        return 100
    }
}
class A <: I {
    public operator func -(input: Int32): Int32 {
        return -100
    }
}
main(): Unit {
  let a = A()
}
