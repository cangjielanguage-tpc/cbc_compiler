interface I<T> {
  mut func f1(x: T): T {
    f2(x)
  }
  mut func f2(x: T): T
}
struct S1 <: I<Int64> { // & I<Bool> {
  public mut func f2(x: Int64): Int64 {
    0
  }
}
main() {
}
