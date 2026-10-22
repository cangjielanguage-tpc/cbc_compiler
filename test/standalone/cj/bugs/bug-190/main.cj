// test.cj
main() {
  println(B<Int64>().test())
  println(B<String>().test())
}
class B<T> {
    public func test(): Option<T> {
        return 2 as T
    }
}
