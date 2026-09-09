class C<T> {
  public C(var x: T) {}
}

func lol<T>(s: T) {
  C(s)
}

main() {
  C("Hoho!")
  lol("haha")
  0
}
