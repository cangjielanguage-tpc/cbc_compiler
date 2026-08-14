import aot

public class Child <: aot.A {
  public open func bar() { 0x20 }
}

main() {
  var obj = Child()
  aot.qwerty(obj)
}
