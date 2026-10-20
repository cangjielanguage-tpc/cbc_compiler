internal import pkgA.*

public interface I2 <: I {
  func ff3() {
    ff1()
  }
}

extend Int64 <: I2 {}

main() {
  0
}
