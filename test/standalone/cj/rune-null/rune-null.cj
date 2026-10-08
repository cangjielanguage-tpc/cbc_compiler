interface I1 {
}

interface I2 {
  func f(): Rune
}

class E <: Exception {}

class A <: I1 & I2 {
  public func f(): Nothing {
    throw E()
  }
}

main() {
}
