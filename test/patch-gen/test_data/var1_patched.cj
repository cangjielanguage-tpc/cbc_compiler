class C {
  let a1Let = 1
  let a2Let = Some(42)
  var b1Var = 2
  var b2Var = Some(43)
  static let c1Let = foo()
  static var c1Var = 42
  static let c2Let = Some(3.14)
}

class D {
  static let d1Let = foo() + foo()
  static var c1Var = 43
  static let d2Let = Some(3.15)
}

func foo(): String {
    "foo"
}

let g1 = 6
var g2 = 5