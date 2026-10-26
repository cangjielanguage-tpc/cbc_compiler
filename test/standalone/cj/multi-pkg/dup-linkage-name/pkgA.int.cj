package pkgA

public interface I {
  static func ff1(): Int64
  func ff2(): Unit
}

extend Int64 <: I {
  public static func ff1(): Int64 {
    3
  }
  public func ff2() {
    ()
  }
}
