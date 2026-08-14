package aot

public func globalFunc<T, S>(t: T, s: S): Unit {
  println("globalFunc")
  log("t", t)
  log("s", s)
}

public class C<T, S> {

  public static func staticFunc(t: T, s: S): Unit {
    println("staticFunc")
    log("t", t)
    log("s", s)
  }

  public func instanceFunc(t: T, s: S): Unit {
    println("instanceFunc")
    log("t", t)
    log("s", s)
  }

  public static func staticFuncG<U, W>(t: T, s: S, u: U, w: W): Unit {
    println("staticFuncG")
    log("t", t)
    log("s", s)
    log("u", u)
    log("w", w)
  }

  public func instanceFuncG<U, W>(t: T, s: S, u: U, w: W): Unit {
    println("instanceFunc")
    log("t", t)
    log("s", s)
    log("u", u)
    log("w", w)
  }

  // TODO: virtual, structs

}

func log<T>(name: String, x: T): Unit {
  match(x) {
    case x: Int64 => println("${name}: ${x}")
    case x: Float64 => println("${name}: ${x}")
    case x: String => println("${name}: ${x}")
    case x => println("${name}: <unknown>")
  }
}

