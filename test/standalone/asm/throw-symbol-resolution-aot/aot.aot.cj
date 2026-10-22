package aot

public interface I {
    func foo(): Int64
}

public func trigger(i: I): Int64 {
    try {
      i.foo()
    } catch (e: Exception) {
      println(e)
      0
    }
}
