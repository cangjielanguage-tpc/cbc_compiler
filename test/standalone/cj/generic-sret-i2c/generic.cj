// Regression test for C2I generic sret preparation.
//
// `Option<T>.getOrThrow()` is AOT code.  When this CBC-compiled function is
// instantiated with a value type, the C2I call must supply a non-null boxed
// result object through its hidden sret argument.
struct RequestNode {
  let requestId: String
  let timestamp: Int64

  init(requestId: String, timestamp: Int64) {
    this.requestId = requestId
    this.timestamp = timestamp
  }
}

func unwrap<T>(value: Option<T>): T {
  value.getOrThrow()
}

main(): Int64 {
  let node = unwrap(Some(RequestNode("request-1", 42)))
  println(node.requestId)
  println(node.timestamp)
  0
}
