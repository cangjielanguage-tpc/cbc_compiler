public enum Tree <: ToString {
  Empty | Node(Int64, Tree, Tree)

  public func toString(): String {
    match(this) {
      case Empty =>
        "Empty"
      case Node(v, l, r) =>
        "(Node ${v} ${l} ${r})"
    }
  }
}

public func printTree(t: Tree): Unit {
  println("${t}")
}

public func printToString(t: ToString): Unit {
  println("${t}")
}

main() {
  printTree(Empty)
  printTree(Node(5, Node(2, Empty, Empty), Node(7, Node(6, Empty, Empty), Empty)))
  println()
  printToString(Empty)
  printToString(Node(5, Node(2, Empty, Empty), Node(7, Node(6, Empty, Empty), Empty)))
  0
}
