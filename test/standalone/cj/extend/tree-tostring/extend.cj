public enum Tree {
  Empty | Node(Int64, Tree, Tree)
}

extend Tree <: ToString {

  public func toString(): String {
    match(this) {
      case Empty =>
        "Empty"
      case Node(v, l, r) =>
        "(Node ${v} ${l} ${r})"
    }
  }

  public func toStringAlt(): String {
    toString()
  }

  public static func toStringStatic(t: Tree): String {
    t.toString()
  }
}

public func printTree(t: Tree): Unit {
  println("${t}")
}

public func printToString(t: ToString): Unit {
  println("${t}")
}

public func printTreeAlt(t: Tree): Unit {
  println(t.toStringAlt())
}

public func printTreeStatic(t: Tree): Unit {
  println(Tree.toStringStatic(t))
}


main() {
  let empty = Empty
  let tree = Node(5, Node(2, Empty, Empty), Node(7, Node(6, Empty, Empty), Empty))
  printTree(empty)
  printTree(tree)
  println()
  printToString(empty)
  printToString(tree)
  println()
  printTreeAlt(empty)
  printTreeAlt(tree)
  println()
  printTreeStatic(empty)
  printTreeStatic(tree)
  0
}
