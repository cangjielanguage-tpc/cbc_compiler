public open class Tree {}

public class Empty <: Tree {}
public class Node <: Tree {
  public let v: Int64
  public let l: Tree
  public let r: Tree

  init(v: Int64, l: Tree, r: Tree) {
    this.v = v
    this.l = l
    this.r = r
  }
}

extend Tree <: ToString {

  public func toString(): String {
    match(this) {
      case t: Empty =>
        "Empty"
      case t: Node =>
        let v = t.v
        let l = t.l
        let r = t.r
        "(Node ${v} ${l} ${r})"
      case _ =>
        "<unexpected>"
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
  let empty = Empty()
  let tree = Node(5, Node(2, Empty(), Empty()), Node(7, Node(6, Empty(), Empty()), Empty()))
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
