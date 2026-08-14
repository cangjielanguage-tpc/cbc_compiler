import aot.*

func printTree(t: Tree): Unit {
  match(t) {
    case Empty =>
      println("Empty")
    case Node(v, l, r) =>
      println("Node ${v}")
      printTree(l)
      printTree(r)
  }
}

main() {
  printTree(Empty)
  printTree(Node(5, Node(2, Empty, Empty), Node(7, Node(6, Empty, Empty), Empty)))
  0
}
