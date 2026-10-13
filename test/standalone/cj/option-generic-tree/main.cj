struct Node<T> where T <: ToString {
  public Node(var value: T,
    let left!: ?Node<T> = None,
    let right!: ?Node<T> = None) {}
  public func traverse(): Unit {
    left?.traverse()
    println(value)
    right?.traverse()
  }
}

main() {
  var tree2 = Node(0,
    right: Node(5, left: Node(6), right: Node(7)))
  tree2.traverse()
}
