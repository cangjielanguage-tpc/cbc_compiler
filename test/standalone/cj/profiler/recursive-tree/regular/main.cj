import std.time.*

open class Tree {}

class Empty <: Tree {}
class Node <: Tree {
    public let left: Tree
    public let right: Tree

    init(left: Tree, right: Tree) {
        this.left = left
        this.right = right
    }
}

func buildTree(depth: Int64): Tree {
    if (depth <= 0) {
        return Empty()
    }

    return Node(buildTree(depth - 1), buildTree(depth - 1))
}

func visit(tree: Tree): Int64 {
    match (tree) {
        case node: Node =>
            return 1 + visit(node.left) + visit(node.right)
        case _ =>
            return 1
    }
}

const treeDepth: Int64 = 16
const iterations: Int64 = 2000
const expectedSum: Int64 = 262142000

main(): Int64 {
    let root = buildTree(treeDepth)
    var sum: Int64 = 0

    let started = MonoTime.now()
    for (_ in 0..iterations) {
        sum += visit(root)
    }
    let elapsed = MonoTime.now() - started

    if (sum != expectedSum) {
        return 1
    }

    println(elapsed.toMilliseconds())
    return 0
}
