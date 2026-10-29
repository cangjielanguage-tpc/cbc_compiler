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

func visit_inlined(tree: Tree): Int64 {
    var result = 1
    match (tree) {
        case node: Node =>
            match (node.left) {
                case left: Node =>
                    result += 1 + visit_inlined(left.left) + visit_inlined(left.right)
                case _ =>
                    result += 1
            }
            match (node.right) {
                case right: Node =>
                    result += 1 + visit_inlined(right.left) + visit_inlined(right.right)
                case _ =>
                    result += 1
            }
        case _ => return 1
    }
    return result
}

const treeDepth: Int64 = 16
const iterations: Int64 = 2000
const expectedSum: Int64 = 262142000

main(): Int64 {
    let root = buildTree(treeDepth)
    var sum: Int64 = 0

    let started = MonoTime.now()
    for (_ in 0..iterations) {
        sum += visit_inlined(root)
    }
    let elapsed = MonoTime.now() - started

    if (sum != expectedSum) {
        return 1
    }

    println(elapsed.toMilliseconds())
    return 0
}
