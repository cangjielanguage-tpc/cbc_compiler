// was enum

abstract class Tree {
    protected func itemCheck(): UInt32
}

class Empty <: Tree {
    protected func itemCheck(): UInt32 {
        return 1
    }
}

class Node <: Tree {
	let lhs: Tree
	let rhs: Tree

	init(lhs: Tree, rhs: Tree) {
	    this.lhs = lhs
	    this.rhs = rhs
	}

    protected func itemCheck(): UInt32 {
        return 1 + lhs.itemCheck() + rhs.itemCheck()
    }
}

func itemCheck(tree:Tree):UInt32{
    return tree.itemCheck()
}

func bottomUpTree(depth:UInt32):Tree{
	if (depth > 0) {
		return Node(bottomUpTree(depth - 1),bottomUpTree(depth - 1))
	}
	return Empty()
}


main() {
	// TODO: parse from args array
	var n: UInt32 = 4
	let minDepth : UInt32 = 4
	var maxDepth : UInt32 = 0
	if (n > minDepth + 2){
		maxDepth = n
	} else {
		maxDepth = minDepth + 2
	}
	let stretchDepth = maxDepth + 1
	let check = itemCheck(bottomUpTree(stretchDepth))
	print("stretch tree of depth ${stretchDepth} check: ${check}\n")

	let longLivedTree = bottomUpTree(maxDepth)
	var depth = minDepth
	while (depth <= maxDepth){
		var iterations = 1 << UInt32(maxDepth + minDepth - depth)
		var check : UInt32 = 0
		var i = 1
		while (i <= iterations){
			check += itemCheck(bottomUpTree(depth))
			i++
		}
		print("${iterations} trees of depth ${depth} check ${check}\n")
		depth += 2
	}
	print("long lived tree of depth ${maxDepth} check ${itemCheck(longLivedTree)}\n")
	return 0

}
