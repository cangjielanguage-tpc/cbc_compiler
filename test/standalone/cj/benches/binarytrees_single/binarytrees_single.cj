// EXEC: %compiler %cmp_opt %optimization_level_2 %overflow_wrapping %f -o %output

import std.convert.*

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

func itemCheck(tree:Tree):UInt32{
	match (tree){
    case t: Node => return 1 + itemCheck(t.left) + itemCheck(t.right)
		case _ => return 1

	}
}


func bottomUpTree(depth:UInt32):Tree{
	if (depth > 0) {
		return Node(bottomUpTree(depth - 1), bottomUpTree(depth - 1))
	}
	return Empty()
}


main(){
  // Original workload: 21
  var n = UInt32(16)
	let minDepth : UInt32 = 0
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
