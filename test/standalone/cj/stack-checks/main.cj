public struct Result {
    public var x: Int64
    public var y: Int64

    init(x: Int64, y: Int64) {
        this.x = x
        this.y = y
    }
}

public struct Arg {
    public var x: Int64 = 100
    public var y: Int64 = 0
    public var z: Int64 = 0

    public init(x: Int64, y: Int64, z: Int64) {
        this.x = x
        this.y = y
        this.z = z
    }
}

public struct Mutable {
    var state: Int64 = 0
    var unused: Int64 = 0

    public mut func helper1(depth: Int64) {
        if (depth <= 0) {
            println("Error!")
        }
    }

    public mut func helper2(arg: Arg): Int64 {
	    return arg.x + arg.y + arg.z
    }

    public mut func test(depth: Int64, arg1: Arg): Result {
        if (depth > 0) {
            var arg1_tmp = Arg(arg1.x, depth, 42)
 	        helper1(depth)

 	        // recursive call
            let res = test(depth - 1, arg1_tmp)

	        unused += helper2(arg1)
	        unused += helper2(arg1_tmp)
	        return res
        } else {
            state = helper2(arg1)
            return Result(6, 4)
        }
    }
}

main() {
    let depth = 80000 // should be big enough to provoke stack extension
    var test = Mutable()

    let res = test.test(depth, Arg(0, 0, 0))

    println(test.state)
    println(res.x)
    println(res.y)
    return 0
}
