package aot

public class Basic {
    public var x: Int64 = 0
    public var y: ?Basic = None
}

// Level 1

public struct L1a {
    public var f1: Int64 = 0
}

public struct L1b {
    public var f1: Basic = Basic()
}

// Level 2

public struct L2a {
    public var f1: Basic = Basic()
    public var f2: L1a  = L1a()
}

public struct L2b {
    public var f1: Int64 = 0
    public var f2: L1b  = L1b()
}

// Level 3

public struct L3 {
    public var f1: L2a = L2a()
    public var f2: L2b = L2b()
}

// Level 4

public struct L4 {
    public var f1: Int64 = 0
    public var f2: Int64 = 0
    public var f3: L3 = L3()
}

// Level 4 container

public class L4C {
    public var f1: Int64 = 0
    public var f2: Int64 = 0
    public var f3: L4 = L4()
}

// Checkers

public func checkPrim(actual: Int64, expected: Int64): Int64 {
    if (actual != expected) {
        println("Incorrect value: expected=${expected}, actual=${actual}")
    }
    return 0
}

public func checkObj(obj: Basic): Int64 {
    if (obj.x != 42 || obj.y.isSome()) {
        println("Corrupted Basic object")
    }
    return 0
}
