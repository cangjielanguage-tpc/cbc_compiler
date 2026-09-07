package aot

public class Point {
    public var x: Int64 = 0
    public var y: Int64 = 0
}

public func checkObj(p: Point): Int64 {
    println("x=${p.x}, y=${p.y}")
    return 0
}