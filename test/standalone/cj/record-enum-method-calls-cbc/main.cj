struct Point {
    Point(let x: Int64, let y: Int64) {}

    func sum(): Int64 {
        x + y
    }
}

enum Direction {
    North | East | South | West

    func code(): Int64 {
        match (this) {
            case North => 0
            case East => 1
            case South => 2
            case West => 3
        }
    }
}

enum Payload {
    Empty | Value(Int64)

    func getOrDefault(default: Int64): Int64 {
        match (this) {
            case Empty => default
            case Value(value) => value
        }
    }
}

main(): Int64 {
    let point = Point(20, 22)
    println(point.sum())

    let direction = Direction.South
    println(direction.code())

    let empty = Payload.Empty
    let value = Payload.Value(37)
    println(empty.getOrDefault(5))
    println(value.getOrDefault(5))

    0
}
