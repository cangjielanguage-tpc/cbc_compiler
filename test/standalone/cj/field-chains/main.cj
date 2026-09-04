var g_counter: Int64 = 100
let g_multiplier: Int64 = 2

func assertEq(actual: Int64, expected: Int64, testId: Int64): Unit {
    if (actual != expected) {
        println("TEST FAILED [ID: ${testId}]: expected ${expected}, got ${actual}")
    }
}

struct Vector2D {
    var x: Int64
    var y: Int64

    public init(x: Int64, y: Int64) {
        this.x = x
        this.y = y
    }

    public mut func translate(dx: Int64, dy: Int64): Unit {
        this.x = this.x + dx
        this.y = this.y + dy
    }

    public mut func scaleAndGetOldX(factor: Int64): Int64 {
        let oldX = this.x
        this.x = this.x * factor
        this.y = this.y * factor
        return oldX
    }
}

struct NodeStats {
    var weight: Int64
    var vec: Vector2D

    public init(weight: Int64, vec: Vector2D) {
        this.weight = weight
        this.vec = vec
    }

    public mut func updateVector(dx: Int64, dy: Int64): Unit {
        this.weight = this.weight + 1
        this.vec.translate(dx, dy)
    }
}

class DataHolder {
    var stats: NodeStats
    var labelId: Int64

    public init(stats: NodeStats, labelId: Int64) {
        this.stats = stats
        this.labelId = labelId
    }

    public func getStatsCopy(): NodeStats {
        return this.stats
    }
}

struct ComplexWrapper {
    var holderRef: DataHolder
    var tag: Int64

    public init(holder: DataHolder, tag: Int64) {
        this.holderRef = holder
        this.tag = tag
    }
}

struct GenericBox<T> {
    var item: T

    public init(item: T) {
        this.item = item
    }

    public mut func updateItem(newItem: T): Unit {
        this.item = newItem
    }
}

struct GenericPair<A, B> {
    var first: A
    var second: B

    public init(first: A, second: B) {
        this.first = first
        this.second = second
    }

    public mut func setFirst(val: A): Unit {
        this.first = val
    }
}

class GenericNode<T> {
    var payload: GenericBox<T>

    public init(payload: GenericBox<T>) {
        this.payload = payload
    }
}

func modifyStructByValue(v: Vector2D): Unit {
    var v2 = v
    v2.x = 9999
    v2.translate(10, 10)
}

main(): Int64 {
    var v1 = Vector2D(10, 20)
    var v2 = v1

    v2.x = 50
    v2.translate(5, 5)

    assertEq(v1.x, 10, 101)
    assertEq(v1.y, 20, 102)
    assertEq(v2.x, 55, 103)

    modifyStructByValue(v1)
    assertEq(v1.x, 10, 104)

    var stats1 = NodeStats(1, Vector2D(100, 200))

    stats1.updateVector(10, 20)
    assertEq(stats1.weight, 2, 201)
    assertEq(stats1.vec.x, 110, 202)

    stats1.vec.translate(5, 5)
    assertEq(stats1.vec.x, 115, 203)
    assertEq(stats1.vec.y, 225, 204)

    let heapObj = DataHolder(stats1, 777)

    stats1.weight = 999
    assertEq(heapObj.stats.weight, 2, 301)

    heapObj.stats.vec.translate(100, 100)
    assertEq(heapObj.stats.vec.x, 215, 302)
    assertEq(heapObj.stats.vec.y, 325, 303)

    heapObj.stats.vec.x = 500
    assertEq(heapObj.stats.vec.x, 500, 304)

    let oldX = heapObj.stats.vec.scaleAndGetOldX(2)
    assertEq(oldX, 500, 305)
    assertEq(heapObj.stats.vec.x, 1000, 306)
    assertEq(heapObj.stats.vec.y, 650, 307)

    var wrapper = ComplexWrapper(heapObj, 42)

    wrapper.holderRef.stats.weight = 888
    assertEq(heapObj.stats.weight, 888, 401)

    wrapper.holderRef.stats.vec.translate(1, 1)
    assertEq(heapObj.stats.vec.x, 1001, 402)

    var statsCopy = heapObj.getStatsCopy()
    statsCopy.vec.x = 9999
    assertEq(heapObj.stats.vec.x, 1001, 501)

    var genBox = GenericBox<Vector2D>(Vector2D(1, 2))

    genBox.item.translate(9, 8)
    assertEq(genBox.item.x, 10, 601)
    assertEq(genBox.item.y, 10, 602)

    genBox.updateItem(Vector2D(100, 100))
    assertEq(genBox.item.x, 100, 603)

    var nestedGen = GenericPair<GenericBox<Vector2D>, Int64>(genBox, 300)

    nestedGen.first.item.x = 777
    assertEq(nestedGen.first.item.x, 777, 604)

    nestedGen.first.item.translate(23, 0)
    assertEq(nestedGen.first.item.x, 800, 605)

    let complexGenericHeap = GenericNode<NodeStats>(
        GenericBox<NodeStats>(NodeStats(50, Vector2D(3, 4)))
    )

    complexGenericHeap.payload.item.vec.y = 444
    assertEq(complexGenericHeap.payload.item.vec.y, 444, 701)

    complexGenericHeap.payload.item.updateVector(10, 10)
    assertEq(complexGenericHeap.payload.item.weight, 51, 702)
    assertEq(complexGenericHeap.payload.item.vec.x, 13, 703)
    assertEq(complexGenericHeap.payload.item.vec.y, 454, 704)

    g_counter = g_counter + complexGenericHeap.payload.item.vec.x * g_multiplier
    assertEq(g_counter, 126, 801)

    complexGenericHeap.payload.item.vec.x = g_counter
    assertEq(complexGenericHeap.payload.item.vec.x, 126, 802)

    println("Finished")
    return 0
}
