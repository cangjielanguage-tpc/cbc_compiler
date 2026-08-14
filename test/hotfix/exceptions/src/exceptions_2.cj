// Interpreted
public func level4(x: Int64, y: Int64): Int64 {
    return x / y
}

// AOT
public func level3(x: Int64, y: Int64): Int64 {
    return level4(x, y)
}

// AOT
public func level2(x: Int64, y: Int64): Int64 {
    return level3(x, y)
}

// Interpreted
public func level1(x: Int64, y: Int64): Int64 {
    return level2(x, y)
}

// AOT
public func level0(x: Int64, y: Int64): Int64 {
    try {
        return level1(x, y)
    } catch (_: IllegalArgumentException) {
        return 42
    } catch (_: ArithmeticException) {
        return 43
    } catch (_: Exception) {
        return 44
    }
}

// AOT
main() {
    println(level0(0, 0))
}