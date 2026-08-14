import aot

public func entryPoint(): Int64 {
    return aot.testPrint(42)
}

main() {
    entryPoint()
}