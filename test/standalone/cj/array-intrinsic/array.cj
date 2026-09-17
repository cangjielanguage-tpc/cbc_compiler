import std.core.*

public var array: Array<Int64> = Array<Int64>()

main() {
  unsafe {
    let arr = Array<Int64>(10) { i => i }
    array =  arr
    let d = acquireArrayRawData<Int64>(arr)
    try {
      let arr = array
    } finally {
      releaseArrayRawData<Int64>(d)
    }
  }
  0
}
