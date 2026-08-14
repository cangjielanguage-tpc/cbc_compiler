import aot.*

public var end: Int64 = 0
public var selector: Int64 = 0

public class GlobalVarsContainer {
    public static var prim: Int64      = 0
    public static var ref:  ?TestClass = None
    public static var rec:  TestRec    = TestRec()
}

main() {
  startGarbageGenerator()

  var counter = 0x50000
  while (counter != end) {
    if (selector == 0) {
        GlobalVarsContainer.prim = 123
        selector += 1
        gcpoint()
    } else if (selector == 1) {
        GlobalVarsContainer.ref = TestClass()
        selector += 1
        gcpoint()
    } else if (selector == 2) {
        GlobalVarsContainer.rec.r1 = TestClass()
        GlobalVarsContainer.rec.r2 = TestClass()
        selector += 1
        gcpoint()
    } else {
        GlobalVarsContainer.prim = 0
        GlobalVarsContainer.ref = None
        GlobalVarsContainer.rec.r1 = None
        GlobalVarsContainer.rec.r2 = None
        selector = 0
        gcpoint()
    }
    counter -= 1
  }
}