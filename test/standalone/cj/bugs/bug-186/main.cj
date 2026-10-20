import std.unittest.*
import std.unittest.testmacro.*

enum E {
    | B(Int64)
    | C(?E, Int64)
}
@Test
public class TestMatch {
    @TestCase
    func test(): Unit {
        let e = C(B(1), 2)
        @PowerAssert(
            match (e) {
                case C(_, _) => 0
                case _ => 0
            }, 0
        )
    }
}

main() {
}
