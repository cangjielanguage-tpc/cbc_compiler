package default.utils

public class Fib {
   public static func fib(n: Int64) {
        var a: Int64 = 0
        var b: Int64 = 1
        for (i in 1..n) {
            let tmp = b
            b = a + b
            a = tmp
        }
        return a
   }
}