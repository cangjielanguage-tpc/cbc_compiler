import std.unittest.*

func checkNotNone<V>(ctx: AssertionCtx, v: ?V): V {
  if (let Some(res) <- v) {                                  // <--- CRASH HERE
  }
  ctx.fail("Expected input value to be Some(_) but got None")
}

main() {

}
