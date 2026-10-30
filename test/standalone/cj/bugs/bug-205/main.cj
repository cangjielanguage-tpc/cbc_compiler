import pkg.*

main() {
    // The closure body is the analogue of the macro-generated benchmark body:
    // it calls the interface default method `reset` on ImmutableProvider<Bool>.
    let w = Wrapper<Bool>(true, { x, _times, max =>
        var p = ImmutableProvider<Bool>(x)
        p.reset(max)
        println("ok: ${x}, max=${max}")
    })
    // Dispatch through the interface so the call goes via the generic
    // wrapper type (analogue of BenchmarkWrapper.measureLoopOnce).
    let r: Runnable = w
    r.run(1, 4)
}
