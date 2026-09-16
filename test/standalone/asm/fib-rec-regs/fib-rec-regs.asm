;strict
@main_type "default"

@method_ref default.fib = default@ref fib(I64)I64

@type default

  @method main()I64
    @code
      movi.64 IR1, 0x7
      call.direct IR1, #default.fib
      ret.64 IR1
    @end
  @end

  @method fib(I64)I64
    @saved_iregs IR12, IR11

    @code
      @live.prim IR1
      bcci.64 LE, IR1, 0x1, r
      mov.64 IR12, IR1
      subi.64 IR1, IR1, 0x1
      call.direct IR1, #default.fib
      mov.64 IR11, IR1
      @dead IR1
      subi.64 IR1, IR12, 0x2
      @dead IR12
      call.direct IR1, #default.fib
      add.64 IR1, IR1, IR11
      @dead IR11
r:
      ret.64 IR1
    @end
  @end
@end
