;strict
@main_type "default"

@method_ref default.fib = default@ref fib(I64)I64

@type default {

  @method main()I64
    @code
      movi.64 IR1, 0x7
      call.direct IR1, #default.fib
      ret.64 IR1
    @end
  @end

  @method fib(I64)I64
    @untyped_count 0x1

    @code
      @live.prim IR1
      bcci.64 LE, IR1, 0x1, r
      st.uslot.64 IR1, $0
      subi.64 IR1, IR1, 0x1
      call.direct IR1, #default.fib
      ld.uslot.64 IR2, $0
      @dead $0
      st.uslot.64 IR1, $0
      @dead IR1
      subi.64 IR1, IR2, 0x2
      @dead IR2
      call.direct IR1, #default.fib
      ld.uslot.64 IR2, $0
      add.64 IR1, IR1, IR2
      @dead IR2, $0
r:
      ret.64 IR1
    @end
  @end
@end
