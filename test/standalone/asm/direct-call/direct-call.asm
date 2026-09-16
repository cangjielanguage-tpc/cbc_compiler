;strict
@main_type "default"

@method_ref default_foo = default@ref foo(I64,I64)I64

@type default
  @method main()I64
    @code
      movi.64 IR1, 0x7
      movi.64 IR2, 0x0
      call.direct IR1, #default_foo
      ret.64 IR1
    @end
  @end

  @method foo(I64,I64)I64
    @code
      @live.prim IR1, IR2
      bcc.64 EQ, IR1, IRZ, r
      add.64 IR2, IR2, IR1
      movi.64 IR3, 0x1
      sub.64 IR1, IR1, IR3
      @dead IR3
      call.direct IR1, #default_foo
r:
      @dead IR1
      mov.64 IR1, IR2
      ret.64 IR1
    @end
  @end
@end
