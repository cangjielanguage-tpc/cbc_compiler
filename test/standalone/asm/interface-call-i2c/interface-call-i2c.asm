;strict
@main_type "default"

@aot_deps "aot"

@aot.direct getILink = "_CN3aot1C4getIHv"
@aot.interface fooLink = 0

@method_ref getI = aot:C@aref getI()aot:I@aref #getILink
@method_ref foo = aot:I@aref foo(aot:I@aref)I64 #fooLink

@type default

  @method main()I64
    @code
      call.direct #getI, IR1
      @live.ref IR1
      call.interf #foo, IR1
      @dead IR1
      @live.prim IR1
      ret.64 IR1
    @end
  @end
@end
