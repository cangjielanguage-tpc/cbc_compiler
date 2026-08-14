;strict
@main_type "default"

@aot_deps "aot"

@aot.direct getALink = "_CN3aot1A4getAHv"
@aot.virtual fooLink = 0 1

@method_ref getA = aot:A@aref getA()aot:A@aref #getALink
@method_ref foo = aot:A@aref foo(aot:A@aref)I64 #fooLink

@type default

  @method main()I64
    @code
	    call.direct IR1, #getA
      @live.ref IR1
      call.virt IR1, #foo
      @dead IR1
      @live.prim IR1
      ret.64 IR1
    @end
  @end
@end
