;strict
@main_type "default"

@aot_deps "aot"

@type std.core:Object
  @flags AOT
@end

@type aot:I
  @flags AOT INTERFACE
  @method foo()I64
    @flags VIRTUAL ABSTRACT
  @end
@end

@aot.direct triggerLink = "_CN3aot7triggerHCNY_1IE"
@method_ref trigger = aot@aref trigger(aot:I@ref)I64 #triggerLink

@type default:Child
  @super std.core:Object@aref
  @interfaces aot:I@ref
@end

@type default
  @method main()I64
    @code
      newobj default:Child@ref
      call.direct #trigger, IR1
      @dead IR1
      @live.prim IR1
      ret.64 IR1
    @end
  @end
@end
