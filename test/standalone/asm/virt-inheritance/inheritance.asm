;strict

@main_type "default"

@aot_deps "aot:cangjie-std-core"
@aot.direct qwerty_data = "_CN3aot6qwertyHCNY_1AE"

@type std.core:Object
  @flags PUBLIC AOT
@end

@type aot:A
  @flags AOT
  @super std.core:Object@aref
  @method foo()I64
    @flags AOT VIRTUAL
    @link "_CN3aot1A3fooHv"
  @end
  @method bar()I64
    @flags AOT VIRTUAL
    @link "_CN3aot1A3barHv"
  @end
@end

@type default:Child
  @super aot:A@aref
  @method bar()I64
    @flags VIRTUAL
    @code
      movi.64 IR1, 0x20
      ret.64 IR1
    @end
  @end
@end

@method_ref qwerty = aot:A@aref qwerty()I64 #qwerty_data

@type default
  @method main()I64
    @code
      newobj default:Child@ref
      call.direct IR1, #qwerty
      @dead IR1
      @live.prim IR1
      ret.64 IR1
    @end
  @end
@end
