;strict
@main_type "default"

@aot_deps "cangjie-std-core"

@type std.core:Object
    @flags AOT
@end

@type default:I
  @flags PUBLIC INTERFACE

  @method dummy()Void
    @flags VIRTUAL ABSTRACT
  @end

  @method foo()Void
    @flags VIRTUAL ABSTRACT
  @end
@end

@type default:Foo
  @flags PUBLIC
  @super std.core:Object@aref
  @interfaces default:I@ref

  @method dummy()Void
    @flags VIRTUAL
    @code
      movi.64 IR1, 0x20
      ret.64 IR1
    @end
  @end

  @method foo()Void
    @flags VIRTUAL
    @code
      movi.64 IR1, 0x10
      ret.64 IR1
    @end
  @end
@end

@method_ref mfoo = default:I@ref foo()Void
@type default
  @method main()I64
    @code
      newobj default:Foo@ref
      call.interf IR1, #mfoo
      @dead IR1
      @live.prim IR1
      ret.64 IR1
    @end
  @end
@end
