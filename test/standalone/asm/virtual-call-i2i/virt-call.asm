;strict
@main_type "default"

@aot_deps "cangjie-std-core"

@type std.core:Object
    @flags AOT
@end

@type default:Foo
  @flags PUBLIC
  @super std.core:Object@aref

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

@method_ref virt_foo = default:Foo@ref foo()Void

@type default
  @method main()I64
    @code
      newobj default:Foo@ref
      call.virt IR1, #virt_foo
      @dead IR1
      @live.prim IR1
      ret.64 IR1
    @end
  @end
@end
