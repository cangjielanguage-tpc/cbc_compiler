;strict
@main_type "default"

@aot_deps "aot"

@type std.core:Object
  @flags PUBLIC AOT
@end

@type aot:Foo
  @flags PUBLIC AOT
  @super std.core:Object@aref
  @type_vars 1

  @field x %0
    @flags PUBLIC
  @end

  @field y %0
    @flags PUBLIC
  @end
@end

@aot.direct fooName = "_CN3aot3fooIG_HG_G_"
@method_ref foo = aot@aref foo(%%0, %%0, I64)aot:Foo[%%0]@ref #fooName
@field_ref foo.x = aot:Foo[I64]@ref x I64
@field_ref foo.y = aot:Foo[I64]@ref y I64

@type default

  @method main()I64
    @code
      movi.64 IR7, 99
      movi.64 IR8, 42

      box IR7, IR1, I64
      box IR8, IR2, I64
      load.type.info IR3, I64

      call.direct IR1, #foo
      @dead IR1, IR2, IR3, IR7, IR8
      @live.ref IR1

      ld.ref.field IR2, IR1, #foo.x
      ld.ref.field IR3, IR1, #foo.y
      @dead IR1

      movi.64 IR1, 0
      bcci.64 NE, IR2, 42, fail
      bcci.64 NE, IR3, 99, fail
      jmp exit
  fail:
      addi.64 IR1, IR1, 1
  exit:
      ret.64 IR1
    @end
  @end

@end
