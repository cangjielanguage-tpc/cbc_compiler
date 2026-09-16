;strict
@main_type "default"

@aot_deps "aot"

@type std.core:Object
  @flags PUBLIC AOT
@end

@type aot:Foo
  @flags PUBLIC AOT RECORD
  @super std.core:Object@aref
  @type_vars 1

  @field x %0
    @flags PUBLIC
  @end

  @field y %0
    @flags PUBLIC
  @end
@end

@type aot:Bar
  @flags PUBLIC AOT
  @type_vars 1

  @field x %0
    @flags PUBLIC
  @end

  @field y %0
    @flags PUBLIC
  @end
@end

@aot.direct fooName = "_CN3aot3fooIG_HRNY_3FooIG_ECNY_3BarIG_E"
@method_ref foo = aot@aref foo(aot:Foo[%%0]@rec, aot:Bar[%%0]@ref, I64)I64 #fooName
@field_ref foo.x = aot:Foo[I64]@rec x I64
@field_ref foo.y = aot:Foo[I64]@rec y I64
@field_ref bar.x = aot:Bar[I64]@ref x I64
@field_ref bar.y = aot:Bar[I64]@ref y I64

@type default

  @method main()I64
    @saved_iregs IR12
    @typed_slots aot:Foo[I64]@rec

    @code
      st.tslot.imm 42, $0, #foo.x
      st.tslot.imm 99, $0, #foo.y

      newobj aot:Bar[I64]@ref
      mov.ref IR2, IR1
      mov.ref IR12, IR1
      @dead IR1
      load.type.info IR3, I64
      box.ts $0, IR1

      call.direct IR1, #foo
      @dead IR1, IR2, IR3

      ld.ref.field IR2, IR12, #bar.x
      ld.ref.field IR3, IR12, #bar.y

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
