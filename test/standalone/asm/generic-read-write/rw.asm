;strict
@main_type "default"

@aot_deps "aot:cangjie-std-core"

@aot.direct invokeLnk = "_CN3aot6invokeIG_HCNY_3FooIG_EG_"

@method_ref invoke = aot@aref invoke(aot:Foo[%%0]@aref, %%0)Void #invokeLnk

@type std.core:Object
  @flags PUBLIC AOT
@end

@type aot:Foo
  @flags PUBLIC AOT
  @super std.core:Object@aref
  @type_vars 1

  @field field %0
    @flags PUBLIC
  @end

  @method run(%0)I32
    @flags AOT VIRTUAL
    @link "_CN3aot3FooIG_E3runHG_"
  @end

@end

@field_ref foo.field = aot:Foo[%0]@ref field %0
@field_ref foo.fieldi64 = aot:Foo[I64]@ref field I64
@field_ref foo.fieldref = aot:Foo[std.core:Object@aref]@ref field std.core:Object@aref

@type default:Generic
  @flags PUBLIC
  @super aot:Foo[%0]@aref
  @type_vars 1

  @method run(%0)I32
    @flags VIRTUAL
    @code
      @live.ref IR1, IR2
      @live.prim IR3

      type.arg IR4, 0, IR3

      ms.hd.obj IR1
        ms.field.g #foo.field, IR3
        ms.st.g IR2, IR4

      @dead IR1, IR2, IR3, IR4
      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end

@type default
  @method main()I64
    @saved_iregs IR11, IR12
    @code
      newobj default:Generic[I64]@ref
      mov.ref IR12, IR1
      movi.64 IR2, 42
      box IR2, IR2, I64

      load.type.info IR3, I64
      call.direct IR1, #invoke
      @dead IR1, IR2, IR3

      newobj default:Generic[std.core:Object@aref]@ref
      mov.ref IR11, IR1

      mov.ref IR2, IR12
      load.type.info IR3, std.core:Object@aref

      call.direct IR1, #invoke
      @dead IR1, IR2, IR3

      ld.ref.field IR2, IR12, #foo.fieldi64
      ld.ref.field IR3, IR11, #foo.fieldref

      movi.64 IR1, 0
      bcci.64 NE, IR2, 42, fail
      bcc.64 RNE, IR3, IR12, fail

      @dead IR2, IR3

      load.type.info IR3, default:Generic[I64]@ref
      load.type.info IR4, I64
      ; load directly boxed value
      ms.hd.obj IR12
        ms.field.g #foo.field, IR3
        ms.ld.g IR2, IR4

      unbox IR2, IR2, I64

      bcci.64 NE, IR2, 42, fail
      @dead IR2, IR3, IR4

      load.type.info IR3, default:Generic[std.core:Object@aref]@ref
      load.type.info IR4, std.core:Object@aref
      ; load directly reference value
      ms.hd.obj IR11
        ms.field.g #foo.field, IR3
        ms.ld.g IR2, IR4

      bcc.64 RNE, IR2, IR12, fail
      @dead IR2, IR3, IR4

      ; use offset instruction
      load.type.info IR3, default:Generic[I64]@ref
      load.type.info IR4, I64
      offset IR5, #foo.field, IR3

      ; load directly boxed value
      ms.hd.obj IR12
        ms.offset IR5
        ms.ld.g IR2, IR4

      unbox IR2, IR2, I64

      bcci.64 NE, IR2, 42, fail
      @dead IR2, IR3, IR4, IR5

      jmp exit
  fail:
      addi.64 IR1, IR1, 1
  exit:
      @dead IR12, IR11
      ret.64 IR1
    @end
  @end
@end
