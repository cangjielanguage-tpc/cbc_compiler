;strict

@main_type "default"

@aot_deps "aot:cangjie-std-core"

@field_ref xfr = default:Bar@ref x I64
@field_ref yfr = default:Bar@ref y I64
@field_ref zfr = default:Bar@ref z I64
@field_ref bfr = default:Bar@ref b default:Bar@ref

@type std.core:Object
  @flags PUBLIC AOT
@end

@type aot:Foo
  @flags PUBLIC AOT
  @super std.core:Object@aref

  @field x I64
    @flags PUBLIC AOT
  @end
  @field y I64
    @flags PUBLIC AOT
  @end
@end

@type default:Bar
  @super aot:Foo@aref
  @field z I64
    @flags PUBLIC
  @end
  @field b default:Bar@ref
    @flags PUBLIC
  @end
@end

@type default
  @method main()I64
    @code
      newobj default:Bar@ref
      movi.64 IR2, 1
      movi.64 IR3, 3
      movi.64 IR4, 5

      st.ref.field IR1, IR1, #bfr
      st.ref.field IR2, IR1, #xfr
      st.ref.field IR3, IR1, #yfr
      st.ref.field IR4, IR1, #zfr

      ld.ref.field IR5, IR1, #bfr
      @dead IR1, IR2, IR3, IR4

      movi.64 IR1, 0x0
      movi.64 IR2, 0x0
      movi.64 IR3, 0x0
      movi.64 IR4, 0x0

      @dead IR2, IR3, IR4

      ld.ref.field IR5, IR5, #bfr
      ld.ref.field IR5, IR5, #bfr
      ld.ref.field IR5, IR5, #bfr
      ld.ref.field IR5, IR5, #bfr

      ld.ref.field IR2, IR5, #xfr
      ld.ref.field IR3, IR5, #yfr
      ld.ref.field IR4, IR5, #zfr

      add.64 IR1, IR1, IR2
      add.64 IR1, IR1, IR3
      add.64 IR1, IR1, IR4

      ret.64 IR1
    @end
  @end
@end
