;strict

@main_type "default"

@aot_deps "cangjie-std-core"

@field_ref index_ref_0 = [I64, I32] 0 I64
@field_ref index_ref_1 = [I64, I32] 1 I32

@type std.core:Object
  @flags PUBLIC AOT
@end

@type default:Foo
  @flags PUBLIC
  @super std.core:Object@aref

  @field x [I64, I32]
    @flags PUBLIC
  @end
  @field y I64
    @flags PUBLIC
  @end
  @field z [I64, I32]
    @flags PUBLIC
  @end
@end

@field_ref zfr = default:Foo@ref z [I64, I32]

@type default
  @method main()I64
    @typed_slots [I64, I32], [I64, I32]
    @code
      newobj default:Foo@ref

      movi.64 IR2, -2
      movi.64 IR3, 1

      st.typed IR3, $0, #index_ref_0

      st.typed IR2, $1, #index_ref_1

      st.field IR2, IR1, #zfr, #index_ref_1

      ld.typed IR4, $0, #index_ref_0

      ld.typed IR5, $1, #index_ref_1

      ld.field IR6, IR1, #zfr, #index_ref_1

      @dead IR1
      add.32 IR1, IR4, IR5
      add.32 IR1, IR1, IR6
      ret.32 IR1
    @end
  @end
@end
