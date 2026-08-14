;strict

@aot_deps "cangjie-std-core"

@main_type "default"

@type std.core:Object
  @flags AOT
@end

@type default:Foo
  @flags PUBLIC
  @super std.core:Object@aref
  @type_vars 1

  @field skip %0
    @flags PUBLIC
  @end

  @field y %0
    @flags PUBLIC
  @end

  @field x %0
    @flags PUBLIC
  @end
@end

@field_ref foo_obj.x = default:Foo[default:Foo[I8]@ref]@ref x default:Foo[I8]@ref
@field_ref foo_obj.y = default:Foo[default:Foo[I8]@ref]@ref y default:Foo[I8]@ref

@field_ref foo_byte.x = default:Foo[I8]@ref x I8
@field_ref foo_byte.y = default:Foo[I8]@ref y I8

@type default
  @method main()I64
    @saved_iregs IR11, IR12
    @code
      newobj default:Foo[default:Foo[I8]@ref]@ref
      mov.ref IR12, IR1
      @dead IR1
      newobj default:Foo[I8]@ref
      mov.ref IR11, IR1
      @dead IR1

      movi.64 IR1, 33
      movi.64 IR2, 9

      st.ref.field IR11, IR12, #foo_obj.x
      st.ref.field IR12, IR12, #foo_obj.y
      st.ref.field IR1, IR11, #foo_byte.x
      st.ref.field IR2, IR11, #foo_byte.y

      ld.ref.field IR12, IR12, #foo_obj.y
      ld.ref.field IR4, IR12, #foo_obj.x
      ld.ref.field IR5, IR4, #foo_byte.y
      ld.ref.field IR4, IR4, #foo_byte.x

      @dead IR1
      add.64 IR1, IR5, IR4
      ret.64 IR1
    @end
  @end
@end

