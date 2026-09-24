@main_type "default"

@field_ref boxed_struct_ref = Box[default:Struct@rec] f2 I64

@type default:Struct
  @flags PUBLIC RECORD
  @field f1 I64
    @flags PUBLIC
  @end
  @field f2 I64
    @flags PUBLIC
  @end
@end

@type default
  @method main()I64
    @code
      newobj default:Struct@rec
      ld.field IR1, IR1, #boxed_struct_ref
      ret.64 IR1
    @end
  @end
@end
