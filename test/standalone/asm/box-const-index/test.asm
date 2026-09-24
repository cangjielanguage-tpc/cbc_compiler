@main_type "default"

@field_ref boxed_tuple_ref = Box[[I64, I64]] 1 I64

@type default
  @method main()I64
    @code
      newobj Box[[I64, I64]]
      ld.field IR1, IR1, #boxed_tuple_ref
      ret.64 IR1
    @end
  @end
@end
