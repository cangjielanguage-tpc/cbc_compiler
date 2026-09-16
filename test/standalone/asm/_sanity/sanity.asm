;strict
@main_type "default"

@type default
  @method main()I64
    @code
      movi.64 IR1, 0x2A
      ret.64 IR1
    @end
  @end
@end
