;strict
@main_type "default"

@type default
  @method main()I64
    @code
      movi.64 IR3, 0x1
      movi.64 IR1, 0x0
      movi.64 IR2, 0x7
l:
      add.64 IR1, IR1, IR2
      sub.64 IR2, IR2, IR3
      bcc.64 NE, IR2, IRZ, l
      ret.64 IR1
    @end
  @end
@end
