;strict
; Reproduces: "failed to find instruction clsl.64" -- checked shift mnemonics
; (clsl/clsr/casr) are not registered at all.
@main_type "default"

@type default
  @method test_clsh()I64
    @code
      @live.prim IR1, IR2
      clsl.64 IR3, IR1, IR2
      @dead IR1, IR2
      mov.64 IR1, IR3
      ret.64 IR1
    @end
  @end

  @method test_crsh()I64
    @code
      @live.prim IR1, IR2
      clsr.64 IR3, IR1, IR2
      @dead IR1, IR2
      mov.64 IR1, IR3
      ret.64 IR1
    @end
  @end

  @method test_cash()I64
    @code
      @live.prim IR1, IR2
      casr.64 IR3, IR1, IR2
      @dead IR1, IR2
      mov.64 IR1, IR3
      ret.64 IR1
    @end
  @end

  @method test_clshi()I64
    @code
      @live.prim IR1
      clsli.64 IR3, IR1, 0x2
      @dead IR1
      mov.64 IR1, IR3
      ret.64 IR1
    @end
  @end
@end
