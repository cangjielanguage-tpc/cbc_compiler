;strict
; Reproduces: "failed to find instruction cadd.8" -- checked ops have no
; .8/.16 forms, only .32/.64 are registered.
@main_type "default"

@type default
  @method test_cadd8()I64
    @code
      @live.prim IR1, IR2
      cadd.8 IR3, IR1, IR2
      @dead IR1, IR2
      mov.64 IR1, IR3
      ret.64 IR1
    @end
  @end

  @method test_csub16()I64
    @code
      @live.prim IR1, IR2
      csub.16 IR3, IR1, IR2
      @dead IR1, IR2
      mov.64 IR1, IR3
      ret.64 IR1
    @end
  @end

  @method test_cmul8()I64
    @code
      @live.prim IR1, IR2
      cmul.8 IR3, IR1, IR2
      @dead IR1, IR2
      mov.64 IR1, IR3
      ret.64 IR1
    @end
  @end

  @method test_cuadd8()I64
    @code
      @live.prim IR1, IR2
      cuadd.8 IR3, IR1, IR2
      @dead IR1, IR2
      mov.64 IR1, IR3
      ret.64 IR1
    @end
  @end

  @method test_cusub16()I64
    @code
      @live.prim IR1, IR2
      cusub.16 IR3, IR1, IR2
      @dead IR1, IR2
      mov.64 IR1, IR3
      ret.64 IR1
    @end
  @end

  @method test_cumul8()I64
    @code
      @live.prim IR1, IR2
      cumul.8 IR3, IR1, IR2
      @dead IR1, IR2
      mov.64 IR1, IR3
      ret.64 IR1
    @end
  @end

  @method test_caddi8()I64
    @code
      @live.prim IR1
      caddi.8 IR3, IR1, 0x5
      @dead IR1
      mov.64 IR1, IR3
      ret.64 IR1
    @end
  @end

  @method test_csubi16()I64
    @code
      @live.prim IR1
      csubi.16 IR3, IR1, 0x4
      @dead IR1
      mov.64 IR1, IR3
      ret.64 IR1
    @end
  @end
@end
