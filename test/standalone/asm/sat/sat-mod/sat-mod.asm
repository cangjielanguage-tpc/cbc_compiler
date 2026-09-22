;strict
@main_type "default"

@aot_deps "aot"

@aot.direct printLnk = "_CN3aot5printHl"

@method_ref print = aot@aref print(I64)I64 #printLnk

@type default
  @method main()I64
    @code
      ; ---- smod.64: no overflow ----
      movi.64 IR2, 7
      movi.64 IR3, 2
      smod.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 1
      @dead IR1 IR2 IR3

      ; ---- smod.64: sign of remainder follows dividend ----
      movi.64 IR2, 7
      movi.64 IR3, -2
      smod.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 1
      @dead IR1 IR2 IR3

      movi.64 IR2, -7
      movi.64 IR3, 2
      smod.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -1
      @dead IR1 IR2 IR3

      ; ---- smod.64: MIN % -1 = 0 (never truly overflows) ----
      movi.64 IR2, 0x8000000000000000
      movi.64 IR3, 0xFFFFFFFFFFFFFFFF
      smod.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 0
      @dead IR1 IR2 IR3

      ; ---- smod.32: MIN32 % -1 = 0 ----
      movi.64 IR2, 0x80000000
      movi.64 IR3, 0xFFFFFFFF
      smod.32 IR1, IR2, IR3
      i2i.u I32, I64, IR1, IR1
      call.direct IR1, #print       ; 0
      @dead IR1 IR2 IR3

      ; ---- sumod.64: unsigned remainder ----
      movi.64 IR2, 10
      movi.64 IR3, 3
      sumod.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 1
      @dead IR1 IR2 IR3

      ; ---- sumod.32: UInt32.Max % 7 = 3 ----
      movi.64 IR2, 0xFFFFFFFF
      movi.64 IR3, 7
      sumod.32 IR1, IR2, IR3
      i2i.u U32, I64, IR1, IR1
      call.direct IR1, #print       ; 3
      @dead IR1 IR2 IR3

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
