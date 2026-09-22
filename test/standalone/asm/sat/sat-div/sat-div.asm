;strict
@main_type "default"

@aot_deps "aot"

@aot.direct printLnk = "_CN3aot5printHl"

@method_ref print = aot@aref print(I64)I64 #printLnk

@type default
  @method main()I64
    @code
      ; ---- sdiv.64: no overflow ----
      movi.64 IR2, 7
      movi.64 IR3, 2
      sdiv.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 3
      @dead IR1 IR2 IR3

      ; ---- sdiv.64: negative operands truncate toward zero ----
      movi.64 IR2, 7
      movi.64 IR3, -2
      sdiv.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -3
      @dead IR1 IR2 IR3

      movi.64 IR2, -7
      movi.64 IR3, 2
      sdiv.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -3
      @dead IR1 IR2 IR3

      ; ---- sdiv.64: MIN / -1 saturates to Int64.Max ----
      movi.64 IR2, 0x8000000000000000
      movi.64 IR3, 0xFFFFFFFFFFFFFFFF
      sdiv.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2 IR3

      ; ---- sdiv.32: MIN32 / -1 saturates to Int32.Max ----
      movi.64 IR2, 0x80000000
      movi.64 IR3, 0xFFFFFFFF
      sdiv.32 IR1, IR2, IR3
      i2i.u I32, I64, IR1, IR1
      call.direct IR1, #print       ; 2147483647
      @dead IR1 IR2 IR3

      ; ---- sudiv.64: unsigned division ----
      movi.64 IR2, 10
      movi.64 IR3, 3
      sudiv.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 3
      @dead IR1 IR2 IR3

      ; ---- sudiv.64: unsigned operands are not sign-extended ----
      movi.64 IR2, 0xFFFFFFFFFFFFFFFF    ; UInt64.Max as unsigned
      movi.64 IR3, 2
      sudiv.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 9223372036854775807 (0x7FFF...)
      @dead IR1 IR2 IR3

      ; ---- sudiv.32: UInt32.Max / 2 ----
      movi.64 IR2, 0xFFFFFFFF
      movi.64 IR3, 2
      sudiv.32 IR1, IR2, IR3
      i2i.u U32, I64, IR1, IR1
      call.direct IR1, #print       ; 2147483647
      @dead IR1 IR2 IR3

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
