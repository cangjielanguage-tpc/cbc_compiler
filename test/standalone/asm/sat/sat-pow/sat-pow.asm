;strict
@main_type "default"

@aot_deps "aot"

@aot.direct printLnk = "_CN3aot5printHl"

@method_ref print = aot@aref print(I64)I64 #printLnk

@type default
  @method main()I64
    @code
      ; ---- spow.64: no overflow ----
      movi.64 IR2, 2
      movi.64 IR3, 10
      spow.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 1024
      @dead IR1 IR2 IR3

      ; ---- spow.64: negative base, odd exponent ----
      movi.64 IR2, -2
      movi.64 IR3, 3
      spow.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -8
      @dead IR1 IR2 IR3

      ; ---- spow.64: negative base, even exponent ----
      movi.64 IR2, -2
      movi.64 IR3, 4
      spow.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 16
      @dead IR1 IR2 IR3

      ; ---- spow.64: x ** 0 = 1 ----
      movi.64 IR2, 5
      movi.64 IR3, 0
      spow.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 1
      @dead IR1 IR2 IR3

      ; ---- spow.64: MIN ** 1 = MIN (no overflow) ----
      movi.64 IR2, 0x8000000000000000
      movi.64 IR3, 1
      spow.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -9223372036854775808
      @dead IR1 IR2 IR3

      ; ---- spow.64: MIN ** 0 = 1 ----
      movi.64 IR2, 0x8000000000000000
      movi.64 IR3, 0
      spow.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 1
      @dead IR1 IR2 IR3

      ; ---- spow.64: (-1) ** 5 = -1 (no overflow ever) ----
      movi.64 IR2, -1
      movi.64 IR3, 5
      spow.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -1
      @dead IR1 IR2 IR3

      ; ---- spow.64: 2 ** 63 saturates to Int64.Max ----
      movi.64 IR2, 2
      movi.64 IR3, 63
      spow.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2 IR3

      ; ---- spow.64: 2 ** 512 saturates to Int64.Max (huge exponent) ----
      movi.64 IR2, 2
      movi.64 IR3, 512
      spow.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2 IR3

      ; ---- spow.64: MIN ** 3 saturates to Int64.Min ----
      movi.64 IR2, 0x8000000000000000
      movi.64 IR3, 3
      spow.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -9223372036854775808
      @dead IR1 IR2 IR3

      ; ---- spow.32: 2 ** 31 saturates to Int32.Max ----
      movi.64 IR2, 2
      movi.64 IR3, 31
      spow.32 IR1, IR2, IR3
      i2i.u I32, I64, IR1, IR1
      call.direct IR1, #print       ; 2147483647
      @dead IR1 IR2 IR3

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
