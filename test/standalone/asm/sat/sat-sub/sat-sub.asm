;strict
@main_type "default"

@aot_deps "aot"

@aot.direct printLnk = "_CN3aot5printHl"

@method_ref print = aot@aref print(I64)I64 #printLnk

@type default
  @method main()I64
    @code
      ; ---- ssub.64: no overflow ----
      movi.64 IR2, 10
      movi.64 IR3, 3
      ssub.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 7
      @dead IR1 IR2 IR3

      ; ---- ssub.64: negative overflow (MIN - 1) clamps to Int64.Min ----
      movi.64 IR2, 0x8000000000000000
      movi.64 IR3, 1
      ssub.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -9223372036854775808
      @dead IR1 IR2 IR3

      ; ---- ssub.64: positive overflow (MAX - (-1)) clamps to Int64.Max ----
      movi.64 IR2, 0x7FFFFFFFFFFFFFFF
      movi.64 IR3, 0xFFFFFFFFFFFFFFFF
      ssub.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2 IR3

      ; ---- ssub.32: negative overflow clamps to Int32.Min (sign-extended) ----
      movi.64 IR2, 0x80000000
      movi.64 IR3, 1
      ssub.32 IR1, IR2, IR3
      i2i.u I32, I64, IR1, IR1
      call.direct IR1, #print       ; -2147483648
      @dead IR1 IR2 IR3

      ; ---- ssub.32: positive overflow clamps to Int32.Max (sign-extended) ----
      movi.64 IR2, 0x7FFFFFFF
      movi.64 IR3, 0xFFFFFFFF
      ssub.32 IR1, IR2, IR3
      i2i.u I32, I64, IR1, IR1
      call.direct IR1, #print       ; 2147483647
      @dead IR1 IR2 IR3

      ; ---- susub.64: unsigned underflow clamps to 0 ----
      movi.64 IR2, 3
      movi.64 IR3, 5
      susub.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 0
      @dead IR1 IR2 IR3

      ; ---- susub.64: no underflow ----
      movi.64 IR2, 5
      movi.64 IR3, 3
      susub.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 2
      @dead IR1 IR2 IR3

      ; ---- susub.32: unsigned underflow clamps to 0 ----
      movi.64 IR2, 0
      movi.64 IR3, 1
      susub.32 IR1, IR2, IR3
      i2i.u U32, I64, IR1, IR1
      call.direct IR1, #print       ; 0
      @dead IR1 IR2 IR3

      ; ---- ssubi.64: negative overflow clamps to Int64.Min ----
      movi.64 IR2, 0x8000000000000000
      ssubi.64 IR1, IR2, 1
      call.direct IR1, #print       ; -9223372036854775808
      @dead IR1 IR2

      ; ---- ssubi.64: positive overflow clamps to Int64.Max ----
      movi.64 IR2, 0x7FFFFFFFFFFFFFFF
      ssubi.64 IR1, IR2, 0xFFFFFFFFFFFFFFFF
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2

      ; ---- susubi.64: unsigned underflow clamps to 0 ----
      movi.64 IR2, 1
      susubi.64 IR1, IR2, 2
      call.direct IR1, #print       ; 0
      @dead IR1 IR2

      ; ---- sneg.64: -(7) ----
      movi.64 IR2, 7
      sneg.64 IR1, IR2
      call.direct IR1, #print       ; -7
      @dead IR1 IR2

      ; ---- sneg.64: -MIN saturates to MAX ----
      movi.64 IR2, 0x8000000000000000
      sneg.64 IR1, IR2
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2

      ; ---- sneg.32: -MIN32 saturates to MAX32 (sign-extended) ----
      movi.64 IR2, 0x80000000
      sneg.32 IR1, IR2
      i2i.u I32, I64, IR1, IR1
      call.direct IR1, #print       ; 2147483647
      @dead IR1 IR2

      ; ---- suneg.64: -5 saturates to 0 ----
      movi.64 IR2, 5
      suneg.64 IR1, IR2
      call.direct IR1, #print       ; 0
      @dead IR1 IR2

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
