;strict
@main_type "default"

@aot_deps "aot"

@aot.direct printLnk = "_CN3aot5printHl"

@method_ref print = aot@aref print(I64)I64 #printLnk

@type default
  @method main()I64
    @code
      ; ---- smul.64: no overflow ----
      movi.64 IR2, 3
      movi.64 IR3, -4
      smul.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -12
      @dead IR1 IR2 IR3

      ; ---- smul.64: positive overflow clamps to Int64.Max ----
      movi.64 IR2, 0x7FFFFFFFFFFFFFFF
      movi.64 IR3, 2
      smul.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2 IR3

      ; ---- smul.64: negative overflow clamps to Int64.Min ----
      movi.64 IR2, 0x4000000000000000
      movi.64 IR3, -2
      smul.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -9223372036854775808
      @dead IR1 IR2 IR3

      ; ---- smul.64: MIN * -1 saturates to Int64.Max ----
      movi.64 IR2, 0x8000000000000000
      movi.64 IR3, 0xFFFFFFFFFFFFFFFF
      smul.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2 IR3

      ; ---- smul.32: positive overflow clamps to Int32.Max ----
      movi.64 IR2, 0x10000
      movi.64 IR3, 0x10000
      smul.32 IR1, IR2, IR3
      i2i.u I32, I64, IR1, IR1
      call.direct IR1, #print       ; 2147483647
      @dead IR1 IR2 IR3

      ; ---- smul.32: negative overflow clamps to Int32.Min ----
      movi.64 IR2, 0x10000
      movi.64 IR3, 0xFFFF8000        ; -32768 as i32
      smul.32 IR1, IR2, IR3
      i2i.u I32, I64, IR1, IR1
      call.direct IR1, #print       ; -2147483648
      @dead IR1 IR2 IR3

      ; ---- sumul.64: unsigned overflow clamps to UInt64.Max ----
      movi.64 IR2, 0xFFFFFFFFFFFFFFFF
      movi.64 IR3, 2
      sumul.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -1 (0xFFFF...)
      @dead IR1 IR2 IR3

      ; ---- sumul.64: no overflow ----
      movi.64 IR2, 3
      movi.64 IR3, 4
      sumul.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 12
      @dead IR1 IR2 IR3

      ; ---- sumul.32: unsigned overflow clamps to UInt32.Max ----
      movi.64 IR2, 0x80000000
      movi.64 IR3, 2
      sumul.32 IR1, IR2, IR3
      i2i.u U32, I64, IR1, IR1
      call.direct IR1, #print       ; 4294967295
      @dead IR1 IR2 IR3

      ; ---- smuli.64: no overflow ----
      movi.64 IR2, 6
      smuli.64 IR1, IR2, 7
      call.direct IR1, #print       ; 42
      @dead IR1 IR2

      ; ---- smuli.64: overflow clamps to Int64.Max ----
      movi.64 IR2, 0x4000000000000000
      smuli.64 IR1, IR2, 2
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2

      ; ---- sumuli.64: overflow clamps to UInt64.Max ----
      movi.64 IR2, 0x8000000000000000
      sumuli.64 IR1, IR2, 3
      call.direct IR1, #print       ; -1 (0xFFFF...)
      @dead IR1 IR2

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
