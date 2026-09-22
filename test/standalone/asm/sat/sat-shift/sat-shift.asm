;strict
@main_type "default"

@aot_deps "aot"

@aot.direct printLnk = "_CN3aot5printHl"

@method_ref print = aot@aref print(I64)I64 #printLnk

@type default
  @method main()I64
    @code
      ; NOTE: saturating shift semantics are provisional (out-of-range shift
      ; amounts are clamped to width - 1) until cjc defines them.

      ; ---- sshl.64: no overflow ----
      movi.64 IR2, 3
      movi.64 IR3, 4
      sshl.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 48
      @dead IR1 IR2 IR3

      ; ---- sshl.64: shift saturates the value to Int64.Max ----
      movi.64 IR2, 0x4000000000000000
      movi.64 IR3, 2
      sshl.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2 IR3

      ; ---- sshl.64: negative shift amount is clamped to 0 ----
      movi.64 IR2, 42
      movi.64 IR3, 0xFFFFFFFFFFFFFFFF
      sshl.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 42
      @dead IR1 IR2 IR3

      ; ---- sshl.64: huge shift amount is clamped to width - 1 ----
      movi.64 IR2, 1
      movi.64 IR3, 512
      sshl.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 9223372036854775807 (1 << 63)
      @dead IR1 IR2 IR3

      ; ---- sshr.64: arithmetic shift right of a negative value ----
      movi.64 IR2, -16
      movi.64 IR3, 2
      sshr.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -4
      @dead IR1 IR2 IR3

      ; ---- sshr.64: positive value shifts logically == arithmetically ----
      movi.64 IR2, 16
      movi.64 IR3, 2
      sshr.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 4
      @dead IR1 IR2 IR3

      ; ---- sshr.64: huge shift amount is clamped to width - 1 ----
      movi.64 IR2, 0x8000000000000000
      movi.64 IR3, 512
      sshr.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -1 (MIN >> 63)
      @dead IR1 IR2 IR3

      ; ---- sshl.32: 0x40000000 << 1 saturates to Int32.Max ----
      movi.64 IR2, 0x40000000
      movi.64 IR3, 1
      sshl.32 IR1, IR2, IR3
      i2i.u I32, I64, IR1, IR1
      call.direct IR1, #print       ; 2147483647
      @dead IR1 IR2 IR3

      ; ---- sshr.32: negative value arithmetic shift (sign-extended print) ----
      movi.64 IR2, 0xFFFFFFFC       ; -4
      movi.64 IR3, 1
      sshr.32 IR1, IR2, IR3
      i2i.u I32, I64, IR1, IR1
      call.direct IR1, #print       ; -2
      @dead IR1 IR2 IR3

      ; ---- sushr.64: logical right shift of MIN pattern ----
      movi.64 IR2, 0x8000000000000000
      movi.64 IR3, 1
      sushr.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 4611686018427387904 (0x4000...)
      @dead IR1 IR2 IR3

      ; ---- sushl.64: unsigned overflow saturates to UInt64.Max ----
      movi.64 IR2, 0x8000000000000000
      movi.64 IR3, 1
      sushl.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -1 (0xFFFF...)
      @dead IR1 IR2 IR3

      ; ---- sslhi.64: immediate shift ----
      movi.64 IR2, 21
      sslhi.64 IR1, IR2, 1
      call.direct IR1, #print       ; 42
      @dead IR1 IR2

      ; ---- ssrhi.64: immediate shift ----
      movi.64 IR2, 42
      ssrhi.64 IR1, IR2, 1
      call.direct IR1, #print       ; 21
      @dead IR1 IR2

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
