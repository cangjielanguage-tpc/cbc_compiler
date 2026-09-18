;strict
@main_type "default"

@aot_deps "aot"

@aot.direct printLnk = "_CN3aot5printHl"

@method_ref print = aot@aref print(I64)I64 #printLnk

@type default
  @method main()I64
    @code
      ; ---- saddi.64: immediate within signed 12-bit range ----
      movi.64 IR2, 40
      saddi.64 IR1, IR2, 2047
      call.direct IR1, #print       ; 2087
      @dead IR1 IR2

      ; ---- saddi.64: negative immediate within signed 12-bit range ----
      movi.64 IR2, 40
      saddi.64 IR1, IR2, -2048
      call.direct IR1, #print       ; -2008
      @dead IR1 IR2

      ; ---- saddi.64: immediate just outside signed 12-bit range ----
      movi.64 IR2, 40
      saddi.64 IR1, IR2, 2048
      call.direct IR1, #print       ; 2088
      @dead IR1 IR2

      ; ---- saddi.64: negative immediate just outside signed 12-bit range ----
      movi.64 IR2, 40
      saddi.64 IR1, IR2, -2049
      call.direct IR1, #print       ; -2009
      @dead IR1 IR2

      ; ---- saddi.64: large immediate ----
      movi.64 IR2, 0
      saddi.64 IR1, IR2, 0x12345678
      call.direct IR1, #print       ; 305419896
      @dead IR1 IR2

      ; ---- saddi.64: large negative immediate ----
      movi.64 IR2, 0
      saddi.64 IR1, IR2, 0x8000000000000000  ; Int64.Min
      call.direct IR1, #print       ; -9223372036854775808
      @dead IR1 IR2

      ; ---- saddi.64: overflow clamp still applies with a large immediate ----
      movi.64 IR2, 0x7FFFFFFFFFFFFFF0
      saddi.64 IR1, IR2, 0x1000
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2

      ; ---- ssubi.64: immediate outside the 12-bit range ----
      movi.64 IR2, 0
      ssubi.64 IR1, IR2, 0x10000
      call.direct IR1, #print       ; -65536
      @dead IR1 IR2

      ; ---- ssubi.64: negative immediate outside the 12-bit range ----
      movi.64 IR2, 0
      ssubi.64 IR1, IR2, 0xFFFFFFFFFFFF0000  ; -65536
      call.direct IR1, #print       ; 65536
      @dead IR1 IR2

      ; ---- smuli.64: immediate outside the 12-bit range ----
      movi.64 IR2, 3
      smuli.64 IR1, IR2, 100000
      call.direct IR1, #print       ; 300000
      @dead IR1 IR2

      ; ---- suaddi.64: immediate outside the 12-bit range ----
      movi.64 IR2, 0
      suaddi.64 IR1, IR2, 0xFFFFFFFFFFFFFFFF  ; UInt64.Max
      call.direct IR1, #print       ; -1
      @dead IR1 IR2

      ; ---- susubi.64: immediate outside the 12-bit range (unsigned underflow -> 0) ----
      movi.64 IR2, 1
      susubi.64 IR1, IR2, 0x10000
      call.direct IR1, #print       ; 0
      @dead IR1 IR2

      ; ---- sumuli.64: immediate outside the 12-bit range ----
      movi.64 IR2, 0x8000000000000000
      sumuli.64 IR1, IR2, 3
      call.direct IR1, #print       ; -1 (0xFFFF...)
      @dead IR1 IR2

      ; ---- saddi.32: 32-bit op with an immediate outside the 12-bit range ----
      movi.64 IR2, 0
      saddi.32 IR1, IR2, 0x40000000
      i2i.u I32, I64, IR1, IR1
      call.direct IR1, #print       ; 1073741824
      @dead IR1 IR2

      ; ---- ssubi.32: 0 - Int32.Min saturates to Int32.Max ----
      movi.64 IR2, 0
      ssubi.32 IR1, IR2, 0x80000000  ; Int32.Min as u64 immediate
      i2i.u I32, I64, IR1, IR1
      call.direct IR1, #print       ; 2147483647
      @dead IR1 IR2

      ; ---- sslhi.64: shift by 32 (outside the signed 12-bit range as a signed value? no, fits) ----
      movi.64 IR2, 1
      sslhi.64 IR1, IR2, 32
      call.direct IR1, #print       ; 4294967296
      @dead IR1 IR2

      ; ---- sslhi.64: shift by 100 (outside the 12-bit signed range) ----
      movi.64 IR2, 1
      sslhi.64 IR1, IR2, 100
      call.direct IR1, #print       ; 9223372036854775807 (clamped)
      @dead IR1 IR2

      ; ---- ssrhi.64: shift by -2049 (negative, outside the 12-bit range; clamped to 0) ----
      movi.64 IR2, 42
      ssrhi.64 IR1, IR2, -2049
      call.direct IR1, #print       ; 42
      @dead IR1 IR2

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
