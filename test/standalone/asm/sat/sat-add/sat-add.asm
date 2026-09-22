;strict
@main_type "default"

@aot_deps "aot"

@aot.direct printLnk = "_CN3aot5printHl"

@method_ref print = aot@aref print(I64)I64 #printLnk

@type default
  @method main()I64
    @code
      ; ---- sadd.64: no overflow ----
      movi.64 IR2, 100
      movi.64 IR3, 23
      sadd.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 123
      @dead IR1 IR2 IR3

      ; ---- sadd.64: positive overflow clamps to Int64.Max ----
      movi.64 IR2, 0x7FFFFFFFFFFFFFFF
      movi.64 IR3, 1
      sadd.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2 IR3

      ; ---- sadd.64: negative overflow clamps to Int64.Min ----
      movi.64 IR2, 0x8000000000000000
      movi.64 IR3, 0xFFFFFFFFFFFFFFFF
      sadd.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -9223372036854775808
      @dead IR1 IR2 IR3

      ; ---- sadd.32: positive overflow clamps to Int32.Max (zero-extended on print) ----
      movi.64 IR2, 0x7FFFFFFF
      movi.64 IR3, 1
      sadd.32 IR1, IR2, IR3
      i2i.u U32, I64, IR1, IR1      ; zero-extend the 32-bit result
      call.direct IR1, #print       ; 2147483647
      @dead IR1 IR2 IR3

      ; ---- sadd.32: negative overflow clamps to Int32.Min (sign-extended on print) ----
      movi.64 IR2, 0x80000000
      movi.64 IR3, 0xFFFFFFFF
      sadd.32 IR1, IR2, IR3
      i2i.u I32, I64, IR1, IR1      ; sign-extend the 32-bit result
      call.direct IR1, #print       ; -2147483648
      @dead IR1 IR2 IR3

      ; ---- suadd.64: unsigned overflow clamps to UInt64.Max ----
      movi.64 IR2, 0xFFFFFFFFFFFFFFFF
      movi.64 IR3, 2
      suadd.64 IR1, IR2, IR3
      call.direct IR1, #print       ; -1 (bit pattern 0xFFFF...)
      @dead IR1 IR2 IR3

      ; ---- suadd.64: no overflow ----
      movi.64 IR2, 40
      movi.64 IR3, 2
      suadd.64 IR1, IR2, IR3
      call.direct IR1, #print       ; 42
      @dead IR1 IR2 IR3

      ; ---- suadd.32: unsigned overflow clamps to UInt32.Max ----
      movi.64 IR2, 0xFFFFFFFF
      movi.64 IR3, 2
      suadd.32 IR1, IR2, IR3
      i2i.u U32, I64, IR1, IR1      ; zero-extend the 32-bit result
      call.direct IR1, #print       ; 4294967295
      @dead IR1 IR2 IR3

      ; ---- saddi.64: no overflow ----
      movi.64 IR2, 40
      saddi.64 IR1, IR2, 2
      call.direct IR1, #print       ; 42
      @dead IR1 IR2

      ; ---- saddi.64: positive overflow clamps to Int64.Max ----
      movi.64 IR2, 0x7FFFFFFFFFFFFFFF
      saddi.64 IR1, IR2, 1
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2

      ; ---- suaddi.64: unsigned overflow clamps to UInt64.Max ----
      movi.64 IR2, 0xFFFFFFFFFFFFFFFF
      suaddi.64 IR1, IR2, 1
      call.direct IR1, #print       ; -1
      @dead IR1 IR2

      ; ---- sneg.64: -Int64.Min saturates to Int64.Max ----
      movi.64 IR2, 0x8000000000000000
      sneg.64 IR1, IR2
      call.direct IR1, #print       ; 9223372036854775807
      @dead IR1 IR2

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
