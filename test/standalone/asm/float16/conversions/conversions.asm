; test conversions between F16, integer types, F32 and F64
; verify truncation, clamping, NaN handling, checked conversions and direct Float64 rounding

@main_type "default"

@aot_deps "aot"
@aot.direct reportFailureLnk = "_CN3aot13reportFailureHl"
@method_ref reportFailure = aot@aref reportFailure(I64)I64 #reportFailureLnk

@type default
  @method main()I64
    @code
      ; assertion 1: i2f I32 to F16 rounds 2049 to even 2048
      movi.64 IR2, 2049
      i2f I32, F16, IR2, FR0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 1
      w.bcci.64 NE, IR2, 0x6800, fail

      ; assertion 2: i2f U32 to F16 rounds 2049 to even 2048
      @dead IR2
      movi.64 IR2, 2049
      i2f U32, F16, IR2, FR0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 2
      w.bcci.64 NE, IR2, 0x6800, fail

      ; assertion 3: i2f I64 to F16 rounds 2049 to even 2048
      @dead IR2
      movi.64 IR2, 2049
      i2f I64, F16, IR2, FR0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 3
      w.bcci.64 NE, IR2, 0x6800, fail

      ; assertion 4: i2f U64 to F16 rounds 2049 to even 2048
      @dead IR2
      movi.64 IR2, 2049
      i2f U64, F16, IR2, FR0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 4
      w.bcci.64 NE, IR2, 0x6800, fail

      ; assertion 5: f2i F16 to I8 truncates 1.75 to 1
      fmovi.bits.16 FR0, 0x3f00 ; value: 1.75
      @dead IR2
      f2i F16, I8, FR0, IR2
      @dead IR1
      movi.64 IR1, 5
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 6: f2i F16 to I8 maps NaN to zero
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      @dead IR2
      f2i F16, I8, FR0, IR2
      @dead IR1
      movi.64 IR1, 6
      w.bcci.64 NE, IR2, 0, fail

      ; assertion 7: f2i F16 to I8 clamps positive infinity to the maximum
      fmovi.bits.16 FR0, 0x7c00 ; value: +infinity
      @dead IR2
      f2i F16, I8, FR0, IR2
      @dead IR1
      movi.64 IR1, 7
      w.bcci.64 NE, IR2, 127, fail

      ; assertion 8: cf2i F16 to I8 accepts 1.75 and truncates it to 1
      fmovi.16 FR0, 1.75
      @dead IR2
      cf2i F16, I8, FR0, IR2
      @dead IR1
      movi.64 IR1, 8
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 9: f2i F16 to U8 truncates 1.75 to 1
      fmovi.bits.16 FR0, 0x3f00 ; value: 1.75
      @dead IR2
      f2i F16, U8, FR0, IR2
      @dead IR1
      movi.64 IR1, 9
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 10: f2i F16 to U8 maps NaN to zero
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      @dead IR2
      f2i F16, U8, FR0, IR2
      @dead IR1
      movi.64 IR1, 10
      w.bcci.64 NE, IR2, 0, fail

      ; assertion 11: f2i F16 to U8 clamps positive infinity to the maximum
      fmovi.bits.16 FR0, 0x7c00 ; value: +infinity
      @dead IR2
      f2i F16, U8, FR0, IR2
      @dead IR1
      movi.64 IR1, 11
      w.bcci.64 NE, IR2, 255, fail

      ; assertion 12: cf2i F16 to U8 accepts 1.75 and truncates it to 1
      fmovi.16 FR0, 1.75
      @dead IR2
      cf2i F16, U8, FR0, IR2
      @dead IR1
      movi.64 IR1, 12
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 13: f2i F16 to I16 truncates 1.75 to 1
      fmovi.bits.16 FR0, 0x3f00 ; value: 1.75
      @dead IR2
      f2i F16, I16, FR0, IR2
      @dead IR1
      movi.64 IR1, 13
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 14: f2i F16 to I16 maps NaN to zero
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      @dead IR2
      f2i F16, I16, FR0, IR2
      @dead IR1
      movi.64 IR1, 14
      w.bcci.64 NE, IR2, 0, fail

      ; assertion 15: f2i F16 to I16 clamps positive infinity to the maximum
      fmovi.bits.16 FR0, 0x7c00 ; value: +infinity
      @dead IR2
      f2i F16, I16, FR0, IR2
      @dead IR1
      movi.64 IR1, 15
      w.bcci.64 NE, IR2, 32767, fail

      ; assertion 16: cf2i F16 to I16 accepts 1.75 and truncates it to 1
      fmovi.16 FR0, 1.75
      @dead IR2
      cf2i F16, I16, FR0, IR2
      @dead IR1
      movi.64 IR1, 16
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 17: f2i F16 to U16 truncates 1.75 to 1
      fmovi.bits.16 FR0, 0x3f00 ; value: 1.75
      @dead IR2
      f2i F16, U16, FR0, IR2
      @dead IR1
      movi.64 IR1, 17
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 18: f2i F16 to U16 maps NaN to zero
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      @dead IR2
      f2i F16, U16, FR0, IR2
      @dead IR1
      movi.64 IR1, 18
      w.bcci.64 NE, IR2, 0, fail

      ; assertion 19: f2i F16 to U16 clamps positive infinity to the maximum
      fmovi.bits.16 FR0, 0x7c00 ; value: +infinity
      @dead IR2
      f2i F16, U16, FR0, IR2
      @dead IR1
      movi.64 IR1, 19
      w.bcci.64 NE, IR2, 65535, fail

      ; assertion 20: cf2i F16 to U16 accepts 1.75 and truncates it to 1
      fmovi.16 FR0, 1.75
      @dead IR2
      cf2i F16, U16, FR0, IR2
      @dead IR1
      movi.64 IR1, 20
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 21: f2i F16 to I32 truncates 1.75 to 1
      fmovi.bits.16 FR0, 0x3f00 ; value: 1.75
      @dead IR2
      f2i F16, I32, FR0, IR2
      @dead IR1
      movi.64 IR1, 21
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 22: f2i F16 to I32 maps NaN to zero
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      @dead IR2
      f2i F16, I32, FR0, IR2
      @dead IR1
      movi.64 IR1, 22
      w.bcci.64 NE, IR2, 0, fail

      ; assertion 23: f2i F16 to I32 clamps positive infinity to the maximum
      fmovi.bits.16 FR0, 0x7c00 ; value: +infinity
      @dead IR2
      f2i F16, I32, FR0, IR2
      @dead IR1
      movi.64 IR1, 23
      w.bcci.64 NE, IR2, 2147483647, fail

      ; assertion 24: cf2i F16 to I32 accepts 1.75 and truncates it to 1
      fmovi.16 FR0, 1.75
      @dead IR2
      cf2i F16, I32, FR0, IR2
      @dead IR1
      movi.64 IR1, 24
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 25: f2i F16 to U32 truncates 1.75 to 1
      fmovi.bits.16 FR0, 0x3f00 ; value: 1.75
      @dead IR2
      f2i F16, U32, FR0, IR2
      @dead IR1
      movi.64 IR1, 25
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 26: f2i F16 to U32 maps NaN to zero
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      @dead IR2
      f2i F16, U32, FR0, IR2
      @dead IR1
      movi.64 IR1, 26
      w.bcci.64 NE, IR2, 0, fail

      ; assertion 27: f2i F16 to U32 clamps positive infinity to the maximum
      fmovi.bits.16 FR0, 0x7c00 ; value: +infinity
      @dead IR2
      f2i F16, U32, FR0, IR2
      @dead IR1
      movi.64 IR1, 27
      w.bcci.64 NE, IR2, 4294967295, fail

      ; assertion 28: cf2i F16 to U32 accepts 1.75 and truncates it to 1
      fmovi.16 FR0, 1.75
      @dead IR2
      cf2i F16, U32, FR0, IR2
      @dead IR1
      movi.64 IR1, 28
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 29: f2i F16 to I64 truncates 1.75 to 1
      fmovi.bits.16 FR0, 0x3f00 ; value: 1.75
      @dead IR2
      f2i F16, I64, FR0, IR2
      @dead IR1
      movi.64 IR1, 29
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 30: f2i F16 to I64 maps NaN to zero
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      @dead IR2
      f2i F16, I64, FR0, IR2
      @dead IR1
      movi.64 IR1, 30
      w.bcci.64 NE, IR2, 0, fail

      ; assertion 31: f2i F16 to I64 clamps positive infinity to the maximum
      fmovi.bits.16 FR0, 0x7c00 ; value: +infinity
      @dead IR2
      f2i F16, I64, FR0, IR2
      @dead IR1
      movi.64 IR1, 31
      w.bcci.64 NE, IR2, 9223372036854775807, fail

      ; assertion 32: cf2i F16 to I64 accepts 1.75 and truncates it to 1
      fmovi.16 FR0, 1.75
      @dead IR2
      cf2i F16, I64, FR0, IR2
      @dead IR1
      movi.64 IR1, 32
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 33: f2i F16 to U64 truncates 1.75 to 1
      fmovi.bits.16 FR0, 0x3f00 ; value: 1.75
      @dead IR2
      f2i F16, U64, FR0, IR2
      @dead IR1
      movi.64 IR1, 33
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 34: f2i F16 to U64 maps NaN to zero
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      @dead IR2
      f2i F16, U64, FR0, IR2
      @dead IR1
      movi.64 IR1, 34
      w.bcci.64 NE, IR2, 0, fail

      ; assertion 35: f2i F16 to U64 clamps positive infinity to the maximum
      fmovi.bits.16 FR0, 0x7c00 ; value: +infinity
      @dead IR2
      f2i F16, U64, FR0, IR2
      @dead IR1
      movi.64 IR1, 35
      w.bcci.64 NE, IR2, -1, fail

      ; assertion 36: cf2i F16 to U64 accepts 1.75 and truncates it to 1
      fmovi.16 FR0, 1.75
      @dead IR2
      cf2i F16, U64, FR0, IR2
      @dead IR1
      movi.64 IR1, 36
      w.bcci.64 NE, IR2, 1, fail

      ; assertion 37: f2f F16 to F32 and back preserves the smallest subnormal
      fmovi.bits.16 FR0, 0x0001 ; value: 0.000000059604644775390625
      f2f F16, F32, FR0, FR1
      f2f F32, F16, FR1, FR0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 37
      w.bcci.64 NE, IR2, 0x0001, fail

      ; assertion 38: f2f F16 to F64 and back preserves the smallest subnormal
      fmovi.bits.16 FR0, 0x0001 ; value: 0.000000059604644775390625
      f2f F16, F64, FR0, FR1
      f2f F64, F16, FR1, FR0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 38
      w.bcci.64 NE, IR2, 0x0001, fail

      ; assertion 39: f2f F64 to F16 rounds above a half midpoint directly
      fmovi.64 FR1, 1.0004882812500002
      f2f F64, F16, FR1, FR0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 39
      w.bcci.64 NE, IR2, 0x3c01, fail

      ; assertion 40: f2f F64 to F16 rounds below a half midpoint directly
      fmovi.64 FR1, 1.0014648437499998
      f2f F64, F16, FR1, FR0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 40
      w.bcci.64 NE, IR2, 0x3c01, fail
      @dead IR1
      movi.64 IR1, 0
      ret.64 IR1

fail:
      call.direct #reportFailure, IR1
      ret.64 IR1
    @end
  @end
@end
