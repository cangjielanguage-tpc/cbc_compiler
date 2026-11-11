; test binary16 boundaries, ties-to-even rounding and direct Float64 immediate conversion
; verify per-instruction rounding, signed zero and NaN payload preservation

@main_type "default"

@aot_deps "aot"
@aot.direct reportFailureLnk = "_CN3aot13reportFailureHl"
@method_ref reportFailure = aot@aref reportFailure(I64)I64 #reportFailureLnk

@type default
  @method main()I64
    @code
      ; assertion 1: fmovi.16 preserves positive zero
      fmovi.16 FR0, 0.0
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 1
      w.bcci.64 NE, IR2, 0x0000, fail

      ; assertion 2: fmovi.16 preserves negative zero
      fmovi.16 FR0, -0.0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 2
      w.bcci.64 NE, IR2, 0x8000, fail

      ; assertion 3: fmovi.16 encodes the largest finite half
      fmovi.16 FR0, 65504.0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 3
      w.bcci.64 NE, IR2, 0x7bff, fail

      ; assertion 4: fmovi.16 rounds the overflow threshold to positive infinity
      fmovi.16 FR0, 65520.0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 4
      w.bcci.64 NE, IR2, 0x7c00, fail

      ; assertion 5: fmovi.16 encodes the smallest positive normal half
      fmovi.16 FR0, 0.00006103515625
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 5
      w.bcci.64 NE, IR2, 0x0400, fail

      ; assertion 6: fmovi.16 encodes the smallest positive subnormal half
      fmovi.16 FR0, 0.000000059604644775390625
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 6
      w.bcci.64 NE, IR2, 0x0001, fail

      ; assertion 7: fmovi.16 rounds half the smallest subnormal to even zero
      fmovi.16 FR0, 0.0000000298023223876953125
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 7
      w.bcci.64 NE, IR2, 0x0000, fail

      ; assertion 8: fmovi.16 rounds a midpoint down to the even half
      fmovi.16 FR0, 1.00048828125
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 8
      w.bcci.64 NE, IR2, 0x3c00, fail

      ; assertion 9: fmovi.16 rounds up directly from Float64 just above the midpoint
      fmovi.16 FR0, 1.0004882812500002
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 9
      w.bcci.64 NE, IR2, 0x3c01, fail

      ; assertion 10: fmovi.16 rounds the next midpoint up to the even half
      fmovi.16 FR0, 1.00146484375
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 10
      w.bcci.64 NE, IR2, 0x3c02, fail

      ; assertion 11: fmovi.16 rounds down directly from Float64 just below the midpoint
      fmovi.16 FR0, 1.0014648437499998
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 11
      w.bcci.64 NE, IR2, 0x3c01, fail

      ; assertion 12: fadd.16 and fsub.16 round after each operation: (2048 + 1) - 2048 == 0
      fmovi.16 FR0, 2048.0
      fmovi.16 FR1, 1.0
      fadd.16 FR0, FR0, FR1
      fmovi.16 FR1, 2048.0
      fsub.16 FR0, FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 12
      w.bcci.64 NE, IR2, 0x0000, fail

      ; assertion 13: fmul.16 overflows the largest finite half times 2 to infinity
      fmovi.16 FR0, 65504.0
      fmovi.16 FR1, 2.0
      fmul.16 FR0, FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 13
      w.bcci.64 NE, IR2, 0x7c00, fail

      ; assertion 14: fdiv.16 rounds the smallest subnormal divided by 2 to even zero
      fmovi.bits.16 FR0, 0x0001 ; value: 0.000000059604644775390625
      fmovi.16 FR1, 2.0
      fdiv.16 FR0, FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 14
      w.bcci.64 NE, IR2, 0x0000, fail

      ; assertion 15: fneg.16 changes positive zero to negative zero
      fmovi.bits.16 FR0, 0x0000 ; value: +0.0
      fneg.16 FR0, FR0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 15
      w.bcci.64 NE, IR2, 0x8000, fail

      ; assertion 16: fabs.16 changes negative zero to positive zero
      fmovi.bits.16 FR0, 0x8000 ; value: -0.0
      fabs.16 FR0, FR0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 16
      w.bcci.64 NE, IR2, 0x0000, fail

      ; assertion 17: fneg.16 toggles the NaN sign without changing its payload
      fmovi.bits.16 FR0, 0x7d55 ; value: NaN (signaling, payload 0x155)
      fneg.16 FR0, FR0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 17
      w.bcci.64 NE, IR2, 0xfd55, fail

      ; assertion 18: fabs.16 clears the NaN sign without changing its payload
      fmovi.bits.16 FR0, 0xfd55 ; value: -NaN (signaling, payload 0x155)
      fabs.16 FR0, FR0
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 18
      w.bcci.64 NE, IR2, 0x7d55, fail
      @dead IR1
      movi.64 IR1, 0
      ret.64 IR1

fail:
      call.direct #reportFailure, IR1
      ret.64 IR1
    @end
  @end
@end
