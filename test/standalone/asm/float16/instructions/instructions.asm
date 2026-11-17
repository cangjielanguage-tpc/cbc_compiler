; test F16 arithmetic, math, moves and every floating comparison condition
; check fscc.16 plus short and wide branches for ordered values, signed zeros and NaNs

@main_type "default"

@aot_deps "aot"
@aot.direct reportFailureLnk = "_CN3aot13reportFailureHl"
@method_ref reportFailure = aot@aref reportFailure(I64)I64 #reportFailureLnk

@type default
  @method main()I64
    @code
      ; assertion 1: fadd.16 adds 1 and 2
      fmovi.16 FR0, 1.0
      fmovi.16 FR1, 2.0
      fadd.16 FR0, FR0, FR1
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 1
      w.bcci.64 NE, IR2, 0x4200, fail

      ; assertion 2: fsub.16 subtracts 2 from 3
      fmovi.16 FR0, 3.0
      fmovi.16 FR1, 2.0
      fsub.16 FR0, FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 2
      w.bcci.64 NE, IR2, 0x3c00, fail

      ; assertion 3: fmul.16 multiplies 3 by 2
      fmovi.16 FR0, 3.0
      fmovi.16 FR1, 2.0
      fmul.16 FR0, FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 3
      w.bcci.64 NE, IR2, 0x4600, fail

      ; assertion 4: fdiv.16 divides 3 by 2
      fmovi.16 FR0, 3.0
      fmovi.16 FR1, 2.0
      fdiv.16 FR0, FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 4
      w.bcci.64 NE, IR2, 0x3e00, fail

      ; assertion 5: fpow.16 raises 2 to the power of 3
      fmovi.16 FR0, 2.0
      fmovi.16 FR1, 3.0
      fpow.16 FR0, FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 5
      w.bcci.64 NE, IR2, 0x4800, fail

      ; assertion 6: fsqrt.16 computes the square root of 4
      fmovi.16 FR1, 4.0
      fsqrt.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 6
      w.bcci.64 NE, IR2, 0x4000, fail

      ; assertion 7: fsin.16 computes sin(0)
      fmovi.16 FR1, 0.0
      fsin.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 7
      w.bcci.64 NE, IR2, 0x0000, fail

      ; assertion 8: fcos.16 computes cos(0)
      fmovi.16 FR1, 0.0
      fcos.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 8
      w.bcci.64 NE, IR2, 0x3c00, fail

      ; assertion 9: flog.16 computes log(1)
      fmovi.16 FR1, 1.0
      flog.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 9
      w.bcci.64 NE, IR2, 0x0000, fail

      ; assertion 10: fceil.16 rounds 1.25 up to 2
      fmovi.16 FR1, 1.25
      fceil.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 10
      w.bcci.64 NE, IR2, 0x4000, fail

      ; assertion 11: ffloor.16 rounds 1.75 down to 1
      fmovi.16 FR1, 1.75
      ffloor.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 11
      w.bcci.64 NE, IR2, 0x3c00, fail

      ; assertion 12: fneg.16 negates 1
      fmovi.16 FR1, 1.0
      fneg.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 12
      w.bcci.64 NE, IR2, 0xbc00, fail

      ; assertion 13: fabs.16 clears the sign of -1
      fmovi.16 FR1, -1.0
      fabs.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 13
      w.bcci.64 NE, IR2, 0x3c00, fail

      ; assertion 14: fmov.16 preserves the NaN sign and payload
      fmovi.bits.16 FR1, 0xfe55 ; value: -NaN (quiet, payload 0x055)
      fmov.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 14
      w.bcci.64 NE, IR2, 0xfe55, fail

      ; assertion 15: fscc.16 and short/wide fbcc.16 FEQ are false for 1 and 2
      fmovi.bits.16 FR0, 0x3c00 ; value: 1.0
      fmovi.bits.16 FR1, 0x4000 ; value: 2.0
      @dead IR2
      fscc.16 FEQ, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 15
      w.bcci.64 NE, IR2, 0, fail
      fbcc.16 FEQ, FR0, FR1, branch15_0
      jmp branch15_0_ok
      branch15_0:
      jmp fail
      branch15_0_ok:
      w.fbcc.16 FEQ, FR0, FR1, branch15_2
      jmp branch15_2_ok
      branch15_2:
      jmp fail
      branch15_2_ok:

      ; assertion 16: fscc.16 and short/wide fbcc.16 FNE are true for 1 and 2
      fmovi.bits.16 FR0, 0x3c00 ; value: 1.0
      fmovi.bits.16 FR1, 0x4000 ; value: 2.0
      @dead IR2
      fscc.16 FNE, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 16
      w.bcci.64 NE, IR2, 1, fail
      fbcc.16 FNE, FR0, FR1, branch16_0
      jmp fail
      branch16_0:
      w.fbcc.16 FNE, FR0, FR1, branch16_2
      jmp fail
      branch16_2:

      ; assertion 17: fscc.16 and short/wide fbcc.16 FLT are true for 1 and 2
      fmovi.bits.16 FR0, 0x3c00 ; value: 1.0
      fmovi.bits.16 FR1, 0x4000 ; value: 2.0
      @dead IR2
      fscc.16 FLT, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 17
      w.bcci.64 NE, IR2, 1, fail
      fbcc.16 FLT, FR0, FR1, branch17_0
      jmp fail
      branch17_0:
      w.fbcc.16 FLT, FR0, FR1, branch17_2
      jmp fail
      branch17_2:

      ; assertion 18: fscc.16 and short/wide fbcc.16 FGE are false for 1 and 2
      fmovi.bits.16 FR0, 0x3c00 ; value: 1.0
      fmovi.bits.16 FR1, 0x4000 ; value: 2.0
      @dead IR2
      fscc.16 FGE, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 18
      w.bcci.64 NE, IR2, 0, fail
      fbcc.16 FGE, FR0, FR1, branch18_0
      jmp branch18_0_ok
      branch18_0:
      jmp fail
      branch18_0_ok:
      w.fbcc.16 FGE, FR0, FR1, branch18_2
      jmp branch18_2_ok
      branch18_2:
      jmp fail
      branch18_2_ok:

      ; assertion 19: fscc.16 and short/wide fbcc.16 FNLT are false for 1 and 2
      fmovi.bits.16 FR0, 0x3c00 ; value: 1.0
      fmovi.bits.16 FR1, 0x4000 ; value: 2.0
      @dead IR2
      fscc.16 FNLT, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 19
      w.bcci.64 NE, IR2, 0, fail
      fbcc.16 FNLT, FR0, FR1, branch19_0
      jmp branch19_0_ok
      branch19_0:
      jmp fail
      branch19_0_ok:
      w.fbcc.16 FNLT, FR0, FR1, branch19_2
      jmp branch19_2_ok
      branch19_2:
      jmp fail
      branch19_2_ok:

      ; assertion 20: fscc.16 and short/wide fbcc.16 FNGE are true for 1 and 2
      fmovi.bits.16 FR0, 0x3c00 ; value: 1.0
      fmovi.bits.16 FR1, 0x4000 ; value: 2.0
      @dead IR2
      fscc.16 FNGE, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 20
      w.bcci.64 NE, IR2, 1, fail
      fbcc.16 FNGE, FR0, FR1, branch20_0
      jmp fail
      branch20_0:
      w.fbcc.16 FNGE, FR0, FR1, branch20_2
      jmp fail
      branch20_2:

      ; assertion 21: fscc.16 and short/wide fbcc.16 FEQ are true for +0 and -0
      fmovi.bits.16 FR0, 0x0000 ; value: +0.0
      fmovi.bits.16 FR1, 0x8000 ; value: -0.0
      @dead IR2
      fscc.16 FEQ, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 21
      w.bcci.64 NE, IR2, 1, fail
      fbcc.16 FEQ, FR0, FR1, branch21_0
      jmp fail
      branch21_0:
      w.fbcc.16 FEQ, FR0, FR1, branch21_2
      jmp fail
      branch21_2:

      ; assertion 22: fscc.16 and short/wide fbcc.16 FNE are false for +0 and -0
      fmovi.bits.16 FR0, 0x0000 ; value: +0.0
      fmovi.bits.16 FR1, 0x8000 ; value: -0.0
      @dead IR2
      fscc.16 FNE, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 22
      w.bcci.64 NE, IR2, 0, fail
      fbcc.16 FNE, FR0, FR1, branch22_0
      jmp branch22_0_ok
      branch22_0:
      jmp fail
      branch22_0_ok:
      w.fbcc.16 FNE, FR0, FR1, branch22_2
      jmp branch22_2_ok
      branch22_2:
      jmp fail
      branch22_2_ok:

      ; assertion 23: fscc.16 and short/wide fbcc.16 FLT are false for +0 and -0
      fmovi.bits.16 FR0, 0x0000 ; value: +0.0
      fmovi.bits.16 FR1, 0x8000 ; value: -0.0
      @dead IR2
      fscc.16 FLT, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 23
      w.bcci.64 NE, IR2, 0, fail
      fbcc.16 FLT, FR0, FR1, branch23_0
      jmp branch23_0_ok
      branch23_0:
      jmp fail
      branch23_0_ok:
      w.fbcc.16 FLT, FR0, FR1, branch23_2
      jmp branch23_2_ok
      branch23_2:
      jmp fail
      branch23_2_ok:

      ; assertion 24: fscc.16 and short/wide fbcc.16 FGE are true for +0 and -0
      fmovi.bits.16 FR0, 0x0000 ; value: +0.0
      fmovi.bits.16 FR1, 0x8000 ; value: -0.0
      @dead IR2
      fscc.16 FGE, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 24
      w.bcci.64 NE, IR2, 1, fail
      fbcc.16 FGE, FR0, FR1, branch24_0
      jmp fail
      branch24_0:
      w.fbcc.16 FGE, FR0, FR1, branch24_2
      jmp fail
      branch24_2:

      ; assertion 25: fscc.16 and short/wide fbcc.16 FNLT are true for +0 and -0
      fmovi.bits.16 FR0, 0x0000 ; value: +0.0
      fmovi.bits.16 FR1, 0x8000 ; value: -0.0
      @dead IR2
      fscc.16 FNLT, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 25
      w.bcci.64 NE, IR2, 1, fail
      fbcc.16 FNLT, FR0, FR1, branch25_0
      jmp fail
      branch25_0:
      w.fbcc.16 FNLT, FR0, FR1, branch25_2
      jmp fail
      branch25_2:

      ; assertion 26: fscc.16 and short/wide fbcc.16 FNGE are false for +0 and -0
      fmovi.bits.16 FR0, 0x0000 ; value: +0.0
      fmovi.bits.16 FR1, 0x8000 ; value: -0.0
      @dead IR2
      fscc.16 FNGE, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 26
      w.bcci.64 NE, IR2, 0, fail
      fbcc.16 FNGE, FR0, FR1, branch26_0
      jmp branch26_0_ok
      branch26_0:
      jmp fail
      branch26_0_ok:
      w.fbcc.16 FNGE, FR0, FR1, branch26_2
      jmp branch26_2_ok
      branch26_2:
      jmp fail
      branch26_2_ok:

      ; assertion 27: fscc.16 and short/wide fbcc.16 FEQ are false for NaN and 1
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      fmovi.bits.16 FR1, 0x3c00 ; value: 1.0
      @dead IR2
      fscc.16 FEQ, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 27
      w.bcci.64 NE, IR2, 0, fail
      fbcc.16 FEQ, FR0, FR1, branch27_0
      jmp branch27_0_ok
      branch27_0:
      jmp fail
      branch27_0_ok:
      w.fbcc.16 FEQ, FR0, FR1, branch27_2
      jmp branch27_2_ok
      branch27_2:
      jmp fail
      branch27_2_ok:

      ; assertion 28: fscc.16 and short/wide fbcc.16 FNE are true for NaN and 1
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      fmovi.bits.16 FR1, 0x3c00 ; value: 1.0
      @dead IR2
      fscc.16 FNE, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 28
      w.bcci.64 NE, IR2, 1, fail
      fbcc.16 FNE, FR0, FR1, branch28_0
      jmp fail
      branch28_0:
      w.fbcc.16 FNE, FR0, FR1, branch28_2
      jmp fail
      branch28_2:

      ; assertion 29: fscc.16 and short/wide fbcc.16 FLT are false for NaN and 1
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      fmovi.bits.16 FR1, 0x3c00 ; value: 1.0
      @dead IR2
      fscc.16 FLT, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 29
      w.bcci.64 NE, IR2, 0, fail
      fbcc.16 FLT, FR0, FR1, branch29_0
      jmp branch29_0_ok
      branch29_0:
      jmp fail
      branch29_0_ok:
      w.fbcc.16 FLT, FR0, FR1, branch29_2
      jmp branch29_2_ok
      branch29_2:
      jmp fail
      branch29_2_ok:

      ; assertion 30: fscc.16 and short/wide fbcc.16 FGE are false for NaN and 1
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      fmovi.bits.16 FR1, 0x3c00 ; value: 1.0
      @dead IR2
      fscc.16 FGE, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 30
      w.bcci.64 NE, IR2, 0, fail
      fbcc.16 FGE, FR0, FR1, branch30_0
      jmp branch30_0_ok
      branch30_0:
      jmp fail
      branch30_0_ok:
      w.fbcc.16 FGE, FR0, FR1, branch30_2
      jmp branch30_2_ok
      branch30_2:
      jmp fail
      branch30_2_ok:

      ; assertion 31: fscc.16 and short/wide fbcc.16 FNLT are true for NaN and 1
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      fmovi.bits.16 FR1, 0x3c00 ; value: 1.0
      @dead IR2
      fscc.16 FNLT, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 31
      w.bcci.64 NE, IR2, 1, fail
      fbcc.16 FNLT, FR0, FR1, branch31_0
      jmp fail
      branch31_0:
      w.fbcc.16 FNLT, FR0, FR1, branch31_2
      jmp fail
      branch31_2:

      ; assertion 32: fscc.16 and short/wide fbcc.16 FNGE are true for NaN and 1
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      fmovi.bits.16 FR1, 0x3c00 ; value: 1.0
      @dead IR2
      fscc.16 FNGE, IR2, FR0, FR1
      @dead IR1
      movi.64 IR1, 32
      w.bcci.64 NE, IR2, 1, fail
      fbcc.16 FNGE, FR0, FR1, branch32_0
      jmp fail
      branch32_0:
      w.fbcc.16 FNGE, FR0, FR1, branch32_2
      jmp fail
      branch32_2:

      ; assertion 33: log10(2)
      fmovi.16 FR1, 2.0
      flog10.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 33
      w.bcci.64 NE, IR2, 0x34d1, fail

      ; assertion 34: log2(3)
      fmovi.16 FR1, 3.0
      flog2.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 34
      w.bcci.64 NE, IR2, 0x3e57, fail

      ; assertion 35: exp(1)
      fmovi.16 FR1, 1.0
      fexp.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 35
      w.bcci.64 NE, IR2, 0x4170, fail

      ; assertion 36: exp2(-24) produces the minimum subnormal
      fmovi.16 FR1, -24.0
      fexp2.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 36
      w.bcci.64 NE, IR2, 0x0001, fail

      ; assertion 37: trunc(-0.75) preserves the negative zero sign
      fmovi.16 FR1, -0.75
      ftrunc.16 FR0, FR1
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 37
      w.bcci.64 NE, IR2, 0x8000, fail

      @dead IR1
      movi.64 IR1, 0
      ret.64 IR1

fail:
      call.direct #reportFailure, IR1
      ret.64 IR1
    @end
  @end
@end
