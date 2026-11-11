;strict
; test C ABI calls with F16 scalars and six-byte aggregates, including stack spills
; verify payload preservation, CBC tail arguments and primitive half receivers

@main_type "default"

@aot_deps "aot"
@aot.direct reportFailureLnk = "_CN3aot13reportFailureHl"
@method_ref reportFailure = aot@aref reportFailure(I64)I64 #reportFailureLnk
@foreign_libs "native"
@field_ref a = [F16,F16,F16] 0 F16
@field_ref b = [F16,F16,F16] 1 F16
@field_ref c = [F16,F16,F16] 2 F16
@method_ref identity = default@ref identity(F16)F16
@method_ref rotate = default@ref rotate([F16,F16,F16])[F16,F16,F16] [SRET]
@method_ref check = default@ref check([F16,F16,F16])U64
@method_ref many = default@ref many(F16,F16,F16,F16,F16,F16,F16,F16,[F16,F16,F16],F16)F16

@method_ref arch = default@ref arch()U64
@method_ref tail = default@ref tail(I64,F16,F16,F16,F16,F16,F16,F16,F16,F16,F16)F16
@method_ref receiver = default@ref receiver()F16 [FPRIM_RECEIVER]

@type default
  @method arch()U64
    @flags FOREIGN AOT
    @link "half_arch"
  @end
  @method tail(I64,F16,F16,F16,F16,F16,F16,F16,F16,F16,F16)F16
    @code
      @live.prim IR1, IR7, IR10
      bcci.64 EQ, IR1, 0, x64
      ld.tail LD_F16, FR0, IR10, 1
      fret.16 FR0
x64:
      ld.tail LD_F16, FR0, IR7, 1
      fret.16 FR0
    @end
  @end
  @method receiver()F16
    @flags FPRIM_RECEIVER
    @code
      fmovi.16 FR1, 1.0
      fadd.16 FR0, FR0, FR1
      fret.16 FR0
    @end
  @end
  @method identity(F16)F16
    @flags FOREIGN AOT
    @link "half_identity"
  @end
  @method rotate([F16,F16,F16])[F16,F16,F16]
    @flags FOREIGN AOT SRET
    @link "half_rotate"
  @end
  @method check([F16,F16,F16])U64
    @flags FOREIGN AOT
    @link "half_check"
  @end
  @method many(F16,F16,F16,F16,F16,F16,F16,F16,[F16,F16,F16],F16)F16
    @flags FOREIGN AOT
    @link "half_many"
  @end
  @method main()I64
    @typed_slots [F16,F16,F16], [F16,F16,F16]
    @code
      fmovi.bits.16 FR0, 0x7d55 ; value: NaN (signaling, payload 0x155)
      call.direct #identity, FR0
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 1
      bcci.64 EQ, IR2, 0x7d55, ok1 ; the C scalar call preserves the NaN payload
      call.direct #reportFailure, IR1
      ret.64 IR1

ok1:
      @dead IR1, IR2
      st.typed.f FR0, $0, #a
      fmovi.bits.16 FR0, 0x8000 ; value: -0.0
      st.typed.f FR0, $0, #b
      fmovi.bits.16 FR0, 0x0001 ; value: 0.000000059604644775390625
      st.typed.f FR0, $0, #c
      ld.stack.rec IR3, $0
      call.direct #check, IR3
      @live.prim IR1
      bcci.64 EQ, IR1, 1, ok2 ; the C aggregate argument preserves all three half payloads
      movi.64 IR2, 2
      call.direct #reportFailure, IR2
      ret.64 IR1
      @dead IR2

ok2:
      @dead IR1, IR3
      ld.stack.rec IR3, $0
      ld.stack.rec IR4, $1
      call.direct #rotate, IR4, IR3
      @dead IR3, IR4
      ld.typed.f FR0, $1, #a
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 3
      bcci.64 EQ, IR2, 0x0001, ok3 ; the rotated C aggregate returns its subnormal first
      call.direct #reportFailure, IR1
      ret.64 IR1

ok3:
      @dead IR2
      ld.typed.f FR0, $1, #b
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      bcci.64 EQ, IR2, 0x7d55, ok4 ; the rotated C aggregate returns its NaN payload second
      call.direct #reportFailure, IR1
      ret.64 IR1

ok4:
      @dead IR2
      ld.typed.f FR0, $1, #c
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      bcci.64 EQ, IR2, 0x8000, ok5 ; the rotated C aggregate returns negative zero third
      call.direct #reportFailure, IR1
      ret.64 IR1

ok5:
      @dead IR1, IR2
      fmovi.16 FR0, 1.0
      st.typed.f FR0, $0, #a
      st.typed.f FR0, $0, #b
      st.typed.f FR0, $0, #c
      ld.stack.rec IR3, $0
      call.direct #many, FR0, FR0, FR0, FR0, FR0, FR0, FR0, FR0, IR3, FR0
      @dead IR3
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 4
      bcci.64 EQ, IR2, 0x4a00, ok6 ; spilled C aggregate and scalar arguments sum to 12
      call.direct #reportFailure, IR1
      ret.64 IR1

ok6:
      @dead IR1, IR2
      call.direct #arch
      @live.prim IR1
      fmovi.bits.16 FR0, 0xfd55 ; value: -NaN (signaling, payload 0x155)
      call.direct #tail, IR1, FR0, FR0, FR0, FR0, FR0, FR0, FR0, FR0, FR0, FR0
      @dead IR1
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 5
      bcci.64 EQ, IR2, 0xfd55, ok7 ; the CBC tail load and return preserve the NaN payload
      call.direct #reportFailure, IR1
      ret.64 IR1

ok7:
      @dead IR1, IR2
      fmovi.16 FR0, 2.0
      call.direct #receiver, FR0
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 6
      bcci.64 EQ, IR2, 0x4200, ok8 ; the primitive half receiver adds 1 to 2
      call.direct #reportFailure, IR1
      ret.64 IR1

ok8:
      @dead IR1
      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
