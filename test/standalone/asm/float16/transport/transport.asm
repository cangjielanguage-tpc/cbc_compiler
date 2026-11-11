; check full-register bit moves and half arithmetic with nonzero upper bits
; round-trip all 65,536 half encodings through slots, moves, calls and boxing
; compare raw bits so signed zeros and every NaN payload must survive unchanged

@main_type "default"

@aot_deps "aot"
@aot.direct reportFailureLnk = "_CN3aot13reportFailureHl"
@method_ref reportFailure = aot@aref reportFailure(I64)I64 #reportFailureLnk
@method_ref identity = default@ref identity(F16)F16

@type default
  @method main()I64
    @untyped_count 1
    @saved_iregs IR11
    @code
      movi.64 IR1, 2
      movi.64 IR2, 0x123456789abc3c00
      movi2f.16 FR0, IR2 ; value: 1.0 in the low 16 bits
      movf2i.16 IR3, FR0
      bcc.64 EQ, IR2, IR3, check_half ; bit moves preserve all 64 register bits
      call.direct #reportFailure, IR1
      ret.64 IR1

check_half:
      @dead IR3
      fadd.16 FR0, FR0, FR0
      movf2i.16 IR3, FR0
      bfxz.64.64 IR3, IR3, 0, 16
      bcci.64 EQ, IR3, 0x4000, transport ; fadd.16 uses only the low 16 bits
      call.direct #reportFailure, IR1
      ret.64 IR1

transport:
      @dead IR1, IR2, IR3
      movi.64 IR11, 0
      movi.64 IR1, 0
      movi.64 IR2, 0
      fmovi.16 FR0, 0.0
      st.uslot.f16 FR0, $0
loop:
      ; keep the current half encoding in IR11 across the CBC call and boxing
      movi2f.16 FR0, IR11 ; value: each binary16 encoding as IR11 runs from 0x0000 to 0xffff
      @dead $0
      st.uslot.f16 FR0, $0
      ld.uslot.f16 FR1, $0
      fmov.16 FR0, FR1
      call.direct #identity, FR0
      box.f FR0, IR4, F16
      unbox.f FR0, IR4, F16
      @dead IR4
      @dead IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR1
      movi.64 IR1, 1
      bcc.64 NE, IR2, IR11, fail ; all half encodings survive slots, moves, calls, boxing and unboxing
      addi.64 IR11, IR11, 1
      bcci.64 LT, IR11, 65536, loop
      @dead IR1
      movi.64 IR1, 0
      ret.64 IR1

fail:
      call.direct #reportFailure, IR1
      ret.64 IR1
    @end
  @end
  @method identity(F16)F16
    @code
      fret.16 FR0
    @end
  @end
@end
