;strict
; test two-byte F16 access through typed slots, fields, statics, arrays and raw/derived addresses
; verify generic loads/stores, payload preservation and adjacent sentinel bytes

@main_type "default"

@aot_deps "aot"
@aot.direct reportFailureLnk = "_CN3aot13reportFailureHl"
@method_ref reportFailure = aot@aref reportFailure(I64)I64 #reportFailureLnk
@field_ref a = [F16,F16,U8,U8] 0 F16
@field_ref b = [F16,F16,U8,U8] 1 F16
@field_ref c = [F16,F16,U8,U8] 2 U8
@field_ref d = [F16,F16,U8,U8] 3 U8
@field_ref staticHalf = default@ref half F16
@field_ref first = [F16] 0 F16

@field_ref objA = default:HalfFields@ref a F16
@field_ref objB = default:HalfFields@ref b F16
@field_ref objC = default:HalfFields@ref c U8
@field_ref objD = default:HalfFields@ref d U8
@type default:HalfFields
  @field a F16
  @end
  @field b F16
  @end
  @field c U8
  @end
  @field d U8
  @end
@end

@type default
  @field half F16
    @flags STATIC
  @end
  @method main()I64
    @typed_slots [F16,F16,U8,U8]
    @code
      movi.64 IR2, 0xa5
      st.typed IR2, $0, #c
      @dead IR2
      movi.64 IR2, 0x5a
      st.typed IR2, $0, #d
      @dead IR2
      fmovi.bits.16 FR0, 0x7d55 ; value: NaN (signaling, payload 0x155)
      fmovi.bits.16 FR1, 0x8000 ; value: -0.0
      st.typed.f FR0, $0, #a
      st.typed.f FR1, $0, #b
      ld.typed.f FR2, $0, #a
      movf2i.16 IR2, FR2
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 1
      bcci.64 EQ, IR2, 0x7d55, ok1 ; typed-slot access preserves the NaN payload
      call.direct #reportFailure, IR1
      ret.64 IR1

ok1:
      @dead IR2
      ld.typed IR2, $0, #c
      bcci.64 EQ, IR2, 0xa5, ok2 ; half stores preserve the first sentinel byte
      call.direct #reportFailure, IR1
      ret.64 IR1

ok2:
      @dead IR2
      ld.typed IR2, $0, #d
      bcci.64 EQ, IR2, 0x5a, ok3 ; half stores preserve the second sentinel byte
      call.direct #reportFailure, IR1
      ret.64 IR1

ok3:
      @dead IR2
      ld.stack.rec IR10, $0
      ld.raw.f16 FR2, IR10, 2
      movf2i.16 IR2, FR2
      bfxz.64.64 IR2, IR2, 0, 16
      bcci.64 EQ, IR2, 0x8000, ok4 ; ld.raw.f16 loads the adjacent negative zero
      call.direct #reportFailure, IR1
      ret.64 IR1

ok4:
      @dead IR2
      st.raw.f16 FR0, IR10, 2
      ld.typed.f FR2, $0, #b
      movf2i.16 IR2, FR2
      bfxz.64.64 IR2, IR2, 0, 16
      bcci.64 EQ, IR2, 0x7d55, ok5 ; st.raw.f16 updates the adjacent half
      call.direct #reportFailure, IR1
      ret.64 IR1

ok5:
      @dead IR2
      @dead IR10
      @live.prim IR10
      st.derived.f FR1, IRZ, IR10, #first
      ld.derived.f FR2, IRZ, IR10, #first
      movf2i.16 IR2, FR2
      bfxz.64.64 IR2, IR2, 0, 16
      bcci.64 EQ, IR2, 0x8000, ok6 ; derived access preserves negative zero
      call.direct #reportFailure, IR1
      ret.64 IR1

ok6:
      @dead IR2, IR10
      st.static.f FR0, #staticHalf
      ld.static.f FR2, #staticHalf
      movf2i.16 IR2, FR2
      bfxz.64.64 IR2, IR2, 0, 16
      bcci.64 EQ, IR2, 0x7d55, ok7 ; static-field access preserves the NaN payload
      call.direct #reportFailure, IR1
      ret.64 IR1

ok7:
      @dead IR1, IR2
      movi.64 IR2, 3
      newarr @arr[F16]
      @dead IR2
      mov.ref IR11, IR1
      @dead IR1
      movi.64 IR3, 0
      st.arr.f16 FR0, IR11, IR3
      addi.64 IR3, IR3, 1
      st.arr.f16 FR1, IR11, IR3
      addi.64 IR3, IR3, 1
      fmovi.bits.16 FR2, 0x0001 ; value: 0.000000059604644775390625
      st.arr.f16 FR2, IR11, IR3
      ld.arr.f16 FR3, IR11, IR3
      movf2i.16 IR2, FR3
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 2
      bcci.64 EQ, IR2, 0x0001, ok8 ; array access preserves the smallest subnormal
      call.direct #reportFailure, IR1
      ret.64 IR1

ok8:
      @dead IR2
      subi.64 IR3, IR3, 1
      ld.arr.f16 FR3, IR11, IR3
      movf2i.16 IR2, FR3
      bfxz.64.64 IR2, IR2, 0, 16
      bcci.64 EQ, IR2, 0x8000, ok9 ; array access preserves the adjacent negative zero
      call.direct #reportFailure, IR1
      ret.64 IR1

ok9:
      @dead IR2
      subi.64 IR3, IR3, 1
      ld.arr.f16 FR3, IR11, IR3
      movf2i.16 IR2, FR3
      bfxz.64.64 IR2, IR2, 0, 16
      bcci.64 EQ, IR2, 0x7d55, ok10 ; array access preserves the adjacent NaN payload
      call.direct #reportFailure, IR1
      ret.64 IR1

ok10:
      @dead IR2, IR3, IR11
      box.f FR0, IR4, F16
      lea.box IR5, IR4
      load.type.info IR6, F16
      ld.g IR7, IR4, IR5, IR6
      unbox.f FR3, IR7, F16
      movf2i.16 IR2, FR3
      bfxz.64.64 IR2, IR2, 0, 16
      bcci.64 EQ, IR2, 0x7d55, ok11 ; ld.g and unbox.f preserve the NaN payload
      call.direct #reportFailure, IR1
      ret.64 IR1

ok11:
      @dead IR2
      box.f FR1, IR8, F16
      st.g IR8, IR4, IR5, IR6
      ld.raw.f16 FR3, IR5, 0
      movf2i.16 IR2, FR3
      bfxz.64.64 IR2, IR2, 0, 16
      @dead IR8
      bcci.64 EQ, IR2, 0x8000, ok12 ; st.g writes the negative-zero payload
      call.direct #reportFailure, IR1
      ret.64 IR1

ok12:
      @dead IR4, IR5, IR6, IR7, IR1, IR2
      newobj default:HalfFields@ref
      fmovi.bits.16 FR0, 0x7d55 ; value: NaN (signaling, payload 0x155)
      fmovi.bits.16 FR1, 0x8000 ; value: -0.0
      st.field.f FR0, IR1, #objA
      st.field.f FR1, IR1, #objB
      movi.64 IR2, 0xa5
      st.field IR2, IR1, #objC
      @dead IR2
      movi.64 IR2, 0x5a
      st.field IR2, IR1, #objD
      @dead IR2
      movi.64 IR3, 3
      ld.field.f FR2, IR1, #objA
      movf2i.16 IR2, FR2
      bfxz.64.64 IR2, IR2, 0, 16
      bcci.64 EQ, IR2, 0x7d55, fields1 ; instance-field access preserves the NaN payload
      @dead IR1
      call.direct #reportFailure, IR3
      @live.prim IR1
      ret.64 IR1
      @dead IR1
      @live.ref IR1

fields1:
      @dead IR2
      ld.field.f FR2, IR1, #objB
      movf2i.16 IR2, FR2
      bfxz.64.64 IR2, IR2, 0, 16
      bcci.64 EQ, IR2, 0x8000, fields2 ; instance-field access preserves negative zero
      @dead IR1
      call.direct #reportFailure, IR3
      @live.prim IR1
      ret.64 IR1
      @dead IR1
      @live.ref IR1

fields2:
      @dead IR2
      ld.field IR2, IR1, #objC
      bcci.64 EQ, IR2, 0xa5, fields3 ; instance-field stores preserve the first sentinel
      @dead IR1
      call.direct #reportFailure, IR3
      @live.prim IR1
      ret.64 IR1
      @dead IR1
      @live.ref IR1

fields3:
      @dead IR2
      ld.field IR2, IR1, #objD
      bcci.64 EQ, IR2, 0x5a, fields4 ; instance-field stores preserve the second sentinel
      @dead IR1
      call.direct #reportFailure, IR3
      @live.prim IR1
      ret.64 IR1
      @dead IR1
      @live.ref IR1

fields4:
      @dead IR1
      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
