;strict
; test F16 calls between CBC and Cangjie AOT code, including mixed and spilled arguments
; cover callbacks, checked conversions, nonvolatile registers and generic/closure results

@main_type "default"
@foreign_libs "native"
@field_ref arm = default@ref arm I64
@field_ref genericResult = [Box[F16]] 0 Box[F16]
@method_ref arch = default@ref arch()U64
@aot_deps "aot:cangjie-std-core"
@aot.direct reportFailureLnk = "_CN3aot13reportFailureHl"
@method_ref reportFailure = aot@aref reportFailure(I64)I64 #reportFailureLnk
@aot.direct identity = "_CN3aot8identityHDh"
@aot.direct mixed = "_CN3aot5mixedHlDhlDh"
@aot.direct many = "_CN3aot4manyHDhDhDhDhDhDhDhDhDhDh"
@aot.direct callback = "_CN3aot8callbackHF0DhE"
@method_ref identity = aot@aref identity(F16)F16 #identity
@method_ref mixed = aot@aref mixed(I64,F16,I64,F16)F16 #mixed
@method_ref many = aot@aref many(F16,F16,F16,F16,F16,F16,F16,F16,F16,F16)F16 #many
@method_ref callback = aot@aref callback(()F16)F16 #callback
@method_ref clobber = default@ref clobber()F16

@type std.core:Object
  @flags AOT
@end
@type aot:HalfCast
  @flags AOT INTERFACE
  @method run(F16)I64
    @flags VIRTUAL ABSTRACT
  @end
@end
@type default:HalfCast
  @super std.core:Object@aref
  @interfaces aot:HalfCast@aref
  @method run(F16)I64
    @flags VIRTUAL
    @code
      @live.ref IR1
      @dead IR1
      cf2i F16, I32, FR0, IR1
      ret.64 IR1
    @end
  @end
@end
@type aot:Holder
  @flags PUBLIC AOT
  @super std.core:Object@aref
  @type_vars 1
  @field value %0
    @flags PUBLIC
  @end
@end
@aot.direct checked = "_CN3aot7checkedHCNY_8HalfCastEDh"
@aot.direct genericCallback = "_CN3aot15genericCallbackIG_HF0G_E"
@method_ref genericCallback = aot@aref genericCallback(()%%0,I64)aot:Holder[%%0]@aref #genericCallback
@aot.direct wrap = "_CN3aot4wrapIG_HG_"
@method_ref checked = aot@aref checked(aot:HalfCast@aref,F16)I64 #checked
@method_ref wrap = aot@aref wrap(%%0,I64)aot:Holder[%%0]@aref #wrap
@aot.instance held = 0
@field_ref held = aot:Holder[F16]@aref value F16 #held

@type default:HalfLambda
  @flags PUBLIC LAMBDA
  @super ()F16
  @field `$gf` I64
    @flags PUBLIC
  @end
  @field `$if` I64
    @flags PUBLIC
  @end
  @method `$g`()Box[F16]
    @flags VIRTUAL SRET HAS_OUTER_TI
    @saved_iregs IR11
    @code
      @live.prim IR1, IR9
      ld.static IR10, #arm
      bcci.64 EQ, IR10, 0, x64
      mov.64 IR11, IR9
      @dead IR1, IR9, IR10
      jmp result
      @dead IR11
      @live.prim IR1, IR9, IR10
x64:
      mov.64 IR11, IR1
      @dead IR1, IR9, IR10
result:
      @dead IR11
      @live.rec IR11
      fmovi.bits.16 FR0, 0x3555 ; value: 0.333251953125
      box.f FR0, IR2, F16
      st.field IR2, IR11, #genericResult
      @dead IR2, IR11
      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
  @method `$i`()F16
    @flags VIRTUAL
    @code
      fmovi.bits.16 FR0, 0x3555 ; value: 0.333251953125
      fret.16 FR0
    @end
  @end
@end

@type default
  @field arm I64
    @flags STATIC
  @end
  @method arch()U64
    @flags FOREIGN AOT
    @link "half_arch"
  @end
  @method clobber()F16
    @saved_fregs FR8
    @code
      fmovi.16 FR8, 9.0
      fret.16 FR8
    @end
  @end
  @method main()I64
    @saved_fregs FR8
    @code
      call.direct #arch
      @live.prim IR1
      st.static IR1, #arm
      @dead IR1
      fmovi.bits.16 FR8, 0xfd55 ; value: -NaN (signaling, payload 0x155)
      fmovi.bits.16 FR0, 0x8000 ; value: -0.0
      call.direct #identity, FR0
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 1
      bcci.64 EQ, IR2, 0x8000, ok1 ; the AOT identity call preserves negative zero
      call.direct #reportFailure, IR1
      ret.64 IR1

ok1:
      @dead IR1, IR2
      movi.64 IR1, 2
      movi.64 IR2, 3
      fmovi.16 FR0, 0.25
      fmovi.16 FR1, 0.5
      call.direct #mixed, IR1, FR0, IR2, FR1
      @dead IR1, IR2
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 2
      bcci.64 EQ, IR2, 0x45c0, ok2 ; mixed integer and half AOT arguments produce 5.75
      call.direct #reportFailure, IR1
      ret.64 IR1

ok2:
      @dead IR1, IR2
      fmovi.16 FR0, 1.0
      fmovi.16 FR1, 2.0
      fmovi.16 FR2, 3.0
      fmovi.16 FR3, 4.0
      fmovi.16 FR4, 5.0
      fmovi.16 FR5, 6.0
      fmovi.16 FR6, 7.0
      fmovi.16 FR7, 8.0
      fmovi.16 FR9, 9.0
      fmovi.16 FR10, 10.0
      call.direct #many, FR0, FR1, FR2, FR3, FR4, FR5, FR6, FR7, FR9, FR10
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 3
      bcci.64 EQ, IR2, 0x52e0, ok3 ; ten half AOT arguments use registers and stack and sum to 55
      call.direct #reportFailure, IR1
      ret.64 IR1

ok3:
      @dead IR1, IR2
      new.closure default:HalfLambda@ref
      call.direct #callback, IR1
      @dead IR1
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 4
      bcci.64 EQ, IR2, 0x3555, ok4 ; the AOT callback receives a concrete CBC closure result
      call.direct #reportFailure, IR1
      ret.64 IR1

ok4:
      @dead IR1, IR2
      call.direct #clobber
      movf2i.16 IR2, FR8
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 5
      bcci.64 EQ, IR2, 0xfd55, ok5 ; the CBC callee preserves nonvolatile FR8
      call.direct #reportFailure, IR1
      ret.64 IR1

ok5:
      @dead IR1, IR2
      newobj default:HalfCast@ref
      fmovi.bits.16 FR0, 0x7e55 ; value: NaN (quiet, payload 0x055)
      call.direct #checked, IR1, FR0
      @dead IR1
      @live.prim IR1
      bcci.64 EQ, IR1, 777, ok6 ; AOT code catches the checked NaN-to-I32 conversion exception
      movi.64 IR2, 6
      call.direct #reportFailure, IR2
      ret.64 IR1
      @dead IR2

ok6:
      @dead IR1
      fmovi.bits.16 FR0, 0x8000 ; value: -0.0
      box.f FR0, IR1, F16
      load.type.info IR2, F16
      call.direct #wrap, IR1, IR2
      @dead IR1, IR2
      @live.ref IR1
      ld.field.f FR0, IR1, #held
      @dead IR1
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 7
      bcci.64 EQ, IR2, 0x8000, ok7 ; the generic AOT holder preserves negative zero
      call.direct #reportFailure, IR1
      ret.64 IR1

ok7:
      @dead IR1, IR2
      new.closure default:HalfLambda@ref
      load.type.info IR2, F16
      call.direct #genericCallback, IR1, IR2
      @dead IR1, IR2
      @live.ref IR1
      ld.field.f FR0, IR1, #held
      @dead IR1
      movf2i.16 IR2, FR0
      bfxz.64.64 IR2, IR2, 0, 16
      movi.64 IR1, 8
      bcci.64 EQ, IR2, 0x3555, ok8 ; the AOT callback receives a generic CBC closure result
      call.direct #reportFailure, IR1
      ret.64 IR1

ok8:
      @dead IR1
      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
