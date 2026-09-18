;strict
@main_type "default"

@aot_deps "aot"

@aot.direct printLnk = "_CN3aot5printHl"

@method_ref print = aot@aref print(I64)I64 #printLnk

@type default
  @method main()I64
    @code
      ; Saturating casts are desugared by the compiler into truncations and
      ; comparisons; the primitives they rely on are exercised here.
      ; i2i.u mnemonic order: <fromType>, <toType>, <from>, <to>.

      ; ---- i2i U32 -> I64: zero-extension ----
      movi.64 IR2, 0xFFFFFFFF
      i2i.u U32, I64, IR2, IR1
      call.direct IR1, #print       ; 4294967295
      @dead IR1 IR2

      ; ---- i2i I32 -> I64: sign-extension ----
      movi.64 IR2, 0xFFFFFFFB       ; -5
      i2i.u I32, I64, IR2, IR1
      call.direct IR1, #print       ; -5
      @dead IR1 IR2

      ; ---- i2i I64 -> I32 -> I64: truncation to low 32 bits, then sign-extend ----
      movi.64 IR2, 0x1FFFFFFFF
      i2i.u I64, I32, IR2, IR1      ; truncate to low 32 bits (0xFFFFFFFF)
      i2i.u I32, I64, IR1, IR1      ; sign-extend back
      call.direct IR1, #print       ; -1
      @dead IR1 IR2

      ; ---- bfxz: extract 8 bits (zero-extended) ----
      movi.64 IR2, 0xFF
      bfxz.64.64 IR1, IR2, 0, 8
      call.direct IR1, #print       ; 255
      @dead IR1 IR2

      ; ---- bfxs: extract 8 bits (sign-extended) ----
      movi.64 IR2, 0x1FF
      bfxs.64.64 IR1, IR2, 0, 8
      call.direct IR1, #print       ; -1
      @dead IR1 IR2

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
