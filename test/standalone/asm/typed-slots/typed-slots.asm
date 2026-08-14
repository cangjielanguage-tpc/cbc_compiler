;strict
@main_type "default"

@aot_deps "aot"

@aot.direct printLnk = "_CN3aot5printHl"

@aot.instance i8_1_Idx = 0
@aot.instance i8_2_Idx = 1
@aot.instance i16_Idx  = 2
@aot.instance i32_Idx  = 3

@method_ref print = aot@aref println()Unit #printLnk

@field_ref rec_i8_1 = aot:Foo@arec i8_1 I8 #i8_1_Idx
@field_ref rec_i8_2 = aot:Foo@arec i8_2 I8 #i8_2_Idx
@field_ref rec_i16  = aot:Foo@arec i16 I16 #i16_Idx
@field_ref rec_i32  = aot:Foo@arec i32 I32 #i32_Idx

@type default
  @method main()I64
    @typed_slots aot:Foo@arec

    @code
      movi.64 IR1, 0x1A
      st.tslot IR1, $0, #rec_i8_1
      @dead IR1
      movi.64 IR1, 0x2B
      st.tslot IR1, $0, #rec_i8_2
      @dead IR1
      st.tslot.imm 0x3456, $0, #rec_i16
      movi.64 IR1, 0x78ABCDEF
      st.tslot IR1, $0, #rec_i32
      @dead IR1
      ld.tslot IR1, $0, #rec_i8_1
      ld.tslot IR2, $0, #rec_i8_2
      ld.tslot IR3, $0, #rec_i16
      ld.tslot IR4, $0, #rec_i32
      lsli.64 IR1, IR1, 56
      lsli.64 IR2, IR2, 48
      lsli.64 IR3, IR3, 32
      add.64 IR1, IR1, IR2
      add.64 IR1, IR1, IR3
      add.64 IR1, IR1, IR4
      call.direct IR1, #print
      @dead IR1
      movi.64 IR1, 0x0
      ret.64 IR1
    @end
  @end
@end
