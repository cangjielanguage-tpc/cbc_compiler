;strict
@main_type "default"

@aot_deps "aot"

; section: fields info

@aot.instance x_idx = 0
@aot.instance y_idx = 1

@aot.instance p1_idx = 0
@aot.instance r1_idx = 1

@field_ref x_field = aot:TestClass@aref x I64 #x_idx
@field_ref y_field = aot:TestClass@aref y I64 #y_idx

@field_ref p1_field = aot:TestContainer@arec p1 I64                #p1_idx
@field_ref r1_field = aot:TestContainer@arec r1 aot:TestClass@aref #r1_idx

; section: methods info

@aot.direct aotLnk = "_CN3aot21startGarbageGeneratorHv"
@method_ref startGarbageGenerator = aot@aref startGarbageGenerator()I64 #aotLnk

@aot.direct checkLnk = "_CN3aot8checkObjHCNY_9TestClassE"
@method_ref check = aot@aref checkObj(aot:TestClass@aref)I64 #checkLnk

@method_ref default.newObj = default@ref newObj()aot:TestClass@aref
@method_ref default.foo = default@ref foo()I64
@method_ref default.bar = default@ref bar()I64
@method_ref default.baz = default@ref baz()I64

; section: types

@type default

  @method newObj()aot:TestClass@aref
    @code
      movi.64 IR2, 123
      movi.64 IR3, 321
      newobj aot:TestClass@aref
      st.ref.field IR2, IR1, #x_field
      st.ref.field IR3, IR1, #y_field
      @dead IR2 IR3
      ret.ref IR1
    @end
  @end

  @method baz()I64
    @typed_slots aot:TestContainer@arec

    @code
      zero.refs $0

      movi.64 IR2, 0xABAB
      st.tslot IR2, $0, #p1_field

      call.direct IR1, #default.newObj
      @live.ref IR1
      st.tslot IR1, $0, #r1_field

      @dead IR1 IR2

      gcpoint

      ld.tslot IR1, $0, #r1_field
      call.direct IR1, #check
      @dead IR1

      movi.64 IR1, 42
      ret.64 IR1
    @end
  @end

  @method bar()I64
    @typed_slots aot:TestContainer@arec, aot:TestContainer@arec

    @code
      zero.refs $0
      zero.refs $1

      call.direct IR1, #default.newObj
      @live.ref IR1
      st.tslot IR1, $0, #r1_field

      movi.64 IR2, 0xABAB
      st.tslot IR2, $0, #p1_field

      @dead IR1 IR2

      call.direct IR1, #default.newObj
      @live.ref IR1
      st.tslot IR1, $1, #r1_field

      movi.64 IR2, 0xBABA
      st.tslot IR2, $1, #p1_field

      @dead IR1 IR2

      call.direct IR1, #default.baz

      ld.tslot IR1, $0, #r1_field
      call.direct IR1, #check
      @dead IR1

      ld.tslot IR1, $1, #r1_field
      call.direct IR1, #check
      @dead IR1

      movi.64 IR1, 42
      ret.64 IR1
    @end
  @end

  @method foo()I64
    @typed_slots aot:TestContainer@arec, aot:TestContainer@arec

    @code
      zero.refs $0
      zero.refs $1

      call.direct IR1, #default.newObj
      @live.ref IR1
      st.tslot IR1, $0, #r1_field

      movi.64 IR2, 0xBEBE
      st.tslot IR2, $0, #p1_field

      @dead IR1 IR2

      call.direct IR1, #default.newObj
      @live.ref IR1
      st.tslot IR1, $1, #r1_field

      movi.64 IR2, 0xB1B1
      st.tslot IR2, $1, #p1_field

      @dead IR1 IR2

      call.direct IR1, #default.bar

      ld.tslot IR1, $0, #r1_field
      call.direct IR1, #check
      @dead IR1

      ld.tslot IR1, $1, #r1_field
      call.direct IR1, #check
      @dead IR1

      movi.64 IR1, 42
      ret.64 IR1
    @end
  @end

  @method main()I64
    @saved_iregs IR12, IR13
    @code
      call.direct IR1, #startGarbageGenerator
      movi.64 IR12, 0x10000
      movi.64 IR13, 0x1
l:
      sub.64 IR12, IR12, IR13
      call.direct IR1, #default.foo
      @live.prim IR1
      bcc.64 NE, IR12, IRZ, l
      ret.64 IR1
    @end
  @end
@end
