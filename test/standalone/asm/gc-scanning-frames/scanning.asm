;strict
@main_type "default"

@aot_deps "aot"

@aot.instance xFieldOrd = 0
@aot.instance yFieldOrd = 1

@aot.direct aotLnk = "_CN3aot21startGarbageGeneratorHv"

@method_ref startGarbageGenerator = aot@aref startGarbageGenerator()I64 #aotLnk

@method_ref default.newObj = default@ref newObj()aot:TestClass@aref
@method_ref default.foo = default@ref foo()I64
@method_ref default.bar = default@ref bar()I64
@method_ref default.baz = default@ref baz()I64

@field_ref xFieldRef = aot:TestClass@aref x I64 #xFieldOrd
@field_ref yFieldRef = aot:TestClass@aref y I64 #yFieldOrd

@type default

  @method newObj()aot:TestClass@aref
    @code
      movi.64 IR2, 123
      movi.64 IR3, 321
      newobj aot:TestClass@aref
      st.ref.field IR2, IR1, #xFieldRef
      st.ref.field IR3, IR1, #yFieldRef
      @dead IR2 IR3
      ret.ref IR1
    @end
  @end

  @method baz()I64
    @untyped_count 0x2

    @code
      movi.64 IR2, 0xABAB
      st.uslot.64 IR2, $0

      call.direct IR1, #default.newObj
      @live.ref IR1
      st.uslot.ref IR1, $1

      @dead IR1 IR2

      gcpoint

      movi.64 IR1, 42
      ret.64 IR1
    @end
  @end

  @method bar()I64
    @untyped_count 0x4

    @code
      call.direct IR1, #default.newObj
      @live.ref IR1
      st.uslot.ref IR1, $0

      movi.64 IR2, 0xABAB
      st.uslot.64 IR2, $1

      @dead IR1 IR2

      call.direct IR1, #default.newObj
      @live.ref IR1
      st.uslot.ref IR1, $2

      movi.64 IR2, 0xBABA
      st.uslot.64 IR2, $3

      @dead IR1 IR2

      call.direct IR1, #default.baz
      @live.prim IR1

      ret.64 IR1
    @end
  @end

  @method foo()I64
    @untyped_count 0x2

    @code
      call.direct IR1, #default.newObj
      @live.ref IR1
      st.uslot.ref IR1, $0

      movi.64 IR2, 0xBEBE
      st.uslot.64 IR2, $1

      @dead IR1 IR2

      call.direct IR1, #default.bar
      @live.prim IR1

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
