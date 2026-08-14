;strict
@main_type "default"

@aot_deps "aot"

@aot.instance xFieldOrd = 0
@aot.instance yFieldOrd = 1

@aot.direct aotLnk = "_CN3aot21startGarbageGeneratorHv"
@aot.direct checkLnk = "_CN3aot8checkObjHCNY_9TestClassE"

@method_ref startGarbageGenerator = aot@aref startGarbageGenerator()I64 #aotLnk
@method_ref check = aot@aref checkObj(aot:TestClass@aref)I64 #checkLnk

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
    @saved_iregs IR12, IR13

    @code
      call.direct IR1, #default.newObj
      @live.ref IR1
      mov.ref   IR12, IR1
      @dead     IR1

      call.direct IR1, #default.newObj
      @live.ref IR1
      mov.ref   IR13, IR1
      @dead     IR1

      call.direct IR1, #default.newObj
      @live.ref IR1
      mov.ref   IR4, IR1
      @dead     IR1

      call.direct IR1, #default.newObj
      @live.ref IR1
      mov.ref   IR6, IR1
      @dead     IR1

      gcpoint

      @dead IR4 IR6 IR12 IR13

      movi.64 IR1, 42
      ret.64 IR1
    @end
  @end

  @method bar()I64
    @saved_iregs IR11, IR13

    @code
      call.direct IR1, #default.newObj
      @live.ref IR1
      mov.ref   IR11, IR1
      @dead     IR1

      call.direct IR1, #default.newObj
      @live.ref IR1
      mov.ref   IR13, IR1
      @dead     IR1

      call.direct IR1, #default.baz
      @live.prim  IR1

      @dead IR11 IR13

      ret.64 IR1
    @end
  @end

  @method foo()I64
    @saved_iregs IR11, IR12, IR13

    @code
      call.direct IR1, #default.newObj
      @live.ref IR1
      mov.ref   IR11, IR1
      @dead     IR1

      call.direct IR1, #default.newObj
      @live.ref IR1
      mov.ref   IR12, IR1
      @dead     IR1

      call.direct IR1, #default.newObj
      @live.ref IR1
      mov.ref   IR13, IR1
      @dead     IR1

      call.direct IR1, #default.bar

      mov.ref IR1, IR11
      call.direct IR1, #check
      @dead IR1

      mov.ref IR1, IR12
      call.direct IR1, #check
      @dead IR1

      mov.ref IR1, IR13
      call.direct IR1, #check
      @dead IR1

      @dead IR11 IR12 IR13

      movi.64 IR1, 42
      ret.64 IR1
    @end
  @end

  @method main()I64
    @saved_iregs IR11, IR12

    @code
      call.direct IR1, #startGarbageGenerator
      movi.64 IR12, 0x10000
      movi.64 IR11, 0x1
l:
      sub.64 IR12, IR12, IR11
      call.direct IR1, #default.foo
      @live.prim IR1
      bcc.64 NE, IR12, IRZ, l
      ret.64 IR1
    @end
  @end
@end
