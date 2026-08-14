;strict
@main_type "default"

@aot_deps "aot"

@aot.instance xFieldOrd   = 0
@aot.instance yFieldOrd   = 1
@aot.instance zFieldOrd   = 2
@aot.instance fooFieldOrd = 3

@aot.direct testFuncName = "_CN3aot7testSumHCNY_3FooE"

@method_ref testSum = aot@aref testSum(aot:Foo@aref)I64 #testFuncName

@field_ref xFieldRef   = aot:Foo@aref x   I64          #xFieldOrd
@field_ref yFieldRef   = aot:Foo@aref y   I64          #yFieldOrd
@field_ref zFieldRef   = aot:Foo@aref z   I64          #zFieldOrd
@field_ref fooFieldRef = aot:Foo@aref foo aot:Foo@aref #fooFieldOrd

@type default

  @method main()I64
    @code
      newobj aot:Foo@aref

      mov.ref IR2, IR1
      @dead IR1
      newobj aot:Foo@aref
      st.ref.field IR1, IR2, #fooFieldRef
      @dead IR1
      mov.ref IR1, IR2
      @dead IR2

      movi.64 IR2, 12
      st.ref.field IR2, IR1, #xFieldRef

      movi.64 IR3, 13
      st.ref.field IR3, IR1, #yFieldRef

      movi.64 IR4, 14
      st.ref.field IR4, IR1, #zFieldRef

      ld.ref.field IR5, IR1, #xFieldRef
      ld.ref.field IR6, IR1, #yFieldRef
      ld.ref.field IR7, IR1, #zFieldRef
      add.64 IR13, IR5, IR6
      add.64 IR13, IR13, IR7

      call.direct IR1, #testSum
      @dead IR1
      @live.prim IR1

      add.64 IR1, IR1, IR13

      ret.64 IR1
    @end
  @end

@end
