;strict
@main_type "default"

@aot_deps "aot:cangjie-std-core"

@aot.static fieldName    = "_CN3aot13testStaticVarE"
@aot.static refFieldName = "_CN3aot10testRefVarE"
@aot.direct testFuncName = "_CN3aot10testAssignHl"

@method_ref testAssign = aot@aref testSum(I64)I64 #testFuncName

@field_ref gFieldRef   = aot@aref testStaticVar I64                  #fieldName
@field_ref refFieldRef = aot@aref testRefVar    std.core:Object@aref #refFieldName

@type default

  @method main()I64
    @code
      movi.64 IR1, 40
      st.static IR1, #gFieldRef

      @dead IR1
      newobj std.core:Object@aref
      st.static IR1, #refFieldRef

      @dead IR1
      movi.64 IR1, 2
      call.direct IR1, #testAssign

      @dead IR1
      movi.64 IR1, 0

      ld.static IR2, #gFieldRef

      ret.64 IR2
    @end
  @end

@end
