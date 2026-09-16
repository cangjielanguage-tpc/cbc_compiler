;strict
@main_type "default"

@aot_deps "aot:cangjie-std-core"

@aot.instance xFieldOrd = 0
@aot.instance yFieldOrd = 1
@aot.direct checkObjName = "_CN3aot8checkObjHCNY_5PointE"

@method_ref checkObj = aot@aref checkObj(aot:Point@aref)I64 #checkObjName

@field_ref xPointRef   = aot:Point@aref x I64 #xFieldOrd
@field_ref yPointRef   = aot:Point@aref y I64 #yFieldOrd
@field_ref fieldRef32  = default@ref field32 I32
@field_ref fieldRef64  = default@ref field64 I64
@field_ref refFieldRef = default@ref refField aot:Point@aref

@type default

  @field refField aot:Point@aref
    @flags STATIC
  @end

  @field field32 I32
    @flags STATIC
  @end

  @field field64 I64
    @flags STATIC
  @end

  @method main()I64
    @code
      newobj aot:Point@aref
      movi.64 IR2, 123
      st.ref.field IR2, IR1, #xPointRef
      movi.64 IR3, 321
      st.ref.field IR3, IR1, #yPointRef
      st.static IR1, #refFieldRef
      @dead IR1 IR2 IR3

      movi.32 IR1, 100
      st.static IR1, #fieldRef32
      @dead IR1

      movi.64 IR1, 155
      st.static IR1, #fieldRef64
      @dead IR1

      ld.static IR1, #refFieldRef
      call.direct IR1, #checkObj
      @dead IR1

      ld.static IR2, #fieldRef32
      ld.static IR3, #fieldRef64
      add.64 IR1, IR2, IR3

      ret.64 IR1
    @end
  @end

@end
