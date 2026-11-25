# Exact std/stdx stub inventory

See [classification and scope](std-compilation-audit.md). Full signatures uniquely identify overloads; source names alone do not.

## std.ast

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| callback-address | parseDeclFragment | std.ast._CNax17parseDeclFragmentHCNax6TokensEl(Rstd.ast:Tokens;_i64)TU&lt;Rstd.ast:Decl;_i64&gt; | assertion failed |
| callback-address | parseExprFragment | std.ast._CNax17parseExprFragmentHCNax6TokensEl(Rstd.ast:Tokens;_i64)TU&lt;Rstd.ast:Expr;_i64&gt; | assertion failed |
| callback-address | parsePatternFragment | std.ast._CNax20parsePatternFragmentHCNax6TokensEl(Rstd.ast:Tokens;_i64)TU&lt;Rstd.ast:Pattern;_i64&gt; | assertion failed |
| callback-address | parseTypeFragment | std.ast._CNax17parseTypeFragmentHCNax6TokensEl(Rstd.ast:Tokens;_i64)TU&lt;Rstd.ast:TypeNode;_i64&gt; | assertion failed |

## std.collection

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| receiver-abi | $valueset | std.collection:TreeMapEntryView._CNac16TreeMapEntryViewIG_G0_E5valuepsHNNat6OptionIG0_E$withoutTI(EOstd.core:Option;TC1&lt;TC1&gt;)U | should not reach here (extra information: arg#2 of InvokeSpecialTarget[std.collection:TreeMapEntryView._CNac16TreeMapEntryViewIG_G0_E3keypgHv()TC0] is expected to have tag VALUE &amp; type Plain but got arg Param[(LongType,1)] with tags ArraySeq(VALUE) &amp; type LongType) |

## std.convert

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| float-conversion | gv$_MAXVAL_F16 | std.convert._CGVai10MAXVAL_F16Hv()V | assertion failed: F64 |
| float-conversion | gv$_MINVAL_F16 | std.convert._CGVai10MINVAL_F16Hv()V | assertion failed: F64 |

## std.core

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| float-conversion | $Maxget | _CNatXDhUhashable$K_E._CNatXDh3MaxpgHv()f16 | assertion failed: F64 |
| float-conversion | $Minget | _CNatXDhUhashable$K_E._CNatXDh3MinpgHv()f16 | assertion failed: F64 |
| float-conversion | $MinDenormalget | _CNatXDhUhashable$K_E._CNatXDh11MinDenormalpgHv()f16 | assertion failed: F64 |
| float-conversion | $MinNormalget | _CNatXDhUhashable$K_E._CNatXDh9MinNormalpgHv()f16 | assertion failed: F64 |
| float-conversion | $MinDenormalget | _CNatXfUhashable$K_E._CNatXf11MinDenormalpgHv()f32 | assertion failed: F64 |
| float-conversion | $MinDenormalget | _CNatXdUhashable$K_E._CNatXd11MinDenormalpgHv()f64 | assertion failed: F64 |
| generic-pointer | read | _CNatXPG_U00000002dMsJaK_IG0_E._CNatXPG_4readHv()TC0 | assertion failed |
| generic-pointer | write | _CNatXPG_U00000002dMsJaK_IG0_E._CNatXPG_5writeHG_(TC0)U | assertion failed |
| generic-pointer | read | _CNatXPG_U00000002dMsJaK_IG0_E._CNatXPG_4readHl(i64)TC0 | assertion failed |
| generic-pointer | write | _CNatXPG_U00000002dMsJaK_IG0_E._CNatXPG_5writeHlG_(i64_TC0)U | assertion failed |
| dummy-intrinsic | init | std.core:Future._CNat6FutureIG_E6&lt;init&gt;HF0G_E(IR$Cg0_;&lt;TC0&gt;)V | Explicit dummy lowering for FutureInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | get | std.core:Future._CNat6FutureIG_E3getHv()TC0 | Explicit dummy lowering for FutureIsComplete; returns zero/null/no-op or constant success. |
| dummy-intrinsic | get | std.core:Future._CNat6FutureIG_E3getHv()TC0 | Explicit dummy lowering for FutureWait; returns zero/null/no-op or constant success. |
| dummy-intrinsic | tryGet | std.core:Future._CNat6FutureIG_E6tryGetHv()EOstd.core:Option;TC0&lt;TC0&gt; | Explicit dummy lowering for FutureIsComplete; returns zero/null/no-op or constant success. |
| dummy-intrinsic | get | std.core:Future._CNat6FutureIG_E3getHRNat8DurationE(Sstd.core:Duration;)TC0 | Explicit dummy lowering for FutureIsComplete; returns zero/null/no-op or constant success. |
| dummy-intrinsic | get | std.core:Future._CNat6FutureIG_E3getHRNat8DurationE(Sstd.core:Duration;)TC0 | Explicit dummy lowering for FutureWait; returns zero/null/no-op or constant success. |
| dummy-intrinsic | execute | std.core:Future._CNat6FutureIG_E7executeHv()U | Explicit dummy lowering for Sleep; returns zero/null/no-op or constant success. |
| dummy-intrinsic | execute | std.core:Future._CNat6FutureIG_E7executeHv()U | Explicit dummy lowering for SetThreadObject; returns zero/null/no-op or constant success. |
| dummy-intrinsic | execute | std.core:Future._CNat6FutureIG_E7executeHv()U | Explicit dummy lowering for FutureNotifyAll; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitForCJThreadHandle | std.core:Thread._CNat6Thread21waitForCJThreadHandleHv()U | Explicit dummy lowering for Sleep; returns zero/null/no-op or constant success. |
| dummy-intrinsic | $currentThreadget | std.core:Thread._CNat6Thread13currentThreadpgHv()Rstd.core:Thread; | Explicit dummy lowering for IsThreadObjectInited; returns zero/null/no-op or constant success. |
| dummy-intrinsic | $currentThreadget | std.core:Thread._CNat6Thread13currentThreadpgHv()Rstd.core:Thread; | Explicit dummy lowering for GetThreadObject; returns zero/null/no-op or constant success. |
| dummy-intrinsic | $currentThreadget | std.core:Thread._CNat6Thread13currentThreadpgHv()Rstd.core:Thread; | Explicit dummy lowering for SetThreadObject; returns zero/null/no-op or constant success. |
| dummy-intrinsic | dumpAllThreads | std.core:ThreadSnapshot._CNat14ThreadSnapshot14dumpAllThreadsHv()ISstd.core:Array;&lt;Rstd.core:ThreadSnapshot;&gt; | Explicit dummy lowering for DumpAllThreadsInfo; returns zero/null/no-op or constant success. |
| dummy-intrinsic | dumpCurrentThread | std.core:ThreadSnapshot._CNat14ThreadSnapshot17dumpCurrentThreadHv()Rstd.core:ThreadSnapshot; | Explicit dummy lowering for DumpCurrentThreadInfo; returns zero/null/no-op or constant success. |
| dummy-intrinsic | sleep | std.core._CNat5sleepHRNat8DurationE(Sstd.core:Duration;)U | Explicit dummy lowering for Sleep; returns zero/null/no-op or constant success. |

## std.deriving.impl

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| cast | $lambda | std.deriving.impl._CNbj8diagnoseHRNbj12InputOptionsERNbl14DerivingTargetECNbl6LoggerEEL0_E(R$ClNbj8diagnoseHRNbj12InputOptionsERNbl14DerivingTargetECNbl6LoggerEEL0_E$0$pkg$std.deriving.impl$;_TU&lt;Rstd.ast:VarDecl;_EOstd.core:Option;Rstd.ast:TypeNode;&lt;Rstd.ast:TypeNode;&gt;&gt;)TU&lt;Rstd.ast:Decl;_EOstd.core:Option;Rstd.ast:TypeNode;&lt;Rstd.ast:TypeNode;&gt;&gt; | not implemented: cast from TU&lt;Rstd.ast:VarDecl;_EOstd.core:Option;Rstd.ast:TypeNode;&lt;Rstd.ast:TypeNode;&gt;&gt; to TU&lt;Rstd.ast:Decl;_EOstd.core:Option;Rstd.ast:TypeNode;&lt;Rstd.ast:TypeNode;&gt;&gt; |

## std.interop

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| missing-intrinsic | getExportedRef | std.interop:ExportTable._CN11std.interop11ExportTable14getExportedRefHm(u64)EOstd.core:Option;Rstd.interop:ExportedRef;&lt;Rstd.interop:ExportedRef;&gt; | not implemented: intrinsic GET_EXPORTED_REF |
| missing-intrinsic | removeExportedRef | std.interop:ExportTable._CN11std.interop11ExportTable17removeExportedRefHm(u64)U | not implemented: intrinsic REMOVE_EXPORTED_REF |
| missing-intrinsic | crossAccessBarrier | std.interop:ExportTable._CN11std.interop11ExportTable18crossAccessBarrierHm(u64)U | not implemented: intrinsic CROSS_ACCESS_BARRIER |
| missing-intrinsic | createExportHandle | std.interop:ExportTable._CN11std.interop11ExportTable18createExportHandleHCNY_11ExportedRefE(Rstd.interop:ExportedRef;)u64 | not implemented: intrinsic CREATE_EXPORT_HANDLE |
| missing-intrinsic | validateHandle | std.interop:ExportedRef._CN11std.interop11ExportedRef14validateHandleHv()U | not implemented: intrinsic CREATE_EXPORT_HANDLE |

## std.math

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| math-types | logBase | std.math._CNbg7logBaseHff(f32_f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[D_LOG] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(FloatType,0)] with tags ArraySeq(VALUE) &amp; type FloatType) |
| math-types | logBase | std.math._CNbg7logBaseHDhDh(f16_f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[D_LOG] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(IntType,0)] with tags ArraySeq(VALUE) &amp; type IntType) |
| math-types | round | std.math._CNbg5roundHf(f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[D_FLOOR] is expected to have tag VALUE &amp; type DoubleType but got arg Abs[FloatType] with tags ArraySeq(CONTROL, XCONTROL, MEMORY, VALUE) &amp; type FloatType) |
| math-types | round | std.math._CNbg5roundHDh(f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[D_FLOOR] is expected to have tag VALUE &amp; type DoubleType but got arg Abs[IntType] with tags ArraySeq(CONTROL, XCONTROL, MEMORY, VALUE) &amp; type IntType) |
| math-types | sqrt | std.math._CNbg4sqrtHDh(f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[F_SQRT] is expected to have tag VALUE &amp; type FloatType but got arg Param[(IntType,0)] with tags ArraySeq(VALUE) &amp; type IntType) |
| math-types | exp | std.math._CNbg3expHf(f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[D_EXP] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(FloatType,0)] with tags ArraySeq(VALUE) &amp; type FloatType) |
| math-types | exp | std.math._CNbg3expHDh(f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[D_EXP] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(IntType,0)] with tags ArraySeq(VALUE) &amp; type IntType) |
| math-types | exp2 | std.math._CNbg4exp2Hf(f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[D_EXP2] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(FloatType,0)] with tags ArraySeq(VALUE) &amp; type FloatType) |
| math-types | exp2 | std.math._CNbg4exp2HDh(f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[D_EXP2] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(IntType,0)] with tags ArraySeq(VALUE) &amp; type IntType) |
| math-types | log10 | std.math._CNbg5log10Hf(f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[D_LOG10] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(FloatType,0)] with tags ArraySeq(VALUE) &amp; type FloatType) |
| math-types | log10 | std.math._CNbg5log10HDh(f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[D_LOG10] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(IntType,0)] with tags ArraySeq(VALUE) &amp; type IntType) |
| math-types | log2 | std.math._CNbg4log2Hf(f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[D_LOG2] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(FloatType,0)] with tags ArraySeq(VALUE) &amp; type FloatType) |
| math-types | log2 | std.math._CNbg4log2HDh(f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[D_LOG2] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(IntType,0)] with tags ArraySeq(VALUE) &amp; type IntType) |
| math-types | log | std.math._CNbg3logHf(f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[D_LOG] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(FloatType,0)] with tags ArraySeq(VALUE) &amp; type FloatType) |
| math-types | log | std.math._CNbg3logHDh(f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[D_LOG] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(IntType,0)] with tags ArraySeq(VALUE) &amp; type IntType) |
| math-types | floor | std.math._CNbg5floorHf(f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[D_FLOOR] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(FloatType,0)] with tags ArraySeq(VALUE) &amp; type FloatType) |
| math-types | floor | std.math._CNbg5floorHDh(f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[D_FLOOR] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(IntType,0)] with tags ArraySeq(VALUE) &amp; type IntType) |
| math-types | ceil | std.math._CNbg4ceilHf(f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[D_CEIL] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(FloatType,0)] with tags ArraySeq(VALUE) &amp; type FloatType) |
| math-types | ceil | std.math._CNbg4ceilHDh(f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[D_CEIL] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(IntType,0)] with tags ArraySeq(VALUE) &amp; type IntType) |
| math-types | trunc | std.math._CNbg5truncHf(f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[D_TRUNC] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(FloatType,0)] with tags ArraySeq(VALUE) &amp; type FloatType) |
| math-types | trunc | std.math._CNbg5truncHDh(f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[D_TRUNC] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(IntType,0)] with tags ArraySeq(VALUE) &amp; type IntType) |
| math-types | sin | std.math._CNbg3sinHf(f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[D_SIN] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(FloatType,0)] with tags ArraySeq(VALUE) &amp; type FloatType) |
| math-types | sin | std.math._CNbg3sinHDh(f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[D_SIN] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(IntType,0)] with tags ArraySeq(VALUE) &amp; type IntType) |
| math-types | cos | std.math._CNbg3cosHf(f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[D_COS] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(FloatType,0)] with tags ArraySeq(VALUE) &amp; type FloatType) |
| math-types | cos | std.math._CNbg3cosHDh(f16)f16 | should not reach here (extra information: arg#0 of MathIntrinsic[D_COS] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(IntType,0)] with tags ArraySeq(VALUE) &amp; type IntType) |
| math-types | pow | std.math._CNbg3powHff(f32_f32)f32 | should not reach here (extra information: arg#0 of MathIntrinsic[F_POW] is expected to have tag VALUE &amp; type DoubleType but got arg Param[(FloatType,0)] with tags ArraySeq(VALUE) &amp; type FloatType) |
| math-types | pow | std.math._CNbg3powHfi(f32_i32)f32 | should not reach here (extra information: arg#1 of PowI[FloatType] is expected to have tag VALUE &amp; type LongType but got arg Param[(IntType,1)] with tags ArraySeq(VALUE) &amp; type IntType) |

## std.reflect

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| missing-intrinsic | $nameget | std.reflect:GlobalVariableInfo._CNam18GlobalVariableInfo4namepgHv()Sstd.core:String; | not implemented: intrinsic GET_STATIC_FIELD_NAME |
| missing-intrinsic | $typeInfoget | std.reflect:GlobalVariableInfo._CNam18GlobalVariableInfo8typeInfopgHv()Rstd.reflect:TypeInfo; | not implemented: intrinsic GET_STATIC_FIELD_TYPE |
| missing-intrinsic | isMutable | std.reflect:GlobalVariableInfo._CNam18GlobalVariableInfo9isMutableHv()b | not implemented: intrinsic GET_STATIC_FIELD_MODIFIER |
| missing-intrinsic | getValue | std.reflect:GlobalVariableInfo._CNam18GlobalVariableInfo8getValueHv()Rstd.core:Any; | not implemented: intrinsic GET_STATIC_FIELD_VALUE |
| missing-intrinsic | setValue | std.reflect:GlobalVariableInfo._CNam18GlobalVariableInfo8setValueHCNat3AnyE(Rstd.core:Any;)U | not implemented: intrinsic SET_STATIC_FIELD_VALUE |
| missing-intrinsic | $nameget | std.reflect:StaticVariableInfo._CNam18StaticVariableInfo4namepgHv()Sstd.core:String; | not implemented: intrinsic GET_STATIC_FIELD_NAME |
| missing-intrinsic | $typeInfoget | std.reflect:StaticVariableInfo._CNam18StaticVariableInfo8typeInfopgHv()Rstd.reflect:TypeInfo; | not implemented: intrinsic GET_STATIC_FIELD_TYPE |
| missing-intrinsic | $modifiersget | std.reflect:StaticVariableInfo._CNam18StaticVariableInfo9modifierspgHv()IRstd.core:Collection;&lt;EPstd.reflect:ModifierInfo;&lt;&gt;&gt; | not implemented: intrinsic GET_STATIC_FIELD_MODIFIER |
| missing-intrinsic | isMutable | std.reflect:StaticVariableInfo._CNam18StaticVariableInfo9isMutableHv()b | not implemented: intrinsic GET_STATIC_FIELD_MODIFIER |
| missing-intrinsic | getValue | std.reflect:StaticVariableInfo._CNam18StaticVariableInfo8getValueHv()Rstd.core:Any; | not implemented: intrinsic GET_STATIC_FIELD_VALUE |
| missing-intrinsic | setValue | std.reflect:StaticVariableInfo._CNam18StaticVariableInfo8setValueHCNat3AnyE(Rstd.core:Any;)U | not implemented: intrinsic SET_STATIC_FIELD_VALUE |
| missing-intrinsic | $nameget | std.reflect:InstanceVariableInfo._CNam20InstanceVariableInfo4namepgHv()Sstd.core:String; | not implemented: intrinsic GET_INSTANCE_FIELD_NAME |
| missing-intrinsic | $typeInfoget | std.reflect:InstanceVariableInfo._CNam20InstanceVariableInfo8typeInfopgHv()Rstd.reflect:TypeInfo; | not implemented: intrinsic GET_INSTANCE_FIELD_TYPE |
| missing-intrinsic | $modifiersget | std.reflect:InstanceVariableInfo._CNam20InstanceVariableInfo9modifierspgHv()IRstd.core:Collection;&lt;EPstd.reflect:ModifierInfo;&lt;&gt;&gt; | not implemented: intrinsic GET_INSTANCE_FIELD_MODIFIER |
| missing-intrinsic | isMutable | std.reflect:InstanceVariableInfo._CNam20InstanceVariableInfo9isMutableHv()b | not implemented: intrinsic GET_INSTANCE_FIELD_MODIFIER |
| missing-intrinsic | getValue | std.reflect:InstanceVariableInfo._CNam20InstanceVariableInfo8getValueHCNat3AnyE(Rstd.core:Any;)Rstd.core:Any; | not implemented: intrinsic GET_FIELD_VALUE |
| missing-intrinsic | setValue | std.reflect:InstanceVariableInfo._CNam20InstanceVariableInfo8setValueHCNat3AnyEY3_(Rstd.core:Any;_Rstd.core:Any;)U | not implemented: intrinsic SET_FIELD_VALUE |
| missing-intrinsic | of | std.reflect:TypeInfo.of()Rstd.reflect:TypeInfo; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | of | std.reflect:TypeInfo.of(Rstd.core:Any;)Rstd.reflect:TypeInfo; | not implemented: intrinsic GET_TYPE_FOR_ANY |
| missing-intrinsic | of | std.reflect:TypeInfo.of(Rstd.core:Object;)Rstd.reflect:ClassTypeInfo; | not implemented: intrinsic GET_OBJ_CLASS |
| missing-intrinsic | get | std.reflect:TypeInfo.get(Sstd.core:String;)Rstd.reflect:TypeInfo; | not implemented: intrinsic IS_GENERIC |
| missing-intrinsic | getInfoPointerByQualifiedName | std.reflect:TypeInfo._CNam8TypeInfo29getInfoPointerByQualifiedNameHRNat6StringE(Sstd.core:String;)TU&lt;Sstd.core:String;_PU&gt; | not implemented: intrinsic GET_TYPE_BY_QUALIFIED_NAME |
| missing-intrinsic | getOrCreate | std.reflect:TypeInfo.getOrCreate(PU)Rstd.reflect:TypeInfo; | not implemented: intrinsic IS_GENERIC |
| missing-intrinsic | getTypeInfoByPointer | std.reflect:TypeInfo.getTypeInfoByPointer(PU_Sstd.core:String;)Rstd.reflect:TypeInfo; | not implemented: intrinsic IS_PRIMITIVE |
| missing-intrinsic | $qualifiedNameget | std.reflect:TypeInfo._CNam8TypeInfo13qualifiedNamepgHv()Sstd.core:String; | not implemented: intrinsic IS_BOX |
| missing-intrinsic | getQualifiedName | std.reflect:TypeInfo._CNam8TypeInfo16getQualifiedNameHPu(PU)Sstd.core:String; | not implemented: intrinsic GET_TYPE_NAME |
| missing-intrinsic | $superInterfacesget | std.reflect:TypeInfo._CNam8TypeInfo15superInterfacespgHv()IRstd.core:Collection;&lt;Rstd.reflect:InterfaceTypeInfo;&gt; | not implemented: intrinsic GET_NUM_OF_INTERFACE |
| missing-intrinsic | $modifiersget | std.reflect:TypeInfo._CNam8TypeInfo9modifierspgHv()IRstd.core:Collection;&lt;EPstd.reflect:ModifierInfo;&lt;&gt;&gt; | not implemented: intrinsic GET_TYPE_INFO_MODIFIER |
| missing-intrinsic | isSubtypeOf | std.reflect:TypeInfo._CNam8TypeInfo11isSubtypeOfHCNamY0_E(Rstd.reflect:TypeInfo;)b | not implemented: intrinsic IS_BOX |
| missing-intrinsic | getInstanceFunctions | std.reflect:TypeInfo._CNam8TypeInfo20getInstanceFunctionsHv()U | not implemented: intrinsic GET_NUM_OF_INSTANCE_METHOD_INFOS |
| missing-intrinsic | getStaticFunctions | std.reflect:TypeInfo._CNam8TypeInfo18getStaticFunctionsHv()U | not implemented: intrinsic GET_NUM_OF_STATIC_METHOD_INFOS |
| missing-intrinsic | parse | std.reflect:TypeTemplateParser._CNam18TypeTemplateParser5parseHRNat6StringE(Sstd.core:String;)Rstd.reflect:TypeInfo; | not implemented: intrinsic GET_OR_CREATE_TYPEINFO_FOR_REFLECT |
| missing-intrinsic | get | std.reflect:TypeTemplate._CNam12TypeTemplate3getHRNat6StringE(Sstd.core:String;)Rstd.reflect:TypeTemplate; | not implemented: intrinsic GET_TYPETEMPLATE |
| missing-intrinsic | $elementsget | std.reflect:TupleTypeInfo._CNam13TupleTypeInfo8elementspgHv()IRstd.collection:ReadOnlyList;&lt;Rstd.reflect:TypeInfo;&gt; | not implemented: intrinsic GET_NUM_OF_FIELD_TYPES |
| missing-intrinsic | of | std.reflect:TupleTypeInfo.of()Rstd.reflect:TupleTypeInfo; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | construct | std.reflect:TupleTypeInfo._CNam13TupleTypeInfo9constructHRNat5ArrayICNat3AnyEE(ISstd.core:Array;&lt;Rstd.core:Any;&gt;)Rstd.core:Any; | not implemented: intrinsic NEW_AND_INIT_OBJECT |
| missing-intrinsic | destruct | std.reflect:TupleTypeInfo._CNam13TupleTypeInfo8destructHCNat3AnyE(Rstd.core:Any;)IRstd.collection:ReadOnlyList;&lt;Rstd.core:Any;&gt; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | of | std.reflect:StructTypeInfo.of()Rstd.reflect:StructTypeInfo; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | of | std.reflect:PrimitiveTypeInfo.of()Rstd.reflect:PrimitiveTypeInfo; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | $indexget | std.reflect:ParameterInfo._CNam13ParameterInfo5indexpgHv()i64 | not implemented: intrinsic GET_PARAMETER_INDEX |
| missing-intrinsic | $nameget | std.reflect:ParameterInfo._CNam13ParameterInfo4namepgHv()Sstd.core:String; | not implemented: intrinsic GET_PARAMETER_NAME |
| missing-intrinsic | $typeInfoget | std.reflect:ParameterInfo._CNam13ParameterInfo8typeInfopgHv()Rstd.reflect:TypeInfo; | not implemented: intrinsic GET_PARAMETER_TYPE |
| missing-intrinsic | $annotationsget | std.reflect:ParameterInfo._CNam13ParameterInfo11annotationspgHv()IRstd.core:Collection;&lt;Rstd.core:Object;&gt; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | get | std.reflect:PackageInfo._CNam11PackageInfo3getHRNat6StringE(Sstd.core:String;)Rstd.reflect:PackageInfo; | not implemented: intrinsic GET_PACKAGE_BY_QUALIFIEDNAME |
| missing-intrinsic | load | std.reflect:PackageInfo._CNam11PackageInfo4loadHRNat6StringE(Sstd.core:String;)Rstd.reflect:PackageInfo; | not implemented: intrinsic LOAD_PACKAGE |
| missing-intrinsic | $qualifiedNameget | std.reflect:PackageInfo._CNam11PackageInfo13qualifiedNamepgHv()Sstd.core:String; | not implemented: intrinsic GET_PACKAGE_NAME |
| missing-intrinsic | $typeInfosget | std.reflect:PackageInfo._CNam11PackageInfo9typeInfospgHv()IRstd.core:Collection;&lt;Rstd.reflect:TypeInfo;&gt; | not implemented: intrinsic GET_PACKAGE_NUM_OF_TYPE_INFOS |
| missing-intrinsic | $versionget | std.reflect:PackageInfo._CNam11PackageInfo7versionpgHv()Sstd.core:String; | not implemented: intrinsic GET_PACKAGE_VERSION |
| missing-intrinsic | getSubPackagesMap | std.reflect:PackageInfo._CNam11PackageInfo17getSubPackagesMapHv()IRstd.collection:HashMap;&lt;Sstd.core:String;_Rstd.reflect:PackageInfo;&gt; | not implemented: intrinsic GET_SUB_PACKAGES |
| missing-intrinsic | getTypeInfos | std.reflect:PackageInfo._CNam11PackageInfo12getTypeInfosHjPuCNac9ArrayListICNam8TypeInfoEE(u32_PU_IRstd.collection:ArrayList;&lt;Rstd.reflect:TypeInfo;&gt;)U | not implemented: intrinsic GET_PACKAGE_TYPE_INFO |
| missing-intrinsic | $variablesget | std.reflect:PackageInfo._CNam11PackageInfo9variablespgHv()IRstd.core:Collection;&lt;Rstd.reflect:GlobalVariableInfo;&gt; | not implemented: intrinsic GET_PACKAGE_NUM_OF_GLOBAL_FIELD_INFOS |
| missing-intrinsic | getGlobalVariableInfos | std.reflect:PackageInfo._CNam11PackageInfo22getGlobalVariableInfosHjPuCNac9ArrayListICNam18GlobalVariableInfoEE(u32_PU_IRstd.collection:ArrayList;&lt;Rstd.reflect:GlobalVariableInfo;&gt;)U | not implemented: intrinsic GET_PACKAGE_GLOBAL_FIELD_INFO |
| missing-intrinsic | $functionsget | std.reflect:PackageInfo._CNam11PackageInfo9functionspgHv()IRstd.core:Collection;&lt;Rstd.reflect:GlobalFunctionInfo;&gt; | not implemented: intrinsic GET_PACKAGE_NUM_OF_GLOBAL_METHODS |
| missing-intrinsic | getGlobalFunctionInfos | std.reflect:PackageInfo._CNam11PackageInfo22getGlobalFunctionInfosHjPuCNac9ArrayListICNam18GlobalFunctionInfoEE(u32_PU_IRstd.collection:ArrayList;&lt;Rstd.reflect:GlobalFunctionInfo;&gt;)U | not implemented: intrinsic GET_PACKAGE_GLOBAL_METHOD_INFO |
| missing-intrinsic | of | std.reflect:InterfaceTypeInfo.of()Rstd.reflect:InterfaceTypeInfo; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | getSignatureTypes | std.reflect:FunctionTypeInfo._CNam16FunctionTypeInfo17getSignatureTypesHv()IRstd.collection:ReadOnlyList;&lt;Rstd.reflect:TypeInfo;&gt; | not implemented: intrinsic GET_NUM_OF_FUNCTION_SIGNATURETYPES |
| missing-intrinsic | of | std.reflect:FunctionTypeInfo.of()Rstd.reflect:FunctionTypeInfo; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | apply | std.reflect:FunctionTypeInfo._CNam16FunctionTypeInfo5applyHCNat3AnyERNat5ArrayIY3_E(Rstd.core:Any;_ISstd.core:Array;&lt;Rstd.core:Any;&gt;)Rstd.core:Any; | not implemented: intrinsic APPLY_CJ_METHOD |
| missing-intrinsic | $returnTypeget | std.reflect:GlobalFunctionInfo._CNam18GlobalFunctionInfo10returnTypepgHv()Rstd.reflect:TypeInfo; | not implemented: intrinsic GET_METHOD_RETURN_TYPE |
| missing-intrinsic | apply | std.reflect:GlobalFunctionInfo._CNam18GlobalFunctionInfo5applyHRNat5ArrayICNat3AnyEE(ISstd.core:Array;&lt;Rstd.core:Any;&gt;)Rstd.core:Any; | not implemented: intrinsic APPLY_CJ_STATIC_METHOD |
| missing-intrinsic | apply | std.reflect:GlobalFunctionInfo._CNam18GlobalFunctionInfo5applyHRNat5ArrayICNam8TypeInfoEERNatY5_ICNat3AnyEE(ISstd.core:Array;&lt;Rstd.reflect:TypeInfo;&gt;_ISstd.core:Array;&lt;Rstd.core:Any;&gt;)Rstd.core:Any; | not implemented: intrinsic CHECK_METHOD_ACTUAL_ARGS |
| missing-intrinsic | $isAbstractget | std.reflect:ConstructorInfo._CNam15ConstructorInfo10isAbstractpgHv()b | not implemented: intrinsic GET_METHOD_MODIFIER |
| missing-intrinsic | apply | std.reflect:ConstructorInfo._CNam15ConstructorInfo5applyHRNat5ArrayICNat3AnyEE(ISstd.core:Array;&lt;Rstd.core:Any;&gt;)Rstd.core:Any; | not implemented: intrinsic METHOD_ENTRYPOINT_IS_NULL |
| missing-intrinsic | $returnTypeget | std.reflect:StaticFunctionInfo._CNam18StaticFunctionInfo10returnTypepgHv()Rstd.reflect:TypeInfo; | not implemented: intrinsic GET_METHOD_RETURN_TYPE |
| missing-intrinsic | $modifiersget | std.reflect:StaticFunctionInfo._CNam18StaticFunctionInfo9modifierspgHv()IRstd.core:Collection;&lt;EPstd.reflect:ModifierInfo;&lt;&gt;&gt; | not implemented: intrinsic GET_METHOD_MODIFIER |
| missing-intrinsic | apply | std.reflect:StaticFunctionInfo._CNam18StaticFunctionInfo5applyHCNam8TypeInfoERNat5ArrayICNat3AnyEE(Rstd.reflect:TypeInfo;_ISstd.core:Array;&lt;Rstd.core:Any;&gt;)Rstd.core:Any; | not implemented: intrinsic METHOD_ENTRYPOINT_IS_NULL |
| missing-intrinsic | apply | std.reflect:StaticFunctionInfo._CNam18StaticFunctionInfo5applyHCNam8TypeInfoERNat5ArrayIY3_ERNatY7_ICNat3AnyEE(Rstd.reflect:TypeInfo;_ISstd.core:Array;&lt;Rstd.reflect:TypeInfo;&gt;_ISstd.core:Array;&lt;Rstd.core:Any;&gt;)Rstd.core:Any; | not implemented: intrinsic METHOD_ENTRYPOINT_IS_NULL |
| missing-intrinsic | $returnTypeget | std.reflect:InstanceFunctionInfo._CNam20InstanceFunctionInfo10returnTypepgHv()Rstd.reflect:TypeInfo; | not implemented: intrinsic GET_METHOD_RETURN_TYPE |
| missing-intrinsic | $modifiersget | std.reflect:InstanceFunctionInfo._CNam20InstanceFunctionInfo9modifierspgHv()IRstd.core:Collection;&lt;EPstd.reflect:ModifierInfo;&lt;&gt;&gt; | not implemented: intrinsic GET_METHOD_MODIFIER |
| missing-intrinsic | apply | std.reflect:InstanceFunctionInfo._CNam20InstanceFunctionInfo5applyHCNat3AnyERNat5ArrayIY3_E(Rstd.core:Any;_ISstd.core:Array;&lt;Rstd.core:Any;&gt;)Rstd.core:Any; | not implemented: intrinsic METHOD_ENTRYPOINT_IS_NULL |
| missing-intrinsic | apply | std.reflect:InstanceFunctionInfo._CNam20InstanceFunctionInfo5applyHCNat3AnyERNat5ArrayICNam8TypeInfoEERNatY7_IY3_E(Rstd.core:Any;_ISstd.core:Array;&lt;Rstd.reflect:TypeInfo;&gt;_ISstd.core:Array;&lt;Rstd.core:Any;&gt;)Rstd.core:Any; | not implemented: intrinsic METHOD_ENTRYPOINT_IS_NULL |
| missing-intrinsic | of | std.reflect:EnumConstructorInfo._CNam19EnumConstructorInfo2ofHCNat3AnyE(Rstd.core:Any;)Rstd.reflect:EnumConstructorInfo; | not implemented: intrinsic GET_ENUM_CONSTRUCTOR_INFO_FROM_ANY |
| missing-intrinsic | apply | std.reflect:EnumConstructorInfo._CNam19EnumConstructorInfo5applyHRNat5ArrayICNat3AnyEE(ISstd.core:Array;&lt;Rstd.core:Any;&gt;)Rstd.core:Any; | not implemented: intrinsic NEW_AND_INIT_OBJECT |
| missing-intrinsic | $constructorNameget | std.reflect:EnumConstructorInfo._CNam19EnumConstructorInfo15constructorNamepgHv()Sstd.core:String; | not implemented: intrinsic GET_ENUM_CONSTRUCTOR_NAME |
| missing-intrinsic | computeParameters | std.reflect:EnumConstructorInfo._CNam19EnumConstructorInfo17computeParametersHv()IRstd.collection:ArrayList;&lt;Rstd.reflect:TypeInfo;&gt; | not implemented: intrinsic GET_NUM_OF_FIELD_TYPES |
| missing-intrinsic | $constructorsget | std.reflect:EnumTypeInfo._CNam12EnumTypeInfo12constructorspgHv()IRstd.core:Collection;&lt;Rstd.reflect:EnumConstructorInfo;&gt; | not implemented: intrinsic GET_NUM_OF_ENUM_CONSTRUCTOR_INFOS |
| missing-intrinsic | of | std.reflect:EnumTypeInfo.of()Rstd.reflect:EnumTypeInfo; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | destruct | std.reflect:EnumTypeInfo._CNam12EnumTypeInfo8destructHCNat3AnyE(Rstd.core:Any;)TU&lt;Rstd.reflect:EnumConstructorInfo;_IRstd.collection:ReadOnlyList;&lt;Rstd.core:Any;&gt;&gt; | not implemented: intrinsic GET_ENUM_CONSTRUCTOR_INFO_FROM_ANY |
| missing-intrinsic | of | std.reflect:ClassTypeInfo.of()Rstd.reflect:ClassTypeInfo; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | $superClassget | std.reflect:ClassTypeInfo._CNam13ClassTypeInfo10superClasspgHv()EOstd.core:Option;Rstd.reflect:ClassTypeInfo;&lt;Rstd.reflect:ClassTypeInfo;&gt; | not implemented: intrinsic GET_SUPER_TYPE_INFO |
| missing-intrinsic | getEnumSuperTypeInfo | std.reflect._CNam20getEnumSuperTypeInfoHCNam12EnumTypeInfoE(Rstd.reflect:EnumTypeInfo;)EOstd.core:Option;Rstd.reflect:EnumTypeInfo;&lt;Rstd.reflect:EnumTypeInfo;&gt; | not implemented: intrinsic GET_SUPER_TYPE_INFO |
| missing-intrinsic | getGenericParameters | std.reflect._CNam20getGenericParametersHPu(PU)IRstd.collection:ArrayList;&lt;Rstd.reflect:GenericTypeInfo;&gt; | not implemented: intrinsic GET_NUM_OF_GENERIC_PARAMETERS |
| missing-intrinsic | getAnnotations | std.reflect._CNam14getAnnotationsHPu(PU)IRstd.core:Collection;&lt;Rstd.core:Object;&gt; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | checkMethodGenericArgs | std.reflect._CNam22checkMethodGenericArgsHPuRNat5ArrayIPuERNatY5_ICNat3AnyEE(PU_ISstd.core:Array;&lt;PU&gt;_ISstd.core:Array;&lt;Rstd.core:Any;&gt;)U | not implemented: intrinsic CHECK_METHOD_ACTUAL_ARGS |
| missing-intrinsic | getFuncName | std.reflect._CNam11getFuncNameHPu(PU)Sstd.core:String; | not implemented: intrinsic GET_METHOD_NAME |
| missing-intrinsic | getParameters | std.reflect._CNam13getParametersHPu(PU)IRstd.collection:ArrayList;&lt;Rstd.reflect:ParameterInfo;&gt; | not implemented: intrinsic GET_NUM_OF_ACTUAL_PARAMETERS |
| missing-intrinsic | getTypeAnnotations | std.reflect._CNam18getTypeAnnotationsHPu(PU)IRstd.core:Collection;&lt;Rstd.core:Object;&gt; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | getInstanceFieldNameString | std.reflect._CNam26getInstanceFieldNameStringHPuPu(PU_PU)Sstd.core:String; | not implemented: intrinsic GET_INSTANCE_FIELD_NAME |
| missing-intrinsic | getStaticFieldNameString | std.reflect._CNam24getStaticFieldNameStringHPuRNat6StringE(PU_Sstd.core:String;)Sstd.core:String; | not implemented: intrinsic GET_STATIC_FIELD_NAME |
| missing-intrinsic | getInstanceFieldTypeInfo | std.reflect._CNam24getInstanceFieldTypeInfoHPuPu(PU_PU)Rstd.reflect:TypeInfo; | not implemented: intrinsic GET_INSTANCE_FIELD_TYPE |
| missing-intrinsic | getStaticFieldTypeInfo | std.reflect._CNam22getStaticFieldTypeInfoHPu(PU)Rstd.reflect:TypeInfo; | not implemented: intrinsic GET_STATIC_FIELD_TYPE |
| missing-intrinsic | getInstanceFieldAnnotationInfos | std.reflect._CNam31getInstanceFieldAnnotationInfosHPu(PU)IRstd.core:Collection;&lt;Rstd.core:Object;&gt; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | getStaticFieldAnnotationInfos | std.reflect._CNam29getStaticFieldAnnotationInfosHPu(PU)IRstd.core:Collection;&lt;Rstd.core:Object;&gt; | not implemented: intrinsic GET_TYPE_BY_MANGLED_NAME |
| missing-intrinsic | getInstanceVariableInfos | std.reflect._CNam24getInstanceVariableInfosHPuRNat6StringEb(PU_Sstd.core:String;_b)ISstd.core:Array;&lt;Rstd.reflect:InstanceVariableInfo;&gt; | not implemented: intrinsic GET_NUM_OF_INSTANCE_FIELD_INFOS |
| missing-intrinsic | getStaticVariableInfos | std.reflect._CNam22getStaticVariableInfosHPuRNat6StringE(PU_Sstd.core:String;)ISstd.core:Array;&lt;Rstd.reflect:StaticVariableInfo;&gt; | not implemented: intrinsic GET_NUM_OF_STATIC_FIELD_INFOS |

## std.runtime

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| missing-intrinsic | $threadCountget | std.runtime:ThreadInfo._CNao10ThreadInfo11threadCountpgHv()i64 | not implemented: intrinsic GET_THREAD_NUMBER |
| missing-intrinsic | $blockingThreadCountget | std.runtime:ThreadInfo._CNao10ThreadInfo19blockingThreadCountpgHv()i64 | not implemented: intrinsic GET_BLOCKING_THREAD_NUMBER |
| missing-intrinsic | $nativeThreadCountget | std.runtime:ThreadInfo._CNao10ThreadInfo17nativeThreadCountpgHv()i64 | not implemented: intrinsic GET_NATIVE_THREAD_NUMBER |
| missing-intrinsic | $maxHeapSizeget | std.runtime:MemoryInfo._CNao10MemoryInfo11maxHeapSizepgHv()i64 | not implemented: intrinsic GET_MAX_HEAP_SIZE |
| missing-intrinsic | $allocatedHeapSizeget | std.runtime:MemoryInfo._CNao10MemoryInfo17allocatedHeapSizepgHv()i64 | not implemented: intrinsic GET_ALLOCATE_HEAP_SIZE |
| missing-intrinsic | GC | std.runtime._CNao2GCHb(b)U | not implemented: intrinsic INVOKE_GC |
| missing-intrinsic | SetGCThreshold | std.runtime._CNao14SetGCThresholdHm(u64)U | not implemented: intrinsic SET_GC_THRESHOLD |
| missing-intrinsic | gc | std.runtime._CNao2gcHb(b)U | not implemented: intrinsic INVOKE_GC |
| missing-intrinsic | setGCThreshold | std.runtime._CNao14setGCThresholdHm(u64)U | not implemented: intrinsic SET_GC_THRESHOLD |
| missing-intrinsic | getGCCount | std.runtime._CNao10getGCCountHv()i64 | not implemented: intrinsic GET_GC_COUNT |
| missing-intrinsic | getGCTime | std.runtime._CNao9getGCTimeHv()i64 | not implemented: intrinsic GET_GC_TIME_US |
| missing-intrinsic | getGCFreedSize | std.runtime._CNao14getGCFreedSizeHv()i64 | not implemented: intrinsic GET_GC_FREED_SIZE |
| missing-intrinsic | boxDumpCjHeapData | std.runtime._CNao17boxDumpCjHeapDataHi(i32)b | not implemented: intrinsic DUMP_CJ_HEAP_DATA |
| missing-intrinsic | getMaxHeapSize | std.runtime._CNao14getMaxHeapSizeHv()i64 | not implemented: intrinsic GET_MAX_HEAP_SIZE |
| missing-intrinsic | getAllocatedHeapSize | std.runtime._CNao20getAllocatedHeapSizeHv()i64 | not implemented: intrinsic GET_ALLOCATE_HEAP_SIZE |
| missing-intrinsic | getUsedHeapSize | std.runtime._CNao15getUsedHeapSizeHv()i64 | not implemented: intrinsic GET_REAL_HEAP_SIZE |
| missing-intrinsic | dumpHeapData | std.runtime._CNao12dumpHeapDataHRNan4PathE(Sstd.fs:Path;)U | not implemented: intrinsic DUMP_CJ_HEAP_DATA |
| missing-intrinsic | boxStopCjCPUProfiling | std.runtime._CNao21boxStopCjCPUProfilingHi(i32)b | not implemented: intrinsic STOP_CJ_CPU_PROFILING |
| missing-intrinsic | startCPUProfiling | std.runtime._CNao17startCPUProfilingHv()U | not implemented: intrinsic START_CJ_CPU_PROFILING |
| missing-intrinsic | stopCPUProfiling | std.runtime._CNao16stopCPUProfilingHRNan4PathE(Sstd.fs:Path;)U | not implemented: intrinsic STOP_CJ_CPU_PROFILING |
| callback-address | static.init | std.runtime._CNao13SignalManager13&lt;static.init&gt;Hv()V | assertion failed |
| callback-address | unregSigChain | std.runtime._CNao13unregSigChainHCNao6SignalE(Rstd.runtime:Signal;)U | assertion failed |
| missing-intrinsic | getThreadCount | std.runtime._CNao14getThreadCountHv()i64 | not implemented: intrinsic GET_THREAD_NUMBER |
| missing-intrinsic | getBlockingThreadCount | std.runtime._CNao22getBlockingThreadCountHv()i64 | not implemented: intrinsic GET_BLOCKING_THREAD_NUMBER |
| missing-intrinsic | getNativeThreadCount | std.runtime._CNao20getNativeThreadCountHv()i64 | not implemented: intrinsic GET_NATIVE_THREAD_NUMBER |
| callback-address | gv$_$init | std.runtime._CGVao13SignalManager5$initHv()V | assertion failed |

## std.sync

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| atomic-option | init | std.sync:AtomicOptionReference._CNab21AtomicOptionReferenceIG_E6&lt;init&gt;HNNat6OptionIG_E(EOstd.core:Option;TC0&lt;TC0&gt;)V | should not reach here (extra information: OptionLikeEnum(std.core:Option,ArraySeq(ClassTypeVariable(0)),ClassTypeVariable(0))) |
| atomic-option | load | std.sync:AtomicOptionReference._CNab21AtomicOptionReferenceIG_E4loadHv()EOstd.core:Option;TC0&lt;TC0&gt; | should not reach here (extra information: OptionLikeEnum(std.core:Option,ArraySeq(ClassTypeVariable(0)),ClassTypeVariable(0))) |
| atomic-option | load | std.sync:AtomicOptionReference._CNab21AtomicOptionReferenceIG_E4loadHNNab11MemoryOrderE(EZstd.sync:MemoryOrder;&lt;&gt;)EOstd.core:Option;TC0&lt;TC0&gt; | should not reach here (extra information: OptionLikeEnum(std.core:Option,ArraySeq(ClassTypeVariable(0)),ClassTypeVariable(0))) |
| atomic-option | store | std.sync:AtomicOptionReference._CNab21AtomicOptionReferenceIG_E5storeHNNat6OptionIG_E(EOstd.core:Option;TC0&lt;TC0&gt;)U | should not reach here (extra information: OptionLikeEnum(std.core:Option,ArraySeq(ClassTypeVariable(0)),ClassTypeVariable(0))) |
| atomic-option | store | std.sync:AtomicOptionReference._CNab21AtomicOptionReferenceIG_E5storeHNNat6OptionIG_ENNab11MemoryOrderE(EOstd.core:Option;TC0&lt;TC0&gt;_EZstd.sync:MemoryOrder;&lt;&gt;)U | should not reach here (extra information: OptionLikeEnum(std.core:Option,ArraySeq(ClassTypeVariable(0)),ClassTypeVariable(0))) |
| atomic-option | swap | std.sync:AtomicOptionReference._CNab21AtomicOptionReferenceIG_E4swapHNNat6OptionIG_E(EOstd.core:Option;TC0&lt;TC0&gt;)EOstd.core:Option;TC0&lt;TC0&gt; | should not reach here (extra information: OptionLikeEnum(std.core:Option,ArraySeq(ClassTypeVariable(0)),ClassTypeVariable(0))) |
| atomic-option | swap | std.sync:AtomicOptionReference._CNab21AtomicOptionReferenceIG_E4swapHNNat6OptionIG_ENNab11MemoryOrderE(EOstd.core:Option;TC0&lt;TC0&gt;_EZstd.sync:MemoryOrder;&lt;&gt;)EOstd.core:Option;TC0&lt;TC0&gt; | should not reach here (extra information: OptionLikeEnum(std.core:Option,ArraySeq(ClassTypeVariable(0)),ClassTypeVariable(0))) |
| atomic-option | compareAndSwap | std.sync:AtomicOptionReference._CNab21AtomicOptionReferenceIG_E14compareAndSwapHNNat6OptionIG_EY5_(EOstd.core:Option;TC0&lt;TC0&gt;_EOstd.core:Option;TC0&lt;TC0&gt;)b | should not reach here (extra information: OptionLikeEnum(std.core:Option,ArraySeq(ClassTypeVariable(0)),ClassTypeVariable(0))) |
| atomic-option | compareAndSwap | std.sync:AtomicOptionReference._CNab21AtomicOptionReferenceIG_E14compareAndSwapHNNat6OptionIG_EY5_NNab11MemoryOrderEY8_(EOstd.core:Option;TC0&lt;TC0&gt;_EOstd.core:Option;TC0&lt;TC0&gt;_EZstd.sync:MemoryOrder;&lt;&gt;_EZstd.sync:MemoryOrder;&lt;&gt;)b | should not reach here (extra information: OptionLikeEnum(std.core:Option,ArraySeq(ClassTypeVariable(0)),ClassTypeVariable(0))) |
| dummy-intrinsic | add | std.sync:TimerScheduler._CNab14TimerScheduler3addHCNab5TimerE(Rstd.sync:Timer;)Rstd.core:Any; | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | schedule | std.sync:TimerScheduler._CNab14TimerScheduler8scheduleHv()U | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitIf | std.sync:SyncList._CNab8SyncList6waitIfHF0bERNat8DurationE(R$CiF0bvE;_Sstd.core:Duration;)U | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitIf | std.sync:SyncList._CNab8SyncList6waitIfHF0bERNat8DurationE(R$CiF0bvE;_Sstd.core:Duration;)U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitIf | std.sync:SyncList._CNab8SyncList6waitIfHF0bERNat8DurationE(R$CiF0bvE;_Sstd.core:Duration;)U | Explicit dummy lowering for MutexUnlock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitInLock | std.sync:SyncList._CNab8SyncList10waitInLockHF0uERNat8DurationE(R$CiF0uvE;_Sstd.core:Duration;)b | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitInLock | std.sync:SyncList._CNab8SyncList10waitInLockHF0uERNat8DurationE(R$CiF0uvE;_Sstd.core:Duration;)b | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitInLock | std.sync:SyncList._CNab8SyncList10waitInLockHF0uERNat8DurationE(R$CiF0uvE;_Sstd.core:Duration;)b | Explicit dummy lowering for MutexUnlock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notifyOne | std.sync:SyncList._CNab8SyncList9notifyOneHv()b | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notifyOne | std.sync:SyncList._CNab8SyncList9notifyOneHv()b | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notifyOne | std.sync:SyncList._CNab8SyncList9notifyOneHv()b | Explicit dummy lowering for MutexUnlock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notifyAll | std.sync:SyncList._CNab8SyncList9notifyAllHv()b | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notifyAll | std.sync:SyncList._CNab8SyncList9notifyAllHv()b | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notifyAll | std.sync:SyncList._CNab8SyncList9notifyAllHv()b | Explicit dummy lowering for MutexUnlock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:SyncList._CNab8SyncList6&lt;init&gt;Hv()V | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:NotifyOnce._CNab10NotifyOnce6&lt;init&gt;Hv()V | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:NotifyOnce._CNab10NotifyOnce6&lt;init&gt;Hv()V | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:NotifyOnce._CNab10NotifyOnce6&lt;init&gt;Hv()V | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:NotifyOnce._CNab10NotifyOnce6&lt;init&gt;Hv()V | Explicit dummy lowering for MutexUnlock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notify | std.sync:NotifyOnce._CNab10NotifyOnce6notifyHv()U | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notify | std.sync:NotifyOnce._CNab10NotifyOnce6notifyHv()U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notify | std.sync:NotifyOnce._CNab10NotifyOnce6notifyHv()U | Explicit dummy lowering for MutexUnlock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitWithoutTimeout | std.sync:NotifyOnce._CNab10NotifyOnce18waitWithoutTimeoutHv()U | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitWithoutTimeout | std.sync:NotifyOnce._CNab10NotifyOnce18waitWithoutTimeoutHv()U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitWithoutTimeout | std.sync:NotifyOnce._CNab10NotifyOnce18waitWithoutTimeoutHv()U | Explicit dummy lowering for MutexUnlock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitWithTimeout | std.sync:NotifyOnce._CNab10NotifyOnce15waitWithTimeoutHRNat8DurationE(Sstd.core:Duration;)U | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitWithTimeout | std.sync:NotifyOnce._CNab10NotifyOnce15waitWithTimeoutHRNat8DurationE(Sstd.core:Duration;)U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitWithTimeout | std.sync:NotifyOnce._CNab10NotifyOnce15waitWithTimeoutHRNat8DurationE(Sstd.core:Duration;)U | Explicit dummy lowering for MutexUnlock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:SyncCounter._CNab11SyncCounter6&lt;init&gt;Hl(i64)V | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | acquire | std.sync:Semaphore._CNab9Semaphore7acquireHl(i64)U | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | tryAcquire | std.sync:Semaphore._CNab9Semaphore10tryAcquireHl(i64)b | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | release | std.sync:Semaphore._CNab9Semaphore7releaseHl(i64)U | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:ReadWriteLockImpl._CNab17ReadWriteLockImpl6&lt;init&gt;Hb(b)V | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:ReentrantWriteMutex._CNab19ReentrantWriteMutex6&lt;init&gt;HCNab17ReadWriteLockImplE(Rstd.sync:ReadWriteLockImpl;)V | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:ReentrantReadMutex._CNab18ReentrantReadMutex6&lt;init&gt;HCNab17ReadWriteLockImplE(Rstd.sync:ReadWriteLockImpl;)V | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:ReentrantReadWriteMutex._CNab23ReentrantReadWriteMutex6&lt;init&gt;HNNab18ReadWriteMutexModeE(EPstd.sync:ReadWriteMutexMode;&lt;&gt;)V | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:ReentrantMutex._CNab14ReentrantMutex6&lt;init&gt;Hv()V | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | lock | std.sync:ReentrantMutex.lock()U | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | tryLock | std.sync:ReentrantMutex.tryLock()b | Explicit dummy lowering for MutexTryLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | unlock | std.sync:ReentrantMutex.unlock()U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | unlock | std.sync:ReentrantMutex.unlock()U | Explicit dummy lowering for MutexUnlock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | checkMutexStatus | std.sync:ReentrantMutex._CNab14ReentrantMutex16checkMutexStatusHv()U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | wait | std.sync:ReadWriteLockCondition.wait(Sstd.core:Duration;)b | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | wait | std.sync:ReadWriteLockCondition.wait(Sstd.core:Duration;)b | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | wait | std.sync:ReadWriteLockCondition.wait(Sstd.core:Duration;)b | Explicit dummy lowering for MutexUnlock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:ReadWriteLockCondition._CNab22ReadWriteLockCondition6&lt;init&gt;HCNab17ReadWriteLockImplE(Rstd.sync:ReadWriteLockImpl;)V | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | condition | std.sync:WriteLock.condition()Rstd.sync:Condition; | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:MutexCondition._CNab14MutexCondition6&lt;init&gt;HCNab5MutexE(Rstd.sync:Mutex;)V | Explicit dummy lowering for MonitorInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | wait | std.sync:MutexCondition.wait()U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | wait | std.sync:MutexCondition.wait()U | Explicit dummy lowering for MoitiorWait; returns zero/null/no-op or constant success. |
| dummy-intrinsic | wait | std.sync:MutexCondition.wait(Sstd.core:Duration;)b | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | wait | std.sync:MutexCondition.wait(Sstd.core:Duration;)b | Explicit dummy lowering for MoitiorWait; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitUntil | std.sync:MutexCondition.waitUntil(R$CiF0bvE;)U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitUntil | std.sync:MutexCondition.waitUntil(R$CiF0bvE;)U | Explicit dummy lowering for MoitiorWait; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitUntil | std.sync:MutexCondition.waitUntil(R$CiF0bvE;_Sstd.core:Duration;)b | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | waitUntil | std.sync:MutexCondition.waitUntil(R$CiF0bvE;_Sstd.core:Duration;)b | Explicit dummy lowering for MoitiorWait; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notify | std.sync:MutexCondition.notify()U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notify | std.sync:MutexCondition.notify()U | Explicit dummy lowering for MoitiorNotify; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notifyAll | std.sync:MutexCondition.notifyAll()U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notifyAll | std.sync:MutexCondition.notifyAll()U | Explicit dummy lowering for MoitiorNotifyAll; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:Mutex._CNab5Mutex6&lt;init&gt;Hv()V | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | lock | std.sync:Mutex.lock()U | Explicit dummy lowering for CjMutexLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | tryLock | std.sync:Mutex.tryLock()b | Explicit dummy lowering for MutexTryLock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | unlock | std.sync:Mutex.unlock()U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | unlock | std.sync:Mutex.unlock()U | Explicit dummy lowering for MutexUnlock; returns zero/null/no-op or constant success. |
| dummy-intrinsic | condition | std.sync:Mutex.condition()Rstd.sync:Condition; | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | condition | std.sync:Mutex.condition()Rstd.sync:Condition; | Explicit dummy lowering for MonitorInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | checkMutexStatus | std.sync:Mutex._CNab5Mutex16checkMutexStatusHv()U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:MultiConditionMonitor._CNab21MultiConditionMonitor6&lt;init&gt;Hv()V | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | newCondition | std.sync:MultiConditionMonitor._CNab21MultiConditionMonitor12newConditionHv()Sstd.sync:ConditionID; | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | newCondition | std.sync:MultiConditionMonitor._CNab21MultiConditionMonitor12newConditionHv()Sstd.sync:ConditionID; | Explicit dummy lowering for WaitQueueInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | wait | std.sync:MultiConditionMonitor._CNab21MultiConditionMonitor4waitHRNab11ConditionIDERNat8DurationE(Sstd.sync:ConditionID;_Sstd.core:Duration;)b | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | wait | std.sync:MultiConditionMonitor._CNab21MultiConditionMonitor4waitHRNab11ConditionIDERNat8DurationE(Sstd.sync:ConditionID;_Sstd.core:Duration;)b | Explicit dummy lowering for MultiConditionWait; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notify | std.sync:MultiConditionMonitor._CNab21MultiConditionMonitor6notifyHRNab11ConditionIDE(Sstd.sync:ConditionID;)U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notify | std.sync:MultiConditionMonitor._CNab21MultiConditionMonitor6notifyHRNab11ConditionIDE(Sstd.sync:ConditionID;)U | Explicit dummy lowering for MultiConditionNotify; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notifyAll | std.sync:MultiConditionMonitor._CNab21MultiConditionMonitor9notifyAllHRNab11ConditionIDE(Sstd.sync:ConditionID;)U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notifyAll | std.sync:MultiConditionMonitor._CNab21MultiConditionMonitor9notifyAllHRNab11ConditionIDE(Sstd.sync:ConditionID;)U | Explicit dummy lowering for MultiConditionNotifyAll; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:WaitQueue._CNab9WaitQueue6&lt;init&gt;Hv()V | Explicit dummy lowering for WaitQueueInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:Monitor._CNab7Monitor6&lt;init&gt;Hv()V | Explicit dummy lowering for MutexInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:Monitor._CNab7Monitor6&lt;init&gt;Hv()V | Explicit dummy lowering for MonitorInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | init | std.sync:Monitor._CNab7Monitor6&lt;init&gt;HCNab5MutexE(Rstd.sync:Mutex;)V | Explicit dummy lowering for MonitorInit; returns zero/null/no-op or constant success. |
| dummy-intrinsic | wait | std.sync:Monitor._CNab7Monitor4waitHRNat8DurationE(Sstd.core:Duration;)b | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | wait | std.sync:Monitor._CNab7Monitor4waitHRNat8DurationE(Sstd.core:Duration;)b | Explicit dummy lowering for MoitiorWait; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notify | std.sync:Monitor._CNab7Monitor6notifyHv()U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notify | std.sync:Monitor._CNab7Monitor6notifyHv()U | Explicit dummy lowering for MoitiorNotify; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notifyAll | std.sync:Monitor._CNab7Monitor9notifyAllHv()U | Explicit dummy lowering for MutexCheckStatus; returns zero/null/no-op or constant success. |
| dummy-intrinsic | notifyAll | std.sync:Monitor._CNab7Monitor9notifyAllHv()U | Explicit dummy lowering for MoitiorNotifyAll; returns zero/null/no-op or constant success. |

## std.unittest

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| field-instantiation | addSuccessfullStep | std.unittest:TestCaseResult._CNaq14TestCaseResult18addSuccessfullStepHCNaq13RunStepResultE(Rstd.unittest:RunStepResult;)U | assertion failed: InstantiatedReference(std.collection:ArrayList,ArraySeq(CangjieNamedReference(std.unittest:RunStepResult))) != InstantiatedReference(std.collection:ArrayList,ArraySeq(ClassTypeVariable(0))) in 62:GetFieldSeqRef[Plain,LongType](60,5,5,61) |
| field-instantiation | hasFailures | std.unittest:TestCaseResult._CNaq14TestCaseResult11hasFailuresHv()b | assertion failed: InstantiatedReference(std.collection:ArrayList,ArraySeq(CangjieNamedReference(std.unittest:RunStepResult))) != InstantiatedReference(std.collection:ArrayList,ArraySeq(ClassTypeVariable(0))) in 16:GetFieldSeqRef[Plain,LongType](14,4,4,15) |
| float-conversion | warmupAndConfigure | std.unittest:BenchRunner._CNaq11BenchRunner18warmupAndConfigureHv()U | assertion failed: F64 |
| float-conversion | doMicroBench | std.unittest:BenchRunner._CNaq11BenchRunner12doMicroBenchHll(i64_i64)Sstd.core:Duration; | assertion failed: F64 |
| float-conversion | measure | std.unittest:Runtime._CNaq7Runtime7measureHv()f64 | assertion failed: F64 |
| float-conversion | $allocatedPerIterationget | std.unittest:WarmupInfo._CNaq10WarmupInfo21allocatedPerIterationpgHv()f64 | assertion failed: F64 |
| float-conversion | chooseWarmupGC | std.unittest:WarmupInfo._CNaq10WarmupInfo14chooseWarmupGCHv()U | assertion failed: F64 |

## std.unittest.diff

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| generic-dispatch |  | _CNcbXRNat5ArrayIG_EUcollections$K_IG0_E.pprintForAssertion(Rstd.unittest.common:PrettyPrinter;_BOXISstd.core:Array;&lt;TC0&gt;_Sstd.core:String;_Sstd.core:String;_i64)Rstd.unittest.common:PrettyPrinter; | should not reach here (extra information: could not find VTable slot for pprintDifferentSizes(Rstd.unittest.common:PrettyPrinter;_BOXISstd.core:Array;&lt;TC0&gt;_Sstd.core:String;_Sstd.core:String;)U in List(InstantiatedRecord(std.core:Array,ArraySeq(ClassTypeVariable(0))))) |
| enum-return | compareByDist | std.unittest.diff:MultilineDiffBuilder._CNcb20MultilineDiffBuilder13compareByDistHRNat5ArrayIcEY3_l(ISstd.core:Array;&lt;c32&gt;_ISstd.core:Array;&lt;c32&gt;_i64)EPstd.unittest.diff:_CNcbU00000000XFlbD6WeakEqE;&lt;&gt; | should not reach here (extra information: arg#2 of Return[IntType] is expected to have tag VALUE &amp; type IntType but got arg VerificationNode with tags ArraySeq(VALUE) &amp; type com.huawei.excelsior.jet.compiler.opt.ir.Types$ValueType$@42501b25) |
| generic-dispatch | $lambda | std.unittest.diff._CNcb25AssertPrintableCollectionIG_E18pprintForAssertionHCNby13PrettyPrinterEG_RNat6StringEY8_lEL_E.1(IR$ClNcb25AssertPrintableCollectionIG_E18pprintForAssertionHCNby13PrettyPrinterEG_RNat6StringEY8_lEL_E.1$0$pkg$std.unittest.diff$;&lt;TL0&gt;)U | should not reach here (extra information: could not find VTable slot for pprintDifferentSizes(Rstd.unittest.common:PrettyPrinter;_BOXISstd.core:Array;&lt;TC0&gt;_Sstd.core:String;_Sstd.core:String;)U in List(InstantiatedRecord(std.core:Array,ArraySeq(ClassTypeVariable(0))))) |

## std.unittest.prop_test

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| generic-instanceof | shrink | _CNbxXCNac7HashMapIG_G0_EUshrink$K0_IG1_G2_E.shrink()IRstd.core:Iterable;&lt;IRstd.collection:HashMap;&lt;TC0_TC1&gt;&gt; | should not reach here (extra information: arg#0 of InstanceOfGeneric[InstantiatedReference(std.unittest.prop_test:_CNbxUshrink$17CanShrinkElementsIG_E,ArraySeq(InstantiatedReference(std.collection:HashMap,ArraySeq(ClassTypeVariable(0), ClassTypeVariable(1)))))] is expected to have tag VALUE &amp; type Plain but got arg StackAlloc[Local(InstantiatedRecord(std.unittest.prop_test:_CNbxUshrink$16ShrinkKeyWrapperIG_G0_E,ArraySeq(ClassTypeVariable(0), ClassTypeVariable(1))))] with tags ArraySeq(VALUE) &amp; type RecordAddrType(InstantiatedRecord(std.unittest.prop_test:_CNbxUshrink$16ShrinkKeyWrapperIG_G0_E,ArraySeq(ClassTypeVariable(0), ClassTypeVariable(1))))) |
| generic-instanceof | shrink | _CNbxXRNat5ArrayIG_EUshrink$K0_IG0_E._CNbxXRNat5ArrayIG_E6shrinkHv()IRstd.core:Iterable;&lt;ISstd.core:Array;&lt;TC0&gt;&gt; | should not reach here (extra information: arg#0 of InstanceOfGeneric[InstantiatedReference(std.unittest.prop_test:_CNbxUshrink$17CanShrinkElementsIG_E,ArraySeq(InstantiatedRecord(std.core:Array,ArraySeq(ClassTypeVariable(0)))))] is expected to have tag VALUE &amp; type Plain but got arg Param[(RecordAddrType(InstantiatedRecord(std.core:Array,ArraySeq(ClassTypeVariable(0)))),0)] with tags ArraySeq(VALUE) &amp; type RecordAddrType(InstantiatedRecord(std.core:Array,ArraySeq(ClassTypeVariable(0))))) |
| math-types | shrinkTo | _CNbxXfUshrink$K_E._CNbxXfUshrink$K_8shrinkToHCNac7HashSetIfE(IRstd.collection:HashSet;&lt;f32&gt;)U | should not reach here (extra information: arg#0 of MathIntrinsic[D_LOG10] is expected to have tag VALUE &amp; type DoubleType but got arg Abs[FloatType] with tags ArraySeq(CONTROL, XCONTROL, MEMORY, VALUE) &amp; type FloatType) |
| float-conversion | nextDenseFloat64 | std.unittest.prop_test._CNbxU00000002iDzC416nextDenseFloat64HCNav6RandomE(Rstd.random:Random;)f64 | assertion failed: F64 |

## stdx.crypto.common

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| generic-pointer | $valueget | stdx.crypto.common:NativePointerResource._CN18stdx.crypto.common21NativePointerResourceIG_E5valuepgHv()TC0 | assertion failed |
| generic-pointer | $valueset | stdx.crypto.common:NativePointerResource._CN18stdx.crypto.common21NativePointerResourceIG_E5valuepsHG_(TC0)U | assertion failed |
| generic-pointer | init | stdx.crypto.common:NativePointerResource._CN18stdx.crypto.common21NativePointerResourceIG_E6&lt;init&gt;HlNNat6OptionIG_E(i64_EOstd.core:Option;TC0&lt;TC0&gt;)V | assertion failed |
| generic-pointer | mallocCopyOf | stdx.crypto.common._CN18stdx.crypto.common12mallocCopyOfIG_HRNat5ArrayIG_E(ISstd.core:Array;&lt;TL0&gt;)IRstdx.crypto.common:NativePointerResource;&lt;TL0&gt; | assertion failed |

## stdx.crypto.crypto

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| float-conversion | nextFloat64 | stdx.crypto.crypto:SecureRandom._CN18stdx.crypto.crypto12SecureRandom11nextFloat64Hv()f64 | assertion failed: F64 |

## stdx.effect

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| cast | tryHandle | stdx.effect:ImmediateHandlerCase.tryHandle(IRstdx.effect:Command;&lt;TL0&gt;_Rstdx.effect:FrameBox;)EOstd.core:Option;TL0&lt;TL0&gt; | not implemented: cast from IRstdx.effect:Command;&lt;TL0&gt; to TC0 |
| generic-dispatch | perf | stdx.effect:HandlerFrame.perf(TL0)TL1 | should not reach here (extra information: could not find VTable slot for tryHandle(IRstdx.effect:Command;&lt;TL1&gt;_Rstdx.effect:FrameBox;)EOstd.core:Option;TL1&lt;TL1&gt; in List(CangjieNamedReference(stdx.effect:HandlerCase))) |

## stdx.fuzz

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| callback-address | startFuzz | stdx.fuzz:Fuzzer._CN9stdx.fuzz6Fuzzer9startFuzzHv()U | assertion failed |

## stdx.net.http

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| cast | protocolService | stdx.net.http:Server._CN13stdx.net.http6Server15protocolServiceHCNaf15StreamingSocketE(Rstd.net:StreamingSocket;)Rstdx.net.http:ProtocolService; | not implemented: cast from TU&lt;ECstdx.net.http:Protocol;&lt;&gt;_Rstdx.net.tls.common:TlsConnection;&gt; to TU&lt;ECstdx.net.http:Protocol;&lt;&gt;_Rstd.net:StreamingSocket;&gt; |

## stdx.net.tls

| Kind / category | Source name | Exact compiler method signature | Reason |
|---|---|---|---|
| callback-address | signCallbackWrapper | _CN12stdx.net.tlsXCNY_10TlsContextEUhandshake$K_E._CN12stdx.net.tlsXCNY_10TlsContextEUhandshake$K_19signCallbackWrapperHv()P(BS_BS_Pu8_i64_Pi64)Pu8 | assertion failed |
| callback-address | decryptCallbackWrapper | _CN12stdx.net.tlsXCNY_10TlsContextEUhandshake$K_E._CN12stdx.net.tlsXCNY_10TlsContextEUhandshake$K_22decryptCallbackWrapperHv()P(BS_Pu8_i64_Pi64)Pu8 | assertion failed |
| generic-pointer | $valueget | stdx.net.tls:NativePointerResource._CN12stdx.net.tls21NativePointerResourceIG_E5valuepgHv()TC0 | assertion failed |
| generic-pointer | $valueset | stdx.net.tls:NativePointerResource._CN12stdx.net.tls21NativePointerResourceIG_E5valuepsHG_(TC0)U | assertion failed |
| generic-pointer | init | stdx.net.tls:NativePointerResource._CN12stdx.net.tls21NativePointerResourceIG_E6&lt;init&gt;HlNNat6OptionIG_E(i64_EOstd.core:Option;TC0&lt;TC0&gt;)V | assertion failed |
| cstring-null | withStream | stdx.net.tls:SocketConnected._CN12stdx.net.tls15SocketConnected10withStreamIkHF0kPRNY_3SslEE(R$CiF0kPRN12stdx.net.tls3SslEE;)BS | CString (of class com.huawei.excelsior.jet.compiler.chir.CHIR$BuiltinType$$anon$2) |
| cstring-null | getVersion | stdx.net.tls:TlsSocket._CN12stdx.net.tls9TlsSocket10getVersionHv()EPstdx.net.tls.common:TlsVersion;&lt;&gt; | CString (of class com.huawei.excelsior.jet.compiler.chir.CHIR$BuiltinType$$anon$2) |
| callback-address | static.init | stdx.net.tls._CN12stdx.net.tls16TlsServerSession13&lt;static.init&gt;Hv()V | assertion failed |
| generic-pointer | mallocCopyOf | stdx.net.tls._CN12stdx.net.tls12mallocCopyOfIG_HRNat5ArrayIG_E(ISstd.core:Array;&lt;TL0&gt;)IRstdx.net.tls:NativePointerResource;&lt;TL0&gt; | assertion failed |
| callback-address | gv$_$init | stdx.net.tls._CGV12stdx.net.tls16TlsServerSession5$initHv()V | assertion failed |
| callback-address | $lambda | stdx.net.tls._CN12stdx.net.tls10TlsContext13createContextHbbEL_E(R$ClN12stdx.net.tls10TlsContext13createContextHbbEL_E$0$pkg$stdx.net.tls$;_PSstdx.net.tls:ExceptionData;)PSstdx.net.tls:Ctx; | assertion failed |
| callback-address | $lambda | stdx.net.tls._CN12stdx.net.tlsXCNY_10TlsContextE22setContextCustomVerifyHF0bRNat5ArrayICN18stdx.crypto.common11CertificateEEEEL_E(R$ClN12stdx.net.tlsXCNY_10TlsContextE22setContextCustomVerifyHF0bRNat5ArrayICN18stdx.crypto.common11CertificateEEEEL_E$0$pkg$stdx.net.tls$;_PSstdx.net.tls:Ctx;_PSstdx.net.tls:ExceptionData;)U | assertion failed |
