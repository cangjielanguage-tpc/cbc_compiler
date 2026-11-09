package com.huawei.excelsior.jet.compiler.chir.patchgen

import com.huawei.excelsior.jet.compiler.chir.CHIR.*

private[chir] object PatchableName {
  def guardFlag(function: Func): String = name(function, "$GVF")
  def guardMethod(function: Func): String = {
    "$" + function.packageName + "$Patch$GC" + name(function, "$GF").stripPrefix("$" + function.packageName)
  }

  private def name(function: Func, suffix: String): String = {
    val owner = function.declaringDef.map(d => "$" + d.srcCodeIdentifier + "$GC").getOrElse("")
    val params = function.tpe.paramTypes
    val signature = if (params.isEmpty) "" else params.map(typeName).mkString("$$", "$_", "")
    "$" + function.packageName + owner + "$" + function.srcCodeIdentifier + signature + suffix
  }

  private def typeName(tpe: Type): String = tpe match {
    case t: RefType => typeName(t.baseType)
    case t: CustomType =>
      val definition = t.typeDef
      val name = if (definition.srcCodeIdentifier.isEmpty) definition.identifier.stripPrefix("@")
        else definition.srcCodeIdentifier
      definition.packageName + "." + name
    case t: GenericType => t.identifier
    case t: RawArrayType => "RawArray<" + typeName(t.elementType) + ">"
    case t: CPointerType => "CPointer<" + typeName(t.elementType) + ">"
    case t: VArrayType => "VArray<" + typeName(t.elementType) + "," + t.size + ">"
    case t: TupleType => t.fieldTypes.map(typeName).mkString("Tuple<", ",", ">")
    case t: BoxType => "Box<" + typeName(t.baseType) + ">"
    case t: FuncType => t.paramTypes.map(typeName).mkString("(", ",", ")->") + typeName(t.returnType)
    case BuiltinType.Boolean => "Bool"
    case t: BuiltinType => t.toString
    case _ => throw new IllegalArgumentException("Unsupported type in patchable function signature")
  }
}
