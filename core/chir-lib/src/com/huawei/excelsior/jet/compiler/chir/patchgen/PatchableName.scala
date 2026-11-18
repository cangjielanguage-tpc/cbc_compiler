package com.huawei.excelsior.jet.compiler.chir.patchgen

import com.huawei.excelsior.jet.compiler.chir.CHIR.*

private[chir] object PatchableName {
  def syntheticConstructor(function: Func): Option[String] = function.kind match {
    case Func.Kind.ClassCtor | Func.Kind.PrimalClassCtor | Func.Kind.StructCtor | Func.Kind.PrimalStructCtor =>
      function.declaringDef.map(d => "$" + function.packageName + "$" + d.srcCodeIdentifier + "$$" + function.srcCodeIdentifier +
        (if (function.tpe.paramTypes.isEmpty) "" else function.tpe.paramTypes.map(typeName).mkString("$$", "$_", "")) + specialization(function) + "$CTOR")
    case _ => None
  }
  def guardFlag(function: Func): String = name(function, "$GVF")
  def guardMethod(function: Func): String = {
    "$" + function.packageName + "$Patch$GC" + name(function, "$GF").stripPrefix("$" + function.packageName)
  }

  private def name(function: Func, suffix: String): String = {
    val owner = function.declaringDef.map(d => "$" + d.srcCodeIdentifier + "$GC").getOrElse("")
    val params = function.tpe.paramTypes
    val signature = if (params.isEmpty) "" else params.map(typeName).mkString("$$", "$_", "")
    "$" + function.packageName + owner + "$" + function.srcCodeIdentifier + signature + specialization(function) + suffix
  }

  private def specialization(function: Func): String =
    if (function.genericDeclaration.nonEmpty || function.attributes.contains(Attribute.GenericInstantiated)) "$I$" + function.identifier.stripPrefix("@") else ""

  private def typeName(tpe: Type): String = tpe match {
    case t: RefType => typeName(t.baseType)
    case t: ClassType if t.typeDef.annotations.exists {
      case a: IsAutoEnvClass => a.value
      case _ => false
    } =>
      val signature = if (t.genericTypeParams.nonEmpty) t.genericTypeParams
        else t.typeDef.superClass.get.genericTypeParams
      signature.dropRight(1).map(typeName).mkString("(", ",", ")->") + typeName(signature.last)
    case t: CustomType =>
      val definition = t.typeDef
      val name = if (definition.srcCodeIdentifier.isEmpty) definition.identifier.stripPrefix("@")
        else definition.srcCodeIdentifier
      definition.packageName + "." + name
    case t: GenericType => t.srcCodeIdentifier
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
