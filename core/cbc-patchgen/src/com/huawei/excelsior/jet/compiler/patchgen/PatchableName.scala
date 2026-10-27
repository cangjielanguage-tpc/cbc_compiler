package com.huawei.excelsior.jet.compiler.patchgen

import com.huawei.excelsior.jet.compiler.chir.model.*

/** Port of `ToCjQualifiedName` / `JoinCjQualifiedNames` (C++ PatchGen.h). */
object CjQualifiedName {
  def join(types: Seq[Type], delimiter: String): String =
    types.map(toCjQualifiedName).mkString(delimiter)

  /** C++ `TYPEKIND_TO_STRING` (Type.h) for the plain-type fallback branch. */
  private val kindToString: Map[TypeKind, String] = Map(
    TypeKind.Invalid -> "Invalid",
    TypeKind.Int8 -> "Int8", TypeKind.Int16 -> "Int16", TypeKind.Int32 -> "Int32",
    TypeKind.Int64 -> "Int64", TypeKind.IntNative -> "IntNative",
    TypeKind.UInt8 -> "UInt8", TypeKind.UInt16 -> "UInt16", TypeKind.UInt32 -> "UInt32",
    TypeKind.UInt64 -> "UInt64", TypeKind.UIntNative -> "UIntNative",
    TypeKind.Float16 -> "Float16", TypeKind.Float32 -> "Float32", TypeKind.Float64 -> "Float64",
    TypeKind.Rune -> "Rune", TypeKind.Boolean -> "Bool", TypeKind.Unit -> "Unit",
    TypeKind.Nothing -> "Nothing", TypeKind.Void -> "Void", TypeKind.Tuple -> "Tuple",
    TypeKind.Struct -> "Struct", TypeKind.Enum -> "Enum", TypeKind.Class -> "Class",
    TypeKind.Func -> "Function", TypeKind.RawArray -> "RawArray", TypeKind.VArray -> "VArray",
    TypeKind.CPointer -> "CPointer", TypeKind.CString -> "CString",
    TypeKind.Generic -> "GenericType", TypeKind.Ref -> "RefType", TypeKind.Box -> "BoxType",
    TypeKind.This -> "This")

  def toCjQualifiedName(tpe: Type): String = {
    tpe match {
      case t: RawArrayType => "RawArray<" + toCjQualifiedName(t.elementType) + ">"
      case t: TypeImpl if t.kind == TypeKind.Ref => toCjQualifiedName(t.argTys.head)
      case t: VArrayType => "VArray<" + toCjQualifiedName(t.elementType) + "," + t.size + ">"
      case t: TypeImpl if t.kind == TypeKind.CPointer => "CPointer<" + toCjQualifiedName(t.argTys.head) + ">"
      case t: TypeImpl if t.kind == TypeKind.Tuple => "Tuple<" + join(t.argTys.toSeq, ",") + ">"
      case t: CustomType =>
        val defn = t.customTypeDef.getOrElse(throw new IllegalStateException("custom type without def"))
        val defName = defn.srcCodeIdentifier
        if (defName.isEmpty) {
          defn.packageName + "." + defn.identifier.drop(1)
        } else {
          defn.packageName + "." + defName
        }
      case t: FuncType => "(" + join(t.paramTypes.toSeq, ",") + ")->" + toCjQualifiedName(t.returnType)
      case t: GenericType => t.srcCodeIdentifier
      case t: TypeImpl if t.kind == TypeKind.Box => "Box<" + toCjQualifiedName(t.argTys.head) + ">"
      case t: TypeImpl => kindToString.getOrElse(t.kind, "UnknownType")
    }
  }
}

/**
  * Port of `PatchableName` (C++ PatchGen.h): represents a function/class qualified
  * name and derives the names of the corresponding guard class, guard var, etc.
  */
final class PatchableName private (pkgName: String, className: Option[String],
                                   funcName: Option[String], funcParams: Seq[Type]) {
  import PatchableName.*

  val packageName: String = pkgName
  val qualifiedName: String = {
    var q = pkgName
    className.foreach(c => q += "." + c)
    funcName.foreach(f => q += "." + f)
    q
  }

  def getClassName: String =
    className.getOrElse(throw new NoSuchElementException("class name is not present"))

  def getFuncName: String =
    funcName.getOrElse(throw new NoSuchElementException("func name is not present"))

  def genPatchClassName: String = className match {
    case Some(name) => name.stripSuffix(GuardClassSuffix) + "Impl" + GuardClassSuffix
    case None => BaseDelimiter + "PatchImpl" + GuardClassSuffix
  }

  def genSyntheticCtorName: String = {
    if (className.isEmpty) {
      throw new IllegalStateException("expected class name to be present")
    }
    genName(BaseDelimiter, "$CTOR")
  }

  def genPackageInitFlagAccessor(kind: PackageInitAccessorKind, isGetter: Boolean): String = {
    val suffix = kind match {
      case PackageInitAccessorKind.PackageInit =>
        if (isGetter) PackageInitGetterName else PackageInitSetterName
      case PackageInitAccessorKind.PackageLiteralInit =>
        if (isGetter) PackageLiteralInitGetterName else PackageLiteralInitSetterName
    }
    genName("", suffix)
  }

  def genGuardMethodName(kind: GuardMethodKind): String = kind match {
    case GuardMethodKind.Plain =>
      val guardMethodName = genName(false, None, Some(GuardFuncSuffix))
      guardMethodName.patch(packageName.length + BaseDelimiter.length, GuardClassName, 0)
    case GuardMethodKind.PackageInitCtor =>
      genName(true, Some(PackageInitCtorName), None)
    case GuardMethodKind.PackageInit =>
      genName(true, Some(PackageInitGuardMethodName), None)
    case GuardMethodKind.PackageLiteralInit =>
      genName(true, Some(PackageLiteralInitGuardMethodName), None)
  }

  def genGuardClassName(forPackageInit: Boolean): String =
    genName(forPackageInit, Some(PackageInitGuardClassName), Some(GuardClassName))

  def genGuardVarName(forPackageInit: Boolean): String =
    genName(forPackageInit, Some(PackageInitGuardVarName), Some(GuardVarSuffix))

  def genGuardVarFlagName(forPackageInit: Boolean): String =
    genName(forPackageInit, Some(PackageInitGuardVarFlagName), Some(GuardVarFlagSuffix))

  def genGuardVarInitializedName: String =
    genName(true, Some(GuardVarsInitializerName), None)

  def canEqual(other: PatchableName): Boolean = other.isInstanceOf[PatchableName]

  override def equals(other: Any): Boolean = other match {
    case other: PatchableName => qualifiedName == other.qualifiedName
    case _ => false
  }

  override def hashCode: Int = qualifiedName.hashCode

  override def toString: String = qualifiedName

  private def genName(forPackageInit: Boolean, packageInitPatchableSuffix: Option[String],
                      patchableSuffix: Option[String]): String = {
    if (forPackageInit) {
      val copy = new PatchableName(packageName, None, None, Seq.empty)
      copy.genName(GuardClassSuffix, packageInitPatchableSuffix.get)
    } else {
      genName(GuardClassSuffix, patchableSuffix.get)
    }
  }

  private def genName(classSuffix: String, suffix: String): String = {
    var name = BaseDelimiter + packageName
    className.foreach(c => name += BaseDelimiter + c + classSuffix)
    funcName.foreach { f =>
      name += BaseDelimiter + f
      val nfuncParams = funcParams.size
      if (nfuncParams > 0) {
        name += ParamStart
        for (i <- 0 until nfuncParams) {
          name += CjQualifiedName.toCjQualifiedName(funcParams(i))
          if (i < nfuncParams - 1) {
            name += AuxDelimiter
          }
        }
      }
    }
    name + suffix
  }
}

object PatchableName {
  private val BaseDelimiter = "$"
  private val ParamStart = "$$"
  private val AuxDelimiter = "$_"

  private val GuardClassSuffix = BaseDelimiter + "GC"
  private val GuardVarSuffix = BaseDelimiter + "GV"
  private val GuardVarFlagSuffix = BaseDelimiter + "GVF"
  private val GuardFuncSuffix = BaseDelimiter + "GF"

  private val GuardVarsInitializerName = BaseDelimiter + "GVI"

  private val GuardClassName = BaseDelimiter + "Patch" + GuardClassSuffix

  private val PackageInitGuardClassName = BaseDelimiter + "PackageInitPatch" + GuardClassSuffix
  private val PackageInitGuardVarName = BaseDelimiter + "packageInit" + GuardVarSuffix
  private val PackageInitGuardVarFlagName = BaseDelimiter + "packageInit" + GuardVarFlagSuffix
  private val PackageInitCtorName = PackageInitGuardClassName + BaseDelimiter + "<init>" + GuardFuncSuffix
  private val PackageInitGuardMethodName =
    PackageInitGuardClassName + BaseDelimiter + "packageInit" + GuardFuncSuffix
  private val PackageLiteralInitGuardMethodName =
    PackageInitGuardClassName + BaseDelimiter + "packageLiteralInit" + GuardFuncSuffix
  private val PackageInitGetterName = "packageInitGetter" + GuardFuncSuffix
  private val PackageInitSetterName = "packageInitSetter" + GuardFuncSuffix
  private val PackageLiteralInitGetterName = "packageLiteralInitGetter" + GuardFuncSuffix
  private val PackageLiteralInitSetterName = "packageLiteralInitSetter" + GuardFuncSuffix

  enum GuardMethodKind { case Plain, PackageInitCtor, PackageInit, PackageLiteralInit }
  enum PackageInitAccessorKind { case PackageInit, PackageLiteralInit }

  def apply(packageDef: Package): PatchableName =
    new PatchableName(packageDef.name, None, None, Seq.empty)

  def apply(typeDef: CustomTypeDef): PatchableName =
    new PatchableName(typeDef.packageName, Some(typeDef.srcCodeIdentifier), None, Seq.empty)

  def apply(func: Func): PatchableName = {
    val funcTy = func.tpe.getOrElse(throw new IllegalStateException("func without type"))
      .asInstanceOf[FuncType]
    new PatchableName(func.packageName, func.declaredParent.map(_.srcCodeIdentifier),
      Some(func.srcCodeIdentifier), funcTy.paramTypes.toSeq)
  }
}
