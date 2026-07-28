/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.common.{Language, LanguagePack}
import com.huawei.excelsior.jet.compiler.Env.{isStandalone, languagePack}
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{FindMethodImplResult, MethodReference, SignatureType, TypeKind, ClassType as SymClassType, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.Approximation.{CC, compareToPartiallyOrdered}
import com.huawei.excelsior.jet.compiler.{Environment, TypeProvider}

import scala.annotation.tailrec

object ReferenceTypes {

  private implicit var _env: Environment = _
  def env: Environment = _env ensuring (_ != null)
  def env_=(x: Environment): Unit = { assert(_env == null); _env = x; typeProvider = x.getTypeProvider }
  def setEnvForUnitTests(x: Environment): Unit = { _env = x; typeProvider = x.getTypeProvider }

  private[types] implicit var typeProvider: TypeProvider = _

  sealed abstract class ReferenceType private[ReferenceTypes] extends CompiledType with PartiallyOrdered[ReferenceType] {
    def sigType: SignatureType
    override final val symType: SymClassType = asClassType(sigType)

    require(sigType.isTraceableReference || sigType.isThinClass || symType.isAJCompoundClass || symType.isValueClass || symType.isNamespace)

    override def toString = symType.toString // TODO: use something better, like JET signature

    final override def equals(that: Any) = that match {
      case that: AnyRef if this eq that => true
      case that: ReferenceType =>
        this.symType == that.symType &&
          // Signature type contains more precise info than symlevel type
          // but lacks classloaderID, which is accounted by previous comparison of symtypes
          SignatureType.Wrapper.skip(this.sigType) == SignatureType.Wrapper.skip(that.sigType)
      case _ => false
    }

    final override def hashCode = symType.hashCode

    final def commonSuper(that: ReferenceType): ReferenceType = ReferenceType.commonSuper(this, that)
    final def compare(that: ReferenceType) = ReferenceType.compare(this, that)
    final def incomparable(that: ReferenceType) = (this compare that) == CC.Incomparable

    override def tryCompareTo[B >: ReferenceType : AsPartiallyOrdered](that: B): Option[Int] = that match {
      case that: ReferenceType => compareToPartiallyOrdered(that)(compare)
      case _ => None
    }

    /** True if this implements given interface. */
    def implements(interf: SignatureType): Boolean

    /** True if this implements given interface. */
    def implements(interf: InterfaceType): Boolean = implements(interf.sigType)

    /** True if there can be no subtype of this. */
    def isFinal: Boolean
  }

  sealed trait CohenReferenceType extends ReferenceType {
    def cohenSuper: CohenReferenceType
    final def cohenLevel: Int = if (cohenSuper == null) 0 else cohenSuper.cohenLevel + 1
  }

  sealed abstract class ClassOrInterface private[ReferenceTypes] extends ReferenceType {
    def getName: String = symType.getName
    def superclass: ClassType
  }

  final class ClassType private[ReferenceTypes](val sigType: SignatureType) extends ClassOrInterface with CohenReferenceType {
    require(sigType.isClass)

    def commonSuper(that: ClassType): ClassType = super.commonSuper(that).asInstanceOf[ClassType]

    def implements(interf: SignatureType) = symType doesImplement interf

    def isFinal = symType.isFinal

    def isAbstract: Boolean = symType.isAbstractClass

    lazy val superclass: ClassType = this match {
      case _ if isStandalone => if (symType.getSuperClassSig == null) null else ClassType(symType.getSuperClassSig)
      case ReferenceType.ajLangThinType | ReferenceType.ajLangAJObject => null
      case ReferenceType.javaLangObject => if languagePack.supports(JAVA) then ReferenceType.javaRefType else ReferenceType.ajLangLockableAJObject
      case tpe if symType.isXScalaType && tpe == ReferenceType.xscalaAnyRef => ReferenceType.scalaRefType
      case _ => ClassType(symType.getSuperClassSig)
    }

    def cohenSuper: ClassType = superclass

    /** Find method which is called when calling original method in scope of this class.
      * Returns either target method or thrown exception. */
    def findMethodImplementation(originalRef: MethodReference): FindMethodImplResult =
      symType.findMethodImplementation(originalRef)
  }

  object ClassType extends CompiledType.Companion[ClassType]

  final class InterfaceType private[ReferenceTypes](val sigType: SignatureType) extends ClassOrInterface {
    require(symType.isInterface)

    def isFinal = false

    def implements(interf: SignatureType) = symType doesImplement interf

    lazy val superclass: ClassType = symType match {
      case _ if isStandalone => null
      case tpe if tpe.isJavaReference => ReferenceType.javaLangObject
      case tpe if tpe.isXScalaType => ReferenceType.xscalaAnyRef
      case _ => ReferenceType.ajLangAJObject
    }
  }

  object InterfaceType extends CompiledType.Companion[InterfaceType]

  sealed abstract class ArrayType private[ReferenceTypes] extends ReferenceType with CohenReferenceType {
    require(symType.isArray)
    def arrayElement: CompiledType
  }

  final class AJArrayType private[ReferenceTypes](val sigType: SignatureType) extends ArrayType {
    require(symType.isAJArray)

    def implements(interf: SignatureType) = false

    def isFinal = true

    def cohenSuper: ClassType = ReferenceType.ajLangAJObject

    def arrayElement = CompiledType(symType.getArrayElemType)
  }

  object AJArrayType extends CompiledType.Companion[AJArrayType]

  final class CangjieArrayType private[ReferenceTypes](val sigType: SignatureType) extends ArrayType {
    require(symType.isCangjieArray)

    def implements(interf: SignatureType) = false

    def isFinal = true

    def cohenSuper: ClassType = ReferenceType.cangjieRefType

    def arrayElement = {
      val elemType = symType.getArrayElemType
      if (elemType.isPrimitive) {
        null
      } else {
        assert(!elemType.isRecord)
        ReferenceType.ajLangAJObject
      }
    }
  }

  object CangjieArrayType extends CompiledType.Companion[CangjieArrayType]

  // TODO: rename to JBCArrayType
  final class JavaArrayType private[ReferenceTypes](val sigType: SignatureType.JavaArray) extends ArrayType {

    def dim = sigType.dimNum

    def base = CompiledType(sigType.baseType)

    /** Element of this array.
      * Example: `(A[][]).arrayElement == A[]`.
      */
    def arrayElement: CompiledType = {
      if (dim == 1) {
        base
      } else {
        JavaArrayType(sigType.copy(dimNum = dim - 1))
      }
    }

    def implements(interf: SignatureType) = JavaArrayType.isSupertype(interf)

    def isFinal = base.isFinal

    def cohenSuper = {
      val root = ReferenceType.typeAnalysisRootBy(sigType)
      val (superBase, superDim) = base match {
        case `root` | _: PrimitiveType => (root, dim - 1)
        case base: ClassOrInterface => (base.superclass, dim)
        case _ => shouldNotReachHere()
      }
      ReferenceType(superBase, superDim).asInstanceOf[CohenReferenceType]
    }

  }

  object JavaArrayType extends CompiledType.Companion[JavaArrayType] {

    def isSupertype(t: CompiledType): Boolean = {
      if (languagePack == LanguagePack.SCALA) {
        t match {
          case ReferenceType.ajLangAJObject |
               ReferenceType.ajLangLockableAJObject |
               ReferenceType.scalaRefType |
               ReferenceType.xscalaAnyRef => true
          case _ => false
        }
      } else {
        t match {
          case ReferenceType.LanguageRoot(_) => true
          case ReferenceType.ajLangAJObject |
               ReferenceType.ajLangLockableAJObject |
               ReferenceType.javaLangObject |
               ReferenceType.javaLangCloneable |
               ReferenceType.javaIOSerializable => true
          case _ => false
        }
      }
    }

    def isSupertype(t: SignatureType): Boolean = isSupertype(CompiledType(t))
  }

  object JavaReferenceArrayType {
    /** Returns array element (may be array of less dimension). */
    def unapply(arrayType: JavaArrayType) = arrayType.arrayElement match {
      case elem: ReferenceType => Some(elem)
      case _ => None
    }
  }

  object ReferenceType extends CompiledType.Companion[ReferenceType] {
    require(typeProvider != null, "ReferenceTypes.setReferenceTypesEnv should be called before using ReferenceTypes")

    private[types] def create(sigType: SignatureType): ReferenceType = sigType.symType.getKind match {
      case TypeKind.THIN |
           TypeKind.CLASS     => new ClassType(sigType)
      case TypeKind.INTERFACE => new InterfaceType(sigType)
      case TypeKind.ARRAY =>
        if (sigType.isAJArray) new AJArrayType(sigType)
        else if (sigType.isCangjieArray) new CangjieArrayType(sigType)
        else new JavaArrayType(SignatureType.Wrapper.skip(sigType).asInstanceOf[SignatureType.JavaArray]) // TODO: use pattern matching
      case kind => shouldNotReachHere(kind)
    }

    def apply(base: SymType, dim: Int): ReferenceType = {
      require(!base.isJBCArray)
      require(dim >= 0)
      apply(if (dim > 0) typeProvider.getArrayType(base, dim) else asClassType(base))
    }

    def apply(base: ReferenceType, dim: Int): ReferenceType = apply(base.symType, dim)

    lazy val ajLangThinType = ClassType(typeProvider.getThinTypeType)
    lazy val ajLangPolyThinType = ClassType(typeProvider.getPolyThinTypeType)

    lazy val ajLangAJObject = ClassType(typeProvider.getAJObjectType)
    lazy val ajLangLockableAJObject = ClassType(typeProvider.getLockableAJObjectType)
    lazy val ajLangAJString = ClassType(typeProvider.getAJStringType)
    lazy val ajLangAJThrowable = ClassType(typeProvider.getAJThrowableType)

    lazy val ajLangAJArray = AJArrayType(typeProvider.getAJArrayType(BytecodeTypeKind.CLASS))

    lazy val javaLangObject       = ClassType(typeProvider.getObjectType)
    lazy val javaLangString       = ClassType(typeProvider.getStringType)
    lazy val javaLangClass        = ClassType(typeProvider.getClassType)
    lazy val javaLangThrowable    = ClassType(typeProvider.getThrowableType)
    lazy val javaLangRefReference = ClassType(typeProvider.getReferenceType)
    lazy val javaLangCloneable    = InterfaceType(typeProvider.getCloneableType)
    lazy val javaIOSerializable   = InterfaceType(typeProvider.getSerializableType)
    lazy val javaUtilIterator     = InterfaceType(typeProvider.getIteratorType)

    lazy val xscalaAnyRef = ClassType(typeProvider.getXScalaAnyRef)
    lazy val xscalaClass = ClassType(typeProvider.getXScalaClass)
    lazy val xscalaString = ClassType(typeProvider.getXScalaString)

    lazy val javaRefType = ClassType(typeProvider.getJavaRefType)
    lazy val scalaRefType = ClassType(typeProvider.getScalaRefType)
    lazy val cangjieRefType = ClassType(typeProvider.getCangjieRefType)

    lazy val cangjieStdCoreObject = ClassType(SignatureType.Reference("std.core:Object", jbc = false))

    object LanguageRoot {
      def unapply(t: ClassType): Option[Language] = {
        if (languagePack.supports(Language.JAVA) && t == javaRefType) {
          Some(Language.JAVA)
        } else if (languagePack.supports(Language.SCALA) && t == scalaRefType) {
          Some(Language.SCALA)
        } else if (languagePack.supports(Language.CANGJIE) && t == cangjieRefType) {
          Some(Language.CANGJIE)
        } else {
          None
        }
      }
    }

    private[ReferenceTypes] def commonSuper(t1: ReferenceType, t2: ReferenceType): ReferenceType = {
      compare(t1, t2) match {
        case CC.Equal => t1
        case CC.Greater => t1
        case CC.Less => t2

        case CC.PartiallyEqual | CC.Incomparable =>
          (t1, t2) match {
            case (t1: CohenReferenceType, t2: CohenReferenceType) =>
              @tailrec
              def iter(t1: CohenReferenceType, l1: Int, t2: CohenReferenceType, l2: Int): ReferenceType = {
                if (t1 == t2)     t1
                else if (l1 > l2) iter(t1.cohenSuper, l1 - 1, t2,            l2    )
                else if (l1 < l2) iter(t1,            l1,     t2.cohenSuper, l2 - 1)
                else              iter(t1.cohenSuper, l1 - 1, t2.cohenSuper, l2 - 1)
              }

              iter(t1, t1.cohenLevel, t2, t2.cohenLevel)

            case (t: InterfaceType, _) => commonSuper(t.superclass, t2)
            case (_, t: InterfaceType) => commonSuper(t1, t.superclass)
          }
      }
    }

    private def compareWithInterface(t: ReferenceType, interf: InterfaceType) = {
      if (t >= interf.superclass) {
        CC.Greater
      } else if (t implements interf) {
        CC.Less
      } else {
        val mayInheritorsImplement = t match {
          case _: JavaArrayType => false
          case _ if t.symType.isJavaReference != interf.symType.isJavaReference => false
          case _ => !t.isFinal
        }
        if (mayInheritorsImplement) CC.PartiallyEqual else CC.Incomparable
      }
    }

    private[ReferenceTypes] def compare(t1: ReferenceType, t2: ReferenceType): CC = {
      (t1, t2) match {
        case (x, y) if x == y => CC.Equal

        case (i1: InterfaceType, i2: InterfaceType) =>
          if (i1 implements i2) CC.Less
          else if (i2 implements i1) CC.Greater
          else CC.PartiallyEqual

        case (t: ReferenceType, interf: InterfaceType) => compareWithInterface(t, interf)
        case (interf: InterfaceType, t: ReferenceType) => compareWithInterface(t, interf).inverse

        case (t1: JavaArrayType, t2: JavaArrayType) =>
          if (t1.dim < t2.dim && JavaArrayType.isSupertype(t1.base)) {
              CC.Greater
          } else if (t1.dim > t2.dim && JavaArrayType.isSupertype(t2.base)) {
              CC.Less
          } else if (t1.dim == t2.dim) {
            (t1.base, t2.base) match {
              case (b1: ReferenceType, b2: ReferenceType) => compare(b1, b2)
              case _ => CC.Incomparable
            }
          } else {
            CC.Incomparable
          }

        case (t1: CohenReferenceType, t2: CohenReferenceType) =>
          @tailrec
          def nthSuper(t: CohenReferenceType, n: Int): ReferenceType =
            if (n == 0) t else nthSuper(t.cohenSuper, n - 1)

          val l1 = t1.cohenLevel
          val l2 = t2.cohenLevel
          val (t1Leveled, t2Leveled, res) =
            if (l1 < l2) (t1, nthSuper(t2, l2 - l1), CC.Greater)
            else         (nthSuper(t1, l1 - l2), t2, CC.Less)

          if (t1Leveled == t2Leveled) res else CC.Incomparable

        case _ => CC.Incomparable
      }
    }

    def typeAnalysisRootBy(t: SignatureType) =
      if (t.isAJManagedType) ReferenceType.ajLangAJObject
      else if (t.isCangjieType) ReferenceType.ajLangAJObject
      else if (t.isXScalaType) ReferenceType.xscalaAnyRef
      else if (t.isJavaReference) ReferenceType.javaLangObject
      else shouldNotReachHere(this)

  }
}
