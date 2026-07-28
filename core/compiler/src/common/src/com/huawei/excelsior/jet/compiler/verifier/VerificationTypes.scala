/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.classfile.{SignatureParser, SignatureTraverser}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.verifier.VerificationTypes.Kind
import xscala.util.StringOps.*

import scala.collection.mutable

/** Type system for Java bytecode verification.
  * Heavily based on symbolic type references with lazy resolve.
  *
  * This system has representation of primitive types, reference types
  * and special verification types (e.g. top, uninitialized this, ...).
  * Note that all interfaces are treated like Object.
  * More information may be found in JVM specification
  * (Chapter 4.10.1.2. Verification Type System).
  *
  * {{{
  *                                top
  *      _________________________/ | \_______
  *     /   /      /      /         |         \
  *   int  long  float  double  long half  reference or return address
  *                                           /                \
  *                                          /                  \
  *                                   reference            return address
  *                  _________________/   |   \__________
  *                 /                     |              \
  *          ____Object____     uninitialized new     uninitialized this
  *         /              \
  *         |    classes   |
  *         |     and      |
  *         |    arrays    |
  *         \______________/
  *                |
  *                |
  *               null
  * }}}
  *
  * @author kit
  * @author cypok
  */
object VerificationTypes {
  enum Kind {
    case
      TOP,
      VOID, BYTE, SHORT, CHAR, INT, LONG, FLOAT, DOUBLE,
      CLASS,
      NULL,
      ANY_REFERENCE,
      RETURN_ADDRESS,
      REFERENCE_OR_RETURN_ADDRESS,
      CATCH_TYPE_PLACEHOLDER,
      LONG_HALF,
      UNINITIALIZED,
      UNINITIALIZED_THIS

    def isPrimitive = this match {
      case VOID | BYTE | SHORT | CHAR | INT | LONG | FLOAT | DOUBLE => true
      case _ => false
    }

    def is2Slots = this == LONG || this == DOUBLE

    def isShortIntegral = this == BYTE || this == SHORT || this == CHAR
  }
}

final class VerificationTypes(tp: TypeProvider, builder: AbstractVerifier.InfoBuilder, refClass: VerifiableType) {

  final case class VerificationType (baseKind: Kind, baseClassName: XString = null, dimNum: Int = 0,
                                    uninitializedOffset: Int = -1, private[VerificationTypes] var resolvedBase: VerifiableType = null) {

    assert(baseKind != null)
    assert(baseClassName == null || baseClassName.charAt(0) != '['.toByte, s"dimensions should be separated from $baseClassName")
    assert(dimNum == 0 || (dimNum > 0 && ((baseKind == Kind.CLASS) || baseKind.isPrimitive)))

    def isAssignableFrom(that: VerificationTypes#VerificationType, context: VerifiableMethod): Boolean = {
      if (this == that || this == TOP) {
        true

      } else if (this == REFERENCE_OR_RETURN_ADDRESS) {
        that == RETURN_ADDRESS || REFERENCE.isAssignableFrom(that, context)

      } else if (this == REFERENCE) {
        that.isUninitializedNew || that.isUninitializedThis || OBJECT.isAssignableFrom(that, context)

      } else if (that.isNull) {
        this.isClassOrArrayOrNull

      } else if (this.isNull) {
        false

      } else if (this.isArray || that.isArray) {
        if (this.dimNum > that.dimNum) {
          false

        } else if (this.dimNum == that.dimNum) {
          if (this.baseKind != that.baseKind) {
            false
          } else {
            assert(this.baseKind == Kind.CLASS)
            this.isAssignableBaseClassFrom(that, context)
          }

        } else {
          if (this.baseKind != Kind.CLASS) {
            false
          } else if (this.isBaseObject) {
            true
          } else {
            val thisType = this.resolveBase
            if (thisType.isDeferred) {
              builder.getObjectType.addVerificationPair(builder, thisType, context)
              true
            } else {
              thisType.isInterface
            }
          }
        }

      } else if (this.baseKind != that.baseKind) {
        false

      } else if (this.isUninitializedNew) {
        assert(this.uninitializedOffset != that.uninitializedOffset)
        false

      } else {
        this.isAssignableBaseClassFrom(that, context)
      }
    }

    private def isAssignableBaseClassFrom(that: VerificationTypes#VerificationType, context: VerifiableMethod): Boolean = {
      if (this.isBaseObject) {
        return true
      }

      val thisType = resolveBase
      if (!thisType.isDeferred) {
        if (thisType.isInterface) return true
        if (that.isBaseObject) return false
      }

      val thatType = that.resolveBase
      if (thisType.isDeferred || thatType.isDeferred) {
        if (thisType != thatType.getAbsentSuper) {
          thatType.addVerificationPair(builder, thisType, context)
        }
        return true
      }

      if (thatType.isUnloadable) {
        throw thatType.getVerificationInfo.getVerifyError.toClassLoadingError
      }

      thisType.isAssignableFrom(thatType)
    }

    private def getObjectArrayDimNum = if (baseKind.isPrimitive) {
      dimNum - 1
    } else {
      dimNum
    }

    def merge(that: VerificationTypes#VerificationType): VerificationTypes#VerificationType = {
      if (this == that || this == TOP) {
        this

      } else if (this.isClassOrArrayOrNull && that.isClassOrArrayOrNull) {
        if (that.isNull) {
          this
        } else if (this.isNull) {
          that
        } else if (this.isArray || that.isArray) {
          if (this.dimNum != that.dimNum || this.baseKind.isPrimitive || that.baseKind.isPrimitive) {
            array(OBJECT, this.getObjectArrayDimNum min that.getObjectArrayDimNum)
          } else {
            this.mergeBases(that, dimNum)
          }
        } else {
          this.mergeBases(that, 0)
        }

      } else {
        TOP
      }
    }

    private def mergeBases(that: VerificationTypes#VerificationType, resultDimNum: Int): VerificationTypes#VerificationType = {
      if (this.isBaseObject || that.isBaseObject) {
        return array(OBJECT, resultDimNum)
      }

      var thisBase = resolveBase
      var thatBase = that.resolveBase

      if (thatBase.isDeferred) return this
      if (thisBase.isDeferred) return that

      if (thisBase.isInterface || thatBase.isInterface) {
        return array(OBJECT, resultDimNum)
      }

      var thisLevel = thisBase.getClassInheritanceLevel
      var thatLevel = thatBase.getClassInheritanceLevel

      while (thisLevel != thatLevel) {
        if (thisLevel > thatLevel) {
          thisBase = thisBase.getSuperClass
          thisLevel -= 1
        } else { // thisLevel < thatLevel
          thatBase = thatBase.getSuperClass
          thatLevel -= 1
        }
      }

      while (thisBase != thatBase) {
        thisBase = thisBase.getSuperClass
        thatBase = thatBase.getSuperClass
      }

      mergedClassOf(thisBase, resultDimNum)
    }

    override def toString = {
      val typeStr = if (isUninitializedThis) {
        "uninitialized this"
      } else if (isUninitializedNew) {
        s"uninitialized($uninitializedOffset)"
      } else if (baseClassName != null) {
        baseClassName
      } else if (baseKind != null) {
        baseKind.toString.asciiToLowerCase
      } else {
        "unknown"
      }
      val dims = "[]" * dimNum
      s"$typeStr$dims"
    }

    override def equals(that: Any) = that match {
      case that: AnyRef if this eq that => true
      case that: VerificationTypes#VerificationType =>
        this.baseKind == that.baseKind &&
          this.dimNum == that.dimNum &&
          this.uninitializedOffset == that.uninitializedOffset &&
          this.baseClassName == that.baseClassName &&
          this.equalResolvedBases(that)
      case _ => false
    }

    private def equalResolvedBases(that: VerificationTypes#VerificationType) = {
      // Most types are resolved using this class as reference class (e.g. signature from CP).
      // Some types are created by some resolved type (e.g. during merge).
      // So there could be two types with equal class names but different resolved types (e.g. using classloaders).
      this.resolvedBase == that.resolvedBase || this.resolveBase == that.resolveBase
    }

    override def hashCode = (uninitializedOffset, baseKind, baseClassName, dimNum).##

    def getArrayElement: VerificationTypes#VerificationType = {
      assert(isArray)
      val newDimNum = dimNum - 1
      if (newDimNum == 0 && baseKind.isShortIntegral) {
        INT
      } else {
        VerificationType(baseKind, baseClassName, newDimNum, uninitializedOffset, resolvedBase)
      }
    }

    def getUninitializedNewOffset = {
      assert(isUninitializedNew && uninitializedOffset >= 0)
      uninitializedOffset
    }

    private def resolveBase = {
      if (resolvedBase == null) {
        assert(baseClassName != null)
        resolvedBase = refClass.resolveTypeByName(tp, baseClassName)
        if (resolvedBase.hasVerificationInfo) refClass.addVerificationImport(resolvedBase)
      }
      resolvedBase
    }

    def is2Slots = baseKind.is2Slots && !isArray

    /** Returns true if this is LONG, DOUBLE or their second half. */
    def is2SlotsOrHalf = isHalf || is2Slots

    def get2ndHalf = {
      assert(is2Slots)
      LONG_HALF
    }

    def isHalf = baseKind == Kind.LONG_HALF

    def isClassOrArrayOrNull = isClass || isArrayOrNull

    def isClass = (baseKind == Kind.CLASS) && !isArray

    def isArrayOrNull = isArray || isNull

    def isArray = dimNum > 0

    def isNull = this == NULL

    private def isBaseObject = baseClassName == OBJECT_NAME

    def isUninitializedNew = baseKind == Kind.UNINITIALIZED

    def isUninitializedThis = baseKind == Kind.UNINITIALIZED_THIS
  }

  private val classCache = mutable.Map.empty[XString, VerificationTypes#VerificationType]

  val TOP = VerificationType(Kind.TOP)

  val VOID = VerificationType(Kind.VOID)
  val INT = VerificationType(Kind.INT)
  val LONG = VerificationType(Kind.LONG)
  val FLOAT = VerificationType(Kind.FLOAT)
  val DOUBLE = VerificationType(Kind.DOUBLE)

  val REFERENCE_OR_RETURN_ADDRESS = VerificationType(Kind.REFERENCE_OR_RETURN_ADDRESS)

  val RETURN_ADDRESS = VerificationType(Kind.RETURN_ADDRESS)

  val REFERENCE = VerificationType(Kind.ANY_REFERENCE) // all objects, arrays and uninitialized
  val NULL = VerificationType(Kind.NULL)

  val OBJECT_NAME = XString("java/lang/Object")
  val OBJECT = classOf(OBJECT_NAME)
  val THROWABLE = classOf(XString("java/lang/Throwable"))
  val CLASS = classOf(XString("java/lang/Class"))
  val STRING = classOf(XString("java/lang/String"))
  val METHOD_TYPE = classOf(XString("java/lang/invoke/MethodType"))
  val METHOD_HANDLE = classOf(XString("java/lang/invoke/MethodHandle"))

  val LONG_HALF = VerificationType(Kind.LONG_HALF)

  /** Special placeholder type stored on top of the stack of an exceptional state.
    * Replaced by actual catch type during state merge.
    */
  val CATCH_TYPE_PLACEHOLDER = VerificationType(Kind.CATCH_TYPE_PLACEHOLDER)

  private def classOf(className: XString, dimNum: Int, verifiableType: VerifiableType): VerificationTypes#VerificationType = {
    assert(className.charAt(0) != '['.toByte)
    if (dimNum == 0) {
      val tpe = classCache.getOrElseUpdate(className, VerificationType(Kind.CLASS, className, 0))

      if (verifiableType != null) {
        if (tpe.resolvedBase != null) {
          assert(tpe.resolvedBase == verifiableType)
        } else {
          tpe.resolvedBase = verifiableType
        }
      }

      return tpe
    }

    VerificationType(Kind.CLASS, className, dimNum)
  }

  def classOf(className: XString, dimNum: Int): VerificationTypes#VerificationType = classOf(className, dimNum, null)

  def classOf(arrayOrClassName: XString): VerificationTypes#VerificationType = if (arrayOrClassName.charAt(0) != '['.toByte) {
    classOf(arrayOrClassName, 0)
  } else {
    parseSingle(SignatureTraverser.fromString(arrayOrClassName))
  }

  private def mergedClassOf(verifiableType: VerifiableType, dimNum: Int): VerificationTypes#VerificationType = {
    // Intentionally non-caching by name.
    assert(verifiableType.isClassOrInterface)
    VerificationType(Kind.CLASS, verifiableType.getXName, dimNum, resolvedBase = verifiableType)
  }

  def array(`type`: VerificationTypes#VerificationType, dimNum: Int): VerificationTypes#VerificationType = if (dimNum == 0) {
    `type`
  } else {
    VerificationType(`type`.baseKind, `type`.baseClassName, `type`.dimNum + dimNum, resolvedBase = `type`.resolvedBase)
  }

  def primitive(sigChar: Char, dimNum: Int): VerificationTypes#VerificationType = {
    if (dimNum > 0) {
      val base = sigChar match {
        case 'Z' | 'B' => Kind.BYTE
        case 'S' => Kind.SHORT
        case 'C' => Kind.CHAR
        case 'I' => Kind.INT
        case 'J' => Kind.LONG
        case 'F' => Kind.FLOAT
        case 'D' => Kind.DOUBLE
        case _ => shouldNotReachHere(sigChar)
      }
      VerificationType(base, null, dimNum)
    } else {
      sigChar match {
        case 'Z' | 'B' | 'S' | 'C' | 'I' => INT
        case 'J' => LONG
        case 'F' => FLOAT
        case 'D' => DOUBLE
        case 'V' => VOID
        case _ => shouldNotReachHere(sigChar)
      }
    }
  }

  def primitive(tk: BytecodeTypeKind, dimNum: Int): VerificationType = {
    if (dimNum > 0) {
      val base = tk match {
        case BytecodeTypeKind.BOOLEAN |
             BytecodeTypeKind.BYTE =>   Kind.BYTE
        case BytecodeTypeKind.SHORT =>  Kind.SHORT
        case BytecodeTypeKind.CHAR =>   Kind.CHAR
        case BytecodeTypeKind.INT =>    Kind.INT
        case BytecodeTypeKind.LONG =>   Kind.LONG
        case BytecodeTypeKind.FLOAT =>  Kind.FLOAT
        case BytecodeTypeKind.DOUBLE => Kind.DOUBLE
        case _ => shouldNotReachHere(tk)
      }
      VerificationType(base, null, dimNum)
    } else {
      tk match {
        case BytecodeTypeKind.BOOLEAN |
             BytecodeTypeKind.BYTE |
             BytecodeTypeKind.SHORT |
             BytecodeTypeKind.CHAR |
             BytecodeTypeKind.INT => INT
        case BytecodeTypeKind.LONG => LONG
        case BytecodeTypeKind.FLOAT => FLOAT
        case BytecodeTypeKind.DOUBLE => DOUBLE
        case BytecodeTypeKind.VOID => VOID
        case _ => shouldNotReachHere(tk)
      }
    }
  }

  def uninitialized(offset: Int) = VerificationType(Kind.UNINITIALIZED, null, 0, offset, null)

  def thisType(uninitialized: Boolean): VerificationTypes#VerificationType = if (uninitialized) {
    VerificationType(Kind.UNINITIALIZED_THIS, refClass.getXName, 0, resolvedBase = refClass)
  } else {
    classOf(refClass)
  }

  def classOf(verifiableType: VerifiableType): VerificationTypes#VerificationType = {
    assert(verifiableType.isClassOrInterface)
    classOf(verifiableType.getXName, 0, verifiableType)
  }

  def parseSingle(sig: SignatureTraverser): VerificationTypes#VerificationType = parseSignature(sig).singleElement

  def parseSignature(sig: SignatureTraverser): SignatureParser[VerificationTypes#VerificationType] = new SignatureParser[VerificationTypes#VerificationType](sig) {
    override protected def parsePrimitive(arrayDim: Int, sigChar: Byte): VerificationTypes#VerificationType = primitive(sigChar.toChar, arrayDim)
    override protected def parseClass(arrayDim: Int, name: XString): VerificationTypes#VerificationType = classOf(name, arrayDim)
  }

  private[verifier] def baseThrowableType: VerificationTypes#VerificationType = if (refClass.isAJType) {
    // OBJECT covers all reference types including managed classes.
    // Unfortunately it also covers Thin types, ignore this, we trust AJ bytecode.
    OBJECT
  } else {
    THROWABLE
  }
}
