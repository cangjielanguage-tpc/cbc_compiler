/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe

import com.huawei.excelsior.common.CodeHelpers.{shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.{Pass, RTConst}
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Bitcode.PointerType
import com.huawei.excelsior.jet.compiler.o2lib.be_386.opAttrsModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.pc.SymType
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pcOModule, pcNamesModule as pcNames}
import com.huawei.excelsior.jet.compiler.o2lib.u.{AttrAPIModule, xmConfigModule, JStringsModule as js, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.symlevel.ConstValues.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{Unit as U, Void as V}
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.primitives
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment
import com.huawei.excelsior.jet.compiler.symlevel.{JBCSignature, MethodSignature, SignatureType}
import com.huawei.excelsior.o2s.runtime.*

import scala.PartialFunction.{cond, condOpt}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object pc {

  /////////////////////////////////////////////////////////////////////////////
  // In the memory of XDS team

  /* Modifications:
     13-Jan-22 conwor NODE class killed
     21-May-02 Paul   It's just absofuckinglutely impossible to comprehend
                      all the changes happened since 1998 ;-)
     05-Oct-98 Vit    Preliminary merging with pre-3.0 version
                      End position is introduced into NODE
     13-May-98 Vit    ORDs, PTRs sets are added
                      pcConst options changed:
                      - en_cnsexp eliminated
     29-Apr-98 Vit    NODE-VALUE revolution; 2nd pos in NODE
     23-Mar-96 Ned    method "VALUE.cast_ordinal" and constant NUM_LITERALs
                      are added.
     24-Mar-96 Ned    <*IF extvalue*> is deleted.
     24-Mar-96 Ned    field "code_rec.en_cnsexp" is added.
     26-Mar-96 Ned    value_rec.get_cardinal is added.
     27-Mar-96 Ned    PRO0046: Actual LEN for strings(ty_array) may be less then
                      type.len. Use binary(sb_len) to retrieve the len.
                      Comments are added for value.unary/binary.
     16-May-96 Ned    PRO0124: field "marks" is added to OBJECT & STRUCT.
  */



  /////////////////////////////////////////////////////////////////////////////
  // Modules (compilation units)

  type MNO = Int // TODO: make it pair of PDB id and index in it, thus this index may be used for references inside PDB
  val INVALID_MNO: MNO = Int.MinValue

  var modules = ArrayBuffer.empty[Symbol] // TODO: make it part of PDB

  /** "Current" module in some context. Used only in JBC parser and should be removed. */
  var currentModule: MNO = INVALID_MNO

  def withModule[T](module: pcOModule.Class)(action: => T): T = {
    val old = currentModule
    currentModule = module.mno
    try action finally currentModule = old
  }


  /////////////////////////////////////////////////////////////////////////////
  // Common root of sym-level objects - types and symbols

  abstract class SymLevelObject extends AttrAPIModule.Attributable


  /////////////////////////////////////////////////////////////////////////////
  // Sym-level types (mirrors of [[symlevel.Type]] concept)

  abstract class SymType(val mno: MNO) extends SymLevelObject {
    private lazy val arrayCache: SymType.Array = new SymType.Array(this)

    def size: Int
    def alignment: Int

    def toJString: XString = js.newJString(toString)

    /** Returns array of `this` typed elements with `dim` dimensions. */
    def array(dim: Int): SymType.Array = dim ensuring (_ > 0) match {
      case 1 => arrayCache
      case _ => arrayCache.array(dim - 1)
    }

    lazy val symType = LightweightEnvironment.newTypeImpl(this)

    lazy val typeHandle = {
      val _size = condOpt(this) { case c: pcOModule.Class if c.isShielded => 0 }
      new DataSymbol.TypeHandle(this, _size)
    }

    lazy val thinTypeInfo = new DataSymbol.ThinTypeInfo(this)
    lazy val instanceDescriptor = new DataSymbol.InstanceDescriptor(this)

    lazy val singletonObject = new DataSymbol.SingletonObject(this,
      Some(NumerateModule.mkAlign(RTConst.HeapObj.size, RTConst.HeapObj.alignment)))
  }

  object SymType {
    /** Kind of types which instances placed in local variables and there are no pointers to them. */
    abstract class Primitive extends SymType(INVALID_MNO)

    /** Kind of types which instances by default placed in heap and represented in local variables by pointers to them. */
    abstract class Reference(_mno: MNO) extends SymType(_mno)

    /** Array type. */
    final class Array private[SymType](val base: SymType) extends SymType.Reference(INVALID_MNO) {
      val (arrayBaseType: SymType, dim: Int) = base match {
        case array: Array => (array.arrayBaseType, array.dim + 1)
        case t => (t, 1)
      }

      override def size: Int = shouldNotCallThis()
      override def alignment: Int = shouldNotCallThis()

      override def toString: String = s"$base[]"
    }

    /** Types specific to Java bytecode (actually now they are used to represent types in any languages, which is wrong). */
    object JBC {
      import com.huawei.excelsior.jet.compiler.symlevel.TypeKind

      final class Primitive(val typeKind: TypeKind) extends SymType.Primitive {
        override def size: Int = typeKind.size
        override def alignment: Int = typeKind.alignment

        override def toString = typeKind.toString.toLowerCase

        def typeHandleName: String = {
          val (head, tail) = typeKind.toString.splitAt(1)
          s"$head${tail.toLowerCase}TD"
        }
      }

      object Primitive {
        private val cache = mutable.LinkedHashMap.from(TypeKind.primitives map (t => (t, new Primitive(t))))
        def apply(kind: TypeKind): Primitive = cache(kind)
        def all = cache.values
      }
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Symbols

  /** [[Symbol]] is an element of program IR, which has:
    *  1. conceptual `name` during compilation process
    *  1. concrete address during program execution
    *
    * These options allow it to be a target of [[com.huawei.excelsior.jet.assembler.fixups.Relocation]].
    *
    * NOTE: nowadays we unfortunately misuse symbols for elements not satisfying both these options,
    * e.g. abstract methods representation.
    */
  class Symbol(var mno: MNO, val nameObj: pcNames.NAME) extends SymLevelObject with assembler.Symbol { // `mno` is a var because of hack at CreateATD::absentTypeDesc
    override def equals(o: Any): Boolean = this eq o.asInstanceOf[AnyRef]
    override def hashCode: Int = nameObj.hashCode

    def name: XString = nameObj.name

    def getReadableName(need_class_name: Boolean, need_full_sign: Boolean = true): XString = {
      val MAX_LENGTH: Int = 4096

      val hasClass = need_class_name && !isInstanceOf[pcOModule.ModuleObject]
      val hasSig = cond(this) {
        case method: pcOModule.Method => need_full_sign || method.isOverloaded
      }

      if (!hasClass && !hasSig) {
        // fast path
        var str = name
        if (str.length > MAX_LENGTH) {
          str = str.substring(0, MAX_LENGTH)
        }
        return str
      }

      val buf = new js.StringBuffer()
      if (hasClass) {
        buf.appendString(modules(mno).name)
        buf.appendChar('.')
      }
      buf.appendString(name)
      this match {
        case proc: pcOModule.Method if hasSig =>
          val sig = if (proc.getDeclaringClass.isCangjieType) proc.getSignature.toJETSignature else JBCSignature(proc.getSignature)
          buf.appendString(XString(sig))
        case _ =>
      }
      if (buf.length > MAX_LENGTH) {
        buf.trunc(MAX_LENGTH)
      }
      buf.toJString
    }

    override def ownsSegment = opAttrsModule.hasSegment(this)
  }

  object DataSymbol {
    /** Symbols which may have optional size. */
    abstract class Sized(_mno: MNO, _nameObj: pcNames.NAME, var size: Option[Int]) extends Symbol(_mno, _nameObj)

    /** Read-only data. */
    class Const(_mno: MNO, _nameObj: pcNames.NAME, _size: Option[Int]) extends Sized(_mno, _nameObj, _size)

    /** Read-write data - initialized at program start (owns segment) or uninitialized. */
    class RW(_mno: MNO, _name: XString, _size: Option[Int]) extends Sized(_mno, pcNames.RawName(_name), _size) {
      def this(_mno: MNO, _name: String, _size: Option[Int]) = this(_mno, js.newJString(_name), _size)
    }

    class TypeInfo(val tpe: SymType, _name: String, _size: Option[Int] = None) extends RW(tpe.mno, _name, _size)

    class RunTimeTypeInfo      (_tpe: SymType)                     extends TypeInfo(_tpe, "$$RTTI")
    class TypeHandle           (_tpe: SymType, _size: Option[Int]) extends TypeInfo(_tpe, "$$THANDLE", _size)
    class ThinTypeHandle       (_tpe: SymType)                     extends TypeInfo(_tpe, "$$THIN")
    class HeaderThinTypeHandle (_tpe: SymType)                     extends TypeInfo(_tpe, "$$HEADERTHIN")
    class InstanceDescriptor   (_tpe: SymType)                     extends TypeInfo(_tpe, "$$IDESC")

    /** ThinTD contains two pieces of MetaInfo:
      *  1. HeaderTypeHandle contains ancestors TDs.
      *  1. ThinTypeHandle contains all the positive part of ThinTD.
      *
      * Essentially, header exists only because we can't place
      * object and its symbol name with different offsets in segment.
      */
    class ThinTypeInfo(t: SymType) {
      val headerTypeHandle = HeaderThinTypeHandle(t)
      val thinTypeHandle = ThinTypeHandle(t)
    }

    class SingletonObject(t: SymType, _size: Option[Int]) extends RW(t.mno, "SINGLOBJ", _size)
  }
}
