/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.be_386.desc

import com.huawei.excelsior.jet.assembler
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.*
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.{addressSize, targetArch}
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.o2lib.be_386.CodeDefModule.Segment
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator.AOTConstantPool.*
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator.SegmentManipulations.objBySegm
import com.huawei.excelsior.jet.compiler.o2lib.be_386.{CodeDefModule as cd, opAttrsModule as at}
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, NumerateModule as Numerate, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JavaClassParserModule as jcp
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.LONG
import com.huawei.excelsior.o2j.runtime.O2JSupport
import xscala.util.MathUtils.isNBitsSigned
import xscala.util.{Set32, UShort}

import scala.collection.mutable.ArrayBuffer


object TypeMetaInfoWriter {
  def genShort(x: Int): Unit = {
    assert(x >= Short.MinValue && x <= Short.MaxValue)
    cd.genWord(x.toShort)
  }

  def genUInt16(x: Int): Unit = {
    assert(x >= 0 && x <= UShort.MaxValue)
    cd.genWord(x.toShort)
  }

  def genSet16(x: Set32): Unit = genUInt16(x.toInt)

  def genInt(x: Int): Unit = cd.genLWord(x)

  def genAddrInt(x: Int): Unit = {
    cd.genLWord(x)

    if (targetArch.is64Bit) {
      // gen sign-ext(x)
      cd.genLWord(if (x < 0) -1 else 0)

    } else {
      assert(targetArch.is32Bit)
    }
  }

  def genNull(): Unit = {
    genAddrInt(0)
  }

  def expandSegment(sg: Segment, newSize: Int): Unit = {
    val size = sg.length
    assert(newSize >= size)
    sg.putZeroes(newSize - size)
  }

  def fixup(obj: pc.Symbol, offs: Int = 0): Unit = {
    if (obj != null) {
      cd.addFixup(if (targetArch.is64Bit) ADDR64 else ADDR32, obj, offs)
    } else {
      assert(offs == 0)
      genNull()
    }
  }

  def rvaRef(obj: pc.Symbol, offs: Int = 0): Unit = {
    rva32(obj, offs)
    genInt(RTConst.RVA.Ref.RVA_COMPLEMENT.intValue)
  }

  def rva32(obj: pc.Symbol, offs: Int = 0): Unit = {
    if (obj != null) {
      cd.addFixup(RVA_32, obj, offs)
    } else {
      genInt(0)
    }
  }

  // Should be used only in `createJavaReflection`
  def rel16(obj: pc.Symbol, offs: Int = 0): Unit = {
    if (obj != null) {
      cd.addFixup(TD_REL_16, obj, offs)
    } else {
      assert(offs == 0)
      genShort(0)
    }
  }

  def rel32(obj: assembler.Symbol, offs: Int = 0, del: Boolean = false): Unit = {
    if (obj != null) {
      cd.addFixup(if (del) TD_REL_32_DEL else TD_REL_32, obj, offs)
    } else {
      assert(offs == 0)
      genInt(0)
    }
  }

  // TODO: allow to set alignment w/o segment expansion (type.align?)
  def alignRawObject(obj: pc.DataSymbol.Sized, align: Int): Unit = {
    val size = pcO.getPlainArrayLength(obj)
    val newSize = Numerate.mkAlign(size, align)
    if (obj.ownsSegment) {
      val sg = at.getSegment(obj)
      assert(size == sg.length)
      expandSegment(sg, newSize)
    }
    pcO.setPlainArrayLength(obj, newSize)
  }

  def outStr(s: XString): pc.Symbol = {
    objBySegm(cd.makeSeg(cd.genBstr(s)))
  }

  def calcNpars(p: pcO.Method): Short = {
    val paramsCountWithoutReceiver = p.getSignature.parameterTypes.size
    assert(paramsCountWithoutReceiver <= Short.MaxValue)
    paramsCountWithoutReceiver.toShort
  }

  def stringRef(s: XString): Unit = {
    if (s == null || s.isEmpty) {
      genInt(RTConst.StringRef.EMPTY.intValue)
    } else {
      val o = at.StringHolder(s)
      cd.addFixup(BYTE_STR_32, o, 0)
    }
  }

  def tdindex(t: pc.SymType): Unit = {
    if (t == null) {
      genShort(0)
    } else {
      val td = auxTDObj(t, from_reflection = true)
      cd.addFixup(TD_INDEX_16, td, 0)
    }
  }

  object Annotations {
    private def addAOTConstantPoolStringEntry(value: XString): UShort = {
      val e = new jcp.ConstantInfo()
      e.constantType = jcp.TagUtf8.toByte
      e.bufferPtr = value
      addAOTConstantPoolEntry(e)
    }

    private def putAnnot(a: jcp.PtrAnnotation): Unit = {
      BigEndian.addW16(addAOTConstantPoolStringEntry(a.type0).toShort)
      val len = a.pairs.length
      assert(len == len.toShort.toInt)
      BigEndian.addW16(len.toShort)
      for (i <- a.pairs.indices) {
        BigEndian.addW16(addAOTConstantPoolStringEntry(a.pairs(i).name).toShort)
        putAnnotElementValue(a.pairs(i).value)
      }
    }

    def putAnnotElementValue(ev: jcp.PtrElementValue): Unit = {
      def addAOTConstantPoolAnnotEntry(ev: jcp.PtrElementValue): UShort = {
        val e = new jcp.ConstantInfo()
        ev.tag match {
          case 'B' |
               'C' |
               'I' |
               'S' |
               'Z' =>
            e.constantType = jcp.TagInteger.toByte
            e.low = ev.asInstanceOf[jcp.PtrIntElementValue].value
            e.high = 0
          case 'J' =>
            e.constantType = jcp.TagLong.toByte
            e.low = ev.asInstanceOf[jcp.PtrLongElementValue].low
            e.high = ev.asInstanceOf[jcp.PtrLongElementValue].high
          case 'F' =>
            e.constantType = jcp.TagFloat.toByte
            e.realVal = ev.asInstanceOf[jcp.PtrFloatElementValue].value
          case 'D' =>
            e.constantType = jcp.TagDouble.toByte
            e.longRealVal = ev.asInstanceOf[jcp.PtrDoubleElementValue].value
          case 's' =>
            e.constantType = jcp.TagUtf8.toByte
            e.bufferPtr = ev.asInstanceOf[jcp.PtrStringElementValue].value
          case 'c' =>
            e.constantType = jcp.TagUtf8.toByte
            e.bufferPtr = ev.asInstanceOf[jcp.PtrClassElementValue].classInfo
        }
        addAOTConstantPoolEntry(e)
      }

      BigEndian.addW8(O2JSupport.convCharToInt(ev.tag).toShort.toInt)
      ev.tag match {
        case 'B' | 'C' | 'I' | 'S' | 'Z' | 'F' | 'J' | 'D' | 's' | 'c' =>
          BigEndian.addW16(addAOTConstantPoolAnnotEntry(ev).toShort)
        case 'e' =>
          BigEndian.addW16(addAOTConstantPoolStringEntry(ev.asInstanceOf[jcp.PtrEnumElementValue].typeName).toShort)
          BigEndian.addW16(addAOTConstantPoolStringEntry(ev.asInstanceOf[jcp.PtrEnumElementValue].constName).toShort)
        case '@' =>
          putAnnot(ev.asInstanceOf[jcp.PtrAnnotationElementValue].value)
        case '[' =>
          val array_value = ev.asInstanceOf[jcp.PtrArrayElementValue]
          val len = array_value.value.length
          assert(len == len.toShort.toInt)
          BigEndian.addW16(len.toShort)
          for (i <- array_value.value.indices) {
            putAnnotElementValue(array_value.value(i))
          }
        case _ =>
          throw new AssertionError
      }
    }

    def putTypeAnnotArray(aa: Array[jcp.PtrTypeAnnotation]): Unit = {
      val len = aa.length
      assert(len == len.toShort.toInt)
      BigEndian.addW16(len.toShort)
      for (i <- aa.indices) {
        val annot = aa(i)
        BigEndian.addW8(annot.targetType.toInt)
        annot.targetInfo match {
          case targetInfo: jcp.PtrOneByteTargetInfo =>
            BigEndian.addW8(targetInfo.index.toInt)
          case targetInfo: jcp.PtrTwoBytesTargetInfo =>
            BigEndian.addW8(targetInfo.index1.toInt)
            BigEndian.addW8(targetInfo.index2.toInt)
          case targetInfo: jcp.PtrWordTargetInfo =>
            BigEndian.addW16(targetInfo.index.toShort)
          case _ =>
        }
        BigEndian.addW8(annot.pathLength.toInt)
        for (j <- annot.path.indices) {
          BigEndian.addW8(annot.path(j).toInt)
        }
        putAnnot(annot.annotation)
      }
    }

    def putAnnotArray(aa: Array[jcp.PtrAnnotation]): Unit = {
      val len = aa.length
      assert(len == len.toShort.toInt)
      BigEndian.addW16(len.toShort)
      for (i <- aa.indices) {
        putAnnot(aa(i))
      }
    }

    def genLongElement(low: Int, high: Int, i: Int): Unit = {
      cd.genLWord(low)
      cd.genLWord(high)
      checkPlaceholder(i)
    }

    def genBootstrapMethodsAttr(bootstrapMethodsAttr: ArrayBuffer[jcp.PtrBootstrapMethod]): Unit = {
      var argsLen: Int = 0

      if (bootstrapMethodsAttr.isEmpty) {
        return
      }

      val elementsCount = bootstrapMethodsAttr.size
      assert(elementsCount > 0)

      // it might be placed unaligned, but it's ok because we read it byte-by-byte via ByteBuffer
      BigEndian.addW16(elementsCount.toShort)

      for (m <- bootstrapMethodsAttr) {
        BigEndian.addW16(m.methodIndex.toShort)
        if (m.args == null) {
          argsLen = 0
        } else {
          argsLen = m.args.length
        }
        BigEndian.addW16(argsLen.toShort)
        for (j <- 0 until argsLen) {
          BigEndian.addW16(m.args(j).toShort)
        }
      }

      bootstrapMethodsAttr.clear()
    }
  }

  object BigEndian {
    def addW8(x: Int): Unit = {
      cd.genByte(x & 0xFF)
    }

    def addW16(x: Int): Unit = {
      assert(isNBitsSigned(x, 16))
      addW8(x >>> 8)
      addW8(x)
    }
  }

  private def auxTDObj(tpe: pc.SymType, from_reflection: Boolean): pc.Symbol = {
    val t = pcO.getCoreType(tpe)

    t match {
      case clazz: pcO.Class if (clazz.isThinClass || clazz.isNamespace) && !clazz.hasManagedMetaInformation =>
        // TODO-THIN: Unmanaged Thin TDs should never be referenced by TDIndex
        // however, they are referenced as parameters types of some RTSProcs from CompilerInterface
        // These RTS procedures should not be used in JIT
        // so their Thin parameters are replaced with longs.
        // Namespace parameters are also replaced with longs
        return auxTDObj(pc.SymType.JBC.Primitive(LONG), from_reflection)
      case _ =>
    }

    t match {
      case t: pcO.Class if t.isShielded => at.addAbsentClass(t)
      case _ => // no checks
    }

    val o = tpe.typeHandle

    if (tpe.isInstanceOf[pc.SymType.Array] && at.findWorkObject(o.name) == null) {
      at.addToWorkObjects(o)
    }

    o
  }
}
