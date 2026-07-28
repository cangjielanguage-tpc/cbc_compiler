/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.be_386.desc

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.*
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.o2lib.be_386.CodeDefModule.{Fixup, Segment, getCodeLen, withSeg}
import MetaInfoEmitter.MetaInfo.currentMetaInfo
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.MetaInfoEmitterDSL.EntryWithValue.*
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.MetaInfoEmitterDSL.{CondEntry, Entry, EntryWithValue}
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator.INVALID_OFFSET
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator.Utils.{Expandable16BOBJECT, isExpandable16}
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoWriter.{expandSegment, genAddrInt, genInt}
import com.huawei.excelsior.jet.compiler.o2lib.be_386.{CodeDefModule as cd, opAttrsModule as at}
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcNamesModule, pcOModule, NumerateModule as Numerate}
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule
import com.huawei.excelsior.jet.compiler.o2lib.u.xiEnvModule as env
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.classByO2Object
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Field}
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object MetaInfoEmitter {
  type Value = Byte | Short | Int | Long

  private class FieldWithSize(val field: Field, var value: EntryWithValue)

  private object FieldWithSize {
    def unapply(f: FieldWithSize): Option[(Field, EntryWithValue)] = Some((f.field, f.value))
  }

  class MetaInfo(private var metaInfoType: MetaInfoType) {
    private var segment: Option[Segment] = None
    private var attachedObj: Option[pc.Symbol] = None
    private val objects = mutable.ArrayBuffer.empty[pc.Symbol]

    private val additionalFieldsValue: mutable.SeqMap[String, EntryWithValue] =
      mutable.SeqMap.from(metaInfoType.additionalFields.map(s => (s, NumericValue(0))))

    private def defaultFieldValue(f: Field) = (f.getName, FieldWithSize(f, NumericValue(0)))

    private val declaredFields: mutable.SeqMap[String, FieldWithSize] =
      mutable.SeqMap.from(metaInfoType.sortedFields.map(defaultFieldValue))


    def define(fieldsValues: Entry*): MetaInfo = {

      def parseEntrySeq(x: Seq[Entry]): Unit = x foreach {
        case CondEntry(cond, x) if cond => parseEntrySeq(x)
        case (fieldName, value) =>
          if (declaredFields.contains(fieldName)) {
            assert(declaredFields(fieldName).value == NumericValue(0))
            declaredFields(fieldName).value = value
          } else if (additionalFieldsValue.contains(fieldName)) {
            additionalFieldsValue(fieldName) = value
          } else {
            shouldNotReachHere(s"Field $fieldName don't contains in struct ${metaInfoType.getName}, or " +
              s"in its additionalFields.")
          }
        case _ =>
      }

      parseEntrySeq(fieldsValues)
      this
    }

    private def extendTo(newMetaInfoType: MetaInfoType): MetaInfo = {
      def checkOverlappedParams(newMetaInfoType: MetaInfoType): Unit = {
        for ((f1, f2) <- metaInfoType.sortedFields.zip(newMetaInfoType.sortedFields)) {
          assert(f1.size == f2.size &&
            f1.getInstanceFieldOffset == f2.getInstanceFieldOffset &&
            f1.getType.toAsm == f2.getType.toAsm,
            s"Incorrect struct cast from $metaInfoType to $newMetaInfoType. " +
              s"Fields $f1 and $f2 are not equal (size, offset or asmType)."
          )
        }
      }
      
      checkOverlappedParams(newMetaInfoType)

      declaredFields ++= mutable.SeqMap.from(newMetaInfoType.sortedFields.drop(metaInfoType.sortedFields.size).map(defaultFieldValue))
      this.metaInfoType = newMetaInfoType
      this
    }

    /** Extends MetaInfo's MetaInfoType to newMetaInfoType.
      * 
      * This method can be useful if you want to define structures with inheritance.
      *
      * For example:
      * {{{
      * @Struct
      * class A {
      *     int intField1;
      *     int intField2;
      * }
      *
      * @Struct
      * class B extends A {
      *     boolean boolField;
      * }
      * }}}
      *
      * For this pair of structures, you can first define 
      * the fields of structure A, then structure B:
      * 
      * {{{
      * val a /*MetaInfo[A]*/ = struct(A)( /* define A fields */ )
      * struct(a >> B)( /* define B fields */ )
      * }}}
      */
    def >>(newMetaInfoType: MetaInfoType): MetaInfo = extendTo(newMetaInfoType)

    def emit(subStructure: Boolean = false): MetaInfo = {
      val seg = segment match {
        case Some(seg) => seg
        case None =>
          segment = Some(cd.newSeg())
          segment.get
      }

      val oldMetaInfo = if (!subStructure) {
        val old = currentMetaInfo
        currentMetaInfo = Some(this)
        old
      } else None

      withSeg(seg) {
        _emit()
      }

      if (!subStructure) {
        currentMetaInfo = oldMetaInfo
      }
      this
    }

    private[desc] def _emit(): MetaInfo = {
      import assembler.AsmType.*

      val startPos = getCodeLen

      if (metaInfoType.isUnion) {
        assert(additionalFieldsValue.isEmpty, "You cannot add fields to union type.")
        val unionSize = declaredFields.valuesIterator.map(_.field.size).max
        val definedField = declaredFields.collect {
          case (_, FieldWithSize(_, a: Action)) => a
        }

        ScalaCollections.singleton(definedField) match {
          case Some(Action(op)) =>
            op()
            assert(segment.isEmpty || cd.getSeg == segment.get, "You cannot change segment in `struct` macro. " +
              "Use `cd.withSeg` instead of `cd.setSeg`")
          case _ =>
            shouldNotReachHere("In union definition only one Action should be used.")
        }
        cd.getSeg.putZeroes(unionSize - (getCodeLen - startPos))
      } else {
        for ((s, FieldWithSize(field, value)) <- declaredFields) {
          if (field.isAJFlat) {
            value match {
              case NumericValue(0) =>
                segment.get.putZeroes(field.size)
              case Action(op) =>
                op()
                assert(segment.isEmpty || cd.getSeg == segment.get, "You cannot change segment in `struct` macro. " +
                  "Use `cd.withSeg` instead of `cd.setSeg`")
              case NumericValue(n) =>
                shouldNotReachHere(s"If field has @Flat annotation, NumericValue(n), " +
                  s"where n != 0 cannot be used (in this case n = $n).")
            }
          } else {
            value match {
              case NumericValue(v) =>
                field.getType.toAsm match {
                  case I8 => cd.genByte(v)
                  case I16 => cd.genWord(v.toShort)
                  case I32 => genInt(v.toShort)
                  case I64 | PTR => genAddrInt(v)
                  case x => shouldNotReachHere(x)
                }
              case Action(op) => op()
            }
          }
          metaInfoType.fieldCheck(field, startPos)
        }
        additionalFieldsValue.values foreach {
          case Action(op) =>
            op()
            assert(cd.getSeg == segment.get, "You cannot change segment in `struct` macro. " +
              "Use `cd.withSeg` instead of `cd.setSeg`")
          case v: NumericValue => 
            shouldNotReachHere(s"In addition field field zeroing cannot be used (in this case NumericValue(${v.v}).")
        }
      }
      this
    }

    def attachObject(o: pc.Symbol): MetaInfo = {
      assert(segment.nonEmpty, "You should attach object only after `MetaInfo.segment` definition.")
      at.setSegment(o, segment.get)
      attachedObj = Some(o)
      this
    }

    def getAttachedObject: pc.Symbol = attachedObj.get

    def addObject(o: pc.Symbol): pc.Symbol = {
      objects += o
      o
    }

    def gatherSegments(bigOffsets: ArrayBuffer[Int] = null): MetaInfo = {
      if (objects.isEmpty) {
        return this
      }

      class SegmentInfo(var seg: Segment = null, var obj: pc.Symbol = null, var ofs: Int = 0, var align: Short = -1)

      assert(segment.nonEmpty, "You should set the segment before use `MetaInfo.gatherSegments`")
      assert(attachedObj.nonEmpty, "You should attach the object before use `MetaInfo.gatherSegments`")

      val (seg, obj) = (segment.get, attachedObj.get)
      val table: mutable.ArrayBuffer[SegmentInfo] = {
        val alignTable = Seq[Short](4, 1, 2, 1)

        (for (o <- objects.reverse; segment = at.getSegment(o))
        yield SegmentInfo(segment, o, align = alignTable(segment.length & 3)))
          .sortInPlaceWith((x, y) => x.align > y.align)  /* sort by alignment (decreasing) */
      }

      def findOfs(o: pc.Symbol): Int = {
        val i = table.map(_.obj).indexOf(o)
        if (i == -1) {
          INVALID_OFFSET
        } else table(i).ofs
      }

      def isExpanded16to32(obj: pc.Symbol): Boolean = obj match {
        case obj: Expandable16BOBJECT => obj.expandedTo32
        case _ => false
      }

      def joinSegments(): Unit = {
        for (segInfo <- table) {
          segInfo.ofs = seg.length
          seg.append(segInfo.seg)
        }
      }

      // add padding to seg for 4-byte alignment
      expandSegment(seg, Numerate.mkAlign(seg.length, 4))

      joinSegments()

      cd.withSeg(seg) {
        seg.transformFixups { fixup => {
          val target = fixup.getTargetAsOBJECT

          def fixupValue(): Int =
            ((findOfs(target) ensuring (_ != INVALID_OFFSET)) + fixup.addend) ensuring (_ > 0)

          fixup.kind match {
            case TD_REL_32 =>
              assert(!isExpandable16(target))
              assert(!isExpanded16to32(target))
              assert(seg.getW32(fixup.position) == 0)
              seg.setW32(fixup.position, fixupValue())
              None

            case TD_REL_16 =>
              assert(bigOffsets != null, "`rel16` should be used only in `createJavaReflection`.")
              var value0 = fixupValue()
              if (isExpandable16(target) || isExpanded16to32(target)) assert((value0 & 1) == 0)
              if (isExpanded16to32(target)) value0 += 1
              val value = if (value0 <= Short.MaxValue) {
                value0.toShort
              } else {
                bigOffsets += value0
                val x = -bigOffsets.size * 4
                assert(x == x.toShort.toInt)
                x.toShort
              }
              assert(seg.getW16(fixup.position) == 0)
              seg.setW16(fixup.position, value.toInt & 0xFFFF)
              None

            case TD_REL_32_DEL =>
              assert(findOfs(target) == INVALID_OFFSET)
              Some(new Fixup(fixup.kind, target, fixup.addend + fixup.position, fixup.position))

            case _ =>
              val ofs = findOfs(target)
              if (ofs != INVALID_OFFSET) {
                Some(new Fixup(fixup.kind, obj, fixup.addend + ofs, fixup.position))
              } else {
                Some(fixup)
              }
          }
        }
        }
      }

      this
    }

    def getObjects: Seq[pc.Symbol] = objects.toSeq

    def getSegment: Segment = segment.get
  }

  object MetaInfo {
    private var currentMetaInfo: Option[MetaInfo] = None

    def getCurrentMetaInfo: MetaInfo = {
      assert(currentMetaInfo.nonEmpty, "Current metaInfo is empty. " +
        "You need to call `getCurrentMetaInfo` only in `struct` function.")
      currentMetaInfo.get
    }
  }

  import MetaInfoType.*

  enum MetaInfoType(typeInfoName: String,
                    val additionalFields: Seq[String] = Nil) {
    private lazy val classType: ClassType = {
      val name = XString(typeInfoName)
      val pdbName = pcNamesModule.newClassName(name).getMangledName
      val pdbPlace = xPDBModule.findPlaceToReadFrom(pdbName, xPDBModule.ContentType.SYM)
      env.loadType(name)

      val clazz = pcOModule.findClass(name, tryAbsent = false)
      assert(clazz != null, s"Could not find TypeInfo '$name', possibly incompatible language pack")
      classByO2Object(clazz)
    }

    def getName: String = classType.getName

    lazy val (sortedFields, isUnion) = {
      val nonStaticFields = classType.getFields.filter(!_.isStatic).toSeq
      (
        nonStaticFields.sortWith((f1, f2) => f1.getInstanceFieldOffset < f2.getInstanceFieldOffset),
        nonStaticFields.forall(_.getInstanceFieldOffset == 0)
      )
    }

    @elidable(ASSERTION)
    def fieldCheck(field: Field, startPos: Int = 0): Unit = {
      if (!(field.isAJFlat && field.size == 0 && sortedFields.last == field)) {
        assert(getCodeLen == startPos + field.getInstanceFieldOffset + field.size,
          s"Incorrect field size: after trying to write field ${field.getName} got incorrect " +
            s"position $getCodeLen, expected ${startPos + field.getInstanceFieldOffset + field.size}.")
      }
    }

    case RunTimeTypeInfo extends MetaInfoType(RTConst.RunTimeTypeInfo.className)
    case InfectedTypeHandle extends MetaInfoType(RTConst.InfectedTypeHandle.className)
    case AbsentContainer extends MetaInfoType(RTConst.AbsentContainer.className)
    case HostingTypeHandle extends MetaInfoType(RTConst.HostingTypeHandle.className)
    case AJArrayTypeHandle extends MetaInfoType(RTConst.AJArrayTypeHandle.className)
    case CangjieArrayTypeHandle extends MetaInfoType(RTConst.CangjieArrayTypeHandle.className)

    case ThinTypeHandle extends MetaInfoType(RTConst.ThinTypeHandle.className)

    case TypeHandle extends MetaInfoType(RTConst.TypeHandle.className)

    case JavaCustomTypeInfo extends MetaInfoType(RTConst.JavaCustomTypeInfo.className)
    case MetaInfoUnion extends MetaInfoType(RTConst.JavaCustomTypeInfo.MetaInfoUnion.className)
    case MetaInfo extends MetaInfoType(RTConst.MetaInfo.className)

    case TDInitInfo extends MetaInfoType(RTConst.TDInitInfo.className)
    case HostedCUDInfo extends MetaInfoType(RTConst.HostedCUDInfo.className)
    case AnnotationsInfo extends MetaInfoType(RTConst.AnnotationsInfo.className)
    case InnerClassInfo extends MetaInfoType(RTConst.InnerClassInfo.className)
    case EnclosingMethodInfo extends MetaInfoType(RTConst.EnclosingMethodInfo.className)
    case JavaPackageDesc extends MetaInfoType(RTConst.JavaPackageDesc.className, additionalFields = Seq("strImpl"))
    case NativeMethodUnion extends MetaInfoType(RTConst.HostingTypeHandle.NativeMethodUnion.className)
  }
}
