/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.be_386.desc

import com.huawei.excelsior.common.Language.CANGJIE
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.{isWorkMode, languagePack, targetPlatform}
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.o2lib.u.{ClassID, CacheAPIModule as CacheAPI, GetPackageSupportModule as GetPackageSupport, JStringsModule as js, PropertiesModule as Properties, xiEnvModule as env}
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoWriter.*
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JavaClassParserModule as jcp
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator.SegmentManipulations.objBySegm
import com.huawei.excelsior.jet.compiler.o2lib.be_386.{CodeDefModule as cd, opAttrsModule as at, opDefModule as def0, opStdModule as std}
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, NumerateModule as Numerate, ObjNamesModule as nms, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.cangjie.CangjieMain
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.symlevel.JBCSignature
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.{getO2Method, methodByO2Object, sigTypeToO2Type}
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.util.UShort


object SegmentGenerators {
  sealed trait SegmentGeneratorByClasses {
    protected def outData(objs: Seq[pcO.Class]): Unit

    def apply(objs: Seq[pcO.Class], expandable16: Boolean = false): pc.Symbol = {
      objBySegm(cd.makeSeg(outData(objs)), expandable16)
    }
  }

  object GenTDs extends SegmentGeneratorByClasses {
    override def outData(objs: Seq[pcO.Class]): Unit = {
      assert(objs.nonEmpty)
      for (o <- objs) {
        tdindex(o)
      }
    }
  }

  //-----------------------------------------------------------------------------

  sealed trait SegmentGeneratorByObjects {
    protected def outData(objs: Seq[pc.Symbol]): Unit

    /** Out segment */
    def apply(objs: Seq[pc.Symbol], expandable16: Boolean = false): pc.Symbol = {
      objBySegm(cd.makeSeg(outData(objs)), expandable16)
    }
  }

  object GenFrameSz extends SegmentGeneratorByObjects {
    override def outData(objs: Seq[pc.Symbol]): Unit = {
      def getSizeOnCallerFrameInSlots(o2m: pcO.Method): Int = {
        targetPlatform.abi(methodByO2Object(o2m)).sizeOnCallerFrameInSlots
      }

      assert(objs.nonEmpty)
      for (o <- objs) {
        val n = getSizeOnCallerFrameInSlots(o.asInstanceOf[pcO.Method])
        assert(n >= 0 && n < 256)
        cd.genByte(n)
      }
    }
  }

  object GenBStrNames extends SegmentGeneratorByObjects {
    override def outData(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)
      for (o <- objs) {
        stringRef(o.name)
      }
    }
  }

  object GenMethodMods extends SegmentGeneratorByObjects {
    override def outData(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)
      for (o <- objs) {
        val m = o.asInstanceOf[pcO.Method]
        genSet16(m.getModifiers) // MethodModifiers // Value<short>
      }
    }
  }

  object GenFieldMods extends SegmentGeneratorByObjects {
    override def outData(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)
      for (o <- objs) {
        val f = o.asInstanceOf[pcO.Field]
        genSet16(f.getModifiers) // FieldModifiers // Value<short>
      }
    }
  }

  object GenFieldTypes extends SegmentGeneratorByObjects {
    override def outData(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)
      for (o <- objs) {
        val f = o.asInstanceOf[pcO.Field]
        tdindex(sigTypeToO2Type(f.sig))
      }
    }
  }

  object GenMethodParNums extends SegmentGeneratorByObjects {
    override def outData(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)
      for (o <- objs) {
        cd.genWord(calcNpars(o.asInstanceOf[pcO.Method]))
      }
    }
  }

  object GenRetTDs extends SegmentGeneratorByObjects {
    override def outData(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)
      for (o <- objs) {
        val m = o.asInstanceOf[pcO.Method]
        tdindex(sigTypeToO2Type(m.getSignature.returnType))
      }
    }
  }

  object GenThrowsNum extends SegmentGeneratorByObjects {
    override def outData(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)
      for (o <- objs) {
        val m = o.asInstanceOf[pcO.Method]
        genShort(m.getThrowsCount)
      }
    }
  }

  object GenMethodIDs extends SegmentGeneratorByObjects {

    override def outData(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)
      val genIDs = O2Env.env.enabled(BoolOption.GenMethodIDs)
      assert(genIDs || at.genStackTrace)
      for (o <- objs) {
        val m = o.asInstanceOf[pcO.Method]
        stringRef(m.name)
        stringRef(if (genIDs) XString(m.getSignature.toJETSignature) else null)

        if (m.getDeclaringClass.isCangjieType) {
          val genCJST = at.genStackTrace

          val sourceFullName = if (genCJST) m.getSourceFullName else null
          val sourceFile = if (genCJST) m.sourceFile else null

          if (CangjieMain.coldStrings != null) {
            def getColdStringOffset(string: XString): Int = if (string == null) 0 else CangjieMain.coldStrings(string.toString)

            genInt(getColdStringOffset(sourceFullName))
            genInt(getColdStringOffset(sourceFile))
          } else {
            stringRef(sourceFullName)
            stringRef(sourceFile)
          }
        }
      }
    }
  }

  object GenNatives extends SegmentGeneratorByObjects {

    override def outData(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)
      for (o <- objs) {
        val m = o.asInstanceOf[pcO.Method]
        stringRef(m.name)
        stringRef(XString(JBCSignature(m.getSignature)))
        cd.genLWord(at.nativeParamsLen(m))
      }
    }

  }

  object GenFieldOffs extends SegmentGeneratorByObjects {
    // TODO: replace generate method with apply
    private var need32: Boolean = _

    def generate(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)

      var maxOffs = -1
      for (o <- objs) {
        maxOffs = math.max(maxOffs, this.getOffset(o))
      }
      assert(maxOffs >= 0)

      this.need32 = maxOffs > UShort.MaxValue
      if (isWorkMode && this.need32) {
        env.info.print(s"\\nJavadesc.GenFieldOffs::generate(Seq[pc.Symbol]): maxoffs=$maxOffs\\n")
      }

      rel16(apply(objs), (if (this.need32) 1 else 0).toShort.toInt) // OFFS TO ARR | (need32 ? 1 : 0)
    }

    override def outData(objs: Seq[pc.Symbol]): Unit = {
      for (o <- objs) {
        val offs = this.getOffset(o)
        assert(offs >= 0)
        if (this.need32) {
          cd.genLWord(offs)
        } else {
          cd.genWord(offs.toShort)
        }
      }
    }

    def getOffset(f: pc.Symbol): Int = {
      assert(f.isInstanceOf[pcO.Field])
      f match {
        case f: pcO.InstanceField =>
          f.getOffset
        case f: pcO.StaticField =>
          if (f.isExternal || f.isAJFlat) {
            return RTConst.MetaInfo.UNREFLECTED_FIELD_OFFS.intValue
          }
          at.getBaseOffsAttr(f).offs
      }
    }
  }

  sealed trait OutRelArraySeg extends SegmentGeneratorByObjects {
    /** out tdrel16[] / tdrel32[] segment */
    def apply(objs: Seq[pc.Symbol]): pc.Symbol = {
      val sg = cd.makeSeg(outData(objs))
      objBySegm(sg, sg.fixups.nonEmpty)
    }
  }

  object GenThrows extends OutRelArraySeg {
    override def outData(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)
      for (o <- objs) {
        val m = o.asInstanceOf[pcO.Method]
        if (m.getThrowsCount != 0) {
          rel16(GenMethodThrows(m))
        } else {
          rel16(null)
        }
      }
    }
  }

  object GenParTDs extends OutRelArraySeg {
    override def outData(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)
      for (o <- objs) {
        val m = o.asInstanceOf[pcO.Method]
        if (m.getSignature.parameterTypes.nonEmpty) {
          rel16(GenMethodParTDs(m))
        } else {
          rel16(null)
        }
      }
    }
  }

  object GenParameters extends OutRelArraySeg {
    override def outData(objs: Seq[pc.Symbol]): Unit = {
      assert(objs.nonEmpty)
      for (o <- objs) {
        val m = o.asInstanceOf[pcO.Method]
        if (m.getParameters != null) {
          rel16(GenMethodParameters(m))
        } else {
          rel16(null)
        }
      }
    }
  }

  // -----------------------------------------------------------------------------

  private sealed trait SegmentGeneratorByMethod {
    def apply(m: pcO.Method): pc.Symbol = objBySegm(cd.makeSeg(outData(m)))

    protected def outData(m: pcO.Method): Unit
  }

  private object GenMethodParTDs extends SegmentGeneratorByMethod {
    override def outData(method: pcO.Method): Unit = {
      for (s <- method.getSignature.parameterTypes) {
        tdindex(sigTypeToO2Type(s))
      }
    }
  }

  private object GenMethodThrows extends SegmentGeneratorByMethod {
    override def outData(method: pcO.Method): Unit = method.getThrows foreach tdindex
  }

  private object GenMethodParameters extends SegmentGeneratorByMethod {

    override def outData(method: pcO.Method): Unit = {
      val pars = method.getParameters
      assert(pars != null) // this generator must be called only for methods
      // that have parameters attribute
      val lvtConverted = method.hasLVTConvertedParameters
      if (pars.length == 0) {
        stringRef(null)
        genInt(RTConst.Parameters.MALFORMED_WRONG_CPI_TYPE.intValue)
      } else if (pars.length != calcNpars(method).toInt) {
        // This way method.getParameters will throw MalformedParameterException
        // Let us just code this fact;
        stringRef(null)
        genInt(RTConst.Parameters.MALFORMED_WRONG_NUM.intValue)
      } else {
        for (i <- pars.indices) {
          if (pars(i).name == null) {
            stringRef(null)
            genInt((pars(i).accessFlags.toUInt + RTConst.Parameter.NULL_NAME_BIT_MASK.intValue.toUInt).toInt)
          } else {
            stringRef(pars(i).name)
            if (lvtConverted) {
              genInt(RTConst.Parameter.LVT_CONVERTED_BIT_MASK.intValue)
            } else {
              genInt(pars(i).accessFlags.toInt)
            }
          }
        }
      }
    }

  }

  //-----------------------------------------------------------------------------

  sealed trait SegmentGeneratorByType {
    protected def outData(type0: pcO.Class): Unit

    protected def apply(type0: pcO.Class, expandable16: Boolean = false): pc.Symbol = {
      objBySegm(cd.makeSeg {
        outData(type0)
      }, expandable16)
    }
  }

  sealed trait GenMembersData extends SegmentGeneratorByType {
    // iterate over methods (static, instance)
    def iterateMethods(type0: pcO.Class): Unit = {
      type0.symType.getGeneratedMethods map getO2Method foreach outDataForMember
    }

    override def outData(type0: pcO.Class): Unit = {
      this.iterateMethods(type0)
    }

    def outDataForMember(o: pcO.Member): Unit
  }

  sealed trait GenTypeAndMembersData extends GenMembersData {
    // iterate over type (ob_module), fields (static, instance),
    // methods (static, instance)
    // order is important!
    private def iterateTypeAndMembers(type0: pcO.Class): Unit = {
      outDataForType(type0)
      type0.declaredFields foreach { f =>
        outDataForMember(f)
      }
      iterateMethods(type0)
    }

    override def outData(type0: pcO.Class): Unit = {
      this.iterateTypeAndMembers(type0)
    }

    def outDataForType(c: pcO.Class): Unit
  }

  object GenGenericSig extends GenTypeAndMembersData {

    def generate(type0: pcO.Class): pc.Symbol = apply(type0)

    override def outDataForMember(o: pcO.Member): Unit = stringRef(o.getGenericSignature)

    override def outDataForType(c: pcO.Class): Unit = stringRef(c.getGenericSignature)
  }

  //-----------------------------------------------------------------------------

  import TypeMetaInfoWriter.Annotations.*

  sealed trait GenAnnotations extends SegmentGeneratorByType {
    var preparation: Boolean = _
    var nonEmpty: Boolean = _

    // Annotation utils
    def countAnnotLength(a: jcp.PtrAnnotation): Int = {
      var len = 2 + 2 // type index, num_element_value_pairs
      for (i <- a.pairs.indices) {
        len += 2 // element_name index
        len += countAnnotElementValueLength(a.pairs(i).value)
      }
      len
    }

    def countAnnotElementValueLength(ev: jcp.PtrElementValue): Int = {
      ev.tag match {
        case 'B' | 'C' | 'I' | 'S' | 'Z' | 'F' | 'J' | 'D' | 's' | 'c' =>
          1 + 2
        case 'e' =>
          1 + 4
        case '@' =>
          1 + countAnnotLength(ev.asInstanceOf[jcp.PtrAnnotationElementValue].value)
        case '[' =>
          val array_value = ev.asInstanceOf[jcp.PtrArrayElementValue]
          var len = 1 + 2 // tag, num_values
          for (i <- array_value.value.indices) {
            len += countAnnotElementValueLength(array_value.value(i))
          }
          len
        case _ =>
          throw new AssertionError
      }
    }

    def countAnnotArrLength(aa: Array[jcp.PtrAnnotation]): Int = {
      var len = 2 // number of elements in the array
      for (i <- aa.indices) {
        len += countAnnotLength(aa(i))
      }
      len
    }

    def generate0(type0: pcO.Class): pc.Symbol = {
      var seg_obj: pc.Symbol = null

      this.nonEmpty = false

      this.preparation = true
      this.outData(type0)
      this.preparation = false

      if (this.nonEmpty) {
        seg_obj = this (type0, expandable16 = true)
      } else {
        seg_obj = null
      }
      seg_obj
    }
  }

  object GenAnnotDefault extends GenAnnotations with GenMembersData {

    def generate(type0: pcO.Class): pc.Symbol = this.generate0(type0)

    override def outDataForMember(o: pcO.Member): Unit = {
      var length: Int = 0

      val m = o.asInstanceOf[pcO.Method]
      val attr = m.getAnnotationDefault

      if (this.preparation) {
        this.nonEmpty = this.nonEmpty || attr != null
        return
      }

      if (attr != null) {
        rel16(objBySegm(cd.makeSeg {
          if (!attr.isMalformed) {
            length = countAnnotElementValueLength(attr.defaultValue)
            cd.genLWord(length)
            putAnnotElementValue(attr.defaultValue)
          } else {
            length = 0
            cd.genLWord(length)
          }
          assert(cd.getCodeLen == length + 4)
        }))
      } else {
        rel16(null)
      }
    }

  }

  object GenParamsAnnot extends GenAnnotations with GenMembersData {

    private var rtVisible: Boolean = _
    private var generatedSize: Int = _

    def generate(type0: pcO.Class, rtVisible: Boolean): pc.Symbol = {
      this.rtVisible = rtVisible
      this.generatedSize = 0
      this.generate0(type0)
    }

    override def outDataForMember(o: pcO.Member): Unit = {
      var length: Int = 0

      val m = o.asInstanceOf[pcO.Method]
      val attr = m.getParameterAnnotations(this.rtVisible)

      if (this.preparation) {
        this.nonEmpty = this.nonEmpty || attr != null
        return
      }

      if (attr != null) {
        assert(attr.annotations.length <= 255)

        rel16(objBySegm(cd.makeSeg {
          if (!attr.isMalformed) {
            length = 1
            for (i <- attr.annotations.indices) {
              length += countAnnotArrLength(attr.annotations(i))
            }

            cd.genLWord(length)

            val nparams = attr.annotations.length
            cd.genByte(nparams)

            for (i <- 0 until nparams) {
              putAnnotArray(attr.annotations(i))
            }
          } else {
            length = 0
            cd.genLWord(length)
          }

          this.generatedSize += length + 4
          assert(cd.getCodeLen == length + 4)
        }))

      } else {
        rel16(null)
      }

      this.generatedSize += 2
    }

  }

  object GenRuntimeAnnot extends GenAnnotations with GenTypeAndMembersData {

    private var rtVisible: Boolean = _
    private var generatedSize: Int = _

    def generate(type0: pcO.Class, rtVisible: Boolean): pc.Symbol = {
      this.rtVisible = rtVisible
      this.generatedSize = 0
      this.generate0(type0)
    }

    override def outDataForMember(o: pcO.Member): Unit = {
      this.outAnnotationsAttr(o.getAnnotations(this.rtVisible))
    }

    override def outDataForType(c: pcO.Class): Unit = {
      this.outAnnotationsAttr(c.getAnnotations(this.rtVisible))
    }

    private def outAnnotationsAttr(attr: jcp.PtrAnnotationsAttr): Unit = {
      var length: Int = 0

      if (this.preparation) {
        this.nonEmpty = this.nonEmpty || attr != null
        return
      }

      if (attr != null) {
        rel16(objBySegm(cd.makeSeg {
          if (!attr.isMalformed) {
            length = countAnnotArrLength(attr.annotations)
            cd.genLWord(length)
            putAnnotArray(attr.annotations)
          } else {
            length = 0
            cd.genLWord(length)
          }

          this.generatedSize += length + 4
          assert(cd.getCodeLen == length + 4)
        }))

      } else {
        rel16(null)
      }

      this.generatedSize += 2
    }

  }

  object GenRuntimeTypeAnnot extends GenAnnotations with GenTypeAndMembersData {

    private var rtVisible: Boolean = _
    private var generatedSize: Int = _

    def generate(type0: pcO.Class, rtVisible: Boolean): pc.Symbol = {
      this.rtVisible = rtVisible
      this.generatedSize = 0
      this.generate0(type0)
    }

    override def outDataForMember(o: pcO.Member): Unit = {
      this.outAnnotationsAttr(o.getTypeAnnotations(this.rtVisible))
    }

    override def outDataForType(c: pcO.Class): Unit = {
      this.outAnnotationsAttr(c.getTypeAnnotations(this.rtVisible))
    }

    private def outAnnotationsAttr(attr: jcp.PtrTypeAnnotationsAttr): Unit = {
      def countTypeAnnotArrLength(aa: Array[jcp.PtrTypeAnnotation]): Int = {
        def countTypeAnnotLength(a: jcp.PtrTypeAnnotation): Int = {
          val targetInfoLen = a.targetInfo match {
            case _: jcp.PtrOneByteTargetInfo => 1
            case _: jcp.PtrTwoBytesTargetInfo => 2
            case _: jcp.PtrWordTargetInfo => 2
            case _ => 0
          }
          1 + targetInfoLen + 1 + a.pathLength * 2 + countAnnotLength(a.annotation)
        }


        var len = 2 // number of elements in the array
        for (i <- aa.indices) {
          len += countTypeAnnotLength(aa(i))
        }
        len
      }

      var length: Int = 0

      if (this.preparation) {
        this.nonEmpty = this.nonEmpty || attr != null
        return
      }

      if (attr != null) {
        rel16(objBySegm(cd.makeSeg {
          if (!attr.isMalformed) {
            length = countTypeAnnotArrLength(attr.typeAnnotations)
            cd.genLWord(length)
            putTypeAnnotArray(attr.typeAnnotations)
          } else {
            // sun.reflect.TypeAnnotationParser does not catch
            // java.nio.BufferUnderflowException for retrieving number of annotations
            // (unlike sun.reflect.AnnotationParser) so let us put fake data for it.
            length = 2
            cd.genLWord(length)
            BigEndian.addW16(1.toShort) // classfile annotations raw data has big-endian byte order
          }

          this.generatedSize += length + 4
          assert(cd.getCodeLen == length + 4)
        }))

      } else {
        rel16(null)
      }

      this.generatedSize += 2
    }

  }
}
