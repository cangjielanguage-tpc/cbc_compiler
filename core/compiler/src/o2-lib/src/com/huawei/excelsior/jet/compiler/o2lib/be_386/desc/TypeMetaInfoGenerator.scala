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
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.layout.FieldsLayout
import com.huawei.excelsior.jet.compiler.layout.FieldsLayout.FieldOffs
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import MetaInfoEmitterDSL.*
import MetaInfoEmitterDSL.EntryWithValue.*
import MetaInfoEmitter.{MetaInfo, MetaInfoType}
import MetaInfoEmitter.MetaInfoType.*
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.o2lib.be_386.CodeDefModule.*
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.SegmentGenerators.*
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator.SegmentManipulations.{addObjectToCurrentMetaInfo, objBySegm}
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator.Utils.*
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoWriter.*
import com.huawei.excelsior.jet.compiler.o2lib.be_386.opAttrsModule.{BOBJECT, TDReflection, currClass}
import com.huawei.excelsior.jet.compiler.o2lib.be_386.{CodeDefModule as cd, opAttrsModule as at, opDefModule as def0, opStdModule as std}
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, NumerateModule as Numerate, ObjNamesModule as nms, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JavaClassParserModule as jcp
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.cangjie.CangjieMain
import com.huawei.excelsior.jet.compiler.o2lib.tools.NamesCommon.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{ClassID, Hashtable, CacheAPIModule as CacheAPI, GetPackageSupportModule as GetPackageSupport, JStringsModule as js, PropertiesModule as Properties, xiEnvModule as env}
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.options.BoolOption.{AOTCPStats, LogBootstrapPromotion}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.*
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*
import com.huawei.excelsior.jet.util.ScalaCollections.sumBy
import com.huawei.excelsior.jet.compiler.{RTConst, RTSGlobal, TypeProvider, symlevel}
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.o2j.runtime.*
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.util.MathUtils.{high32Bits, isAligned, low32Bits}
import xscala.util.UShort

import java.lang.Double.doubleToRawLongBits
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object TypeMetaInfoGenerator {

  val INVALID_OFFSET: Int = Int.MinValue

  private var datasize: Int = 0

  private var metaWhiteList: mutable.HashSet[XString] = _

  implicit def typeProvider: TypeProvider = LightweightEnvironment.getInstance

  object AOTConstantPool {
    private var aotConstantPool: ArrayBuffer[jcp.ConstantInfo] = _
    private var mhClassEntryIdx: UShort = _

    var CPSizeTotal: Int = 0

    inline def checkPlaceholder(i: Int): Unit = assert(aotConstantPool(i + 1) eq jcp.placeholderCPElement)

    inline def ensureAOTConstantPoolIsEmpty(): Unit = assert(aotConstantPool == null, "AOT constant pool is not empty")


    import TypeMetaInfoWriter.Annotations.*

    private def countAnnotElementValueLength(ev: jcp.PtrElementValue): Int = {
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

    private def countAnnotLength(a: jcp.PtrAnnotation): Int = {
      var len = 2 + 2 // type index, num_element_value_pairs
      for (i <- a.pairs.indices) {
        len += 2 // element_name index
        len += countAnnotElementValueLength(a.pairs(i).value)
      }
      len
    }


    private val bootstrapMethodsAttr = ArrayBuffer.empty[jcp.PtrBootstrapMethod]

    private def addAOTConstantPoolEntryFromCP(C: jcp.PtrClassInfo, index: Int): UShort = {
      def isEntryOfSigpolyMethod(cp: Array[jcp.ConstantInfo], index: Int): Boolean = {
        if (cp(index).constantType != jcp.TagMethod.toByte) {
          return false
        }

        val className = cp(cp(cp(index).index.toInt).indexName.toInt).bufferPtr /*Method.Class*/ /*Class.Name*/
        if (!className.equals2("java/lang/invoke/MethodHandle")) {
          return false
        }

        val methodName = cp(cp(cp(index).indexName.toInt).indexName.toInt).bufferPtr /*Method.n&t*/ /*n&t.Name*/

        methodName.equals2("invoke") || methodName.equals2("invokeExact") || methodName.equals2("invokeBasic") ||
          methodName.equals2("linkToVirtual") || methodName.equals2("linkToStatic") ||
          methodName.equals2("linkToSpecial") || methodName.equals2("linkToInterface")
      }

      def addBootstrapMethodFromCP(C: jcp.PtrClassInfo, bootMethodAttrIndex: Int): UShort = {
        def addBootstrapMethod(m: jcp.PtrBootstrapMethod): UShort = {
          var index = bootstrapMethodsAttr.indexOf(m)
          if (index == -1) {
            index = bootstrapMethodsAttr.size
            bootstrapMethodsAttr += m
          }
          index.toUShort
        }

        var args: Array[UShort] = null
        var argsNum: Int = 0
        val attr = jcp.getAttribute(C, C.attribute, C.attributeCount.toInt, jcp.jstrBootstrapMethods).get

        val bootMethodIndex = attr.bootstrapMethods(bootMethodAttrIndex).methodIndex
        val bootMethodArgs = attr.bootstrapMethods(bootMethodAttrIndex).args

        if (bootMethodArgs == null) {
          argsNum = 0
          args = null
        } else {
          argsNum = bootMethodArgs.length
          args = new Array[UShort](argsNum)
        }

        for (i <- 0 until argsNum) {
          args(i) = addAOTConstantPoolEntryFromCP(C, bootMethodArgs(i).toInt)
        }

        addBootstrapMethod(jcp.PtrBootstrapMethod(addAOTConstantPoolEntryFromCP(C, bootMethodIndex.toInt), args))
      }

      val e = new jcp.ConstantInfo()
      val cp = C.constantPool
      e.constantType = cp(index).constantType

      e.constantType match {
        case jcp.TagUtf8 =>
          e.bufferPtr = cp(index).bufferPtr
        case jcp.TagInteger |
             jcp.TagLong =>
          e.low = cp(index).low
          e.high = cp(index).high
        case jcp.TagFloat =>
          e.realVal = cp(index).realVal
        case jcp.TagDouble =>
          e.longRealVal = cp(index).longRealVal
        case jcp.TagClass =>
          e.indexName = addAOTConstantPoolEntryFromCP(C, cp(index).indexName.toInt)
          e.high = index
        case jcp.TagMethodType =>
          e.index = addAOTConstantPoolEntryFromCP(C, cp(index).index.toInt)
          e.high = index
        case jcp.TagString =>
          e.index = addAOTConstantPoolEntryFromCP(C, cp(index).index.toInt)
        case jcp.TagField |
             jcp.TagMethod |
             jcp.TagIMethod |
             jcp.TagNameAndType =>
          e.index = addAOTConstantPoolEntryFromCP(C, cp(index).index.toInt)
          e.indexName = addAOTConstantPoolEntryFromCP(C, cp(index).indexName.toInt)

          if (isEntryOfSigpolyMethod(cp, index)) {
            e.constantType = jcp.TagSigpolyMethod.toByte
            e.high = index
            if (mhClassEntryIdx == UShort(0)) {
              mhClassEntryIdx = e.index
            } else {
              assert(mhClassEntryIdx == e.index)
            }
          }
        case jcp.TagMethodHandle =>
          e.index = addAOTConstantPoolEntryFromCP(C, cp(index).index.toInt)
          e.low = cp(index).low
          e.high = index
        case jcp.TagInvokeDynamic =>
          e.index = addBootstrapMethodFromCP(C, cp(index).index.toInt)
          e.indexName = addAOTConstantPoolEntryFromCP(C, cp(index).indexName.toInt)
      }

      addAOTConstantPoolEntry(e)
    }

    def addAOTConstantPoolEntry(e: jcp.ConstantInfo): UShort = {
      def ensureAOTConstantPoolCreated(): Unit = {
        if (aotConstantPool == null) {
          aotConstantPool = new ArrayBuffer[jcp.ConstantInfo]
          aotConstantPool += jcp.placeholderCPElement // 0 must be invalid index
        }
      }

      ensureAOTConstantPoolCreated()

      if (mhClassEntryIdx != UShort(0)) {
        if (e.constantType == jcp.TagClass.toByte) {
          if (e.indexName == aotConstantPool(mhClassEntryIdx.toInt).indexName) {
            // store only one Class entry for MethodHandle class
            return mhClassEntryIdx
          }
        }
      }

      var index = aotConstantPool.indexOf(e)
      if (index == -1) {
        index = aotConstantPool.size
        aotConstantPool += e
        e.constantType match {
          case jcp.TagLong |
               jcp.TagDouble =>
            // occupy additional index
            aotConstantPool += jcp.placeholderCPElement
          case _ =>
        }
      }
      index.toUShort
    }

    /**
      * Returns index of an AOT constant pool entry containing given <pre>importIndex</pre> of a class or interface.
      */
    def addAOTConstantPoolClassRefEntry(importIndex: UShort): UShort = {
      val e = new jcp.ConstantInfo()
      e.constantType = jcp.TagAOTClassRef.toByte
      e.index = importIndex
      addAOTConstantPoolEntry(e)
    }

    def addAOTConstantPoolEntryFromClass(to0: pcO.Class, from: pcO.Class, index: Int): UShort = {
      assert(to0 eq at.currClass)
      addAOTConstantPoolEntryFromCP(from.classInfo, index)
    }

    def genAOTConstantPool(): Segment = {
      var value: Int = 0

      if (aotConstantPool == null) {
        return null
      }

      cd.makeSeg(4) {
        val elementsCount = aotConstantPool.size
        assert(elementsCount > 1) // 0th entry is placeholder and there must be at least one real entry

        assert(elementsCount == (elementsCount & 0xFFFF))
        cd.genWord(elementsCount.toShort)

        assert(mhClassEntryIdx == (mhClassEntryIdx & 0xFFFF.toUShort))
        cd.genWord(mhClassEntryIdx.toShort)

        // note that elementsCount and mhClassEntryIdx together occupy 0th entry now
        // write entries
        var i = 1
        loop {
          val e = aotConstantPool(i)
          e.constantType match {
            case jcp.TagInteger =>
              cd.genLWord(e.low)
            case jcp.TagFloat =>
              cd.genFloat(e.realVal)
            case jcp.TagLong =>
              genLongElement(e.low, e.high, i)
              i += 1 // skip placeholder
            case jcp.TagDouble =>
              val bits = doubleToRawLongBits(e.longRealVal)
              genLongElement(low32Bits(bits), high32Bits(bits), i)
              i += 1 // skip placeholder
            case jcp.TagUtf8 =>
              val strTable = at.currClass.getStringTable
              if (strTable != null) {
                value = strTable.getIndexByStringIfPresent(e.bufferPtr)
              } else {
                value = -1
              }
              if (value != -1) {
                cd.addFixup(BYTE_STR_32, strTable.getStringHolder(value), 0)
              } else {
                stringRef(e.bufferPtr)
              }
            case jcp.TagClass =>
              val high = e.high // import index
              val low = e.indexName.toInt // class name
              cd.genLWord(O2JSupport.logicLeftShift(high, 32, 16) | low)
            case jcp.TagMethodType =>
              val high = e.high // import index
              val low = e.index.toInt // sig index
              cd.genLWord(O2JSupport.logicLeftShift(high, 32, 16) | low)
            case jcp.TagString |
                 jcp.TagAOTClassRef =>
              cd.genLWord(e.index.toInt)
            case jcp.TagField |
                 jcp.TagMethod |
                 jcp.TagIMethod =>
              val high = e.index.toInt // class
              val low = e.indexName.toInt // n&t
              cd.genLWord(O2JSupport.logicLeftShift(high, 32, 16) | low)
            case jcp.TagNameAndType =>
              val high = e.indexName.toInt // name
              val low = e.index.toInt // sig
              cd.genLWord(O2JSupport.logicLeftShift(high, 32, 16) | low)
            case jcp.TagMethodHandle =>
              val high = e.high // import index
              val low = e.index.toInt // refIdx
              cd.genLWord(O2JSupport.logicLeftShift(high, 32, 16) | low)
            case jcp.TagInvokeDynamic =>
              val high = e.index.toInt // bootstrap method index
              val low = e.indexName.toInt // n&t
              cd.genLWord(O2JSupport.logicLeftShift(high, 32, 16) | low)
            case jcp.TagSigpolyMethod =>
              assert(mhClassEntryIdx != UShort(0))
              val high = e.high // import index
              val low = e.indexName.toInt // n&t
              cd.genLWord(O2JSupport.logicLeftShift(high, 32, 16) | low)
          }

          i += 1
          if (i == elementsCount) {
            break()
          }
        }

        // write tags for checks separately: so that reads of entries are always aligned
        for (i <- 0 until elementsCount) {
          val e = aotConstantPool(i)
          value = (e.constantType & 0x1F.toByte).toByte.toInt
          // We store tags in lower 5 bits, because there are less than 32 tags for now.
          assert(value == e.constantType.toInt)
          if (e.constantType == jcp.TagMethodHandle.toByte) {
            value = O2JSupport.logicLeftShift(e.low, 32, 4) | value // refKind | tag
            // Higher 4 bits of entry with tag 0FH are used to store value of its refKind (which has less than 16 values).
            // Note that for tags we use lower 5 bits, so the lower bit of refKind becomes the higher bit of tag.
            // Thus, we must treat tag with value 1FH same as the one with value 0FH.
            // Luckily there is no tag with value 1FH for now.
          }
          cd.genByte(value)
        }

        aotConstantPool = null
        mhClassEntryIdx = UShort(0)

        genBootstrapMethodsAttr(bootstrapMethodsAttr)

        if (env.config.option(s"$AOTCPStats")) {
          env.info.print("\\n====AOT Constant Pool==== class: %S, size: %d ====\\n", at.currClass.name, cd.getCodeLen)
          CPSizeTotal += cd.getCodeLen
        }
      }
    }
  }

  object Imports {
    // properly ordered import list
    /*RO*/ var importTable: Hashtable = _ /*<Class, Int>*/
    // Minimal available number for new entry in import list
    private var importLength: Int = _
    // Segment containing expandable array of imported types
    /*RO*/ var importTableSeg: Segment = _

    def writeImport(imp: pc.SymType): Unit = {
      assert(importTableSeg != null)
      cd.withSeg(importTableSeg) {
        tdindex(imp)
      }

      val old = importTable.put(imp, Integer.valueOf(importLength))
      assert(old == null)

      importLength += 1
    }

    def getImportedClassIdx(imp: pc.SymType): Int = {
      if (!importTable.containsKey(imp)) {
        val idx = importLength
        writeImport(imp)
        return idx
      }

      importTable.get(imp).asInstanceOf[Integer]
    }

    def outImportTable(): Unit = {
      def meaningfullImport(from: pcO.Class, imp: pcO.Class): Boolean = {
        if (from.isNoJavaClass && !imp.isNoJavaClass) {
          // We should not import anything from Java for no-Java Classes (lightweight runtime build).
          // Nevertheless import to j.l.Object, j.l.String, j.l.Throwable can persist
          // because AJ classes are compiled to verifiable Java bytecode that
          // requires to have j.l.Object as super class (implicitly) for types such as AJObject,
          // string literals for bstr/ustr intrinsics to have j.l.String type,
          // CONSTR_FAILED to have j.l.Throwbale type, etc.
          assert(imp.name.startsWith(js.newJString("java/"), 0) ||
            // TODO: remove when scala lib is fully integrated
            imp.name.startsWith(js.newJString("scala/"), 0),
            s"import of java class ${imp.name} from no java class ${from.name}")

          return false
        }

        if (from.isNoJavaClass && imp.isAnnotation) {
          // WORKAROUND JET-13029
          // skip AJ annotations that somehow managed to get through all other checks
          // e.g. by InnerClass attribute
          return false
        }

        imp.hasManagedMetaInformation && !imp.isShielded && !imp.isClassDefinitionError
      }

      importTable = new Hashtable()
      importLength = 0
      importTableSeg = cd.newSeg()

      for (imp <- at.currClass.getImport) {
        if (meaningfullImport(at.currClass, imp)) {
          writeImport(imp)
        }
      }
      writeImport(null)
    }

    def exit(): Unit = {
      importTable = null
      importTableSeg = null
    }
  }

  import AOTConstantPool.*

  object SegmentManipulations {
    def objBySegm(seg: Segment, expandable16: Boolean = false): pc.Symbol = {
      for (o <- MetaInfo.getCurrentMetaInfo.getObjects if isExpandable16(o) == expandable16) {
        val thatSeg = at.getSegment(o)
        if (thatSeg != null && segmentsHaveSameBytesAndFixups(thatSeg, seg)) {
          return o
        }
      }

      // Not found, create
      at.setSegment(addObjectToCurrentMetaInfo(expandable16), seg)
    }

    def addObjectToCurrentMetaInfo(expandable16: Boolean = false): pc.Symbol = {
      MetaInfo.getCurrentMetaInfo.addObject(
        if (expandable16) {
          Expandable16BOBJECT(currClass.mno)
        } else {
          BOBJECT(currClass.mno)
        })
    }
  }

  object Utils {
    class Expandable16BOBJECT(_mno: Int) extends BOBJECT(_mno) {
      var expandedTo32: Boolean = false
    }

    def isExpandable16(obj: pc.Symbol): Boolean = obj.isInstanceOf[Expandable16BOBJECT]

    /** Add all interface of class, if addSuper = true recursively add superinterface of them */
    private def addIntfs(t: pcO.Class, addSuper: Boolean): Seq[pcO.Class] = {
      val result = mutable.ArrayBuffer.empty[pcO.Class]
      for (interf <- t.getSuperInterfacesO2) {
        result += interf
        result ++= (if (addSuper) {
          addIntfs(interf, addSuper = true)
        } else {
          Nil
        })
      }
      result.distinct.toSeq
    }

    def getDirectSuperInterfaces(t: pcO.Class): Seq[pcO.Class] = {
      // TODO: JET-7463
      addIntfs(t, addSuper = false)
    }

    def getAllSuperInterfaces(t: pcO.Class): Seq[pcO.Class] = {
      if (!t.isVerifiable) {
        return Nil
      }
      val super0 = t.getSuperClassO2
      ((if (super0 != null) {
        getAllSuperInterfaces(super0)
      } else {
        Nil
      }) ++ addIntfs(t, addSuper = true)).distinct
    }

    def shouldNotGenerateMetaInfo(t: pcO.Class): Boolean = {
      if (t.isAJManagedType || t.isCangjieType || t.isXScalaType) {
        return true
      }

      if (env.config.option("GenMetaInfoForRuntimeClasses")) {
        return false
      }

      if (!t.isJetRuntimeClass) {
        return false // always generate info for non-runtime classes
      }

      if (t.isJetRuntimeEnum) {
        return false // always generate info for enums
      }

      if (metaWhiteList == null) {
        metaWhiteList = env.convValueToSet(Properties.getJCProperty("metaWhiteList"))
      }

      !metaWhiteList.contains(t.name)
    }

    def makePackageName(cls: pcO.Class, symbolName: Boolean): XString = {
      // TODO: JET-7463

      if (nms.getPackageName(cls) == null) {
        return null
      }
      val jar: XString = if (cls.isSystemClass) {
        null
      } else if (GetPackageSupport.getPackageInfo(cls) == null) {
        null // there's no package-specific info
      } else {
        GetPackageSupport.getJarName(cls)
      }
      nms.makePackageName(cls, jar, cls.isSystemClass, symbolName)
    }

    def allocateInstanceDescriptor(c: pcO.Class, size: Int): pc.DataSymbol.InstanceDescriptor = {
      val desc = c.instanceDescriptor
      desc.size = Some(size)
      desc
    }

    def genMethodCodeTable(t: pcO.Class): pc.Symbol = {
      /** We can't generate RVA for external methods in case of prelinker (see JET-12422)
        * But on the other hand we really need it in CompilerInterface for JIT.
        * So, here we somehow improve only case of Managed classes with external methods.
        * TODO make it right
        */
      def isMethodCodeAvailable(m: pcO.Method): Boolean = m.shouldBeGenerated &&
        (!(m.isExternal && (t.isAJManagedType || t.isCangjieType)) || CacheAPI.isThisClass(t, ClassID.CompilerInterface))

      val methods = t.symType.getGeneratedMethods.map(getO2Method).toSeq

      val emptyTable = !methods.exists(isMethodCodeAvailable)
      val it = ExteriorMethodsVersioning.getIteratorOverVersionedMethods(t)
      if (emptyTable && it.isEmpty) {
        return null
      }

      assert(t.isVerifiable)

      objBySegm(cd.makeSeg {
        for (m <- methods) {
          if (!isMethodCodeAvailable(m)) {
            rva32(null)
          } else {
            rva32(m)
          }
        }
        for (versioned <- it) {
          rva32(versioned.bodyObj)
        }
      })
    }

    def printStatistics(): Unit = {
      env.info.print(s"stub data size: $datasize\\n")
    }
  }

  private object CreateATD extends at.ModuleVisitor {
    // TODO: replace TypeHandle by RTTI
    override def absentContainer(atd: pc.DataSymbol.TypeHandle): Unit = {
      val klass = atd.tpe.asInstanceOf[pcO.Class]

      assert(atd.size.isDefined)

      // Note: this hack is required in order to correctly process fixups below.
      // TODO: feel free to rework this (and probably formOMF.ObjPlacer)
      atd.mno = at.currClass.mno

      val classname = nms.getClassName(klass, CL_slash)

      val absentContainer = struct(AbsentContainer)(
        "isAbsentArray" -> cd.genByte(0),
        "name" -> stringRef(classname),
        "fromClass" -> fixup(at.currClass.typeHandle),
      ).attachObject(atd).gatherSegments()

      pcO.setPlainArrayLength(atd, absentContainer.getSegment.length)

      datasize += absentContainer.getSegment.length
      alignRawObject(atd, addressSize)
    }
  }

  def stubAbsentClasses(): Unit = {
    at.iterateModule(CreateATD)
  }

  def createBootstrapObject(t: pcO.Class): Unit = {
    val bootstrapPreparationRequirements = typesForBootstrapPreparation
    if (bootstrapPreparationRequirements.nonEmpty) {
      val log = O2Env.env.enabled(LogBootstrapPromotion)
      if (log) env.info.forcePrint("\\n")
      val bootstrapSegm = makeSeg {
        for (x <- bootstrapPreparationRequirements.drain) {
          tdindex(x)
          if (log) env.info.forcePrint(s"[BOOTSTRAP:JavaDesc] ${classByO2Object(x)} from ${classByO2Object(t)}\\n")
        }
      }

      assert(bootstrapSegm.length > 0)

      at.setSegment(at.createSpecialObject(at.BootstrapRequirements), bootstrapSegm)
    }
  }

  /** Generate managed descriptor (ClassOrInterfTypeHandle/RTTI) and return MetaInfo (RTTI). */
  private def createClassOrInterfTypeHandle(base: MetaInfo)(t: pcO.Class): MetaInfo = {
    def isNative(m: pcO.Method) = m.isDeclaredNative && !m.isAjReplaced && !t.isCangjieType

    def getPackageRef: pc.Symbol = {
      if (!classByO2Object(t).isJavaReference) {
        null
      } else {
        val s = makePackageName(t, symbolName = true)
        if (s == null) {
          null
        } else {
          at.newUnsizedConst(s, pcO.x2cClass)
        }
      }
    }

    def getTypeModifiers: Int = {
      var mdf = t.getModifiers.toInt
      if (t.isDeprecated) {
        mdf += RTConst.TypeModifiers.DEPRECATED.intValue
      }
      if (t.isJetRuntimeClass) {
        mdf += RTConst.TypeModifiers.RUNTIME.intValue
      }
      if (shouldNotGenerateMetaInfo(t)) {
        mdf += RTConst.TypeModifiers.NO_META_INFO.intValue
      }
      if (!t.isInterface && t.symType.finalizable) {
        mdf += RTConst.TypeModifiers.FINALIZABLE.intValue
      }
      if (t.isVerifiable && !t.needVerify) {
        mdf += RTConst.TypeModifiers.VERIFIED.intValue
      }
      if (t.isClassDefinitionError) {
        mdf += RTConst.TypeModifiers.CLASS_DEF_ERROR.intValue
      }
      if (!t.isVerifiable) {
        mdf += RTConst.TypeModifiers.VERIFY_ERROR.intValue
      }
      if (t.isPlatformClass || env.shouldGenerateVCF()) {
        if (t.isVerifiable && !t.isVCFExcluded) {
          mdf += RTConst.TypeModifiers.HAS_VCF.intValue
        }
      }
      if (t.isAnonymous) {
        mdf += RTConst.TypeModifiers.ANONYMOUS.intValue
      }
      if (t.isThrowableSubclass) {
        mdf += RTConst.TypeModifiers.THROWABLE.intValue
      }
      if (t.isMethodAccessorImplSubclass) {
        mdf += RTConst.TypeModifiers.METHOD_ACCESSOR_IMPL.intValue
      }
      if (t.hasDefaults(withClinit = false)) {
        mdf += RTConst.TypeModifiers.HAS_DEFAULTS.intValue
      }
      if (t.hasSuperInterfacesWithDefaults(withClinit = false)) {
        mdf += RTConst.TypeModifiers.SUPERINTERFS_HAVE_DEFAULTS.intValue
      }
      if (t.isHideDeprecatedInCPMode) {
        mdf += RTConst.TypeModifiers.HIDE_DEPRECATED_IN_CP_MODE.intValue
      }
      if (t.isAJManagedType) {
        mdf += RTConst.TypeModifiers.LIGHT.intValue
      }
      if (t.isCangjieType) {
        mdf += RTConst.TypeModifiers.CANGJIE.intValue
      }
      if (CangjieMain.coldStrings != null) {
        mdf += RTConst.TypeModifiers.HAS_CANGJIE_COLD_STRINGS_AT_STDLIB_CBC.intValue
      }
      mdf
    }

    def genInitInfo(sfObjects: Int): pc.Symbol = {
      def prepareSfieldInits(objs: Int): (pc.Symbol, Int) = {
        var staticFieldsInits: pc.Symbol = null
        var staticFieldsInitsNum = 0

        var sFieldsSeg: Segment = null

        for (f <- t.declaredStaticFields) {
          val stringIndex = f.getConstStringValue
          if (stringIndex >= 0) {
            assert(!f.isExternal)

            val table = t.getStringTable
            assert(table != null)
            assert(stringIndex < table.getLength)

            if (staticFieldsInits == null) {
              staticFieldsInits = addObjectToCurrentMetaInfo()
              sFieldsSeg = cd.newSeg()
            }

            cd.withSeg(sFieldsSeg) {
              val offs = GenFieldOffs.getOffset(f)
              assert(isAligned(offs, addressSize))
              assert(offs >= 0)
              assert(offs < objs * addressSize)

              // calculate index of field among static object fields
              val sfieldIndex = O2JSupport.div(offs, addressSize)

              assert(sfieldIndex <= UShort.MaxValue)
              assert(stringIndex <= UShort.MaxValue)

              cd.genWord(sfieldIndex.toShort) // sfieldIndex  :CARD16;
              cd.genWord(stringIndex.toShort) // stringIndex  :CARD16;

              staticFieldsInitsNum += 1
            }
          }
        }

        if (staticFieldsInits != null) {
          at.setSegment(staticFieldsInits, sFieldsSeg)
        }
        (staticFieldsInits, staticFieldsInitsNum)
      }

      val (staticFieldInits, staticFieldInitsNum) = prepareSfieldInits(sfObjects)

      val strTable = at.currClass.getStringTable
      val strNum = if (strTable == null) {
        0
      } else {
        strTable.getLength
      }

      if (strNum > 0 || staticFieldInitsNum > 0) {
        assert(staticFieldInitsNum <= UShort.MaxValue)

        subStruct(TDInitInfo)(
          "localStringPool" -> rva32(strTable),
          "staticFieldInitializers" -> rva32(staticFieldInits),
          "staticFieldInitNum" -> genShort(staticFieldInitsNum.toShort)
        ).attachObject(addObjectToCurrentMetaInfo()).getAttachedObject
      } else {
        null
      }
    }

    def getSfieldData(t: pcO.Class): (pc.Symbol, Int) = {
      def isTracedField(f: pcO.Field): Boolean = f.sig.isTraceableReference

      def addToBundle(field: pcO.StaticField, bundlePar: pc.DataSymbol.Sized, offsetPar: Int): pc.DataSymbol.Sized = {
        assert(offsetPar >= 0)
        var bundle = bundlePar
        var offset = offsetPar
        var sg: Segment = null

        assert(!field.isExternal)

        if (bundle == null) {
          bundle = at.createSpecialObject(at.StaticBundle)
          pcO.setPlainArrayLength(bundle, offset)
        }

        assert(!at.hasBaseOffsAttr(field))
        at.setBaseOffsAttr(field, bundle, offset)

        val bsize = def0.objectSize(bundle)
        assert(offset == Numerate.mkAlign(bsize, field.alignment))

        val empty = !bundle.ownsSegment

        if (empty && field.value == null) { // uninitialized var
          offset += field.size
          pcO.setPlainArrayLength(bundle, offset)
          return bundle
        }

        if (empty) {
          sg = cd.newSeg()
        } else {
          sg = at.getSegment(bundle)
          assert(bsize == sg.length)
        }

        cd.withSeg(sg) {
          if (field.value == null) {
            offset += field.size
            sg.putZeroes(offset - sg.length)
          } else {
            sg.putZeroes(offset - sg.length)
            def0.putStaticFieldValue(field)
            offset += field.size
          }
          assert(offset == sg.length)
        }

        if (empty) {
          at.setSegment(bundle, sg)
        }
        pcO.setPlainArrayLength(bundle, offset)

        bundle
      }

      var bundle: pc.DataSymbol.Sized = null
      var objs = 0

      val fieldOffsets = FieldsLayout.staticFieldsLayout(t.asCT)(LightweightEnvironment.getInstance)

      for (FieldOffs(f, offs) <- fieldOffsets) {
        val field = fieldToO2Field(f).asInstanceOf[pcO.StaticField]
        assert(offs == field.getOffset)
        bundle = addToBundle(field, bundle, offs)

        if (isTracedField(field)) {
          objs += 1
        }
      }

      if (bundle != null) {
        alignRawObject(bundle, RTConst.HeapObj.alignment)
      }

      (bundle, objs)
    }

    def genInstanceDescriptor(): pc.Symbol = {
      // TODO: consider not allocating InstanceDescriptor for abstract types
      val procsLen = if (O2TypeProvider.isXScalaAnyRef(t)) {
        // We must allocate the same size for supertype of all arrays as for each array,
        // because when array is initialized it does memcpy of ArrayBuilder.ALLOC_SIZE bytes from supertype.
        // So if AnyRef has size less than that, there will be memory corruption of subsequent array descriptors.
        // FIXME: must be reworked somehow
        RTConst.ScalaInstanceDescriptor.ArrayBuilder.VMT_SIZE.intValue
      } else if (t.isAbstract || !t.isVerifiable) {
        0
      } else {
        t.getVMTSize
      }

      val vmtOffs = if (t.isAJManagedType) {
        RTConst.ManagedInstanceDescriptor.VMT_OFFSET.intValue
      } else if (t.isCangjieType) {
        RTConst.CangjieInstanceDescriptor.VMT_OFFSET.intValue
      } else if (t.isXScalaType) {
        RTConst.ScalaInstanceDescriptor.VMT_OFFSET.intValue
      } else {
        RTConst.JavaInstanceDescriptor.VMT_OFFSET.intValue
      }

      val descSize = vmtOffs + procsLen * addressSize

      if (O2TypeProvider.isXScalaAnyRef(t)) {
        assert(descSize == RTConst.ScalaInstanceDescriptor.ArrayBuilder.ALLOC_SIZE.intValue)
      }

      allocateInstanceDescriptor(t, descSize)
    }

    def allocateSingletonObject(): pc.DataSymbol.SingletonObject = {
      assert(t.hasInstanceDescriptor, t.name)
      assert(!t.symType.classHasRefFields)
      t.singletonObject
    }


    val methods: Seq[pcO.Method] = t.symType.getGeneratedMethods.map(getO2Method).toSeq
    val shouldGenerateMethodIDs = (t.isAJManagedType || t.isCangjieType || t.isXScalaType) &&
      (O2Env.env.enabled(BoolOption.GenMethodIDs) || at.genStackTrace) && methods.nonEmpty
    val nativeMethods = methods.filter(isNative)


    def getTildaInitIndex: Int = if (pcO.isCangjie) methods.indexWhere(_.isFinalize) else -1

    def getClinitIndex: Int = {
      for ((m, i) <- methods.zipWithIndex) {
        if (m.isClinit || m.isPackageInit) {
          return i
        }
      }

      assert(!t.hasClinit)
      -1 // NoClinitIndex
    }

    val (sfBundle, sfObjects) = getSfieldData(t)

    struct(base >> HostingTypeHandle)(
      "sourceFile"  -> ((t.isAJManagedType || t.isCangjieType || t.isXScalaType) && at.genStackTrace) ? stringRef(t.getBCSourceName),

      "name"            -> rel32(outStr(nms.getClassName(t, CL_slash))),
      "typesTable"      -> rva32(std.dataSymbol(RTSGlobal.LINK_LocalTypesTable)),
      "methodCodeTable" -> rva32(genMethodCodeTable(t)),

      "serialTypeInfo" -> (!t.isClassDefinitionError) ? rva32(DataGen.genSerialTypeInfo(t)),
      "methodIDs"      -> shouldGenerateMethodIDs ? rva32(GenMethodIDs(methods)),

      "customTypeInfo" -> classByO2Object(t).isJavaReference ? flatStruct(JavaCustomTypeInfo)(
        "metaInfoUnion" -> flatStruct(MetaInfoUnion)(
          "aotMetaInfo" -> fixup(at.findSpecialObject(at.TDReflection))
        ),
        "packageInfo"   -> fixup(getPackageRef),
        "hostClass"     -> tdindex(t.hostClass),
        "accFlags"      -> genSet16(t.getAccessFlags),
      ),

      "classloaderID" -> genInt(t.getClassloaderID),
      "modifiers"     -> genInt(getTypeModifiers),
      "initialized"   -> (!t.isCangjiePackage && t.isPreclinited) ? genAddrInt(1),
      "clinitIndex"   -> genInt(getClinitIndex),

      "allSuperInterfacesNum" -> genShort(getAllSuperInterfaces(t).length),

      "nativeNum" -> genUInt16(nativeMethods.length),
      "nativeMethodUnion" -> nativeMethods.nonEmpty ? flatStruct(NativeMethodUnion)(
        "nativeDescs"     -> rva32(GenNatives(nativeMethods)),
      ),

      "sfBundle"  -> rvaRef(sfBundle),
      "sfObjects" -> genUInt16(sfObjects),

      "initInfo" -> rva32(genInitInfo(sfObjects)),
      "vmtEncoding" -> (!t.isClassDefinitionError) ? rva32(DataGen.genVMTEncoding(t)),

      "methodTableSize" -> t.isVerifiable ? genInt(t.getVMTSize),
      "methodStackParamSizes" -> shouldGenerateMethodIDs ? rva32(GenFrameSz(methods)),

      "tildaInitIndex" -> genInt(getTildaInitIndex),

      "instanceDescriptor" -> t.hasInstanceDescriptor ? rvaRef(genInstanceDescriptor()),
      "singletonObject"    -> t.isSingletonObject ? rvaRef(allocateSingletonObject()),
    )
  }

  private def createInfectedTypeHandle(base: MetaInfo)(t: pcO.Class): MetaInfo = {
    struct(base >> InfectedTypeHandle)(
      "name" -> rel32(outStr(nms.getClassName(t, CL_slash))),

      "sourceFile"  -> at.genStackTrace ? stringRef(t.getBCSourceName),
      "typesTable"  -> rva32(std.dataSymbol(RTSGlobal.LINK_LocalTypesTable)),

      "methodCodeTable" -> rva32(genMethodCodeTable(t)),

      "serialTypeInfo" -> rva32(DataGen.genSerialTypeInfo(t)),

      "methodIDs" -> (t.declaredMethods.nonEmpty && (O2Env.env.enabled(BoolOption.GenMethodIDs) || at.genStackTrace)) ?
        rva32(GenMethodIDs(t.declaredMethods.toSeq)),
    )
  }

  private def createAJArrayTypeHandle(base: MetaInfo)(t: pcO.Class): MetaInfo = {
    struct(base >> AJArrayTypeHandle)(
      "name" -> rel32(outStr(nms.getClassName(t, CL_slash))),

      "serialTypeInfo" -> rva32(DataGen.genSerialTypeInfo(t)),
      "instanceDescriptor" -> rvaRef(allocateInstanceDescriptor(t, RTConst.AJArrayInstanceDescriptor.size)),

      "methodIDs" -> (t.declaredMethods.nonEmpty && (O2Env.env.enabled(BoolOption.GenMethodIDs) || at.genStackTrace)) ?
        rva32(GenMethodIDs(t.declaredMethods.toSeq)),
    )
  }

  private def createCangjieArrayTypeHandle(base: MetaInfo)(t: pcO.Class): MetaInfo = {
    struct(base >> CangjieArrayTypeHandle)(
      "name" -> rel32(outStr(nms.getClassName(t, CL_slash))),

      "instanceDescriptor" -> rvaRef(allocateInstanceDescriptor(t, RTConst.CangjieInstanceDescriptor.size)),

      "serialTypeInfo" -> rva32(DataGen.genSerialTypeInfo(t)),

      "methodIDs" -> (t.declaredMethods.nonEmpty && (O2Env.env.enabled(BoolOption.GenMethodIDs) || at.genStackTrace)) ?
        rva32(GenMethodIDs(t.declaredMethods.toSeq)),
    )
  }

  /** Generate ThinTypeHandle and returns a pair of Segment and MetaInfo (HeaderThinTypeHandle, ThinTypeHandle) */
  private def createThinDescriptor(t: pcO.Class): Unit = {
    inline def genThinVMT(): Unit = {
      assert(t.isPolyThinClass && !t.isAbstract)
      for (m <- MethodTablesImpl.getVMTForThinType(t.asCT)) {
        assert(!m.isAbstract)
        fixup(m)
      }
    }

    var level = 0
    val headerThinTypeHandle = withSeg(newSeg()) {
      var nextBase = t
      while (nextBase != null && !CacheAPI.isThisClass(nextBase, ClassID.PolyThinType)) {
        fixup(nextBase.thinTypeInfo.thinTypeHandle)
        nextBase = nextBase.getSuperClassO2
        level += 1
      }
    }

    assert(level == t.getThinInheritanceLevel)
    val thinTypeHandle = struct(MetaInfoType.ThinTypeHandle)(
      "magic" -> genInt(RTConst.ThinTypeHandle.MAGIC.intValue),
      "level" -> genInt(level),

      "instanceSize" -> genInt(t.size),
      "padding"      -> genInt(0),
      "typeName"     -> fixup(outStr(nms.getClassName(t, CL_slash))),

      "vmt" -> (!t.isAbstract) ? genThinVMT(),
    )

    // TODO-THIN: remove LINK_LocalTypesTable reference from Thin TD (currently required by linker)
    cd.withSeg(thinTypeHandle.getSegment) {
      fixup(std.dataSymbol(RTSGlobal.LINK_LocalTypesTable))
    }

    at.setSegment(t.thinTypeInfo.headerTypeHandle, headerThinTypeHandle)
    thinTypeHandle.attachObject(t.thinTypeInfo.thinTypeHandle).gatherSegments()
  }

  private def getTDTag(t: pcO.Class): Int = {
    if (t.isInfectedAJClass) {
      RTConst.TDTag.INFECTED.intValue
    } else if (t.isAJArray) {
      RTConst.TDTag.AJ_ARRAY.intValue
    } else if (t.isCangjieArray) {
      RTConst.TDTag.CANGJIE_ARRAY.intValue
    } else if (t.isRecord || t.isCangjiePackage) {
      RTConst.TDTag.RECORD.intValue
    } else if (t.isInterface) {
      RTConst.TDTag.INTERF.intValue
    } else {
      RTConst.TDTag.CLASS.intValue
    }
  }

  private def createTypeHandle(t: pcO.Class): MetaInfo = {
    structDraft(TypeHandle)(
      "tag"   -> genShort(getTDTag(t)),
      "flags" -> (t.isInfectedAJClass || t.isAJManagedType) ? genShort(RTConst.TypeHandle.Flags.LIGHT.intValue),
    )
  }

  private def createJavaReflection(t: pcO.Class): Unit = {

    def genInnerClassInfo(): pc.Symbol = {
      // TODO: JET-7463
      var sg: Segment = null

      // collect outer & nested classes
      var outerClass = t.outerClass

      var nestedCls: pc.Symbol = null
      var nestedNum = 0

      val innerClasses = t.getInnerClasses
      if (innerClasses != null) { // ArrayOfFlat<EnclosedClassInfo> {
        for (ic <- innerClasses if ic.getClass0.hasManagedMetaInformation) { // TODO: JET-7463
          if (nestedCls == null) { // struct EnclosedClassInfo {
            nestedCls = addObjectToCurrentMetaInfo()
            sg = cd.newSeg()
          }
          cd.withSeg(sg) {
            tdindex(ic.getClass0) // TDIndex      clazz;
            genSet16(ic.getAccessFlags) // short        accFlags;
            nestedNum += 1 // } /* EnclosedClassInfo */
          }
        }

        if (nestedCls != null) {
          at.setSegment(nestedCls, sg) // } /* ArrayOfFlat<EnclosedClassInfo> */
        }
      }

      val em = t.getEnclosingMethod
      val outerMethod: pc.Symbol = if (em != null) {
        subStruct(EnclosingMethodInfo)(
          "enclosingClass" -> tdindex(em.enclosingClass),
          "methodName" -> stringRef(em.methodName),
          "methodSig" -> stringRef(em.methodSig)
        ).attachObject(addObjectToCurrentMetaInfo()).getAttachedObject
      } else {
        null
      }

      if (outerClass != null && !outerClass.hasManagedMetaInformation) {
        outerClass = null
      }

      if (outerClass != null || nestedCls != null || outerMethod != null) {
        subStruct(InnerClassInfo)(
          "declaredIn" -> tdindex(outerClass),
          "enclosingMeth" -> rel16(outerMethod),
          "enclosedClassesNum" -> genUInt16(nestedNum),
          "enclosedClasses" -> rel16(nestedCls),
        ).attachObject(addObjectToCurrentMetaInfo()).getAttachedObject
      } else {
        null
      }
    }

    def genAnnotations(): pc.Symbol = {
      if (!t.hasAnnotations) { // TODO: JET-7463
        return null
      }

      objBySegm(subStruct(AnnotationsInfo)(
        "runtimeVisAnnotData" -> rel16(GenRuntimeAnnot.generate(t, rtVisible = true)),
        "runtimeInvisAnnotData" -> rel16(GenRuntimeAnnot.generate(t, rtVisible = false)),

        "runtimeVisTypeAnnotData" -> rel16(GenRuntimeTypeAnnot.generate(t, rtVisible = true)),
        "runtimeInvisTypeAnnotData" -> rel16(GenRuntimeTypeAnnot.generate(t, rtVisible = false)),

        "rtVisParamsAnnotData" -> rel16(GenParamsAnnot.generate(t, rtVisible = true)),
        "rtInvisParamsAnnotData" -> rel16(GenParamsAnnot.generate(t, rtVisible = false)),
        "annotDefaultData" -> rel16(GenAnnotDefault.generate(t))
      ).getSegment
      )
    }

    def genVerificationInfo(): pc.Symbol = {
      var vp = pcO.getVerificationPairs(t)
      if (t.isVerifiable && vp == null) {
        return null
      }

      at.setSegment(addObjectToCurrentMetaInfo(), cd.makeSeg {
        if (vp != null) {
          assert(t.needVerify)

          while (vp != null) {
            tdindex(vp.from) // from   :X2C_TDINDEX;
            tdindex(vp.to0) // to     :X2C_TDINDEX;
            stringRef(vp.errmsg) // errMsg :StringRef;
            vp = vp.next
          }
          // end-of-list mark (record with zeroed fields)
          tdindex(null) // from   :X2C_TDINDEX;
          tdindex(null) // to     :X2C_TDINDEX;
          stringRef(null) // errMsg :StringRef;
        } else {
          val ve = t.getVerifyError
          stringRef(ve.errmsg) // StringRef      errMsg;    // Value<int>
          genUInt16(ve.getRTCode.toInt) // ExceptionCode  errCode;   // Value<short>
        }
      })
    }

    def putMethodRawVirtNums(methods: Seq[pc.Symbol]): Unit = {
      def getRawVirtNum(m: pcO.Method): Int = MethodTablesImpl.getVNum(m) + 1

      assert(methods.nonEmpty)
      val rvnMax = methods.map(m => getRawVirtNum(m.asInstanceOf[pcO.Method])).maxOption.getOrElse(0)

      val need32 = rvnMax > UShort.MaxValue
      if (isWorkMode) {
        if (need32) {
          env.info.print(s"\\nJavadesc.putRawVirtNums: rvnmax=$rvnMax\\n")
        }
      }

      val seg = cd.makeSeg {
        for (i <- methods.indices) {
          val m = methods(i).asInstanceOf[pcO.Method]
          val rvn = getRawVirtNum(m)
          if (need32) {
            genInt(rvn)
          } else {
            genUInt16(rvn)
          }
        }
        assert((cd.getCodeLen & 1) == 0)
      }
      rel16(objBySegm(seg), (if (need32) 1 else 0).toShort.toInt) // methodRawVirtNums
    }

    def getFields: Seq[pc.Symbol] = {
      val result = mutable.ArrayBuffer.empty[pc.Symbol]
      for (f <- t.declaredFields) {
        assert(!pcO.isStringTable(f))
        result += f
      }

      if (t.isUnloadable) {
        assert(result.isEmpty)
      } else {
        Numerate.checkFieldOrder(t)
      }
      result.toSeq
    }

    def hasThrows(methods: Seq[pc.Symbol]): Boolean = methods.exists(_.asInstanceOf[pcO.Method].getThrowsCount > 0)

    def hasParametersAttrs(methods: Seq[pc.Symbol]): Boolean =
      methods.exists(_.asInstanceOf[pcO.Method].getParameters != null)

    // all refl.array objects have expandable16 mark
    // after preparation expanded16to32 mark is also set on arrays with 32-bit offsets
    def prepareReflArrays(reflectMetaInfo: MetaInfo): Unit = {
      def transformTdrelArray(obj: Expandable16BOBJECT): Unit = {
        // pick out i-th fixup from tdrel16[]
        def tdrel16Target(sg: Segment, i: Int, fxi: Int): assembler.Symbol = {
          assert(sg.getW16(i * 2) == 0.toShort)

          if (fxi >= sg.fixups.length) { // no fixup at i-th pos
            return null
          }
          val fx = sg.fixups(fxi)
          if (fx.position > i * 2) { // no fixup at i-th pos
            return null
          }

          assert(fx.position == i * 2)
          assert(fx.kind == TD_REL_16)
          assert(fx.addend == 0)
          assert(fx.target != null)
          fx.target
        }

        // already transformed; can occur due to equal segments merging /see obj_by_segm()/
        if (obj.expandedTo32) {
          return
        }

        val old = at.getSegment(obj)
        val n = O2JSupport.div(old.length, 2)
        assert(n * 2 == old.length)

        val sg = cd.withSeg(cd.newSeg()) {
          var fxi = 0
          for (i <- 0 until n) {
            val obj2 = tdrel16Target(old, i, fxi)
            rel32(obj2)
            if (obj2 != null) {
              fxi += 1
            }
          }
        }

        at.setSegment(obj, sg)
        obj.expandedTo32 = true
      }

      val reflectObjects = reflectMetaInfo.getObjects
      val sz = Numerate.mkAlign(reflectMetaInfo.getSegment.length, 4) + sumBy(reflectObjects)(at.getSegment(_).length)
      if (sz > Short.MaxValue) {
        for (o <- reflectObjects if isExpandable16(o)) {
          // reflection does not fit in 32K: let's change td16fxup[] to td32fxup[]
          transformTdrelArray(o.asInstanceOf[Expandable16BOBJECT])
        }
      }
    }

    if (classByO2Object(t).isJavaReference) {
      val refl = at.findSpecialObject(TDReflection)

      val methods: Seq[pcO.Method] = t.symType.getGeneratedMethods.map(getO2Method).toSeq
      val interfaces = getDirectSuperInterfaces(t)
      val fields = getFields
      val shouldGenerateMetaInfo = !shouldNotGenerateMetaInfo(t)

      val metaInfo = struct(MetaInfoType.MetaInfo)(
        "directSuperInterfsNum" -> genShort(interfaces.length),
        "directSuperInterfs" -> interfaces.nonEmpty ? rel16(GenTDs(interfaces)),

        when(shouldGenerateMetaInfo)(
          "codeSource" -> stringRef(t.getCodeSource),
          "sourceFile" -> at.genStackTrace ? stringRef(t.getBCSourceName),
          "innerClassInfo" -> rel16(genInnerClassInfo()),

          "fieldNum" -> genUInt16(fields.length),

          when(fields.nonEmpty)(
            "fieldOffs" -> GenFieldOffs.generate(fields),
            "fieldNames" -> rel16(GenBStrNames(fields)),
            "fieldModifiers" -> rel16(GenFieldMods(fields)),
            "fieldTypes" -> rel16(GenFieldTypes(fields)),
          ),

          "methodNum" -> genUInt16(methods.length),

          when(methods.nonEmpty)(
            "methodNames" -> rel16(GenBStrNames(methods)),
            "methodRawVirtNums" -> putMethodRawVirtNums(methods),
            "methodStackParamSizes" -> rel16(GenFrameSz(methods)),
            "methodModifiers" -> rel16(GenMethodMods(methods)),
            "methodParamNums" -> rel16(GenMethodParNums(methods)),
            "methodParamTypes" -> rel16(GenParTDs(methods)),
            "methodRetTypes" -> rel16(GenRetTDs(methods)),

            "methodThrowsNums" -> hasThrows(methods) ? rel16(GenThrowsNum(methods)),
            "methodThrowsTypes" -> hasThrows(methods) ? rel16(GenThrows(methods)),

            "methodParameters" -> hasParametersAttrs(methods) ? rel16(GenParameters(methods)),
          ),

          "genericSignatures" -> t.hasGenericsInfo ? rel16(GenGenericSig.generate(t)),
          "annotations" -> rel16(genAnnotations()),

          "majorCFVersion" -> genShort(t.getMajorClassFileVersion),
          "minorCFVersion" -> genShort(t.getMinorClassFileVersion),

          "verificationInfo" -> rel16(genVerificationInfo()),
        )
      )

      val bigOffsets = ArrayBuffer.empty[Int]
      prepareReflArrays(metaInfo)
      metaInfo.attachObject(refl).gatherSegments(bigOffsets)

      pcO.setPlainArrayLength(refl, metaInfo.getSegment.length)

      alignRawObject(refl, addressSize)

      def makeBigOffsets(): Segment = {
        if (bigOffsets.isEmpty) {
          return null
        }
        if (isWorkMode) {
          env.info.print("\\nJavadesc.MakeBigOffsets %d\\n", bigOffsets.size)
        }

        cd.makeSeg {
          for (i <- bigOffsets.size - 1 to 0 by -1) {
            cd.genLWord(bigOffsets(i))
          }
          bigOffsets.clearAndShrink(0): Unit
        }
      }

      val sg = makeBigOffsets()
      if (sg != null) {
        val Rneg = at.createSpecialObject(at.TDReflectionNegative)
        at.setSegment(Rneg, sg)
      }
    }
  }

  def genRunTimeTypeInfo(t: pcO.Class): Unit = {
    assert(t eq at.currClass) // for the current module

    // TODO: do not create this object if current type is not java reference
    at.createSpecialObject(at.TDReflection)

    if (t.hasManagedMetaInformation) {
      val handle: MetaInfo = createTypeHandle(t)

      if (t.isInfectedAJClass) {
        createInfectedTypeHandle(handle)(t)
      } else if (t.isAJArray) {
        createAJArrayTypeHandle(handle)(t)
      } else if (t.isCangjieArray) {
        createCangjieArrayTypeHandle(handle)(t)
      } else {
        createClassOrInterfTypeHandle(handle)(t)
      }
      handle.attachObject(t.typeHandle).gatherSegments()
    }

    if (t.hasThinTD) {
      createThinDescriptor(t)
    }

    createJavaReflection(t)
  }

  def genPackageDescr(cls: pcO.Class): Segment = {
    var pos = RTConst.JavaPackageDesc.size

    // TODO: refactor this
    def putStrPos(s: XString): Unit = {
      if (s != null) {
        genAddrInt(pos)
        pos += s.length + 1
      } else {
        genAddrInt(0)
      }
    }

    def putNullableStr(s: XString): Unit = {
      if (s != null) {
        cd.genBstr(s)
      }
    }

    val pname = nms.getPackageName(cls)
    assert(pname != null)

    val system = cls.isSystemClass
    val pInfo = if (!system) GetPackageSupport.getPackageInfo(cls) else null
    val jar = if (pInfo != null) pInfo.jar else null

    assert(pInfo == null || pname.equals(pInfo.name))

    struct(JavaPackageDesc)(
      "name" -> putStrPos(pname),
      "jar"  -> putStrPos(jar),

      when(pInfo != null)(
        "specTitle"   -> putStrPos(pInfo.specTitle),
        "specVersion" -> putStrPos(pInfo.specVersion),
        "specVendor"  -> putStrPos(pInfo.specVendor),
        "implTitle"   -> putStrPos(pInfo.implTitle),
        "implVersion" -> putStrPos(pInfo.implVersion),
        "implVendor"  -> putStrPos(pInfo.implVendor),
      ),

      "jarRelToJRE" -> system ? cd.genByte(1),
      "sealed"      -> (pInfo != null && pInfo.sealed0) ? cd.genByte(1),

      "hash"        -> cd.genLWord(pname.hashCode),

      "classLoaders" -> {
        for (_ <- 0 until RTConst.JavaPackageDesc.NUM_OF_PKG_CLASSLOADERS.intValue) {
          genNull()
        }
      },

      // This field contains all strings that used in object.
      // In runtime, we will resolve fixups to these strings.
      "strImpl" -> EntryWithValue.Action(() => {
        putNullableStr(pname)
        putNullableStr(jar)

        if (pInfo != null) {
          putNullableStr(pInfo.specTitle)
          putNullableStr(pInfo.specVersion)
          putNullableStr(pInfo.specVendor)
          putNullableStr(pInfo.implTitle)
          putNullableStr(pInfo.implVersion)
          putNullableStr(pInfo.implVendor)
        }
      })
    ).getSegment
  }

  def genDataStaticFields(f: pcO.StaticField): Unit = {
    import com.huawei.excelsior.common.DataAnnotationParsing.*
    f.getDataAnnotData.foreach {
      case Integer(w, v) => w match {
        case 1 => cd.genByte(v.toInt & 0xFF)
        case 2 => cd.genWord(v.toShort)
        case 4 => cd.genLWord(v.toInt)
        case 8 => cd.genQWord(v)
        case x => shouldNotReachHere(s"unexpected static field size: $x")
      }
      case FieldRef(ref) =>
        var (className, fieldName) = ref.splitAt(ref.lastIndexOf('.'))
        fieldName = fieldName.substring(1)
        val klass = pcO.findClass(XString(className.replace('.', '/')))
        assert(klass != null, f"Cannot find class \"$className\" during data field initialization")
        val field = klass.findField(XString(fieldName), null)
        assert(field != null, f"Cannot find field \"$fieldName\" in class \"$className\" during data field initialization")
        fixup(field)
    }
    assert(cd.getSeg.length == f.size, s"Incorrect @Data annotation: size of result segment = ${cd.getSeg.length} are not equals to size of the field = ${f.size}")
  }
}
