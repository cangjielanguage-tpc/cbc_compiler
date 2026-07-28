/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe_jbc

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.langjava.MethodInfo
import com.huawei.excelsior.jet.compiler.o2lib.fe.pc
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JavaClassParserModule.PtrBootstrapMethod
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JavaIdentifierModule as ji
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule
import com.huawei.excelsior.o2j.runtime.*
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.util.{MathUtils, Set32, UByte, UInt, UShort}
import xscala.util.MathUtils.{high32Bits, low32Bits}

import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object JavaClassParserModule {

  class ConstantInfo extends Object {
    var constantType: Byte = TagInvalid.toByte
    var index: UShort = _
    var indexName: UShort = _
    var realVal: Float = _
    var longRealVal: Double = _
    var low: Int = _
    var high: Int = _
    var bufferPtr: XString = _
    var resolvedType: pc.SymType.Reference = _ // Resolved type for TagClass constant (class, interface or array)

    override def hashCode: Int = {
      constantType match {
        case TagUtf8 =>
          bufferPtr.hashCode()
        case TagInteger |
             TagLong =>
          low | high
        case TagFloat =>
          floatToRawIntBits(realVal)
        case TagDouble =>
          val bits = doubleToRawLongBits(longRealVal)
          val low = low32Bits(bits)
          val high = high32Bits(bits)
          high * 31 + low
        case TagClass =>
          indexName.toInt
        case TagString |
             TagMethodType |
             TagAOTClassRef =>
          index.toInt
        case TagField |
             TagMethod |
             TagIMethod |
             TagNameAndType |
             TagInvokeDynamic |
             TagSigpolyMethod =>
          (index | indexName).toInt
        case TagMethodHandle =>
          index.toInt | low
      }
    }

    override def equals(that: Any): Boolean = that match {
      case that: ConstantInfo =>
        this.constantType match {
          case x if x != that.constantType =>
            false
          case TagUtf8 =>
            this.bufferPtr.equals(that.bufferPtr)
          case TagInteger |
               TagLong =>
            this.low == that.low && this.high == that.high
          case TagFloat =>
            floatToRawIntBits(this.realVal) == floatToRawIntBits(that.realVal)
          case TagDouble =>
            doubleToRawLongBits(this.longRealVal) == doubleToRawLongBits(that.longRealVal)
          case TagString |
               TagAOTClassRef =>
            this.index == that.index
          case TagClass =>
            this.indexName == that.indexName && this.high == that.high
          case TagMethodType =>
            this.index == that.index && this.high == that.high
          case TagField |
               TagMethod |
               TagIMethod |
               TagNameAndType |
               TagInvokeDynamic =>
            this.index == that.index && this.indexName == that.indexName
          case TagSigpolyMethod =>
            this.index == that.index && this.indexName == that.indexName && this.high == that.high
          case TagMethodHandle =>
            this.index == that.index && this.low == that.low && this.high == that.high
        }
      case _ =>
        false
    }
  }

  class LineNumber {
    var startPC: UShort = _
    var lineNumber: UShort = _
  }

  class LocalVariable {
    var startPC: UShort = _
    var length: UShort = _
    var nameIndex: UShort = _
    var signatureIndex: UShort = _
    var slot: UShort = _
  }

  class InnerClass {
    var innerClassInfoIndex: UShort = _
    var outerClassInfoIndex: UShort = _
    var innerNameIndex: UShort = _
    var innerClassAccessFlags: Set32 = _
  }

  /** Corresponds to element_value of ClassFormat spec */
  abstract class PtrElementValue extends Object {
    var tag: Char = _ // 'B', 'C', 'D', 'F', 'I', 'J', 'S', and 'Z' indicate a primitive type (ConstElementValue).
                      // 's' -- StringElementValue
                      // 'e' -- EnumElementValue
                      // 'c' -- ClassElementValue
                      // '@' -- AnnotationElementValue
                      // '[' -- ArrayElementValue
                      // 'p' -- placeholder slot for second part of 'J' or 'D'
  }

  /** Permitted tag: 'B', 'C', 'D', 'F', 'I', 'J', 'S', 'Z' ans 's' */
  class PtrConstElementValue extends PtrElementValue

  /** Permitted tag: 'B', 'C', 'I',  'S', 'Z' */
  case class PtrIntElementValue(value: Int) extends PtrConstElementValue

  /** Permitted tag: 'J' */
  case class PtrLongElementValue(low: Int, high: Int) extends PtrConstElementValue

  /** Permitted tag: 'F' */
  class PtrFloatElementValue(val value: Float) extends PtrConstElementValue {
    override def hashCode: Int = floatToRawIntBits(this.value)

    override def equals(that: Any): Boolean = that match {
      case that: PtrFloatElementValue => floatToRawIntBits(this.value) == floatToRawIntBits(that.value)
      case _ => false
    }
  }

  /** Permitted tag: 'D' */
  class PtrDoubleElementValue(val value: Double) extends PtrConstElementValue {
    override def hashCode: Int = {
      val bits = doubleToRawLongBits(this.value)
      high32Bits(bits) * 31 + low32Bits(bits)
    }

    override def equals(that: Any): Boolean = that match {
      case that: PtrDoubleElementValue => doubleToRawLongBits(this.value) == doubleToRawLongBits(that.value)
      case _ => false
    }
  }

  /** Permitted tag: 's' */
  case class PtrStringElementValue(value: XString) extends PtrConstElementValue

  /** Permitted tag: 'e'
    * `typeName` represents the binary name (JLS 13.1) of the type of the enum constant.
    * `constName` represents the simple name of the enum constant.
    */
  case class PtrEnumElementValue(typeName: XString, constName: XString) extends PtrElementValue

  /** Permitted tag: 'c'
    * `classInfo` represents the return descriptor (4.4.3) of the type that is reified by the class.
    */
  case class PtrClassElementValue(classInfo: XString) extends PtrElementValue

  /** Permitted tag: '@'
    * `value` represents a "nested" annotation.
    */
  case class PtrAnnotationElementValue(value: PtrAnnotation) extends PtrElementValue

  /** Permitted tag: '['
    * Each element of the `value` table gives the value of an element of the array-typed value.
    */
  case class PtrArrayElementValue(value: Array[PtrElementValue]) extends PtrElementValue

  /** Represents a single element-value pair in the annotation represented. */
  class AnnotationPair {
    var name: XString = _
    var value: PtrElementValue = _
  }

  /** Each value of the PtrAnnotationPairArray table represents a single element-value pair in  the annotation represented. */
  class PtrAnnotation {
    var type0: XString = _
    var pairs: Array[AnnotationPair] = _ // annotations pairs
  }

  class PtrTargetInfo

  case class PtrOneByteTargetInfo(index: Byte) extends PtrTargetInfo

  case class PtrTwoBytesTargetInfo(index1: Byte, index2: Byte) extends PtrTargetInfo

  case class PtrWordTargetInfo(index: UShort) extends PtrTargetInfo

  class PtrTypeAnnotation {
    var targetType: Byte = _
    var targetInfo: PtrTargetInfo = _
    var pathLength: Byte = _
    var path: Array[Byte] = _
    var annotation: PtrAnnotation = _
  }

  class PtrAbstractAnnotationAttr {
    var isMalformed: Boolean = _
  }

  class PtrAnnotationsAttr extends PtrAbstractAnnotationAttr {
    var annotations: Array[PtrAnnotation] = _
  }

  class PtrTypeAnnotationsAttr extends PtrAbstractAnnotationAttr {
    var typeAnnotations: Array[PtrTypeAnnotation] = _
  }

  class PtrParameterAnnotationsAttr extends PtrAbstractAnnotationAttr {
    var annotations: Array[Array[PtrAnnotation]] = _
  }

  class PtrAnnotationDefaultAttr extends PtrAbstractAnnotationAttr {
    var defaultValue: PtrElementValue = _
  }

  case class PtrBootstrapMethod(methodIndex: UShort, args: Array[UShort])

  class MethodParameter {
    var name: XString = _
    var accessFlags: UShort = _
  }

  class AttributeInfo {
    var nameIndex: UShort = _
    var length: UInt = _
    var code: PtrCodeInfo = _
    var localVariableTable: Array[LocalVariable] = _
    var lineNumberTable: Array[LineNumber] = _
    var exceptionIndexTable: Array[UShort] = _
    var innerClasses: Array[InnerClass] = _
    var annotation: PtrAbstractAnnotationAttr = _
    var bootstrapMethods: Array[PtrBootstrapMethod] = _
    var methodParameters: Array[MethodParameter] = _
    var index: UShort = _
    var index2: UShort = _
    var info: Array[Byte] = _
    var unknown: Boolean = _
  }

  class FieldInfo {
    var accessFlag: Set32 = _
    var nameIndex: UShort = _
    var signatureIndex: UShort = _
    var attributeCount: UShort = _
    var attribute: Array[AttributeInfo] = _
  }

  class MethodInfo {
    var accessFlag: Set32 = _
    var nameIndex: UShort = _
    var signatureIndex: UShort = _
    var attributeCount: UShort = _
    var attribute: Array[AttributeInfo] = _
  }

  class PtrClassInfo {
    var magic: UInt = _
    var versionMinor: UShort = _
    var versionMajor: UShort = _
    var accessFlag: Set32 = _
    var thisClass: UShort = _
    var superClass: UShort = _
    var interfaceCount: UShort = _
    var interface: Array[UShort] = _
    var attributeCount: UShort = _
    var attribute: Array[AttributeInfo] = _
    var bytecodeSize: Int = _

    var constantPool: Array[ConstantInfo] = _
    var constantPoolCount: UShort = _
    def constants: Iterator[ConstantInfo] = constantPool.iterator

    private var _methods: Array[MethodInfo] = _
    def initMethods(count: Int): Unit = _methods = Array.fill(count)(new MethodInfo())
    def method(index: Int) = _methods(index)
    def methods: Iterator[MethodInfo] = if (_methods == null) Iterator.empty[MethodInfo] else _methods.iterator

    private var _fields: Array[FieldInfo] = _
    def initFields(count: Int): Unit = _fields = Array.fill(count)(new FieldInfo())
    def fields: Iterator[FieldInfo] = if (_fields == null) Iterator.empty[FieldInfo] else _fields.iterator
    def fieldsCount: Int = _fields.length
  }

  class ExcepInfo {
    var startPC: UShort = _
    var endPC: UShort = _
    var handlerPC: UShort = _
    var catchType: UShort = _
  }

  class PtrCodeInfo {
    var stackSize: UShort = _
    var localSize: UShort = _
    var codeLength: Int = _
    var codePtr: Array[Byte] = _
    var excepTableLength: UShort = _
    var excepTable: Array[ExcepInfo] = _
    var attributeCount: UShort = _
    var attribute: Array[AttributeInfo] = _
  }

  private case class NameAndSig(name: XString, sig: XString)

  // during class parsing, array type with dim>255 was found
  /* Forward procedure declaration */
  type AttrType = UByte
  private val classAttrType: AttrType = UByte(0)
  private val methodAttrType: AttrType = UByte(1)
  private val fieldAttrType: AttrType = UByte(2)
  private val codeAttrType: AttrType = UByte(3)


  class SignatureIterator {

    private[JavaClassParserModule] var sig: XString = _
    private[JavaClassParserModule] var curPos: Int = _
    private[JavaClassParserModule] var endPos: Int = _
    private[JavaClassParserModule] var lastElemPos: Int = _
    private[JavaClassParserModule] var dim: Int = _
    private[JavaClassParserModule] var badSignature: Boolean = _

    def getLastElemEndPos: Int = {
      assert(!this.badSignature)
      this.curPos
    }

    def getLastElemPos(array: Boolean = false): Int = {
      assert(!this.badSignature)
      assert(this.lastElemPos >= 0)
      if (array) {
        assert(this.dim > 0)
        assert(this.lastElemPos >= this.dim)
        this.lastElemPos - this.dim
      } else {
        this.lastElemPos
      }
    }

    def getCurSigChar: Char = {
      assert(!this.badSignature)
      assert(this.hasNext)
      this.sig.charAtAsChar(this.curPos)
    }

    def isArray: Boolean = {
      assert(!this.badSignature)
      assert(this.dim >= 0)
      this.dim != 0
    }

    def getSig: XString = this.sig

    def next(): Boolean = {
      assert(!this.badSignature)
      assert(this.hasNext)
      this.dim = 0
      var ch = this.sig.charAt(this.curPos)
      while (ch == '[') {
        this.curPos += 1
        if (this.curPos >= this.endPos) {
          this.badSignature = true
          return false
        }
        this.dim += 1
        ch = this.sig.charAt(this.curPos)
      }
      this.lastElemPos = this.curPos
      ch match {
        case 'B' |
             'C' |
             'F' |
             'D' |
             'I' |
             'J' |
             'S' |
             'Z' |
             'V' =>
        case 'L' =>
          // do nothing
          val newpos = this.sig.indexOf(';', this.curPos)
          assert(newpos != 0)
          if (newpos < 0 || newpos >= this.endPos) {
            this.badSignature = true
            return false
          } else {
            this.curPos = newpos
          }
        case _ =>
          this.badSignature = true
          return false
      }
      this.curPos += 1
      true
    }

    def hasNext: Boolean = {
      assert(!this.badSignature)
      this.curPos < this.endPos
    }

    def init(sig: XString): Unit = {
      this.initEx(sig, 0, sig.length)
    }

    def initEx(sig: XString, startPos: Int, endPos: Int): Unit = {
      assert(0 <= startPos && startPos <= endPos && endPos <= sig.length)
      this.sig = sig
      this.curPos = startPos
      this.endPos = endPos
      this.dim = -1
      this.lastElemPos = -1
      this.badSignature = false
    }

  }

  //---------------------------------------------------------------------------

  private class AttributeData {

    private[JavaClassParserModule] var attributePtr: Array[AttributeInfo] = _
    private[JavaClassParserModule] var hasSynthetic: Boolean = _

  }

  /* Decoding of java executable files
  */
  //  Magic*              = 0CAFEBABEH;
  private val MinSupportedVersion: Int = 45
  val MaxSupportedVersion: Int = 52
  private val MaxSupportedMinorVersion: Int = 0
  val Java5BytecodeVersion: Int = 49
  val Java6BytecodeVersion: Int = 50
  val Java7BytecodeVersion: Int = 51
  val Java8BytecodeVersion: Int = 52
  /* Tag of constants */
  val TagInvalid: Int = 0
  val TagUtf8: Int = 1
  //  TagUnicode*         = 2;
  val TagInteger: Int = 3
  val TagFloat: Int = 4
  val TagLong: Int = 5
  val TagDouble: Int = 6
  val TagClass: Int = 7
  val TagString: Int = 8
  val TagField: Int = 9
  val TagMethod: Int = 10
  val TagIMethod: Int = 11 // interface method
  val TagNameAndType: Int = 12
  val TagMethodHandle: Int = 15
  val TagMethodType: Int = 16
  val TagInvokeDynamic: Int = 18
  val TagSigpolyMethod: Int = 26
      // AOT CP entries tags
  val TagAOTClassRef: Int = 30
                /* Access flags */
  val AccPublic: Int = 0
  val AccPrivate: Int = 1
  val AccProtected: Int = 2
  val AccStatic: Int = 3
  val AccFinal: Int = 4
  val AccSynchronized: Int = 5
  val AccVolatile: Int = 6
  val AccTransient: Int = 7
  val AccNative: Int = 8
  val AccInterface: Int = 9
  val AccAbstract: Int = 10
  val AccStrict: Int = 11
  val AccSuper: Int = 5
  val AccSynthetic: Int = 12
  val AccAnnotation: Int = 13
  val AccEnum: Int = 14
  val AccBridge: Int = 6
  val AccVarargs: Int = 7
  /* Method handle reference kinds (bytecode behavior) */
  val REF_getField: Int = 1
  val REF_getStatic: Int = 2
  val REF_putField: Int = 3
  val REF_putStatic: Int = 4
  val REF_invokeVirtual: Int = 5
  val REF_invokeStatic: Int = 6
  val REF_invokeSpecial: Int = 7
  val REF_newInvokeSpecial: Int = 8
  val REF_invokeInterface: Int = 9
  private val MH_REF_FIRST: Int = REF_getField
  private val MH_REF_LAST: Int = REF_invokeInterface

  val jstrCodeName = js.internJString("Code")
  val jstrLocVarName = js.internJString("LocalVariableTable")
  val jstrCValue = js.internJString("ConstantValue")
  val jstrSourceFile = js.internJString("SourceFile")
  val jstrLineNumber = js.internJString("LineNumberTable")
  val jstrException = js.internJString("Exceptions")
  val jstrInnerClasses = js.internJString("InnerClasses")
  val jstrDeprecated = js.internJString("Deprecated")
  val jstrSynthetic = js.internJString("Synthetic")

  // since 1.5
  val jstrSignature = js.internJString("Signature")
  val jstrSourceDebugExtension = js.internJString("SourceDebugExtension")
  val jstrLocalVariableTypeTable = js.internJString("LocalVariableTypeTable")
  val jstrRuntimeVisibleAnnotations = js.internJString("RuntimeVisibleAnnotations")
  val jstrRuntimeInvisibleAnnotations = js.internJString("RuntimeInvisibleAnnotations")
  val jstrRuntimeVisibleParameterAnnotations = js.internJString("RuntimeVisibleParameterAnnotations")
  val jstrRuntimeInvisibleParameterAnnotations = js.internJString("RuntimeInvisibleParameterAnnotations")
  val jstrAnnotationDefault = js.internJString("AnnotationDefault")
  val jstrEnclosingMethod = js.internJString("EnclosingMethod")

  // since 1.6
  val jstrStackMapTable = js.internJString("StackMapTable")

  // since 7.0
  val jstrBootstrapMethods = js.internJString("BootstrapMethods")

  // since 8.0
  val jstrRuntimeVisibleTypeAnnotations = js.internJString("RuntimeVisibleTypeAnnotations")
  val jstrRuntimeInvisibleTypeAnnotations = js.internJString("RuntimeInvisibleTypeAnnotations")
  val jstrMethodParameters = js.internJString("MethodParameters")

  // AJ specific
  val jstrAjExternal = js.internJString("Lcom/huawei/excelsior/aj/lang/External;")
  val jstrAjExternalName = js.internJString("name")

  val jstrAjExport = js.internJString("Lcom/huawei/excelsior/aj/lang/Export;")
  val jstrAjExportId = js.internJString("id")

  val jstrAjData = js.internJString("Lcom/huawei/excelsior/aj/lang/Data;")
  val jstrAjDataData = js.internJString("data")

  val jstrAjCallConv = js.internJString("Lcom/huawei/excelsior/aj/lang/CallConv;")
  val jstrAjCallConvValue = js.internJString("value")

  val jstrAjEnvironment = js.internJString("Lcom/huawei/excelsior/aj/lang/Environment;")
  val jstrAjEnvironmentValue = js.internJString("value")

  val jstrAjCallTypeStdCall = js.internJString("STDCALL")
  val jstrAjCallTypeC = js.internJString("C")
  val jstrAjCallTypeVMCall = js.internJString("VMCALL")
  val jstrAjCallTypeManaged = js.internJString("MANAGED")
  val jstrAjCallTypeUnmanaged = js.internJString("UNMANAGED")
  val jstrAjCallTypeGCAware = js.internJString("GCAWARE")
  val jstrAjCallTypeManual = js.internJString("MANUAL")
  val jstrAjCallTypeRTCall = js.internJString("RTCALL")

  val jstrAjInline       = js.internJString("Lcom/huawei/excelsior/aj/lang/Inline;")
  val jstrAjInlineForced = js.internJString("forced")

  val jstrAjNoInline = js.internJString("Lcom/huawei/excelsior/aj/lang/NoInline;")

  val jstrAjIntrinsic = js.internJString("Lcom/huawei/excelsior/aj/lang/Intrinsic;")

  val jstrGenTableSwitch = js.internJString("Lcom/huawei/excelsior/aj/lang/GenTableSwitch;")

  val jstrAjCallToManaged = js.internJString("Lcom/huawei/excelsior/aj/jetrt/CallToManaged;")
  val jstrAjCallToManagedClassName = js.internJString("declaringClassName")
  val jstrAjCallToManagedName = js.internJString("name")

  val jstrAjFlat = js.internJString("Lcom/huawei/excelsior/aj/lang/Flat;")

  val jstrAjLayoutInfoInstanceField = js.internJString("Lcom/huawei/excelsior/aj/internal/comp/LayoutInfo$InstanceField;")
  val jstrAjLayoutInfoInstanceFieldOffset = js.internJString("offset")

  val jstrAjLayoutInfoFlatField = js.internJString("Lcom/huawei/excelsior/aj/internal/comp/LayoutInfo$FlatField;")
  val jstrAjLayoutInfoType = js.internJString("Lcom/huawei/excelsior/aj/internal/comp/LayoutInfo$Type;")
  val jstrAjLayoutInfoANYSize = js.internJString("size")
  val jstrAjLayoutInfoANYAlignment = js.internJString("alignment")

  val jstrAjReplacement = js.internJString("Lcom/huawei/excelsior/aj/jetrt/Replacement;")
  val jstrAjReplacementClassName = js.internJString("declaringClassName")
  val jstrAjReplacementName = js.internJString("name")
  val jstrAjReplacementSig = js.internJString("sig")
  val jstrAjReplacementEnvironment = js.internJString("environment")

  val jstrAjUncheckedCall = js.internJString("Lcom/huawei/excelsior/aj/jetrt/UncheckedCall;")
  val jstrAjUncheckedCallClassName = js.internJString("declaringClassName")
  val jstrAjUncheckedCallName = js.internJString("name")
  val jstrAjUncheckedCallSig = js.internJString("sig")

  val jstrAjUncheckedNew = js.internJString("Lcom/huawei/excelsior/aj/jetrt/UncheckedNew;")
  val jstrAjUncheckedNewClassName = js.internJString("declaringClassName")
  val jstrAjUncheckedNewSig = js.internJString("sig")

  val jstrAjProcedureTypeInvokePrefix = js.internJString("__aj__invoke__")

  val jstrAjImplicitImport = js.internJString("Lcom/huawei/excelsior/aj/internal/comp/ImplicitImport;")
  val jstrAjImplicitImportValue = js.internJString("value")

  val jstrAjStruct = js.internJString("Lcom/huawei/excelsior/aj/lang/Struct;")
  val jstrAjThin = js.internJString("Lcom/huawei/excelsior/aj/lang/Thin;")
  val jstrAjValue = js.internJString("Lcom/huawei/excelsior/aj/lang/Value;")
  val jstrAjPolyThin = js.internJString("Lcom/huawei/excelsior/aj/internal/comp/PolyThin;")
  val jstrAjNamespace = js.internJString("Lcom/huawei/excelsior/aj/lang/Namespace;")
  val jstrAjManaged = js.internJString("Lcom/huawei/excelsior/aj/lang/Managed;")
  val jstrAjExtended = js.internJString("Lcom/huawei/excelsior/aj/lang/AJExtended;")
  val jstrAjThinConstructor = js.internJString("Lcom/huawei/excelsior/aj/internal/comp/ThinConstructor;")

  val jstrAjCompilerHintMethod = js.internJString("Lcom/huawei/excelsior/aj/lang/CompilerHint$Method;")
  val jstrAjCompilerHintMethodValue = js.internJString("value")
  val jstrAjCompilerHintMethodNoEscape = js.internJString("no-escape")
  val jstrAjCompilerHintMethodRetThis = js.internJString("ret-this")
  val jstrAjCompilerHintMethodAllocator = js.internJString("allocator")
  val jstrAjCompilerHintMethodNoReturn = js.internJString("no-return")

  val jstrAjCompilerHintStackCheckByCaller = js.internJString("Lcom/huawei/excelsior/aj/lang/CompilerHint$StackCheckByCaller;")
  val jstrAjCompilerHintStackCheckByCallerValue = js.internJString("value")

  val jstrAjInlineIfConstParamsIndices = js.internJString("Lcom/huawei/excelsior/aj/internal/comp/InlineIfConstParamsIndices;")
  val jstrAjInlineIfConstParamsIndicesValue = js.internJString("value")

  val jstrAjGCAware = js.internJString("Lcom/huawei/excelsior/aj/lang/GCAware;")
  val jstrAjLongSafe = js.internJString("Lcom/huawei/excelsior/aj/lang/LongSafe;")
  val jstrAjNoLocalGCPoints = js.internJString("Lcom/huawei/excelsior/aj/lang/NoLocalGCPoints;")
  val jstrAjNoTracedRegsOnEntry = js.internJString("Lcom/huawei/excelsior/aj/jetrt/NoTracedRegsOnEntry;")
  val jstrAjDirtyForClassGC = js.internJString("Lcom/huawei/excelsior/aj/internal/comp/DirtyForClassGC;")
  val jstrAjStrictMemory = js.internJString("Lcom/huawei/excelsior/aj/lang/StrictMemory;")
  val jstrAjVersionedContext = js.internJString("Lcom/huawei/excelsior/aj/lang/VersionedContext;")
  val jstrAjHookInvoker = js.internJString("Lcom/huawei/excelsior/aj/jetrt/Hook$Invoker;")

  val jstrAjBootstrap = js.internJString("Lcom/huawei/excelsior/aj/lang/Bootstrap;")
  val jstrAjNonBootstrap = js.internJString("Lcom/huawei/excelsior/aj/internal/NonBootstrap;")
  val jstrAjNoPreparationCheck = js.internJString("Lcom/huawei/excelsior/aj/internal/NoPreparationCheck;")
  val jstrAjInterpretationLoop = js.internJString("Lcom/huawei/excelsior/aj/jetrt/InterpretationLoop;")
  val jstrAjInterpreterInternals = js.internJString("Lcom/huawei/excelsior/aj/jetrt/InterpreterInternals;")

  val jstrNonThrowing = js.internJString("Lcom/huawei/excelsior/aj/jetrt/NonThrowing;")

  val jstrAjThinTypeUncheckedCastPrefix = js.internJString("__thin__uncheckedCast")
  val jstrAjThinTypeGetFlatPrefix = js.internJString("__thin__getFlat")

  val jstrAjDelayedIntrinsic = js.internJString("Lcom/huawei/excelsior/aj/jetrt/DelayedIntrinsic;")
  val jstrAjDelayedIntrinsicDeclaringClassName = js.internJString("declaringClassName")
  val jstrAjDelayedIntrinsicName = js.internJString("name")

  val jstrAjDomain: XString = js.internJString("Lcom/huawei/excelsior/aj/lang/Domain;")
  val jstrAjDomainValue: XString = js.internJString("value")
  val jstrAjDomainTypeAj: XString = js.internJString("AJ")
  val jstrAjDomainTypeJava: XString = js.internJString("Java")
  val jstrAjDomainTypeCangjie: XString = js.internJString("Cangjie")

  val jstrAjVersionedMarker: XString = js.internJString("Lcom/huawei/excelsior/aj/internal/comp/VersionedMarker;")
  val jstrAjVersionedMarkerDeclaringClassNameGCAware: XString = js.internJString("declaringClassNameGCAware")
  val jstrAjVersionedMarkerNameGCAware: XString = js.internJString("nameGCAware")
  val jstrAjVersionedMarkerDeclaringClassNameUnmanaged: XString = js.internJString("declaringClassNameUnmanaged")
  val jstrAjVersionedMarkerNameUnmanaged: XString = js.internJString("nameUnmanaged")

  val jstrAjCallConvHead = js.internJString("Lcom/huawei/excelsior/aj/lang/CallConv$Head;")
  val jstrAjCallConvHeadInLimit = js.internJString("inLimit")
  val jstrAjCallConvHeadOutLimit = js.internJString("outLimit")

  val jstrAjCallConvPreserved = js.internJString("Lcom/huawei/excelsior/aj/lang/CallConv$Preserved;")
  val jstrAjCallConvAltLocation = js.internJString("Lcom/huawei/excelsior/aj/lang/CallConv$AltLocation;")

  val jstrAjMethodInfoFrameDescriptorGetter = js.internJString("Lcom/huawei/excelsior/aj/jetrt/MethodInfoFrameDescriptorGetter;")

  val jstrRecordInitializer = js.internJString("Lcom/huawei/excelsior/jet/runtime/cangjie/type/mappings/RecordInitializer;")

  //---------------------------------------------------------------------------
  private val magic: UInt = 0x0CAFEBABE.toUInt
  var c: PtrClassInfo = _
  var error: XString = _
  private var maxBootstrapMethodAttrIndex: Int = _
  var needVerify: Boolean = true
  var relaxVerify: Boolean = true // true if VerifyAll compiler option turned off
  private var bcVersionAt15: Boolean = _
  private var bcVersionAt16: Boolean = _
  private var bcVersionAt7: Boolean = _
  private var bcVersionAt8: Boolean = _
  private var dimOverflow: Boolean = _
  // number of parameterss of
                                                     // method to which this attribute belongs
  // code length of code to which this attribute belongs
  //---------------------------------------------------------------------------
  /*******************************************************************************/
  private var fileLength: Int = _
  private var readed: Int = _
  private var curFile: xfs.SymFile = _
  val CLASS_TYPE_PARAMETER: Int = 0x0
  val METHOD_TYPE_PARAMETER: Int = 0x1
  val CLASS_EXTENDS: Int = 0x10
  val CLASS_TYPE_PARAMETER_BOUND: Int = 0x11
  val METHOD_TYPE_PARAMETER_BOUND: Int = 0x12
  val FIELD: Int = 0x13
  val METHOD_RETURN: Int = 0x14
  val METHOD_RECEIVER: Int = 0x15
  val METHOD_FORMAL_PARAMETER: Int = 0x16
  val THROWS: Int = 0x17
  val placeholderCPElement = new ConstantInfo

  private def setBytecode(f: xfs.SymFile): Unit = {
    curFile = f
    fileLength = f.lengthAsInt
    readed = 0
  }

  private def readUByte(): Int = {
    var b: Int = -1

    if (readed < fileLength) {
      readed += 1
      b = curFile.read()
    }
    if (b < 0) {
      error = js.newJString("Truncated file")
      throw new AssertionError
    }
    b
  }

  /** Read an integer (16 bits) from a binary file.  */
  private def readUShort(): UShort = {
    val hi = readUByte()
    val lo = readUByte()
    ((hi << 8) + lo).toUShort
  }

  /** Read a long integer (32 bits) from a binary file.  */
  private def readUInt(): UInt = {
    val hi = readUShort().toInt
    val lo = readUShort().toInt
    ((hi << 16) + lo).toUInt
  }

  private def checkUtf8(str: XString, length: Int): Boolean = {
    var i = 0
    while (i < length) {
      val ch: Int = str.charAt(i) & 0xff
      if (ch == 0) {
        return false
      }
      if (ch >= 128) {
        ch >>> 4 match {
          case 0x8 | 0x9 | 0xA | 0xB | 0xF =>
            return false
          case 0xC | 0xD =>
            /* 110xxxxx  10xxxxxx */
            i += 1
            if (i >= length) {
              return false
            }
            if ((str.charAt(i) & 0xC0) != 0x80) {
              return false
            }
          case 0xE =>
            /* 1110xxxx 10xxxxxx 10xxxxxx */
            i += 2
            if (i >= length) {
              return false
            }
            if ((str.charAt(i - 1) & 0xC0) != 0x80 || (str.charAt(i) & 0xC0) != 0x80) {
              return false
            }
        }
      }
      i += 1
    }
    true
  }

  private def readUtf8(Length: Int, buf: js.StringBuffer): XString = {
    var needVerify = false
    buf.trunc(0)
    buf.ensureCapacity(Length)

    for (_ <- 0 until Length) {
      val ch = readUByte()

      if (ch == 0) { // invalid UTF-8 string
        return null
      }

      if (ch > 0x7f) {
        needVerify = true
      }

      buf.appendChar(ch.toChar)
    }
    val s = buf.intern()

    if (needVerify && !checkUtf8(s, Length)) null else s
  }

  private def obtainString(NameIndex: UShort, IsName: Boolean): XString = {
    if (c.constantPool(NameIndex.toInt).constantType == TagUtf8.toByte) {
      c.constantPool(NameIndex.toInt).bufferPtr
    } else {
      error = js.format("utf8 expected at constant pool index: %d", NameIndex.toUInt.toInt)
      null
    }
  }

  private def getCPEType(index: UShort): Byte = {
    if (index == UShort(0) || index >= c.constantPoolCount) {
      return TagInvalid.toByte
    }
    c.constantPool(index.toInt).constantType
  }

  private def checkCPEType(index: UShort, Tag: UShort): Boolean = {
    if (getCPEType(index).toInt != Tag.toInt) {
      error = js.format("Invalid constant pool type at: %d", index.toUInt.toInt)
      return false
    }
    true
  }

  def getString(classInfo: PtrClassInfo, i: Int): XString = {
    assert(classInfo.constantPool(i).constantType == TagUtf8.toByte)
    classInfo.constantPool(i).bufferPtr
  }

  def getAttribute(C: PtrClassInfo, attrs: Array[AttributeInfo], count: Int, name: XString): Option[AttributeInfo] = {
    for (i <- 0 until count) {
      val attr = attrs(i)
      if (!attr.unknown && getString(C, attr.nameIndex.toInt).equals(name)) {
        return Some(attr)
      }
    }
    None
  }

  def getCode(C: PtrClassInfo, method: MethodInfo): PtrCodeInfo =
    getAttribute(C, method.attribute, method.attributeCount.toInt, jstrCodeName).map(_.code).orNull

  private def checkNamePart(name: XString, from: Int, to0: Int, namekind: ji.NameKinds, relaxed: Boolean): Boolean = {
    if (relaxed) {
      return true // do not check name
    }
    ji.utf8PartIsIdentifier(name, from, to0, namekind == ji.nk_class)
  }

  private def checkName(name: XString, namekind: ji.NameKinds): Boolean = {
    var res: Boolean = false

    if (namekind == ji.nk_method && name.charAt(0) == '<') {
      res = name.equals(js.jstrInit) || name.equals(js.jstrClinit)
    } else if (!bcVersionAt15) {
      res = checkNamePart(name, 0, name.length, namekind, relaxVerify)
    } else {
      res = relaxVerify || ji.check15Name(name, namekind)
    }
    if (!res) {
      namekind match {
        case ji.nk_class =>
          error = js.format("Invalid class name: %S", name)
        case ji.nk_method =>
          error = js.format("Invalid method name: %S", name)
        case ji.nk_field =>
          error = js.format("Invalid field name: %S", name)
      }
    }
    res
  }

  /* for 1.5 checks for signatures relaxed */
  private def checkAllClassNames15InSig(sig: XString): Boolean = sig.indexOf('.') < 0

  private def checkOneSigElement(/*VAR*/ iter: SignatureIterator): Boolean = {
    assert(iter.hasNext)
    if (!iter.next()) {
      return false
    }
    if (iter.isArray) {
      if (iter.dim > 255) {
        dimOverflow = true
        return false
      }
    }
    val sig = iter.getSig
    val pos = iter.getLastElemPos()
    val endpos = iter.getLastElemEndPos
    sig.charAt(pos) match {
      case 'B' |
           'C' |
           'F' |
           'D' |
           'I' |
           'J' |
           'S' |
           'Z' =>
        true
      case 'L' =>
        if (!bcVersionAt15) {
          if (!checkNamePart(sig, pos + 1, endpos - 1, ji.nk_class, relaxed = false)) {
            return false
          }
        } else {
          // class names in the whole signature are already checked
        }
        true
      case _ =>
        false
    }
  }

  private def checkFieldDescriptor0(sig: XString): Boolean = {
    var iter: SignatureIterator = new SignatureIterator()

    if (sig.isEmpty) {
      return false
    }

    if (bcVersionAt15) {
      if (!checkAllClassNames15InSig(sig)) {
        return false
      }
    }

    iter.init(sig)

    if (checkOneSigElement(iter)) {
      !iter.hasNext
    } else {
      false
    }
  }

  private def checkClassName(sig: XString): Boolean = checkFieldDescriptor0(sig)

  private def checkFieldDescriptor(sig: XString): Boolean = {
    val res = checkFieldDescriptor0(sig)
    if (!res) {
      error = js.format("Invalid field decriptor: %S", sig)
    }
    res
  }

  // on success, returns arguments number
  // on failure, returns -1
  private def checkMethodDescriptor0(sig: XString, static: Boolean): Int = {
    var retIter: SignatureIterator = new SignatureIterator()
    var argIter: SignatureIterator = new SignatureIterator()
    var np: Int = 0

    if (sig.isEmpty || sig.charAt(0) != '(') {
      return -1
    }
    val endpos = sig.lastIndexOf(')')
    if (endpos == -1 || sig.length == endpos + 1) {
      return -1
    }
    if (bcVersionAt15) {
      if (!checkAllClassNames15InSig(sig)) {
        return -1
      }
    }
    if (static) {
      np = 0
    } else {
      np = 1
    }
    argIter.initEx(sig, 1, endpos)
    while (argIter.hasNext) {
      val ch = argIter.getCurSigChar
      if (ch == 'J' || ch == 'D') {
        np += 2
      } else {
        np += 1
      }
      if (!checkOneSigElement(argIter)) {
        return -1
      }
    }
    if (np > 255) {
      error = js.format("Invalid method arguments number: %d", np)
      return -1
    }
    val retpos = endpos + 1
    if (sig.charAt(retpos) == 'V') {
      if (sig.length == retpos + 1) {
        return np
      }
    } else {
      retIter.initEx(sig, retpos, sig.length)
      if (checkOneSigElement(retIter)) {
        if (!retIter.hasNext) {
          return np
        }
      }
    }
    -1
  }

  // on success, returns arguments number
  // on failure, returns -1
  private def checkMethodDescriptor(sig: XString, static: Boolean): Int = {
    val np = checkMethodDescriptor0(sig, static)
    if (np < 0 && error == null) {
      error = js.format("Invalid method decriptor: %S", sig)
    }
    np
  }

  private def checkEnd(): Boolean = {
    if (relaxVerify) {
      return true
    }
    val res = readed == fileLength
    if (!res) {
      error = js.format("File has extra bytes")
    }
    res
  }

  private def getConstantPool: Boolean = {
    if (c.constantPoolCount == UShort(0)) {
      return false
    }
    c.constantPool = Array.fill[ConstantInfo](c.constantPoolCount.toInt)(new ConstantInfo())
    c.constantPool(0).constantType = TagInteger.toByte
    c.constantPool(0).high = 0
    c.constantPool(0).low = 0

    val buf = new js.StringBuffer() // working buffer; used by ReadUtf8; declared here for performance reasons
    var I = 1
    while (I < c.constantPoolCount.toInt) {
      val constant = c.constantPool(I)
      val constantType = readUByte()
      constant.constantType = constantType.toByte
      constantType match {
        case TagClass =>
          constant.indexName = readUShort()
          constant.resolvedType = null
        case TagField =>
          constant.index = readUShort()
          constant.indexName = readUShort()
        case TagMethod =>
          constant.index = readUShort()
          constant.indexName = readUShort()
        case TagIMethod =>
          constant.index = readUShort()
          constant.indexName = readUShort()
        case TagString =>
          constant.index = readUShort()
        case TagInteger =>
          constant.high = 0
          constant.low = readUInt().toInt
        case TagFloat =>
          constant.realVal = java.lang.Float.intBitsToFloat(readUInt().toInt)
        case TagLong =>
          constant.high = readUInt().toInt
          constant.low = readUInt().toInt
          I += 1
        case TagDouble =>
          val high = readUInt().toInt
          val low = readUInt().toInt
          constant.longRealVal = java.lang.Double.longBitsToDouble(MathUtils.makeLong(low, high))
          I += 1
        case TagNameAndType =>
          constant.indexName = readUShort()
          constant.index = readUShort()
        case TagUtf8 =>
          val length = readUShort()
          val str = readUtf8(length.toInt, buf)
          if (str == null) {
            error = js.format("Bad Utf8 string at: %d", I)
            return false
          }
          constant.bufferPtr = str
        case TagMethodHandle =>
          if (!bcVersionAt7) {
            error = js.format("Class file version does not support constant tag %d", constantType.toInt)
            return false
          }
          val refKind = readUByte()
          if (refKind < MH_REF_FIRST || refKind > MH_REF_LAST) {
            error = js.format("Bad method handle kind at constant pool index %d", I)
            return false
          }
          constant.low = refKind
          constant.index = readUShort()
        case TagMethodType =>
          if (!bcVersionAt7) {
            error = js.format("Class file version does not support constant tag %d", constantType.toInt)
            return false
          }
          constant.index = readUShort()
        case TagInvokeDynamic =>
          if (!bcVersionAt7) {
            error = js.format("Class file version does not support constant tag %d", constantType.toInt)
            return false
          }
          val index = readUShort()
          if (index.toInt >= maxBootstrapMethodAttrIndex) {
            maxBootstrapMethodAttrIndex = index.toInt
          }
          constant.index = index
          constant.indexName = readUShort()
        case _ =>
          return false
      }
      I += 1
    }

    // verifying constant pool
    I = 1
    while (I < c.constantPoolCount.toInt) {
      val constant = c.constantPool(I)
      val ConstantType = constant.constantType
      ConstantType match {
        case TagClass =>
          val StrPtr = obtainString(constant.indexName, IsName = false) /*IsName:=*/
          if (StrPtr == null || StrPtr.isEmpty) {
            return false
          }
          if (StrPtr.charAt(0) == '[') {
            if (!checkClassName(StrPtr)) {
              error = js.format("Invalid class name: %S", StrPtr)
              return false
            }
          } else if (!checkName(StrPtr, ji.nk_class)) {
            return false
          }
        case TagField |
             TagMethod |
             TagIMethod |
             TagInvokeDynamic =>
          if (ConstantType != TagInvokeDynamic.toByte) {
            if (!checkCPEType(constant.index, TagClass.toUShort)) {
              return false
            }
          }
          if (!checkCPEType(constant.indexName, TagNameAndType.toUShort)) {
            return false
          }
          val sig = obtainString(c.constantPool(constant.indexName.toInt).index, IsName = false)
                             /*IsName:=*/
          if (sig == null || sig.isEmpty) {
            return false
          }
          val StrPtr = obtainString(c.constantPool(constant.indexName.toInt).indexName, IsName = false)
                                /*IsName:=*/
          if (StrPtr == null) {
            return false
          }
          if (ConstantType != TagField.toByte) {
            if (!checkName(StrPtr, ji.nk_method)) {
              return false
            }
          } else if (!checkName(StrPtr, ji.nk_field)) {
            return false
          }
          if (ConstantType == TagField.toByte) {
            if (bcVersionAt7) {
              if (sig.charAt(0) == '(') {
                error = js.format("Invalid field decriptor: %S", sig)
                return false
              }
            } else if (!checkFieldDescriptor(sig)) {
              return false
            }
          } else {
            if (bcVersionAt7) {
              if (sig.charAt(0) != '(') {
                error = js.format("Invalid method decriptor: %S", sig)
                return false
              }
            } else {
              val np = checkMethodDescriptor(sig, ConstantType == TagMethod.toByte || ConstantType == TagInvokeDynamic.toByte)
              if (np < 0) {
                return false
              }
            }
            if (ConstantType == TagMethod.toByte) {
              // Sun RI check this since 1.4:
              if (StrPtr.charAt(0) == '<' && !(StrPtr.equals(js.jstrInit) && sig.charAt(sig.length - 1) == 'V')) {
                error = js.format("Invalid method reference: %S%S", StrPtr, sig)
                return false
              }
              // Sun RI check this since 1.3:
              /*            IF (StrPtr^='<init>') & ~(sig[LENGTH(sig^)-1]='V') THEN
                            Error := js.format("Invalid method reference: %S%S", StrPtr, sig);
                            RETURN FALSE;
                          END;*/
            }
          }
        case TagNameAndType =>
          val sig = obtainString(constant.index, IsName = false) /*IsName:=*/
          if (sig == null) {
            return false
          }
          if (bcVersionAt7) {
            if (sig.charAt(0) == '(') {
              val np = checkMethodDescriptor(sig, static = true)
              if (np < 0) {
                return false
              }
            } else if (!checkFieldDescriptor(sig)) {
              return false
            }
          }




          /*      Sun RI does not check this
                  IF ~CheckFieldDescriptor0(sig) & (CheckMethodDescriptor0(sig,TRUE) < 0) THEN
                    Error := js.format("Invalid member decriptor: %S", sig);
                    RETURN FALSE;
                  END;*/
          val StrPtr = obtainString(constant.indexName, IsName = false) /*IsName:=*/
          if (StrPtr == null) {
            return false
          }
        case TagString =>
          /*      Sun RI does not check this
                  IF ~CheckName(StrPtr, TRUE) & ~CheckName(StrPtr, FALSE) THEN
                    RETURN FALSE
                  END;*/
          if (!checkCPEType(constant.index, TagUtf8.toUShort)) {
            return false
          }
        case TagLong |
             TagDouble =>
          I += 1
          if (I >= c.constantPoolCount.toInt) {
            error = js.newJString("Illegal constant pool entry")
            return false
          }
        case TagMethodHandle =>
          val refKind = constant.low
          refKind match {
            case REF_getField |
                 REF_getStatic |
                 REF_putField |
                 REF_putStatic =>
              if (!checkCPEType(constant.index, TagField.toUShort)) {
                return false
              }
            case REF_invokeVirtual |
                 REF_newInvokeSpecial =>
              if (!checkCPEType(constant.index, TagMethod.toUShort)) {
                return false
              }
            case REF_invokeInterface =>
              if (!checkCPEType(constant.index, TagIMethod.toUShort)) {
                return false
              }
            case REF_invokeStatic |
                 REF_invokeSpecial =>
              if (!(bcVersionAt8 && getCPEType(constant.index) == TagIMethod.toByte) && !checkCPEType(constant.index, TagMethod.toUShort)) {
                return false
              }
          }

          if (refKind == REF_invokeVirtual || refKind == REF_invokeStatic || refKind == REF_invokeSpecial || refKind == REF_newInvokeSpecial || refKind == REF_invokeInterface) {
            if (!checkCPEType(c.constantPool(constant.index.toInt).indexName, TagNameAndType.toUShort)) {
              return false
            }

            val IndexName = c.constantPool(c.constantPool(constant.index.toInt).indexName.toInt).indexName
            val StrPtr = obtainString(IndexName, IsName = true) /*IsName:=*/
            if (StrPtr == null) {
              return false
            }

            if (refKind == REF_newInvokeSpecial) {
              if (!StrPtr.equals(js.jstrInit)) {
                error = js.format("Bad constructor name at constant pool index %d", IndexName.toUInt.toInt)
                return false
              }
            } else if (StrPtr.equals(js.jstrInit) || StrPtr.equals(js.jstrClinit)) {
              error = js.format("Bad method name at constant pool index %d", IndexName.toUInt.toInt)
              return false
            }
          }
        case TagMethodType =>
          val sig = obtainString(constant.index, IsName = false) /*IsName:=*/
          if (sig == null) {
            return false
          }

          val np = checkMethodDescriptor(sig, static = true)
          if (np < 0) {
            return false
          }
        case _ =>
      }
      I += 1
    }
    true
  }

  private def checkPC(Code: PtrCodeInfo, PC: UShort): Boolean = {
    if (PC.toInt >= Code.codeLength) {
      error = js.format("Illegal exception table range")
      return false
    }
    true
  }

  private def loadCode(Code: PtrCodeInfo, NP: Int): Boolean = {
    var attr: AttributeData = new AttributeData()

    var Ok = false

    /* Get the beginning of header */
    Code.stackSize = readUShort()
    Code.localSize = readUShort()

    val codeULen = readUInt()
    if (codeULen > 65535) {
      error = js.newJString("Code is too long")
      return false
    }
    Code.codeLength = codeULen.toInt
    if (Code.codeLength == 0) {
      error = js.newJString("Code of a method has length 0")
      return false
    }
    if (NP > Code.localSize.toInt) {
      error = js.newJString("Arguments can\'t fit into locals")
      return false
    }
    Code.codePtr = new Array[Byte](Code.codeLength)

    /* Disassemble the code, and store it */
    for (i <- 0 until Code.codeLength) { // TODO: blockRead
      Code.codePtr(i) = readUByte().toByte
    }

    /* Get the continuation of header */
    Code.excepTableLength = readUShort()
    if (Code.excepTableLength == UShort(0)) {
      Code.excepTable = null
    } else {
      Code.excepTable = Array.fill[ExcepInfo](Code.excepTableLength.toInt)(new ExcepInfo())
      for (k <- 0 until Code.excepTableLength.toInt) {
        Code.excepTable(k).startPC = readUShort()
        Code.excepTable(k).endPC = readUShort()
        if (bcVersionAt15 && Code.excepTable(k).startPC >= Code.excepTable(k).endPC) {
          error = js.newJString("Illegal exception table range")
          return false
        }
        Code.excepTable(k).handlerPC = readUShort()
        if (bcVersionAt15 && !(checkPC(Code, Code.excepTable(k).startPC) &&
          (Code.excepTable(k).endPC.toInt == Code.codeLength || checkPC(Code, Code.excepTable(k).endPC)) && checkPC(Code, Code.excepTable(k).handlerPC))) {
          return false
        }
        Code.excepTable(k).catchType = readUShort()
        if (Code.excepTable(k).catchType != UShort(0)) {
          if (getCPEType(Code.excepTable(k).catchType) != TagClass.toByte) {
            return false
          }
        }
      }
    }

    Code.attributeCount = readUShort()
    Ok = readAttribute(Code.attributeCount, codeAttrType, -1, Code.codeLength, attr)
    Code.attribute = attr.attributePtr
    if (!Ok) {
      return false
    }

    if (bcVersionAt16 && Code.attributeCount != UShort(0)) {
      var seenStackMapTable = false
      for (j <- 0 until Code.attributeCount.toInt) {
        val StrPtr = obtainString(Code.attribute(j).nameIndex, IsName = false)
        if (StrPtr == null) {
          return false
        }
        if (bcVersionAt16 && StrPtr.equals(jstrStackMapTable)) {
          if (seenStackMapTable) {
            error = js.newJString("Duplicate StackMapTable attribute")
            return false
          }
          seenStackMapTable = true
        }
      }
    }

    true
  }

  private def allocLocalVariable(Length: UShort, CodeLength: Int): Array[LocalVariable] = {
    // Note that this code checks not all assertions from specification.
    // Old Oberon verifier checks some more but not all.
    // New Java verifier checks nothing (because nobody needs it).
    // Refactor all this when time comes.
    assert(Length > UShort(0))
    val Table = Array.fill[LocalVariable](Length.toInt)(new LocalVariable())
    for (i <- 0 until Length.toInt) {
      Table(i).startPC = readUShort()



      Table(i).length = readUShort()

      if (Table(i).startPC.toInt >= CodeLength || (Table(i).startPC + Table(i).length).toInt > CodeLength) {
        error = js.newJString("Invalid start_pc/length in local var table")
        return null
      }

      var Index = readUShort()
      var StrPtr = obtainString(Index, IsName = true) /*IsName:=*/
      if (StrPtr == null) {
        return null
      }
      if (!checkName(StrPtr, ji.nk_field)) {
        return null
      }



      Table(i).nameIndex = Index

      Index = readUShort()
      StrPtr = obtainString(Index, IsName = true) /*IsName:=*/
      if (StrPtr == null) {
        return null
      }
      if (!checkFieldDescriptor(StrPtr)) {
        return null
      }



      Table(i).signatureIndex = Index

      Index = readUShort()



      Table(i).slot = Index
    }
    Table.sortInPlaceBy(_.slot.toInt).array
  }

  private def allocLineNumber(Length: UShort, CodeLength: Int): Array[LineNumber] = {
    assert(Length > UShort(0))
    val Table = Array.fill[LineNumber](Length.toInt)(new LineNumber())
    for (i <- 0 until Length.toInt) {
      Table(i).startPC = readUShort()
      if (Table(i).startPC.toInt >= CodeLength) {
        error = js.newJString("Invalid pc in line number table")
        return null
      }
      Table(i).lineNumber = readUShort()
    }
    Table
  }

  // Set-method in order to access LineNumber.LineNumber field from CodeAttributeImpl.
  def setLineNumber(/*VAR*/ this0: LineNumber, lineNum: UShort): Unit = {
    this0.lineNumber = lineNum
  }

  private def allocExceptions(Length: UShort): Array[UShort] = {
    assert(Length > UShort(0))
    val Table = new Array[UShort](Length.toInt)
    for (i <- 0 until Length.toInt) {
      Table(i) = readUShort()
      if (!checkCPEType(Table(i), TagClass.toUShort)) {
        return null
      }
    }
    Table
  }

  private def allocInnerClasses(Length: UShort): Array[InnerClass] = {
    assert(Length > UShort(0))
    val Classes = Array.fill[InnerClass](Length.toInt)(new InnerClass())
    for (i <- 0 until Length.toInt) {
      val innerClass = Classes(i)

      val innerClassInfoIndex = readUShort()
      if (innerClassInfoIndex != UShort(0) && !checkCPEType(innerClassInfoIndex, TagClass.toUShort)) {
        return null
      }
      innerClass.innerClassInfoIndex = innerClassInfoIndex

      val outerClassInfoIndex = readUShort()
      if (outerClassInfoIndex != UShort(0) && !checkCPEType(outerClassInfoIndex, TagClass.toUShort)) {
        return null
      }
      innerClass.outerClassInfoIndex = outerClassInfoIndex

      val innerNameIndex = readUShort()
      if (innerNameIndex != UShort(0) && !checkCPEType(innerNameIndex, TagUtf8.toUShort)) {
        return null
      }
      innerClass.innerNameIndex = innerNameIndex

      if (bcVersionAt15 && innerClass.outerClassInfoIndex == innerClass.innerClassInfoIndex) {
        return null
      }

      innerClass.innerClassAccessFlags = readUShort().toSet32

      /* workaround for SUN bug: enable abstract for interfaces */
      if (innerClass.innerClassAccessFlags contains AccInterface) {
        innerClass.innerClassAccessFlags += AccAbstract.toUByte
      }
    }

    if (bcVersionAt15) {
      // check duplicates
      for (i <- 0 until Length.toInt) {
        for (j <- i + 1 until Length.toInt) {
          if (Classes(i).innerClassInfoIndex == Classes(j).innerClassInfoIndex &&
            Classes(i).outerClassInfoIndex == Classes(j).outerClassInfoIndex &&
            Classes(i).innerNameIndex == Classes(j).innerNameIndex &&
            Classes(i).innerClassAccessFlags == Classes(j).innerClassAccessFlags) {

            return null
          }
        }
      }
    }

    Classes
  }

  private def readAnnotation(): PtrAnnotation = {
    var Index = readUShort()
    if (!checkCPEType(Index, TagUtf8.toUShort)) {
      return null
    }
    val annot = new PtrAnnotation()
    annot.type0 = c.constantPool(Index.toInt).bufferPtr
    Index = readUShort()
    annot.pairs = Array.fill[AnnotationPair](Index.toInt)(new AnnotationPair())
    for (i <- annot.pairs.indices) {
      Index = readUShort()
      if (!checkCPEType(Index, TagUtf8.toUShort)) {
        return null
      }
      annot.pairs(i).name = c.constantPool(Index.toInt).bufferPtr
      val elem = readElementValue()
      if (elem == null) {
        return null
      }
      annot.pairs(i).value = elem
    }
    annot
  }

  private def readElementValue(): PtrElementValue = {
    val tag = readUByte().toChar
    val value = tag match {
      case 'B' | 'C' | 'I' | 'S' | 'Z' =>
        val index = readUShort()
        if (!checkCPEType(index, TagInteger.toUShort)) return null
        PtrIntElementValue(c.constantPool(index.toInt).low)
      case 'J' =>
        val index = readUShort()
        if (!checkCPEType(index, TagLong.toUShort)) return null
        PtrLongElementValue(c.constantPool(index.toInt).low, c.constantPool(index.toInt).high)
      case 'F' =>
        val index = readUShort()
        if (!checkCPEType(index, TagFloat.toUShort)) return null
        new PtrFloatElementValue(c.constantPool(index.toInt).realVal)
      case 'D' =>
        val index = readUShort()
        if (!checkCPEType(index, TagDouble.toUShort)) return null
        new PtrDoubleElementValue(c.constantPool(index.toInt).longRealVal)
      case 's' =>
        val index = readUShort()
        if (!checkCPEType(index, TagUtf8.toUShort)) return null
        PtrStringElementValue(c.constantPool(index.toInt).bufferPtr)
      case 'e' =>
        val typeNameIndex = readUShort()
        if (!checkCPEType(typeNameIndex, TagUtf8.toUShort)) return null
        val constNameIndex = readUShort()
        if (!checkCPEType(constNameIndex, TagUtf8.toUShort)) return null
        PtrEnumElementValue(c.constantPool(typeNameIndex.toInt).bufferPtr, c.constantPool(constNameIndex.toInt).bufferPtr)
      case 'c' =>
        val index = readUShort()
        if (!checkCPEType(index, TagUtf8.toUShort)) return null
        PtrClassElementValue(c.constantPool(index.toInt).bufferPtr)
      case '@' =>
        val annot = PtrAnnotationElementValue(readAnnotation())
        if (annot.value == null) return null
        annot
      case '[' =>
        val index = readUShort()
        val array = new Array[PtrElementValue](index.toInt)
        for (i <- array.indices) {
          array(i) = readElementValue()
          if (array(i) == null) return null
        }
        PtrArrayElementValue(array)
      case _ =>
        return null
    }
    value.tag = tag
    value
  }

  private def readTargetInfo(targetType: Byte): PtrTargetInfo = {
    targetType match {
      case CLASS_TYPE_PARAMETER |
           METHOD_TYPE_PARAMETER |
           METHOD_FORMAL_PARAMETER =>
        PtrOneByteTargetInfo(readUByte().toByte)
      case CLASS_EXTENDS |
           THROWS =>
        PtrWordTargetInfo(readUShort())
      case CLASS_TYPE_PARAMETER_BOUND |
           METHOD_TYPE_PARAMETER_BOUND =>
        PtrTwoBytesTargetInfo(readUByte().toByte, readUByte().toByte)
      case FIELD |
           METHOD_RETURN |
           METHOD_RECEIVER =>
        val empty = new PtrTargetInfo()
        empty
      case _ =>
        // Code attribute: do not parse
        null
    }
  }

  private def readTypeAnnotation(): PtrTypeAnnotation = {
    val annot = new PtrTypeAnnotation()
    annot.targetType = readUByte().toByte
    annot.targetInfo = readTargetInfo(annot.targetType)

    if (annot.targetInfo == null) {
      return null
    }

    annot.pathLength = readUByte().toByte
    annot.path = new Array[Byte](2 * annot.pathLength)
    for (i <- annot.path.indices) {
      annot.path(i) = readUByte().toByte
    }

    annot.annotation = readAnnotation()
    if (annot.annotation == null) {
      return null
    }

    annot
  }

  private def malformedAnnotation(ann: PtrAbstractAnnotationAttr, skipBytesPar: Int): PtrAbstractAnnotationAttr = {
    var skipBytes = skipBytesPar

    assert(skipBytes >= 0)
    ann.isMalformed = true
    while (skipBytes != 0) {
      readUByte()
      skipBytes -= 1
    }
    error = null // reset error
    ann
  }

  private def allocAnnotations(attrLength: Int): PtrAbstractAnnotationAttr = {
    val startPos = readed
    val ann = new PtrAnnotationsAttr()
    ann.isMalformed = false
    val Index = readUShort()
    ann.annotations = new Array[PtrAnnotation](Index.toInt)
    for (i <- ann.annotations.indices) {
      val v = readAnnotation()
      if (v == null) {
        return malformedAnnotation(ann, attrLength - (readed - startPos))
      }
      ann.annotations(i) = v
    }
    ann
  }

  private def allocTypeAnnotations(attrLength: Int): PtrAbstractAnnotationAttr = {
    val startPos = readed
    val ann = new PtrTypeAnnotationsAttr()
    ann.isMalformed = false
    val Index = readUShort()
    ann.typeAnnotations = new Array[PtrTypeAnnotation](Index.toInt)
    for (i <- ann.typeAnnotations.indices) {
      val v = readTypeAnnotation()
      if (v == null) {
        return malformedAnnotation(ann, attrLength - (readed - startPos))
      }
      ann.typeAnnotations(i) = v
    }
    ann
  }

  private def allocParameterAnnotations(attrLength: Int): PtrAbstractAnnotationAttr = {
    val startPos = readed
    val ann = new PtrParameterAnnotationsAttr()
    ann.isMalformed = false
    ann.annotations = new Array[Array[PtrAnnotation]](readUByte())
    for (i <- ann.annotations.indices) {
      val Index = readUShort()
      ann.annotations(i) = new Array[PtrAnnotation](Index.toInt)
      for (j <- ann.annotations(i).indices) {
        val v = readAnnotation()
        if (v == null) {
          return malformedAnnotation(ann, attrLength - (readed - startPos))
        }
        ann.annotations(i)(j) = v
      }
    }
    ann
  }

  private def allocAnnotationDefault(attrLength: Int): PtrAbstractAnnotationAttr = {
    val startPos = readed
    val ann = new PtrAnnotationDefaultAttr()
    ann.isMalformed = false
    val value = readElementValue()
    if (value == null) {
      return malformedAnnotation(ann, attrLength - (readed - startPos))
    }
    ann.defaultValue = value
    ann
  }

  private def allocBootstrapMethods(attrLength: Int, numBootstrapMethods: Int): Array[PtrBootstrapMethod] = {
    var args: Array[UShort] = null

    val startPos = readed - 2  // numBootstrapMethods has been read already

    val methods = ArrayBuffer.empty[PtrBootstrapMethod]
    for (_ <- 0 until numBootstrapMethods) {
      val methodIndex = readUShort()
      if (!checkCPEType(methodIndex, TagMethodHandle.toUShort)) {
        return null
      }

      val numArgs = readUShort().toInt
      if (numArgs > 0) {
        args = new Array[UShort](numArgs)

        for (j <- 0 until numArgs) {
          val argIndex = readUShort()

          val argTag = getCPEType(argIndex)
          if (argTag != TagString.toByte && argTag != TagClass.toByte && argTag != TagInteger.toByte && argTag != TagLong.toByte && argTag != TagFloat.toByte && argTag != TagDouble.toByte && argTag != TagMethodHandle.toByte && argTag != TagMethodType.toByte) {
            error = js.format("Invalid constant pool type at: %d", argIndex.toUInt.toInt)
            return null
          }

          args(j) = argIndex
        }
      } else {
        args = null
      }

      methods += PtrBootstrapMethod(methodIndex, args)
    }

    if (readed - startPos != attrLength) {
      error = js.newJString("BootstrapMethods attribute has wrong length")
      return null
    }

    methods.toArray
  }

  private def zero(): Int = 0
  // workaround for XDS compiler "feature": it does not allow to create
  // arrays with zero length explicitely

  private def allocMethodParameters(attrLength: Int, numMethodParams: Int): Array[MethodParameter] = {
    assert(numMethodParams > 0)

    val startPos = readed - 1  // numMethodParams has been read already

    var malformed = false
    var params = Array.fill[MethodParameter](numMethodParams)(new MethodParameter())
    for (i <- 0 until numMethodParams) {
      val nameIndex = readUShort()
      if (getCPEType(nameIndex) == TagUtf8.toByte) {
        params(i).name = c.constantPool(nameIndex.toInt).bufferPtr
      } else if (getCPEType(nameIndex) == 0.toByte) {
        params(i).name = null
      } else {
        malformed = true
      }
      params(i).accessFlags = readUShort()
    }

    assert(readed - startPos == attrLength) // should be checked by caller
    if (malformed) {
      // Encode malformed parameters with an array of zero length.
      // It must not clash with the attribute where numMethodParams = 0, because
      // we do not create the attribute in this case.
      params = Array.fill[MethodParameter](zero())(new MethodParameter())
    }
    params
  }

  private def getInterface: Boolean = {
    if (c.interfaceCount == UShort(0)) {
      c.interface = null
    } else {
      c.interface = new Array[UShort](c.interfaceCount.toInt)
      val names = mutable.HashSet.empty[NameAndSig]
      for (i <- 0 until c.interfaceCount.toInt) {
        c.interface(i) = readUShort()
        if (!checkCPEType(c.interface(i), TagClass.toUShort)) {
          return false
        }
        val s = obtainString(c.constantPool(c.interface(i).toInt).indexName, IsName = false)
        if (s == null) {
          return false
        }
        val nameAndSig = NameAndSig(s, null)
        if (names contains nameAndSig) {
          error = js.newJString("Duplicate super interfaces found")
          return false
        }
        names += nameAndSig
      }
    }
    true
  }

  private def skipAttribute(ulength: UInt): Array[Byte] = {
    val length = ulength.toInt
    if (length > 0) {
      val info = new Array[Byte](length)
      for (i <- info.indices) { // TODO: readBlock
        info(i) = readUByte().toByte
      }
      info
    } else null
  }

  private def readAttribute(AttributeCount: UShort, atype: AttrType, NP: Int, codeLength: Int, /*VAR*/ attr: AttributeData): Boolean = {
    // number of parameterss of method to which this attribute belongs
    // code length of code to which this attribute belongs
    var Ok = false
    val alreadySeen = new mutable.HashSet[XString]
    attr.hasSynthetic = false
    if (AttributeCount == UShort(0)) {
      attr.attributePtr = null
    } else {
      attr.attributePtr = Array.fill[AttributeInfo](AttributeCount.toInt)(new AttributeInfo())
      for (i <- 0 until AttributeCount.toInt) {
        val NameIndex = readUShort()

        attr.attributePtr(i).nameIndex = NameIndex
        attr.attributePtr(i).index = UShort(0)
        attr.attributePtr(i).unknown = false

        val StrPtr = obtainString(NameIndex, IsName = true) /*IsName:=*/
        if (StrPtr == null) {
          return false
        }
        val Length = readUInt()

        attr.attributePtr(i).length = Length

        if (StrPtr.equals(jstrCodeName)) {
          attr.attributePtr(i).code = new PtrCodeInfo()
          Ok = loadCode(attr.attributePtr(i).code, NP)
          if (!Ok) {
            return false
          }
        } else if (StrPtr.equals(jstrLocVarName)) {
          val TLength = readUShort()
          if (Length != (TLength * UShort(10) + UShort(2)).toUInt) {
            error = js.newJString("Local variables table has wrong length")
            return false
          }
          if (TLength > UShort(0)) {
            val LVTable = allocLocalVariable(TLength, codeLength)
            if (LVTable == null) {
              return false
            }
            attr.attributePtr(i).localVariableTable = LVTable
          }
        } else if (StrPtr.equals(jstrSourceFile)) {
          attr.attributePtr(i).index = readUShort()
          if (!checkCPEType(attr.attributePtr(i).index, TagUtf8.toUShort)) {
            return false
          }
        } else if (StrPtr.equals(jstrCValue)) {
          if (Length != UInt(2)) {
            error = js.format("Invalid attribute length %d", Length.toInt)
            return false
          }
          attr.attributePtr(i).index = readUShort()
          if (atype == fieldAttrType && !(Set32.of(TagLong.toUByte, TagFloat.toUByte, TagDouble.toUByte, TagInteger.toUByte, TagString.toUByte) contains getCPEType(attr.attributePtr(i).index).toUInt)) {
            error = js.format("Invalid constant pool type at: %d", attr.attributePtr(i).index.toUInt.toInt)
            return false
          }
        } else if (StrPtr.equals(jstrLineNumber)) {
          val TLength = readUShort()
          if (Length != (TLength * UShort(4) + UShort(2)).toUInt) {
            error = js.newJString("Line number table has wrong length")
            return false
          }
          if (TLength > UShort(0)) {
            val LNTable = allocLineNumber(TLength, codeLength)
            if (LNTable == null) {
              return false
            }
            attr.attributePtr(i).lineNumberTable = LNTable
          }
        } else if (StrPtr.equals(jstrException)) {
          val TLength = readUShort()
          if (Length != (TLength * UShort(2) + UShort(2)).toUInt) {
            error = js.newJString("Exceptions attribute has wrong length")
            return false
          }
          if (TLength > UShort(0)) {
            val EITable = allocExceptions(TLength)
            if (EITable == null) {
              return false
            }
            attr.attributePtr(i).exceptionIndexTable = EITable
          }
        } else if (StrPtr.equals(jstrInnerClasses)) {
          val TLength = readUShort()
          if (TLength > UShort(0)) {
            val Classes = allocInnerClasses(TLength)
            if (Classes == null) {
              return false
            }
            attr.attributePtr(i).innerClasses = Classes
          }
        } else if (StrPtr.equals(jstrDeprecated)) {
          if (Length != UInt(0)) {
            error = js.format("Invalid Deprecated attribute length %d", Length.toInt)
            return false
          }
        } else if (StrPtr.equals(jstrSynthetic)) {
          if (Length != UInt(0)) {
            error = js.format("Invalid Synthetic attribute length %d", Length.toInt)
            return false
          }
          attr.hasSynthetic = true
        } else if (bcVersionAt15 && StrPtr.equals(jstrSignature)) {
          if (Length != UInt(2)) {
            error = js.format("Invalid Signature attribute length %d", Length.toInt)
            return false
          }
          attr.attributePtr(i).index = readUShort()
          if (!checkCPEType(attr.attributePtr(i).index, TagUtf8.toUShort)) {
            return false
          }
        } else if (bcVersionAt15 && StrPtr.equals(jstrEnclosingMethod)) {
          if (Length != UInt(4)) {
            error = js.format("Invalid EnclosingMethod attribute length %d", Length.toInt)
            return false
          }
          attr.attributePtr(i).index = readUShort()
          if (!checkCPEType(attr.attributePtr(i).index, TagClass.toUShort)) {
            return false
          }
          attr.attributePtr(i).index2 = readUShort()
          if (attr.attributePtr(i).index2 != UShort(0) && !checkCPEType(attr.attributePtr(i).index2, TagNameAndType.toUShort)) {
            return false
          }
        } else if (bcVersionAt15 && atype != codeAttrType && (StrPtr.equals(jstrRuntimeVisibleAnnotations) || StrPtr.equals(jstrRuntimeInvisibleAnnotations))) {
          // both RuntimeVisibleAnnotations and RuntimeInvisibleAnnotations attributes have similar structure
          if (!alreadySeen.add(StrPtr)) {
            error = js.format("Multiple %S attributes", StrPtr)
            return false
          }

          attr.attributePtr(i).annotation = allocAnnotations(Length.toInt)
          assert(attr.attributePtr(i).annotation != null)
        } else if (bcVersionAt8 && (StrPtr.equals(jstrRuntimeVisibleTypeAnnotations) || StrPtr.equals(jstrRuntimeInvisibleTypeAnnotations))) {
          // both RuntimeVisibleTypeAnnotations and RuntimeInvisibleTypeAnnotations attributes have similar structure
          if (!alreadySeen.add(StrPtr)) {
            error = js.format("Multiple %S attributes", StrPtr)
            return false
          }

          if (atype != codeAttrType) {
            attr.attributePtr(i).annotation = allocTypeAnnotations(Length.toInt)
            assert(attr.attributePtr(i).annotation != null)
          } else {
            // skip code attribute
            attr.attributePtr(i).info = skipAttribute(Length)
          }
        } else if (bcVersionAt15 && atype == methodAttrType && (StrPtr.equals(jstrRuntimeVisibleParameterAnnotations) || StrPtr.equals(jstrRuntimeInvisibleParameterAnnotations))) {
          // both RuntimeVisibleParameterAnnotations and RuntimeInvisibleParameterAnnotations attributes have similar structure
          if (!alreadySeen.add(StrPtr)) {
            error = js.format("Multiple %S attributes", StrPtr)
            return false
          }

          attr.attributePtr(i).annotation = allocParameterAnnotations(Length.toInt)
          assert(attr.attributePtr(i).annotation != null)
        } else if (bcVersionAt15 && StrPtr.equals(jstrAnnotationDefault)) {
          val Annot = allocAnnotationDefault(Length.toInt)
          assert(Annot != null)
          attr.attributePtr(i).annotation = Annot
        } else if (bcVersionAt7 && atype == classAttrType && StrPtr.equals(jstrBootstrapMethods)) {
          if (!alreadySeen.add(jstrBootstrapMethods)) {
            error = js.newJString("Multiple BootstrapMethods attributes")
            return false
          }

          val TLength = readUShort()
          if (maxBootstrapMethodAttrIndex >= TLength.toInt) {
            error = js.newJString("Short length of BootstrapMethods")
            return false
          }

          if (TLength > UShort(0)) {
            val BootstrapMethods = allocBootstrapMethods(Length.toInt, TLength.toInt)
            if (BootstrapMethods == null) {
              return false
            }
            attr.attributePtr(i).bootstrapMethods = BootstrapMethods
          }
        } else if (bcVersionAt8 && StrPtr.equals(jstrMethodParameters)) {
          if (!alreadySeen.add(jstrMethodParameters)) {
            error = js.newJString("Multiple MethodParameters attributes")
            return false
          }

          val TLength = readUByte()
          if (Length != (4 * TLength + 1).toUInt) {
            error = js.format("Invalid MethodParameters method attribute length %d in class file", Length.toInt)
            return false
          }

          if (TLength > 0) {
            val MethodParamters = allocMethodParameters(Length.toInt, TLength)
            if (MethodParamters == null) {
              return false
            }
            attr.attributePtr(i).methodParameters = MethodParamters
          }
        } else if (bcVersionAt15 && (StrPtr.equals(jstrSourceDebugExtension) || StrPtr.equals(jstrLocalVariableTypeTable)) || bcVersionAt16 && StrPtr.equals(jstrStackMapTable)) {
          // known predefined attributes, but no need to parse at the moment
          attr.attributePtr(i).info = skipAttribute(Length)
        } else { /* Unknown attribute, skip it */
          attr.attributePtr(i).info = skipAttribute(Length)
          attr.attributePtr(i).unknown = true
        }
      }
    }

    if (bcVersionAt7 && atype == classAttrType) {
      if (maxBootstrapMethodAttrIndex >= 0 && !alreadySeen.contains(jstrBootstrapMethods)) {
        error = js.newJString("Missing BootstrapMethods attribute")
        return false
      }
    }

    true
  }

  private def checkAccessFlags(af: Set32): Boolean = {
    if (af contains AccPublic) {
      (af & Set32.of(AccPrivate.toUByte, AccProtected.toUByte)) == Set32.empty
    } else if (af contains AccPrivate) {
      (af & Set32.of(AccPublic.toUByte, AccProtected.toUByte)) == Set32.empty
    } else if (af contains AccProtected) {
      (af & Set32.of(AccPublic.toUByte, AccPrivate.toUByte)) == Set32.empty
    } else {
      true
    }
  }

  private def getFields(fieldCount: UShort): Boolean = {
    val attr: AttributeData = new AttributeData()
    val InterfaceFields: Set32 = Set32.of(AccPublic.toUByte, AccStatic.toUByte, AccFinal.toUByte, AccSynthetic.toUByte)
    val InterfaceMustFields: Set32 = Set32.of(AccPublic.toUByte, AccStatic.toUByte, AccFinal.toUByte)
    val AllFlags: Set32 = Set32.of(AccPublic.toUByte, AccPrivate.toUByte, AccProtected.toUByte, AccStatic.toUByte, AccFinal.toUByte, AccVolatile.toUByte, AccTransient.toUByte, AccSynthetic.toUByte, AccEnum.toUByte)

    var Ok = false
    c.initFields(fieldCount.toInt)
    val names = mutable.HashSet.empty[NameAndSig]
    for (field <- c.fields) {
      val accessFlag = readUShort()
      field.accessFlag = accessFlag.toSet32 & AllFlags

      // check access flags
      if (!checkAccessFlags(field.accessFlag)) {
        error = js.format("Invalid field modifiers: %x", accessFlag.toUInt.toInt)
        return false
      }
      if ((field.accessFlag contains AccVolatile) && (field.accessFlag contains AccFinal)) {
        error = js.format("Invalid field modifiers: %x", accessFlag.toUInt.toInt)
        return false
      }
      if ((c.accessFlag contains AccInterface) && ((InterfaceMustFields & field.accessFlag) != InterfaceMustFields || (field.accessFlag &~ InterfaceFields) != Set32.empty)) {
        error = js.format("Invalid field modifiers: %x", accessFlag.toUInt.toInt)
        return false
      }

      val nameIndex = readUShort()
      field.nameIndex = nameIndex
      val name = obtainString(nameIndex, IsName = true) /*IsName:=*/
      if (name == null) {
        return false
      }
      if (!checkName(name, ji.nk_field)) {
        return false
      }

      val signatureIndex = readUShort()
      field.signatureIndex = signatureIndex
      val sig = obtainString(signatureIndex, IsName = false) /*IsName:=*/
      if (sig == null) {
        return false
      }
      if (!checkFieldDescriptor(sig)) {
        return false
      }

      val nameAndSig = NameAndSig(name, sig)
      if (names contains nameAndSig) {
        error = js.newJString("Duplicate fields found")
        return false
      }
      names += nameAndSig

      val attributeCount = readUShort()
      field.attributeCount = attributeCount

      Ok = readAttribute(attributeCount, fieldAttrType, -1, -1, attr)
      field.attribute = attr.attributePtr
      if (!Ok) {
        if (error == null) {
          error = js.newJString("Invalid field attributes")
        }
        return false
      }

      if (attr.hasSynthetic) {
        field.accessFlag += AccSynthetic.toUByte
      }

      // check static constant attributes
      if (attributeCount != UShort(0) && (field.accessFlag contains AccStatic)) {
        var seenCV = false
        for (j <- 0 until attributeCount.toInt) {
          val attribute = obtainString(field.attribute(j).nameIndex, IsName = false)
          if (attribute == null) {
            return false
          }
          if (attribute.equals(jstrCValue)) {
            if (seenCV) {
              error = js.newJString("Duplicate Constant attribute")
              return false
            }
            sig.charAt(0) match {
              case 'J' =>
                if (!checkCPEType(field.attribute(j).index, TagLong.toUShort)) {
                  return false
                }
              case 'F' =>
                if (!checkCPEType(field.attribute(j).index, TagFloat.toUShort)) {
                  return false
                }
              case 'D' =>
                if (!checkCPEType(field.attribute(j).index, TagDouble.toUShort)) {
                  return false
                }
              case 'B' |
                   'C' |
                   'I' |
                   'S' |
                   'Z' =>
                if (!checkCPEType(field.attribute(j).index, TagInteger.toUShort)) {
                  return false
                }
              case 'L' =>
                if (!sig.equals(js.jstrStringSig)) {
                  return false
                }
                if (!checkCPEType(field.attribute(j).index, TagString.toUShort)) {
                  return false
                }
            }
            seenCV = true
          }
        }
      }
    }
    true
  }

  private def verifyMethodModifiers(af: Set32, name: XString, isInterface: Boolean): Boolean = {
    val InterfaceBadFlags: Set32 = Set32.of(AccProtected.toUByte, AccFinal.toUByte, AccSynchronized.toUByte, AccNative.toUByte)
    val ConstructorFlags: Set32 = Set32.of(AccPublic.toUByte, AccPrivate.toUByte, AccProtected.toUByte, AccVarargs.toUByte, AccStrict.toUByte, AccSynthetic.toUByte)

    if (!needVerify) {
      return true
    }

    if (!checkAccessFlags(af)) {
      error = js.format("Invalid method modifiers: %x", af.toUInt.toInt)
      return false
    }

    if (af contains AccAbstract) {
      if ((af & Set32.of(AccFinal.toUByte, AccNative.toUByte, AccPrivate.toUByte, AccStatic.toUByte)) != Set32.empty || bcVersionAt15 && (af & Set32.of(AccStrict.toUByte, AccSynchronized.toUByte)) != Set32.empty) {
        error = js.format("Invalid method modifiers: %x", af.toUInt.toInt)
        return false
      }
    }

    if (isInterface) {
      if ((af & InterfaceBadFlags) != Set32.empty || (af contains AccPublic) == (af contains AccPrivate) || !bcVersionAt8 && !(af contains AccAbstract)) {
        error = js.format("Invalid interface method modifiers: %x", af.toUInt.toInt)
        return false
      }
    }

    if (name.equals(js.jstrInit) && (af &~ ConstructorFlags) != Set32.empty) {
      error = js.format("Invalid constructor modifiers: %x", af.toUInt.toInt)
      return false
    }

    true
  }

  private def getMethods(methodCount: UShort): Boolean = {
    val attr: AttributeData = new AttributeData()
    val AllFlags: Set32 = Set32.of(AccPublic.toUByte, AccPrivate.toUByte, AccProtected.toUByte, AccStatic.toUByte, AccFinal.toUByte, AccSynchronized.toUByte, AccBridge.toUByte, AccVarargs.toUByte, AccNative.toUByte, AccAbstract.toUByte, AccStrict.toUByte, AccSynthetic.toUByte)
    val ClinitFlags: Set32 = Set32.of(AccStatic.toUByte, AccStrict.toUByte)

    var Ok = false
    c.initMethods(methodCount.toInt)
    val names = mutable.HashSet.empty[NameAndSig]
    for (method <- c.methods) {
      val accessFlag = readUShort()
      method.accessFlag = accessFlag.toSet32 & AllFlags

      val nameIndex = readUShort()
      method.nameIndex = nameIndex

      val name = obtainString(nameIndex, IsName = true) /*IsName:=*/
      if (name == null) {
        return false
      }
      if (!checkName(name, ji.nk_method)) {
        return false
      }

      val signatureIndex = readUShort()
      method.signatureIndex = signatureIndex
      val sig = obtainString(signatureIndex, IsName = false) /*IsName:=*/
      if (sig == null) {
        return false
      }

      if (name.equals(js.jstrClinit) && sig.equals(js.jstrVoidMethodSig)) {
        if (bcVersionAt7) {
          // since Java 7, <clinit> must be static in order to qualify as class initializer
          if (method.accessFlag contains AccStatic) {
            // The value of class initializer access flags is ignored except for the setting of the STRICT flag.
            method.accessFlag = method.accessFlag & ClinitFlags
          }
        } else {
          // JVM ignores all flags of clinit except static
          method.accessFlag = Set32.of(AccStatic.toUByte)
        }
      } else if (!verifyMethodModifiers(method.accessFlag, name, c.accessFlag contains AccInterface)) {
        return false
      }

      val NP = checkMethodDescriptor(sig, method.accessFlag contains AccStatic)
      if (NP < 0) {
        return false
      }

      val nameAndSig = NameAndSig(name, sig)
      if (names contains nameAndSig) {
        error = js.newJString("Duplicate methods found")
        return false
      }
      names += nameAndSig

      val attributeCount = readUShort()
      method.attributeCount = attributeCount
      Ok = readAttribute(attributeCount, methodAttrType, NP, -1, attr)
      method.attribute = attr.attributePtr
      if (!Ok) {
        if (error == null) {
          error = js.format("Invalid method attributes")
        }
        return false
      }

      if (attr.hasSynthetic) {
        method.accessFlag += AccSynthetic.toUByte
      }

      var seenCode = false
      var seenExcep = false
      if (attributeCount != UShort(0)) {
        for (j <- 0 until attributeCount.toInt) {
          val attribute = obtainString(method.attribute(j).nameIndex, IsName = false)
          if (attribute == null) {
            return false
          }
          if (attribute.equals(jstrCodeName)) {
            if (seenCode) {
              error = js.format("Duplicate Code attribute")
              return false
            }
            if ((method.accessFlag & Set32.of(AccNative.toUByte, AccAbstract.toUByte)) != Set32.empty) {
              return false
            }
            seenCode = true
          } else if (attribute.equals(jstrException)) {
            if (seenExcep) {
              error = js.format("Duplicate Exception attribute")
              return false
            }
            seenExcep = true
          }
        }
      }

      if (!seenCode && (method.accessFlag & Set32.of(AccNative.toUByte, AccAbstract.toUByte)) == Set32.empty) {
        error = js.newJString("No Code attribute")
        return false
      }
    }
    true
  }

  private def checkClassAccessFlags(af: Set32): Boolean = {
    val InterfaceFlags: Set32 = Set32.of(AccPublic.toUByte, AccInterface.toUByte, AccAbstract.toUByte, AccSuper.toUByte, AccAnnotation.toUByte, AccSynthetic.toUByte)

    if (af contains AccInterface) {
      if ((af &~ InterfaceFlags) != Set32.empty) {
        return false
      }

      if (!(af contains AccAbstract)) {
        return false
      }

      if ((af contains AccSuper) && bcVersionAt15) {
        return false
      }
    } else {
      if ((af contains AccFinal) && (af contains AccAbstract)) {
        return false
      }

      if ((af contains AccAnnotation) && bcVersionAt15) {
        return false
      }
    }

    true
  }

  private def initBCVersionsAt(C: PtrClassInfo): Unit = {
    bcVersionAt15 = C.versionMajor >= Java5BytecodeVersion.toUShort
    bcVersionAt16 = C.versionMajor >= Java6BytecodeVersion.toUShort
    bcVersionAt7 = C.versionMajor >= Java7BytecodeVersion.toUShort
    bcVersionAt8 = C.versionMajor >= Java8BytecodeVersion.toUShort
  }

  /** Return pair of `loadIsOk` and `verifyError`. */
  def load(file: xfs.SymFile): (Boolean, Boolean) = {
    // if we should throw VerifyError
    // instead of ClassFormatError upon failure.
    var verifyError = true
    val attr: AttributeData = new AttributeData()
    val AllFlags: Set32 = Set32.of(AccPublic.toUByte, AccFinal.toUByte, AccSuper.toUByte, AccInterface.toUByte, AccAbstract.toUByte)
    val AllFlags15: Set32 = Set32.of(AccPublic.toUByte, AccFinal.toUByte, AccSuper.toUByte, AccInterface.toUByte, AccAbstract.toUByte, AccAnnotation.toUByte, AccSynthetic.toUByte, AccEnum.toUByte)

    //  ClassPtr^.Next := C;  (* Linked list *)
    //    PrintChars( "False magic number : " );  Int( C.Magic, 12 ); PrintLn;
    // workaround for SUN bug: enable abstract for interfaces before 1.6
    // check access flags
    // read and check this
    // read and check super
    try {
      var loadIsOk = false
      c = new PtrClassInfo()
      error = null
      setBytecode(file)

      c.bytecodeSize = fileLength

      dimOverflow = false
      verifyError = false

      c.magic = readUInt()
      if (c.magic != magic) {
        error = js.format("False magic number: %d", magic.toInt)
        return (false, false)
      }

      c.versionMinor = readUShort()
      c.versionMajor = readUShort()
      if (c.versionMajor < MinSupportedVersion.toUShort || c.versionMajor > MaxSupportedVersion.toUShort || c.versionMajor == MaxSupportedVersion.toUShort && c.versionMinor > MaxSupportedMinorVersion.toUShort) {
        error = js.format("Unsupported class-file version: %d.%d", c.versionMajor.toUInt.toInt, c.versionMinor.toUInt.toInt)
        return (false, false)
      }

      initBCVersionsAt(c)

      c.constantPoolCount = readUShort()
      maxBootstrapMethodAttrIndex = -1

      loadIsOk = getConstantPool
      if (!loadIsOk) {
        if (error == null) {
          error = js.newJString("Invalid constant pool")
        }
        return (false, false)
      }

      val AccessFlag = readUShort()
      c.accessFlag = if (!bcVersionAt15) {
        AccessFlag.toSet32 & AllFlags
      } else {
        AccessFlag.toSet32 & AllFlags15
      }

      if ((c.accessFlag contains AccInterface) && !bcVersionAt16) {
        c.accessFlag += AccAbstract.toUByte
      }

      if (!checkClassAccessFlags(c.accessFlag)) {
        error = js.format("Invalid class modifiers: %x", AccessFlag.toUInt.toInt)
        return (false, false)
      }

      c.thisClass = readUShort()
      loadIsOk = getCPEType(c.thisClass) == TagClass.toByte
      if (!loadIsOk) {
        error = js.format("Invalid this class index: %d", c.thisClass.toUInt.toInt)
        return (false, false)
      }

      c.superClass = readUShort()
      if (c.superClass == UShort(0)) {
        if (!c.constantPool(c.constantPool(c.thisClass.toInt).indexName.toInt).bufferPtr.equals(js.jstrObject)) {
          error = js.format("Invalid super class index: %d", c.superClass.toUInt.toInt)
          return (false, false)
        }
      } else {
        loadIsOk = getCPEType(c.superClass) == TagClass.toByte
        if (!loadIsOk) {
          error = js.format("Invalid super class index: %d", c.superClass.toUInt.toInt)
          return (false, false)
        }
        if (c.accessFlag contains AccInterface) {
          if (!c.constantPool(c.constantPool(c.superClass.toInt).indexName.toInt).bufferPtr.equals(js.jstrObject)) {
            error = js.newJString("Interfaces must have java.lang.Object as superclass")
            return (false, false)
          }
        }
      }

      c.interfaceCount = readUShort()
      loadIsOk = getInterface
      if (!loadIsOk) {
        if (error == null) {
          error = js.newJString("Invalid super interfaces reference")
        }
        return (loadIsOk, false)
      }

      val fieldCount = readUShort()
      loadIsOk = getFields(fieldCount)
      if (!loadIsOk) {
        if (error == null) {
          error = js.newJString("Invalid fields references")
        }
        return (false, false)
      }

      val methodCount = readUShort()
      loadIsOk = getMethods(methodCount)
      if (!loadIsOk) {
        if (error == null) {
          error = js.newJString("Invalid methods references")
        }
        return (false, false)
      }

      c.attributeCount = readUShort()
      loadIsOk = readAttribute(c.attributeCount, classAttrType, -1, -1, attr)
      c.attribute = attr.attributePtr
      if (!loadIsOk) {
        if (error == null) {
          error = js.newJString("Invalid class attributes")
        }
        return (loadIsOk, false)
      }

      if (attr.hasSynthetic) {
        c.accessFlag += AccSynthetic.toUByte
      }

      if (c.attributeCount != UShort(0)) {
        var seenSource = false
        var seenInnerClasses = false
        for (j <- 0 until c.attributeCount.toInt) {
          val StrPtr = obtainString(c.attribute(j).nameIndex, IsName = false)
          if (StrPtr == null) {
            return (false, false)
          }
          if (StrPtr.equals(jstrSourceFile)) {
            if (seenSource) {
              error = js.newJString("Duplicate source attribute")
              return (false, false)
            }
            seenSource = true
          } else if (StrPtr.equals(jstrInnerClasses)) {
            if (seenInnerClasses) {
              error = js.newJString("Duplicate inner classes attribute")
              return (false, false)
            }
            seenInnerClasses = true
          }
        }
      }
      (checkEnd(), false)
    } catch {
      case e: OutOfMemoryError => throw e
      case _: Throwable => (false, verifyError)
    }
  }

  def loadHead(File: xfs.SymFile): Boolean = {
    //  ClassPtr^.Next := C;  (* Linked list *)
    // read and check super
    try {
      var Ok = false
      c = new PtrClassInfo()
      setBytecode(File)
      error = null

      c.magic = readUInt()
      if (c.magic != magic) {
        return false
      }

      c.versionMinor = readUShort()
      c.versionMajor = readUShort()
      initBCVersionsAt(c)
      c.constantPoolCount = readUShort()
      Ok = getConstantPool
      if (!Ok) {
        return Ok
      }
      val AccessFlag = readUShort()
      c.accessFlag = AccessFlag.toSet32

      c.thisClass = readUShort()
      Ok = getCPEType(c.thisClass) == TagClass.toByte
      if (!Ok) {
        error = js.format("Invalid this class index: %d", c.thisClass.toUInt.toInt)
        return false
      }


      c.superClass = readUShort()
      if (c.superClass == UShort(0)) {
        if (!c.constantPool(c.constantPool(c.thisClass.toInt).indexName.toInt).bufferPtr.equals(js.jstrObject)) {
          error = js.format("Invalid supper class index: %d", c.superClass.toUInt.toInt)
          return false
        }
      } else {
        Ok = getCPEType(c.superClass) == TagClass.toByte
        if (!Ok) {
          error = js.format("Invalid supper class index: %d", c.superClass.toUInt.toInt)
          return false
        }
      }
      c.interfaceCount = readUShort()
      Ok = getInterface
      if (!Ok) {
        return false
      }
      Ok
    } catch {
      case _: Throwable => false
    }
  }
}
