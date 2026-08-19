/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.{Arch, DataAnnotationParsing}
import com.huawei.excelsior.common.Language.{JAVA, SCALA}
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.common.XString.xstr
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.abi.ABI
import com.huawei.excelsior.jet.compiler.abi.ABI.makeABISignature
import com.huawei.excelsior.jet.compiler.cangjie.{CHIRVTable, CangjieEnumInfo, CangjieSymLevelMaker}
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.{CONSTRUCTOR_NAME, isArraySliceConstructor}
import com.huawei.excelsior.jet.compiler.debug.info.{CompilationUnitInfo, DebugType, Language}
import com.huawei.excelsior.jet.compiler.driver.CompilationMode.{O1, O2}
import com.huawei.excelsior.jet.compiler.driver.{CompilationMode, ProjectLogic}
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.o2lib.opt.{O2Env, VZCModule}
import com.huawei.excelsior.jet.compiler.o2lib.be_386.opAttrsModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule.MethodSet.MethodId
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pcJCAModule as jca, pcNamesModule as pcNames}
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.{JBCPreprocessor, JavaClassParserModule as jcp}
import com.huawei.excelsior.jet.compiler.o2lib.tools.ExportIds
import com.huawei.excelsior.jet.compiler.o2lib.u.AttrAPIModule.FEXT
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule as xPDB
import com.huawei.excelsior.jet.compiler.o2lib.u.{CacheAPIModule, ClassID, Hashtable, ReplacementLibrary, AttrAPIModule as AttrAPI, JStringsModule as js, xcVersionModule as xcVersion, xiEnvModule as env, xiFilesModule as xfs, xmErrorsModule as xmErrors, xmZipModule as xmZip}
import com.huawei.excelsior.jet.compiler.o2lib.xjRTSModule as xjRTS
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.{FSModule as FS, MemoryManagementModule as mm}
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.NumOption.StackCheckByCallerAdditionalValueForO1Compiled
import com.huawei.excelsior.jet.compiler.options.StrOption.CangjiePackagesToO1
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.*
import com.huawei.excelsior.jet.compiler.symlevel.ConstValues.ConstValue
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{ClassTypeVariable, LocalTypeVariable, Unit as U, Void as V}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.{LightweightEnvironment, MethodTablesImpl, O2TypeProvider as TypeProvider}
import com.huawei.excelsior.jet.compiler.symlevel.indy.{CHIRDef, LambdaInfo, MethodHandle, ReferenceKind}
import com.huawei.excelsior.jet.compiler.symlevel.{CallConv, ClassType, GenericInfo, MethodSignature, MethodType, Signature, SignatureType, VersionedMarker, Method as SymMethod}
import com.huawei.excelsior.jet.compiler.types.CompiledType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ReferenceType, ClassType as RefClassType, InterfaceType as RefInterfaceType}
import com.huawei.excelsior.jet.util.ScalaCollections.{peekUntilNull, singleton}
import com.huawei.excelsior.jet.compiler.verifier.VerificationError
import com.huawei.excelsior.jet.compiler.verifier.VerificationError.ExceptionKind.*
import com.huawei.excelsior.jet.compiler.{Domain, RTConst, Stage, TypeProvider}
import com.huawei.excelsior.jet.util.{Closure, ScalaCollections}
import com.huawei.excelsior.o2j.runtime.*
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.util.{Set32, Set64, UByte}
import xscala.util.StringOps.r

import scala.annotation.targetName
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer

object pcOModule {

  /////////////////////////////////////////////////////////////////////////////
  // In the memory of XDS team

  /* Modifications:
     14-Mar-96 Ned    BUG006: otag_marked is appended.
     19-Mar-96 Ned    fn_ref & SYSTEM.REF are added.
     08-Apr-96 Ned    out_list: put "sym_case sym_end" for empty variant in a
                      record type.
     15-Apr-96 Ned    constants of proctype with value NIL are invented.
                      Fixes in rd_obj, wr_obj and gather_value.
     07-May-96 Enal   rewrite sort_list to use QuickSort.
     22-May-96 Ned    BYTE now have the same STRUCT as LOC
     23-May-96 Ned    symfile version VERS = 23
     24-May-96 Ned    BUG0128: inp_sym_file: browse parameter is added and used.
     27-Feb-97 VitVit read from/write to SYM-file DLL's name
     30-Mar-98 Sergic modifications for Java
     29-Feb-99 Kit    fully rewritten writing to and reading from symfile.
  */

  type XOTAG = UByte
  val XOTAG_SET = Set32
  type XOTAG_SET = Set32

  val xot_public                 : XOTAG = UByte(0)
  val xot_private                : XOTAG = UByte(1)
  val xot_protected              : XOTAG = UByte(2)
  val xot_static                 : XOTAG = UByte(3)
  val xot_final                  : XOTAG = UByte(4)
  val xot_synchron               : XOTAG = UByte(5)
  val xot_volatile               : XOTAG = UByte(6)
  val xot_transient              : XOTAG = UByte(7)
  val xot_native                 : XOTAG = UByte(8)
  val xot_interface              : XOTAG = UByte(9)
  val xot_abstract               : XOTAG = UByte(10)
  val xot_strictfp               : XOTAG = UByte(11)
  val xot_synthetic              : XOTAG = UByte(12)
  val xot_annotation             : XOTAG = UByte(13)
  val xot_enum                   : XOTAG = UByte(14)
  val xot_optimized_aggressively : XOTAG = UByte(15) // well-known controlled classes: runtime, rt.jar, compiler itself, ...
  val xot_systemclass            : XOTAG = UByte(16) // class is loaded by system classloader
  val xot_extension_classloader  : XOTAG = UByte(17) // class is loaded by extension classloader
  val xot_hierarchy_root         : XOTAG = UByte(18) // this class is a root of one of the hierarchies of objects
  val xot_xscala                 : XOTAG = UByte(19) // XScala type
  val xot_has_deferred_super     : XOTAG = UByte(20) // class has deferred superclass or superinterface
  val xot_jet_runtime            : XOTAG = UByte(21) // JET runtime class/interface
  /* the above "xot" constants must correspond to "mdf_*" declared in xjRTS.def */
  val xot_needcheckpairs         : XOTAG = UByte(22) // it has to be checked verify pairs for this class and all supers
  val xot_chir_def               : XOTAG = UByte(23) // class is defined in CHIR
  val xot_locale                 : XOTAG = UByte(24) // class is from locale component
  val xot_aj_managed             : XOTAG = UByte(25) // class is annotated with @Managed
  val xot_interp_internals       : XOTAG = UByte(26) // class is annotated with @InterpreterInternals
  val xot_aj_array               : XOTAG = UByte(27) // class is AJ array type
  val xot_cangjie                : XOTAG = UByte(28) // Cangjie language class
  val xot_record                 : XOTAG = UByte(29) // class is record
  val xot_java_annotated         : XOTAG = UByte(30) // @java-annotated class in Cangjie language
  val xot_bitcode_deferred       : XOTAG = UByte(31) // deferred class from Cangjie bitcode

  val xot_statini         : XOTAG = xot_native     // on ob_module: this class has non-empty <clinit>
  val xot_constr          : XOTAG = xot_enum       // methods: constructor, fields: enum
  val xot_deprecated      : XOTAG = xot_annotation // method/field is deprecated, types: annotation
  val xot_type_deprecated : XOTAG = xot_volatile   // methods: bridge, fields: volatile, types: deprecated
  val xot_constval        : XOTAG = xot_abstract   // methods, types: abstract, fields: has a ConstantValue bytecode attribute
  val xot_java_varargs    : XOTAG = xot_transient  // methods: varargs, fields: transient

  type CTAG = UByte
  val CTAG_SET = Set64
  type CTAG_SET = Set64

  val ctag_forced_o1_compiled         : CTAG = UByte( 0) // all class methods should be compiled with O1
  val ctag_UNUSED_1                   : CTAG = UByte( 1)
  val ctag_num_virtual                : CTAG = UByte( 2)
  val ctag_num_instance_layout        : CTAG = UByte( 3)
  val ctag_cangjie_enum               : CTAG = UByte( 4) // class is cangjie enum
  val ctag_is_namespace               : CTAG = UByte( 5) // class is specified with AJ annotation Namespace
  val ctag_is_thin_class              : CTAG = UByte( 6) // class is specified with AJ annotation Thin
  val ctag_recompile_class            : CTAG = UByte( 7) // class could be used from cache, but it was explicitly stated that we should recompile it
  val ctag_platformclass              : CTAG = UByte( 8) // Java SE Platform class
  val ctag_turbo_clinited             : CTAG = UByte( 9) // system class clinited during boostrap before loading any non-system class
  val ctag_runtime_reusable           : CTAG = UByte(10) // class is from profile which generated code we are going to reuse on subsequent compilation.*)
  val ctag_hide_deprecated_in_cp_mode : CTAG = UByte(11) // deprecated methods should be hided in reflection if Compact Profiles mode is enabled
  val ctag_in_inactive_environment    : CTAG = UByte(12) // class is specified with AJ annotation Environment and its environment is inactive
  val ctag_is_poly_thin_class         : CTAG = UByte(13) // class is specified with internal AJ annotation PolyThin
  val ctag_is_struct_class            : CTAG = UByte(14) // class is specified with AJ annotation Struct
  val ctag_is_value_class             : CTAG = UByte(15) // class is specified with AJ annotation Value
  val ctag_classdeferror              : CTAG = UByte(16) // verify error should be thrown during class definition
  val ctag_notverifiedcode            : CTAG = UByte(17) // verify error should be thrown during class preparation
  val ctag_absent                     : CTAG = UByte(18) // class is absent (not found by the compiler)
  val ctag_has_absent_super           : CTAG = UByte(19) // class cannot be compiled because it has one or more unavailable supers.
  val ctag_has_main                   : CTAG = UByte(20) // class has "public static void main" method
  val ctag_has_generics_info          : CTAG = UByte(21) // has Generic Signature attribute on class, its method, or field
  val ctag_has_annotations            : CTAG = UByte(22) // has Annotations attribute on class, its method or field
  val ctag_lambda_class               : CTAG = UByte(23) // generated lambda class
  val ctag_is_bootstrap               : CTAG = UByte(24) // class is specified with AJ annotation Bootstrap
  val ctag_vcf_excluded               : CTAG = UByte(25) // VCF should not be generated for the class
  val ctag_no_java_class              : CTAG = UByte(26) // class that cannot reference Java classes (lightweight runtime build)*)
  val ctag_is_non_bootstrap           : CTAG = UByte(27) // type is always prepared, don't mark as bootstrap root
  val ctag_absent_symimport           : CTAG = UByte(28) // class is imported from sym-file of another class and absent
  val ctag_contains_managed           : CTAG = UByte(29) // class contains methods with managed calling convention
  val ctag_is_ajextended              : CTAG = UByte(30) // class is specified with AJ annotation AJExtended
  val ctag_has_method_fext            : CTAG = UByte(31)
  val ctag_synthetic                  : CTAG = UByte(32) // class is synthetic
  val ctag_is_anonymous               : CTAG = UByte(33) // class is anonymous and has host class
  val ctag_is_cangjie_package         : CTAG = UByte(34) // class is cangjie package
  val ctag_cangjie_lambda             : CTAG = UByte(35) // class is Lambda, LambdaCommon or Auto_Env
  val ctag_is_varray                  : CTAG = UByte(36) // class is VArray
  val ctag_is_generic                 : CTAG = UByte(37) // class is cangjie and generic
  val ctag_evacuated_type             : CTAG = UByte(38) // class should be stack-allocated by EvacuateAnalysis
  val ctag_has_outer_class            : CTAG = UByte(39) // class has outer class
  val ctag_cangjie_array              : CTAG = UByte(40) // class is cangjie array
  val ctag_num_static_layout          : CTAG = UByte(41)

  type ETAG = UByte
  val ETAG_SET = Set32
  type ETAG_SET = Set32

  val etag_import          : ETAG = UByte(0)
  val etag_methods         : ETAG = UByte(1)
  val etag_fields          : ETAG = UByte(2)
  val etag_attributes      : ETAG = UByte(3)
  val etag_inner_classes   : ETAG = UByte(4)
  val etag_imt_slots       : ETAG = UByte(5)
  val etag_debug_type      : ETAG = UByte(6)
  val etag_str_table       : ETAG = UByte(7)

  val etag_first = etag_import
  val etag_last = etag_str_table

  implicit def typeProvider: TypeProvider = LightweightEnvironment.getInstance

  // Type ImportTypesIndexes

  class ImportEntry(val name: pcNames.NAME, private var _class: Class = null) {
    def resolveFrom(host: Class): Class = {
      if (_class == null) {
        _class = findClassByNameObject(name)
        if (_class == null) {
          _class = if (symExists(name)) {
            makeClassFromSymFile(name)
          } else {
            assert(pcNames.isAbsent(name))
            makeAbsentClass(name, importedFromSym = true)
          }
        }

        assert(_class != null)

        if (host.resolvedClasses == null) {
          host.resolvedClasses = mutable.HashMap.empty[XString, Class]
        }
        host.resolvedClasses(name.name) = _class
      }
      _class
    }
  }

  object CangjieClass {
    private val SEALED_MODIFIER = UByte(16)

    private[pcOModule] val MODIFIERS = Set32.of(
      xjRTS.mdf_public.toUByte, xjRTS.mdf_final.toUByte,
      xjRTS.mdf_abstract.toUByte, xjRTS.mdf_interface, SEALED_MODIFIER)
  }

  final class Class(_mno: Int) extends pc.SymType.Reference(_mno) {
    // Completion mechanism fields
    private[pcOModule] var elementsRequireCompletion: ETAG_SET = ETAG_SET.empty  // class IR elements which require completion (currently unavailable)
    private[pcOModule] var symio: SymIO = _
    private[pcOModule] var cached: Boolean = false
    private[pcOModule] var lastUsageTime: Int = curSession

    // Plain fields not written in sym-file
    private[pcOModule] var packageName: XString = _                             // "com/xxx/foo" or ""
    private[pcOModule] var resolvedClasses: mutable.HashMap[XString, Class] = _ // hashtable of resolved classes
    private[pcOModule] var from: Class = _                                      // for real absent classes, "from" means a class for which this class is absent
    private[pcOModule] var bcSourceName: XString = _
    private var _fileDescriptor: xfs.FileDescriptor = _                         // source file descriptor -- TODO: drop after parsing
    private var _classInfo: jcp.PtrClassInfo = _                                // bytecode class info -- TODO: drop after parsing (except constant pool)

    // Plain field written in sym-file (sl_zero)
    private[pcOModule] var ctags: CTAG_SET = CTAG_SET.empty            // class tags
    private[pcOModule] var accflags: XOTAG_SET = XOTAG_SET.empty       // .class file "AccessFlag" field & extra tags
    private[pcOModule] var srctags: XOTAG_SET = XOTAG_SET.empty        // InnerClassAccessFlags field from "InnerClasses" attribute
    private[pcOModule] var _size: Int = -1                             // size of an object of the class
    private[pcOModule] var _alignment: Int = -1                        // alignment of an object of the class
    private[pcOModule] var vmtSize: Int = -1                           // vmt/imt length (# of slots)
    private[pcOModule] var level: Int = 0                              // inheritance level of the class

    private[pcOModule] var superclass: RefClassType = _
    private[pcOModule] val interfaces = ArrayBuffer.empty[RefInterfaceType]
    private[pcOModule] var _cangjiePackage: Class = _

    private[pcOModule] val importList = new ArrayBuffer[ImportEntry]
    private[pcOModule] var persistentImportNum: Int = 0                            // number of persistent import entries

    // etag_methods:
    private[pcOModule] var methods = ArrayBuffer.empty[Method] // static & instance methods in original source file order

    // etag_fields:
    private[pcOModule] var fields = ArrayBuffer.empty[Field] // static & instance fields in original source file order

    // etag_attributes: FEXTs

    // etag_inner_classes:
    private[pcOModule] var innerClasses: ArrayBuffer[InnerClass] = _ // list of inner classes

    // etag_imt_slots:
    private[pcOModule] var imtSlots: Array[Int] = _ // slots of each super interface imt in vmt

    // etag_debug_type:
    private[pcOModule] var llvmDebugType: DebugType = _

    // etag_str_table:
    private[pcOModule] var strTable: StringTable = _ // class string table


    ///////////////////////////////////////////////////////////////////////////
    // Attributes

    override def addFEXT(fext: FEXT, kind: Byte): Unit = { onGet(this, etag_attributes); super.addFEXT(fext, kind) }
    override def getFEXT[F <: FEXT](kind: Byte): F     = { onGet(this, etag_attributes); super.getFEXT(kind) }


    ///////////////////////////////////////////////////////////////////////////
    // Tags setters/getters

    def markAsForcedO1Compiled()          : Unit = ctags += ctag_forced_o1_compiled
    def markAsVirtualNumbersNumerated()   : Unit = ctags += ctag_num_virtual
    def markAsInstanceLayoutNumerated()   : Unit = ctags += ctag_num_instance_layout
    def markAsStaticLayoutNumerated()     : Unit = ctags += ctag_num_static_layout
    def markAsNamespace()                 : Unit = ctags += ctag_is_namespace
    def markAsThinClass()                 : Unit = ctags += ctag_is_thin_class
    def markAsRequiredRecompilation()     : Unit = ctags += ctag_recompile_class
    def markAsPlatformClass()             : Unit = ctags += ctag_platformclass
    def markAsTurboClinited()             : Unit = ctags += ctag_turbo_clinited
    def markAsRuntimeReusable()           : Unit = ctags += ctag_runtime_reusable
    def markAsHideDeprecatedInCPMode()    : Unit = ctags += ctag_hide_deprecated_in_cp_mode
    def markAsInInactiveEnvironment()     : Unit = ctags += ctag_in_inactive_environment
    def markAsPolyThinClass()             : Unit = ctags += ctag_is_poly_thin_class
    def markAsStructClass()               : Unit = ctags += ctag_is_struct_class
    def markAsValueClass()                : Unit = ctags += ctag_is_value_class
    def markAsClassDefError()             : Unit = ctags += ctag_classdeferror
    def markAsNotVerifiedCode()           : Unit = ctags += ctag_notverifiedcode
    def markAsAbsent()                    : Unit = ctags += ctag_absent
    def markAsHasAbsentSuper()            : Unit = ctags += ctag_has_absent_super
    def markAsHasMain()                   : Unit = ctags += ctag_has_main
    def markAsHasGenericsInfo()           : Unit = ctags += ctag_has_generics_info
    def markAsHasAnnotations()            : Unit = ctags += ctag_has_annotations
    def markAsLambdaClass()               : Unit = ctags += ctag_lambda_class
    def markAsBootstrap()                 : Unit = ctags += ctag_is_bootstrap
    def markAsVCFExcluded()               : Unit = ctags += ctag_vcf_excluded
    def markAsNoJavaClass()               : Unit = ctags += ctag_no_java_class
    def markAsNonBootstrap()              : Unit = ctags += ctag_is_non_bootstrap
    def markAsAbsentSymImport()           : Unit = ctags += ctag_absent_symimport
    def markAsContainsManaged()           : Unit = ctags += ctag_contains_managed
    def markAsAJExtended()                : Unit = ctags += ctag_is_ajextended
    def markAsHasMethodFEXT()             : Unit = ctags += ctag_has_method_fext
    def markAsSynthetic()                 : Unit = ctags += ctag_synthetic
    def markAsAnonymous()                 : Unit = ctags += ctag_is_anonymous
    def markAsCangjiePackage()            : Unit = ctags += ctag_is_cangjie_package
    def markAsCangjieLambdaBaseClass()    : Unit = ctags += ctag_cangjie_lambda
    def markAsVArray()                    : Unit = ctags += ctag_is_varray
    def markAsUniversalGeneric()          : Unit = ctags += ctag_is_generic
    def markAsEvacuatedType()             : Unit = ctags += ctag_evacuated_type
    def markAsHasOuterClass()             : Unit = ctags += ctag_has_outer_class
    def markAsCangjieEnum()               : Unit = ctags += ctag_cangjie_enum

    def isForcedO1Compiled                : Boolean = ctags contains ctag_forced_o1_compiled
    def virtualNumbersAreNumerated        : Boolean = ctags contains ctag_num_virtual
    def instanceLayoutIsNumerated         : Boolean = ctags contains ctag_num_instance_layout
    def staticLayoutIsNumerated           : Boolean = ctags contains ctag_num_static_layout
    def isNamespace                       : Boolean = ctags contains ctag_is_namespace
    def isThinClass                       : Boolean = ctags contains ctag_is_thin_class
    def requiredRecompilation             : Boolean = ctags contains ctag_recompile_class
    def isPlatformClass                   : Boolean = ctags contains ctag_platformclass
    def isTurboClinited                   : Boolean = ctags contains ctag_turbo_clinited
    def isRuntimeReusable                 : Boolean = ctags contains ctag_runtime_reusable
    def isHideDeprecatedInCPMode          : Boolean = ctags contains ctag_hide_deprecated_in_cp_mode
    def isInInactiveEnvironment           : Boolean = ctags contains ctag_in_inactive_environment
    def isPolyThinClass                   : Boolean = ctags contains ctag_is_poly_thin_class
    def isStructClass                     : Boolean = ctags contains ctag_is_struct_class
    def isValueClass                      : Boolean = ctags contains ctag_is_value_class
    def isClassDefinitionError            : Boolean = ctags contains ctag_classdeferror
    def isNotVerifiedCode                 : Boolean = ctags contains ctag_notverifiedcode
    def isAbsent                          : Boolean = ctags contains ctag_absent
    def hasAbsentSuper                    : Boolean = ctags contains ctag_has_absent_super
    def hasMain                           : Boolean = ctags contains ctag_has_main
    def hasGenericsInfo                   : Boolean = ctags contains ctag_has_generics_info
    def hasAnnotations                    : Boolean = ctags contains ctag_has_annotations
    def isLambdaClass                     : Boolean = ctags contains ctag_lambda_class
    def isBootstrap                       : Boolean = ctags contains ctag_is_bootstrap
    def isVCFExcluded                     : Boolean = ctags contains ctag_vcf_excluded
    def isNoJavaClass                     : Boolean = ctags contains ctag_no_java_class
    def isNonBootstrap                    : Boolean = ctags contains ctag_is_non_bootstrap
    def isAbsentSymImport                 : Boolean = ctags contains ctag_absent_symimport
    def containsManaged                   : Boolean = ctags contains ctag_contains_managed
    def isAJExtended                      : Boolean = ctags contains ctag_is_ajextended
    def hasMethodFEXT                     : Boolean = ctags contains ctag_has_method_fext
    def isSynthetic                       : Boolean = ctags contains ctag_synthetic
    def isAnonymous                       : Boolean = ctags contains ctag_is_anonymous
    def isCangjiePackage                  : Boolean = ctags contains ctag_is_cangjie_package
    def isCangjieLambdaBaseClass          : Boolean = ctags contains ctag_cangjie_lambda
    def isVArray                          : Boolean = ctags contains ctag_is_varray
    def isUniversalGeneric                : Boolean = ctags contains ctag_is_generic
    def isEvacuatedType                   : Boolean = ctags contains ctag_evacuated_type
    def hasOuterClass                     : Boolean = ctags contains ctag_has_outer_class
    def isCangjieEnum                     : Boolean = ctags contains ctag_cangjie_enum

    def isInActiveEnvironment: Boolean = !isInInactiveEnvironment


    ///////////////////////////////////////////////////////////////////////////

    def nameObj: pcNames.NAME = pc.modules(mno).nameObj

    def name: XString = nameObj.name

    def getMangledName: XString = nameObj.getMangledName

    def getReadableName: XString = pc.modules(mno).getReadableName(need_class_name = false, need_full_sign = false)

    override def toString: String = name.toString

    def asCT: ClassType = symType.asInstanceOf[ClassType]

    def getThinInheritanceLevel: Int = {
      assert(isThinClass)
      getInheritanceLevel - 1
      // TODO We use -1 here because of poly-thins hierarchy structure: ThinType (a real root) -> PolyThinType -> ... -> Your poly-thin class
      // TODO So, we skip this one level for PolyThinType right now.
      // TODO Remove and just set correct inheritanceLevel for thin classes
    }

    def getManagedTypeHandleSizeForThinClass: Int = {
      assert(isThinClass)
      if (hasManagedMetaInformation) RTConst.InfectedTypeHandle.size else 0
    }

    def getEnclosingMethod: EnclosingMethod = {
      // used only in JavaDesc (TD writing)
      fextOption[EnclosingMethodFEXT](encmethtype).map(_.encmeth).orNull
    }

    def setEnclosingMethod(enClass: Class, methName: XString, methSig: XString): Unit = {
      addImport(enClass)
      val m = new EnclosingMethod(enClass, methName, methSig)
      val fext = new EnclosingMethodFEXT
      fext.encmeth = m
      addFEXT(fext, encmethtype)
    }

    def getTypeAnnotations(rtVisible: Boolean): jcp.PtrTypeAnnotationsAttr = {
      assert(this.hasAnnotations)
      val attrType = if (rtVisible) rtVisTypeAnnotType else rtInvisTypeAnnotType
      fextOption[AnnotationAttr](attrType).map(_.attr.asInstanceOf[jcp.PtrTypeAnnotationsAttr]).orNull
    }

    def setTypeAnnotations(attr: jcp.PtrTypeAnnotationsAttr, rtVisible: Boolean): Unit = {
      val attrType = if (rtVisible) rtVisTypeAnnotType else rtInvisTypeAnnotType
      val a = new AnnotationAttr
      a.attr = attr
      addFEXT(a, attrType)
      this.markAsHasAnnotations()
    }

    def getAnnotations(rtVisible: Boolean): jcp.PtrAnnotationsAttr = {
      assert(this.hasAnnotations)
      val attrType = if (rtVisible) rtVisAnnotType else rtInvisAnnotType
      fextOption[AnnotationAttr](attrType).map(_.attr.asInstanceOf[jcp.PtrAnnotationsAttr]).orNull
    }

    def setAnnotations(attr: jcp.PtrAnnotationsAttr, rtVisible: Boolean): Unit = {
      val attrType = if (rtVisible) rtVisAnnotType else rtInvisAnnotType
      val a = new AnnotationAttr
      a.attr = attr
      addFEXT(a, attrType)
      this.markAsHasAnnotations()
    }

    def addCJAnnotationFactory(factoryMethod: Method): Unit = {
      factoryMethod.markAsCangjieAnnotationFactory()
      val fext = newMethodFEXT(this, name, factoryMethod.name, factoryMethod.getSignature, null, allowNotFound = true)
      addFEXT(fext, cjAnnotation)
    }

    def getCJAnnotationFactory: Method = {
      fextOption[MethodFEXT](cjAnnotation).map(_.getMethod).orNull
    }

    def addGenericInfo(info: GenericInfo): Unit = {
      assert(isUniversalGeneric)
      val fext = newGenericInfoFEXT(info.constraints)
      addFEXT(fext, genericInfo)
    }

    def getGenericInfo: GenericInfo = {
      assert(isUniversalGeneric)
      fextOption[GenericInfoFEXT](genericInfo).map(_.get).get
    }

    def setCangjieArrayElementType(elemType: SignatureType): Unit = {
      assert(isCangjieArray || isVArray || symType.isArraySlice)
      addImport(elemType)
      addFEXT(newSignatureTypeFEXT(elemType), cangjieArrayElementType)
    }

    def getCangjieArrayElementType: SignatureType = {
      fextOption[SignatureTypeFEXT](cangjieArrayElementType).map(_.sig).orNull
    }

    def setCangjieBoxValueType(baseType: SignatureType): Unit = {
      assert(isCangjieBox)
      addImport(baseType)
      addFEXT(newSignatureTypeFEXT(baseType), cangjieBoxValueType)
    }

    def getCangjieBoxValueType: SignatureType = {
      fextOption[SignatureTypeFEXT](cangjieBoxValueType).map(_.sig).orNull
    }

    def setCHIRVTable(vtable: CHIRVTable): Unit = {
      if (!hasFEXT(chirVTable)) {
        // TODO: stop serializing everything
        for (ed <- vtable.extDefs) {
          addImport(ed.extType)
          for (m <- ed.funcTable) {
            addImport(m.sig)
            m.genericParams.foreach(addImport)
            for (i <- m.impl) {
              addImport(typeToO2Class(i.getDeclaringClass))
              addImport(i.getSignature)
            }
            addImport(m.originalSig)
            addImport(m.instantiatedRefType)
            addImport(m.instantiatedReturnType)
          }
        }
        addFEXT(CHIRVTableFEXT(vtable), chirVTable)
      }
    }

    def getCHIRVTable: Option[CHIRVTable] = {
      fextOption[CHIRVTableFEXT](chirVTable).map(_.getVTable)
    }

    def setCangjieEnumInfo(info: CangjieEnumInfo): Unit = {
      assert(isCangjieEnum)
      if (!hasFEXT(cangjieEnumInfo)) {
        // TODO: stop serializing everything
        for (c <- info.constructors) {
          c.params.foreach(addImport)
        }
        addFEXT(CangjieEnumInfoFEXT(info), cangjieEnumInfo)
      }
    }

    def getCangjieEnumInfo: Option[CangjieEnumInfo] = {
      fextOption[CangjieEnumInfoFEXT](cangjieEnumInfo).map(_.info)
    }

    def setCangjieExtendInfo(info: SignatureType): Unit = {
      if (!hasFEXT(cangjieExtendInfo)) {
        // TODO: stop serializing everything
        addImport(info)
        addFEXT(newSignatureTypeFEXT(info), cangjieExtendInfo)
      }
    }

    def getCangjieExtendInfo: Option[SignatureType] = {
      fextOption[SignatureTypeFEXT](cangjieExtendInfo).map(_.sig)
    }

    def addLambdaInfo(info: LambdaInfo): Unit = {
      assert(isLambdaClass)

      // Note: Only instantiatedMethodType needs explicit import here:
      //   - capturingClass is imported as hostClass.
      //   - samClass is imported as superinterface
      //   - samMethodType is imported from overriding corresponding samMethod
      //   - types from impl.member are imported in newMethodFEXT
      addImport(info.instantiatedMethodType)

      val fext = newLambdaInfoFEXT(this, info)
      addFEXT(fext, lambdaInfo)
    }

    def getLambdaInfo: LambdaInfo = {
      assert(isLambdaClass)
      fextOption[LambdaInfoFEXT](lambdaInfo).map(_.get).get
    }

    def getGenericSignature: XString = {
      assert(this.hasGenericsInfo)
      fextOption[StrFEXT](sigtype).map(_.str).orNull
    }

    def setGenericSignature(sig: XString): Unit = {
      addFEXT(new StrFEXT(sig), sigtype)
      this.markAsHasGenericsInfo()
    }

    def copyVerifyErrorFrom(src: Class): Unit = {
      assert(!src.isVerifiable)
      val v = src.getVerifyError
      if (src.isClassDefinitionError) {
        this.setClassDefinitionError0(v)
      } else {
        assert(!src.isVerifiable)
        this.setNotVerifiedCodeError0(v)
      }
    }

    def setNotVerifiedCodeError(excep: VerificationError.ExceptionKind, err: XString): Unit = {
      this.setNotVerifiedCodeError0(newVerifyError(excep, err))
    }

    def setClassDefinitionError(excep: VerificationError.ExceptionKind, err: XString): Unit = {
      this.setClassDefinitionError0(newVerifyError(excep, err))
    }

    def setNotVerifiedCodeError0(vererr: VerifyError): Unit = {
      env.info.print("\\n-- VerifyError = %S\\n", vererr.errmsg)
      if (!this.isVerifiable) {
        return
      }
      this.markAsNotVerifiedCode()
      this.exclModifier(xot_statini)
      this.dropElementsForBadClass()
      this.setVerifyError0(vererr)
    }

    def setClassDefinitionError0(vererr: VerifyError): Unit = {
      if (isClassDefinitionError) {
        return
      }
      if (isCangjieType) {
        shouldNotReachHere(s"Verification Error: ${vererr.errmsg}")
      }
      markAsClassDefError()
      exclModifier(xot_statini)
      dropElementsForBadClass()
      setVerifyError0(vererr)
    }

    def getVerifyError: VerifyError = {
      fextOption[VerErrFEXT](vererrtype).map(_.err).orNull
    }

    def setVerifyError0(vererr: VerifyError): Unit = {
      val vext = new VerErrFEXT
      vext.err = vererr
      addFEXT(vext, vererrtype)
    }

    def setAbsentSuper(absentSuper: Class): Unit = {
      markAsHasAbsentSuper()
      srctags = XOTAG_SET.empty
      accflags = XOTAG_SET.empty

      if (!containsImport(absentSuper)) {
        addImport(absentSuper)
      }

      addFEXT(new ClassFEXT(absentSuper), absentsupertype)

      dropElementsForBadClass()
    }

    //--------- Absent supers support -------------
    def getAbsentSuper: Class = {
      assert(this.hasAbsentSuper)
      getFEXT[ClassFEXT](absentsupertype).class0
    }

    def getCodeSource: XString = {
      // TODO: JET-7463
      val fd = xfs.sys.lookup(FS.addExt(this.name, env.config.equation("JAVABC")))

      fd match {
        case fd: xmZip.FileDescriptor =>
          var cs = codesources
          while (cs != null) {
            if (FS.HOST.isSameFile(cs.jar, fd.zname)) {
              return FS.cutPath(FS.HOST.fromPlatform(cs.jar))
            }
            cs = cs.next
          }
        case _ =>
      }
      null
    }

    def checkImport(): Unit = {
      def checkImportEntry(imp: Class): Unit = {
        if (imp != null && (imp ne this) && !containsImport(imp)) {
          env.errors.fault(ErrMsg209, imp.name, name)
        }
      }

      def checkImportType(t: pc.SymType): Unit = checkImportEntry(getCoreClassType(t))

      for (f <- declaredFields) {
        foreachO2TypeInSignature(f.sig)(checkImportType)
      }

      for (m <- declaredMethods) {
        m.getThrows foreach checkImportEntry
        foreachO2TypeInSignature(m.getSignature)(checkImportType)
      }
    }

    def getClassSymRef(imp: Class): Int = {
      if (imp == null) {
        0
      } else {
        val n = this.findImportEntryIndex(imp)
        if (n < 0) {
          -1
        } else {
          n + 1
        }
      }
    }

    def resolveClassSymRef(n: Int): Class = {
      if (n == 0) {
        null
      } else {
        this.getImportedClassByIndex0(n - 1)
      }
    }

    def hasSymReader: Boolean = this.symio != null && this.symio.isInstanceOf[SymReader]

    def makeSymWriter(): Unit = {
      assert(symio == null)
      symio = new SymWriter(this)
    }

    /** Looks for method in the class and its superclasses.
      *
      * @param sig can be null, which means that search should be performed using method name only.
      */
    def findMethod(name: XString, sig: MethodSignature): Method = {
      var c = this

      while (c != null) {
        val m = c.findLocalMethod(name, sig)
        if (m != null) {
          return m
        }

        c = c.getSuperClassO2
      }

      null
    }

    /** Looks for method among methods declared in the current class.
      *
      * @param sig can be null, which means that search should be performed using method name only.
      */
    def findLocalMethod(name: XString, sig: MethodSignature = null): Method = {
      // TODO: remove copy-paste with ClassType.findDeclaredMethodOrNull
      for (m <- declaredMethods) {
        if (m.name == name && (sig == null || m.getSignature == sig)) {
          return m
        }
      }
      null
    }

    /** Looks for field in the class, its superclasses, and superinterfaces.
      *
      * @param sig can be null, which means that search should be performed using field name only.
      */
    def findField(name: XString, sig: SignatureType): Field = {
      var c = this

      while (c != null) {
        var f = c.findLocalField(name, sig)
        if (f != null) {
          return f
        }

        // search in superinterfaces, after that in base class
        val found = c.getSuperInterfacesO2 map (_.findField(name, sig)) find (_ != null)
        if (found.nonEmpty) {
          return found.get
        }

        c = c.getSuperClassO2
      }
      null
    }

    /** Looks for field among fields declared in the current class.
      * @param sig can be null, which means that search should be performed using field name only.
      */
    def findLocalField(name: XString, sig: SignatureType): Field = {
      // TODO: remove copy-paste with ClassType.findDeclaredFieldOrNull
      for (f <- declaredFields) {
        if (f.name == name && (sig == null || f.getSignature == sig)) {
          return f
        }
      }
      null
    }

    def getStringTable: StringTable = {
      if (this.strTable == null && !this.isUnloadable) {
        this.initStringTable()
      }
      this.strTable
    }

    def initStringTable(): Unit = {
      assert(!this.isUnloadable)

      val table = new StringTable(this.mno)
      val stringTypeID = if (languagePack.supports(JAVA)) {
        ClassID.String
      } else if (languagePack.supports(SCALA)) {
        ClassID.XScalaString
      } else {
        ClassID.AJString
      }
      table.sig = SignatureType.JavaArray(SignatureType.JBCReference(stringTypeID.name.toString))

      this.strTable = table
    }

    /** Check whether "c" inherits (implements or extends) "super" */
    def isSubType(super0: Class): Boolean = {
      if (super0.isInterface) {
        this.isInheritedFromInterface(super0)
      } else if (!this.isInterface) {
        this.isSubClass(super0)
      } else if (this.isAJManagedType || this.isCangjieType) {
        // an interface cannot inherit from any class except Object (for Java) or AJObject (for AJ)
        TypeProvider.isAJObject(super0)
      } else if (this.isXScalaType) {
        TypeProvider.isXScalaAnyRef(super0)
      } else {
        TypeProvider.isJavaLangObject(super0)
      }
    }

    /** For interface "super" and class or interface "c"
      * Check if "c" is a subtype of "super"
      */
    def isInheritedFromInterface(super0: Class): Boolean = {
      assert(super0.isInterface)
      def supers(t: Class) = {
        // Can't access supertypes of unloadable type.
        if (t.isUnloadableOnClassDefStage || super0.isUnloadableOnClassDefStage) {
          Iterator.empty
        } else {
          Option(t.getSuperClassO2).iterator ++ t.getSuperInterfacesO2
        }
      }
      val classes = mutable.HashSet.empty[Class]
      Closure.collect(classes, Seq(this))(supers)
      classes contains super0
    }

    /** Check if "c" is a subclass of "super" */
    def isSubClass(super0: Class): Boolean = {
      var type0 = this
      if (type0.isUnloadableOnClassDefStage || super0.isUnloadableOnClassDefStage) {
        return false
      }

      if (type0 eq super0) {
        return true
      }

      if (type0.isInterface) {
        // according JVM spec Object is super class of any interface
        if (type0.isXScalaType) {
          return TypeProvider.isXScalaAnyRef(super0)
        } else {
          return TypeProvider.isJavaLangObject(super0) // TODO MANAGED-KIT this will become false with our own interfaces
        }
      } else if (super0.isInterface) {
        // interface cannot be super class of any class/interface
        return false
      }

      while ((type0 ne super0) && type0 != null) {
        type0 = type0.getSuperClassO2
      }
      type0 != null
    }

    /** @return true if we don't need to perform 'clinit' of this class */
    def isPreclinited: Boolean = {
      if (isStandalone) {
        return true
      }
      
      if (hasDeferredSuper0) {
        return false
      }

      if (noOptimizeClinits) {
        return false
      }

      var cls = this
      while (cls != null) {
        if (cls.needClinit()) {
          return false
        }

        assert(cls.isInterface || cls.getSuperClassO2 != null || cls.isHierarchyRoot || cls.isRecord || cls.isCangjiePackage || cls.isValueClass || cls.isNamespace)

        cls = cls.getSuperClassO2
      }

      !this.hasSuperInterfacesWithDefaults(withClinit = true)
    }

    def hasSuperInterfacesWithDefaults(withClinit: Boolean): Boolean = {
      if (this.isClassDefinitionError) {
        return false
      }

      if (this.getSuperClassO2 != null) {
        val super0 = this.getSuperClassO2
        if (super0.hasSuperInterfacesWithDefaults(withClinit)) {
          return true
        }
      }

      getSuperInterfacesO2 exists { i =>i.hasDefaults(withClinit) || i.hasSuperInterfacesWithDefaults(withClinit) }
    }

    def hasDefaults(withClinit: Boolean): Boolean =
      isInterface && (!withClinit || needClinit()) && (declaredMethods exists (m => !m.isAbstract && !m.isStatic))

    /** @return True if this class requires run-time class initialization;
      *         Otherwise, there should be no clinit call for this class in generated code.
      */
    def needClinit(): Boolean = {
      if (TypeProvider.isJavaLangObject(this)) {
        return false
      }
      // TODO: filter classes with empty clinit (clinit with only return)
      hasClinit || !isVerifiable || needVerify
    }

    /** @return `true` iff this class defines clinit method.
      * @note on x86 returns false for empty clinits despite their presence in original bytecode (which is wrong).
      */
    def hasClinit: Boolean = {
      this.srctags contains xot_statini
    }

    def addInnerClass(clazz: Class, accessFlags: Set32): Unit = {
      addInnerClass0(clazz, accessFlags)
      addImport(clazz)
    }

    def addInnerClass0(clazz: Class, accessFlags: Set32): Unit = {
      onGet(this, etag_inner_classes)
      if (this.innerClasses == null) {
        this.innerClasses = new ArrayBuffer[InnerClass]
      }
      this.innerClasses += newInnerClass(clazz, accessFlags)
    }

    def getInnerClasses: ArrayBuffer[InnerClass] = {
      onGet(this, etag_inner_classes)
      this.innerClasses
    }

    def isAccessibleFrom(scope: Class): Boolean = {
      this.isUnavailable || (this.accflags contains xot_public) || this.isSamePackage(scope) || scope.isUnderHostClass(this)
    }

    def isUnderHostClass(host: Class): Boolean = {
      var clazz = this
      infiniteLoop {
        if (clazz eq host) {
          return true
        }
        if (!clazz.isAnonymous) {
          return false
        }
        clazz = clazz.hostClass
      }
    }

    def isSamePackage(that: Class): Boolean = (this eq that) ||
      ((this.packageName == that.packageName) && (this.isUnloadable || that.isUnloadable || this.isLoadedBySameClassloader(that)))

    def getPackageName: XString = this.packageName

    def isLoadedBySameClassloader(c2: Class): Boolean = this.getClassloaderID == c2.getClassloaderID

    def outerClass_=(outerClass: Class): Unit = {
      assert(!hasOuterClass)
      markAsHasOuterClass()

      addFEXT(new ClassFEXT(outerClass), outerClassFEXTType)
      addImport(outerClass)
    }

    def outerClass: Class =
      if (hasOuterClass) getFEXT[ClassFEXT](outerClassFEXTType).class0 else null

    /** Tests if class is a subclass of `java.lang.Throwable` */
    def isThrowableSubclass: Boolean = {
      if (!this.isVerifiable || this.isInterface) {
        return false
      }
      var sup = this
      while (sup != null) {
        if (sup.name.equals2("java/lang/Throwable")) {
          return true
        }
        sup = sup.getSuperClassO2
      }
      false
    }

    /** Tests if class is a subclass of `sun.reflect.MethodAccessorImpl`` */
    def isMethodAccessorImplSubclass: Boolean = {
      if (!this.isVerifiable || this.isInterface) {
        return false
      }
      var sup = this
      while (sup != null) {
        if (sup.name.equals2("sun/reflect/MethodAccessorImpl")) {
          return true
        }
        sup = sup.getSuperClassO2
      }
      false
    }

    def setSuperInterfaces(iarray: Array[RefInterfaceType]): Unit = {
      interfaces.clear()
      if (iarray != null) {
        iarray foreach addImport
        interfaces ++= iarray
      }
    }

    def getSuperInterfaces: Iterator[RefInterfaceType] = interfaces.iterator

    def getSuperInterfacesO2: Iterator[Class] = getSuperInterfaces.map(t => typeToO2Class(t.symType))

    def getSuperInterfacesCount: Int = interfaces.size

    def hostClass_=(host: Class): Unit = {
      assert(!isAnonymous)
      markAsAnonymous()

      host.addImport(this)
      addImport(host)
      addFEXT(new ClassFEXT(host), hostClassFEXTType)

      // inherit classloader from host class
      if (host.isSystemClass) {
        srctags += xot_systemclass
      } else if (host.isFromExtensionClassloader()) {
        srctags += xot_extension_classloader
      }
    }

    def hostClass: Class =
      if (isAnonymous) getFEXT[ClassFEXT](hostClassFEXTType).class0 else null

    def cangjiePackage_=(p: Class): Unit = {
      assert(isCangjieType)
      if (this eq p) {
        markAsCangjiePackage()
      } else {
        addImport(p)
        _cangjiePackage = p
      }

      val packageName = p.name.toString
      if (O2Env.env.listOf(CangjiePackagesToO1) contains packageName) {
        markAsForcedO1Compiled()
      }
    }

    def cangjiePackage: Class = {
      if (isCangjieType) {
        if (isCangjiePackage) {
          this
        } else {
          _cangjiePackage
        }
      } else {
        null
      }
    }

    def setSuperClass(base: RefClassType): Unit = {
      assert(base != null)
      addImport(base)
      superclass = base
    }

    def getSuperClass: RefClassType = {
      assert(!isClassDefinitionError)
      assert(!hasAbsentSuper)

      if (isHierarchyRoot || isNamespace) {
        null
      } else {
        superclass
      }
    }

    def getSuperClassO2: Class = {
      val s = getSuperClass
      if (s == null) null else typeToO2Class(getSuperClass.symType)
    }

    def addImport(imp: ReferenceType): Unit = {
      addImport(typeToO2Class(imp.symType))
    }

    def addImport(imp: Class): Unit = {
      if (this eq imp) {
        return
      }

      if (env.stage == env.BACK) {
        // After symlevel serialized, the is no way to safely append import to any class, except current class,
        // because at any moment we can drop cached symlevel and re-read it from *.sym lately on demand.
        //
        // So, this operation:
        //   1) may not do what we want it to do
        //   2) may lead to unstable results of compilation
        //
        // Thus it is forbidden.
        assert(opAttrsModule.currClass eq this)
      }

      onGet(this, etag_import)

      val index = findImportEntryIndex(imp)

      if (index < 0) {
        importList += new ImportEntry(imp.nameObj, imp)
      }

      if (resolvedClasses == null) {
        resolvedClasses = mutable.HashMap.empty[XString, Class]
      }
      // Do not overwrite already resolved classes to avoid replacing
      // absent class (added when parsing class file import) with non-absent
      // (added later, for example when checking signatures of overridden methods).
      // Class which is added later may have incorrect import tags, such as missing utag_activation_import (JET-6835).
      if (!(resolvedClasses contains imp.name)) {
        resolvedClasses(imp.name) = imp
      }
    }

    def getImport: Iterator[Class] = {
      onGet(this, etag_import)
      importList.iterator.map(_.resolveFrom(this))
    }

    def getClassByPersistentImportIndex(n: Int): Class = {
      onGet(this, etag_import)
      assert(0 <= n && n < this.persistentImportNum)
      this.getImportedClassByIndex0(n)
    }

    def getPersistentImportIndex(imp: Class): Int = {
      onGet(this, etag_import)
      val idx = this.findImportEntryIndex(imp)
      if (idx >= this.persistentImportNum) {
        return -1
      }
      idx
    }

    def setPersistentImportNum(): Unit = {
      if (persistentImportNum == 0) {
        persistentImportNum = importList.size
      } else {
        assert(persistentImportNum == importList.size)
      }
    }

    def getImportedClassByIndex0(n: Int): Class = importList(n).resolveFrom(this)

    def containsImport(imp: Class): Boolean = {
      val name = imp.nameObj
      importList.exists(_.name == name)
    }

    def findImportEntryIndex(imp: Class): Int = {
      val name = imp.nameObj
      importList.indexWhere(_.name == name)
    }

    def getClinit: Method = {
      if (this.hasClinit) {
        val m = this.findLocalMethod(js.jstrClinit, MethodSignature()(V))
        assert(m != null)
        assert(m.isClinit)
        return m
      }
      null
    }

    def declaredFields: Iterator[Field] = {
      onGet(this, etag_fields)
      fields.iterator
    }

    def declaredFieldsCount: Int = {
      onGet(this, etag_fields)
      fields.size
    }

    def declaredStaticFields: Iterator[StaticField] = declaredFields collect { case f: StaticField => f }

    def declaredInstanceFields: Iterator[InstanceField] = declaredFields collect { case f: InstanceField => f }

    def declaredMethods: Iterator[Method] = {
      onGet(this, etag_methods)
      methods.iterator
    }

    def declaredMethodsCount: Int = {
      onGet(this, etag_methods)
      methods.size
    }

    def declaredInstanceMethods: Iterator[Method] = declaredMethods filterNot (_.isStatic)

    def setIMTSlots(slots: Array[Int]): Unit = {
      assert(!virtualNumbersAreNumerated)
      this.imtSlots = slots
    }

    def getVMTSize: Int = {
      assert(virtualNumbersAreNumerated)
      this.vmtSize
    }

    def setVMTSize(sz: Int): Unit = {
      assert(!virtualNumbersAreNumerated)
      this.vmtSize = sz
    }

    def getObjectHeaderSize: Int = {
      if (this.isAJManagedType) {
        // liontiger: Explicitly filter out AJObject and LockableAJObject to please Gods of symlevel.
        if (TypeProvider.isAJObject(this)) {
          RTConst.HeapObj.size // com/huawei/excelsior/aj/lang/AJObject
        } else if (TypeProvider.isLockableAJObject(this)) {
          RTConst.LockableObj.size // com/huawei/excelsior/aj/lang/LockableAJObject
        } else if (this.isSubType(CacheAPIModule.getClass(ClassID.LockableAJObject))) {
          RTConst.LockableObj.size // com/huawei/excelsior/aj/lang/LockableAJObject
        } else {
          RTConst.HeapObj.size // com/huawei/excelsior/aj/lang/AJObject
        }
      } else if (isCangjieType || isXScalaType) {
        RTConst.HeapObj.size
      } else if (languagePack.supports(JAVA)) { // TODO: remove when java classes are not added in non-java LPs
        RTConst.JavaObj.size // java/lang/Object
      } else {
        // Classes like java/lang/Object may be present in non-java language pack.
        // In that case we can't use JavaObj.size directly, but need to return something.
        0
      }
    }

    override def alignment: Int = {
      assert(instanceLayoutIsNumerated)
      getInstanceAlignment0
    }

    def getInstanceAlignment0: Int = {
      assert(alignmentCalculated)
      assert(isAJCompoundClass || isCangjieArray || isRecord || _alignment == RTConst.HeapObj.alignment || targetArch == Arch.CBC)
      _alignment
    }

    def alignment_=(a: Int): Unit = {
      assert(!instanceLayoutIsNumerated)
      assert(!alignmentCalculated)
      _alignment = a
    }

    def alignmentCalculated: Boolean = this._alignment >= 0

    override def size: Int = {
      assert(instanceLayoutIsNumerated)
      getInstanceSize0
    }

    def getInstanceSize0: Int = {
      assert(sizeCalculated)
      _size
    }

    def size_=(sz: Int): Unit = {
      assert(!instanceLayoutIsNumerated)
      assert(!sizeCalculated)
      _size = sz
    }

    def sizeCalculated: Boolean = _size >= 0

    def getBCSourceName: XString = this.bcSourceName

    def setBCSourceName(name: XString): Unit = {
      this.bcSourceName = name
    }

    def setInheritanceLevel(level: Int): Unit = {
      assert(level >= 0)
      this.level = level
    }

    def getInheritanceLevel: Int = {
      assert(!this.isInterface)
      assert(this.level >= 0)
      this.level
    }

    def isOptimizedAggressively: Boolean = {
      this.srctags contains xot_optimized_aggressively
    }

    def isJDKClass: Boolean = this.isSystemClass && !this.isJetRuntimeClass || this.isFromExtensionClassloader()

    def isJetRuntimeEnum: Boolean = this.isJetRuntimeClass && (this.srctags contains xot_enum)

    def isJetRuntimeClass: Boolean = {
      this.srctags contains xot_jet_runtime
    }

    def isUnloadableOnClassDefStage: Boolean = this.isUnavailable || this.isClassDefinitionError

    def isCompilable: Boolean = !this.isShielded && !this.isSynthetic

    def getFrom: Class = {
      assert(this.isAbsent)
      this.from
    }

    def setFrom(from: Class): Unit = {
      assert(this.isAbsent)
      if (this.from != null) {
        // JET-6670: clear old members as they may reference wrong classes
        this.dropElementsForBadClass()
      }
      this.from = from
    }

    //---------------------------------------------------------------------------
    def dropElementsForBadClass(): Unit = {
      val set = ETAG_SET.of(etag_fields, etag_methods, etag_inner_classes, etag_imt_slots, etag_str_table)

      this.dropElements(set)
      this.disableCompletion(set)
    }

    def isCangjieType: Boolean = {
      this.srctags contains xot_cangjie
    }

    def markAsCangjieType(): Unit = {
      this.inclModifier(xot_cangjie)
    }

    def isXScalaType: Boolean = {
      srctags contains xot_xscala
    }

    def markAsXScalaType(): Unit = {
      markAsNoJavaClass()
      inclModifier(xot_xscala)
    }

    def isJavaAnnotatedCangjieClass: Boolean = {
      this.srctags contains xot_java_annotated
    }

    def markAsJavaAnnotatedCangjieClass(): Unit = {
      this.inclModifier(xot_java_annotated)
    }

    def isBitcodeDeferred: Boolean = {
      this.srctags contains xot_bitcode_deferred
    }

    def markAsBitcodeDeferred(): Unit = {
      this.inclModifier(xot_bitcode_deferred)
    }

    def hasDeferredSuper: Boolean = {
      assert(instanceLayoutIsNumerated || this.isUnloadable)
      hasDeferredSuper0
    }

    def hasDeferredSuper0: Boolean = {
      this.srctags contains xot_has_deferred_super
    }

    def markAsHasDeferredSuper(): Unit = {
      assert(!instanceLayoutIsNumerated)
      this.inclModifier(xot_has_deferred_super)
    }

    def isCHIRDef: Boolean = {
      this.srctags contains xot_chir_def
    }

    def markAsCHIRDef(): Unit = {
      this.inclModifier(xot_chir_def)
    }

    def isCangjieJavaHelper: Boolean = {
      isCangjieType && this.name.toString.endsWith(CangjieSymLevelMaker.JAVA_HELPER_SUFFIX)
    }

    def isCangjieBox: Boolean = {
      isCangjieType && this.name.toString.startsWith(CangjieSymLevelMaker.BOX_PREFIX)
    }

    def isCangjieArray: Boolean = {
      isCangjieType && this.name.toString.startsWith(CangjieSymLevelMaker.CANGJIE_ARRAY_PREFIX)
    }

    def isAJArray: Boolean = {
      this.srctags contains xot_aj_array
    }

    def markAsAJArray(): Unit = {
      this.markAsAJManagedType();
      this.inclModifier(xot_aj_array)
    }

    def isAJManagedEnum: Boolean = this.isAJManagedType && (this.srctags contains xot_enum)

    def isHierarchyRoot: Boolean = {
      this.srctags contains xot_hierarchy_root
    }

    def needVerify: Boolean = {
      this.srctags contains xot_needcheckpairs
    }

    def isVerifiable: Boolean = !this.isClassDefinitionError && !this.isNotVerifiedCode

    def isInterpreterInternals: Boolean = {
      this.srctags contains xot_interp_internals
    }

    def markAsInterpreterInternals(): Unit = {
      this.inclModifier(xot_interp_internals)
    }

    def hasInstanceDescriptor: Boolean = !this.isInterface && this.hasManagedMetaInformation && !this.isInfectedAJClass

    def hasTypeHandle: Boolean = this.hasMetaInformation

    def hasMetaInformation: Boolean = this.hasManagedMetaInformation || this.hasThinTD

    def hasManagedMetaInformation: Boolean = {
      containsManaged || isAJManagedType || isAJExtended || isCangjieType || isXScalaType || isRecord ||
        !(isAJCompoundClass || TypeProvider.isAJCompoundType(this) || isValueClass || isNamespace || isAJManagedType || isAJExtended)
    }

    def isSingletonObject: Boolean = symType.isClass && isCangjieLambdaBaseClass && !symType.isAbstractClass && symType.getFields.isEmpty

    def isAJManagedType: Boolean = {
      this.srctags contains xot_aj_managed
    }

    def markAsAJManagedType(): Unit = {
      this.inclModifier(xot_aj_managed)
    }

    def isInfectedAJClass: Boolean = this.containsManaged && (this.isAJCompoundClass || this.isValueClass || this.isNamespace)

    def hasThinTD: Boolean = this.isThinClass && this.isPolyThinClass

    def isAJCompoundClass: Boolean = this.isStructClass || this.isThinClass

    def isRecord: Boolean = {
      this.srctags contains xot_record
    }

    def markAsRecord(): Unit = {
      this.inclModifier(xot_record)
    }

    def isAnnotation: Boolean = {
      this.accflags contains xot_annotation
    }

    def isDeprecated: Boolean = this.srctags contains xot_type_deprecated

    def markAsDeprecated(): Unit = {
      this.inclModifier(xot_type_deprecated)
    }

    def getModifiers: Set32 = {
      this.srctags.toSet32 & xjRTS.JMDF_TYPE_MASK
    }

    def getCJModifiers: Set32 = {
      this.accflags.toSet32 & pcOModule.CangjieClass.MODIFIERS
    }

    def getAccessFlags: Set32 = {
      this.accflags.toSet32 & xjRTS.ACC_FLAGS_MASK
    }

    def isAbstract: Boolean = {
      this.accflags contains xot_abstract
    }

    def isDetectedFinal: Boolean = {
      this.accflags contains xot_final
    }

    def isFinal: Boolean = {
      this.accflags contains xot_final
    }

    def getClassloaderID: Int = if (languagePack.supports(JAVA)) {
      pcOModule.getClassloaderID(this)
    } else {
      ClassloaderIDGetter.SYSTEM_CLID
    }

    override def hashCode: Int = {
      val s = this.name
      s.hashCode
    }

    override def equals(oPar: Any): Boolean = {
      val o = oPar.asInstanceOf[AnyRef]

      if (this eq o) {
        return true
      }
      if (!o.isInstanceOf[Class]) {
        return false
      }
      this.mno == o.asInstanceOf[Class].mno
    }

    def newMethod(name: XString, sig: XString, accf: Set32, addSignatureImport: Boolean): Method = {
      val msig = typeProvider.resolveMethodSignature(sig, classByO2Object(this))
      newMethod(name, msig, accf, addSignatureImport, ABI.Description(Option.unless(accf contains xjRTS.mdf_static)(SignatureType.fromSymType(symType))))
    }

    def newMethod(name: XString, sig: MethodSignature, accf: MTAG_ANNOT_SET, addSignatureImport: Boolean,
                  abiDesc: ABI.Description) = {
      // SET of xjRTS.mdf_*
      // accf contains both bytecode access flags & JET-specific modifiers
      var m: Method = null

      if (!(accf contains xjRTS.mdf_static)) {
        // we cannot add instance method to class with computed VMT layout
        assert(!virtualNumbersAreNumerated)
      }

      this.beforeNewMethod()
      m = new Method(this.mno, pcNames.NameAndSig(name, sig), methods.size + 1)
      this.afterNewMethod(m, name, sig, accf, addSignatureImport, abiDesc)
      m
    }

    def afterNewMethod(m: Method, name: XString, msig: MethodSignature, accf: MTAG_ANNOT_SET, addSignatureImport: Boolean,
                       abiDesc: ABI.Description): Unit = {
      // SET of xjRTS.mdf_*
      // accf contains both bytecode access flags & JET-specific modifiers
      declareMember(this, m)
      methods += m

      if (O2Env.env.enabled(SemiO1) && (m.lref % 2 == 0)) {
        m.markAsForcedO1Compiled()
      }

      if (isForcedO1Compiled) {
        val packageName = cangjiePackage.name.toString

        if (isCangjiePackage) {
          // Filter out methods without mangled package name in name (this check required to not sent to O1 generic
          // global methods which randomly generated in this package and may be used from other packages).
          if (m.name.toString contains packageName) {
            m.markAsForcedO1Compiled()
          }

        } else {
          if (name.toString contains packageName) {
            // This class name contains mangled package name (this check requires to not sent to O1 generic instances
            // from some types not from this package, like ArrayList<Int>).
            m.markAsForcedO1Compiled()
          }
        }
      }

      if (addSignatureImport) {
        msig.parameterTypes foreach addImport
        addImport(msig.returnType)
      }

      assert(m.modifiers == XOTAG_SET.empty)
      m.modifiers = accf.toSet32/*XOTAG_SET*/


      val arraySliceConstructor = isArraySliceConstructor(name.toString)
      val msigAdjusted = if (arraySliceConstructor) msig.copy(parameterTypes = msig.parameterTypes.tail) else msig

      val (abiSig, specialParamSet) = makeABISignature(msigAdjusted, abiDesc)
      m.abiSig = abiSig
      m.specialParamSet = specialParamSet

      if (m.modifiers contains xot_static) {
        assert(!name.equals(js.jstrInit) || isCangjieType)

        if (name.equals(js.jstrClinit) && m.getSignature == MethodSignature()(V) && m.isStatic) {
          m.mtags += mtag_clinit
          inclModifier(xot_statini)
        } else if (arraySliceConstructor) {
          m.markAsRecordConstructor()
        }
      } else if (name.equals(js.jstrInit)) {
        // not static
        m.markAsConstructor()
        if (m.getDeclaringClass.isRecord) {
          m.markAsRecordConstructor()
        }
      }

      if (ProjectLogic.compilationMode == O1) {
        m.markAsForcedO1Compiled()
      }

      if (ProjectLogic.useMiddleStage) {
        m.markAsCompiledWithMiddleStage()
      }
    }

    def beforeNewMethod(): Unit = {
      onGet(this, etag_methods)
      classByO2Object(this).dropDeclaredMethodsCache() // workaround for JET-16660
    }

    def exclModifier(tag: XOTAG): Unit = {
      this.srctags -= tag
    }

    def inclModifier(tag: XOTAG): Unit = {
      this.srctags += tag
    }

    def getMemberByLRef(lref: Int): Member = {
      assert(lref != 0)
      if (lref > 0) {
        onGet(this, etag_methods)
        methods(lref - 1) ensuring (_.lref == lref)
      } else {
        onGet(this, etag_fields)
        fields(-lref - 1) ensuring (_.lref == lref)
      }
    }

    def getLLVMDebugType: DebugType = {
      onGet(this, etag_debug_type)
      this.llvmDebugType
    }

    def setLLVMDebugType(tpe: DebugType): Unit = {
      this.llvmDebugType = tpe
    }

    /** @param accf a [[Set32]] of xjRTS.mdf_*, see [[xjRTS.MDF_FIELD_MASK]] */
    def newField(name: XString, sig: XString, accf: Set32, addSignatureImport: Boolean): Field = {
      val sigType = typeProvider.resolveSingleElementSignature(sig, classByO2Object(this))
      newField(name, sigType, accf, addSignatureImport)
    }

    def newField(name: XString, sig: SignatureType, accf: Set32, addSignatureImport: Boolean): Field = {
      // SET of xjRTS.mdf_*
      if (accf contains xjRTS.mdf_static) {
        this.newStaticField(name, sig, accf, addSignatureImport)
      } else {
        this.newInstanceField(name, sig, accf, addSignatureImport)
      }
    }

    /** @param accf a [[Set32]] of xjRTS.mdf_*, see [[xjRTS.MDF_FIELD_MASK]] */
    private def newInstanceField(name: XString, sig: SignatureType, accf: Set32, addSignatureImport: Boolean): InstanceField = {
      // we cannot add instance field to class with computed object instance layout
      assert(!instanceLayoutIsNumerated)

      this.beforeNewField()
      val f = new InstanceField(this.mno, pcNames.NameAndSig(name, sig), -1 - fields.size)
      this.afterNewField(f, name, sig, accf, addSignatureImport)
      f
    }

    /** @param accf a [[Set32]] of xjRTS.mdf_*, see [[xjRTS.MDF_FIELD_MASK]] */
    def newStaticField(name: XString, sig: SignatureType, accf: Set32, addSignatureImport: Boolean): StaticField = {
      this.beforeNewField()
      val f = new StaticField(this.mno, pcNames.NameAndSig(name, sig), -1 - fields.size)
      this.afterNewField(f, name, sig, accf, addSignatureImport)
      f
    }

    /** @param accf a [[Set32]] of xjRTS.mdf_*, see [[xjRTS.MDF_FIELD_MASK]] */
    private def afterNewField(f: Field, name: XString, sig: SignatureType, accf: Set32, addSignatureImport: Boolean): Unit = {
      declareMember(this, f)

      fields += f

      if (addSignatureImport) {
        addImport(sig)
      }
      f.sig = sig

      assert(accf == (accf & xjRTS.MDF_FIELD_MASK), accf)

      assert(f.modifiers == XOTAG_SET.empty)
      f.modifiers = accf.toSet32/*XOTAG_SET*/
    }

    private def beforeNewField(): Unit = {
      onGet(this, etag_fields)
      classByO2Object(this).dropDeclaredFieldsCache() // workaround for JET-16660
    }

    /** Resolves class reference in constant pool and cache the result */
    def resolveClassRef(C: jcp.PtrClassInfo, idx: Int): pc.SymType.Reference = {
      assert(C.constantPool(idx).constantType == jcp.TagClass.toByte)
      var t = C.constantPool(idx).resolvedType
      if (t == null) {
        val typeName = C.constantPool(C.constantPool(idx).indexName.toInt).bufferPtr
        t = resolveRefType(typeName)
        C.constantPool(idx).resolvedType = t
      }
      t
    }

    /** Resolves class or array. Returns pcO.Class or array type */
    private def resolveRefType(name: XString): pc.SymType.Reference = {
      if (name.charAt(0) == '[') {
        sigTypeToO2Type(typeProvider.resolveSingleElementSignature(name, this.symType)).asInstanceOf[pc.SymType.Array]
      } else {
        // Do not add resolved class to import if we are in BACK stage and not read file from sym. It may happen if
        // we compile with AddImportFromConstantPool option.
        this.resolveClass(name, addImport = (env.stage != env.BACK))
      }
    }

    /** This predicate detects classes that will never be loaded into the JVM
      * (absent classes, present classes with an absent superclass, and non-verifiable classes).
      * Any operation with this classes results in either throwing certain
      * JVM error or getting null value of variables of such types.
      */
    def isUnloadable: Boolean = this.isUnavailable || !this.isVerifiable

    def isInterface: Boolean = {
      this.accflags contains xot_interface
    }

    /** This class is not found by the compiler (former 'absent stub')
      * or it cannot be compiled because it has one or more unavailable supers
      */
    def isUnavailable: Boolean = this.isAbsent || this.hasAbsentSuper

    /** This class is not contained in compilation set nor in any imported component.
      * It can be unavailable at the moment of compilation or explicitly excluded.
      * All such classes can be accessed/referenced only via reflection shield.
      */
    def isShielded: Boolean = this.isUnavailable

    //--------------------------------------------------------------------
    def resolveClass(name0: XString, addImport: Boolean): Class = {
      val name = JBCPreprocessor.movedScalaClassName(name0)
      if (this.name.equals(name)) {
        return this
      }

      val c = if (this.isAbsent) {
        this.from ensuring (_ != null)
      } else {
        this
      }

      c.ensureImportLoaded() // import should be added to resolvedClasses

      // to search them there first
      if (c.resolvedClasses != null) {
        for (res <- c.resolvedClasses.get(name)) {
          return res
        }
      }

      val res = c.makeClass(name)
      assert(res != null)

      if (c.resolvedClasses == null) {
        c.resolvedClasses = mutable.HashMap.empty[XString, Class]
      }
      c.resolvedClasses(name) = res

      if (addImport) {
        c.addImport(res)
      }
      res
    }

    def ensureImportLoaded(): Unit = {
      importList.foreach(_.resolveFrom(this)) // trigger loading
    }

    private def addImport(t: Signature): Unit =
      foreachO2TypeInSignature(t) { case clazz: Class => addImport(clazz) }

    def addImport(mt: MethodType): Unit = {
      addImport(mt.signature)
    }

    def makeClass(className: XString): Class =
      pc.withModule(this) { pcOModule.makeClass(className) }

    def isFromExtensionClassloader(): Boolean = {
      this.srctags contains xot_extension_classloader
    }

    def isSystemClass: Boolean = {
      this.srctags contains xot_systemclass
    }

    def dropElements(elementsPar: ETAG_SET): Unit = {
      val elements = elementsPar &~ elementsRequireCompletion

      for (element <- etag_first to etag_last) {
        if (elements contains element) {
          element match {
            case `etag_import`          => // nothing to do
            case `etag_inner_classes`   => innerClasses = null
            case `etag_imt_slots`       => imtSlots = null
            case `etag_str_table`       => strTable = null
            case `etag_debug_type`      => llvmDebugType = null

            case `etag_attributes` => cleanFEXTs()

            case `etag_methods` =>
              methods foreach (_.clean())
              methods.clearAndShrink()

            case `etag_fields` =>
              fields.clearAndShrink()
          }

          enableCompletion(ETAG_SET.of(element))
        }
      }
    }

    def enableCompletion(elements: ETAG_SET): Unit = {
      this.elementsRequireCompletion = this.elementsRequireCompletion | elements
    }

    def disableCompletion(elements: ETAG_SET): Unit = {
      this.elementsRequireCompletion = this.elementsRequireCompletion &~ elements
    }

    def isCold: Boolean = curSession - lastUsageTime > classCoolingTime

    def fileDescriptor = _fileDescriptor

    def fileDescriptor_=(fd: xfs.FileDescriptor): Unit = if (_fileDescriptor == null && fd != null) {
      _fileDescriptor = fd
    }

    def isInCompilationSet: Boolean = if (isAnonymous) hostClass.isInCompilationSet else _fileDescriptor != null

    def classInfo = {
      if (_classInfo == null) _classInfo = loadClass(this)
      _classInfo
    }

    def classInfo_=(info: jcp.PtrClassInfo): Unit = _classInfo = info

    def setBytecodeInfo(majorCFVersion: Short, minorCFVersion: Short, vcfSizeEstimation: Int): Unit =
      addFEXT(new BytecodeInfo(majorCFVersion, minorCFVersion, vcfSizeEstimation), bytecodeInfo)

    def getMajorClassFileVersion : Short = fextOption[BytecodeInfo](bytecodeInfo).map(_.majorCFVersion).getOrElse(0)
    def getMinorClassFileVersion : Short = fextOption[BytecodeInfo](bytecodeInfo).map(_.minorCFVersion).getOrElse(0)
    def getVCFSizeEstimation     : Int   = fextOption[BytecodeInfo](bytecodeInfo).map(_.vcfSizeEstimation).getOrElse(0)
  }

  class InnerClass extends Object {
    private[pcOModule] var clazz: Class = _ // inner class
    private[pcOModule] var accessFlags: Set32 = _ // its access flags (inner_class_access_flags field in the "InnerClasses" attribute)

    def getAccessFlags: Set32 = this.accessFlags & xjRTS.JMDF_TYPE_MASK

    def getClass0: Class = this.clazz
  }


  /////////////////////////////////////////////////////////////////////////////
  // Collection of all project classes

  private var _classes = ArrayBuffer.empty[Class]

  def allClasses: Iterator[Class] = new Iterator[Class] {
    private var curr: Int = 0
    override def hasNext = curr < pc.modules.size
    override def next() = if (hasNext) { curr += 1; _classes(curr - 1) } else Iterator.empty.next()
  }

  def allClassesInReversedOrder: Iterator[Class] = new Iterator[Class] {
    private var curr: Int = pc.modules.size - 1
    override def hasNext = curr >= 0
    override def next() = if (hasNext) { curr -= 1; _classes(curr + 1) } else Iterator.empty.next()
  }


  /////////////////////////////////////////////////////////////////////////////
  // Special sets for sym-level objects

  class ClassSet {
    private val impl = mutable.BitSet.empty

    @targetName("append") def +=(c: Class): Unit = impl += c.mno
    @targetName("remove") def -=(c: Class): Unit = impl -= c.mno

    def contains(c: Class): Boolean = impl contains c.mno
    def apply(c: Class): Boolean = contains(c)
  }

  class MethodSet {
    private val impl = mutable.LinkedHashSet.empty[MethodId]
    private def id(m: Method) = MethodId(m.mno, m.lref)

    @targetName("append") def +=(m: Method): Unit = impl += id(m)
    @targetName("remove") def -=(m: Method): Unit = impl -= id(m)

    def contains(m: Method): Boolean = impl contains id(m)
    def apply(m: Method): Boolean = contains(m)

    def size = impl.size
  }

  object MethodSet {
    private case class MethodId(mno: Int, lref: Int)
    def empty = new MethodSet
  }


  /////////////////////////////////////////////////////////////////////////////
  // Bytecode information

  private class BytecodeInfo(var majorCFVersion: Short = 0, var minorCFVersion: Short = 0, var vcfSizeEstimation: Int = 0) extends FEXT {
    override def internalize(si: SymIO): Unit = {
      majorCFVersion = si.curFile.read2().toShort
      minorCFVersion = si.curFile.read2().toShort
      vcfSizeEstimation = si.curFile.readInt()
    }

    override def externalize(si: SymIO): Unit = {
      si.curFile.write2(majorCFVersion.toUShort)
      si.curFile.write2(minorCFVersion.toUShort)
      si.curFile.writeInt(vcfSizeEstimation)
    }
  }


  //------------------- Members Hierarchy -----------------------------
  /** member tags */
  type MEMTAG = UByte
  val MEMTAG_SET = Set32
  type MEMTAG_SET = Set32

  val memtag_ajexternal : MEMTAG = UByte( 0) // this member is @External
  val memtag_ajexported : MEMTAG = UByte( 1) // this member is marked with @Export
  val memtag_overloaded : MEMTAG = UByte( 2) // this member is overloaded

  abstract class Member(_mno: Int, _nameObj: pcNames.NAME, private val _lref: Int) extends pc.Symbol(_mno, _nameObj) {

    /** Logical reference of member. Pair of `mno` and `lref` identify member in compilation set.
      *
      * [[Method]] lref is its index in [[getDeclaringClass]] `methods` + 1, [[Field]] lref is negative index in
      * [[getDeclaringClass]] `fields` - 1, [[StringTable]] lref is [[Int.MaxValue]].
      *
      * `0` is invalid `lref` used for [[VersionedMethodBody]].
      */
    def lref = _lref ensuring (_ != 0)

    override def hashCode: Int = mno * 35 + lref

    override def equals(that: Any): Boolean = that match {
      case that: AnyRef if this eq that => true
      case that: Member => (this.mno == that.mno) && (this.lref == that.lref)
      case _ => false
    }

    def getRef: MemberRef = MemberRef(mno, lref)

    private[pcOModule] var modifiers: XOTAG_SET = _ // field/method modifiers & access flags
    private[pcOModule] var memtags: MEMTAG_SET = _
    private[pcOModule] var numberInClassFile: Int = -1
    var exportID: Int = ExportIds.INVALID_ID

    protected def strFEXT(`type`: Byte): Option[XString] = fextOption[StrFEXT](`type`).map(_.str)
    protected def intFEXT(`type`: Byte): Option[Int] = fextOption[IntFEXT](`type`).map(_.value)

    protected def attrFEXT[T <: jcp.PtrAbstractAnnotationAttr](`type`: Byte): Option[T] =
      fextOption[AnnotationAttr](`type`).map(_.attr.asInstanceOf[T])

    type SigType <: Signature

    def getSignature: SigType = nameObj.asInstanceOf[pcNames.NameAndSig].sig.asInstanceOf[SigType]

    def getExportedName: XString = {
      assert(this.isExported)
      strFEXT(ajExportIDType).orNull
    }

    def getExternalName: XString = {
      assert(this.isExternal)
      strFEXT(extnametype).orNull
    }

    def getTypeAnnotations(rtVisible: Boolean): jcp.PtrTypeAnnotationsAttr = {
      var attrType: Byte = 0

      assert(this.getDeclaringClass.hasAnnotations)
      if (rtVisible) {
        attrType = rtVisTypeAnnotType.toByte
      } else {
        attrType = rtInvisTypeAnnotType.toByte
      }
      attrFEXT[jcp.PtrTypeAnnotationsAttr](attrType).orNull
    }

    def setTypeAnnotations(attr: jcp.PtrTypeAnnotationsAttr, rtVisible: Boolean): Unit = {
      var attrType: Byte = 0

      if (rtVisible) {
        attrType = rtVisTypeAnnotType.toByte
      } else {
        attrType = rtInvisTypeAnnotType.toByte
      }
      val a = new AnnotationAttr
      a.attr = attr
      addFEXT(a, attrType)
      this.getDeclaringClass.markAsHasAnnotations()
    }

    def getAnnotations(rtVisible: Boolean): jcp.PtrAnnotationsAttr = {
      var attrType: Byte = 0

      assert(this.getDeclaringClass.hasAnnotations)
      if (rtVisible) {
        attrType = rtVisAnnotType.toByte
      } else {
        attrType = rtInvisAnnotType.toByte
      }
      attrFEXT[jcp.PtrAnnotationsAttr](attrType).orNull
    }

    def setAnnotations(attr: jcp.PtrAnnotationsAttr, rtVisible: Boolean): Unit = {
      var attrType: Byte = 0

      if (rtVisible) {
        attrType = rtVisAnnotType.toByte
      } else {
        attrType = rtInvisAnnotType.toByte
      }
      val a = new AnnotationAttr
      a.attr = attr
      addFEXT(a, attrType)
      this.getDeclaringClass.markAsHasAnnotations()
    }

    def addCJAnnotationFactory(factoryMethod: Method): Unit = {
      factoryMethod.markAsCangjieAnnotationFactory()
      val declClass = this.getDeclaringClass
      val fext = newMethodFEXT(declClass, declClass.name, factoryMethod.name, factoryMethod.getSignature, null, allowNotFound = true)
      addFEXT(fext, cjAnnotation)
    }

    def getCJAnnotationFactory: Method = {
      fextOption[MethodFEXT](cjAnnotation).map(_.getMethod).orNull
    }

    def getGenericSignature: XString = {
      assert(this.getDeclaringClass.hasGenericsInfo)
      strFEXT(sigtype).orNull
    }

    def setGenericSignature(sig: XString): Unit = {
      addFEXT(new StrFEXT(sig), sigtype)
      this.getDeclaringClass.markAsHasGenericsInfo()
    }

    def getNumberInClassFile: Int = this.numberInClassFile
    // -1 if unknown

    def setNumberInClassFile(num: Int): Unit = {
      assert(this.numberInClassFile == -1)
      this.numberInClassFile = num
    }

    def markAsExported(name: XString = null): Unit = {
      memtags += memtag_ajexported
      if (name != null) {
        this.setExportedName0(name)
      }
    }

    def setExportedName0(id: XString): Unit = {
      assert(this.isExported)
      addFEXT(new StrFEXT(id), ajExportIDType)
    }

    def isExported: Boolean = memtags contains memtag_ajexported

    def markAsExternal(name: XString = null): Unit = {
      memtags += memtag_ajexternal
      if (name != null) {
        addFEXT(new StrFEXT(name), extnametype)
      }
    }

    def isExternal: Boolean = memtags contains memtag_ajexternal

    def addCHIRDef(src: XString, id: Int): Unit = {
      if (!hasFEXT(chirDef)) {
        addFEXT(CHIRDefFEXT(src, id), chirDef)
      }
    }

    def getCHIRDef: Option[CHIRDef] = {
      fextOption[CHIRDefFEXT](chirDef).map(f => CHIRDef(f.src, f.id))
    }

    def markAsDeprecated(): Unit = {
      modifiers += xot_deprecated
    }

    def markAsFinal(): Unit = {
      modifiers += xot_final
    }

    override def getReadableName(need_class_name: Boolean, need_full_sign: Boolean = true): XString = {
      if (this.getDeclaringClass eq x2cClass) {
        this.name
      } else {
        super.getReadableName(need_class_name, need_full_sign)
      }
    }

    def isFinal: Boolean = modifiers contains xot_final

    def isProtected: Boolean = modifiers contains xot_protected

    def isPrivate: Boolean = modifiers contains xot_private

    def isPublic: Boolean = modifiers contains xot_public

    def isStatic: Boolean = modifiers contains xot_static

    def getDeclaringClass: Class = getClassRecord(this.mno)

    def sourceName: XString     = strFEXT(memberSourceName).orNull
    def cppLinkageName: XString = strFEXT(memberCPPLinkageName).orNull
    /** Get source file where this member was defined. You can also try [[Class.getBCSourceName]] if this method returns `null`. */
    def sourceFile: XString     = strFEXT(memberSourceFile).orNull
    def sourceLine: Int         = intFEXT(memberSourceLine).getOrElse(-1)
    def debugType: DebugType    = fextOption[DebugTypeFEXT](memberDebugType).map(_.debugType ).orNull

    def sourceName_=(name: XString): Unit       = addFEXT(new StrFEXT(name.ensuring(n => n != null && !n.isEmpty)), memberSourceName)
    def cppLinkageName_=(name: XString): Unit   = addFEXT(new StrFEXT(name.ensuring(n => n != null && !n.isEmpty)), memberCPPLinkageName)
    def sourceFile_=(file: XString): Unit       = addFEXT(new StrFEXT(file),                                        memberSourceFile)
    def sourceLine_=(line: Int): Unit           = addFEXT(new IntFEXT(line),                                        memberSourceLine)
    def debugType_=(debugType: DebugType): Unit = addFEXT(new DebugTypeFEXT(debugType),                             memberDebugType)

    def markAsOverloaded(): Unit = { memtags += memtag_overloaded }
    def isOverloaded: Boolean = memtags contains memtag_overloaded
  }

  type FTAG = UByte
  type FTAG_SET = Set32

  val ftag_ajflat              : FTAG = UByte(0) // this static field is flat and has FlatFieldInfo attribute
  val ftag_has_offset          : FTAG = UByte(1) // offset has been calculated for this instance field
  val ftag_data                : FTAG = UByte(4) // has @Data annotation, will be writen in .data section

  object CangjieField {
    private[pcOModule] val MODIFIERS: Set32 = Set32.of(xjRTS.mdf_public.toUByte, xjRTS.mdf_private.toUByte, xjRTS.mdf_protected.toUByte,
      xjRTS.mdf_static.toUByte, xjRTS.mdf_final.toUByte, xjRTS.mdf_volatile.toUByte)
  }

  class Field(_mno: Int, _nameObj: pcNames.NAME, _lref: Int) extends Member(_mno, _nameObj, _lref) {
    type SigType = SignatureType

    var sig: SignatureType = _
    private[pcOModule] var ftags: FTAG_SET = _
    private[pcOModule] var offset: Int = _ // field offset in the object or static bundle]

    def setAJFlatInfo(size: Int, alignment: Int): Unit = {
      assert(this.isAJFlat)
      val fext = new SizeAndAlignment
      fext.size = size
      fext.alignment = alignment
      addFEXT(fext, ajFlatFieldInfo)
    }

    def markAsAJFlat(): Unit = {
      this.ftags += ftag_ajflat
    }

    def size: Int =
      if (isAJFlat) getFEXT[SizeAndAlignment](ajFlatFieldInfo).size else sig.symKindErased.size

    def alignment: Int =
      if (isAJFlat) getFEXT[SizeAndAlignment](ajFlatFieldInfo).alignment else sig.symKindErased.alignment

    def getOffset: Int = {
      val c = this.getDeclaringClass
      this match {
        case _: InstanceField => assert(c.instanceLayoutIsNumerated)
        case _: StaticField   => assert(c.staticLayoutIsNumerated)
      }
      if (this.ftags contains ftag_has_offset) {
        this.offset
      } else {
        // static flat fields do not have offset: are moved to BSS
        assert(this.isStatic && this.isAJFlat)
        if (languagePack.supports(JAVA)) {
          RTConst.MetaInfo.UNREFLECTED_FIELD_OFFS.intValue
        } else {
          0
        }
      }
    }

    def isAJFlat: Boolean = this.ftags contains ftag_ajflat

    def setOffset(offs: Int): Unit = {
      val c = this.getDeclaringClass
      assert(!c.isSynthetic)
      this match {
        case _: InstanceField => assert(!c.instanceLayoutIsNumerated)
        case _: StaticField   => assert(!c.staticLayoutIsNumerated)
      }
      assert(!(this.ftags contains ftag_has_offset))
      this.offset = offs
      this.ftags += ftag_has_offset
    }

    def setDataInfo(data: XString): Unit = {
      val fext = DataAnnotFEXT(data)
      this.ftags += ftag_data
      addFEXT(fext, ajDataFieldInfo)
    }

    def offsetIsCalculated: Boolean = this.ftags contains ftag_has_offset

    def getJavaModifiers: Set32 = modifiers.toSet32 & xjRTS.JMDF_FIELD_MASK

    def getCJModifiers: Set32 = {
      modifiers.toSet32 & pcOModule.CangjieField.MODIFIERS
    }

    //--------------- Field Access Methods -----------------
    def getModifiers: Set32 = modifiers.toSet32 & xjRTS.MDF_FIELD_MASK
  }


  class StaticField(_mno: Int, _nameObj: pcNames.NAME, _lref: Int) extends Field(_mno, _nameObj, _lref) {
    var value: ConstValue = _

    def getConstStringValue: Int = intFEXT(conststrtype).getOrElse(-1)

    def setConstStringValue(stringIndex: Int): Unit =
      addFEXT(new IntFEXT(stringIndex ensuring (_ >= 0)), conststrtype)

    def hasInitialValue: Boolean = modifiers contains xot_constval

    def markAsHasInitialValue(): Unit = {
      modifiers += xot_constval
    }

    def isCompileTimeConstant: Boolean = {
      if (isFinal) {
        if (sig.isPrimitive) {
          return value != null
        } else {
          return getConstStringValue >= 0
        }
      }
      false
    }

    def hasDataAnnot: Boolean = this.ftags contains ftag_data

    /** There is compiler-generated binary object (code or static data) for this static field */
    def shouldBeGenerated: Boolean = getDeclaringClass.hasManagedMetaInformation || !isCompileTimeConstant

    def getDataAnnotData = {
      assert(hasDataAnnot)
      DataAnnotationParsing.parse(getFEXT[DataAnnotFEXT](ajDataFieldInfo).dataStr.toString)
    }
  }


  class InstanceField(_mno: Int, _nameObj: pcNames.NAME, _lref: Int) extends Field(_mno, _nameObj, _lref)


  class StringTable(_mno: Int) extends StaticField(_mno, pcNames.NameAndSig(STRTABLE_NAME, SignatureType.Address), _lref = Int.MaxValue) {
    modifiers = XOTAG_SET.of(xot_final)

    // TODO: remove subclassing of StaticField
    private[pcOModule] var strList: ArrayBuffer[XString] = null                      // list of the strings
    private[pcOModule] var strTable: Hashtable = null /*<js.JString, lang.Integer>*/ // mapping [string => index]
    private[pcOModule] var holders: ArrayBuffer[pc.Symbol] = _ // list of strings holders
    private[pcOModule] var symLength: Int = -1

    /**
      Returns length of StringTable that was at sym file writing.
    */
    def getSymFileTimeLength: Int = {
      onGet(getDeclaringClass, etag_str_table)
      this.symLength
    }

    def getLength: Int = {
      onGet(getDeclaringClass, etag_str_table)
      if (this.strList == null) {
        return 0
      }

      this.strList.size
    }

    def setStringHolder(index: Int, holder: pc.Symbol): Unit = {
      if (this.holders == null) {
        this.holders = new ArrayBuffer[pc.Symbol]()
      }

      val pos = this.holders.size
      assert(pos == index)
      this.holders += holder
    }

    def getStringHolder(index: Int): pc.Symbol = {
      assert(this.holders != null)
      this.holders(index)
    }

    def getStringByIndex(index: Int): XString = {
      onGet(getDeclaringClass, etag_str_table)
      assert(this.strList != null)
      this.strList(index)
    }

    def getIndexByString(str: XString): Int = {
      val idx = this.getIndexByStringIfPresent(str)
      assert(idx != -1)
      idx
    }

    def getIndexByStringIfPresent(str: XString): Int = {
      onGet(getDeclaringClass, etag_str_table)
      if (this.strTable == null) {
        return -1
      }

      val idx = this.strTable.get(str).asInstanceOf[Integer]
      if (idx == null) {
        return -1
      }

      idx
    }

    def addString(str: XString): Int = {
      onGet(getDeclaringClass, etag_str_table)
      if (this.strTable == null) {
        assert(this.strList == null)

        this.strList = new ArrayBuffer[XString]
        this.strTable = new Hashtable()
      }

      val idx = this.strTable.get(str).asInstanceOf[Integer]
      if (idx != null) {
        return idx
      }

      val index = this.strList.size
      this.strList += str
      this.strTable.put(str, Integer.valueOf(index))

      index
    }

  }


  /* method tags */
  type MTAG = UByte
  val mtag_jca_always_inline: MTAG = UByte(0)                 // this method is marked as !ALWAYS_INLINE in JCA-file
  val mtag_never_inline: MTAG = UByte(1)                      // this method is marked as !NOTINLINE IN JCA-file or annotated as @NoInline
  val mtag_compiled_with_middle_stage: MTAG = UByte(2)        // this method was compiled in compilation mode with enabled middle stage (means that it will be serialized with extra info even in O1 compilation mode)
  val mtag_clinit: MTAG = UByte(3)                            // this is <clinit> method (Java or AJ)
  val mtag_global_init: MTAG = UByte(4)                       // this is one of global_init functions (Cangjie)
  val mtag_record_constr: MTAG = UByte(5)                     // this is <init> method of Cangjie record
  val mtag_no_local_gc_points: MTAG = UByte(6)                // this method should not contain GC points (i.e. annotated as AJ @NoLocalGCPoints or as NO_LOCAL_GC_POINTS in JCA-file)
  val mtag_package_init: MTAG = UByte(7)                      // this is the initialization function of Cangjie package
  val mtag_codeaddr_used: MTAG = UByte(8)                     // codeaddr of this method is used in runtime
  val mtag_package_literal_init: MTAG = UByte(9)              // this is the literal initialization function of Cangjie package
  val mtag_ajreplaced: MTAG = UByte(10)                       // method is replaced by some @Replacement method
  val mtag_ajreplaced_initialized: MTAG = UByte(11)           // mtag_ajreplaced is initialized
  val mtag_dirty_for_class_gc: MTAG = UByte(12)               // this method is annotated with @DirtyForClassGC
  val mtag_no_traced_regs_on_entry: MTAG = UByte(13)          // this method is annotated with @NoTracedRegsOnEntry
  val mtag_aj_domain: MTAG = UByte(14)                        // this method is annotated with @Domain(DomainType.AJ)
  val mtag_java_domain: MTAG = UByte(15)                      // this method is annotated with @Domain(DomainType.Java)
  val mtag_cangjie_domain: MTAG = UByte(16)                   // this method is annotated with @Domain(DomainType.Cangjie)
  val mtag_aj_rt_noescape: MTAG = UByte(17)                   // method is RT procedure from CompilerInterface class with CompilerHint "no-escape"
  val mtag_aj_rt_allocator: MTAG = UByte(18)                  // method is RT procedure from CompilerInterface class with CompilerHint "allocator"
  val mtag_retthis: MTAG = UByte(19)                          // method returns 'this' (0-th parameter)
  val mtag_aj_no_return: MTAG = UByte(20)                     // method is annotated with @CompilerHint.Method("no-return")
  val mtag_no_code_gen: MTAG = UByte(21)                      // method is not compiled (no code generation)
  val mtag_stack_check_by_caller_byte_count: MTAG = UByte(22) // method is annotated with @CompilerHint.StackCheckByCaller(value = byteCount), byteCount stored in FEXT with type 'StackCheckByCallerByteCount'.
  val mtag_forced_o1_compiled: MTAG = UByte(23)               // method is compiled with O1 (by FastBackEnd option or by some sym-level oracle)
  val mtag_unroll_loops: MTAG = UByte(24)                     // this method is marked as !UNROLL_LOOPS in JCA-file
  val mtag_contains_monitor_operations: MTAG = UByte(25)      //
  val mtag_get_flat_thin_intrinsic: MTAG = UByte(26)          // method is getFlat* intrinsic of Thin class
  val mtag_unused27: MTAG = UByte(27)
  val mtag_is_generic: MTAG = UByte(28)                       //
  val mtag_has_source_full_name: MTAG = UByte(29)             //
  val mtag_thin_unchecked_cast: MTAG = UByte(30)              // method is uncheckedCast intrinsic of Thin class
  val mtag_inline_with_context_point_test: MTAG = UByte(31)   // this method is marked as !INLINE_WITH_CONTEXT_POINT_TEST in JCA-file
  val mtag_unused32: MTAG = UByte(32)                         //
  val mtag_unstable_forwarder: MTAG = UByte(33)               // this method is marked as workaround for JET-17425 and JET-17444
  val MTAG_SET = Set64
  type MTAG_SET = Set64
  /* method tags from annotations*/
  type MTAG_ANNOT = UByte
  val mtag_annot_ajindirectcall: MTAG_ANNOT = UByte(0)                    // = 0  (* @IndirectCall *)
  val mtag_annot_aj_inline_forced: MTAG_ANNOT = UByte(1)                  // = 1  (* @Inline(forced = true) *)
  val mtag_annot_ajcalltomanaged: MTAG_ANNOT = UByte(2)                   // = 2  (* @CallToManaged *)
  val mtag_annot_ajreplacement: MTAG_ANNOT = UByte(3)                     // = 3  (* @Replacement *)
  val mtag_annot_unused4: MTAG_ANNOT = UByte(4)                           // = 4
  val mtag_annot_ajuncheckedcall: MTAG_ANNOT = UByte(5)                   // = 5  (* @UncheckedCall *);
  val mtag_annot_ajuncheckednew: MTAG_ANNOT = UByte(6)                    // = 6  (* @UncheckedNew *);
  val mtag_annot_ajhookinvoker: MTAG_ANNOT = UByte(7)                     // = 7  (* @Hook.Invoker *);
  val mtag_annot_aj_versioned_context: MTAG_ANNOT = UByte(8)              // = 8  (* @Versioned context *)
  val mtag_annot_cj_c: MTAG_ANNOT = UByte(9)                              // = 9  (* @c in a CangJie sense *)
  val mtag_annot_aj_strict_memory: MTAG_ANNOT = UByte(10)                 // = 10 (* @StrictMemory *)
  val mtag_annot_aj_gc_aware: MTAG_ANNOT = UByte(11)                      // = 11 (* @GCAware *)
  val mtag_annot_aj_long_safe: MTAG_ANNOT = UByte(12)                     // = 12 (* @LongSafe *)
  val mtag_annot_aj_inline: MTAG_ANNOT = UByte(13)                        // = 13 (* @Inline *)
  val mtag_annot_aj_no_preparation_check: MTAG_ANNOT = UByte(14)          // = 14 (* @NoPreparationCheck *)
  val mtag_annot_aj_inline_if_const_params: MTAG_ANNOT = UByte(15)        // = 15 (* @InlineIfConstParams *)
  val mtag_annot_aj_thin_constructor: MTAG_ANNOT = UByte(16)              // = 16 (* @ThinConstructor *)
  val mtag_annot_aj_interpretation_loop: MTAG_ANNOT = UByte(17)           // = 17 (* @InterpretationLoop *)
  val mtag_annot_domain: MTAG_ANNOT = UByte(18)                           // = 18 (* @Domain *)
  val mtag_annot_aj_non_throwing: MTAG_ANNOT = UByte(19)                  // = 18 (* @NonThrowing *)
  val mtag_annot_aj_versioned_marker: MTAG_ANNOT = UByte(20)              // = 20 (* @VersionedMarker *)
  val mtag_annot_cangjie_annotation_factory: MTAG_ANNOT = UByte(21)       // = 21 (* Cangjie annotation factory produced by FE *)
  val mtag_annot_aj_record_initializer: MTAG_ANNOT = UByte(22)            // = 22 (* @RecordInitializer *)
  val mtag_annot_aj_alt_location_result: MTAG_ANNOT = UByte(23)           // = 23 (* @AltLocation *)
  val mtag_annot_aj_method_info_frame_descriptor_getter: MTAG_ANNOT = UByte(24) // = 24 @MethodInfoFrameDescriptorGetter
  val mtag_annot_aj_delayed_intrinsic: MTAG_ANNOT = UByte(25)             // = 25 (* @DelayedIntrinsic *)
  val mtag_annot_gen_table_switch: MTAG_ANNOT = UByte(26)              // = 26 (* @GenTableSwitch *)
  val mtag_annot_unused27: MTAG_ANNOT = UByte(27)                         // = 27
  val mtag_annot_unused28: MTAG_ANNOT = UByte(28)                         // = 28
  val mtag_annot_unused29: MTAG_ANNOT = UByte(29)                         // = 29
  val mtag_annot_unused30: MTAG_ANNOT = UByte(30)                         // = 30
  val mtag_annot_unused31: MTAG_ANNOT = UByte(31)                         // = 31

  val MTAG_ANNOT_SET = Set32
  type MTAG_ANNOT_SET = Set32

  object CangjieMethod {
    val FOREIGN_MODIFIER = UByte(7)
    val MUT_MODIFIER = UByte(17)
    private val REDEF_MODIFIER = UByte(18)
    private val OVERRIDE_MODIFIER = UByte(19)

    private[pcOModule] val MODIFIERS: Set32 = Set32.of(xjRTS.mdf_public.toUByte, xjRTS.mdf_private.toUByte, xjRTS.mdf_protected.toUByte,
      xjRTS.mdf_static.toUByte, xjRTS.mdf_final.toUByte, xjRTS.mdf_abstract.toUByte, FOREIGN_MODIFIER, MUT_MODIFIER, REDEF_MODIFIER, OVERRIDE_MODIFIER)
  }

  /** Root of Java methods */

  class Method(_mno: Int, _nameObj: pcNames.NAME, _lref: Int) extends Member(_mno, _nameObj, _lref) {
    type SigType = MethodSignature

    private[pcOModule] var mtags: MTAG_SET = MTAG_SET.empty
    private[pcOModule] var mtagsAnnot: MTAG_ANNOT_SET = MTAG_ANNOT_SET.empty
    private[pcOModule] var callconv: CallConv = MANAGED
    private[pcOModule] var preservedParams = Set32.empty   // Set32 is currently used instead of BitSet because it is
                                                           // not expected to have @Preserved or @AltLocation on parameters
    private[pcOModule] var altLocationParams = Set32.empty // with large index
    private[pcOModule] var abiSig: MethodSignature = _ // signature of this method as seen by ABI (except receiver param)
    private[pcOModule] var specialParamSet: MethodType.SpecialParamSet = _
    private[pcOModule] var throwsList: Array[Class] = _
    private[pcOModule] var bytecodeSize: Int = _
    private[pcOModule] var llvmIdx: Int = CangjieSymLevelMaker.NO_LLVM_INDEX

    private var frameDesc: pc.DataSymbol.Sized = _

    def getABISignature = abiSig

    def getSpecialParamSet = specialParamSet

    def isManagedFrame = hasManagedExecEnv || isAjCallToManaged

    def hasFrameDescriptor = methodByO2Object(this).hasFrameDescriptor

    def getFrameDescriptor: pc.DataSymbol.Sized = {
      assert(hasFrameDescriptor)
      if (frameDesc == null) {
        frameDesc = opAttrsModule.newFrameDescriptor(this)
      }
      frameDesc
    }

    def initJcaKnownSafeInfo(): Unit = {
      this.getJcaKnownSafeInfo
    }

    def getJcaKnownSafeInfo: Int = fextOption[JcaKnownSafeAttr](kstype) match {
      case Some(fext) =>
        val res = fext.retParamNum
        assert(res != JCA_NO_KNOWN_SAFE_INFO)
        res

      case None =>
        var res = jca.findKnownSafeInfo(this)
        if (res == JCA_NO_KNOWN_SAFE_INFO && isAjReplacement && getAjReplacementTarget != null) {
          res = this.getAjReplacementTarget.getJcaKnownSafeInfo
        }

        if (res != JCA_NO_KNOWN_SAFE_INFO) {
          this.setJcaKnownSafeInfo(res)
        }
        res
    }

    def setJcaKnownSafeInfo(retParamNum: Int): Unit = {
      assert(retParamNum != JCA_NO_KNOWN_SAFE_INFO)

      val ks = new JcaKnownSafeAttr
      ks.retParamNum = retParamNum
      tryAddFEXT(ks, kstype)
    }

    def hasLVTConvertedParameters: Boolean =
      fextOption[MethodParametersFEXT](methodParametersType).exists(_.lvtConverted)

    def getParameters: Array[jcp.MethodParameter] =
      fextOption[MethodParametersFEXT](methodParametersType).map(_.methodParameters).orNull

    def setParameters(methParameters: Array[jcp.MethodParameter], lvtConverted: Boolean): Unit = {
      val fext = new MethodParametersFEXT
      fext.methodParameters = methParameters
      fext.lvtConverted = lvtConverted
      addFEXT(fext, methodParametersType)
    }

    def setJCAUnrollLoops(): Unit = {
      mtags += mtag_unroll_loops
    }

    def setJCAInlineWithContextPointTest(): Unit = {
      mtags += mtag_inline_with_context_point_test
    }

    def setNeverInline(): Unit = {
      assert(!this.isInlineAllAndRemove)
      assert(!this.isJCAInline)
      mtags += mtag_never_inline
    }

    def setJCAInlined(): Unit = {
      assert(!this.isNeverInline)
      mtags += mtag_jca_always_inline
    }

    def setAJInline(): Unit = {
      assert(!this.isNeverInline)
      mtagsAnnot += mtag_annot_aj_inline
    }

    def setAJInlineForced(): Unit = {
      assert(!this.isNeverInline)
      mtagsAnnot += mtag_annot_aj_inline_forced
    }

    def setUnstableForwarder(): Unit = {
      mtags += mtag_unstable_forwarder
    }

    /** check whether `UNROLL_LOOPS` is specified in JCA file
      *   1. JCA file used during compilation of host class stores inline information in `mtags`,
      *   1. current JCA file may provide additional information
      */
    def isJCAUnrollLoops: Boolean = (mtags contains mtag_unroll_loops) || jca.isJCAUnrollLoops(this)

    /** check whether `INLINE_WITH_CONTEXT_POINT_TEST` is specified in JCA file
      *   1. JCA file used during compilation of host class stores inline information in `mtags`,
      *   1. current JCA file may provide additional information
      */
    def isJCAInlineWithContextPointTest: Boolean = (mtags contains mtag_inline_with_context_point_test) || jca.isJCAInlineWithContextPointTest(this)

    /** check whether AJ annotation `@NoInline` is specified
      * or `NOTINLINE` is specified in JCA file
      *  1. JCA file used during compilation of host class stores inline information in mtags,
      *  1. current JCA file may provide additional information
      */
    def isNeverInline: Boolean = (mtags contains mtag_never_inline) || jca.isJCANoInline(this) || !isManaged && isVarArgs // TODO: set @NoInline on m in aj-javac

    /** check whether ALWAYS_INLINE is specified in JCA file
      *  1. JCA file used during compilation of host class stores inline information in mtags,
      *  1. current JCA file may provide additional information
      */
    def isJCAInline: Boolean = (mtags contains mtag_jca_always_inline) || jca.isJCAInline(this)

    def isAJInline: Boolean = mtagsAnnot contains mtag_annot_aj_inline

    def isGenTableSwitch: Boolean = mtagsAnnot contains mtag_annot_gen_table_switch

    def markAsGenTableSwitch(): Unit = {
      mtagsAnnot += mtag_annot_gen_table_switch
    }

    def getAnnotationDefault: jcp.PtrAnnotationDefaultAttr = {
      assert(this.getDeclaringClass.hasAnnotations)
      attrFEXT[jcp.PtrAnnotationDefaultAttr](annotDefaultType).orNull
    }

    def setAnnotationDefault(attr: jcp.PtrAnnotationDefaultAttr): Unit = {
      val a = new AnnotationAttr
      a.attr = attr
      addFEXT(a, annotDefaultType)
      this.getDeclaringClass.markAsHasAnnotations()
    }

    def getParameterAnnotations(rtVisible: Boolean): jcp.PtrParameterAnnotationsAttr = {
      assert(this.getDeclaringClass.hasAnnotations)
      val attrType = if (rtVisible) rtVisParAnnotType else rtInvisParAnnotType
      attrFEXT[jcp.PtrParameterAnnotationsAttr](attrType).orNull
    }

    def setParameterAnnotations(attr: jcp.PtrParameterAnnotationsAttr, rtVisible: Boolean): Unit = {
      val attrType = if (rtVisible) rtVisParAnnotType else rtInvisParAnnotType
      val a = new AnnotationAttr
      a.attr = attr
      addFEXT(a, attrType)
      this.getDeclaringClass.markAsHasAnnotations()
    }

    def getCJAnnotationFactoriesForParameters: Array[Method] = {
      fextOption[CJAnnotationFactoriesForParametersFEXT](cjParametersAnnotations).map(_.getFactories).orNull
    }

    def addCJAnnotationFactoriesForParameters(factories: Array[Method]): Unit = {
      val fext = new CJAnnotationFactoriesForParametersFEXT
      fext.factories = factories.map { factory =>
        if (factory == null) {
          val fext = new MethodFEXT
          fext.mlref = -1
          fext.allowNotFound = true
          fext
        } else {
          factory.markAsCangjieAnnotationFactory()
          newMethodFEXT(this.getDeclaringClass, factory.getDeclaringClass.name, factory.name, factory.getSignature, null, allowNotFound = true)
        }
      }
      addFEXT(fext, cjParametersAnnotations)
    }

    def markAsCangjieAnnotationFactory(): Unit = {
      mtagsAnnot += mtag_annot_cangjie_annotation_factory
    }

    def isCangjieAnnotationFactory: Boolean = mtagsAnnot contains mtag_annot_cangjie_annotation_factory

    def setCodeAddrUsed(): Unit = mtags += mtag_codeaddr_used

    //-------------------------------------------------------------------
    def isCodeAddrUsed: Boolean = mtags contains mtag_codeaddr_used

    def markAsAjReplacement(className: XString, methodName: XString, methodSig: XString, envMatch: Boolean): Unit = {
      mtagsAnnot += mtag_annot_ajreplacement

      if (className.nonEmpty && methodSig.nonEmpty) {
        addFEXT(newJBCMethodFEXT(this.getDeclaringClass, className, methodName, methodSig, "@Replacement", !envMatch), ajReplacementType)
      }
    }

    def getAjReplacementTarget: Method = {
      val fext = getFEXT[MethodFEXT](ajReplacementType)
      if (fext != null && this.isAjReplacement) fext.getMethod else null
    }

    def isAjReplacement: Boolean = mtagsAnnot contains mtag_annot_ajreplacement

    def getAjUncheckedNewTarget: Method = {
      assert(this.isAjUncheckedNew)
      getFEXT[MethodFEXT](ajUncheckedNewType).getMethod ensuring (_ != null)
    }

    def markAsAjUncheckedNew(className: XString, methodSig: XString): Unit = {
      mtagsAnnot += mtag_annot_ajuncheckednew
      addFEXT(newJBCMethodFEXT(this.getDeclaringClass, className, js.newJString("<init>"), methodSig, "@UncheckedNew", allowNotFound = false), ajUncheckedNewType)
      this.markAsNoCodeGen()
    }

    //--------- AJ @UncheckedNew support -------------
    def isAjUncheckedNew: Boolean = mtagsAnnot contains mtag_annot_ajuncheckednew

    def getAJInlineIfConstParamsIndices: Array[Int] = {
      assert(this.isAJInlineIfConstParams)
      getFEXT[InlineIfConstParamsFEXT](ajInlineIfConstParams).paramsIndices
    }

    def isAJInlineIfConstParams: Boolean = mtagsAnnot contains mtag_annot_aj_inline_if_const_params

    def setAJInlineIfConstParams(paramsIndices: Array[Int]): Unit = {
      val fext = new InlineIfConstParamsFEXT
      fext.paramsIndices = paramsIndices
      addFEXT(fext, ajInlineIfConstParams)
      mtagsAnnot += mtag_annot_aj_inline_if_const_params
    }

    def hasDefinedStackCheckByCallerByteCount: Boolean = mtags contains mtag_stack_check_by_caller_byte_count

    def setStackCheckByCallerByteCount(count: Int): Unit = {
      addFEXT(new IntFEXT(count ensuring (_ >= 0)), ajStackCheckByCallerByteCount)
      mtags += mtag_stack_check_by_caller_byte_count
    }

    def getStackCheckByCallerByteCount: Int = {
      assert(hasDefinedStackCheckByCallerByteCount)
      val annotValue = intFEXT(ajStackCheckByCallerByteCount).get
      if (initialCompilationMode == O1) {
        annotValue + O2Env.env.valueOf(StackCheckByCallerAdditionalValueForO1Compiled)
      } else {
        annotValue
      }
    }

    def markAsAjNoReturn(): Unit = {
      mtags += mtag_aj_no_return
    }

    def isAjNoReturn: Boolean = mtags contains mtag_aj_no_return

    def markAsAjRTAllocator(): Unit = {
      mtags += mtag_aj_rt_allocator
    }

    def isAjRTAllocator: Boolean = mtags contains mtag_aj_rt_allocator

    def markAsRetThis(): Unit = {
      mtags += mtag_retthis
    }

    def isRetThis: Boolean = mtags contains mtag_retthis

    def markAsAjRTNoEscape(): Unit = {
      mtags += mtag_aj_rt_noescape
    }

    //--------- AJ @CompilerHint support ------------
    def getAjUncheckedCallTarget: Method = {
      assert(this.isAjUncheckedCall)
      getFEXT[MethodFEXT](ajUncheckedCallType).getMethod ensuring (_ != null)
    }

    def markAsAjUncheckedCall(className: XString, methodName: XString, methodSig: XString): Unit = {
      mtagsAnnot += mtag_annot_ajuncheckedcall
      addFEXT(newJBCMethodFEXT(this.getDeclaringClass, className, methodName, methodSig, "@UncheckedCall", allowNotFound = false), ajUncheckedCallType)
      this.markAsNoCodeGen()
    }

    //--------- AJ @UncheckedCall support -------------
    def isAjUncheckedCall: Boolean = mtagsAnnot contains mtag_annot_ajuncheckedcall

    //--------- @CallConv.Head support -------------

    def setCallConvHeadInLimit(limit: Int): Unit = {
      assert(limit >= 0)
      assert(getCallConv.isJET, "@CallConv.Head(inLimit) set for method without JET CC")
      addFEXT(new IntFEXT(limit), ajCallConvHeadInLimit)
    }

    def getCallConvHeadInLimit: Int = {
      intFEXT(ajCallConvHeadInLimit).getOrElse(Int.MaxValue)
    }

    def setCallConvHeadOutLimit(limit: Int): Unit = {
      assert(limit >= 0)
      assert(getCallConv.isJET, "@CallConv.Head(outLimit) set for method without JET CC")
      addFEXT(new IntFEXT(limit), ajCallConvHeadOutLimit)
    }

    def getCallConvHeadOutLimit: Int = {
      intFEXT(ajCallConvHeadOutLimit).getOrElse(1)
    }

    //--------- @CallConv.Preserved support -------------

    def addCallConvPreservedParam(param: Int): Unit = {
      assert(param >= 0)
      assert(getCallConv.isJET, "@Preserved parameters can only be used in methods with JET CC")
      assert(!getDeclaringClass.isCangjieType)
      assert(specialParamSet.elements.forall(_ == MethodType.SpecialParameter.Receiver))
      val addend = if (isStatic) 0 else 1 // Account for receiver for AJ instance methods
      preservedParams += param + addend
    }

    //--------- @CallConv.AltLocation support -------------

    def addCallConvAltLocationParam(param: Int): Unit = {
      assert(param >= 0)
      assert(getCallConv.isManaged, "@AltLocation parameters can only be used in methods with Managed CC")
      assert(!getDeclaringClass.isCangjieType)
      assert(specialParamSet.elements.forall(_ == MethodType.SpecialParameter.Receiver))
      val addend = if (isStatic) 0 else 1 // Account for receiver for AJ instance methods
      altLocationParams += param + addend
    }

    def markAsAltLocationResult(): Unit = mtagsAnnot += mtag_annot_aj_alt_location_result
    def hasAltLocationResult: Boolean = mtagsAnnot contains mtag_annot_aj_alt_location_result

    def markAsMethodInfoFrameDescriptorGetter(): Unit = mtagsAnnot += mtag_annot_aj_method_info_frame_descriptor_getter
    def isMethodInfoFrameDescriptorGetter: Boolean = mtagsAnnot contains mtag_annot_aj_method_info_frame_descriptor_getter

    /*****************************************************************************/

    def isMainMethod: Boolean =
      isStatic && (isPublic || getDeclaringClass.isCangjieType) && (js.jstrMainName == name) && env.isMainMethodSig(getSignature, getDeclaringClass)

    //------------------ Method util procedures -------------
    def clean(): Unit = {
      if (!getDeclaringClass.isAbsent) {
        // do not clean methods from absent classes as they can be cached in AMD64 compiler.
        abiSig = null
        throwsList = null
        numberInClassFile = -1
        cleanFEXTs()
      }
    }

    def isVirtual: Boolean = MethodTablesImpl.getVNum(this) >= 0

    /** Check whether AJ annotation `@Inline(forced = true)` is specified */
    def isAJInlineForced: Boolean = mtagsAnnot contains mtag_annot_aj_inline_forced

    /** Workaround for JET-17425 and JET-17444 */
    def isUnstableForwarder: Boolean = mtags contains mtag_unstable_forwarder

    /** Inline methods that should be inlined everywhere and removed from obj files, see JET-15699 */
    def isInlineAllAndRemove: Boolean = isAJInlineForced ||
      !O2Env.env.enabled(InlineOnlyForced) && isAJInline && !isExported && (isPrivate || isStatic || isConstructor)

    def getLLVMIndex: Int = this.llvmIdx

    def setLLVMIndex(llvmIdx: Int): Unit = {
      this.llvmIdx = llvmIdx
    }

    def getBytecodeSize: Int = this.bytecodeSize

    def setBytecodeSize(bytecodeSize: Int): Unit = {
      this.bytecodeSize = bytecodeSize
    }

    def setThrows(tarray: Array[Class]): Unit = {
      throwsList = tarray
    }

    def getThrows: Iterator[Class] = {
      if (throwsList == null) Iterator.empty else throwsList.iterator
    }

    def getThrowsCount: Int = {
      if (throwsList == null) 0 else throwsList.length
    }

    def markAsHookInvoker(): Unit = {
      mtagsAnnot += mtag_annot_ajhookinvoker
    }

    def isAjHookInvoker: Boolean = mtagsAnnot contains mtag_annot_ajhookinvoker

    def isAjReplaced: Boolean = {
      if (!(mtags contains mtag_ajreplaced_initialized)) {
        initializeAJReplaced(this.getDeclaringClass)
      }
      mtags contains mtag_ajreplaced
    }

    def markAsAjCallToManaged(className: XString, methodName: XString): Unit = {
      mtagsAnnot += mtag_annot_ajcalltomanaged
      addFEXT(newMethodFEXT(this.getDeclaringClass, className.replace('.', '/'), methodName, null, "@CallToManaged", allowNotFound = false), ajCallToManagedType.toByte)
    }

    def isAJDelayedIntrinsic: Boolean = mtagsAnnot contains mtag_annot_aj_delayed_intrinsic

    def markAsAJDelayedIntrinsic(className: XString, methodName: XString): Unit = {
      addFEXT(new DelayedIntrinsicFEXT(className, methodName), ajDelayedIntrinsic)
      mtagsAnnot += mtag_annot_aj_delayed_intrinsic
    }

    def getAJDelayedIntrinsicName: XString = fextOption[DelayedIntrinsicFEXT](ajDelayedIntrinsic).map(_.getName).orNull

    def getAJDelayedIntrinsicClassName: XString = fextOption[DelayedIntrinsicFEXT](ajDelayedIntrinsic).map(_.getClassName).orNull

    //--------- AJ @CallToManaged support -------------
    def getAjCallToManagedTarget: Method = {
      assert(this.isAjCallToManaged)
      getFEXT[MethodFEXT](ajCallToManagedType).getMethod ensuring (_ != null)
    }

    def isAjCallToManaged: Boolean = mtagsAnnot contains mtag_annot_ajcalltomanaged

    def markAsGetFlatThinIntrinsic(): Unit = {
      mtags += mtag_get_flat_thin_intrinsic
    }

    def isGetFlatThinIntrinsic: Boolean = mtags contains mtag_get_flat_thin_intrinsic

    def markAsThinUncheckedCast(): Unit = {
      mtags += mtag_thin_unchecked_cast
    }

    def isThinUncheckedCast: Boolean = mtags contains mtag_thin_unchecked_cast

    def markAsAjIndirectCall(): Unit = {
      mtagsAnnot += mtag_annot_ajindirectcall
    }

    def isAjIndirectCall: Boolean = mtagsAnnot contains mtag_annot_ajindirectcall

    def hasManagedExecEnv: Boolean = this.getCallConv.hasManagedExecEnv

    def isManaged: Boolean = this.getCallConv.isManaged

    def markAsNoCodeGen(): Unit = {
      mtags += mtag_no_code_gen
    }

    def isNoCodeGen: Boolean = mtags contains mtag_no_code_gen

    def markAsAjVersionedContext(): Unit = {
      mtagsAnnot += mtag_annot_aj_versioned_context
    }

    def isAjVersionedContext: Boolean = mtagsAnnot contains mtag_annot_aj_versioned_context

    def markAsVersionedMarker(marker: VersionedMarker): Unit = {
      assert(isDeclaredNative && !isVersionedMarker)
      mtagsAnnot += mtag_annot_aj_versioned_marker
      addFEXT(new VersionedMarkerFEXT(marker), versionedMarkerMethod)
    }

    def isVersionedMarker: Boolean = mtagsAnnot contains mtag_annot_aj_versioned_marker

    def getVersionedMarker: Option[VersionedMarker] = {
      if (isVersionedMarker) {
        Some(getFEXT[VersionedMarkerFEXT](versionedMarkerMethod).marker)
      } else {
        None
      }
    }

    def isThinConstructor: Boolean = mtagsAnnot contains mtag_annot_aj_thin_constructor

    def markAsThinConstructor(): Unit = {
      mtagsAnnot += mtag_annot_aj_thin_constructor
    }

    def markAsInterpretationLoop(): Unit = {
      mtagsAnnot += mtag_annot_aj_interpretation_loop
    }

    def isInterpretationLoop: Boolean = mtagsAnnot contains mtag_annot_aj_interpretation_loop

    def markAsNonThrowing(): Unit = {
      mtagsAnnot += mtag_annot_aj_non_throwing
    }

    def isNonThrowing: Boolean = mtagsAnnot contains mtag_annot_aj_non_throwing

    def markAsAjLongSafe(): Unit = {
      mtagsAnnot += mtag_annot_aj_long_safe
    }

    def isAjLongSafe: Boolean = mtagsAnnot contains mtag_annot_aj_long_safe

    def markAsCAnnotated(): Unit = {
      mtagsAnnot += mtag_annot_cj_c
    }

    def isCAnnotated: Boolean = mtagsAnnot contains mtag_annot_cj_c

    def getCFuncWrapperIndex: Int = {
      assert(this.isCAnnotated)
      intFEXT(cFuncWrapperIdx).get ensuring (_ >= 0)
    }

    def setCFuncWrapperIndex(idx: Int): Unit = {
      assert(this.isCAnnotated)
      addFEXT(new IntFEXT(idx ensuring (_ >= 0)), cFuncWrapperIdx)
    }

    def markAsAjGCAware(): Unit = {
      mtagsAnnot += mtag_annot_aj_gc_aware
    }

    def markAsAjStrictMemory(): Unit = {
      mtagsAnnot += mtag_annot_aj_strict_memory
    }

    def isAjStrictMemory: Boolean = mtagsAnnot contains mtag_annot_aj_strict_memory

    def markAsDirtyForClassGC(): Unit = {
      mtags += mtag_dirty_for_class_gc
    }

    def isDirtyForClassGC: Boolean = mtags contains mtag_dirty_for_class_gc

    def markAsNoTracedRegsOnEntry(): Unit = {
      mtags += mtag_no_traced_regs_on_entry
    }

    def isNoTracedRegsOnEntry: Boolean = mtags contains mtag_no_traced_regs_on_entry

    def markAsNoLocalGCPoints(): Unit = {
      mtags += mtag_no_local_gc_points
    }

    def isNoLocalGCPoints: Boolean = mtags contains mtag_no_local_gc_points
    // Note that we ignore current JCA file for methods from another components
    // because this property is a local property of a method.

    def markAsContainingMonitorOperations(): Unit = {
      mtags += mtag_contains_monitor_operations
    }

    def markAsAJDomain(): Unit = {
      mtags += mtag_aj_domain
    }

    def markAsJavaDomain(): Unit = {
      mtags += mtag_java_domain
    }

    def markAsCangjieDomain(): Unit = {
      mtags += mtag_cangjie_domain
    }

    def getDomain: Domain = {
      if (mtags contains mtag_aj_domain) {
        Domain.AJ
      } else if (mtags contains mtag_java_domain) {
        Domain.JAVA
      } else if (mtags contains mtag_cangjie_domain) {
        Domain.CANGJIE
      } else {
        null
      }
    }

    def markAsNoPreparationCheck(): Unit = {
      mtagsAnnot += mtag_annot_aj_no_preparation_check
    }

    def isAjNoPreparationCheck: Boolean = mtagsAnnot contains mtag_annot_aj_no_preparation_check

    def markAsAJRecordInitializer(): Unit = {
      mtagsAnnot += mtag_annot_aj_record_initializer
    }

    def isAJRecordInitializer: Boolean = mtagsAnnot contains mtag_annot_aj_record_initializer

    def containsMonitorOperations(): Boolean = mtags contains mtag_contains_monitor_operations

    def isDetectedFinal: Boolean = modifiers contains xot_final

    /*----------------------------------------------------------------*/
    def isDeclaredNative: Boolean = modifiers contains xot_native

    def isClinit: Boolean = {
      if (isWorkMode) {
        assert((mtags contains mtag_clinit) == (isStatic && (js.jstrClinit == name) && (getSignature == MethodSignature()(V))))
      }
      mtags contains mtag_clinit
    }

    def isPackageInit: Boolean = mtags contains mtag_package_init

    def markAsPackageInit(): Unit = mtags += mtag_package_init

    def isPackageLiteralInit: Boolean = mtags contains mtag_package_literal_init

    def markAsPackageLiteralInit(): Unit = mtags += mtag_package_literal_init

    def isGlobalInit: Boolean = mtags contains mtag_global_init

    def markAsGlobalInit(): Unit = mtags += mtag_global_init

    def isConstructor: Boolean = modifiers contains xot_constr

    def markAsConstructor(): Unit = modifiers += xot_constr

    def isRecordConstructor: Boolean = mtags contains mtag_record_constr

    def markAsRecordConstructor(): Unit = mtags += mtag_record_constr

    def isVarArgs: Boolean = modifiers contains xot_java_varargs

    def isSynchronized: Boolean = modifiers contains xot_synchron

    def getModifiers: Set32 = modifiers.toSet32 & xjRTS.MDF_METHOD_MASK

    //--------------- Method Access Methods -----------------
    // TODO: stop use java modifiers for jet-specific tags
    def getJavaModifiers: Set32 = modifiers.toSet32 & xjRTS.JMDF_METHOD_MASK

    def getCJModifiers: Set32 = {
      var result = modifiers.toSet32 & pcOModule.CangjieMethod.MODIFIERS
      if (isExternal) {
        result += pcOModule.CangjieMethod.FOREIGN_MODIFIER
      }
      result
    }

    def isAbstract: Boolean = modifiers contains xot_abstract

    def setCallConv(callconv: CallConv): Unit = {
      this.callconv = callconv
    }

    def getCallConv: CallConv = this.callconv

    def getPreservedParamsSet: Int = preservedParams.toInt

    def getAltLocationParamsSet: Int = altLocationParams.toInt

    def getSourceFullName: XString = {
      if (mtags contains mtag_has_source_full_name) {
        strFEXT(methodSourceFullName).get
      } else {
        null
      }
    }

    def setSourceFullName(fullName: XString): Unit = {
      assert(!(mtags contains mtag_has_source_full_name))
      mtags += mtag_has_source_full_name

      assert(fullName != null && !fullName.isEmpty)
      addFEXT(new StrFEXT(fullName), methodSourceFullName)
    }

    /** There is compiler-generated binary object (code or static data) for this method */
    def shouldBeGenerated: Boolean = {
      val c = getDeclaringClass
      if ((isStandalone && getCHIRDef.isEmpty) || isAbstract || isNoCodeGen || isClinit && !c.hasClinit || isInlineAllAndRemove || c.isAJArray || c.isCangjieArray || isVersionedMarker || isAJDelayedIntrinsic) {
        assert(!isAjCallToManaged)
        false
      } else {
        true
      }
    }

    def markAsForcedO1Compiled(): Unit = mtags += mtag_forced_o1_compiled

    def isForcedO1Compiled: Boolean = mtags contains mtag_forced_o1_compiled

    def initialCompilationMode: CompilationMode = if (isForcedO1Compiled) O1 else O2

    def markAsCompiledWithMiddleStage(): Unit = mtags += mtag_compiled_with_middle_stage

    def compiledWithMiddleStage: Boolean = mtags contains mtag_compiled_with_middle_stage

    private def shouldBeSerializedEvenInO1: Boolean =
      isInlineAllAndRemove ||                   // All methods which should be everywhere inline-able should be serialized
        getDeclaringClass.isJetRuntimeClass ||  // Workaround for methods from classes containing delayed intrinsics which may be recompiled during stdlib compilation
        compiledWithMiddleStage                 // Methods compiled with middle stage should be serialized because otherwise we will parse them twice

    def shouldBeSerialized: Boolean = (initialCompilationMode == O2) || shouldBeSerializedEvenInO1

    def isFinalize: Boolean = if (isCangjie) {
      name == js.jstrFinalizeCangjie &&
        // See JET-16563
        (getSignature == MethodSignature()(U) || getSignature == MethodSignature()(V))
    } else {
      name == js.jstrFinalize && getSignature == MethodSignature()(V)
    }

    def isUniversalGeneric: Boolean = mtags contains mtag_is_generic

    def markAsUniversalGeneric(): Unit = mtags += mtag_is_generic

    def addGenericInfo(info: GenericInfo): Unit = {
      assert(isUniversalGeneric)
      val fext = newGenericInfoFEXT(info.constraints)
      addFEXT(fext, genericInfo)
    }

    def getGenericInfo: GenericInfo = {
      assert(isUniversalGeneric)
      fextOption[GenericInfoFEXT](genericInfo).map(_.get).get
    }

    ///////////////////////////////////////////////////////////////////////////
    // JCA access support

    // Use [[Option]] because `null` is valid value for [[jca.PJCATREE]]
    private var jcaTreeCache: Option[jca.JCATree] = None

    def jcaOptionEnabled(name: String): Boolean = {
      if (jcaTreeCache.isEmpty) jcaTreeCache = Some(jca.findMethodJCA(this))
      jca.getOptValue(jcaTreeCache.get, name) == 1
    }

    def jcaOptionDisabled(name: String): Boolean = {
      if (jcaTreeCache.isEmpty) jcaTreeCache = Some(jca.findMethodJCA(this))
      jca.getOptValue(jcaTreeCache.get, name) == 0
    }
  }

  class VersionedMethodBody(mno: Int, _nameObj: pcNames.NAME) extends Member(mno, _nameObj, _lref = 0)


  object ClassloaderIDGetter {
    // Avoid bootstrap problem with ClassLoaderIDProvider.
    // See ClassloaderIDGetter.verify() uses.

    val UKNOWN_CLID = -1
    val SYSTEM_CLID = 0
    val EXT_CLID = 1
    val APP_CLID = 2
    val LAST_STD_CLID = 2

    def verify(): Unit = {
      if (languagePack.supports(JAVA)) {
        assert(UKNOWN_CLID == RTConst.ClassLoaderIDProvider.UKNOWN_CLID.intValue)
        assert(SYSTEM_CLID == RTConst.ClassLoaderIDProvider.SYSTEM_CLID.intValue)
        assert(EXT_CLID == RTConst.ClassLoaderIDProvider.EXT_CLID.intValue)
        assert(APP_CLID == RTConst.ClassLoaderIDProvider.APP_CLID.intValue)
        assert(LAST_STD_CLID == RTConst.ClassLoaderIDProvider.LAST_STD_CLID.intValue)
      }
    }
  }

  class ClassloaderIDGetter {
    def getID(clazz: Class): Int = {
      if (clazz.isSystemClass) {
        // Avoid bootstrap problem with ClassLoaderIDProvider.
        // See ClassloaderIDGetter.verify() uses.
        ClassloaderIDGetter.SYSTEM_CLID
      } else if (clazz.isFromExtensionClassloader()) {
        ClassloaderIDGetter.EXT_CLID
      } else if (clazz.isAbsent) {
        ClassloaderIDGetter.UKNOWN_CLID
      } else if (clazz.isAnonymous) {
        getID(clazz.hostClass)
      } else {
        ClassloaderIDGetter.APP_CLID
      }
    }
  }

  class ImportAdder {
    def addTypeImport(host: Class, type0: pc.SymType): Unit = {
      throw new AssertionError
    }
  }

  //--------------------------------------------------------------
  type SymLevel = UByte
  val sl_zero           : SymLevel = UByte(0)
  val sl_import         : SymLevel = UByte(1)
  val sl_methods        : SymLevel = UByte(2)
  val sl_fields         : SymLevel = UByte(3)
  val sl_codegen_info   : SymLevel = UByte(4)
  val sl_str_table      : SymLevel = UByte(5)
  val sl_last = sl_str_table


  private final class SymFileClassCompleter {
    private def requiredSymLevelFor(element: ETAG): SymLevel =
      SymFileLevelElements.indexWhere(_ contains element)

    private def getLoadedSymLevel(c: Class): SymLevel = {
      // Hand-rewritten "for" for performance. Keep it.
      var sl = sl_zero
      while (sl <= sl_last) {
        if ((c.elementsRequireCompletion & SymFileLevelElements(sl.toInt)) != ETAG_SET.empty) {
          assert(sl != sl_zero)
          return sl - UByte(1)
        }
        sl += UByte(1)
      }
      sl_last
    }

    def complete(c: Class, element: ETAG): Unit = O2Env.stage(Stage.Completer) {
      symCache_add(c)

      val firstLevel = this.getLoadedSymLevel(c) + UByte(1)
      val maxLevel = this.requiredSymLevelFor(element)

      val sr = getSymReader(c)
      assert(sr.context eq c)

      sr.openSymReader()
      sr.moveSymReaderTo(firstLevel)

      // Hand-rewritten "for" for performance. Keep it.
      var sl = firstLevel
      while (sl <= maxLevel) {
        sr.readLevel(sl)
        sl += UByte(1)
      }
      sr.closeSymReader()
    }
  }


  case class MemberRef private[pcOModule] (mno: Int, lref: Int) {
    private def get: Member = getClassRecord(mno).getMemberByLRef(lref)

    def getField = get.asInstanceOf[Field]
    def getMethod = get.asInstanceOf[Method]
  }

  private class IntFEXT extends FEXT {
    private[pcOModule] var value: Int = _
    def this(_value: Int) = { this(); value = _value }

    override def internalize(si: SymIO): Unit = { value = si.curFile.readUInt() }
    override def externalize(si: SymIO): Unit = si.curFile.writeUInt(value)
  }

  private class StrFEXT extends FEXT {
    private[pcOModule] var str: XString = _
    def this(_str: XString) = { this(); str = _str }

    override def internalize(si: SymIO): Unit = { str = si.curFile.readJString() }
    override def externalize(si: SymIO): Unit = si.curFile.writeJString(str)
  }

  private class DebugTypeFEXT extends FEXT {
    private[pcOModule] var debugType: DebugType = _
    def this(_debugType: DebugType) = { this(); debugType = _debugType }

    override def internalize(si: SymIO): Unit = { debugType = DebugType.deserialize(si.curFile.readInt, si.curFile.readJString) }
    override def externalize(si: SymIO): Unit = debugType.serialize(si.curFile.writeInt, si.curFile.writeJString)
  }

  private class CHIRDefFEXT extends FEXT {
    private[pcOModule] var src: XString = _
    private[pcOModule] var id: Int = _
    def this(_src: XString, _id: Int) = { this(); src = _src; id = _id }

    override def internalize(si: SymIO): Unit = { src = si.curFile.readJString(); id = si.curFile.readUInt() - 1 }
    override def externalize(si: SymIO): Unit = { si.curFile.writeJString(src); si.curFile.writeUInt(id + 1) }
  }

  private class CHIRVTableFEXT extends FEXT {
    private[pcOModule] var vtable: CHIRVTable = _
    private[pcOModule] var unresolvedVTable: CHIRVTable = _
    private[pcOModule] var unresolvedImpls: Seq[Seq[Option[MethodFEXT]]] = _
    def this(vtable: CHIRVTable) = { this(); this.vtable = vtable }

    def getVTable: CHIRVTable = {
      if (vtable == null) {
        vtable = CHIRVTable(
          unresolvedVTable.extDefs.zip(unresolvedImpls).map { (extDef, extDefImpls) =>
            extDef.copy(funcTable = extDef.funcTable.zip(extDefImpls).map { (entry, entryImpl) =>
              entry.copy(impl = entryImpl.map(f => methodByO2Object(f.getMethod)))
            })
          }
        )
      }
      vtable
    }

    override def internalize(si: SymIO): Unit = {
      unresolvedVTable = CHIRVTable(
        si.readSeq { () =>
          CHIRVTable.ExtDef(
            si.readSignatureType(),
            si.readSeq { () =>
              CHIRVTable.Entry(
                si.readString(),
                si.readMethodSignature(),
                si.readSeq(si.readSignatureType),
                None, // will be resolved later
                Modifiers(si.readInt()),
                si.readMethodSignature(),
                si.readSignatureType(),
                si.readSignatureType()
              )
            }
          )
        }
      )
      unresolvedImpls = si.readSeq { () =>
        si.readSeq { () =>
          Option.when(si.readBool()) {
            val fext = new MethodFEXT
            fext.internalize(si)
            fext
          }
        }
      }
    }
    override def externalize(si: SymIO): Unit = {
      si.writeSeq(vtable.extDefs) { extDef =>
        si.writeSignatureType(extDef.extType)
        si.writeSeq(extDef.funcTable) { entry =>
          si.writeString(entry.name)
          si.writeMethodSignature(entry.sig)
          si.writeSeq(entry.genericParams)(si.writeSignatureType)
          si.writeInt(entry.modifiers.value)
          si.writeMethodSignature(entry.originalSig)
          si.writeSignatureType(entry.instantiatedRefType)
          si.writeSignatureType(entry.instantiatedReturnType)
        }
      }
      si.writeSeq(vtable.extDefs) { extDef =>
        si.writeSeq(extDef.funcTable) { entry =>
          entry.impl match {
            case None =>
              si.writeBool(false)
            case Some(m) =>
              si.writeBool(true)
              val fext = new MethodFEXT
              fext.method = methodToO2Method(m)
              fext.externalize(si)
          }
        }
      }
    }
  }

  private class CangjieEnumInfoFEXT extends FEXT {
    import CangjieEnumInfo.*

    private[pcOModule] var info: CangjieEnumInfo = _
    def this(info: CangjieEnumInfo) = { this(); this.info = info }

    override def internalize(si: SymIO): Unit = {
      info = CangjieEnumInfo(si.readSeq(() => CangjieEnumInfo.Constructor(si.readSeq(si.readSignatureType))))
    }
    override def externalize(si: SymIO): Unit = {
      si.writeSeq(info.constructors) { c =>
        si.writeSeq(c.params)(si.writeSignatureType)
      }
    }
  }

  private class ClassFEXT extends FEXT {
    private[pcOModule] var class0: Class = _
    def this(_class0: Class) = { this(); class0 = _class0 }

    override def internalize(si: SymIO): Unit = { class0 = si.asInstanceOf[SymReader].getClassByRef() }
    override def externalize(si: SymIO): Unit = si.putClassRef(class0)
  }

  private class MethodFEXT extends FEXT {
    private[pcOModule] var method: Method = _ // resolved entry
    // if method is not already resolved,
    // it can be resolved from the below information
    private[pcOModule] var class0: Class = _
    private[pcOModule] var mname: XString = _
    private[pcOModule] var msig: MethodSignature = _
    private[pcOModule] var mlref: Int = _
    // Suppress error if method cannot be found.
    private[pcOModule] var allowNotFound: Boolean = _

    override def internalize(si: SymIO): Unit = {
      // we cannot resolve method reference right now,
      // because we could be in the process of methods reading.
      // So postpone resolution at get stage
      val b = si.curFile.read()

      if (b == ol_null) {
        this.allowNotFound = true
        this.mlref = -1
        return
      }

      if (b == ol_foreign) {
        this.class0 = si.asInstanceOf[SymReader].getClassByRef()
      } else {
        assert(b == ol_own)
        this.class0 = si.context
      }
      this.mlref = si.curFile.readInt()
    }

    override def externalize(si: SymIO): Unit = {
      val m = getMethod
      if (m == null) {
        assert(allowNotFound)
        si.curFile.write(ol_null)
      } else {
        val c = m.getDeclaringClass
        if (c ne si.context) {
          si.curFile.write(ol_foreign)
          si.putClassRef(c)
        } else {
          si.curFile.write(ol_own)
        }
        si.curFile.writeInt(m.lref ensuring (_ > 0))
      }
    }

    def getMethod: Method = {
      if (method == null || method.nameObj == null) {
        // already dropped method
        // resolve method
        if (mname != null) {
          method = class0.findLocalMethod(mname, msig)
          if (method == null) {
            if (allowNotFound) {
              return null
            }
            env.errors.fault(ErrMsg980, mname, class0.name)
          }
        } else {
          if (mlref == -1) {
            assert(allowNotFound)
            return null
          }
          method = class0.getMemberByLRef(mlref).asInstanceOf[Method]
        }
      }
      method
    }

  }

  private class CJAnnotationFactoriesForParametersFEXT extends FEXT {

    private[pcOModule] var factories: Array[MethodFEXT] = _

    override def internalize(si: SymIO): Unit = {
      val size = si.curFile.read()
      if (size == ol_null) {
        return
      }
      factories = Array.tabulate(size) { _ =>
        val fext = new MethodFEXT
        fext.internalize(si)
        fext
      }
    }

    override def externalize(si: SymIO): Unit = {
      if (factories == null) {
        si.curFile.write(ol_null)
        return
      }
      assert(factories.nonEmpty)
      si.curFile.write(factories.length)
      factories.foreach(_.externalize(si))
    }

    def getFactories: Array[Method] = {
      factories.map(_.getMethod)
    }
  }

  private def newGenericInfoFEXT(constraints: Seq[GenericInfo.Constraint]): GenericInfoFEXT = {
    val fext = new GenericInfoFEXT
    fext.constraints = constraints
    fext
  }

  private class GenericInfoFEXT extends FEXT {
    private[pcOModule] var constraints: Seq[GenericInfo.Constraint] = _

    override def internalize(si: SymIO): Unit = {
      val reader = si.asInstanceOf[SymReader]

      def readConstraint(): GenericInfo.Constraint = {
        val idx = reader.readInt()
        val upperBounds: Seq[SignatureType] = reader.readSeq(reader.readSignatureType)
        GenericInfo.Constraint(LocalTypeVariable(idx), upperBounds)
      }

      constraints = reader.readSeq(readConstraint)
    }

    override def externalize(si: SymIO): Unit = {
      val writer = si.asInstanceOf[SymWriter]
      writer.writeSeq(constraints) { c =>
        writer.writeInt(c.typeVariable.idx)
        writer.writeSeq(c.upperBounds)(writer.writeSignatureType)
      }
    }

    def get: GenericInfo = GenericInfo(constraints)
  }

  private def newSignatureTypeFEXT(sig: SignatureType): SignatureTypeFEXT = {
    val fext = SignatureTypeFEXT()
    fext.sig = sig
    fext
  }

  private class SignatureTypeFEXT extends FEXT {
    private[pcOModule] var sig: SignatureType = _

    override def internalize(si: SymIO): Unit = {
      val reader = si.asInstanceOf[SymReader]
      sig = reader.readSignatureType()
    }

    override def externalize(si: SymIO): Unit = {
      val writer = si.asInstanceOf[SymWriter]
      writer.writeSignatureType(sig)
    }
  }

  private def newLambdaInfoFEXT(clazz: Class, info: LambdaInfo): LambdaInfoFEXT = {
    val fext = new LambdaInfoFEXT
    fext.capturingClass = typeToO2Class(info.capturingClass)
    fext.samClass = typeToO2Class(info.samClass)
    fext.samMethodName = info.samMethodName
    fext.samMethodType = info.samMethodType
    val implMethod = getO2Method(info.impl.member.asInstanceOf[SymMethod])
    fext.implMethod = newMethodFEXT(clazz, implMethod.getDeclaringClass.name, implMethod.name, implMethod.getSignature, null, allowNotFound = true)
    fext.implRefKind = info.impl.refKind
    fext.instantiatedMethodType = info.instantiatedMethodType
    fext
  }

  private class LambdaInfoFEXT extends FEXT {

    // Class where lambda is defined
    private[pcOModule] var capturingClass: Class = _

    // Single Abstract Method (SAM) reference
    private[pcOModule] var samClass: Class = _
    private[pcOModule] var samMethodName: XString = _
    private[pcOModule] var samMethodType: MethodType = _

    // Implementation MethodHandle
    private[pcOModule] var implMethod: MethodFEXT = _
    private[pcOModule] var implRefKind: ReferenceKind = _

    // Instantiated method type
    private[pcOModule] var instantiatedMethodType: MethodType = _

    override def internalize(si: SymIO): Unit = {
      val reader = si.asInstanceOf[SymReader]
      capturingClass = reader.getClassByRef()
      samClass = reader.getClassByRef()
      samMethodName = reader.readJString()
      samMethodType = MethodType(reader.readMethodSignature())
      implMethod = new MethodFEXT
      implMethod.internalize(reader)
      implRefKind = ReferenceKind.fromOrdinal(reader.curFile.readPackedInt())
      instantiatedMethodType = MethodType(reader.readMethodSignature())
    }

    override def externalize(si: SymIO): Unit = {
      val writer = si.asInstanceOf[SymWriter]
      writer.putClassRef(capturingClass)
      writer.putClassRef(samClass)
      writer.writeJString(samMethodName)
      writer.writeMethodSignature(samMethodType.signature)
      implMethod.externalize(writer)
      writer.curFile.writePackedInt(implRefKind.ordinal)
      writer.writeMethodSignature(instantiatedMethodType.signature)
    }

    def get: LambdaInfo = {
      LambdaInfo(
        classByO2Object(capturingClass),
        classByO2Object(samClass),
        samMethodName,
        samMethodType,
        new MethodHandle(implRefKind, methodByO2Object(implMethod.getMethod)),
        instantiatedMethodType
      )
    }
  }

  private class DelayedIntrinsicFEXT extends FEXT {
    private[pcOModule] var name: XString = _
    private[pcOModule] var className: XString = _

    def this(_className: XString, _name: XString) = {
      this()
      name = _name
      className = _className
    }

    override def internalize(si: SymIO): Unit = {
      name = si.curFile.readJString()
      className = si.curFile.readJString()
    }

    override def externalize(si: SymIO): Unit = {
      si.curFile.writeJString(name)
      si.curFile.writeJString(className)
    }

    def getName: XString = name

    def getClassName: XString = className

  }


  private class AnnotationAttr extends FEXT {
    private[pcOModule] var attr: jcp.PtrAbstractAnnotationAttr = _

    override def internalize(si: SymIO): Unit = {
      si.curFile.read() match {
        case 0 =>
          val rtann = new jcp.PtrAnnotationsAttr()
          this.attr = rtann
          readRuntimeVisibleAnnotationAttr(si.curFile, rtann)
        case 1 =>
          val rttypeann = new jcp.PtrTypeAnnotationsAttr()
          this.attr = rttypeann
          readRuntimeVisibleTypeAnnotationAttr(si.curFile, rttypeann)
        case 2 =>
          val rtpann = new jcp.PtrParameterAnnotationsAttr()
          this.attr = rtpann
          readRuntimeVisibleParameterAnnotationAttr(si.curFile, rtpann)
        case 3 =>
          val adann = new jcp.PtrAnnotationDefaultAttr()
          this.attr = adann
          readAnnotationDefaultAttr(si.curFile, adann)
      }
    }

    override def externalize(si: SymIO): Unit = {
      this.attr match {
        case a: jcp.PtrAnnotationsAttr =>
          si.curFile.write(0)
          writeRuntimeVisibleAnnotationAttr(si.curFile, a)
        case a: jcp.PtrTypeAnnotationsAttr =>
          si.curFile.write(1)
          writeRuntimeVisibleTypeAnnotationAttr(si.curFile, a)
        case a: jcp.PtrParameterAnnotationsAttr =>
          si.curFile.write(2)
          writeRuntimeVisibleParameterAnnotationAttr(si.curFile, a)
        case a: jcp.PtrAnnotationDefaultAttr =>
          si.curFile.write(3)
          writeAnnotationDefaultAttr(si.curFile, a)
      }
    }
  }

  private class JcaKnownSafeAttr extends FEXT {
    private[pcOModule] var retParamNum: Int = _ // returnable param num (0=this, 1,..). or -1 when no params may be returned

    override def internalize(si: SymIO): Unit = {
      this.retParamNum = si.curFile.readPackedInt()
    }

    /** Escape analyse stores `JsaKnowsSafeInfo` on some methods: --- */
    override def externalize(si: SymIO): Unit = {
      si.curFile.writePackedInt(this.retParamNum)
    }
  }

  case class VerifyError(errcode: VerificationError.ExceptionKind, errmsg: XString) {
    def getRTCode: xjRTS.ClassCode = {
      this.errcode match {
        case NoClassDefFoundError                         => xjRTS.X2C_NoClassDefFoundError
        case IncompatibleClassChangeError                 => xjRTS.X2C_IncompatibleClassChangeError
        case VerificationError.ExceptionKind.VerifyError  => xjRTS.X2C_VerifyError
        case ClassFormatError                             => xjRTS.X2C_ClassFormatError
        case UnsupportedClassVersionError                 => xjRTS.X2C_UnsupportedClassVersionError
        case ClassCircularityError                        => xjRTS.X2C_ClassCircularityError
        case IllegalAccessError                           => xjRTS.X2C_IllegalAccessError
        case FatalError =>
          // TODO: add X2C_FatalError. getRTCode is only used in JavaDesc to
          // mark class to throw an error when it will be loaded dynamically.
          // FatalError code is used to mark classes that are not in environment and
          // unlikely to be used dynamically.
          xjRTS.X2C_VerifyError
        case _ => shouldNotReachHere()
      }
    }
  }

  private class VerErrFEXT extends FEXT {
    private[pcOModule] var err: VerifyError = _

    override def internalize(si: SymIO): Unit = {
      this.err = VerifyError(VerificationError.ExceptionKind.byIndex(si.curFile.read()), si.curFile.readJString())
    }

    override def externalize(si: SymIO): Unit = {
      si.curFile.write(this.err.errcode.ordinal)
      si.curFile.writeJString(this.err.errmsg)
    }
  }

  class EnclosingMethod(var enclosingClass: Class, var methodName: XString, var methodSig: XString)

  private class EnclosingMethodFEXT extends FEXT {
    private[pcOModule] var encmeth: EnclosingMethod = _

    override def internalize(si: SymIO): Unit = {
      encmeth = new EnclosingMethod(si.asInstanceOf[SymReader].getClassByRef(), si.readJString(), si.readJString())
    }

    override def externalize(si: SymIO): Unit = {
      si.putClassRef(encmeth.enclosingClass)
      si.writeJString(encmeth.methodName)
      si.writeJString(encmeth.methodSig)
    }
  }

  private class SizeAndAlignment extends FEXT {
    private[pcOModule] var size: Int = _
    private[pcOModule] var alignment: Int = _

    override def internalize(si: SymIO): Unit = {
      this.size = si.curFile.readPackedInt()
      this.alignment = si.curFile.readPackedInt()
    }

    override def externalize(si: SymIO): Unit = {
      si.curFile.writePackedInt(this.size)
      si.curFile.writePackedInt(this.alignment)
    }
  }

  private class MethodParametersFEXT extends FEXT {
    private[pcOModule] var methodParameters: Array[jcp.MethodParameter] = _
    private[pcOModule] var lvtConverted: Boolean = _

    override def internalize(si: SymIO): Unit = {
      this.lvtConverted = si.curFile.read() == 1
      val len = si.curFile.readPackedInt()
      this.methodParameters = Array.fill[jcp.MethodParameter](len)(new jcp.MethodParameter())
      for (i <- this.methodParameters.indices) {
        if (si.curFile.read() == 1) {
          this.methodParameters(i).name = si.curFile.readJString()
        }
        this.methodParameters(i).accessFlags = si.curFile.readPackedInt().toUShort
      }
    }

    override def externalize(si: SymIO): Unit = {
      if (this.lvtConverted) {
        si.curFile.write(1)
      } else {
        si.curFile.write(0)
      }
      si.curFile.writePackedInt(this.methodParameters.length)
      for (i <- this.methodParameters.indices) {
        if (this.methodParameters(i).name == null) {
          si.curFile.write(0)
        } else {
          si.curFile.write(1)
          si.curFile.writeJString(this.methodParameters(i).name)
        }
        si.curFile.writePackedInt(this.methodParameters(i).accessFlags.toInt)
      }
    }
  }

  private class InlineIfConstParamsFEXT extends FEXT {
    private[pcOModule] var paramsIndices: Array[Int] = _

    override def internalize(si: SymIO): Unit = {
      val len = si.curFile.readPackedInt()
      this.paramsIndices = new Array[Int](len)
      for (i <- this.paramsIndices.indices) {
        this.paramsIndices(i) = si.curFile.read()
      }
    }

    override def externalize(si: SymIO): Unit = {
      si.curFile.writePackedInt(this.paramsIndices.length)
      for (i <- this.paramsIndices.indices) {
        val x = this.paramsIndices(i)
        assert(0 <= x && x <= 0xFF)
        si.curFile.write(x)
      }
    }
  }

  private class DataAnnotFEXT extends FEXT {
    var dataStr: XString = _

    def this(dataStr: XString) = {
      this()
      this.dataStr = dataStr
    }

    override def internalize(si: SymIO): Unit = dataStr = si.readJString()

    override def externalize(si: SymIO): Unit = si.writeJString(dataStr)
  }

  class VerificationPair {
    /*RO*/ var from: Class = _
    /*RO*/ var to0: Class = _
    /*RO*/ var errmsg: XString = _
    /*RO*/ var next: VerificationPair = _
  }

  private class VerPairFEXT extends FEXT {
    private[pcOModule] var pairs: VerificationPair = _

    override def internalize(si: SymIO): Unit = {
      val numOfPairs = si.curFile.readPackedInt()
      var head: VerificationPair = null
      var tail: VerificationPair = null
      val sr = si.asInstanceOf[SymReader]
      for (_ <- 0 until numOfPairs) {
        val vp = newVerificationPair(sr.readClassRefSelfIncluded(), sr.readClassRefSelfIncluded(), sr.readJString())
        if (head == null) {
          head = vp
        } else {
          tail.next = vp
        }
        tail = vp
      }
      this.pairs = head
    }

    override def externalize(si: SymIO): Unit = {
      var numOfPairs = 0
      var vp = this.pairs
      while (vp != null) {
        vp = vp.next
        numOfPairs += 1
      }
      si.curFile.writePackedInt(numOfPairs)
      vp = this.pairs
      while (vp != null) {
        si.writeClassRefSelfIncluded(vp.from)
        si.writeClassRefSelfIncluded(vp.to0)
        si.writeJString(vp.errmsg)
        vp = vp.next
      }
    }
  }

  private class VersionedMarkerFEXT(var marker: VersionedMarker) extends FEXT {
    override def internalize(si: SymIO): Unit = {
      assert(marker == null)
      marker = VersionedMarker(si.curFile.readJString(), si.curFile.readJString(), si.curFile.readJString(), si.curFile.readJString())
    }

    override def externalize(si: SymIO): Unit = {
      si.curFile.writeJString(marker.declaringClassNameGCAware)
      si.curFile.writeJString(marker.nameGCAware)
      si.curFile.writeJString(marker.declaringClassNameUnmanaged)
      si.curFile.writeJString(marker.nameUnmanaged)
    }
  }

  private class ClassSearchResult extends Object {
    private[pcOModule] var result: Class = _
  }

  abstract class SymIO(val context: Class) {
    private[pcOModule] var filePlace: xPDB.Placeholder = _
    private[pcOModule] var coordn: XString = _
    private[pcOModule] var curFile: xfs.SymFile = _
    private[pcOModule] var positions: Array[Int] = Array.fill[Int](sl_last.toInt + 1)(-1)


    def writeProcRest(m: Method): Unit = {
      this.curFile.writeSet64(m.mtags.toSet64)
      this.curFile.writeSet(m.mtagsAnnot.toSet32)
      this.curFile.write(m.getCallConv.ordinal)
      this.curFile.writeSet(m.preservedParams)
      this.curFile.writeSet(m.altLocationParams)
      this.curFile.writePackedInt(m.getBytecodeSize)
      this.curFile.writePackedInt(m.getLLVMIndex)

      writeMethodSignature(m.abiSig)
      writeSpecialParamSet(m.specialParamSet)

      this.curFile.writeUInt(m.getThrowsCount)
      m.getThrows foreach writeClassRefSelfIncluded
    }

    def writeMember(o: Member): Unit = {
      val value = o match {
        case sf: StaticField if sf.value != null => sf.value
        case _ => null
      }

      import MemberKind.*
      val memberKind = (o match {
        case _: Method         => METHOD
        case _: StaticField    => STATIC_FIELD
        case _: InstanceField  => INSTANCE_FIELD
      }).ordinal + 1

      val mode = if (value != null) memberKind + oa_val else memberKind
      curFile.write(mode)
      curFile.writeInt(o.lref)
      writeJString(o.name)
      writeSignature(o.getSignature)

      curFile.writeSet(o.modifiers.toSet32)
      curFile.writeSet(o.memtags.toSet32)
      curFile.writePackedInt(o.numberInClassFile)

      o match {
        case o: Field =>
          curFile.writeSet(o.ftags.toSet32)
          curFile.writeUInt(o.getOffset)
        case _ =>
      }
      writeFEXT(o)
      o match {
        case o: Method =>
          writeProcRest(o)
        case o: Field =>
          writeSignatureType(o.sig)
          if (value != null) {
            curFile.writeConstValue(value)
          }
      }
    }

    def writeBool(x: Boolean): Unit = {
      curFile.writePackedInt(if (x) 1 else 0)
    }

    def writeInt(v: Int): Unit = {
      curFile.writeInt(v)
    }

    def writeSeq[T](xs: Seq[T])(writeOne: T => Unit): Unit = {
      curFile.writePackedInt(xs.size)
      xs foreach writeOne
    }

    def writeReferenceType(t: ReferenceType): Unit = {
      writeSignatureType(t.sigType)
    }

    def writeSignature(sig: Signature): Unit = sig match {
      case sig: SignatureType =>
        curFile.writePackedInt(0)
        writeSignatureType(sig)
      case sig: MethodSignature =>
        curFile.writePackedInt(1)
        writeMethodSignature(sig)
    }

    /** Correspond to [[SymReader.readSignatureType]] */
    def writeSignatureType(t: SignatureType): Unit = {
      import SignatureType.*
      t match {
        case t: Primitive =>
          curFile.writePackedInt(0)
          curFile.writePackedInt(t.id)
        case JavaArray(baseType, dimNum) =>
          curFile.writePackedInt(1)
          writeSignatureType(baseType)
          curFile.writePackedInt(dimNum)
        case t: Reference with NameBased =>
          curFile.writePackedInt(2)
          curFile.writeJString(XString(t.name))
          writeBool(t.jbc)
        case t: NamedRecord =>
          curFile.writePackedInt(3)
          curFile.writeJString(XString(t.name))
        case t: Reference with SymTypeBased =>
          curFile.writePackedInt(4)
          writeClassRefSelfIncluded(typeToO2Class(t.symType))
        case t: SymRecord =>
          curFile.writePackedInt(5)
          writeClassRefSelfIncluded(typeToO2Class(t.symType))
        case BString =>
          curFile.writePackedInt(6)
        case CPointer(pointee) =>
          curFile.writePackedInt(7)
          writeSignature(pointee)
        case ArraySlice(elemType) =>
          curFile.writePackedInt(8)
          writeSignatureType(elemType)
        case CangjieEnumWrapper(baseType, name) =>
          curFile.writePackedInt(9)
          writeSignatureType(baseType)
          writeJString(xstr(name))
        case VArray(elemType, length) =>
          curFile.writePackedInt(10)
          writeSignatureType(elemType)
          curFile.write8(length)
        case t: InstantiatedReference =>
          curFile.writePackedInt(11)
          writeJString(xstr(t.name))
          writeSeq(t.instantiatedTypeParameters)(writeSignatureType)
        case t: InstantiatedRecord =>
          curFile.writePackedInt(12)
          writeJString(xstr(t.name))
          writeSeq(t.instantiatedTypeParameters)(writeSignatureType)
        case t: LocalTypeVariable =>
          curFile.writePackedInt(13)
          curFile.writePackedInt(t.idx)
        case CangjieArray(elemType) =>
          curFile.writePackedInt(14)
          writeSignatureType(elemType)
        case ThisTypeInfo =>
          curFile.writePackedInt(15)
        case NullableWrapper(baseType) =>
          curFile.writePackedInt(16)
          writeSignatureType(baseType)
        case NonNullableWrapper(baseType) =>
          curFile.writePackedInt(17)
          writeSignatureType(baseType)
        case t: ClassTypeVariable =>
          curFile.writePackedInt(18)
          curFile.writePackedInt(t.idx)
        case t: Tuple =>
          curFile.writePackedInt(19)
          writeSeq(t.params)(writeSignatureType)
        case t: Box =>
          curFile.writePackedInt(20)
          writeSignatureType(t.base)
        case t: ZeroSizedEnum =>
          curFile.writePackedInt(21)
          writeJString(xstr(t.name))
          writeSeq(t.params)(writeSignatureType)
        case t: PrimitiveBasedEnum =>
          curFile.writePackedInt(22)
          writeJString(xstr(t.name))
          writeSeq(t.params)(writeSignatureType)
        case t: ClassBasedEnum =>
          curFile.writePackedInt(23)
          writeJString(xstr(t.name))
          writeSeq(t.params)(writeSignatureType)
        case t: UnionBasedEnum =>
          curFile.writePackedInt(24)
          writeJString(xstr(t.name))
          writeSeq(t.params)(writeSignatureType)
        case t: OptionLikeEnum =>
          curFile.writePackedInt(25)
          writeJString(xstr(t.name))
          writeSeq(t.params)(writeSignatureType)
          writeSignatureType(t.someType)
      }
    }

    def writeMethodSignature(sig: MethodSignature): Unit = {
      writeSignatureType(sig.returnType)
      writeSeq(sig.parameterTypes.toSeq)(writeSignatureType)
    }

    def writeSpecialParamSet(set: MethodType.SpecialParamSet): Unit = {
      val Array(mask) = set.toBitSet.toBitMask
      curFile.write8(mask)
    }

    def readFEXT(o: AttrAPI.Attributable): Unit = {
      var type0 = curFile.read()
      while (type0 != 0) {
        val fext = newFEXT(type0)
        fext.internalize(this)
        o.tryAddFEXT(fext, type0.toByte)
        type0 = curFile.read()
      }
    }

    def writeFEXT(o: AttrAPI.Attributable): Unit = {
      for (fext <- o.fexts) {
        this.curFile.write(fext.kind & 0xFF)
        fext.externalize(this)
      }
      this.curFile.write(0)
    }

    def writeInternalizableName(name: pcNames.NAME): Unit = {
      pcOModule.writeInternalizableName(name, this.curFile)
    }

    def writeJString(str: XString): Unit = {
      if (str == null) {
        this.curFile.writeJString(js.jstrEmpty)
      } else {
        this.curFile.writeJString(str)
      }
    }

    def writeString(str: String): Unit = {
      if (str == null) {
        this.curFile.writeString("")
      } else {
        this.curFile.writeString(str)
      }
    }

    def writeClassRefSelfIncluded(imp: Class): Unit = {
      val c = this.context
      if (c eq imp) {
        this.curFile.writeUInt(0)
      } else {
        this.putClassRef(imp)
      }
    }

    def putClassRef(imp: Class): Unit = {
      // TODO: make sw: SymWriter
      val c = context
      val n = c.getClassSymRef(imp)
      if (n == -1) {
        env.errors.fault(ErrMsg209, imp.name, c.name)
      }
      curFile.writeUInt(n)
    }

    def getClassByRef(): Class = context.resolveClassSymRef(curFile.readUInt())

    def setLevelPosition(sl: SymLevel): Unit = {
      this.positions(sl.toInt) = this.curFile.getPosAsInt
    }

    def readJString(): XString = this.curFile.readJString()

    def readString(): String = readJString().toString

    def readBool(): Boolean = curFile.readPackedInt() != 0

    def readInt(): Int = curFile.readInt()

    def readSeq[T](readOne: () => T): Seq[T] = {
      Seq.tabulate(curFile.readPackedInt())(_ => readOne())
    }

    def readReferenceType[T <: ReferenceType](companion: CompiledType.Companion[T]): T = {
      companion(readSignatureType())
    }

    def readSignature(): Signature = {
      curFile.readPackedInt() match {
        case 0 => readSignatureType()
        case 1 => readMethodSignature()
      }
    }

    def readSignatureType(): SignatureType = {
      import SignatureType.*
      curFile.readPackedInt() match {
        case 0 => Primitive.byID(curFile.readPackedInt())
        case 1 => JavaArray(readSignatureType(), curFile.readPackedInt())
        case 2 => Reference(curFile.readJString().toString, readBool())
        case 3 => Record(curFile.readJString().toString)
        case 4 => Reference(readClassRefSelfIncluded().symType)
        case 5 => Record(readClassRefSelfIncluded().symType)
        case 6 => BString
        case 7 => CPointer(readSignature())
        case 8 => ArraySlice(readSignatureType())
        case 9 => CangjieEnumWrapper(readSignatureType().asInstanceOf[CangjieEnumWrapper.Base], readJString().toString)
        case 10 => VArray(readSignatureType(), curFile.read8())
        case 11 => InstantiatedReference(curFile.readJString().toString, readSeq(readSignatureType))
        case 12 => InstantiatedRecord(curFile.readJString().toString, readSeq(readSignatureType))
        case 13 => LocalTypeVariable(curFile.readPackedInt())
        case 14 => CangjieArray(readSignatureType())
        case 15 => ThisTypeInfo
        case 16 => NullableWrapper(readSignatureType().asInstanceOf[NullableWrapper.Base])
        case 17 => NonNullableWrapper(readSignatureType().asInstanceOf[NonNullableWrapper.Base])
        case 18 => ClassTypeVariable(curFile.readPackedInt())
        case 19 => Tuple(readSeq(readSignatureType))
        case 20 => Box(readSignatureType())
        case 21 => ZeroSizedEnum(curFile.readJString().toString, readSeq(readSignatureType))
        case 22 => PrimitiveBasedEnum(curFile.readJString().toString, readSeq(readSignatureType))
        case 23 => ClassBasedEnum(curFile.readJString().toString, readSeq(readSignatureType))
        case 24 => UnionBasedEnum(curFile.readJString().toString, readSeq(readSignatureType))
        case 25 => OptionLikeEnum(curFile.readJString().toString, readSeq(readSignatureType), readSignatureType())
      }
    }

    def readMethodSignature(): MethodSignature = {
      MethodSignature(readSignatureType(), readSeq(readSignatureType))
    }

    def readClassRefSelfIncluded(): Class = {
      val ref = this.curFile.readUInt()
      if (ref == 0) {
        return this.context
      }
      this.context.resolveClassSymRef(ref)
    }
  }

  private enum MemberKind {
    case METHOD, STATIC_FIELD, INSTANCE_FIELD
  }

  private class SymWriter(_class: Class) extends SymIO(_class) {
    private[pcOModule] var sw: Byte = sw_none.toByte /* see sw_* consts */

    def writeMagic(): Unit = {
      this.curFile.writePackedInt(sym_magic)
      this.curFile.writePackedInt(xcVersion.SymFileVersion)
    }

    /** IMPORTANT: this format is also read in [[getOptComponentsInfo]]! */
    def writeSym(): Unit = {
      val c = this.context

      assert(c.persistentImportNum == 0)
      c.setPersistentImportNum()

      this.setLevelPosition(sl_zero)
      this.writePlainFields()
      this.writeImport()
      this.writeSupers()

      this.setLevelPosition(sl_import)

      if (!c.isUnloadable) {
        assert(c.virtualNumbersAreNumerated)
        assert(c.instanceLayoutIsNumerated)
        assert(c.staticLayoutIsNumerated)
      }

      this.setLevelPosition(sl_methods)
      this.writeMembers(c.declaredMethods)

      this.setLevelPosition(sl_fields)
      this.writeMembers(c.declaredFields)

      this.setLevelPosition(sl_codegen_info)
      this.writeAttributes()
      this.writeInnerClasses()
      this.writeIMTSlots()

      if (c.isCangjieType) {
        this.writeDebugType(c.llvmDebugType)
      }

      this.setLevelPosition(sl_str_table)
      this.writeStringTable()
    }

    def writeMembers(members: Iterator[Member]): Unit = {
      members foreach writeMember
      curFile.write(0)
    }

    def writeAttributes(): Unit = {
      writeFEXT(context)
    }

    def writeInnerClasses(): Unit = {
      val c = this.context
      val file = this.curFile

      val icArray = c.getInnerClasses
      if (icArray == null) {
        file.writePackedInt(0)
      } else {
        file.writePackedInt(icArray.size)
        for (ic <- icArray) {
          this.putClassRef(ic.getClass0)
          file.writeSet(ic.getAccessFlags)
        }
      }
    }

    def writeDebugType(tpe: DebugType): Unit = {
      val file = this.curFile
      if (tpe != null) {
        file.write(1)
        tpe.serialize(file.writeInt, file.writeJString)
      } else {
        file.write(0)
      }
    }

    def writeIMTSlots(): Unit = {
      var slots: Array[Int] = null

      val c = this.context
      val file = this.curFile

      if (!c.isUnloadable) {
        slots = c.imtSlots
      } else {
        slots = null
      }

      if (slots == null) {
        file.writeUInt(0)
        return
      }

      val size = slots.length
      file.writeUInt(size)

      for (i <- 0 until size) {
        file.writeUInt(slots(i))
      }
    }

    def writeStringTable(): Unit = {
      val c = this.context
      if (c.isUnloadable) {
        return
      }

      if (c.strTable == null) {
        this.curFile.writePackedInt(0)
      } else {
        val table = c.getStringTable
        this.curFile.writePackedInt(table.getLength)
        for (i <- 0 until table.getLength) {
          this.writeJString(table.getStringByIndex(i))
        }
        table.symLength = table.getLength
      }
    }


    def writeSupers(): Unit = {
      val hasSuperclass = context.superclass != null && !context.hasAbsentSuper
      writeBool(hasSuperclass)
      if (hasSuperclass) {
        writeReferenceType(context.superclass)
      }

      if (context.hasAbsentSuper) {
        curFile.writeUInt(0)
      } else {
        curFile.writeUInt(context.interfaces.size)
        for (i <- context.interfaces) {
          writeReferenceType(i)
        }
      }

      putClassRef(context._cangjiePackage)
    }

    def writeImport(): Unit = {
      curFile.writeUInt(context.importList.size)
      for (imp <- context.importList) {
        this.writeInternalizableName(imp.name)
      }
    }

    def writePlainFields(): Unit = {
      val c = this.context
      val file = this.curFile

      file.writeSet64(c.ctags.toSet64)
      file.writeSet(c.accflags.toSet32)
      file.writeSet(c.srctags.toSet32)

      if (!c.isUnloadable) {
        file.writeUInt(c.size)
        file.writeUInt(c.alignment)
        file.writeUInt(c.getVMTSize)
      } else {
        file.writeUInt(0)
        file.writeUInt(0)
        file.writeUInt(0)
      }
      if (!c.isInterface) {
        file.writeUInt(c.getInheritanceLevel)
      } else {
        file.writeUInt(0)
      }

      this.writeJString(c.getBCSourceName)
    }

    def numerateSymObjects(): Unit = {
      val c = this.context
      if (this.sw <= sw_none.toByte) {
        numerateZero(c)
      }
    }
  }

  private class SymReader(_context: Class) extends SymIO(_context) {
    private[pcOModule] var opened: Boolean = false

    def this(_class: Class, sw: SymWriter) = {
      this(_class)
      filePlace = sw.filePlace
      coordn = sw.coordn

      for (sl <- sl_zero to sl_last) {
        this.positions(sl.toInt) = sw.positions(sl.toInt)
      }
    }

    /** IMPORTANT: this format is also read in [[getOptComponentsInfo]]! */
    def readLevel(sl: SymLevel): Unit = {
      val c = this.context
      c.disableCompletion(SymFileLevelElements(sl.toInt))

      sl match {
        case `sl_zero` =>
          readPlainFields()
          readImport()
          readSupers()
          c.setPersistentImportNum()
        case `sl_import` =>
          c.ensureImportLoaded()
        case `sl_methods` =>
          if (!c.isUnloadable) {
            assert(c.virtualNumbersAreNumerated)
            assert(c.instanceLayoutIsNumerated)
            assert(c.staticLayoutIsNumerated)
          }
          this.readMethods()
        case `sl_fields` =>
          this.readFields()
        case `sl_codegen_info` =>
          this.readAttributes()
          this.readInnerClasses()
          this.readIMTSlots()
          if (c.isCangjieType) {
            c.setLLVMDebugType(readDebugType())
          }
        case `sl_str_table` =>
          this.readStringTable()
      }

      if (sl != sl_last) {
        this.setLevelPosition(sl + UByte(1))
      }
    }

    def readMethods(): Unit = readMembers(context.methods)
    def readFields(): Unit = readMembers(context.fields)

    def readMembers[T <: Member](buf: ArrayBuffer[T]): Unit = {
      assert(buf.isEmpty)
      for (case m: T <- peekUntilNull(readMember)) {
        buf += m
        if (context.isUnloadable) {
          declareMember(context, m)
        }
      }
    }

    def readAttributes(): Unit = {
      val c = this.context

      this.readFEXT(c)
    }

    def readInnerClasses(): Unit = {
      val c = this.context
      val file = this.curFile

      val sz = file.readPackedInt()
      for (_ <- 0 until sz) {
        val ic = this.getClassByRef()
        val accFlags = file.readSet()
        c.addInnerClass0(ic, accFlags)
      }
    }

    def readDebugType(): DebugType = {
      val file = this.curFile
      if (file.read() == 1) {
        return DebugType.deserialize(file.readInt, file.readJString)
      }
      null
    }

    def readIMTSlots(): Unit = {
      var slots: Array[Int] = null

      val c = this.context
      val file = this.curFile

      val size = file.readUInt()
      if (size > 0) {
        slots = new Array[Int](size)

        for (i <- 0 until size) {
          slots(i) = file.readUInt()
        }
      } else {
        slots = null
      }

      c.imtSlots = slots
    }

    def readStringTable(): Unit = {
      val c = this.context
      if (c.isUnloadable) {
        return
      }

      val table = c.getStringTable
      val len = this.curFile.readPackedInt()
      table.symLength = len
      if (len != 0) {
        for (i <- 0 until len) {
          val index = table.addString(this.readJString())
          assert(index == i)
        }
      }
    }

    def readSupers(): Unit = {
      if (readBool()) {
        context.superclass = readReferenceType(RefClassType)
      }

      assert(context.interfaces.isEmpty)
      context.interfaces ++= Array.fill(curFile.readUInt())(readReferenceType(RefInterfaceType))

      context._cangjiePackage = getClassByRef()
    }

    def readImport(): Unit = {
      val curElements = context.elementsRequireCompletion  // this hack made for disable recursive completer calling in addImport method
      context.disableCompletion(ETAG_SET.of(etag_import))

      assert(context.importList.isEmpty)
      for (_ <- 0 until curFile.readUInt()) {
        val name = readInternalizableName(curFile)
        context.importList += new ImportEntry(name)
      }

      context.elementsRequireCompletion = curElements  // this hack made for disable recursive completer calling in addImport method
    }

    def readPlainFields(): Unit = {
      val c = this.context
      val file = this.curFile

      c.ctags = file.readSet64().toSet64
      c.accflags = file.readSet().toSet32
      c.srctags = file.readSet().toSet32

      c._size = file.readUInt()
      c._alignment = file.readUInt()
      c.vmtSize = file.readUInt()
      c.level = file.readUInt()

      c.setBCSourceName(this.readJString())
    }

    def readProcRest(m: Method): Unit = {
      /* read proc type*/
      m.mtags = this.curFile.readSet64().toSet64/*MTAG_SET*/
      m.mtagsAnnot = this.curFile.readSet().toSet32/*MTAG_ANNOT_SET*/
      m.callconv = CallConv.fromOrdinal(this.curFile.read())
      m.preservedParams = this.curFile.readSet()
      m.altLocationParams = this.curFile.readSet()
      m.setBytecodeSize(this.curFile.readPackedInt())
      m.setLLVMIndex(this.curFile.readPackedInt())
      m.abiSig = readMethodSignature()
      m.specialParamSet = readSpecialParamSet()
      this.readMethodThrows(m)
    }

    def readMethodThrows(m: Method): Unit = {
      val tnum = this.curFile.readUInt()
      if (tnum > 0) {
        val tarray = new Array[Class](tnum)
        for (i <- 0 until tnum) {
          tarray(i) = readClassRefSelfIncluded()
        }
        m.setThrows(tarray)
      }
    }

    def readMember(): Member = {
      var value: Boolean = false

      var memberKind = curFile.read()
      if (memberKind == 0) {
        return null
      }

      assert(memberKind < oa_attr)
      if (memberKind >= oa_val) {
        value = true
        memberKind -= oa_val
      } else {
        value = false
      }

      val lref = curFile.readInt()
      val nameObj = pcNames.NameAndSig(readJString(), readSignature())

      import MemberKind.*
      val m = MemberKind.fromOrdinal(memberKind - 1) match {
        case METHOD          => new Method(context.mno, nameObj, lref)
        case STATIC_FIELD    => new StaticField(context.mno, nameObj, lref)
        case INSTANCE_FIELD  => new InstanceField(context.mno, nameObj, lref)
      }

      m.modifiers = curFile.readSet().toSet32
      m.memtags = curFile.readSet().toSet32
      m.numberInClassFile = curFile.readPackedInt()
      m match {
        case o: Field =>
          o.ftags = curFile.readSet().toSet32
          o.offset = curFile.readUInt()
        case _ =>
      }
      readFEXT(m)
      m match {
        case o: Method =>
          readProcRest(o)
        case o: Field =>
          o.sig = readSignatureType()
          if (value) {
            o.asInstanceOf[StaticField].value = curFile.readConstValue()
          }
      }
      m
    }

    def readSpecialParamSet(): MethodType.SpecialParamSet = {
      MethodType.SpecialParamSet.fromBitSet(immutable.BitSet.fromBitMask(Array(curFile.read8())))
    }

    def closeSymReader(): Unit = {
      assert(opened)
      curFile.close()
      opened = false
      curFile = null
    }

    def moveSymReaderTo(sl: SymLevel): Unit = {
      assert(this.opened)
      assert(this.positions(sl.toInt) != -1)
      this.curFile.setPos(this.positions(sl.toInt))
    }

    def openSymReader(): Unit = {
      assert(!this.opened)

      if (this.filePlace == null) {
        this.filePlace = xPDB.findPlaceToReadFrom(this.context.getMangledName, xPDB.ContentType.SYM)
      }

      if (this.filePlace == null) {
        env.errors.fault(ErrMsg504, xPDB.createPlaceName(this.context.getMangledName, xPDB.ContentType.SYM))
      }

      this.curFile = this.filePlace.openAsSymForRead()

      if (this.curFile == null) {
        env.errors.fault(ErrMsg425, xfs.sym.errmsg)
      }

      if (showFoundSym && this.filePlace.fullName != null) {
        env.info.print("Found sym: \'%S\'\\n", this.filePlace.fullName)
      }

      if (this.coordn != null) {
        this.curFile.setCoordName(this.coordn)
      }

      this.readMagic()

      this.setLevelPosition(sl_zero)

      this.opened = true
    }

    def readMagic(): Unit = {
      var i = this.curFile.readPackedInt()
      if (i != sym_magic) {
        env.errors.fault(ErrMsg190, this.filePlace.fullName)
      }
      i = this.curFile.readPackedInt()
      if (i != xcVersion.SymFileVersion) {
        env.errors.fault(ErrMsg191, this.filePlace.fullName, i, xcVersion.SymFileVersion.toUInt.toInt)
      }
    }
  }

  private class CodeSourcePtr {
    private[pcOModule] var jar: XString = _
    private[pcOModule] var next: CodeSourcePtr = _
  }

  class ObjectsLoader {
    def load(name: XString): Unit = {
      throw new AssertionError
    }
  }

  private var classCoolingTime: Int = _
  // elements which should be used only during codegen stage of their class
  private val CodegenElements: ETAG_SET = ETAG_SET.of(etag_attributes, etag_inner_classes, etag_str_table, etag_imt_slots)
  private var dropWorking: Boolean = false
  var x2cClass: Class = _
  var classAbsenceErr: Boolean = _
  private var classloaderIDGetter: ClassloaderIDGetter = new ClassloaderIDGetter()
  private val symFileCompleter = new SymFileClassCompleter
  var isTomcat: Boolean = _
  var isIdea: Boolean = _
  var isSpringBoot: Boolean = _
  var isCustomClassloaders: Boolean = _
  var isCangjie: Boolean = _
  private var noOptimizeClinits: Boolean = _
  val STRTABLE_NAME: XString = js.newJString("<cstrings>")

  // persistent fext types consts
  private val extnametype: Byte = 1
  private val sigtype: Byte = 2
  private val absentsupertype: Byte = 3
  private val vererrtype: Byte = 4
  private val conststrtype: Byte = 5
  private val kstype: Byte = 6
  private val encmethtype: Byte = 7
  private val outerClassFEXTType: Byte = 8
  private val cFuncWrapperIdx: Byte = 9
  private val hostClassFEXTType: Byte = 10
  // annotations
  private val rtVisAnnotType: Byte = 11
  private val rtInvisAnnotType: Byte = 12
  private val rtVisTypeAnnotType: Byte = 13
  private val rtInvisTypeAnnotType: Byte = 14
  private val rtVisParAnnotType: Byte = 15
  private val rtInvisParAnnotType: Byte = 16
  private val annotDefaultType: Byte = 17
  // Java 8
  private val methodParametersType: Byte = 18
  // AJ specific annotations
  private val ajCallToManagedType: Byte = 19
  private val ajStackAllocInfo: Byte = 20
  private val ajFlatFieldInfo: Byte = 21
  private val ajReplacementType: Byte = 22
  private val ajDataFieldInfo: Byte = 23
  private val ajExportIDType: Byte = 24
  private val ajUncheckedCallType: Byte = 25
  private val ajUncheckedNewType: Byte = 26
  private val ajStackCheckByCallerByteCount: Byte = 27
  private val ajInlineIfConstParams: Byte = 28
  private val verpairtype: Byte = 29
  private val methodSourceFullName: Byte = 30
  private val memberCPPLinkageName: Byte = 31
  private val memberSourceName: Byte = 32
  private val memberSourceFile: Byte = 33
  private val memberSourceLine: Byte = 34
  private val memberDebugType: Byte = 35
  private val chirVTable: Byte = 36
  private val versionedMarkerMethod: Byte = 38
  private val ajCallConvHeadInLimit: Byte = 39
  private val ajCallConvHeadOutLimit: Byte = 40
  private val cjAnnotation: Byte = 41
  private val cjParametersAnnotations: Byte = 42
  private val lambdaInfo: Byte = 43
  private val bytecodeInfo: Byte = 44
  private val genericInfo: Byte = 45
  private val chirDef: Byte = 46
  private val cangjieArrayElementType: Byte = 47
  private val ajDelayedIntrinsic: Byte = 48
  private val cangjieBoxValueType: Byte = 49
  private val cangjieEnumInfo: Byte = 50
  private val cangjieExtendInfo: Byte = 51
  val lastpersistenttype: Byte = 51

  /*----------------------------------------------------------------*/
  private var classLookupTable: Hashtable = _
  private var classSearchCache: Hashtable = _/*<js.InternJString, ClassSearchResult>*/
  /*------------------------ Sym. files ----------------------------*/
  private val sym_magic: Int = 0x4F4D53
  private val sw_none: Int = 0
  private val sw_zero: Int = 1   /* zero level numerated */
  private val sw_saved: Int = 3
  private var showFoundSym: Boolean = false
  private var symexistscache = mutable.HashMap.empty[pcNames.NAME, Boolean]
  private val ol_null: Int = 0
  private val ol_own: Int = 11 /* <lref> */
  private val ol_foreign: Int = 12 /* <mod_num> <lref> */
  private val oa_val: Int = 64
  private val oa_attr: Int = 128

  private val SymFileLevelElements: Array[ETAG_SET] = Array[ETAG_SET](
    ETAG_SET.empty, // sl_zero
    ETAG_SET.of(etag_import), // sl_import
    ETAG_SET.of(etag_methods), // sl_methods
    ETAG_SET.of(etag_fields), // sl_fields
    ETAG_SET.of(etag_attributes, etag_inner_classes, etag_imt_slots, etag_debug_type), // sl_codegen_info
    ETAG_SET.of(etag_str_table), // sl_str_table
  )

  private val symFileElements: ETAG_SET = SymFileLevelElements.reduce(_ | _)

  /*
  "*"      - field is written.
  "void"   - value is always a reference to void type.
  field->  - field value is restored from "field"
  (1)      - notes.
  */

  private var codesources: CodeSourcePtr = _
  private var unfilteredSet: Hashtable = new Hashtable()
  val JCA_NO_KNOWN_SAFE_INFO: Int = Int.MinValue
  //--------------------------------------------------
  private var symCache: Array[Class] = new Array[Class](16)
  private var symCacheSize: Int = 0
  private var testSymLevelCleanupRate: Int = _
  private var testSymLevelCleanupCounter: Int = _
  private var printGCThrashWarning: Boolean = _
  private var dropOnEverySession: Boolean = _
  private var curSession: Int = 0

  private def touch(c: Class): Unit = {
    c.lastUsageTime = curSession
  }

  private def onGet(c: Class, element: ETAG): Unit = {
    assert(!dropWorking)
    touch(c)
    if (c.elementsRequireCompletion contains element) {
      symFileCompleter.complete(c, element)
    }
  }

  def setClassloaderIDGetter(getter: ClassloaderIDGetter): Unit = {
    classloaderIDGetter = getter
  }

  def getClassloaderID(clazz: Class): Int = classloaderIDGetter.getID(clazz)

  var makeClass: XString => Class = null

  def currentComponentName(): XString = {
    var ext: XString = null
    val outname = env.config.equation("OUTPUTNAME")

    if (env.config.option("gendll")) {
      ext = env.config.equation("dllext_target")
    } else if (O2Env.env.enabled(GenMegaObj)) {
      ext = env.config.equation("mobjext")
    } else {
      ext = env.config.equation("exeext_target")
    }

    val buf = new js.StringBuffer(outname.length + 1 + ext.length)
    buf.appendString(outname)
    if (ext.nonEmpty && ext.charAt(0) != '.') {
      buf.appendChar('.')
    }
    buf.appendString(ext)

    buf.intern()
  }

  private def newInnerClass(clazz: Class, accessFlags: Set32): InnerClass = {
    val icls = new InnerClass()
    icls.clazz = clazz
    icls.accessFlags = accessFlags
    icls
  }

  def getCoreType(t: pc.SymType): pc.SymType = t match {
    case t: pc.SymType.Array => t.arrayBaseType
    case t => t
  }

  def getCoreClassType(t: pc.SymType): Class = getCoreType(t) match {
    case t: Class => t
    case _ => null
  }

  def isStringTable(o: pc.Symbol): Boolean = o.isInstanceOf[StringTable]

  private def declareMember(c: Class, m: Member): Unit = m match {
    case _: Method => onGet(c, etag_methods)
    case _: Field  => onGet(c, etag_fields)
  }

  private def addClassToLookupTable(c: Class): Unit = {
    if (classLookupTable == null) {
      classLookupTable = new Hashtable()
      classSearchCache = new Hashtable()
    }
    val name = c.nameObj
    val e = classLookupTable.put(name, c)
    assert(e == null)

    if (pcNames.isClassName(name) || pcNames.isAbsent(name)) {
      classSearchCache.remove(name.name)
    }
  }

  def findClassByNameObject(name: pcNames.NAME): Class = O2Env.stage(Stage.pcOfind) {
    if (classLookupTable == null) {
      return null
    }
    classLookupTable.get(name).asInstanceOf[Class]
  }

  def findAbsentClass(classname: XString): Class = findClassByNameObject(pcNames.newAbsentClassName(classname))

  def findClass(classnamePar: XString, tryAbsent: Boolean = true, classloaderID: XString = null, tryLambda: Boolean = false): Class = {
    var classname = JBCPreprocessor.movedScalaClassName(classnamePar)
    var name: pcNames.NAME = null

    if (classLookupTable == null) {
      return null
    }

    if (classloaderID == null) {
      classname = js.intern(classname)
      val csr = classSearchCache.get(classname).asInstanceOf[ClassSearchResult]
      if (csr != null) {
        return csr.result
      }
      name = pcNames.newClassName(classname)
    } else {
      name = pcNames.newBundleClassName(classname, classloaderID)
    }

    var c = findClassByNameObject(name)
    if (c == null && tryAbsent) {
      c = findAbsentClass(classname)
    }

    if (tryLambda && (c == null || c.isUnavailable)) {
      // failed to find usual class, let's try to find lambda class
      val lambdaClassName = pcNames.newLambdaClassName(classname, classloaderID)
      c = findClassByNameObject(lambdaClassName)
    }

    if (classloaderID == null) {
      val csr = new ClassSearchResult()
      csr.result = c
      if  (c != null || (tryAbsent && tryLambda)) classSearchCache.put(classname, csr)
    }

    c
  }

  // to remove
  def getClassRecord(mno: Int): Class = _classes(mno)

  /*
  A field or method "r" is accessible to a class or interface "scope"
  if and only if any of the following conditions is true:

  - "r" is public

  - "r" is protected and is declared in a class C,
     and "scope" is either a subclass of C or C itself.
     WARNING: new in 1.5:
     Furthermore, if r is not static, then the symbolic reference to r
     must contain a symbolic reference to a class "memberRefClass",
     such that "memberRefClass" is either a subclass
     of "scope", a superclass of "scope" or "scope" itself.

  - "r" is either protected or package private
    (that is, neither public nor private),
    and is declared by a class in the same runtime package as "scope".

  - "r" is private and is declared in "scope".
  */
  private def isMemberAccessible0(member: Member, memberRefClass: Class, checkRefClass: Boolean, scope: Class): Boolean = {
    if (memberRefClass == null) {
      assert(!checkRefClass)
    }

    val declClass = member.getDeclaringClass

    if ((declClass eq scope) || member.isPublic) {
      return true
    }

    var hostClass = scope
    while (hostClass.isAnonymous) {
      hostClass = hostClass.hostClass
      assert(hostClass != null)
      if (declClass eq hostClass) {
        return true
      }
    }

    if (member.isPrivate) {
      return false
    }

    if (declClass.isSamePackage(scope)) {
      return true
    }

    if (member.isProtected && !hostClass.isInterface && hostClass.isSubType(declClass)) {
      if (checkRefClass) {
        member.isStatic || (scope eq memberRefClass) || (declClass eq memberRefClass) || hostClass.isSubType(memberRefClass) || memberRefClass.isSubType(hostClass)
      } else {
        true
      }
    } else {
      /* member is package private or scope is not derived from member's host */
      false
    }
  }

  /* Used for accesibility check for invokevirtual lookup and
     counting all fields in rfField.
     Does not need new modifications in 1.5 for accessibility.
  */
  def isMemberAccessible(member: Member, scope: Class): Boolean = isMemberAccessible0(member, null, checkRefClass = false, scope)

  /* Used for accessibility check of field/method resolution.
     Needs new modifiactions in 1.5 for accessibility.
   */
  def isMemberAccessibleNew(member: Member, memberRefClass: Class, scope: Class): Boolean = isMemberAccessible0(member, memberRefClass, checkRefClass = true, scope)

  /*----------------Class Creation----------------------------------------*/

  class ModuleObject(_mno: Int, _nameObj: pcNames.NAME) extends pc.Symbol(_mno, _nameObj)

  private def newClass(name: pcNames.NAME): Class = {
    val mno = pc.modules.size
    pc.currentModule = mno

    val mod = new ModuleObject(mno, name)
    pc.modules += mod

    val class0 = new Class(mno)
    val nameString = JBCPreprocessor.movedScalaClassName(name.name)
    val namePos = nameString.lastIndexOf('/')
    class0.packageName = if (namePos == -1) js.jstrEmpty else js.internSubstring(nameString, 0, namePos)

    _classes += class0
    addClassToLookupTable(class0)

    class0
  }

  def makeClassHead(name: pcNames.NAME): Class = {
    val old_cur = pc.currentModule
    val class0 = newClass(name)
    pc.currentModule = old_cur
    class0
  }

  private def makeClassFromSymFile(name: pcNames.NAME): Class = {
    val c = makeClassHead(name)
    val reader = new SymReader(c)
    c.symio = reader
    reader.openSymReader()
    reader.moveSymReaderTo(sl_zero)
    reader.readLevel(sl_zero)
    reader.closeSymReader()
    c.enableCompletion(symFileElements)
    c
  }

  def makeAbsentClass(name: pcNames.NAME, importedFromSym: Boolean): Class = {
    val c = makeClassHead(name)
    c.markAsAbsent()
    if (importedFromSym) {
      c.markAsAbsentSymImport()
    }
    c
  }

  def makeClassHeadTags(this0: pcNames.NAME, srctags: XOTAG_SET, accflags: XOTAG_SET): Class = {
    val class0 = newClass(this0)

    class0.srctags = srctags
    class0.accflags = accflags

    ExtraPassModule.markToPreExtra(class0)

    class0
  }

  /*---------------Plain arrays management--------------------------------*/
  /** Sets length of plain array type for specified object */
  def setPlainArrayLength(arr: pc.DataSymbol.Sized, length: Int): Unit = arr.size = Some(length)

  /** Returns length of plain array type for specified object */
  def getPlainArrayLength(arr: pc.DataSymbol.Sized): Int = arr.size.get

  private def newFEXT(type0: Int): FEXT = type0 match {
    case `sigtype` |
         `extnametype` |
         `methodSourceFullName` |
         `ajExportIDType` |
         `memberCPPLinkageName` |
         `memberSourceName` |
         `memberSourceFile` =>
      new StrFEXT
    case `conststrtype` |
         `memberSourceLine` |
         `cFuncWrapperIdx` |
         `ajCallConvHeadInLimit` |
         `ajCallConvHeadOutLimit` |
         `ajStackCheckByCallerByteCount` =>
      new IntFEXT
    case `absentsupertype` |
         `outerClassFEXTType` |
         `hostClassFEXTType` =>
      new ClassFEXT
    case `vererrtype` =>
      new VerErrFEXT
    case `rtVisAnnotType` |
         `rtInvisAnnotType` |
         `rtVisTypeAnnotType` |
         `rtInvisTypeAnnotType` |
         `rtVisParAnnotType` |
         `rtInvisParAnnotType` |
         `annotDefaultType` =>
      new AnnotationAttr
    case `kstype` =>
      new JcaKnownSafeAttr
    case `encmethtype` =>
      new EnclosingMethodFEXT
    case `ajCallToManagedType` |
         `ajUncheckedCallType` |
         `ajUncheckedNewType` |
         `ajReplacementType` |
         `cjAnnotation` =>
      new MethodFEXT
    case `cjParametersAnnotations` =>
      new CJAnnotationFactoriesForParametersFEXT
    case `ajStackAllocInfo` |
         `ajFlatFieldInfo` =>
      new SizeAndAlignment
    case `ajInlineIfConstParams` =>
      new InlineIfConstParamsFEXT
    case `methodParametersType` =>
      new MethodParametersFEXT
    case `verpairtype` =>
      new VerPairFEXT
    case `memberDebugType` =>
      new DebugTypeFEXT
    case `versionedMarkerMethod` =>
      new VersionedMarkerFEXT(null)
    case `lambdaInfo` =>
      new LambdaInfoFEXT
    case `bytecodeInfo` =>
      new BytecodeInfo
    case `genericInfo` =>
      new GenericInfoFEXT
    case `chirDef` =>
      new CHIRDefFEXT
    case `cangjieArrayElementType` =>
      new SignatureTypeFEXT
    case `ajDelayedIntrinsic` =>
      new DelayedIntrinsicFEXT
    case `cangjieBoxValueType` =>
      new SignatureTypeFEXT
    case `ajDataFieldInfo` =>
      new DataAnnotFEXT
    case `chirVTable` =>
      new CHIRVTableFEXT
    case `cangjieEnumInfo` =>
      new CangjieEnumInfoFEXT
    case `cangjieExtendInfo` =>
      new SignatureTypeFEXT
    case _ =>
      throw new AssertionError(type0)
  }

  private def getSymReader(c: Class): SymReader = {
    assert(c.symio != null)
    c.symio.asInstanceOf[SymReader]
  }

  private def getSymWriter(c: Class): SymWriter = {
    assert(c.symio != null)
    c.symio.asInstanceOf[SymWriter]
  }

  private def numerateZero(c: Class): Unit = {
    /* mark lref's for zero layer objects */
    if (c.symio == null) {
      c.makeSymWriter()
    } else if (c.hasSymReader) {
      return
    }
    val sw = getSymWriter(c)
    if (sw.sw >= sw_zero.toByte) {
      return
    }
    sw.sw = sw_zero.toByte
  }

  def writeInternalizableName(name: pcNames.NAME, file: xfs.SymFile): Unit = {
    pcNames.writeName(name)(file.write, file.writeJString)
  }

  def readInternalizableName(file: xfs.SymFile): pcNames.NAME = {
    pcNames.readName(file.read, file.readJString)
  }

  private def symExists(name: pcNames.NAME): Boolean = {
    val this0 = name.getMangledName
    if (this0 == null) {
      return false
    }

    symexistscache.getOrElseUpdate(name, {
      val place = xPDB.findPlaceToReadFrom(this0, xPDB.ContentType.SYM)
      place != null && place.exists
    })
  }

  private def checkSymLevel(cls: Class): Unit = {
    if (cls.isUnavailable) {
      assert(cls.hasAbsentSuper)
      // we write sym-files for super absent classes, however
      // during MethodHandle resolutions to members of the class
      // while lambda classes generation, we could add methods and fields
      // to the class while they are not needed to be written to sym file.
      cls.methods.clearAndShrink()
      cls.fields.clearAndShrink()
    }
  }

  def outSymFile(c: Class): Unit = {
    if (c.isAbsent || env.errors.errDetected) {
      return
    }

    var sw: SymWriter = null

    // TODO: remove
    // JET-8350: some MethodFEXTs can be in unresolved state because
    // of cycling dependencies beetween classes.
    // On the other hand, classes of referenced methods by MethodFEXT
    // can be already written to sym files and their contents can be dropped
    // to this point.
    // However we cannot read other sym-files of the same pdb
    // while writing our own due to (not so good) architecture.
    // So we need to resolve MethodFEXTs before starting sym file writing.
    try {
      if (env.config.option("nosymwrite")) {
        return
      }

      pc.currentModule = c.mno

      checkSymLevel(c)

      if (c.hasMethodFEXT) {
        for {
          method <- c.declaredMethods
          case fext: MethodFEXT <- method.fexts
        } {
          fext.getMethod
        }
      }

      if (c.isUnavailable && env.config.option("nosuperabsentsymwrite")) {
        assert(c.hasAbsentSuper)
        return
      }
      if (c.symio == null) {
        c.makeSymWriter()
      }
      sw = getSymWriter(c)
      sw.numerateSymObjects()
      c.checkImport()
      c.getImport foreach numerateZero

      val this0 = c.getMangledName
      sw.filePlace = xPDB.findPlaceToWriteTo(this0, xPDB.ContentType.SYM)
      val readPlace = xPDB.findPlaceToReadFrom(this0, xPDB.ContentType.SYM)
      if (readPlace != null && readPlace.exists) {
        val s = readPlace.fullName
        if (!s.equals(sw.filePlace.fullName)) {
          env.errors.fault(ErrMsg954, s)
        }
      }
      sw.curFile = sw.filePlace.openAsSymForWrite()

      sw.writeMagic()
      sw.sw = sw_saved.toByte
      sw.writeSym()
      sw.curFile.closeNew()
      env.info.newSF = true
      sw.curFile = null

      c.symio = new SymReader(c, sw)
      symCache_add(c)

      symCache_gc_ClassSerialized(c)
    } catch {
      case e: Throwable =>
        if (sw != null && sw.curFile != null) {
          sw.curFile.closeNew()
        }
        throw e
    }
  }

  def prjSys_getClassByName(name: pcNames.NAME): Class = {
    var cls = findClassByNameObject(name)
    if (cls != null) {
      return cls
    }
    makeClassFromSymFile(name)
  }

  /** @return `null`, if import level is not ready */
  // TODO: refactor/remove
  def prjSys_getClassByName2(name: pcNames.NAME): Class = {
    val c = prjSys_getClassByName(name)
    if (!c.hasSymReader) {
      return null
    }
    c
  }

  private def findSYM(name: pcNames.NAME): xfs.SymFile = {
    val place = xPDB.findPlaceToReadFrom(name.getMangledName, xPDB.ContentType.SYM)
    if (place == null) {
      return null
    }

    val file = place.openAsSymForRead()
    if (file == null) {
      return null
    }

    if (showFoundSym) {
      env.info.print("Found sym: \'%S\'\\n", place.fullName)
    }

    file
  }

  private def checkSymMagic(file: xfs.SymFile): Boolean = {
    var i = file.readPackedInt()
    if (i != sym_magic) {
      return false
    }

    i = file.readPackedInt()
    if (i != xcVersion.SymFileVersion) {
      return false
    }

    true
  }

  def getOptComponentsInfo(name: pcNames.NAME): Option[(XString, XOTAG_SET)] = {
    val file = findSYM(name)
    if (file == null) {
      return None
    }
    if (!checkSymMagic(file)) {
      file.close()
      return None
    }

    file.readSet64()                     // .ctags
    file.readSet()                       // .accflags
    val xotTags = file.readSet().toSet32 // .srctags
    val extName = file.readJString()     // .extensionName

    file.close()
    Some(extName, xotTags)
  }

  /*----------------------------------------------------------------------------*/
  /*----------------------------------------------------------------------------*/

  def addCodeSource(jar: XString): Unit = {
    val cs = new CodeSourcePtr()
    cs.jar = jar
    cs.next = codesources
    codesources = cs
  }

  def isClassShouldNotBeFiltered(classname: XString): Boolean = unfilteredSet.get(classname) != null

  def exi(): Unit = {
    classLookupTable = null
    classSearchCache = null
    _classes = null
    pc.modules = null
  }

  //------------------------------------------------
  private def newJBCMethodFEXT(mclazz: Class, className: XString, methodName: XString, methodSig: XString, annotName: String, allowNotFound: Boolean): MethodFEXT = {
    val msig = O2Env.env.parseMethodSignature(methodSig)
    newMethodFEXT(mclazz, className.replace('.', '/'), methodName, msig, annotName, allowNotFound)
  }

  private def newMethodFEXT(mclazz: Class, className: XString, methodName: XString, methodSig: MethodSignature, annotName: String, allowNotFound: Boolean): MethodFEXT = {
    mclazz.markAsHasMethodFEXT()
    val tclass = mclazz.resolveClass(className, addImport = true)
    var mth: Method = null

    if (tclass == null) {
      if (!allowNotFound) {
        env.errors.fault(ErrMsg981, annotName, className)
      }
    } else {
      mth = tclass.findLocalMethod(methodName, methodSig) // could return NIL
    }

    val fext = new MethodFEXT
    fext.method = mth
    fext.class0 = tclass
    fext.mname = methodName
    fext.msig = methodSig
    fext.allowNotFound = allowNotFound
    fext
  }

  def initializeAJReplaced(c: Class): Unit = {
    for (m <- c.declaredMethods) {
      if (!(m.mtags contains mtag_ajreplaced_initialized)) {
        val replacement = ReplacementLibrary.getReplacement(m).orNull
        if (replacement != null) {
          m.mtags += mtag_ajreplaced
        }
      }
      m.mtags += mtag_ajreplaced_initialized
    }
  }

  //--------- Verification support -------------
  def newVerifyError(errcode: VerificationError.ExceptionKind, errmsg: XString): VerifyError = VerifyError(errcode, errmsg)

  def getVerificationPairs(cls: Class): VerificationPair = {
    cls.fextOption[VerPairFEXT](verpairtype).map(_.pairs).orNull
  }

  private def newVerificationPair(from: Class, to0: Class, errmsg: XString): VerificationPair = {
    val v = new VerificationPair()
    v.from = from
    v.to0 = to0
    v.errmsg = errmsg
    v
  }

  private def newVerificationPairAndAddImport(currClass: Class, from: Class, to0: Class, errmsg: XString): VerificationPair = {
    currClass.addImport(from)
    currClass.addImport(to0)
    newVerificationPair(from, to0, errmsg)
  }

  def addVerificationPair(currClass: Class, from: Class, to0: Class, errmsg: XString): Boolean = {
    if (currClass.isSystemClass || currClass.isFromExtensionClassloader()) {
      return false
    }
    currClass.inclModifier(xot_needcheckpairs)
    var v = getVerificationPairs(currClass)
    if (v == null) {
      val vext = new VerPairFEXT
      vext.pairs = newVerificationPairAndAddImport(currClass, from, to0, errmsg)
      currClass.addFEXT(vext, verpairtype)
      return true
    }
    var prev: VerificationPair = null
    while (v != null) {
      if ((v.to0 eq to0) && (v.from eq from)) {
        return false
      }
      prev = v
      v = v.next
    }
    prev.next = newVerificationPairAndAddImport(currClass, from, to0, errmsg)
    true
  }

  private def getFDForPlatformClass(cls: Class): xfs.FileDescriptor = {
    if (cls.isAnonymous) {
      null
    } else {
      xfs.sys.lookup(FS.addExt2(JBCPreprocessor.originalScalaClassName(cls.name, isRuntimeClass = cls.isJetRuntimeClass), "class"), lookInCurrentDir = false)
    }
  }

  def canLoadClass(cls: Class): Boolean =
    (cls.fileDescriptor != null) || (getFDForPlatformClass(cls) != null)

  /** Loads bytecode for 'cls' and set ClassInfo attribute to 'cls' */
  private def loadClass(cls: Class): jcp.PtrClassInfo = {
    var fd = cls.fileDescriptor
    if (fd == null) {
      fd = getFDForPlatformClass(cls)
      assert(fd != null)
    }
    val file = fd.openSymFile()
    assert(jcp.load(file)._1)
    val C = jcp.c
    jcp.c = null
    file.close()
    C
  }

  private def writeAnnotation(f: xfs.SymFile, annot: jcp.PtrAnnotation): Unit = {
    f.writeJString(annot.type0)
    f.writePackedInt(annot.pairs.length)
    for (i <- annot.pairs.indices) {
      f.writeJString(annot.pairs(i).name)
      writeElementValue(f, annot.pairs(i).value)
    }
  }

  private def writeElementValue(f: xfs.SymFile, value: jcp.PtrElementValue): Unit = {
    f.write(value.tag)
    value match {
      case value: jcp.PtrIntElementValue =>
        f.writePackedInt(value.value)
      case value: jcp.PtrLongElementValue =>
        f.writePackedInt(value.low)
        f.writePackedInt(value.high)
      case value: jcp.PtrFloatElementValue =>
        f.writeFloat(value.value)
      case value: jcp.PtrDoubleElementValue =>
        f.writeReal(value.value)
      case value: jcp.PtrStringElementValue =>
        f.writeJString(value.value)
      case value: jcp.PtrEnumElementValue =>
        f.writeJString(value.typeName)
        f.writeJString(value.constName)
      case value: jcp.PtrClassElementValue =>
        f.writeJString(value.classInfo)
      case value: jcp.PtrAnnotationElementValue =>
        writeAnnotation(f, value.value)
      case value: jcp.PtrArrayElementValue =>
        f.writePackedInt(value.value.length)
        for (i <- value.value.indices) {
          writeElementValue(f, value.value(i))
        }
    }
  }

  private def writeRuntimeVisibleAnnotationAttr(f: xfs.SymFile, a: jcp.PtrAnnotationsAttr): Unit = {
    if (!a.isMalformed) {
      f.write(1)
      f.writePackedInt(a.annotations.length)
      for (i <- a.annotations.indices) {
        writeAnnotation(f, a.annotations(i))
      }
    } else {
      f.write(0)
    }
  }

  private def writeTargetInfo(f: xfs.SymFile, targetInfo: jcp.PtrTargetInfo): Unit = {
    targetInfo match {
      case targetInfo: jcp.PtrOneByteTargetInfo =>
        f.write(targetInfo.index)
      case targetInfo: jcp.PtrWordTargetInfo =>
        f.writePackedInt(targetInfo.index.toInt)
      case targetInfo: jcp.PtrTwoBytesTargetInfo =>
        f.write(targetInfo.index1)
        f.write(targetInfo.index2)
      case _ =>
    }
    // nothing
  }

  private def writeRuntimeVisibleTypeAnnotationAttr(f: xfs.SymFile, a: jcp.PtrTypeAnnotationsAttr): Unit = {
    if (!a.isMalformed) {
      f.write(1)
      f.writePackedInt(a.typeAnnotations.length)
      for (i <- a.typeAnnotations.indices) {
        val annot = a.typeAnnotations(i)
        f.write(annot.targetType)
        writeTargetInfo(f, annot.targetInfo)
        f.write(annot.pathLength)
        for (j <- 0 until annot.pathLength * 2) {
          f.write(annot.path(j))
        }
        writeAnnotation(f, annot.annotation)
      }
    } else {
      f.write(0)
    }
  }

  private def writeRuntimeVisibleParameterAnnotationAttr(f: xfs.SymFile, a: jcp.PtrParameterAnnotationsAttr): Unit = {
    if (!a.isMalformed) {
      f.write(1)
      f.writePackedInt(a.annotations.length)
      for (i <- a.annotations.indices) {
        f.writePackedInt(a.annotations(i).length)
        for (j <- a.annotations(i).indices) {
          writeAnnotation(f, a.annotations(i)(j))
        }
      }
    } else {
      f.write(0)
    }
  }

  private def writeAnnotationDefaultAttr(f: xfs.SymFile, a: jcp.PtrAnnotationDefaultAttr): Unit = {
    if (!a.isMalformed) {
      f.write(1)
      writeElementValue(f, a.defaultValue)
    } else {
      f.write(0)
    }
  }

  private def readAnnotation(f: xfs.SymFile): jcp.PtrAnnotation = {
    val annot = new jcp.PtrAnnotation()
    annot.type0 = f.readJString()
    val len = f.readPackedInt()
    annot.pairs = Array.fill[jcp.AnnotationPair](len)(new jcp.AnnotationPair())
    for (i <- annot.pairs.indices) {
      annot.pairs(i).name = f.readJString()
      annot.pairs(i).value = readElementValue(f)
    }
    annot
  }

  private def readElementValue(f: xfs.SymFile): jcp.PtrElementValue = {
    val tag = f.read()
    val value = tag match {
      case 'B' |
           'C' |
           'I' |
           'S' |
           'Z' =>
        jcp.PtrIntElementValue(f.readPackedInt())
      case 'J' =>
        jcp.PtrLongElementValue(f.readPackedInt(), f.readPackedInt())
      case 'F' =>
        new jcp.PtrFloatElementValue(f.readFloat())
      case 'D' =>
        new jcp.PtrDoubleElementValue(f.readReal())
      case 's' =>
        jcp.PtrStringElementValue(f.readJString())
      case 'e' =>
        jcp.PtrEnumElementValue(f.readJString(), f.readJString())
      case 'c' =>
        jcp.PtrClassElementValue(f.readJString())
      case '@' =>
        jcp.PtrAnnotationElementValue(readAnnotation(f))
      case '[' =>
        val len = f.readPackedInt()
        val array = new Array[jcp.PtrElementValue](len)
        for (i <- array.indices) {
          array(i) = readElementValue(f)
        }
        jcp.PtrArrayElementValue(array)
    }
    value.tag = tag.toChar
    value
  }

  private def readRuntimeVisibleAnnotationAttr(f: xfs.SymFile, a: jcp.PtrAnnotationsAttr): Unit = {
    a.isMalformed = f.read() == 0
    if (!a.isMalformed) {
      val len = f.readPackedInt()
      a.annotations = new Array[jcp.PtrAnnotation](len)
      for (i <- a.annotations.indices) {
        a.annotations(i) = readAnnotation(f)
      }
    }
  }

  private def readTargetInfo(f: xfs.SymFile, targetType: Byte): jcp.PtrTargetInfo = {
    targetType match {
      case jcp.CLASS_TYPE_PARAMETER |
           jcp.METHOD_TYPE_PARAMETER |
           jcp.METHOD_FORMAL_PARAMETER =>
        jcp.PtrOneByteTargetInfo(f.read().toByte)
      case jcp.CLASS_EXTENDS |
           jcp.THROWS =>
        jcp.PtrWordTargetInfo(f.readPackedInt().toUShort)
      case jcp.CLASS_TYPE_PARAMETER_BOUND |
           jcp.METHOD_TYPE_PARAMETER_BOUND =>
        jcp.PtrTwoBytesTargetInfo(f.read().toByte, f.read().toByte)
      case jcp.FIELD |
           jcp.METHOD_RETURN |
           jcp.METHOD_RECEIVER =>
        val empty = new jcp.PtrTargetInfo()
        empty
      case _ =>
        throw new AssertionError
    }
    // Code attribute: do not parse
  }

  private def readRuntimeVisibleTypeAnnotationAttr(f: xfs.SymFile, a: jcp.PtrTypeAnnotationsAttr): Unit = {
    a.isMalformed = f.read() == 0
    if (!a.isMalformed) {
      val len = f.readPackedInt()
      a.typeAnnotations = new Array[jcp.PtrTypeAnnotation](len)
      for (i <- a.typeAnnotations.indices) {
        val annot = new jcp.PtrTypeAnnotation()
        annot.targetType = f.read().toByte
        annot.targetInfo = readTargetInfo(f, annot.targetType)
        annot.pathLength = f.read().toByte
        annot.path = new Array[Byte](annot.pathLength * 2)
        for (j <- annot.path.indices) {
          annot.path(j) = f.read().toByte
        }
        annot.annotation = readAnnotation(f)
        a.typeAnnotations(i) = annot
      }
    }
  }

  private def readRuntimeVisibleParameterAnnotationAttr(f: xfs.SymFile, a: jcp.PtrParameterAnnotationsAttr): Unit = {
    a.isMalformed = f.read() == 0
    if (!a.isMalformed) {
      var len = f.readPackedInt()
      a.annotations = new Array[Array[jcp.PtrAnnotation]](len)
      for (i <- a.annotations.indices) {
        len = f.readPackedInt()
        a.annotations(i) = new Array[jcp.PtrAnnotation](len)
        for (j <- a.annotations(i).indices) {
          a.annotations(i)(j) = readAnnotation(f)
        }
      }
    }
  }

  private def readAnnotationDefaultAttr(f: xfs.SymFile, a: jcp.PtrAnnotationDefaultAttr): Unit = {
    a.isMalformed = f.read() == 0
    if (!a.isMalformed) {
      a.defaultValue = readElementValue(f)
    }
  }

  private def symCache_add(c: Class): Unit = {
    if (c.cached) {
      return
    }

    // resize
    if (symCache.length == symCacheSize) {
      val newCache = new Array[Class](symCacheSize * 2)
      for (i <- 0 until symCacheSize) {
        newCache(i) = symCache(i)
      }
      symCache = newCache
    }

    symCache(symCacheSize) = c
    symCacheSize += 1

    c.cached = true
  }

  private def dropClass(c: Class, elements: ETAG_SET): Unit = {
    if (c.hasSymReader) {
      c.dropElements(elements)
    }

    if (c.elementsRequireCompletion contains etag_str_table) {
      c.strTable = null
    }
  }

  private def drop(dropAll: Boolean): Unit = {
    dropWorking = true

    if (dropAll) {
      VZCModule.dropSymCache()
      allClasses foreach (_.classInfo = null)
    }

    var i = 0
    while (i < symCacheSize) {
      val c = symCache(i)
      if (dropAll || c.isCold) {
        dropClass(c, symFileElements)

        c.cached = false
        symCacheSize -= 1
        symCache(i) = symCache(symCacheSize)
        symCache(symCacheSize) = null
      } else {
        i += 1
      }
    }

    dropWorking = false
  }

  private def symCache_CleanSyntaxTree(): Unit = {
    xmErrors.printMem(doPrintErr = false)
    drop(dropAll = true)
    symexistscache = mutable.HashMap.empty[pcNames.NAME, Boolean]
    js.cleanStringsCache()
    if (isWorkMode) {
      env.info.print("\\n-- Syntax tree has been cleaned\\n")
    }
    xmErrors.printMem(doPrintErr = false)
  }

  private def testSymLevelCleanup(): Boolean = {
    if (testSymLevelCleanupRate != -1) {
      testSymLevelCleanupCounter += 1
      if (O2JSupport.mod(testSymLevelCleanupCounter, testSymLevelCleanupRate) == 0) {
        return true
      }
    }
    false
  }

  private var symCacheGCProhibited: Boolean = false

  def withSymCacheGCProhibited[T](action: => T): T = {
    assert(!symCacheGCProhibited)
    symCacheGCProhibited = true
    try {
      action
    } finally {
      symCacheGCProhibited = false
      symCache_gc()
    }
  }

  private def symCache_gc(): Unit = {
    if (symCacheGCProhibited) return

    if (mm.isGCThrashWarning || testSymLevelCleanup()) {
      if (printGCThrashWarning) {
        env.info.print("\\n-- GCThrashWarning - cleaning syntax tree\\n")
      }
      symCache_CleanSyntaxTree()
      mm.setGCThrashWarningHandled()
    } else if (dropOnEverySession) {
      drop(dropAll = false)
    }
  }

  private def symCache_gc_ClassSerialized(c: Class): Unit = O2Env.stage(Stage.SymCacheDrop) {
    dropClass(c, CodegenElements)
    symCache_gc()
  }

  def symCache_gc_StartCodegenStage(): Unit = O2Env.stage(Stage.SymCacheDrop) {
    if (!isCangjie) {
      symCache_CleanSyntaxTree()
    }
  }

  def symCache_gc_EndCodegenStage(): Unit = O2Env.stage(Stage.SymCacheDrop) {
    if (!isCangjie) {
      symCache_CleanSyntaxTree()
    }
  }

  def symCache_gc_EndProcessResources(): Unit = O2Env.stage(Stage.SymCacheDrop) {
    symCache_CleanSyntaxTree()
  }

  def symCache_gc_BackEndFinishedFor(c: Class): Unit = O2Env.stage(Stage.SymCacheDrop) {
    dropClass(c, CodegenElements)
    symCache_gc()
  }

  def iniOpt(): Unit = {
    classCoolingTime = js.parseIntOrElse(env.config.equation("cool_class"), 1000)
    showFoundSym = env.config.option("showfoundsym")
    printGCThrashWarning = isWorkMode || env.config.option("printGCThrashWarning")
    dropOnEverySession = env.config.option("dropOnEverySession")
    testSymLevelCleanupRate = js.parseIntOrElse(env.config.equation("TestSymLevelCleanup"), -1)
    classAbsenceErr = env.config.equation("ClassAbsence").equals2("ERR")
    noOptimizeClinits = env.config.option("noOptimizeClinits")
  }
}
