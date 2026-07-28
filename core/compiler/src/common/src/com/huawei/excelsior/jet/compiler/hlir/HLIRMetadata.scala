/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.hlir

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.hlir.HLIRErrorReporter.{assertion, fatal, withErrorReporter}
import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata.Ref.DelayedValue
import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata.{AutoEnvRegex, AutoEnvRegexLinkageName, JavaAnnotationElementTags, LambdaCommonRegex, LambdaRegex, Modifier, ParsingState, Ref, Resolved, Resolving, Tag, TypeTags, Unresolved}
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Bitcode
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Bitcode.*
import com.huawei.excelsior.jet.compiler.options.BoolOption.{HLIRExplicitAccessModifiers, LambdaCommonSuperclass, StdCoreAnyHierarchyRoot, StrictHLIRLinkageNameChecks}
import com.huawei.excelsior.jet.compiler.symlevel.{MethodSignature, SignatureType}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.*
import com.huawei.excelsior.jet.util.{ScalaCollections, Worklist}

import scala.PartialFunction.condOpt
import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import xscala.matching.Regex
import xscala.properties.OS
import xscala.util.StringOps.r

/** HLIR metadata context.
  *
  * Provides minimal required parsing of HLIR bitcode metadata entries into
  * proper [[Ref]] representation (see [[HLIRMetadata]] companion object for more details).
  *
  * Performs minimal format and version verification.
  *
  * @author liontiger
  */
class HLIRMetadata(val module: ParsedModule, verify: Boolean = true)(implicit val env: Environment) {

  val packageName = module.sourceFilename

  val version: HLIRVersion = parseHLIRVersion("hlir.version") match {
    case None =>
      fatal("missing or incorrect !hlir.version", "!hlir.version", packageName)

    case Some(version) =>
      assertion(version.isSupported,
        s"unsupported HLIR version: ${version.pretty}", "!hlir.version", packageName)
      version
  }

  private def parseHLIRVersion(versionName: String): Option[HLIRVersion] = {
    module.namedMetadata(versionName) flatMap {
      case Array(MDNode(major: MDValue, minor: MDValue, patch: MDValue)) =>
        for {
          major <- module.getConstValue(major)
          minor <- module.getConstValue(minor)
          patch <- module.getConstValue(patch)
        } yield HLIRVersion(major.toInt, minor.toInt, patch.toInt)
      case _ => None
    }
  }

  val packageRef = new DelayedValue[Ref.Package]

  private val entries = mutable.LinkedHashMap.empty[MDResolvedItem, ParsingState]
  def refs: Iterator[Ref] = entries.valuesIterator collect { case r: Ref => r }
  def ref(md: MDItem): Option[Ref] = entries.get(md.resolve()) collect { case r: Ref => r }

  private val linkageNameToRef = mutable.HashMap.empty[String, Ref]
  def ref(linkageName: String): Option[Ref] = linkageNameToRef.get(linkageName)

  private val lambdaClasses = mutable.HashMap.empty[String, Ref.Class]

  private val stdCoreAny = new DelayedValue[Ref]
  private val stdCoreObject = new DelayedValue[Ref]

  def isIntrinsic(linkageName: String) = isLLVMIntrinsic(linkageName) || isHLIRIntrinsic(linkageName)
  def isLLVMIntrinsic(linkageName: String) = linkageName.startsWith("llvm.")
  def isHLIRIntrinsic(linkageName: String) = extractHLIRIntrinsic(linkageName).nonEmpty
  def extractHLIRIntrinsic(linkageName: String): Option[String] = {
    val prefix = "hlir."
    Option.when(linkageName.startsWith(prefix))(linkageName.substring(prefix.length))
  }

  withErrorReporter(packageName) { reporter =>
    import reporter.*

    module.namedMetadata("hlir.metadata") match {
      case Some(metadata) =>
        for (md <- metadata) {
          entries(md) = Unresolved
        }

      case None =>
        fatal("missing !hlir.metadata", "!hlir.metadata", packageName)
    }

    // When moving LambdaCommon from interface to class, we need to have ClassRef of std.core.Object, but 
    // the interface may come to processing earlier. To avoid similar situation, we parse root of hierarchies in advance.  
    entries.keys foreach {
      case md @ MDNode(MDTag(Tag.ClassRef), MDString("std.core.Object")) => stdCoreObject.init(processEntry(md).get)
      case md @ MDNode(MDTag(Tag.InterfaceRef), MDString("std.core.Any")) => stdCoreAny.init(processEntry(md).get)
      case _ =>
    }

    // Resolve all HLIR metadata entries.
    entries.keys.toSeq foreach processEntry

    require(packageRef.initialized,
      s"missing !{!\"${Tag.PackageRef}\", !\"$packageName\"}", packageName)
    require(packageRef.getOption.forall(_.packageDef.initialized),
      s"missing !{!\"${Tag.PackageDef}\", !\"$packageName\"}", packageName)

    for ((md, resolved) <- ScalaCollections.collectDuplicatesBy(entries.iterator.filter(_._2.isInstanceOf[Ref]))(_._2)) {
      resolved match {
        // Lambda and Auto_Env parse into L$..., so entries may contain duplicates
        case clazz: Ref.Class if clazz.name.startsWith("L$") =>  
        case _ => parsingError(s"duplicate metadata entry $md", md)
      }
    }

    if (verify) {
      // TODO: check refs consistency better

      verifyLinkageNames()
    }

    // ------------------------------------------------

    /** Verifies linkage name consistency with HLIR metadata.
      *
      * More precisely verifies that
      *
      *  - all relevant bitcode entities (globals, functions and records), and
      *  - all reference types and records in signatures
      *
      * have HLIR metadata associated with them via linkage name mapping.
      */
    def verifyLinkageNames(): Unit = {
      def linkageName(x: Any): Option[String] = condOpt(x) {
        case x: StructType if x != Types.ARRAY_SLICE => x.name
        case x: TypeVariableType => x.name
        case x: Function if !isIntrinsic(x.name) => x.name
        case x: Global => x.name
      }
      for {
        x <- module.types.iterator ++ module.functions ++ module.globals
        name <- linkageName(x)
      } {
        import Tag.*
        linkageNameToRef.get(name) match {
          case Some(ref) =>
            val tags = x match {
              case x: TypeVariableType =>
                Seq(TypeVariableRef)
              case x: StructType =>
                Seq(RecordRef, MonomorphicRecordRef, InstantiatedRecordRef, TupleRef, GenericRecordRef)
              case x: Function =>
                Seq(GlobalFunctionRef, GenericGlobalFunctionRef, InstantiatedGlobalFunctionRef,
                  GlobalCFunctionRef, ForeignCFunctionRef,
                  InstanceMethodRef, GenericInstanceMethodRef, InstantiatedInstanceMethodRef,
                  StaticMethodRef, GenericStaticMethodRef, InstantiatedStaticMethodRef)
              case x: Global =>
                Seq(GlobalVariableRef, StaticFieldRef)
              case _ => Seq.empty
            }
            require(ref.tag.in(tags*),
              s"incompatible ${Tag.LinkageName} reference ${ref.md}", x)

          case None =>
            parsingError(s"missing ${Tag.LinkageName} for \"$name\"", x)
        }
      }

      // Temporary requirement: all named types should have linkage name so that we can use it during HLIR transition period.
      // TODO: remove this restriction
      refs foreach {
        case ref @ (_: Ref.Class | _: Ref.Interface | _: Ref.Record |
                    _: Ref.MonomorphicClass | _: Ref.MonomorphicInterface | _: Ref.MonomorphicRecord | _: Ref.Tuple) =>
          require(ref.linkageName.initialized, s"missing linkage name [temporary requirement]", ref.md)
        case _ =>
      }
    }

    /** Resolves given potentially unresolved entry.
      *
      * Entry in the `entries` map can be in one of the following states:
      *  - [[Unresolved]] - not yet resolved
      *  - [[Resolving]] - resolve process has started
      *  - [[Resolved]]/[[Ref]] - entry already resolved
      *
      * This set of states allows us to
      *  - detect entries that are missing from `!hlir.metadata`
      *  - avoid unnecessary parsing of the same entry
      *  - detect cyclic references
      */
    def processEntry(unresolved: MDItem): Option[Ref] = {
      val md = unresolved.resolve()
      entries.get(md) match {
        case None =>
          parsingError(s"metadata entry is not listed in !hlir.metadata", md)
          None

        case Some(Resolving) =>
          parsingError("cyclic reference", md)
          None

        case Some(ref: Ref) =>
          // Already resolved reference
          Some(ref)

        case Some(Resolved) =>
          // Already resolved non-reference
          None

        case Some(Unresolved) =>
          entries(md) = Resolving
          val res = parseEntry(md)
          entries(md) = res match {
            case Some(ref) => ref match {
              // TODO this is a workaround not to initialize CString metadata twice.
              //  This happens due to CString is a singleton and it's unique for every Resolved MDItem.
              case Ref.CString =>
                if (!ref.md.initialized) {
                  ref.md.init(md)
                }
                ref

              case c: Ref.Class if env.enabled(LambdaCommonSuperclass) =>
                md match {
                  // Lambdas should get lambdas md, not an Auto_Env md
                  case MDNode(_, MDString(AutoEnvRegex(_)))
                    if c.name.startsWith(CangjieSymLevelMaker.CANGJIE_LAMBDA_PREFIX) =>
                  case _ => ref.md.init(md)
                }
                
                ref

              case _ =>
                ref.md.init(md)
                ref
            }
            case None =>
              Resolved
          }
          res
      }
    }

    def resolveLambda(mdString: String): (Boolean, String) = mdString match {
      case _ if !env.enabled(LambdaCommonSuperclass) => (false, mdString)

      case AutoEnvRegex(newName) =>
        (true, CangjieSymLevelMaker.CANGJIE_LAMBDA_PREFIX + newName)
      case LambdaRegex(newName) =>
        val patchedLastName = if (newName.contains(packageName + "E")) {
          newName.patch(newName.lastIndexOf(packageName), "", packageName.length + 1)
        } else newName
        (true, CangjieSymLevelMaker.CANGJIE_LAMBDA_PREFIX + patchedLastName)
      case _ => (false, mdString)
    }

    def parseEntry(md: MDResolvedItem): Option[Ref] = {
      import Modifier.*
      import Tag.*

      implicit def refToStr(x: Ref): String = x.md.toString
      implicit def modifierToStr(x: Modifier): String = s"!\"$x\""

      def requireVersion(condition: Boolean, token: Any): Unit = {
        require(condition, s"$token is not supported in HLIR ${version.pretty}", md)
      }

      md match {
        case MDNode(MDTag(PrimitiveRef), MDString(sig)) =>
          require(sig.nonEmpty, s"empty sig", md)
          require(Ref.Primitive.parseSignatureType(sig).nonEmpty,
            s"invalid signature $sig", md)
          Some(Ref.Primitive(sig))

        case MDNode(MDTag(ClassRef), MDString(name)) =>
          require(name.nonEmpty, s"empty name", md)
          val (isLambda, resolvedName) = resolveLambda(name)

          if (isLambda) {
            Some(lambdaClasses.getOrElseUpdate(resolvedName, Ref.Class(resolvedName)))
          } else {
            Some(Ref.Class(resolvedName))
          }

        case MDNode(tag @ MDTag(InstantiatedClassRef), MDRef(generic: Ref.GenericClass), instantiatedTypeParametersMD @ MDRefs(instantiatedTypeParameters*)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(instantiatedTypeParameters.nonEmpty, "empty instantiated type parameters", md)
          require(instantiatedTypeParameters.size == generic.typeParameters.size,
            "incompatible number of instantiated type parameters", md)
          require(instantiatedTypeParameters.forall(x => TypeTags(x.tag) || (x.tag in (ThisTypeRef))),
            s"unexpected instantiated type parameters $instantiatedTypeParametersMD", md)
          Some(Ref.InstantiatedClass(generic, instantiatedTypeParameters.asInstanceOf[Seq[Ref.TypeOrThisType]]))

        case MDNode(tag @ MDTag(MonomorphicClassRef), MDString(name), instantiatedTypeParametersMD @ MDRefs(instantiatedTypeParameters*)) =>
          require(name.nonEmpty, s"empty name", md)
          require(instantiatedTypeParameters.nonEmpty, "empty instantiated type parameters", md)
          require(instantiatedTypeParameters.forall(x => TypeTags(x.tag) || (x.tag in (ThisTypeRef))),
            s"unexpected instantiated type parameters $instantiatedTypeParametersMD", md)
          Some(Ref.MonomorphicClass(name, instantiatedTypeParameters.asInstanceOf[Seq[Ref.TypeOrThisType]]))

        case MDNode(MDTag(InterfaceRef), MDString(name)) =>
          def lambdaCommonSuperclass = LambdaCommonRegex.pattern.matcher(name).matches

          require(name.nonEmpty, s"empty name", md)
          if (env.enabled(LambdaCommonSuperclass) && lambdaCommonSuperclass) {
            Some(Ref.Class(name))
          } else {
            name match {
              case CangjieSymLevelMaker.STD_CORE_ANY_NAME if env.enabled(StdCoreAnyHierarchyRoot) => Some(Ref.Class(name))
              case _ => Some(Ref.Interface(name))
            }
          }

        case MDNode(tag @ MDTag(InstantiatedInterfaceRef), MDRef(generic: Ref.GenericInterface), instantiatedTypeParametersMD @ MDRefs(instantiatedTypeParameters*)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(instantiatedTypeParameters.nonEmpty, "empty instantiated type parameters", md)
          require(instantiatedTypeParameters.size == generic.typeParameters.size,
            "incompatible number of instantiated type parameters", md)
          require(instantiatedTypeParameters.forall(x => TypeTags(x.tag) || (x.tag in (ThisTypeRef))),
            s"unexpected instantiated type parameters $instantiatedTypeParametersMD", md)
          Some(Ref.InstantiatedInterface(generic, instantiatedTypeParameters.asInstanceOf[Seq[Ref.TypeOrThisType]]))

        case MDNode(tag @ MDTag(MonomorphicInterfaceRef), MDString(name), instantiatedTypeParametersMD @ MDRefs(instantiatedTypeParameters*)) =>
          require(name.nonEmpty, s"empty name", md)
          require(instantiatedTypeParameters.nonEmpty, "empty instantiated type parameters", md)
          require(instantiatedTypeParameters.forall(x => TypeTags(x.tag) || (x.tag in (ThisTypeRef))),
            s"unexpected instantiated type parameters $instantiatedTypeParametersMD", md)
          Some(Ref.MonomorphicInterface(name, instantiatedTypeParameters.asInstanceOf[Seq[Ref.TypeOrThisType]]))

        case MDNode(MDTag(RecordRef), MDString(name)) =>
          require(name.nonEmpty, s"empty name", md)
          Some(Ref.Record(name))

        case MDNode(tag @ MDTag(InstantiatedRecordRef), MDRef(generic: Ref.GenericRecord), instantiatedTypeParametersMD @ MDRefs(instantiatedTypeParameters*)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(instantiatedTypeParameters.nonEmpty, "empty instantiated type parameters", md)
          require(instantiatedTypeParameters.size == generic.typeParameters.size,
            "incompatible number of instantiated type parameters", md)
          require(instantiatedTypeParameters.forall(x => TypeTags(x.tag)),
            s"unexpected instantiated type parameters $instantiatedTypeParametersMD", md)
          Some(Ref.InstantiatedRecord(generic, instantiatedTypeParameters.asInstanceOf[Seq[Ref.Type]]))

        case MDNode(tag @ MDTag(MonomorphicRecordRef), MDString(name), instantiatedTypeParametersMD @ MDRefs(instantiatedTypeParameters*)) =>
          require(name.nonEmpty, s"empty name", md)
          require(instantiatedTypeParameters.nonEmpty, "empty instantiated type parameters", md)
          require(instantiatedTypeParameters.forall(x => TypeTags(x.tag)),
            s"unexpected instantiated type parameters $instantiatedTypeParametersMD", md)
          Some(Ref.MonomorphicRecord(name, instantiatedTypeParameters.asInstanceOf[Seq[Ref.Type]]))

        case MDNode(tag @ MDTag(TupleRef), elemTypesMD @ MDRefs(elemTypes*)) =>
          require(elemTypes.size >= 2,
            s"require two or more element types $elemTypesMD", md)
          require(elemTypes.forall(x => TypeTags(x.tag)),
            s"unexpected element types $elemTypesMD", md)
          Some(Ref.Tuple(elemTypes))

        case MDNode(MDTag(ArrayRef), MDRef(elemType: Ref.Type)) =>
          require(TypeTags(elemType.tag),
            s"unexpected element type ${elemType.md}", md)
          Some(Ref.Array(elemType))

        case MDNode(MDTag(ArraySliceRef), MDRef(elemType: Ref.Type)) =>
          require(TypeTags(elemType.tag),
            s"unexpected element type ${elemType.md}", md)
          Some(Ref.ArraySlice(elemType))

        case MDNode(tag @ MDTag(VArrayRef), MDRef(elemType: Ref.Type), MDNumber(length)) =>
          requireVersion(version.hasVArray, tag)
          require(elemType.tag in (PrimitiveRef, CPointerRef, CStringRef, RecordRef, MonomorphicRecordRef, VArrayRef, TupleRef),
            s"unexpected element type type ${elemType.md}", md)
          Some(Ref.VArray(elemType, length))

        case MDNode(MDTag(JavaClassRef), MDString(name)) =>
          require(name.nonEmpty, s"empty name", md)
          Some(Ref.JavaClass(name))

        case MDNode(MDTag(JavaInterfaceRef), MDString(name)) =>
          require(name.nonEmpty, s"empty name", md)
          Some(Ref.JavaInterface(name))

        case MDNode(MDTag(JavaArrayRef), MDRef(baseType: Ref.Type), MDNumber(dimNum)) =>
          require(baseType.tag in (PrimitiveRef, JavaClassRef, JavaInterfaceRef, JavaArrayRef),
            s"unexpected base type ${baseType.md}", md)
          Some(Ref.JavaArray(baseType, dimNum.toInt))

        case MDNode(tag @ MDTag(CPointerRef), MDRef(pointee: Ref.Sig)) =>
          require(pointee.tag in (PrimitiveRef, CPointerRef, CStringRef, RecordRef, MonomorphicRecordRef,
            TupleRef, FunctionalTypeRef, VArrayRef),
            s"unexpected pointee type ${pointee.md}", md)
          Some(Ref.CPointer(pointee))

        case MDNode(tag @ MDTag(CStringRef)) =>
          Some(Ref.CString)

        case MDNode(tag @ MDTag(NullableRef), MDRef(referenceType: Ref.Type)) =>
          val tags = Set(ClassRef, MonomorphicClassRef, InterfaceRef, MonomorphicInterfaceRef,
            ArrayRef, JavaClassRef, JavaInterfaceRef, JavaArrayRef, RawEnumRef) ++
            (if (version.hasUniversalGenericsForFusion) Seq(InstantiatedClassRef, InstantiatedInterfaceRef) else Set.empty)
          require(tags contains referenceType.tag,
            s"unexpected reference type ${referenceType.md}", md)
          Some(Ref.Nullable(referenceType))

        case MDNode(tag @ MDTag(RawEnumRef), MDString(name), MDRef(baseType: Ref.Type)) =>
          val tags = Set(PrimitiveRef, ClassRef, MonomorphicClassRef, InterfaceRef, MonomorphicInterfaceRef) ++
            (if (version.hasUniversalGenericsForFusion) Seq(InstantiatedClassRef, InstantiatedInterfaceRef) else Set.empty)
          require(tags contains baseType.tag,
            s"unexpected underlying base type ${baseType.md}", md)
          Some(Ref.RawEnum(name, baseType))

        case MDNode(tag @ MDTag(FunctionalTypeRef), MDRef(returnType: Ref.Type), parameterTypesMD @ MDRefs(parameterTypes*)) =>
          require(TypeTags(returnType.tag),
            s"unexpected return type ${returnType.md}", md)
          require(parameterTypes.forall(x => TypeTags(x.tag)),
            s"unexpected parameter types $parameterTypesMD", md)
          Some(Ref.FunctionalType(returnType, parameterTypes.asInstanceOf[Seq[Ref.Type]]))

        case MDNode(tag @ MDTag(TypeVariableRef), MDRef(generic: Ref.Generic), paramMD @ MDRef(param: Ref.TypeParameter)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(generic.tag in (GenericClassRef, GenericInterfaceRef, GenericRecordRef,
            GenericInstanceMethodRef, GenericStaticMethodRef, GenericGlobalFunctionRef),
            s"unexpected generic type or function ${generic.md}", md)
          require(generic.typeParameters.contains(param),
            s"incompatible type parameter $paramMD", md)
          Some(Ref.TypeVariable(generic, param))

        case MDNode(tag @ MDTag(OwnTypeVariableRef), MDRef(param: Ref.TypeParameter)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          Some(Ref.OwnTypeVariable(param))

        case MDNode(MDTag(PackageRef), MDString(name)) =>
          require(name.nonEmpty, s"empty name", md)
          val ref = Ref.Package(name)
          if (name == packageName) {
            packageRef.initOrElse(ref,
              parsingError(s"duplicate ${Tag.PackageRef} for $packageName", md)
            )
          }
          Some(ref)

        case MDNode(tag @ MDTag(GenericClassRef), MDString(name), typeParametersMD @ MDRefs(typeParameters*)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(name.nonEmpty, s"empty name", md)
          require(typeParameters.nonEmpty, "empty type parameters", md)
          require(typeParameters.forall(_.tag in (TypeParameterRef)),
            s"unexpected type parameters $typeParametersMD", md)
          Some(Ref.GenericClass(name, typeParameters.asInstanceOf[Seq[Ref.TypeParameter]]))

        case MDNode(tag @ MDTag(GenericInterfaceRef), MDString(name), typeParametersMD @ MDRefs(typeParameters*)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(name.nonEmpty, s"empty name", md)
          require(typeParameters.nonEmpty, "empty type parameters", md)
          require(typeParameters.forall(_.tag in (TypeParameterRef)),
            s"unexpected type parameters $typeParametersMD", md)
          Some(Ref.GenericInterface(name, typeParameters.asInstanceOf[Seq[Ref.TypeParameter]]))

        case MDNode(tag @ MDTag(GenericRecordRef), MDString(name), typeParametersMD @ MDRefs(typeParameters*)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(name.nonEmpty, s"empty name", md)
          require(typeParameters.nonEmpty, "empty type parameters", md)
          require(typeParameters.forall(_.tag in (TypeParameterRef)),
            s"unexpected type parameters $typeParametersMD", md)
          Some(Ref.GenericRecord(name, typeParameters.asInstanceOf[Seq[Ref.TypeParameter]]))

        case MDNode(MDTag(InstanceMethodRef), MDRef(refType: Ref), MDString(name), sigMD @ MDRef(sig: Ref.FunctionalType)) =>
          require(refType.tag in (ClassRef, GenericClassRef, InstantiatedClassRef, MonomorphicClassRef,
            InterfaceRef, GenericInterfaceRef, InstantiatedInterfaceRef, MonomorphicInterfaceRef,
            RecordRef, GenericRecordRef, InstantiatedRecordRef, MonomorphicRecordRef,
            TupleRef, JavaClassRef, JavaInterfaceRef,
            InterfaceExtensionRef, GenericInterfaceExtensionRef, InstantiatedInterfaceExtensionRef),
            s"unexpected reference type ${refType.md}", md)
          require(name.nonEmpty, s"empty name", md)
          Some(Ref.InstanceMethod(refType, name, sig))

        case MDNode(tag @ MDTag(GenericInstanceMethodRef), MDRef(refType: Ref), MDString(name), typeParametersMD @ MDRefs(typeParameters*), sigMD @ MDRef(sig: Ref.FunctionalType)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(refType.tag in (ClassRef, GenericClassRef, InterfaceRef, GenericInterfaceRef, RecordRef, GenericRecordRef,
            InterfaceExtensionRef, InstantiatedInterfaceExtensionRef),
            s"unexpected reference type ${refType.md}", md)
          require(name.nonEmpty, s"empty name", md)
          require(typeParameters.nonEmpty, "empty type parameters", md)
          require(typeParameters.forall(_.tag in (TypeParameterRef)),
            s"unexpected type parameters $typeParametersMD", md)
          Some(Ref.GenericInstanceMethod(refType, name, typeParameters.asInstanceOf[Seq[Ref.TypeParameter]], sig))

        case MDNode(tag @ MDTag(InstantiatedInstanceMethodRef), MDRef(generic: Ref.GenericInstanceMethod),
                    MDRef(refType: Ref.Type), instantiatedTypeParametersMD @ MDRefs(instantiatedTypeParameters*), sigMD @ MDRef(sig: Ref.FunctionalType)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(refType.tag in (ClassRef, InstantiatedClassRef, InterfaceRef, InstantiatedInterfaceRef, RecordRef, InstantiatedRecordRef,
            InterfaceExtensionRef, InstantiatedInterfaceExtensionRef),
            s"unexpected reference type ${refType.md}", md)
          require(instantiatedTypeParameters.nonEmpty, "empty type parameters", md)
          require(instantiatedTypeParameters.size == generic.typeParameters.size,
            "incompatible number of instantiated type parameters", md)
          require(instantiatedTypeParameters.forall(x => TypeTags(x.tag) || (x.tag in (ThisTypeRef))),
            s"unexpected type parameters $instantiatedTypeParametersMD", md)
          Some(Ref.InstantiatedInstanceMethod(generic, refType, instantiatedTypeParameters.asInstanceOf[Seq[Ref.TypeOrThisType]], sig))

        case MDNode(MDTag(StaticMethodRef), MDRef(refType: Ref), MDString(name), sigMD @ MDRef(sig: Ref.FunctionalType)) =>
          require(refType.tag in (ClassRef, GenericClassRef, InstantiatedClassRef, MonomorphicClassRef,
            InterfaceRef, GenericInterfaceRef, InstantiatedInterfaceRef, MonomorphicInterfaceRef,
            RecordRef, GenericRecordRef, InstantiatedRecordRef, MonomorphicRecordRef,
            TupleRef, JavaClassRef, JavaInterfaceRef,
            InterfaceExtensionRef, GenericInterfaceExtensionRef, InstantiatedInterfaceExtensionRef),
            s"unexpected reference type ${refType.md}", md)
          require(name.nonEmpty, s"empty name", md)
          Some(Ref.StaticMethod(refType, name, sig))

        case MDNode(tag @ MDTag(GenericStaticMethodRef), MDRef(refType: Ref), MDString(name), typeParametersMD @ MDRefs(typeParameters*), sigMD @ MDRef(sig: Ref.FunctionalType)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(refType.tag in (ClassRef, GenericClassRef, InterfaceRef, GenericInterfaceRef, RecordRef, GenericRecordRef,
            InterfaceExtensionRef, InstantiatedInterfaceExtensionRef),
            s"unexpected reference type ${refType.md}", md)
          require(name.nonEmpty, s"empty name", md)
          require(typeParameters.nonEmpty, "empty type parameters", md)
          require(typeParameters.forall(_.tag in (TypeParameterRef)),
            s"unexpected type parameters $typeParametersMD", md)
          Some(Ref.GenericStaticMethod(refType, name, typeParameters.asInstanceOf[Seq[Ref.TypeParameter]], sig))

        case MDNode(tag @ MDTag(InstantiatedStaticMethodRef), MDRef(generic: Ref.GenericStaticMethod),
        MDRef(refType: Ref.Type), instantiatedTypeParametersMD @ MDRefs(instantiatedTypeParameters*), sigMD @ MDRef(sig: Ref.FunctionalType)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(refType.tag in (ClassRef, InstantiatedClassRef, InterfaceRef, InstantiatedInterfaceRef, RecordRef, InstantiatedRecordRef,
            InterfaceExtensionRef, InstantiatedInterfaceExtensionRef),
            s"unexpected reference type ${refType.md}", md)
          require(instantiatedTypeParameters.nonEmpty, "empty type parameters", md)
          require(instantiatedTypeParameters.size == generic.typeParameters.size,
            "incompatible number of instantiated type parameters", md)
          require(instantiatedTypeParameters.forall(x => TypeTags(x.tag) || (x.tag in (ThisTypeRef))),
            s"unexpected type parameters $instantiatedTypeParametersMD", md)
          Some(Ref.InstantiatedStaticMethod(generic, refType, instantiatedTypeParameters.asInstanceOf[Seq[Ref.TypeOrThisType]], sig))

        case MDNode(MDTag(GlobalFunctionRef), MDRef(pkg: Ref.Package), MDString(name), sigMD @ MDRef(sig: Ref.FunctionalType)) =>
          require(name.nonEmpty, s"empty name", md)
          Some(Ref.GlobalFunction(pkg, name, sig))

        case MDNode(tag @ MDTag(GenericGlobalFunctionRef), MDRef(pkg: Ref.Package), MDString(name),
                    typeParametersMD @ MDRefs(typeParameters*), sigMD @ MDRef(sig: Ref.FunctionalType)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(name.nonEmpty, s"empty name", md)
          require(typeParameters.nonEmpty, s"empty type parameters", md)
          require(typeParameters.forall(_.tag in (TypeParameterRef)),
            s"unexpected type parameters $typeParametersMD", md)
          Some(Ref.GenericGlobalFunction(pkg, name, typeParameters.asInstanceOf[Seq[Ref.TypeParameter]], sig))

        case MDNode(tag @ MDTag(InstantiatedGlobalFunctionRef), MDRef(generic: Ref.GenericGlobalFunction),
                    instantiatedTypeParametersMD @ MDRefs(instantiatedTypeParameters*), sigMD @ MDRef(sig: Ref.FunctionalType)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(instantiatedTypeParameters.nonEmpty, "empty type parameters", md)
          require(instantiatedTypeParameters.size == generic.typeParameters.size,
            "incompatible number of instantiated type parameters", md)
          require(instantiatedTypeParameters.forall(x => TypeTags(x.tag) || (x.tag in (ThisTypeRef))),
            s"unexpected type parameters $instantiatedTypeParametersMD", md)
          Some(Ref.InstantiatedGlobalFunction(generic, instantiatedTypeParameters.asInstanceOf[Seq[Ref.TypeOrThisType]], sig))

        case MDNode(tag @ MDTag(GlobalCFunctionRef), MDRef(pkg: Ref.Package), MDString(name), sigMD @ MDRef(sig: Ref.FunctionalType)) =>
          require(name.nonEmpty, s"empty name", md)
          Some(Ref.GlobalCFunction(pkg, name, sig))

        case MDNode(tag @ MDTag(ForeignCFunctionRef), MDString(name), sigMD @ MDRef(sig: Ref.FunctionalType)) =>
          require(name.nonEmpty, s"empty name", md)
          Some(Ref.ForeignCFunction(name, sig))

        case MDNode(MDTag(InstanceFieldRef), MDRef(refType: Ref), MDString(name), sigMD @ MDRef(sig: Ref.Type)) =>
          require(refType.tag in (ClassRef, GenericClassRef, InstantiatedClassRef, MonomorphicClassRef,
            RecordRef, GenericRecordRef, InstantiatedRecordRef, MonomorphicRecordRef,
            TupleRef, JavaClassRef),
            s"unexpected reference type ${refType.md}", md)
          require(name.nonEmpty, s"empty name", md)
          require(TypeTags(sig.tag), s"unexpected signature ${sig.md}", md)
          Some(Ref.InstanceField(refType, name, sig))

        case MDNode(MDTag(StaticFieldRef), MDRef(refType: Ref), MDString(name), sigMD @ MDRef(sig: Ref.Type)) =>
          require(refType.tag in (ClassRef, GenericClassRef, InstantiatedClassRef, MonomorphicClassRef,
            InterfaceRef, GenericInterfaceRef, InstantiatedInterfaceRef, MonomorphicInterfaceRef,
            RecordRef, GenericRecordRef, InstantiatedRecordRef, MonomorphicRecordRef,
            TupleRef, JavaClassRef, JavaInterfaceRef),
            s"unexpected reference type ${refType.md}", md)
          require(name.nonEmpty, s"empty name", md)
          require(TypeTags(sig.tag), s"unexpected signature ${sig.md}", md)
          Some(Ref.StaticField(refType, name, sig))

        case MDNode(MDTag(GlobalVariableRef), MDRef(pkg: Ref.Package), MDString(name), sigMD @ MDRef(sig: Ref.Type)) =>
          require(name.nonEmpty, s"empty name", md)
          require(TypeTags(sig.tag), s"unexpected signature ${sig.md}", md)
          Some(Ref.GlobalVariable(pkg, name, sig))

        case MDNode(tag @ MDTag(ParameterRef), refMD @ MDRef(func: Ref.HasParameters), MDNumber(index), MDString(name), sigMD @ MDRef(sig: Ref.Type)) =>
          require(func.tag in (InstanceMethodRef, GenericInstanceMethodRef, InstantiatedInstanceMethodRef,
            StaticMethodRef, GenericStaticMethodRef, InstantiatedStaticMethodRef,
            GlobalFunctionRef, GenericGlobalFunctionRef, InstantiatedGlobalFunctionRef),
            s"unexpected method or function reference $refMD", md)
          require(!func.parameters.exists(_.index == index), s"duplicate $ParameterRef at $index for $refMD", md)
          require(TypeTags(sig.tag), s"unexpected signature ${sig.md}", md)
          val parameter = Ref.Parameter(func, index.toInt, name, sig)
          func.parameters += parameter
          Some(parameter)

        case MDNode(tag @ MDTag(TypeParameterRef), MDString(name)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(name.nonEmpty, s"empty name", md)
          Some(Ref.TypeParameter(name))

        case MDNode(tag @ MDTag(BoxRef), MDRef(baseType: Ref.Type)) =>
          requireVersion(version.hasUniversalGenericsForFusion, tag)
          Some(Ref.Box(baseType))

        case MDNode(tag @ MDTag(InterfaceExtensionRef), MDRef(baseType: Ref.Type), interfacesMD @ MDRefs(interfaces*)) =>
          requireVersion(version.hasUniversalGenericsForFusion, tag)
          require(interfaces.nonEmpty, "empty interfaces", md)
          require(interfaces.forall(_.tag in (InterfaceRef, InstantiatedInterfaceRef, MonomorphicInterfaceRef)) ||
            (env.enabled(StdCoreAnyHierarchyRoot) && interfaces.count(_.isInstanceOf[Ref.Class]) == 1 &&
              interfaces.contains(stdCoreAny.get)),
            s"unexpected interfaces $interfacesMD", md)
          requireNoDuplicates(interfaces, md)
          Some(Ref.InterfaceExtension(baseType, interfaces.asInstanceOf[Seq[Ref.Type]]))

        case MDNode(tag @ MDTag(InstantiatedInterfaceExtensionRef), MDRef(generic: Ref.GenericInterfaceExtension), instantiatedTypeParametersMD @ MDRefs(instantiatedTypeParameters*)) =>
          requireVersion(version.hasUniversalGenericsForFusion, tag)
          require(instantiatedTypeParameters.nonEmpty, "empty instantiated type parameters", md)
          require(instantiatedTypeParameters.size == generic.typeParameters.size,
            "incompatible number of instantiated type parameters", md)
          require(instantiatedTypeParameters.forall(x => TypeTags(x.tag)),
            s"unexpected instantiated type parameters $instantiatedTypeParametersMD", md)
          Some(Ref.InstantiatedInterfaceExtension(generic, instantiatedTypeParameters.asInstanceOf[Seq[Ref.Type]]))

        case MDNode(tag @ MDTag(GenericInterfaceExtensionRef), MDRef(baseType: Ref.Type), interfacesMD @ MDRefs(interfaces*), typeParametersMD @ MDRefs(typeParameters*)) =>
          requireVersion(version.hasUniversalGenericsForFusion, tag)
          require(interfaces.nonEmpty, "empty interfaces", md)
          require(interfaces.forall(_.tag in (InterfaceRef, InstantiatedInterfaceRef, MonomorphicInterfaceRef)) ||
            (env.enabled(StdCoreAnyHierarchyRoot) && interfaces.count(_.isInstanceOf[Ref.Class]) == 1 &&
              interfaces.contains(stdCoreAny.get)),
            s"unexpected interfaces $interfacesMD", md)
          requireNoDuplicates(interfaces, md)
          require(typeParameters.nonEmpty, "empty type parameters", md)
          require(typeParameters.forall(_.tag in (TypeParameterRef)),
            s"unexpected type parameters $typeParametersMD", md)
          Some(Ref.GenericInterfaceExtension(baseType, interfaces.asInstanceOf[Seq[Ref.Type]], typeParameters.asInstanceOf[Seq[Ref.TypeParameter]]))

        case MDNode(tag @ MDTag(ThisTypeRef)) =>
          requireVersion(version.hasUniversalGenericsForFusion, tag)
          Some(Ref.ThisType)

        case MDNode(tag @ MDTag(PackageDef),
                    refMD @ MDRef(ref: Ref.Package),
                    pkgsMD @ MDRefs(importedPackages*),
                    globalsMD @ MDRefs(globals*),
                    restMD: _*) =>
          require(ref.tag in (PackageRef),
            s"unexpected ref $refMD", md)
          require(importedPackages.forall(_.tag in (PackageRef)),
            s"unexpected imported packages $pkgsMD", md)
          requireNoDuplicates(importedPackages, md)
          require(globals.forall(_.tag in (GlobalFunctionRef, GenericGlobalFunctionRef, GlobalCFunctionRef, GlobalVariableRef)),
            s"unexpected globals $globalsMD", md)
          requireNoDuplicates(globals, md)

          val modifiers = restMD match {
            case Seq(modifiersMD @ MDModifiers(modifiers*)) =>
              require(modifiers.forall(_ in (`public`, `protected`, `private`, `internal`)),
                s"unexpected modifiers $modifiersMD", md)
              requireNoDuplicates(modifiers, md)
              requireExplicitAccessModifiers(modifiers, md)
              modifiers
            case _ =>
              parsingError(s"unexpected elements ${restMD.mkString("[", ",", "]")}", md)
              Seq()
          }

          ref.packageDef.initOrElse(
            Ref.PackageDef(
              importedPackages.asInstanceOf[Seq[Ref.Package]],
              globals.asInstanceOf[Seq[Ref.Global]],
              modifiers.toSet),
            parsingError(s"duplicate $tag", md)
          )
          None

        case MDNode(tag @ MDTag(ClassDef),
                    refMD @ MDRef(ref: Ref.HasClassDef),
                    superclassMD @ MDRefs(superclassSeq*),
                    superinterfacesMD @ MDRefs(superinterfaces*),
                    membersMD @ MDRefs(members*),
                    modifiersMD @ MDModifiers(modifiers*)) =>
          require(superclassSeq.size <= 1,
            s"more than one superclass $superclassMD", md)
          val superclass = ScalaCollections.singleton(superclassSeq)
          require(superclass.forall(_.tag in (ClassRef, InstantiatedClassRef, MonomorphicClassRef, JavaClassRef)),
            s"unexpected superclass $superclassMD", md)
          require(superinterfaces.forall(_.tag in (InterfaceRef, InstantiatedInterfaceRef, MonomorphicInterfaceRef, JavaInterfaceRef)) ||
            (superinterfaces.length == 1 && superinterfaces.head.tag == ClassRef && ref.name.startsWith(CangjieSymLevelMaker.CANGJIE_LAMBDA_PREFIX)) ||
              (env.enabled(StdCoreAnyHierarchyRoot) && superinterfaces.count(_.isInstanceOf[Ref.Class]) == 1 &&
                superinterfaces.contains(stdCoreAny.get)),
            s"unexpected superinterfaces $superinterfacesMD", md)
          requireNoDuplicates(superinterfaces, md)
          require(members.forall(_.tag in (InstanceMethodRef, GenericInstanceMethodRef, StaticMethodRef, GenericStaticMethodRef,
            InstanceFieldRef, StaticFieldRef)),
            s"unexpected members $membersMD", md)
          requireNoDuplicates(members, md)
          require(modifiers.forall(_ in (`public`, `protected`, `private`, `internal`, `open`, `sealed`, `abstract`)),
            s"unexpected modifiers $modifiersMD", md)
          requireNoDuplicates(modifiers, md)
          requireExplicitAccessModifiers(modifiers, md)

          if (ref.name.startsWith(CangjieSymLevelMaker.CANGJIE_LAMBDA_PREFIX) &&
            superinterfaces.headOption.exists(_.isInstanceOf[Ref.Class])) {

            val members0: Seq[Ref.MemberDef] = ref.classDef.getOption match {
              case Some(classDef) =>
                val (fields, methods) = classDef.members.partition(_.isInstanceOf[Ref.InstanceField])
                val curMembers = if (fields.nonEmpty) fields else methods
                curMembers ++ members.asInstanceOf[Seq[Ref.MemberDef]]
              case None =>
                members.asInstanceOf[Seq[Ref.MemberDef]]
            }

            val interfacesSeq = if (env.enabled(StdCoreAnyHierarchyRoot)) {
              Seq.empty[Ref]
            } else {
              Seq(stdCoreAny.get)
            }

            require(superclass.contains(stdCoreObject.get) || superclass.contains(ref),
              "unexpected superclass of lambda type (expected std.core.Object or Auto_Env)", md)

            // Lambda classes in this branch must have LambdaCommon as superclass, so we don't need to change it to std.core.Any[Ref.Class]
            ref.classDef.init(Ref.ClassDef(Some(superinterfaces.head), interfacesSeq, members0, Set(`private`, `immutable`), isLambdaClass = true))
          } else if (env.enabled(StdCoreAnyHierarchyRoot) && ref == stdCoreObject.get) {
            stdCoreObject.get.asInstanceOf[Ref.Class].classDef.init(
              Ref.ClassDef(stdCoreAny.getOption, Seq(), members.asInstanceOf[Seq[Ref.MemberDef]], modifiers.toSet)
            )
          } else {
            superclass match {
              case Some(c: Ref.Class) =>
                require(!c.name.startsWith(CangjieSymLevelMaker.CANGJIE_LAMBDA_PREFIX), "unexpected Lambda or AutoEnv superclass", md)
              case _ =>
            }
            
            val isLambdaClass = if (env.enabled(LambdaCommonSuperclass)) {
              ref.name.startsWith(CangjieSymLevelMaker.CANGJIE_LAMBDA_PREFIX)
            } else {
                ref.name match {
                case AutoEnvRegex(_) | LambdaRegex(_) => true
                case _ => false
              }
            }

            // If option "StdCoreAnyHierarchyRoot" is enabled, we need to
            // 1. Remove std.core.Any from `superinterfaces`
            // 2. If superclass is empty, change it to std.core.Any[Ref.Class]
            val (superclass0, superinterfaces0) = if (env.enabled(StdCoreAnyHierarchyRoot)) {
              val c = if (superclass.isEmpty) {
                stdCoreAny.getOption
              } else superclass

              (c, superinterfaces.filter(_ != stdCoreAny.get))
            } else (superclass, superinterfaces)

            ref.classDef.initOrElse(Ref.ClassDef(superclass0, superinterfaces0, members.asInstanceOf[Seq[Ref.MemberDef]], modifiers.toSet,
              isLambdaClass),
              parsingError(s"duplicate $tag", md)
            )
          }
          None

        case MDNode(tag @ MDTag(InterfaceDef),
                    // All LambdaCommon Ref.Interfaces at that moment must be a Ref.Classes.
                    // So here ref could be either Ref.HasInterfaceDef or Ref.HasClassDef.
                    // std.core.Any is also interface that should be transformed to a Ref.Class
                    refMD @ MDRef(ref: (Ref.HasInterfaceDef | Ref.HasClassDef)),
                    superinterfacesMD @ MDRefs(superinterfaces*),
                    membersMD @ MDRefs(members*),
                    modifiersMD @ MDModifiers(modifiers*)) =>
          require(superinterfaces.forall(_.tag in (InterfaceRef, InstantiatedInterfaceRef, MonomorphicInterfaceRef, JavaInterfaceRef)) ||
            (env.enabled(StdCoreAnyHierarchyRoot) && superinterfaces.count(_.isInstanceOf[Ref.Class]) == 1 &&
              superinterfaces.contains(stdCoreAny.get)),
            s"unexpected superinterfaces $superinterfacesMD", md)
          requireNoDuplicates(superinterfaces, md)
          require(members.forall(_.tag in (InstanceMethodRef, GenericInstanceMethodRef, StaticMethodRef, GenericStaticMethodRef, StaticFieldRef)),
            s"unexpected members $membersMD", md)
          requireNoDuplicates(members, md)
          require(modifiers.forall(_ in (`public`, `protected`, `private`, `internal`, `open`, `sealed`, `abstract`)),
            s"unexpected modifiers $modifiersMD", md)
          requireNoDuplicates(modifiers, md)
          requireExplicitAccessModifiers(modifiers, md)

          ref match {
            case ref: Ref.HasInterfaceDef =>
              val superinterfaces0 = if (env.enabled(StdCoreAnyHierarchyRoot)) superinterfaces.filter(_ != stdCoreAny.get) else superinterfaces
              ref.interfaceDef.initOrElse(Ref.InterfaceDef(superinterfaces0, members.asInstanceOf[Seq[Ref.MemberDef]], modifiers.toSet),
                parsingError(s"duplicate $tag", md)
              )
            // std.core.Any's ClassDef should be initialized when std.core.Object is parsed (if option StdCoreAnyHierarchyRoot is enabled)
            case ref: Ref.HasClassDef if ref == stdCoreAny.get =>
              require(env.enabled(StdCoreAnyHierarchyRoot), "unexpected std.core.Any reference type", md)
              ref.classDef.init(Ref.ClassDef(None, Seq(), Seq(), Set(`open`, `public`)))
            case ref: Ref.HasClassDef =>
              require(env.enabled(LambdaCommonSuperclass), "unexpected lambda's interface", md)
              require(LambdaCommonRegex.pattern.matcher(ref.name).matches, "unexpected Lambda name format", md)
              val rootOfHierarchy = if (env.enabled(StdCoreAnyHierarchyRoot)) stdCoreAny else stdCoreObject
              val superinterfaces0 = if (env.enabled(StdCoreAnyHierarchyRoot)) Seq.empty[Ref] else Seq(stdCoreAny.get)
              ref.classDef.init(Ref.ClassDef(rootOfHierarchy.getOption, superinterfaces0, members.asInstanceOf[Seq[Ref.MemberDef]], (modifiers :+ `abstract`).toSet, isLambdaClass = true))
          }
          None

        case MDNode(tag @ MDTag(RecordDef),
                    refMD @ MDRef(ref: Ref.HasRecordDef),
                    membersMD @ MDRefs(members*),
                    modifiersMD @ MDModifiers(modifiers*)) =>
          require(members.forall(_.tag in (InstanceMethodRef, GenericInstanceMethodRef, StaticMethodRef, GenericStaticMethodRef, InstanceFieldRef, StaticFieldRef)),
            s"unexpected members $membersMD", md)
          requireNoDuplicates(members, md)
          require(modifiers.forall(_ in (`public`, `protected`, `private`, `internal`)),
            s"unexpected modifiers $modifiersMD", md)
          requireNoDuplicates(modifiers, md)
          requireExplicitAccessModifiers(modifiers, md)

          ref.recordDef.initOrElse(Ref.RecordDef(members.asInstanceOf[Seq[Ref.MemberDef]], modifiers.toSet),
            parsingError(s"duplicate $tag", md)
          )
          None

        case MDNode(tagMD @ MDTag(tag @ (InstanceMethodDef | StaticMethodDef | GlobalFunctionDef |
                                         InstanceFieldDef | StaticFieldDef | GlobalVariableDef)),
                    refMD @ MDRef(ref: Ref.HasModifiers),
                    modifiersMD @ MDModifiers(modifiers*)) =>
          val accessModifiers = Seq(`public`, `protected`, `private`, `internal`)
          val (allowedRefs, allowedModifiers) = tag match {
            case InstanceMethodDef => (Seq(InstanceMethodRef, GenericInstanceMethodRef), Seq(`open`, `abstract`, `mut`, `override`))
            case StaticMethodDef   => (Seq(StaticMethodRef, GenericStaticMethodRef),   Seq(`abstract`, `redef`))
            case GlobalFunctionDef => (Seq(GlobalFunctionRef, GenericGlobalFunctionRef, GlobalCFunctionRef), Seq.empty)
            case InstanceFieldDef  => (Seq(InstanceFieldRef),  Seq(`immutable`))
            case StaticFieldDef    => (Seq(StaticFieldRef),    Seq(`immutable`))
            case GlobalVariableDef => (Seq(GlobalVariableRef), Seq(`immutable`))
          }
          require(ref.tag.in(allowedRefs*),
            s"unexpected ref $refMD", md)
          require(modifiers.forall(_.in(accessModifiers ++ allowedModifiers*)),
            s"unexpected modifiers $modifiersMD", md)
          requireNoDuplicates(modifiers, md)
          requireExplicitAccessModifiers(modifiers, md)

          ref.modifiers.initOrElse(modifiers.toSet,
            parsingError(s"duplicate $tagMD", md)
          )
          None

        case MDNode(tag @ MDTag(InterfaceExtensionDef),
                    refMD @ MDRef(ref: Ref.HasInterfaceExtensionDef),
                    membersMD @ MDRefs(members*)) =>
          requireVersion(version.hasUniversalGenericsForFusion, tag)
          require(members.forall(_.tag in (InstanceMethodRef, GenericInstanceMethodRef, StaticMethodRef, GenericStaticMethodRef)),
            s"unexpected members $membersMD", md)
          requireNoDuplicates(members, md)

          ref.interfaceExtensionDef.initOrElse(Ref.InterfaceExtensionDef(members.asInstanceOf[Seq[Ref.MemberDef]]),
            parsingError(s"duplicate $tag", md)
          )
          None

        case MDNode(tag @ MDTag(AnnotationLink), MDRef(ref: Ref.HasAnnotations), MDRef(annotation: Ref.Annotation)) =>
          annotation match {
            case _: Ref.CangjieAnnotation =>
              require(ref.annotations.isEmpty, "more than one annotation factory", md)
            case _: Ref.JavaAnnotations =>
              require(ref.annotations.forall(_.isInstanceOf[Ref.JavaAnnotations]),
                s"unexpected non-Java annotations ${ref.annotations.mkString("[", ",", "]")}", md)
          }
          ref.annotations += annotation
          None

        case MDNode(tag @ MDTag(CangjieAnnotationFactory), MDRef(factory: Ref)) =>
          require(factory.tag in (StaticMethodRef, GlobalFunctionRef),
            s"unexpected Cangjie annotation factory function ${factory.md}", md)
          Some(Ref.CangjieAnnotation(factory))

        case MDNode(tag @ MDTag(JavaSignatureAttribute), MDRef(ref: Ref.HasJavaSignatureAttribute), MDString(sig)) =>
          ref.javaSignatureAttribute.init(sig)
          None

        case MDNode(tagMD @ MDTag(tag @ (JavaRuntimeVisibleAnnotations | JavaRuntimeInvisibleAnnotations)), valuesMD @ MDRefs(values*)) =>
          require(values.forall(_.tag == JavaAnnotation), s"unexpected $tagMD $valuesMD", md)
          Some(Ref.JavaAnnotations(tag, values.asInstanceOf[Seq[Ref.JavaAnnotation]]))

        case MDNode(tag @ MDTag(JavaAnnotation), MDRef(tpe: Ref.JavaInterface), elementsMD @ MDRefs(elements*)) =>
          require(elements.forall(_.tag == JavaAnnotationElement), s"unexpected $tag elements $elementsMD", md)
          Some(Ref.JavaAnnotation(tpe, elements.asInstanceOf[Seq[Ref.JavaAnnotationElement]]))

        case MDNode(tag @ MDTag(JavaAnnotationElement), MDString(name), MDRef(value: Ref)) =>
          require(JavaAnnotationElementTags(value.tag), s"unexpected $tag value ${value.tag}", md)
          Some(Ref.JavaAnnotationElement(name, value))

        case MDNode(tag @ MDTag(JavaAnnotationNumericConstant), MDRef(tpe: Ref.Primitive), MDNumber(value)) =>
          // TODO: verify type
          Some(Ref.JavaAnnotationNumericConstant(tpe, value))

        case MDNode(tag @ MDTag(JavaAnnotationString), MDString(value)) =>
          Some(Ref.JavaAnnotationString(value))

        case MDNode(tag @ MDTag(JavaAnnotationEnumValue), MDRef(ref: Ref.StaticField)) =>
          // TODO: checks for enum declaring class
          Some(Ref.JavaAnnotationEnumValue(ref))

        case MDNode(tag @ MDTag(JavaAnnotationArrayValue), valuesMD @ MDRefs(values*)) =>
          require(values.forall(x => JavaAnnotationElementTags(x.tag)), s"unexpected $tag values $valuesMD", md)
          require(values.forall(_.tag != JavaAnnotationArrayValue), s"nested array values found in $valuesMD", md)
          Some(Ref.JavaAnnotationArrayValue(values))

        case MDNode(tag @ MDTag(LinkageName), refMD @ MDRef(ref), MDString(value)) =>
          def defaultLinkageNameInit(): Unit = {
            require(!linkageNameToRef.contains(value), s"duplicate $tag", md)

            ref.linkageName.initOrElse(value,
              parsingError(s"duplicate $tag for $refMD", md)
            )
          }

          require(value.nonEmpty, "empty linkage name", md)

          if (env.enabled(LambdaCommonSuperclass)) {
            value match {
              case AutoEnvRegexLinkageName(nameCheck) =>
                require(ref.tag == ClassRef && resolveLambda(value)._1, "unexpected Auto_Env type tag", md)
                val autoEnvWithoutPackage = nameCheck.patch(nameCheck.lastIndexOf(packageName), "", packageName.length)
                require(ref.asInstanceOf[Ref.Class].name == CangjieSymLevelMaker.CANGJIE_LAMBDA_PREFIX + autoEnvWithoutPackage,
                  "unexpected Auto_Env name format", md)
                if (!ref.linkageName.initialized) {
                  ref.linkageName.makeMutable
                  ref.linkageName.initOrElse(value,
                    parsingError(s"duplicate $tag for $refMD", md)
                  )
                }
              case _ => defaultLinkageNameInit()
            }
          } else {
            defaultLinkageNameInit()
          }
          linkageNameToRef(value) = ref

          None

        case MDNode(tag @ MDTag(ConstantString), MDString(value)) =>
          Some(Ref.ConstantString(value))

        case MDNode(tag @ MDTag(GenericConstraints), typeVariableMD @ MDRef(typeVariable: Ref.TypeVariable), upperBoundsMD @ MDRefs(upperBounds*)) =>
          requireVersion(version.hasUniversalGenerics, tag)
          require(upperBounds.nonEmpty, "empty type parameters", md)
          require(upperBounds.forall(x => TypeTags(x.tag)),
            s"unexpected type parameters $upperBoundsMD", md)
          require(typeVariable.generic.constraints.forall(_.typeVariable != typeVariable),
            s"multiple constraints for single type variable $typeVariableMD", md)
          val constraints = Ref.GenericConstraints(typeVariable, upperBounds.asInstanceOf[Seq[Ref.Type]])
          typeVariable.generic.constraints += constraints
          None

        case MDNode(MDTag(tag), _*) =>
          parsingError(s"malformed $tag entry", md)
          None

        case _ =>
          parsingError(s"unexpected metadata entry in !hlir.metadata", md)
          None
      }
    }

    object MDTag {
      def unapply(x: MDString): Option[Tag] = {
        try Some(Tag.valueOf(x.value))
        catch {
          case _: IllegalArgumentException => None
        }
      }
    }

    object MDModifier {
      def unapply(x: MDString): Option[Modifier] = {
        try Some(Modifier.valueOf(x.value))
        catch {
          case _: IllegalArgumentException => None
        }
      }
    }

    object MDModifiers {
      def unapplySeq(x: MDItem): Option[Seq[Modifier]] = x.resolve() match {
        case x: MDNode => ScalaCollections.sequence(x.elts.toSeq map {
          case MDModifier(m) => Some(m)
          case _ => None
        })
        case _ => None
      }
    }

    object MDRef {
      def unapply(x: MDItem): Option[Ref] = processEntry(x)
    }

    def requireNoDuplicates[T](xs: IterableOnce[T], token: => AnyRef)(implicit toStr: T => String): Unit = {
      val duplicates = ScalaCollections.collectDuplicates(xs)
      require(duplicates.isEmpty, s"unexpected duplicates ${duplicates.map(toStr).mkString("!{", ",", "}")}", token)
    }
    
    def requireExplicitAccessModifiers(modifiers: Seq[Modifier] , token: => AnyRef)(implicit toStr: Modifier => String): Unit = {
      if (env.enabled(HLIRExplicitAccessModifiers)) {
        require(modifiers.exists(_.isAccessModifier),
          s"missing access modifier in ${modifiers.map(toStr).mkString("!{", ",", "}")}", token)
      }
    }

    object MDRefs {
      // Note the use of `sequence` here: if any of the elements are not processed as refs,
      // then the whole unapply will not match. In case of `parseEntry` routine, it will result
      // in reporting of malformed entry error.
      def unapplySeq(x: MDItem): Option[Seq[Ref]] = x.resolve() match {
        case x: MDNode => ScalaCollections.sequence(x.elts.toSeq map processEntry)
        case _ => None
      }
    }

    object MDNumber {
      def unapply(x: MDValue): Option[Long] = module.getConstValue(x)
    }
  }
}

/** HLIR metadata representation.
  *
  * Specification:
  *
  *  - [[Tag]] describes all tags of HLIR metadata entries according to specification.
  *    Should be used only for initial HLIR metadata parsing from bitcode.
  *  - [[Ref]] describes referenceable entities such as types, functions and annotations,
  *    which can be referenced from other metadata entries and from HLIR intrinsics in bitcode.
  *  - Non-referenceable entries only accumulate data in corresponding [[Ref]] entities
  *    (e.g. [[Tag.InstanceMethodDef]] entry initializes modifiers of corresponding [[Ref.InstanceMethod]]).
  *
  * Note that HLIR metadata format does not allow direct or indirect recursive references,
  * so the whole metadata representation can be parsed in a single pass.
  */
object HLIRMetadata {
  enum Tag {
    // Types
    case PrimitiveRef
    case ClassRef
    case InstantiatedClassRef
    case MonomorphicClassRef
    case InterfaceRef
    case InstantiatedInterfaceRef
    case MonomorphicInterfaceRef
    case RecordRef
    case InstantiatedRecordRef
    case MonomorphicRecordRef
    case TupleRef
    case ArrayRef
    case ArraySliceRef
    case VArrayRef
    case JavaClassRef
    case JavaInterfaceRef
    case JavaArrayRef
    case CPointerRef
    case CStringRef
    case NullableRef
    case RawEnumRef
    case FunctionalTypeRef
    case TypeVariableRef
    case OwnTypeVariableRef

    // References
    case PackageRef
    case GenericClassRef
    case GenericInterfaceRef
    case GenericRecordRef
    case InstanceMethodRef
    case GenericInstanceMethodRef
    case InstantiatedInstanceMethodRef
    case StaticMethodRef
    case GenericStaticMethodRef
    case InstantiatedStaticMethodRef
    case GlobalFunctionRef
    case GenericGlobalFunctionRef
    case InstantiatedGlobalFunctionRef
    case GlobalCFunctionRef
    case ForeignCFunctionRef
    case InstanceFieldRef
    case StaticFieldRef
    case GlobalVariableRef
    case ParameterRef
    case TypeParameterRef
    case BoxRef
    case InterfaceExtensionRef
    case InstantiatedInterfaceExtensionRef
    case GenericInterfaceExtensionRef
    case ThisTypeRef

    // Definitions
    case PackageDef
    case ClassDef
    case InterfaceDef
    case RecordDef
    case InstanceMethodDef
    case StaticMethodDef
    case GlobalFunctionDef
    case InstanceFieldDef
    case StaticFieldDef
    case GlobalVariableDef
    case InterfaceExtensionDef

    // Annotations
    case AnnotationLink
    case CangjieAnnotationFactory

    // Java bytecode
    case JavaSignatureAttribute
    case JavaRuntimeVisibleAnnotations
    case JavaRuntimeInvisibleAnnotations
    case JavaAnnotation
    case JavaAnnotationElement
    case JavaAnnotationNumericConstant
    case JavaAnnotationString
    case JavaAnnotationEnumValue
    case JavaAnnotationArrayValue

    // Utility
    case LinkageName
    case ConstantString
    case GenericConstraints
  }

  val LambdaCommonRegex = """^_ZN\d+\$LambdaCommon_.*""".r
  val LambdaRegex = """^_ZN\d+\$Lambda_(.*E+)$""".r
  val AutoEnvRegex = """.*\$+Auto_Env_(.*)""".r
  val AutoEnvRegexLinkageName = """_ZN\d+\$+Auto_Env_(.*)E+""".r

  val JavaAnnotationElementTags: Set[HLIRMetadata.Tag] = {
    import Tag.*
    Set(
      JavaAnnotationNumericConstant, JavaAnnotationString, JavaAnnotationEnumValue, JavaAnnotationArrayValue,
      JavaAnnotation, JavaClassRef, JavaInterfaceRef, JavaArrayRef, PrimitiveRef
    )
  }

  val TypeTags: Set[HLIRMetadata.Tag] = {
    import Tag.*
    Set(
      PrimitiveRef, ClassRef, InstantiatedClassRef, MonomorphicClassRef,
      InterfaceRef, InstantiatedInterfaceRef, MonomorphicInterfaceRef,
      RecordRef, InstantiatedRecordRef, MonomorphicRecordRef, TupleRef,
      ArrayRef, ArraySliceRef, VArrayRef, JavaClassRef, JavaInterfaceRef, JavaArrayRef, CPointerRef, CStringRef,
      NullableRef, RawEnumRef, TypeVariableRef, OwnTypeVariableRef,
      ThisTypeRef
      // Note: FunctionalTypeRef is not considered a type until lambda and functional types are supported explicitly in HLIR
    )
  }

  implicit class TagWrapper(val self: Tag) extends AnyVal {
    def in(tags: Tag*) = tags.contains(self)
  }

  enum Modifier {
    case `public`
    case `protected`
    case `private`
    case `internal`
    case `open`
    case `sealed`
    case `abstract`
    case `immutable`
    case `mut`
    case `override`
    case `redef`

    def isAccessModifier: Boolean = this match {
      case `public` | `internal` | `protected` | `private` => true
      case _ => false
    }
  }

  implicit class ModifierWrapper(val self: Modifier) extends AnyVal {
    def in(tags: Modifier*) = tags.contains(self)
  }

  sealed trait ParsingState
  private object Unresolved extends ParsingState
  private object Resolving extends ParsingState
  private object Resolved extends ParsingState

  sealed abstract class Ref(val tag: Tag) extends ParsingState {
    val md = new DelayedValue[MDItem]
    val linkageName = new DelayedValue[String]
  }
  object Ref {

    case class Primitive(sig: String) extends Ref(Tag.PrimitiveRef) with Type {
      def asSignatureType = Primitive.parseSignatureType(sig).get
    }

    object Primitive {
      private[HLIRMetadata] def parseSignatureType(sig: String): Option[SignatureType.Primitive] = condOpt(sig) {
        case "v"  => Void
        case "n"  => Nothing
        case "u"  => Unit
        case "b"  => Boolean
        case "a"  => Int8
        case "h"  => UInt8
        case "s"  => Int16
        case "t"  => UInt16
        case "c"  => UnicodeChar32
        case "i"  => Int32
        case "j"  => UInt32
        case "l"  => Int64
        case "m"  => UInt64
        case "q"  => AddrInt
        case "r"  => AddrUInt
        case "f"  => Float32
        case "d"  => Float64
        case "Dh" => Float16
      }
    }

    case class Class(name: String) extends Ref(Tag.ClassRef) with Type with HasClassDef
    case class InstantiatedClass(generic: GenericClass, instantiatedTypeParameters: Seq[TypeOrThisType]) extends Ref(Tag.InstantiatedClassRef) with Type with InstantiatedWithName
    case class MonomorphicClass(name: String, instantiatedTypeParameters: Seq[TypeOrThisType]) extends Ref(Tag.MonomorphicClassRef) with Type with HasClassDef

    case class Interface(name: String) extends Ref(Tag.InterfaceRef) with Type with HasInterfaceDef
    case class InstantiatedInterface(generic: GenericInterface, instantiatedTypeParameters: Seq[TypeOrThisType]) extends Ref(Tag.InstantiatedInterfaceRef) with Type with InstantiatedWithName
    case class MonomorphicInterface(name: String, instantiatedTypeParameters: Seq[TypeOrThisType]) extends Ref(Tag.MonomorphicInterfaceRef) with Type with HasInterfaceDef

    case class Record(name: String) extends Ref(Tag.RecordRef) with Type with HasRecordDef with HasAnnotations
    case class InstantiatedRecord(generic: GenericRecord, instantiatedTypeParameters: Seq[Type]) extends Ref(Tag.InstantiatedRecordRef) with Type with InstantiatedWithName
    case class MonomorphicRecord(name: String, instantiatedTypeParameters: Seq[Type]) extends Ref(Tag.MonomorphicRecordRef) with Type with HasRecordDef with HasAnnotations

    case class Tuple(elemTypes: Seq[Ref]) extends Ref(Tag.TupleRef) with Type with HasRecordDef {
      def name = linkageName.get // TODO: synthesize internal name
    }

    case class Array(elemType: Type) extends Ref(Tag.ArrayRef) with Type
    case class ArraySlice(elemType: Type) extends Ref(Tag.ArraySliceRef) with Type
    case class VArray(elemType: Type, length: Long) extends Ref(Tag.VArrayRef) with Type
    case class JavaClass(name: String) extends Ref(Tag.JavaClassRef) with Type with HasClassDef with Java with HasJavaSignatureAttribute
    case class JavaInterface(name: String) extends Ref(Tag.JavaInterfaceRef) with Type with HasInterfaceDef with Java with HasJavaSignatureAttribute
    case class JavaArray(baseType: Ref.Type, dimNum: Int) extends Ref(Tag.JavaArrayRef) with Type with Java

    case class CPointer(pointee: Sig) extends Ref(Tag.CPointerRef) with Type
    case object CString extends Ref(Tag.CStringRef) with Type

    case class Nullable(referenceType: Type) extends Ref(Tag.NullableRef) with Type
    case class RawEnum(name: String, baseType: Type) extends Ref(Tag.RawEnumRef) with Type with HasName
    case class FunctionalType(returnType: Type, parameterTypes: Seq[Ref.Type]) extends Ref(Tag.FunctionalTypeRef)
    case class TypeVariable(generic: Ref.Generic, param: TypeParameter) extends Ref(Tag.TypeVariableRef) with Type
    case class OwnTypeVariable(param: TypeParameter) extends Ref(Tag.OwnTypeVariableRef) with Type

    case class PackageDef(importedPackages: Seq[Package], globals: Seq[Global], modifiers: Set[Modifier])
    case class Package(name: String) extends Ref(Tag.PackageRef) with HasName {
      val packageDef = new DelayedValue[PackageDef]
    }

    case class GenericClass(name: String, typeParameters: Seq[TypeParameter]) extends Ref(Tag.GenericClassRef) with HasClassDef with GenericWithName
    case class GenericInterface(name: String, typeParameters: Seq[TypeParameter]) extends Ref(Tag.GenericInterfaceRef) with HasInterfaceDef with GenericWithName
    case class GenericRecord(name: String, typeParameters: Seq[TypeParameter]) extends Ref(Tag.GenericRecordRef) with HasRecordDef with GenericWithName with HasAnnotations

    case class InstanceMethod(refType: Ref, name: String, sig: FunctionalType) extends Ref(Tag.InstanceMethodRef) with MethodDef
    case class GenericInstanceMethod(refType: Ref, name: String, typeParameters: Seq[TypeParameter], sig: FunctionalType) extends Ref(Tag.GenericInstanceMethodRef) with MethodDef with GenericWithName
    case class InstantiatedInstanceMethod(generic: GenericInstanceMethod, refType: Ref.Type, instantiatedTypeParameters: Seq[TypeOrThisType], sig: FunctionalType) extends Ref(Tag.InstantiatedInstanceMethodRef) with MethodRef with InstantiatedWithName

    case class StaticMethod(refType: Ref, name: String, sig: FunctionalType) extends Ref(Tag.StaticMethodRef) with MethodDef
    case class GenericStaticMethod(refType: Ref, name: String, typeParameters: Seq[TypeParameter], sig: FunctionalType) extends Ref(Tag.GenericStaticMethodRef) with MethodDef with GenericWithName
    case class InstantiatedStaticMethod(generic: GenericStaticMethod, refType: Ref.Type, instantiatedTypeParameters: Seq[TypeOrThisType], sig: FunctionalType) extends Ref(Tag.InstantiatedStaticMethodRef) with MethodRef with InstantiatedWithName

    case class GlobalFunction(pkg: Package, name: String, sig: FunctionalType) extends Ref(Tag.GlobalFunctionRef) with Global with HasModifiers with HasAnnotations with HasParameters
    case class GenericGlobalFunction(pkg: Package, name: String, typeParameters: Seq[TypeParameter], sig: FunctionalType) extends Ref(Tag.GenericGlobalFunctionRef) with Global with GenericWithName with HasModifiers with HasAnnotations with HasParameters
    case class InstantiatedGlobalFunction(generic: GenericGlobalFunction, instantiatedTypeParameters: Seq[TypeOrThisType], sig: FunctionalType) extends Ref(Tag.InstantiatedGlobalFunctionRef) with HasSignature with InstantiatedWithName {
      def pkg = generic.pkg
    }

    case class GlobalCFunction(pkg: Package, name: String, sig: FunctionalType) extends Ref(Tag.GlobalCFunctionRef) with Global with HasModifiers
    case class ForeignCFunction(name: String, sig: FunctionalType) extends Ref(Tag.ForeignCFunctionRef) with HasName with HasSignature with HasModifiers

    case class InstanceField(refType: Ref, name: String, sig: Type) extends Ref(Tag.InstanceFieldRef) with Field
    case class StaticField(refType: Ref, name: String, sig: Type) extends Ref(Tag.StaticFieldRef) with Field
    case class GlobalVariable(pkg: Package, name: String, sig: Type) extends Ref(Tag.GlobalVariableRef) with Global with HasModifiers with HasAnnotations

    case class Parameter(func: Ref, index: Int, name: String, sig: Type) extends Ref(Tag.ParameterRef) with HasName with HasSignature with HasAnnotations
    case class TypeParameter(name: String) extends Ref(Tag.TypeParameterRef) with HasName

    case class Box(baseType: Type) extends Ref(Tag.BoxRef)

    case class InterfaceExtension(baseType: Type, interfaces: Seq[Type]) extends Ref(Tag.InterfaceExtensionRef) with HasInterfaceExtensionDef
    case class InstantiatedInterfaceExtension(generic: GenericInterfaceExtension, instantiatedTypeParameters: Seq[Type]) extends Ref(Tag.InstantiatedInterfaceExtensionRef) with Instantiated[Type]
    case class GenericInterfaceExtension(baseType: Type, interfaces: Seq[Type], typeParameters: Seq[TypeParameter]) extends Ref(Tag.GenericInterfaceExtensionRef) with HasInterfaceExtensionDef with Generic

    case object ThisType extends Ref(Tag.ThisTypeRef)

    case class CangjieAnnotation(factory: Ref) extends Ref(Tag.CangjieAnnotationFactory) with Annotation

    case class JavaAnnotations(override val tag: Tag, values: Seq[Ref.JavaAnnotation]) extends Ref(tag) with Annotation with JavaAnnotationRelated {
      import Tag.*
      require(tag in (JavaRuntimeVisibleAnnotations, JavaRuntimeInvisibleAnnotations))
      val isRuntimeVisible: Boolean = tag == JavaRuntimeVisibleAnnotations
    }

    case class JavaAnnotation(tpe: Ref.JavaInterface, elements: Seq[Ref.JavaAnnotationElement]) extends Ref(Tag.JavaAnnotation) with JavaAnnotationRelated
    case class JavaAnnotationElement(name: String, value: Ref) extends Ref(Tag.JavaAnnotationElement) with JavaAnnotationRelated
    case class JavaAnnotationNumericConstant(tpe: Ref.Primitive, value: Long) extends Ref(Tag.JavaAnnotationNumericConstant) with JavaAnnotationRelated
    case class JavaAnnotationString(value: String) extends Ref(Tag.JavaAnnotationString) with JavaAnnotationRelated
    case class JavaAnnotationEnumValue(value: Ref.StaticField) extends Ref(Tag.JavaAnnotationEnumValue) with JavaAnnotationRelated
    case class JavaAnnotationArrayValue(values: Seq[Ref]) extends Ref(Tag.JavaAnnotationArrayValue) with JavaAnnotationRelated

    case class ConstantString(value: String) extends Ref(Tag.ConstantString)

    case class GenericConstraints(typeVariable: TypeVariable, upperBounds: Seq[Type]) extends Ref(Tag.GenericConstraints)

    sealed trait Type extends Ref {
      require(TypeTags(tag))
    }

    type TypeOrThisType = Type | ThisType.type

    sealed trait Java extends Ref { this: Type => }

    sealed trait HasName extends Ref {
      def name: String
    }

    type Sig = Ref.Type | Ref.FunctionalType | Ref.Box | Ref.InterfaceExtension | Ref.InstantiatedInterfaceExtension

    sealed trait HasSignature extends Ref {
      def sig: Sig
    }

    sealed trait Global extends Ref with HasName with HasSignature {
      import Tag.*
      require(tag in (GlobalFunctionRef, GenericGlobalFunctionRef, InstantiatedGlobalFunctionRef, GlobalCFunctionRef, GlobalVariableRef))

      def pkg: Package
    }

    sealed trait MemberRef extends Ref with HasName with HasSignature {
      import Tag.*
      require(tag in (InstanceMethodRef, GenericInstanceMethodRef, InstantiatedInstanceMethodRef,
        StaticMethodRef, GenericStaticMethodRef, InstantiatedStaticMethodRef,
        InstanceFieldRef, StaticFieldRef))

      def refType: Ref
    }

    sealed trait MemberDef extends MemberRef with HasModifiers with HasAnnotations with HasJavaSignatureAttribute {
      import Tag.*
      require(tag in (InstanceMethodRef, StaticMethodRef, InstanceFieldRef, StaticFieldRef, GenericInstanceMethodRef, GenericStaticMethodRef))
    }

    sealed trait Generic extends Ref {
      import Tag.*
      require(tag in (GenericClassRef, GenericInterfaceRef, GenericRecordRef,
        GenericInstanceMethodRef, GenericStaticMethodRef, GenericGlobalFunctionRef))

      def typeParameters: Seq[TypeParameter]
      val constraints = ArrayBuffer.empty[GenericConstraints]
    }

    sealed trait GenericWithName extends Generic with HasName

    sealed trait Instantiated[T] extends Ref {
      import Tag.*
      require(tag in (InstantiatedClassRef, InstantiatedInterfaceRef, InstantiatedRecordRef,
        InstantiatedInstanceMethodRef, InstantiatedStaticMethodRef, InstantiatedGlobalFunctionRef))

      def generic: Generic
      def instantiatedTypeParameters: Seq[T]
    }

    sealed trait InstantiatedWithName extends Instantiated[TypeOrThisType] with HasName {
      def generic: Generic with HasName
      def name: String = generic.name
    }

    sealed trait Field extends MemberDef

    sealed trait MethodRef extends MemberRef with HasParameters

    sealed trait MethodDef extends MethodRef with MemberDef

    case class ClassDef(superclass: Option[Ref], superinterfaces: Seq[Ref], members: Seq[MemberDef], modifiers: Set[Modifier], isLambdaClass: Boolean = false)
    sealed trait HasClassDef extends HasName with HasAnnotations {
      import Tag.*
      require(tag in (ClassRef, GenericClassRef, MonomorphicClassRef, JavaClassRef))

      // TODO: remove mutable when Lambda is properly represented in HLIR
      val classDef = new DelayedValue[ClassDef](name.contains("L$"))
    }

    case class InterfaceDef(superinterfaces: Seq[Ref], members: Seq[MemberDef], modifiers: Set[Modifier])
    sealed trait HasInterfaceDef extends HasName with HasAnnotations {
      import Tag.*
      require(tag in (InterfaceRef, GenericInterfaceRef, MonomorphicInterfaceRef, JavaInterfaceRef))

      val interfaceDef = new DelayedValue[InterfaceDef]
    }

    case class RecordDef(members: Seq[MemberDef], modifiers: Set[Modifier])
    sealed trait HasRecordDef extends HasName {
      import Tag.*
      require(tag in (RecordRef, GenericRecordRef, MonomorphicRecordRef, TupleRef))

      val recordDef = new DelayedValue[RecordDef]
    }

    case class InterfaceExtensionDef(members: Seq[MemberDef])
    sealed trait HasInterfaceExtensionDef extends Ref {
      import Tag.*
      require(tag in (InterfaceExtensionRef, GenericInterfaceExtensionRef))

      val interfaceExtensionDef = new DelayedValue[InterfaceExtensionDef]

      def baseType: Type
      def interfaces: Seq[Type]
    }

    sealed trait HasModifiers extends Ref {
      val modifiers = new DelayedValue[Set[Modifier]]
    }

    sealed trait HasAnnotations extends Ref {
      import Tag.*
      require(tag in (ClassRef, GenericClassRef, MonomorphicClassRef, InterfaceRef, GenericInterfaceRef, MonomorphicInterfaceRef,
        RecordRef, GenericRecordRef, MonomorphicRecordRef,
        InstanceFieldRef, StaticFieldRef, GlobalVariableRef, InstanceMethodRef, GenericInstanceMethodRef,
        StaticMethodRef, GenericStaticMethodRef, GlobalFunctionRef, GenericGlobalFunctionRef,
        ParameterRef, JavaClassRef, JavaInterfaceRef))

      val annotations = ArrayBuffer.empty[Annotation]
    }

    sealed trait HasParameters extends Ref {
      import Tag.*
      require(tag in (InstanceMethodRef, GenericInstanceMethodRef, InstantiatedInstanceMethodRef,
        StaticMethodRef, GenericStaticMethodRef, InstantiatedStaticMethodRef,
        GlobalFunctionRef, GenericGlobalFunctionRef, InstantiatedGlobalFunctionRef))

      val parameters = ArrayBuffer.empty[Parameter]
    }

    sealed trait HasJavaSignatureAttribute extends Ref {
      import Tag.*
      require(tag in (JavaClassRef, JavaInterfaceRef, InstanceFieldRef, StaticFieldRef, InstanceMethodRef, StaticMethodRef,
                      GenericInstanceMethodRef, GenericStaticMethodRef)) // TODO: remove generics from here

      val javaSignatureAttribute = new DelayedValue[String]
    }

    sealed trait Annotation extends Ref

    sealed trait JavaAnnotationRelated

    class DelayedValue[T <: AnyRef](private var mutable: Boolean = false) {
      private var value: T = _

      def makeMutable: Unit = mutable = true

      def init(value: T): Unit = {
        assert(!initialized || mutable)
        assert(value != null)
        this.value = value
      }

      def initOrElse(value: => T, handler: => Unit): Unit = {
        if (initialized && !mutable) {
          handler
        } else {
          init(value)
        }
      }

      def get: T = {
        assert(initialized)
        value
      }

      def getOption: Option[T] = Option(value)

      def initialized: Boolean = value != null

      override def toString: String =
        if (initialized) value.toString else "<uninitialized>"
    }
  }
}
