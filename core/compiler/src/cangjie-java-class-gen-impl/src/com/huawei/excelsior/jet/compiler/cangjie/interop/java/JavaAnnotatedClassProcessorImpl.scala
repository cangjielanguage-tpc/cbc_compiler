/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cangjie.interop.java

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.cangjie.interop.java.JavaSymbols.*
import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata
import com.huawei.excelsior.jet.compiler.hlir.interop.java.HLIRJavaSymbols
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.{BRIDGE, STATIC, SYNTHETIC}
import com.huawei.excelsior.jet.compiler.symlevel.{ConstValues, JBCSignature, SignatureType, TypeKind}
import com.huawei.excelsior.jet.util.Worklist
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.commons.{GeneratorAdapter, Method as AsmMethod}
import org.objectweb.asm.{AnnotationVisitor, ClassWriter, Handle, Type as AsmType}
import xscala.io.{DataOutput, Files, Path}

import java.lang.Double.longBitsToDouble
import java.lang.Float.intBitsToFloat
import java.lang.invoke.{MethodHandles, MethodType}
import scala.collection.mutable
import scala.util.Using

/** Processes Cangjie classes annotated with `@java` macro.
  *
  * Splits them to two parts: Java class with all data and methods delegating through native intrinsics to
  * Cangjie code, and Cangjie helper class with actual code of the methods.
  *
  * The Java classes are generated on the fly, while the original Cangjie class is changed into "helper" class during
  * parsing in [[CangjieSymLevelMaker]].
  */
class JavaAnnotatedClassProcessorImpl(symbols: JavaSymbols, javaClass: Class, javaHelperClassName: String, sourceFile: String,
                                      delegateConstructor: Method => Option[Method]) {
  import com.huawei.excelsior.jet.compiler.cangjie.interop.java.JavaAnnotatedClassProcessorImpl.*

  // TODO: keep only HLIR implementation
  private val hlir: HLIRJavaSymbols = symbols match {
    case hlir: HLIRJavaSymbols => hlir
    case _ => null
  }

  def process(): Unit = {
    val className = javaClass.name
    val superClass = javaClass.superClass

    val superClassName = if (superClass == null) "java/lang/Object" else superClass.name

    val interfaces = (javaClass.declaredSuperInterfaces map (_.name)).toArray

    import org.objectweb.asm.ClassWriter.*

    val classWriter = new ClassWriter(COMPUTE_MAXS | COMPUTE_FRAMES)

    val signature = javaClass.signature.orNull

    classWriter.visit(V1_8, javaClass.accessFlags, className, signature, superClassName, interfaces)
    classWriter.visitSource(sourceFile, null)

    processAnnotations(javaClass) { (annotationClass, runtimeVisible) =>
      classWriter.visitAnnotation(annotationClass, runtimeVisible)
    }

    for (f <- javaClass.declaredFields) {
      processJavaField(classWriter, f)
    }

    for (m <- javaClass.declaredMethods) {
      processJavaMethod(classWriter, m)
    }

    classWriter.visitEnd()

    // [TODO JAVA_INTEROP] create an option FFIOutputDir to overwrite dir for writing Java classes
    // [TODO JAVA_INTEROP] is this '.' replacement needed?
    val path = Path(className.replace('.', '/') + ".class")
    Files.makeDir(path.parent)
    Using.resource(DataOutput.from(path)) { out =>
      out.putBytes(classWriter.toByteArray)
    }
  }

  private def processAnnotations(container: HasAnnotations)(write: (String, Boolean) => AnnotationVisitor): Unit = {
    import HLIRMetadata.Ref
    import Ref.*

    def resolveRef(ref: Ref): JavaSymbols.Type = {
      require(hlir != null, "Annotations are supported by HLIR parser only")
      hlir.classByRef(ref)
    }

    def writeValue(writer: AnnotationVisitor, name: String, ref: Ref): Unit = {
      ref match {
        case JavaAnnotationNumericConstant(tpe, value) =>
          val boxedValue: Object = (tpe.asSignatureType: @unchecked) match {
            case SignatureType.Primitive(TypeKind.BOOLEAN) => Boolean.box(value != 0)
            case SignatureType.Primitive(TypeKind.BYTE) => Byte.box(value.toByte)
            case SignatureType.Primitive(TypeKind.SHORT) => Short.box(value.toShort)
            case SignatureType.Primitive(TypeKind.CHAR) => Char.box(value.toChar)
            case SignatureType.Primitive(TypeKind.INT) => Int.box(value.toInt)
            case SignatureType.Primitive(TypeKind.LONG) => Long.box(value)
            case SignatureType.Primitive(TypeKind.FLOAT) => Float.box(intBitsToFloat(value.toInt))
            case SignatureType.Primitive(TypeKind.DOUBLE) => Double.box(longBitsToDouble(value))
          }
          writer.visit(name, boxedValue)
        case JavaAnnotationString(str) => writer.visit(name, str)
        case JavaAnnotationEnumValue(f @ StaticField(refType, constant, sig)) =>
          val descriptor = resolveRef(refType).descriptor
          require(JBCSignature(hlir.typeSignature(f)) == descriptor, s"$sig != $descriptor for $name -> $ref")
          writer.visitEnum(name, descriptor, constant)
        case JavaAnnotationArrayValue(elements) =>
          val nested = writer.visitArray(name)
          for (value <- elements) {
            writeValue(nested, null, value)
          }
          nested.visitEnd()
        case JavaAnnotation(tpe, elements) =>
          val nested = writer.visitAnnotation(name, resolveRef(tpe).descriptor)
          writeAnnotations(nested, elements)
          nested.visitEnd()
        case tpe: Type => writer.visit(name, AsmType.getType(resolveRef(tpe).descriptor))
        case _ => shouldNotReachHere(s"$name -> $ref is unexpected")
      }
    }

    def writeAnnotations(writer: AnnotationVisitor, elements: Seq[JavaAnnotationElement]): Unit = {
      require(hlir != null)
      for (case JavaAnnotationElement(name, value) <- elements) {
        writeValue(writer, name, value)
      }
    }

    for ((runtimeVisible, annotation) <- container.javaAnnotations) {
      val annotationClass = resolveRef(annotation.tpe).asInstanceOf[JavaSymbols.Class]
      val writer = write(annotationClass.descriptor, runtimeVisible)
      writeAnnotations(writer, annotation.elements)
      writer.visitEnd()
    }
  }

  private def processMethodAnnotations(method: Method, mg: GeneratorAdapter): Unit = {
    processAnnotations(method) { (annotationClass, runtimeVisible) =>
      mg.visitAnnotation(annotationClass, runtimeVisible)
    }

    final class AnnotatedParameter(p: HLIRMetadata.Ref.Parameter) extends HasAnnotations {
      override protected def annotations: IndexedSeq[HLIRMetadata.Ref.Annotation] = p.annotations.toIndexedSeq
    }

    if (hlir != null) {
      method match {
        case m: hlir.MethodImpl =>
          for (p <- m.parameters if p.annotations.nonEmpty) {
            processAnnotations(new AnnotatedParameter(p)) { (annotationClass, runtimeVisible) =>
              mg.visitParameterAnnotation(p.index, annotationClass, runtimeVisible)
            }
          }
      }
    }
  }

  private def processJavaField(classWriter: ClassWriter, f: Field): Unit = {
    val descriptor = f.tpe.descriptor
    val signature = f.signature.orNull

    val fieldVisitor = classWriter.visitField(
      (f.javaModifiers & Modifiers.JBC.publicFieldMask).value, f.name, descriptor, signature,
      if (f.isStatic) (f.constValue map getConstantValue).orNull else null
    )
    processAnnotations(f) { (annotationClass, runtimeVisible) =>
      fieldVisitor.visitAnnotation(annotationClass, runtimeVisible)
    }
    fieldVisitor.visitEnd()
  }

  private case class MethodDescription(needsReceiver: Boolean, method: AsmMethod)

  private def processJavaMethod(classWriter: ClassWriter, m: Method): Unit = {
    val name = getJavaMethodName(m)
    val methodType = m.methodType
    val descriptor = methodType.descriptor
    if (name == JAVA_CONSTRUCTOR_NAME) {

      val superConstr = delegateConstructor(m).orNull
      val preInitBridgeMethod = if (superConstr == null || AsmType.getArgumentTypes(superConstr.methodType.descriptor).isEmpty) {
        // no need to call preInit, ignore it
        null
      } else {
        val objectArrayType = symbols.arrayType(symbols.objectType)
        val preInitSig = getBridgeMethodType(methodType, javaClass, addThis = false).copy(returnType = objectArrayType).descriptor
        MethodDescription(needsReceiver = false, new AsmMethod("$preInit", preInitSig))
      }

      // currently we always generate postInit, although sometimes it may be skipped (e.g. for empty constructors)
      val postInitSig = getBridgeMethodType(methodType, javaClass, addThis = true).descriptor
      val postInitBridgeMethod = MethodDescription(needsReceiver = true, new AsmMethod("$postInit", postInitSig))

      genConstructorWithDelegation(classWriter, m, superConstr, preInitBridgeMethod, postInitBridgeMethod)

    } else {
      val javaModifiers = m.javaModifiers & Modifiers.JBC.publicMethodMask

      val javaMethod = if (m.isAbstract) {
        assert(!m.isStatic)
        genAbstractMethod(classWriter, m, name, descriptor, javaModifiers)
      } else {
        val cangjieBridgeMethod = getBridgeToCangjieMethod(m, javaClass)
        genDelegateToCangjieBridge(classWriter, m, name, descriptor, javaModifiers, cangjieBridgeMethod)
      }

      // TODO: remove when bridges for overrides will be generated by FE
      if (!m.isStatic) {
        val overriddenRetTypes = findOverriddenRetTypes(javaClass, name, methodType)
        for (retType <- overriddenRetTypes) {
          assert(AsmType.getType(retType.descriptor).getSort == AsmType.OBJECT) // no overrides for primitive types
          val overriddenDescriptor = methodType.copy(returnType = retType)

          genBridgeMethod(classWriter, javaClass.name, m, name, overriddenDescriptor.descriptor, javaModifiers, javaMethod)
        }
      }
    }
  }

  /** Finds all methods overridden by the given method that have different (less exact) return type, extracts those
    * return types and returns them as set.
    *
    * For each such overridden return type, a separate bridge method needs to be generated.
    */
  private def findOverriddenRetTypes(klass: Class, methodName: String, methodType: JavaSymbols.MethodType): mutable.Set[Type] = {
    val retTypes = mutable.LinkedHashSet.empty[Type]

    def isAccessibleInSubclasses(m: Method) = {
      if (m.isPrivate) {
        false
      } else if (m.isPublic || m.isProtected) {
        true
      } else {
        m.declaringClass.packageName == klass.packageName
      }
    }

    def findOverriddenRetTypesIn(c: Class): Boolean = {
      for (m <- c.declaredMethods) {
        // NOTE: we cannot check generic substitution, so check only for full equality of argument types
        //       Full support of bridges shall be done in FE
        if (!m.isStatic && m.name == methodName && m.methodType.paramTypes == methodType.paramTypes) {
          if (!isAccessibleInSubclasses(m)) {
            // considered as another method, skip further checks in supers
            return false
          }
          if (m.methodType != methodType) {
            retTypes.add(m.methodType.returnType)
            return true
          }
        }
      }
      true
    }

    val worklist = Worklist.empty[Class]
    worklist += klass
    for (c <- worklist.drain; s <- Option(c.superClass) ++ c.declaredSuperInterfaces) {
      val processSuper = findOverriddenRetTypesIn(s)
      if (processSuper) worklist += s
    }

    retTypes
  }

  def method(name: String, descriptor: String, signature: String, access: Modifiers, classWriter: ClassWriter): (AsmMethod, GeneratorAdapter) = {
    val m = new AsmMethod(name, descriptor)
    val mg = new GeneratorAdapter(access.value, m, signature, null, classWriter)
    (m, mg)
  }

  private def invokeDynamic(mg: GeneratorAdapter, m: AsmMethod): Unit = {
    val isInterface = javaClass.isInterface
    val methodType = MethodType.methodType(
      classOf[Object], // return value: CallSite
      classOf[MethodHandles.Lookup], // pushed by JVM
      classOf[String], // name, pushed by JVM
      classOf[MethodType], // expected CallSite signature, pushed by JVM
      classOf[String] // JavaHelper class name as additional bootstrap method parameter
    )
    val handle = new Handle(H_INVOKESTATIC, "com/huawei/excelsior/jet/runtime/jit/CbcIndyFactory", "link", methodType.toMethodDescriptorString, isInterface)
    mg.invokeDynamic(m.getName, m.getDescriptor, handle, javaHelperClassName)
  }

  private def genConstructorWithDelegation(classWriter: ClassWriter, m: Method,
                                           superConstr: Method,
                                           preInitBridgeMethod: MethodDescription,
                                           postInitBridgeMethod: MethodDescription): Unit = {
    val (_, mg) = method(JAVA_CONSTRUCTOR_NAME, m.methodType.descriptor, m.signature.orNull,
      m.javaModifiers & Modifiers.JBC.constructorMask, classWriter)
    processMethodAnnotations(m, mg)

    if (superConstr != null) { // if null, error is already reported
      // put 'this' to stack as receiver of further call of super-constructor
      mg.loadThis()

      val superArgTypes = AsmType.getArgumentTypes(superConstr.methodType.descriptor)
      if (superArgTypes.nonEmpty) {
        // call $preInit to convert parameters to arguments of a delegate constructor
        mg.loadArgs()
        invokeDynamic(mg, preInitBridgeMethod.method)

        // now we have Object[] with super-constructor arguments on stack. Get and unbox them if necessary.
        // Receiver (this) is already on stack
        for (i <- 0 until superArgTypes.length) {
          if (i < superArgTypes.length - 1) {
            // save array for further array loads
            mg.dup()
          }
          mg.push(i)
          mg.arrayLoad(AsmType.getType(classOf[Object]))
          val superArgType = superArgTypes(i)
          mg.unbox(superArgType) // unbox or cast if necessary
          if (i < superArgTypes.length - 1) {
            // swap arr (X) & arg (V or VV) on stack
            if (superArgType.getSize == 1) {
              mg.swap() // XV -> VX
            } else {
              assert(superArgType.getSize == 2)
              // XVV -> VVXVV -> VVX
              mg.dup2X1()
              mg.pop2()
            }
          }
        }
      }

      // now both receiver and prepared arguments are on stack, so call super(..)/this(..)
      mg.invokeConstructor(AsmType.getObjectType(superConstr.declaringClass.name),
        new AsmMethod(JAVA_CONSTRUCTOR_NAME, superConstr.methodType.descriptor))
    }

    // now "this" is initialized, so we can call the postInit bridge method
    mg.loadThis()
    mg.loadArgs()
    invokeDynamic(mg, postInitBridgeMethod.method)
    mg.returnValue()

    // all instructions are ready, generate the method
    mg.endMethod()
  }

  private def genDelegateToCangjieBridge(classWriter: ClassWriter,
                                         sym: Method, name: String, descriptor: String, modifiers: Modifiers,
                                         bridgeMethod: MethodDescription): AsmMethod = {
    val (m, mg) = method(name, descriptor, sym.signature.orNull, modifiers, classWriter)
    processMethodAnnotations(sym, mg)

    // receiver is always first since it is Java
    if (bridgeMethod.needsReceiver) {
      // put 'this' on stack as receiver for instance bridge method call
      mg.loadThis()
    }

    mg.loadArgs()
    invokeDynamic(mg, bridgeMethod.method)
    mg.returnValue()

    mg.endMethod()

    m
  }

  private def genBridgeMethod(classWriter: ClassWriter, className: String,
                              sym: Method, name: String, descriptor: String, modifiers: Modifiers,
                              targetMethod: AsmMethod): Unit = {
    assert(!(modifiers contains STATIC))
    val ownerType = AsmType.getObjectType(className)
    val (_, mg) = method(name, descriptor, sym.signature.orNull, modifiers + BRIDGE + SYNTHETIC, classWriter)
    processMethodAnnotations(sym, mg)

    mg.loadThis()
    mg.loadArgs()
    if (javaClass.isInterface) {
      mg.invokeInterface(ownerType, targetMethod)
    } else {
      mg.invokeVirtual(ownerType, targetMethod)
    }
    mg.returnValue()
    mg.endMethod()
  }

  private def getBridgeToCangjieMethod(origCJMethod: Method, proxyClass: Class): MethodDescription = {
    val name = if (origCJMethod.name == CangjieSymLevelMaker.JAVA_CLINIT_NAME) {
      CangjieSymLevelMaker.JAVA_HELPER_CLINIT_NAME
    } else {
      getJavaMethodName(origCJMethod)
    }
    val hasReceiver = !origCJMethod.isStatic
    val msig = getBridgeMethodType(origCJMethod.methodType, proxyClass, hasReceiver)

    MethodDescription(hasReceiver, new AsmMethod(name, msig.descriptor))
  }

  private def getBridgeMethodType(original: JavaSymbols.MethodType, proxyClass: Class, addThis: Boolean) = {
    if (addThis) {
      // add $this as the first parameter
      original.copy(paramTypes = proxyClass +: original.paramTypes)
    } else {
      original
    }
  }

  private def genAbstractMethod(classWriter: ClassWriter, sym: Method, name: String, sig: String, modifiers: Modifiers): AsmMethod = {
    val (m, mg) = method(name, sig, sym.signature.orNull, modifiers, classWriter)
    processMethodAnnotations(sym, mg)
    mg.endMethod()
    m
  }
}

object JavaAnnotatedClassProcessorImpl extends JavaAnnotatedClassProcessor {

  private val JAVA_CONSTRUCTOR_NAME = "<init>"

  override def process(symbols: JavaSymbols, javaClass: Class, javaHelperName: String, sourceFile: String,
                       delegateConstructor: Method => Option[Method]): Unit = {
    new JavaAnnotatedClassProcessorImpl(symbols, javaClass, javaHelperName, sourceFile, delegateConstructor).process()
  }

  private def getJavaMethodName(origCJMethod: Method) = {
    origCJMethod.name
  }

  private def getConstantValue(v: ConstValues.ConstValue): Object = v match {
    case v: ConstValues.IntValue => v.value.asInstanceOf[Object]
    case v: ConstValues.LongValue => v.value.asInstanceOf[Object]
    case v: ConstValues.FloatValue => v.value.asInstanceOf[Object]
    case v: ConstValues.DoubleValue => v.value.asInstanceOf[Object]
    case v: ConstValues.StringValue => v.value.toString
  }
}
