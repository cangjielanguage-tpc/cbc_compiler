/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.lambda.impl

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPoolAccessResult.{DEFERRED, OK}
import com.huawei.excelsior.jet.compiler.bytecode.{ConstantPool, ConstantPoolAccessResult}
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.{FINAL, PRIVATE, PUBLIC, SYNTHETIC}
import com.huawei.excelsior.jet.compiler.lambda.{LambdaClassNaming, LambdaTypeGenerator}
import com.huawei.excelsior.jet.compiler.o2lib.opt.OptEnvModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.{SymLevelBuilderModule, pcNamesModule}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{DisableLambdaClassGeneration, IgnoreResolveErrors, LogUnresolvedErrors}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.{classByO2Object, typeToO2Class}
import com.huawei.excelsior.jet.compiler.symlevel.indy.{LambdaInfo, MethodHandle}
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Member, Method, MethodReference, MethodReferenceAccessKind, MethodSignature, MethodType, SignatureType, Type}
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ReferenceType, ClassType as RefClassType, InterfaceType as RefInterfaceType}
import xscala.util.Set32

object LambdaTypeGeneratorImpl extends LambdaTypeGenerator {

  private def env = LightweightEnvironment.getInstance

  def getLambdaClass(hostClass: ClassType, cpInvokeDynamicEntry: Int): Type = {
    // Note: we should not leave invokedynamic in runtime classes to avoid any bootstrap problems.
    if (env.enabled(DisableLambdaClassGeneration) && !hostClass.isJetRuntimeClass) return null

    val lambdaClass = findLambdaClass(hostClass, cpInvokeDynamicEntry)
    if (lambdaClass == null) genLambdaClass(hostClass, cpInvokeDynamicEntry)
    else lambdaClass
  }

  def getLambdaConstructor(hostClass: ClassType, cpInvokeDynamicEntry: Int): MethodReference = {
    // at this point all lambda classes should be already generated: just look for them.
    val lambdaClass = findLambdaClass(hostClass, cpInvokeDynamicEntry)
    if (lambdaClass != null) {
      val cp = hostClass.getClassConstantPool
      val constrType = getMethodType(cp, cpInvokeDynamicEntry).changeReturnType(SignatureType.Void)
      lambdaClass.getMethodRefToLocal(
        XString("<init>"),
        constrType.signature,
        MethodReferenceAccessKind.SPECIAL
      )
    } else null
  }

  /** Generates lambda class by given constant pool InvokeDynamic entry of the host class. */
  private def genLambdaClass(hostClass: ClassType, cpInvokeDynamicEntry: Int): Type = {
    val cp = hostClass.getClassConstantPool
    val bmAccess = cp.getMethodHandle(cp.getBootstrapMethodIndex(cpInvokeDynamicEntry))
    // TODO: override LambdaMetafactory under Scala language pack and omit deferred case
    if (bmAccess.getResult == OK || (hostClass.isXScalaType && bmAccess.getResult == DEFERRED)) {
      val bm = bmAccess.getObject
      val bmArgs = cp.getBootstrapMethodArgs(cpInvokeDynamicEntry)
      if (bmArgs != null && bm.member.getDeclaringClass.getName == "java/lang/invoke/LambdaMetafactory") {
        val methodType = getMethodType(cp, cpInvokeDynamicEntry)
        val closureTypes = methodType.parameterTypes.toSeq
        val samClass = asClassType(methodType.returnType)(env)
        val samMethodName = cp.getRefName(cpInvokeDynamicEntry)

        val lambdaClassName = XString(LambdaClassNaming.getLambdaClassName(hostClass, cpInvokeDynamicEntry))

        (bm.member.getName, bmArgs) match {
          case ("metafactory", Array(samMethodType: MethodType, impl: MethodHandle, instMethodType: MethodType)) =>
            val info = LambdaInfo(hostClass, samClass, samMethodName, samMethodType, impl, instMethodType)
            return createLambdaClass(lambdaClassName, closureTypes, info, false, Seq.empty, Seq.empty)

          case ("altMetafactory", Array(samMethodType: MethodType, impl: MethodHandle, instMethodType: MethodType, flags: Int, args: _*)) =>
            val info = LambdaInfo(hostClass, samClass, samMethodName, samMethodType, impl, instMethodType)

            val serializable = (flags & 0x1) != 0

            var idx = 0
            val markers = if ((flags & 0x2) != 0) {
              // marker interfaces
              val markerCount = args(idx).asInstanceOf[Int]
              idx += 1
              val markerStart = idx
              idx += markerCount
              args.slice(markerStart, idx).map(_.asInstanceOf[ClassType])
            } else {
              Seq.empty
            }

            val bridges = if ((flags & 0x4) != 0) {
              // bridge signatures
              val bridgeCount = args(idx).asInstanceOf[Int]
              idx += 1
              val bridgeStart = idx
              idx += bridgeCount
              args.slice(bridgeStart, idx).map(_.asInstanceOf[MethodType])
            } else {
              Seq.empty
            }

            return createLambdaClass(lambdaClassName, closureTypes, info, serializable, markers, bridges)
        }
      }
    }
    null
  }

  private def createLambdaClass(className: XString, closureTypes: Seq[SignatureType], info: LambdaInfo,
                                serializable: Boolean, markerInterfaces: Seq[ClassType], bridges: Seq[MethodType]): ClassType = {

    locally {
      // Do not create lambda classes with anything deferred (JET-15812, JET-15814, JET-15815).
      // TODO: support it using deferred operations (JET-15822).

      implicit val tp: TypeProvider = env.getTypeProvider

      val implMethod = info.impl.member.asInstanceOf[Method]
      val sigTypes = closureTypes ++ (for {
        mt <- info.samMethodType +: implMethod.getMethodType +: info.instantiatedMethodType +: bridges
        t <- mt.returnType +: mt.parameterTypes.toSeq
      } yield t)

      val symTypes = info.samClass +: implMethod.getDeclaringClass +: markerInterfaces

      val deferredTypes = sigTypes.filter(_.isDeferred) ++ symTypes.filter(_.isDeferred)
      if (deferredTypes.nonEmpty) {
        def msg = s"deferred types ${deferredTypes.mkString(",")} in lambda $className"
        if (env.enabled(LogUnresolvedErrors)) {
          println(s"UNRESOLVED: $msg")
        }
        assert(env.enabled(IgnoreResolveErrors), msg)
        return null
      }
    }

    val hostClass = info.capturingClass

    // Create symlevel class.
    val classNameObject = pcNamesModule.newLambdaClassName(className, OptEnvModule.hostClassloaderID(typeToO2Class(hostClass)))
    val cls = SymLevelBuilderModule.newClass(classNameObject, Modifiers(FINAL, SYNTHETIC), null)
    val rcvType = SignatureType.fromSymType(cls.symType)

    if (hostClass.isXScalaType) {
      cls.setSuperClass(RefClassType(env.getXScalaAnyRef))
      cls.markAsXScalaType()
    } else {
      cls.setSuperClass(RefClassType(env.getObjectType))
    }

    def serializableType = if (hostClass.isXScalaType) env.getXScalaSerializable else env.getSerializableType
    val interfs = (info.samClass +: markerInterfaces) ++ Option.when(serializable)(serializableType)
    cls.setSuperInterfaces(interfs.toArray.map(RefInterfaceType.apply))

    cls.hostClass = typeToO2Class(hostClass)

    cls.markAsLambdaClass()
    cls.addLambdaInfo(info)

    // Create closure fields.
    for ((t, i) <- closureTypes.zipWithIndex) {
      // Note: JDK metafactory numerates fields starting from `arg$1`
      SymLevelBuilderModule.addField(cls, XString(s"arg$$${i + 1}"), t, Set32(Modifiers(FINAL, PRIVATE).value))
    }

    // Create constructor.
    SymLevelBuilderModule.addMethod(cls, XString("<init>"), MethodSignature(closureTypes*)(SignatureType.Void), Set32(Modifiers(PUBLIC).value), Some(rcvType))

    // Create actual implementation method and additional bridge methods.
    val samMethodName = info.samMethodName
    for (sig <- info.samMethodType +: bridges) {
      // Note: we don't set BRIDGE access flag for bridge methods as it doesn't affect anything
      SymLevelBuilderModule.addMethod(cls, samMethodName, sig.signature, Set32(Modifiers(PUBLIC).value), Some(rcvType))
    }
    
    if (!hostClass.isXScalaType) {
      // Support serialization if needed.
      if (serializable) {
        SymLevelBuilderModule.addMethod(cls, XString("writeReplace"), MethodSignature()(SignatureType.javaLangObject(env)), Set32(Modifiers(PRIVATE).value), Some(rcvType))
      } else {
        val serializableType = env.getSerializableType
        if (interfs exists serializableType.isAssignableFrom) {
          // Accidentally serializable (without flag).
          // These methods will throw java/io/NotSerializableException.
          SymLevelBuilderModule.addMethod(cls, XString("writeObject"),
            MethodSignature(SignatureType.JBCReference("java/io/ObjectOutputStream"))(SignatureType.Void),
            Set32(Modifiers(PRIVATE).value),
            Some(rcvType))
          SymLevelBuilderModule.addMethod(cls, XString("readObject"),
            MethodSignature(SignatureType.JBCReference("java/io/ObjectInputStream"))(SignatureType.Void),
            Set32(Modifiers(PRIVATE).value),
            Some(rcvType))
        }
      }
    }

    // Finish class building.
    SymLevelBuilderModule.preprocessClass(cls)
    SymLevelBuilderModule.processClass(cls)

    classByO2Object(cls)
  }

  /** Finds already parsed lambda-class in the sym-level.
    * It is assumed that [[createLambdaClass]] was called before.
    */
  private def findLambdaClass(hostClass: ClassType, cpInvokeDynamicEntry: Int): ClassType = {
    val className = LambdaClassNaming.getLambdaClassName(hostClass, cpInvokeDynamicEntry)
    val lambdaClass = OptEnvModule.findLambdaClass(typeToO2Class(hostClass), XString.ascii(className))
    if (lambdaClass == null) null else classByO2Object(lambdaClass)
  }

  private def getMethodType(cp: ConstantPool, index: Int): MethodType =
    MethodType.forJava(cp.getRefSignature(index), env, cp.getHost)
}
