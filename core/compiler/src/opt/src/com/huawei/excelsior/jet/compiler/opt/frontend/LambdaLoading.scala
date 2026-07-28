/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.CLASS
import com.huawei.excelsior.jet.compiler.symlevel.indy.{LambdaInfo, ReferenceKind}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.{Domain, PreparationRequired, symlevel}

trait LambdaLoading { this: Universe =>

  // Helper function to avoid passing method argument everywhere in this trait.
  private def loadedMethod = currentInlineContext.method

  def loadLambda(method: Method, args: Seq[Node]): Return = {
    assert(method == loadedMethod)
    if (method.isConstructor) {
      loadLambdaConstructor(method, args)
    } else {
      val lambdaInfo = method.getDeclaringClass.getLambdaInfo
      if (method.getXName == lambdaInfo.samMethodName) {
        loadLambdaMethod(method, lambdaInfo, args)
      } else {
        method.getName match {
          case "writeReplace" => loadLambdaWriteReplace(method, lambdaInfo, args)
          case "writeObject" | "readObject" => loadLambdaNotSerializableMethod(method, args)
        }
      }
    }
  }

  private def loadLambdaConstructor(method: Method, args: Seq[Node]): Return = {
    assert(method.isConstructor)
    val lambdaClass = method.getDeclaringClass

    currentScope.inState(entryBlock, entryBlock) {
      Node.withImplicitArgConversion(enrichArg()) {
        assert(lambdaClass.getDeclaredFields.size + 1 == args.size)
        val thisArg = args.head
        for ((f, v) <- lambdaClass.getDeclaredFields.zip(args.tail)) {
          PutField(f)(thisArg, v)
        }
        Return.proto(VoidType)(Void())
      }
    }
  }

  private def loadLambdaMethod(method: Method, lambdaInfo: LambdaInfo, args: Seq[Node]): Return = {
    val lambdaClass = method.getDeclaringClass

    val target = lambdaInfo.impl.member.asInstanceOf[Method]
    val refKind = lambdaInfo.impl.refKind
    val mak = refKind.asMethodAccessKind.asMethodRefAccessKind
    val targetRef = new MethodReference(target, mak)

    currentScope.inState(entryBlock, entryBlock) {
      Node.withImplicitArgConversion(enrichArg()) {
        val thisArg = args.head

        val capturedArgs = lambdaClass.getDeclaredFields.map(getField(_, thisArg)).toSeq
        
        val newObj = if (refKind == ReferenceKind.REF_newInvokeSpecial) {
          val targetClass = target.getDeclaringClass
          Clinit(targetClass)()
          ensurePrepared(PreparationRequired.forType(targetClass))
          val obj = New(SignatureType.fromSymType(targetClass))()
          Some(obj)
        } else {
          None
        }
        
        val freeArgs = newObj ++ capturedArgs

        val convertedArgs = {
          val fromMethodType = method.getMethodType.dropReceiverParameter
          val toMethodType = target.getMethodType.dropFirstNParameters(freeArgs.size)
          val instMethodType = lambdaInfo.instantiatedMethodType.dropReceiverParameter

          val paramCount = args.size - 1
          assert(fromMethodType.parameterCount == paramCount &&
            toMethodType.parameterCount == paramCount &&
            instMethodType.parameterCount == paramCount)

          args.tail.zipWithIndex map { (arg, idx) =>
            convert(
              arg,
              fromMethodType.parameterType(idx),
              toMethodType.parameterType(idx),
              instMethodType.parameterType(idx),
            )
          }
        }

        val targetArgs = freeArgs.toSeq ++ convertedArgs
        def receiver = targetArgs(method.getReceiverArgIdx)

        ensurePrepared(PreparationRequired.forInvoke(targetRef))
        if (!target.isStatic) {
          nullCheck(receiver)
        }

        val call = if (targetRef.isInterfCall) {
          val ciao = WeakCast(targetRef.refClass)(receiver, WeakCast.NoCheck())
          InvokeInterface(targetRef, ciao)(targetArgs: _*)
        } else {
          Invoke(targetRef)(targetArgs: _*)
        }

        val methodReturnType = method.getReturnType

        val retVal = if (methodReturnType.isZST) {
          Void()
        } else {
          val (rawRetVal, fromType) = newObj match {
            case Some(obj) => (obj, SignatureType.fromSymType(target.getDeclaringClass))
            case None => (depriveIfNeeded(call), target.getReturnType)
          }
          convert(
            rawRetVal,
            fromType,
            methodReturnType,
            lambdaInfo.instantiatedMethodType.returnType
          )
        }

        Return.proto(ValueType.fromSig(methodReturnType, instantiateRich = true))(retVal)
      }
    }
  }

  private def loadLambdaWriteReplace(method: Method, lambdaInfo: LambdaInfo, args: Seq[Node]): Return = {
    assert(args.size == 1)
    val lambdaClass = method.getDeclaringClass

    val implMethod = lambdaInfo.impl.member.asInstanceOf[Method]
    val implRefKind = lambdaInfo.impl.refKind

    val serializedLambdaType = typeProvider.resolveTypeByName(lambdaClass, xstr("java/lang/invoke/SerializedLambda"))
    val serializedLambdaConstr = serializedLambdaType.findDeclaredMethod(xstr("<init>"), null)
    val serializedLambdaConstrRef = new MethodReference(serializedLambdaConstr, MethodReferenceAccessKind.SPECIAL)

    currentScope.inState(entryBlock, entryBlock) {
      Node.withImplicitArgConversion(enrichArg()) {
        val thisArg = args.head

        Clinit(serializedLambdaType)()
        ensurePrepared(PreparationRequired.forType(serializedLambdaType))
        val serializedLambda = New(SignatureType.fromSymType(serializedLambdaType))()

        val capturedFields = lambdaClass.getDeclaredFields.toSeq
        val capturedArgsArrayType = SignatureType.fromSymType(typeProvider.get1DimArrayType(CLASS))
        val capturedArgsArray = NewArray(capturedArgsArrayType)(IConst(capturedFields.size))
        for ((f, i) <- capturedFields.zipWithIndex) {
          val capturedArg = getField(f, thisArg)
          val capturedBoxedArg = convert(
            capturedArg,
            f.getType,
            SignatureType.javaLangObject,
            SignatureType.javaLangObject
          )
          ArrayPut(capturedArgsArrayType)(capturedArgsArray, IConst(i), capturedBoxedArg)
        }

        ensurePrepared(PreparationRequired.forInvoke(serializedLambdaConstrRef))

        Invoke(serializedLambdaConstrRef)(
          serializedLambda,                                                              // this
          ClassObject(lambdaInfo.capturingClass)(),                                      // Class<?> capturingClass
          strConst(lambdaInfo.samClass.getName),                                         // String functionalInterfaceClass
          strConst(lambdaInfo.samMethodName),                                            // String functionalInterfaceMethodName
          strConst(JBCSignature(lambdaInfo.samMethodType.toMethodDescriptor)),           // String functionalInterfaceMethodSignature
          IConst(implRefKind.ordinal),                                                   // int implMethodKind
          strConst(implMethod.getDeclaringClass.getName),                                // String implClass
          strConst(implMethod.getName),                                                  // String implMethodName
          strConst(JBCSignature(implMethod.getMethodType.toMethodDescriptor)),           // String implMethodSignature
          strConst(JBCSignature(lambdaInfo.instantiatedMethodType.toMethodDescriptor)),  // String instantiatedMethodType
          capturedArgsArray,                                                             // Object[] capturedArgs
        )

        Return.proto(ValueType.fromSig(method.getReturnType, instantiateRich = true))(serializedLambda)
      }
    }
  }

  private def loadLambdaNotSerializableMethod(method: Method, args: Seq[Node]): Return = {
    assert(args.size == 2)
    val lambdaClass = method.getDeclaringClass

    val notSerializableException = typeProvider.resolveTypeByName(lambdaClass, xstr("java/io/NotSerializableException"))
    val notSerializableExceptionConstr = notSerializableException.findDeclaredMethod(xstr("<init>"), MethodSignature(SignatureType.javaLangString)(SignatureType.Void))
    val notSerializableExceptionConstrRef = new MethodReference(notSerializableExceptionConstr, MethodReferenceAccessKind.SPECIAL)

    currentScope.inState(entryBlock, entryBlock) {
      Node.withImplicitArgConversion(enrichArg()) {
        Clinit(notSerializableException)()
        ensurePrepared(PreparationRequired.forType(notSerializableException))
        val exception = New(SignatureType.fromSymType(notSerializableException))()

        ensurePrepared(PreparationRequired.forInvoke(notSerializableExceptionConstrRef))

        val msg = strConst("Non-serializable lambda")
        Invoke(notSerializableExceptionConstrRef)(exception, msg)
        Throw(exception)
        Halt.afterThrow("non-serializable lambda")()
        null
      }
    }
  }

  /** Converts given `arg` from `from`-type to `to`-type ensuring that it is also compatible with `inst`-type.
    *
    * Conversion process involves
    *   - sign- and zero-extension of primitives
    *   - boxing and unboxing of primitives
    *   - casts via [[CheckCast]] of references if necessary
    */
  private def convert(arg: Node, from: SignatureType, to: SignatureType, inst: SignatureType): Node = {
    import SignatureType.Primitive
    (from, to, inst) match {
      case _ if from.isZST || to.isZST =>
        // If either is Void, then return Void (relevant for return type conversion).
        Void()

      case _ if from == to && to == inst =>
        // Same type, no conversion required.
        arg

      case (from: Primitive, to: Primitive, _) =>
        // Primitive conversion, instantiated type is irrelevant.
        JavaConvert(from.jbcKind, to.jbcKind)(arg)

      case (from: Primitive, to, _) =>
        // From type is primitive, but to type can be
        to.symType match {
          case BoxType(box) =>
            // Box type, then first do primitive conversion and then box the value.
            val convertedArg = JavaConvert(from.jbcKind, box.kind)(arg)
            BoxedValue(box)(arg)

          case t =>
            // Non-box type (e.g. java/lang/Number), then simply box the value and cast it.
            // TODO: replace domain with hierarchy entity
            val domain = if (t.isXScalaType) Domain.SCALA else Domain.JAVA
            val boxedArg = BoxedValue(from.jbcKind, domain)(arg)
            CheckCast(to)(boxedArg)
            boxedArg
        }

      case (from, to: Primitive, inst) =>
        // From type is reference and to type is primitive.
        if (!inst.isPrimitive) {
          // Ensure compatibility with instantiated type.
          CheckCast(inst)(arg)
        }
        // From type can be
        from.symType match {
          case BoxType(box) =>
            // Box type, then unbox it and convert to required type.
            nullCheck(arg)
            val unboxedArg = getField(box.value, arg)
            JavaConvert(box.kind, to.jbcKind)(unboxedArg)

          case t =>
            // Non-box type, then first cast it to appropriate box type and then unbox the value.
            // Note: here we assume that non-boxing subclasses of `Number` can't be passed in place of a primitive argument
            val box = if (t.isXScalaType) {
              XScala.Support.BoxType(to.jbcKind)
            } else {
              Java.Support.BoxType(to.jbcKind)
            }

            CheckCast(SignatureType.fromSymType(box.symType))(arg)
            nullCheck(arg)
            getField(box.value, arg)
        }

      case (_, to, inst) =>
        // Both from and to types are reference types, so simply cast one to the other.
        if (!inst.isPrimitive) {
          // Ensure compatibility with instantiated type.
          CheckCast(inst)(arg)
        }
        CheckCast(to)(arg)
        arg
    }
  }

  private def strConst(str: String): Node = strConst(xstr(str))
  private def strConst(str: XString): Node =
    ConstString(loadedMethod.getDeclaringClass.getConstString(str), typeProvider.getStringType)()
  
  private def getField(f: Field, obj: Node): Node = {
    depriveIfNeeded(GetField(f)(obj))
  }

  private def nullCheck(arg: Node): Unit = {
    NullCheck(trusted = loadedMethod.noNullCheck(env))(arg)
  }

  private object BoxType {
    def unapply(t: symlevel.Type) = {
      Java.Support.BoxType.unapply(t) orElse 
        XScala.Support.BoxType.unapply(t)
    }
  }

}
