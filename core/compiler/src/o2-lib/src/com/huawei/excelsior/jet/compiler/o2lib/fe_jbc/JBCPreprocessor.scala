/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe_jbc

import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcJCAModule.findJreOverride
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JavaClassParserModule.{AccPublic, AccStatic}
import com.huawei.excelsior.jet.compiler.o2lib.u.ClassID.{XScalaAnyRef, XScalaArrays, XScalaBoxBoolean, XScalaBoxChar, XScalaBoxNumber}
import com.huawei.excelsior.jet.compiler.o2lib.u.{CacheAPIModule, ClassID}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{AllowMappingOfJDKIOToXScala, BuildXKRN}
import xscala.matching.Regex
import xscala.util.Set32
import xscala.util.StringOps.r

object JBCPreprocessor {

  private val scalaClassNameReplacements = Map.from(Seq(
    ClassID.Object.name                 -> ClassID.XScalaAnyRef.name,
    ClassID.String.name                 -> ClassID.XScalaString.name,
    ClassID.Class.name                  -> ClassID.XScalaClass.name,
    ClassID.Field.name                  -> ClassID.XScalaField.name,
    ClassID.JavaReference.name          -> ClassID.XScalaReference.name,
    ClassID.JavaSoftReference.name      -> ClassID.XScalaSoftReference.name,
    ClassID.JavaWeakReference.name      -> ClassID.XScalaWeakReference.name,
    ClassID.JavaMath.name               -> ClassID.XScalaMath.name,
    ClassID.JavaStringBuilder.name      -> ClassID.XScalaStringBuilder.name,
    ClassID.JavaStringBuffer.name       -> ClassID.XScalaStringBuffer.name,
    ClassID.JavaCharSequence.name       -> ClassID.XScalaCharSequence.name,
    ClassID.JavaCharset.name            -> ClassID.XScalaCharset.name,
    ClassID.JavaThread.name             -> ClassID.XScalaThread.name,
    ClassID.System.name                 -> ClassID.XScalaSystem.name,
    ClassID.JavaAutoCloseable.name      -> ClassID.XScalaAutoCloseable.name,
    ClassID.JavaCloseable.name          -> ClassID.XScalaCloseable.name,
    ClassID.Serializable.name           -> ClassID.XScalaSerializable.name,
    ClassID.Cloneable.name              -> ClassID.XScalaCloneable.name,
    ClassID.JavaArrays.name             -> ClassID.XScalaArrays.name,
    ClassID.JavaReflectArray.name       -> ClassID.XScalaReflectArray.name,
    ClassID.JavaRunnable.name           -> ClassID.XScalaRunnable.name,

    ClassID.JavaBoxBoolean.name         -> ClassID.XScalaBoxBoolean.name,
    ClassID.JavaBoxByte.name            -> ClassID.XScalaBoxByte.name,
    ClassID.JavaBoxShort.name           -> ClassID.XScalaBoxShort.name,
    ClassID.JavaBoxChar.name            -> ClassID.XScalaBoxChar.name,
    ClassID.JavaBoxInteger.name         -> ClassID.XScalaBoxInteger.name,
    ClassID.JavaBoxLong.name            -> ClassID.XScalaBoxLong.name,
    ClassID.JavaBoxFloat.name           -> ClassID.XScalaBoxFloat.name,
    ClassID.JavaBoxDouble.name          -> ClassID.XScalaBoxDouble.name,
    ClassID.JavaBoxNumber.name          -> ClassID.XScalaBoxNumber.name,
    ClassID.JavaBoxVoid.name            -> ClassID.XScalaBoxVoid.name,

    ClassID.JavaThrowable.name                        -> ClassID.XScalaThrowable.name,
    ClassID.JavaException.name                        -> ClassID.XScalaException.name,
    ClassID.JavaError.name                            -> ClassID.XScalaError.name,
    ClassID.JavaRuntimeException.name                 -> ClassID.XScalaRuntimeException.name,
    ClassID.JavaIOException.name                      -> ClassID.XScalaIOException.name,
    ClassID.JavaInterruptedException.name             -> ClassID.XScalaInterruptedException.name,
    ClassID.JavaGeneralSecurityException.name         -> ClassID.XScalaGeneralSecurityException.name,
    ClassID.JavaArrayIndexOutOfBoundsException.name   -> ClassID.XScalaArrayIndexOutOfBoundsException.name,
    ClassID.JavaArrayStoreException.name              -> ClassID.XScalaArrayStoreException.name,
    ClassID.JavaNullPointerException.name             -> ClassID.XScalaNullPointerException.name,
    ClassID.JavaArithmeticException.name              -> ClassID.XScalaArithmeticException.name,
    ClassID.JavaClassCastException.name               -> ClassID.XScalaClassCastException.name,
    ClassID.JavaNegativeArraySizeException.name       -> ClassID.XScalaNegativeArraySizeException.name,
    ClassID.JavaNoSuchMethodException.name            -> ClassID.XScalaNoSuchMethodException.name,
    ClassID.JavaIllegalArgumentException.name         -> ClassID.XScalaIllegalArgumentException.name,
    ClassID.JavaIllegalStateException.name            -> ClassID.XScalaIllegalStateException.name,
    ClassID.JavaIndexOutOfBoundsException.name        -> ClassID.XScalaIndexOutOfBoundsException.name,
    ClassID.JavaSecurityException.name                -> ClassID.XScalaSecurityException.name,
    ClassID.JavaUnsupportedOperationException.name    -> ClassID.XScalaUnsupportedOperationException.name,
    ClassID.JavaConcurrentModificationException.name  -> ClassID.XScalaConcurrentModificationException.name,
    ClassID.JavaNoSuchElementException.name           -> ClassID.XScalaNoSuchElementException.name,
    ClassID.JavaLinkageError.name                     -> ClassID.XScalaLinkageError.name,
    ClassID.JavaVirtualMachineError.name              -> ClassID.XScalaVirtualMachineError.name,
    ClassID.JavaAssertionError.name                   -> ClassID.XScalaAssertionError.name,
    ClassID.JavaIOError.name                          -> ClassID.XScalaIOError.name,
    ClassID.JavaThreadDeath.name                      -> ClassID.XScalaThreadDeath.name,
    ClassID.JavaIncompatibleClassChangeError.name     -> ClassID.XScalaIncompatibleClassChangeError.name,
    ClassID.JavaInstantiationError.name               -> ClassID.XScalaInstantiationError.name,
    ClassID.JavaStackOverflowError.name               -> ClassID.XScalaStackOverflowError.name,
    ClassID.JavaOutOfMemoryError.name                 -> ClassID.XScalaOutOfMemoryError.name,
    ClassID.JavaInternalError.name                    -> ClassID.XScalaInternalError.name,
    ClassID.JavaNumberFormatException.name            -> ClassID.XScalaNumberFormatException.name,
    ClassID.JavaNoSuchAlgorithmException.name         -> ClassID.XScalaNoSuchAlgorithmException.name,
    ClassID.JavaEOFException.name                     -> ClassID.XScalaEOFException.name,
    ClassID.JavaFileNotFoundException.name            -> ClassID.XScalaFileNotFoundException.name,
    ClassID.JavaUnsupportedEncodingException.name     -> ClassID.XScalaUnsupportedEncodingException.name,
    ClassID.JavaStringIndexOutOfBoundsException.name  -> ClassID.XScalaStringIndexOutOfBoundsException.name,
    ClassID.JavaUncaughtExceptionHandler.name         -> ClassID.XScalaUncaughtExceptionHandler.name,
  ))

  def preprocessClassName(name: XString, importer: pcOModule.Class): XString = {
    if (name startsWith xscalaClassPrefix) {
      // Classes from XScala package don't need to be preprocessed
      name match {
        case `foreignRefTypeName`  => ClassID.AJObject.name
        case `foreignRefType0Name` => ClassID.AJObject.name
        case _ => name
      }

    } else if (importer.isXScalaType) {
      name match {
        case ClassID.JavaPrintStream.name if O2Env.env.enabled(AllowMappingOfJDKIOToXScala) ||
          CacheAPIModule.isThisClass(importer, ClassID.XScalaIOJET) ||
          CacheAPIModule.isThisClass(importer, ClassID.XScalaThrowable) => ClassID.XScalaPrintStream.name

        case _ =>
          scalaClassNameReplacements.get(name)
            .orElse(findJreOverride(name))
            .getOrElse(name)
      }
    } else {
      name
    }
  }

  /** Moves Scala standard library classes in Java XKRN to private runtime package,
    * so that they will not clash with classes from Scala applications.
    *
    * See JET-8955.
    */
  def movedScalaClassName(name: XString, isRuntimeClass: Boolean = O2Env.env.enabled(BuildXKRN)): XString = {
    if (languagePack.supports(JAVA) && isRuntimeClass && name.startsWith(scalaPrefix)) {
      movedScalaAddend.concat(name)
    } else {
      name
    }
  }

  /** Restores original Scala standard library classes packages in Java XKRN from private runtime package
    * movement by [[movedScalaClassName]].
    *
    * See JET-8955.
    */
  def originalScalaClassName(name: XString, isRuntimeClass: Boolean = O2Env.env.enabled(BuildXKRN)): XString = {
    if (languagePack.supports(JAVA) && isRuntimeClass && name.startsWith(movedScalaPrefix)) {
      name.substring(movedScalaAddend.length)
    } else {
      name
    }
  }

  def preprocessMethodName(name: XString, clazz: pcOModule.Class): XString = {
    if (clazz.isXScalaType) {
      // This workaround required because we could not declare
      //   @static def hashCode(x: Byte): Int
      // in BoxedByte class (and other boxes). Scalac failed on this code despite that there are
      //   static int hashCode(long value)
      // in java.lang.Long class.
      name match {
        case XString(`hashCode0Name` | `toString0Name` | `isNaN0Name` | `isInfinite0Name`)
          if CacheAPIModule.isThisClass(clazz.getSuperClassO2, XScalaBoxNumber) =>
          name.substring(0, name.length - 1)

        case XString(`hashCode0Name` | `toString0Name`)
          if CacheAPIModule.isThisClass(clazz, XScalaBoxChar) || CacheAPIModule.isThisClass(clazz, XScalaBoxBoolean) =>
          name.substring(0, name.length - 1)

        case XString(`_equalsName`) if CacheAPIModule.isThisClass(clazz, XScalaArrays) =>
          XString(`equalsName`)

        case _ => name
      }
    } else {
      name
    }
  }

  def ignoreMethod(name: XString, sig: XString, clazz: pcOModule.Class): Boolean = {
    clazz.isXScalaType && (
      // writeReplace and $deserializeLambda$ functions are ignored in XScala lambda functions
      // because we do not use "serialized" lambda types
      name == writeReplaceName && sig == writeReplaceSig ||
        name == deserializeLambda ||
        // chars() and codePoints() are inserted by scalac for unknown reason
        // when type inherits CharSequence, however, they are not used and can be safely ignored
        (name == charsName || name == codePointsName) && sig == charsOrCodePointsSig
      )
  }

  def isUnstableForwarder(name: XString, clazz: pcOModule.Class, accessFlags: Set32): Boolean = {
    clazz.isXScalaType && (accessFlags contains (AccPublic | AccStatic)) &&  (
      name.toString match {
        case unstableForwarderPattern() => true
        case _ => false
      }
    )
  }

  def ignoreDeferred(name: String, clazz: pcOModule.Class): Boolean = {
    clazz.isXScalaType && (
      // LambdaMetafactory should be only used in lambda invokedynamic
      // which we replace with generated class in LambdaTypeGeneratorImpl
      name.startsWith(lambdaMetafactoryName) ||
        // Deferred types are allowed only inside CHIR parsing and flatbuffers packages
        // which are not used in XScala.
        // TODO: separate CHIR parsing from opt and remove this workaround
        clazz.name.startsWith(chirPrefix) ||
        clazz.name.startsWith(flatbuffersPrefix)
      )
  }

  private val writeReplaceName = XString("writeReplace")
  private val writeReplaceSig = XString("()Ljava/lang/Object;")
  private val deserializeLambda = XString("$deserializeLambda$")

  private val charsName = XString("chars")
  private val codePointsName = XString("codePoints")
  private val charsOrCodePointsSig = XString("()Ljava/util/stream/IntStream;")

  private val xscalaClassPrefix = XString("xscala/")
  private val jreClassPrefix = XString("java/")

  private val xscalaNativesPrefix = XString("com/huawei/excelsior/jet/runtime/natives/xscala")

  private val foreignRefTypeName = XString("xscala/internal/ForeignRefType")
  private val foreignRefType0Name = XString("xscala/internal/ForeignRefType0")

  private val lambdaMetafactoryName = "java/lang/invoke/LambdaMetafactory"

  private val hashCode0Name = "hashCode0"
  private val toString0Name = "toString0"

  private val isNaN0Name = "isNaN0"
  private val isInfinite0Name = "isInfinite0"

  private val _equalsName = "_equals"
  private val equalsName = "equals"

  private val scalaPrefix = XString("scala/")
  private val movedScalaAddend = XString("com/huawei/excelsior/jet/")
  private val movedScalaPrefix = movedScalaAddend.concat(scalaPrefix)

  private val unstableForwarderPattern: Regex = """.*\$[0-9]+\$$""".r

  private val flatbuffersPrefix = XString("com/google/flatbuffers")
  private val chirPrefix = XString("com/huawei/excelsior/jet/compiler/chir")
}
