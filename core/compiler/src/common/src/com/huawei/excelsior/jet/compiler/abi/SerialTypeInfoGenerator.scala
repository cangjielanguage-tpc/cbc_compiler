/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier
import com.huawei.excelsior.jet.compiler.layout.{FieldsLayout, MethodTables}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.*
import xscala.io.LEB128Encoder.*
import xscala.io.{ByteBuffer, DataOutput}
import xscala.util.MathUtils

object SerialTypeInfoGenerator {

  private def genAOTGCInfo(uleb: Int => Unit, sleb: Int => Unit, tpe: Type, env: Environment): Unit = {
    if (tpe.isInfectedAJClass) return

    implicit val typeProvider: TypeProvider = env.getTypeProvider

    if (tpe.isArray) {
      genArrayGCInfo(uleb, sleb)(tpe)
    } else {
      gatherStaticRecordRefFieldOffsets(uleb, sleb)(tpe)
      genHostingGCInfo(uleb, sleb)(tpe, env)
    }
  }

  private def genHostingGCInfo(uleb: Int => Unit, sleb: Int => Unit)(tpe: Type, env: Environment): Unit = {
    implicit val typeProvider: TypeProvider = env.getTypeProvider
    def genRefFieldOffsets(fieldOffsets: Int*): Unit =
      gatherRefFieldOffsets(uleb, sleb)(fieldOffsets *)

    assert(tpe.isClassOrInterface || tpe.isRecord)
    if ((tpe.isClass || tpe.isRecord) && !tpe.isErroneous) {
      uleb(tpe.getRawObjectSize)

      if (tpe.isAbstractClass) {
        uleb(0) // flags
        genRefFieldOffsets() // no ref fields

      } else {
        var flags = 0
        // Shared flags.
        if (tpe.isAJLockable) {
          flags |= RTConst.InstanceDescriptor.Builder.Flags.LOCKABLE_MASK.intValue
        }
        if (env.getTypeProvider.getBacktraceType.isAssignableFrom(tpe)) {
          flags |= RTConst.InstanceDescriptor.Builder.Flags.BACKTRACE_MASK.intValue
        }
        if (tpe.finalizable) {
          flags |= RTConst.InstanceDescriptor.Builder.Flags.HAS_FINALIZE_MASK.intValue
        }
        if (tpe.isGuest) {
          flags |= RTConst.InstanceDescriptor.Builder.Flags.GUEST_MASK.intValue
        }
        if (env.getTypeProvider.getReferenceType.isAssignableFrom(tpe) ||
          env.getTypeProvider.getAJWeakRefType.isAssignableFrom(tpe) ||
          env.getTypeProvider.isCangjieWeakRef(tpe)) {
          flags |= RTConst.InstanceDescriptor.Builder.Flags.WEAK_OBJECT_MASK.intValue
        }

        // Java-specific flags.
        if (tpe.isJavaReference) {
          if (env.getTypeProvider.getCloneableType.isAssignableFrom(tpe)) {
            flags |= RTConst.JavaInstanceDescriptor.Builder.JavaFlags.CLONEABLE_MASK.intValue
          }
          if (env.getTypeProvider.getSerializableType.isAssignableFrom(tpe)) {
            flags |= RTConst.JavaInstanceDescriptor.Builder.JavaFlags.SERIALIZABLE_MASK.intValue
          }
        }
        uleb(flags)

        val fieldOffs = asClassType(tpe).getRefFieldOffsets
        genRefFieldOffsets(fieldOffs.toSeq *)
      }
    }
  }

  private def genArrayGCInfo(uleb: Int => Unit, sleb: Int => Unit)(tpe: Type)(implicit typeProvider: TypeProvider): Unit = {
    assert(tpe.isArray)
    def genRefFieldOffsets(fieldOffsets: Int*): Unit =
      gatherRefFieldOffsets(uleb, sleb)(fieldOffsets*)

    val elemType = tpe.getArrayElemType
    if (elemType.isRecord) {
      val elementTpe = asClassType(elemType)
      if (elementTpe.isDeferred) {
        sleb(-1) // elem size
      } else {
        sleb(elementTpe.getRawObjectSize) // elem size
      }
      val fieldOffs = elementTpe.getRefFieldOffsets
      genRefFieldOffsets(fieldOffs.toSeq *)

    } else if (tpe.isAJArray || tpe.isCangjieArray) {
      // TODO: remove ref field offsets from AJArray
      val kind = elemType.symKindErased
      sleb(kind.size) // elem size
      if (kind.isReference) {
        genRefFieldOffsets(0) // single ref field
      } else {
        genRefFieldOffsets() // no ref fields
      }
    }
  }

  def gen(tpe: ClassType, env: Environment): ByteBuffer = {
    implicit val typeProvider: TypeProvider = env.getTypeProvider
    val buf = new ByteBuffer()

    def importIndex(c: ClassType) = env.getImportedClassIdx(c, tpe)
    def uleb(i: Int): Unit = buf.putULEB(i)
    def sleb(i: Int): Unit = buf.putSLEB(i)

    assert(tpe.isTraceableReference || tpe.isInfectedAJClass || tpe.isRecord)

    def genTypeInfo(): Unit = {
      if (tpe.isAJArray || tpe.isCangjieArray || tpe.isInfectedAJClass) return

      if (tpe.isClass && !tpe.isCangjiePackage) {
        val cohenSuper = tpe.getCohenSupertype
        val cohenSuperIdx = if (cohenSuper == null) {
          RTConst.RunTimeTypeInfo.Builder.HIERARCHY_ROOT_IMPORT_INDEX.intValue
        } else {
          env.getImportedClassIdx(cohenSuper, tpe)
        }
        sleb(cohenSuperIdx)

        // write diff of all cohenSuper interfaces and ours
        val inherited = if (cohenSuper == null) 0 else cohenSuper.allSuperInterfaces.size
        val interfs = tpe.allSuperInterfaces.iterator.drop(inherited).toArray
        uleb(interfs.length)
        for (i <- interfs) {
          uleb(importIndex(i))
          sleb(tpe.getIMTSlot(i))
        }

      } else {
        // write all interfaces
        val interfs = tpe.allSuperInterfaces.toArray
        uleb(interfs.length)
        for (i <- interfs) {
          uleb(importIndex(i))
        }
      }
    }

    def genMethodInfo(): Unit = {
      if (tpe.isAJArray || tpe.isCangjieArray) return

      val codeUnits = tpe.getGeneratedMethods.toSeq ++ tpe.getVersionedMethods
      uleb(codeUnits.size)

      for (x <- codeUnits) {
        var flags = 0
        x match {
          case m: Method =>
            flags |= RTConst.CodeUnitFlags.COMPILED.intValue
            if (m.isCAnnotated) {
              flags |= RTConst.CodeUnitFlags.C_ANNOTATED.intValue
            } else if (tpe.isCompilerInterface) {
              flags |= RTConst.CodeUnitFlags.RTS_PROC.intValue
            }

          case cu: CodeUnit =>
            assert(cu.isVersionedMethod)
            flags |= RTConst.CodeUnitFlags.COMPILED.intValue
            flags |= RTConst.CodeUnitFlags.VERSIONED.intValue
        }
        uleb(flags)
      }

      for (x <- codeUnits) {
        x match {
          case m: Method =>
            if (m.isCAnnotated) {
              uleb(m.getParamsCount)
              for (i <- 0 until m.getParamsCount) {
                val paramType = m.getParamType(i).symKindErased
                assert(paramType != TypeKind.RECORD) // TODO: support pass-by-value in native calls
                uleb(paramType.getBasicType)
              }
              val retType = m.getReturnType.symKindErased
              assert(retType != TypeKind.RECORD) // TODO: support pass-by-value in native calls
              uleb(retType.getBasicType)
            } else if (tpe.isCompilerInterface) {
              assert(!m.getMethodType.isAJLongSafe)
              val cc = m.getCallConv.ordinal
              uleb(cc)
            }

          case cu: CodeUnit =>
            assert(cu.isVersionedMethod)
            uleb(importIndex(cu.method.getDeclaringClass))
            uleb(cu.getHostedIndex)
        }
      }
    }

    genTypeInfo()
    genMethodInfo()
    genAOTGCInfo(uleb, sleb, tpe, env)

    buf
  }

  /**
    * Converts Java method's access modifier to RT MethodAccessFlags representation
    */
  def convertJavaAccessModifiers(acc: Modifiers, env: Environment): Int = {
    var flags = 0
    def addFlag(methodAccessModifier: RTConst) : Unit = {
      flags |= 1 << methodAccessModifier.intValue
    }

    import RTConst.MethodAccessModifier.*
    if (acc contains Modifier.PRIVATE) {
      addFlag(PRIVATE)
    }
    if (acc contains Modifier.PROTECTED) {
      addFlag(PROTECTED)
    }
    if (acc contains Modifier.PUBLIC) {
      addFlag(PUBLIC)
    }

    if (acc contains Modifier.STATIC) {
      addFlag(STATIC)
    }
    if (acc contains Modifier.FINAL) {
      addFlag(FINAL)
    }
    if (acc contains Modifier.ABSTRACT) {
      addFlag(ABSTRACT)
    }
    if (acc contains Modifier.STRICT) {
      addFlag(STRICT)
    }
    if (acc contains Modifier.SYNCHRONIZED) {
      addFlag(SYNCHRONIZED)
    }

    flags
  }

  def genPreparationInfo(tpe: Type, env: Environment): ByteBuffer = {
    val superTypes = if (tpe.isInfectedAJClass) {
      tpe.getSuperClasses.filter(_.preparationRequired)
    } else {
      Iterator.empty
    }
    val prepTypes = (env.getMarkedForPreparationTypes ++ superTypes).filter(_ != tpe).toArray
    if (prepTypes.isEmpty) return null

    assert(tpe.preparationRequired) // Otherwise preparation from import cannot be used

    val buf = new ByteBuffer()

    buf.putULEB(prepTypes.length)
    for (t <- prepTypes) {
      assert(t.preparationRequired)
      buf.putULEB(env.getImportedClassIdx(t, tpe))
    }

    buf
  }

  /** @see [[com.huawei.excelsior.jet.runtime.typedesc.VMTEncoding]] */
  def encodeMTLayout(tpe: ClassType, env: Environment)(implicit typeProvider: TypeProvider): ByteBuffer = {
    val buf = new ByteBuffer()
    encodeMTLayout(tpe, env, buf)
    buf
  }

  /** @see [[com.huawei.excelsior.jet.runtime.typedesc.VMTEncoding]] */
  def encodeMTLayoutForUnitTest(tpe: ClassType, env: Environment, out: DataOutput, typeProvider: TypeProvider): Unit = {
    implicit val tp = typeProvider
    encodeMTLayout(tpe, env, out, env.getImportedClassIdx(_, tpe))
  }


  /** @see [[com.huawei.excelsior.jet.runtime.typedesc.VMTEncoding]] */
  def encodeMTLayout(tpe: ClassType, env: Environment, out: DataOutput)(implicit typeProvider: TypeProvider): Unit = {
    encodeMTLayout(tpe, env, out, env.getImportedClassIdx(_, tpe))
  }

  def encodeMTLayout(tpe: ClassType, env: Environment, out: DataOutput, importIndex: ClassType => Int)(implicit typeProvider: TypeProvider): Unit = {
    if (tpe.isThinClass || tpe.isErroneous) {
      // no vmt init info
      out.putULEB(0)
      return
    }

    //////////////////// Encoding utilities ////////////////////

    val buf = new ByteBuffer()

    import RTConst.VMTEncoding.Instruction.Kind.*
    def genInstr(tagConst: RTConst, value: Int): Unit = {
      val tagSize = SHIFT.intValue
      val tag = tagConst.intValue
      assert(MathUtils.isNBits(tag, tagSize))
      assert(MathUtils.isNBits(value, 32 - tagSize))
      buf.putULEB(tag | (value << tagSize))
    }

    var skipAmount = 0
    def skipOne(): Unit = {
      skipAmount += 1
    }

    def genSkip(): Unit = {
      if (skipAmount > 0) {
        genInstr(SKIP, skipAmount)
        skipAmount = 0
      }
    }

    def genError(error: MethodSearchError): Unit = {
      assert(tpe.isJavaReference || tpe.isAbstractClass, "Corrupted class hierarchy (abstract method in non-abstract class *OR* illegal access). Please recompile all classes.")
      genSkip()
      genInstr(ERROR, error.ordinal)
    }

    def genCodeUnit(codeUnit: CodeUnit): Unit = {
      genSkip()
      val hostingClass = codeUnit.getHostingClass
      if (hostingClass == tpe) {
        genInstr(OWN_METHOD, codeUnit.getHostedIndex)
      } else {
        genInstr(SUPER_METHOD, importIndex(hostingClass))
        buf.putULEB(codeUnit.getHostedIndex)
      }
    }

    //////////////////// VMTs for class and super ////////////////////

    def implAndRef(implClass: ClassType, ref: MethodTables.Ref) = implClass.findMethodImplementation(ref) match {
      case r: FindMethodImplResult.Error => (r.result, ref)
      case r: FindMethodImplResult.Found => (implClass.chooseMethodVersion(r.result), ref)
    }

    val vmt: Array[(Object, MethodTables.Ref)] = MethodTables.buildMT(tpe, implAndRef(tpe, _))
    assert(vmt.length == tpe.getMTLayout.size, s"vmt.size=${vmt.length}, layout.size=${tpe.getMTLayout.size}")

    val superclass = asClassType(tpe.getSuperClassSig)
    val superVMT: Array[(Object, MethodTables.Ref)] = if (superclass == null) {
      Array.empty
    } else {
      MethodTables.buildMT(superclass, implAndRef(superclass, _))
    }

    //////////////////// Instructions ////////////////////

    for (((impl, ref), i) <- vmt.zipWithIndex) {
      if (i < superVMT.length && superVMT(i) == (impl, ref)) {
        skipOne()
      } else {
        impl match {
          case error: MethodSearchError =>
            val method = ref.method
            val target = CodeUnit.of(method)
            if (error == MethodSearchError.ABSTRACT_METHOD && method.isAbstract) {
              // Size optimization: do not generate AME error instruction for abstract methods (decoder will infer it)
              genCodeUnit(target)
            } else {
              genError(error)
              genCodeUnit(target)
            }

          case target: CodeUnit =>
            genCodeUnit(target)

          case x => shouldNotReachHere(x)
        }
      }
    }

    //////////////////// Output ////////////////////

    out.putULEB(buf.length)
    out.putBytes(buf.getBytesPointer, 0, buf.length)
  }


  private def gatherRefFieldOffsets(uleb: Int => Unit, sleb: Int => Unit)(fieldOffsets: Int*): Unit = {
    uleb(fieldOffsets.size)
    var curOffs = 0
    for (fieldOffs <- fieldOffsets) {
      val offset = fieldOffs
      assert(offset >= curOffs) // Note: can be zero for AJ array
      uleb(offset - curOffs) // TODO: compress further (e.g. divide diff by 8, use bitmap etc.)
      curOffs = offset
    }
  }

  private def gatherStaticRecordRefFieldOffsets(uleb: Int => Unit, sleb: Int => Unit)(tpe: Type)(implicit typeProvider: TypeProvider): Unit = {
    val staticRecords = FieldsLayout.staticFieldsLayout(asClassType(tpe)) filter (_.field.getType.isRecord)
    val refsInStaticRecords = staticRecords flatMap { x =>
      FieldsLayout.getRefFieldOffsets(asClassType(x.field.getType), x.offs)
    }
    gatherRefFieldOffsets(uleb, sleb)(refsInStaticRecords.toSeq *)
  }
}
