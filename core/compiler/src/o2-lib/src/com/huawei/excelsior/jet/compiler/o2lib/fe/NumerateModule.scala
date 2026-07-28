/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe

import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.{RTConst, TypeProvider}
import com.huawei.excelsior.jet.compiler.layout.FieldsLayout
import com.huawei.excelsior.jet.compiler.layout.FieldsLayout.FieldOffs
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, ExtraPassModule as ExtraPass, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.u.xiEnvModule as env
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.{classByO2Object, fieldToO2Field, sigTypeToO2Type, typeToO2Class, typeToO2Type}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.{LightweightEnvironment, MethodTablesImpl, O2TypeProvider}
import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.o2j.runtime.*
import xscala.util.MathUtils.{alignUp, isAligned}

import scala.annotation.tailrec
import scala.collection.mutable

object NumerateModule { /* paul, 30 mar 2006 */

  private implicit val typeProvider: TypeProvider = LightweightEnvironment.getInstance

  private val rem2align: Array[Int] = Array[Int](8, 1, 2, 1, 4, 1, 2, 1)
  def getAlign(adr: Int): Int = rem2align(adr & 7)

  /*  a := mk_align(adr,align) is equivalent to a := adr + pad(adr, align)  */
  def mkAlign(adrPar: Int, align: Int): Int = {
    var adr = adrPar

    val i = O2JSupport.mod(adr, align)
    if (i != 0) {
      adr += align - i
    }
    adr
  }

  private def checkMembersInClassfileOrder(members: Iterator[pcO.Member]): Unit =
    assert(members.zipWithIndex.forall((member, idx) => member.getNumberInClassFile == idx))

  def checkMethodOrder(c: pcO.Class): Unit = {
    if (isWorkMode) {
      checkMembersInClassfileOrder(c.declaredMethods)
    }
  }

  def checkFieldOrder(c: pcO.Class): Unit = {
    if (isWorkMode) {
      checkMembersInClassfileOrder(c.declaredFields)
    }
  }

  /* ------ Calculation of virtual/interface numbers for methods ------ */
  private def numerateSuperInterfs(t: pcO.Class): Boolean = {
    assert(!t.virtualNumbersAreNumerated)
    assert(!t.isUnloadable)

    pc.withModule(t) {
      for (intf <- t.getSuperInterfacesO2) {
        if (!calculateVirtualNumbers(intf)) {
          return false
        }
      }
    }

    checkMethodOrder(t)
    true
  }

  private def calculateVirtualNumbers(t: pcO.Class): Boolean = {
    if (t.isUnloadable) {
      return false
    }

    if (t.virtualNumbersAreNumerated) {
      return true
    }

    if (ExtraPass.checkSuperForClassDefError(t)) {
      return false
    }

    if (targetArch == Arch.CBC) {
      t.setVMTSize(0)
      t.markAsVirtualNumbersNumerated()
      return true
    }

    val super0 = t.getSuperClassO2
    if (super0 != null) {
      if (!calculateVirtualNumbers(super0)) {
        // Calling CalculateVirtualNumbers for super class can make it not verifiable
        // (by CheckOverrideFinal, for example in JET-4906).
        // For saving closure of not verifiable tags we check it and
        // set for current class if necessary.
        // Moreover as result of fixing JET-5813, JET-5860 verifier for superclass
        // can be invoked before numerate of current class but after make_objects.
        // In this case we need to mark current class as not verifable also.
        assert(!super0.isVerifiable)
        t.copyVerifyErrorFrom(super0)
        return false
      }
      t.setInheritanceLevel(super0.getInheritanceLevel + 1)
    } else {
      t.setInheritanceLevel(0)
    }

    if (!numerateSuperInterfs(t)) {
      return false
    }

    for (m <- t.declaredInstanceMethods) {
      if (super0 != null && !(m.isConstructor || m.isPrivate)) {
        // do not check final overriding for private methods (like HotSpot does)
        val b = super0.findMethod(m.name, m.getSignature)
        if (b != null && !b.isStatic && pcO.isMemberAccessible(b, t)) {
          if (ExtraPass.checkOverrideFinal(b, t)) {
            return false
          }
        }
      }

      // Propagate final modifier from class to methods.
      // This is required to ensure that @Inline "virtual" functions are not added to VMT.
      // Otherwise, such function body will be removed and linker will fail to find appropriate symbol to link with VMT entry.
      if (t.isJetRuntimeClass && t.isFinal) {
        m.markAsFinal()
      }
    }

    MethodTablesImpl.buildMTLayout(t.asCT)
    t.markAsVirtualNumbersNumerated()
    true
  }

  private def calculateInheritanceLevelForNotVerifiable(t: pcO.Class): Unit = {
    if (t.virtualNumbersAreNumerated) {
      return
    }

    if (ExtraPass.checkSuperForClassDefError(t)) {
      return
    }

    val super0 = t.getSuperClassO2
    if (super0 != null) {
      if (!calculateVirtualNumbers(super0)) {
        if (super0.isClassDefinitionError) {
          t.copyVerifyErrorFrom(super0)
          return
        } else if (super0.isNotVerifiedCode) {
          // super could also have not verified code
          calculateInheritanceLevelForNotVerifiable(super0)
        } else {
          throw new AssertionError
        }
      }
      t.setInheritanceLevel(super0.getInheritanceLevel + 1)
    } else {
      t.setInheritanceLevel(0)
    }

    t.markAsVirtualNumbersNumerated()
  }

  /* ------ Memory layout of objects; offsets for instance fields ----- */
  private def calculateStaticFieldsOffsets(t: pcO.Class): Unit = {
    val fieldOffsets = FieldsLayout.staticFieldsLayout(t.asCT)
    for (FieldOffs(f, offs) <- fieldOffsets) {
      fieldToO2Field(f).asInstanceOf[pcO.StaticField].setOffset(offs)
    }
  }

  private def makeInstanceFieldLayout(t: pcO.Class, startOffs: Int): (Int, Int) = {
    val fieldOffsets = FieldsLayout.instanceFieldsLayout(t.asCT)
    var size = startOffs
    var maxAlignment = 1

    for (FieldOffs(f, offs) <- fieldOffsets) {
      val field = fieldToO2Field(f).asInstanceOf[pcO.InstanceField]
      field.setOffset(offs)
      maxAlignment = maxAlignment max field.alignment

      val curSize = offs + field.size
      if (curSize > size) {
        size = curSize
      }
    }

    (size, maxAlignment)
  }

  private def checkInstanceFieldOffsets(t: pcO.Class): Unit =
    assert(t.declaredInstanceFields forall (_.offsetIsCalculated))

  private def checkStaticFieldOffsets(t: pcO.Class): Unit =
    assert(t.declaredStaticFields forall (f => f.offsetIsCalculated || f.isStatic && f.isAJFlat))

  private def checkDeferredSupers(t: pcO.Class): Unit = {
    Closure.withPreAction(mutable.HashSet.empty, Seq(t))(s => Option(s.getSuperClassO2) ++ s.getSuperInterfacesO2) { s =>
      if (s.hasDeferredSuper0 || s.isBitcodeDeferred) {
        t.markAsHasDeferredSuper()
        return
      }
    }
  }

  private def calculateInstanceLayout(t: pcO.Class): Boolean = {

    if (t.isUnloadable) {
      return false
    }

    if (t.instanceLayoutIsNumerated) {
      return true
    }

    if (ExtraPass.checkSuperForClassDefError(t)) {
      return false
    }

    checkDeferredSupers(t)
    if (targetArch == Arch.CBC) {
      // avoid layout calculating
      t.declaredInstanceFields.foreach(_.setOffset(0))
      t.size = 0
      t.alignment = 0
      t.markAsInstanceLayoutNumerated()
      return true
    }

    val super0 = t.getSuperClassO2
    if (super0 != null) {
      if (!calculateInstanceLayout(super0)) {
        return false
      }
    }

    // Calculate layout of flat record field types, before calculating own layout
    for (f <- t.declaredInstanceFields if f.isAJFlat && f.getSignature.isRecord) {
      val sig = f.getSignature
      assert(!sig.isDeferred)
      val fieldType = typeToO2Class(sig.symType)
      if (!calculateInstanceLayout(fieldType)) {
        return false
      }
      f.setAJFlatInfo(fieldType.size, fieldType.alignment)
    }

    if (t.isAJCompoundClass) {
      // For AJ Compound classes layout must be calculated by aj-javac and saved in annotations.
      // Check that we have already read and applied that annotations values.
      assert(t.sizeCalculated)
      assert(t.alignmentCalculated)

      checkInstanceFieldOffsets(t)

      t.markAsInstanceLayoutNumerated()
      true

    } else if (t.isCangjieArray && t.getCangjieArrayElementType.isRecord || t.isVArray) {
      assert(t.declaredFields.isEmpty)

      val elemType = typeToO2Type(t.getCangjieArrayElementType.symType)
      elemType match {
        case elemType: pcO.Class => calculateInstanceLayout(elemType)
        case _ =>
      }

      val elemSize = elemType.size
      val elemAlignment = elemType.alignment

      if (t.isCangjieArray) {
        assert(isAligned(RTConst.CangjieArray.BODY_OFFS.intValue, elemAlignment))

        // TODO: for Cangjie array, size and alignment of reference type should be here instead of record element
        t.size = elemSize
        t.alignment = elemAlignment

      } else {
        assert(t.isVArray)

        val fullVArrSize: Long = elemSize * t.symType.getVArrayLength
        assert(fullVArrSize.isValidInt, s"Unsupported non-32-bit VArray size $fullVArrSize")

        t.size = fullVArrSize.toInt
        t.alignment = elemAlignment
      }

      t.markAsInstanceLayoutNumerated()
      true

    } else {

      var startOffs: Int = 0

      if (t.isRecord || t.isValueClass || t.isNamespace || O2TypeProvider.isAJCompoundType(t)) {
        startOffs = 0
      } else if (O2TypeProvider.isAJObject(t) || O2TypeProvider.isLockableAJObject(t)) {
        startOffs = t.getObjectHeaderSize
      } else if (super0 != null) {
        startOffs = super0.size
      } else if (!t.isInterface) {
        startOffs = t.getObjectHeaderSize
      } else {
        startOffs = 0
      }

      checkFieldOrder(t)

      val (newOffs, alignment) = makeInstanceFieldLayout(t, startOffs)
      val offs = newOffs

      checkInstanceFieldOffsets(t)

      if (O2TypeProvider.isAJCompoundType(t)) {
        assert(offs == 0)
        // Note: size and alignment cannot be zero even for this fake type, because of assertions in type serialization.
        t.size = Int.MaxValue
        t.alignment = addressSize
      } else if (t.isRecord) {
        t.size = alignUp(offs, alignment)
        t.alignment = alignment
      } else {
        t.size = offs
        // NOTE currently we do not have fields aligned at value greater than 8 => HeapObj.alignment is ok here.
        t.alignment = RTConst.HeapObj.alignment
      }

      t.markAsInstanceLayoutNumerated()
      true
    }
  }

  private def calculateStaticLayout(t: pcO.Class): Boolean = {
    if (t.isUnloadable) {
      return false
    }

    if (t.staticLayoutIsNumerated) {
      return true
    }

    if (!t.instanceLayoutIsNumerated) {
      // don't try to process static fields if instance fields already failed to numerate
      return false
    }

    if (targetArch == Arch.CBC) {
      // avoid layout calculating
      t.declaredStaticFields.foreach(_.setOffset(0))
      t.markAsStaticLayoutNumerated()
      return true
    }

    for (f <- t.declaredStaticFields if f.isAJFlat && f.getSignature.isRecord) {
      val sig = f.getSignature
      assert(!sig.isDeferred)
      val fieldType = typeToO2Class(sig.symType)
      if (!calculateInstanceLayout(fieldType)) {
        return false
      }
      f.setAJFlatInfo(fieldType.size, fieldType.alignment)
    }

    calculateStaticFieldsOffsets(t)

    checkStaticFieldOffsets(t)

    t.markAsStaticLayoutNumerated()
    true
  }

  /* --------------------- sorting objects by name -------------------- */
  // ctag_num_field_order     => fields are 'in right order' (order in which they're defined in class file)
  // ctag_num_method_order    => methods are 'in right order' (their names are lexically ordered)
  // ctag_num_virtual         => instance methods' virtual numbers are calculated
  // ctag_num_instance_layout => instance fields' offsets are calculated
  // ctag_num_static_layout   => static fields' offsets are calculated
  // class parsed from bytecode  => fields-, methods-, virtual-, layout-,
  //                                  (in fact, fields are in BC order)
  // PreProcessBytecode() called => fields+, methods+, virtual-, layout-,
  // ProcessClass() called       => fields+, methods+, virtual+, layout+.
  // class loaded from sym-file  => fields+, methods+, virtual+, layout+,
  // class imported from TD      => fields+, methods+, virtual-, layout-,
  //   Instance fields' offsets & instances methods' virtual numbers
  //   was read from TD and stored in xxx_TD fields of pcO.InstanceField, pcO.InstanceMethod, pcO.Class.
  // ProcessClass() called       => fields+, methods+, virtual+, layout+.
  // after sym-level clean       => fields-, methods-, virtual-, layout-.
  private def checkClassProcessed(clazz: pcO.Class): Unit = {
    assert(clazz.virtualNumbersAreNumerated)
    assert(clazz.instanceLayoutIsNumerated)
    assert(clazz.staticLayoutIsNumerated)
  }

  def processClass(clazz: pcO.Class): Unit = {
    if (clazz.isUnloadable || clazz.isSynthetic) {
      if (!clazz.isClassDefinitionError && clazz.isNotVerifiedCode) {
        calculateInheritanceLevelForNotVerifiable(clazz)
      }
      return
    }

    // ctag_num_virtual & ctag_num_instance_layout & ctag_num_static_layout may be already set by recursive calls
    var ok = calculateInstanceLayout(clazz)
    ok &= calculateStaticLayout(clazz)
    ok &= calculateVirtualNumbers(clazz)
    if (ok) {
      checkClassProcessed(clazz)
    }
  }

  def preProcessBytecode(clazz: pcO.Class): Unit = {
    assert(!clazz.virtualNumbersAreNumerated)
    assert(!clazz.instanceLayoutIsNumerated)
    assert(!clazz.staticLayoutIsNumerated)

    if (!clazz.isVerifiable) {
      return
    }

    checkMethodOrder(clazz)
    checkFieldOrder(clazz)
  }
}
