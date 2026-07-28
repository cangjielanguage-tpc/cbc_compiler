/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.layout

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, FindMethodImplResult, Method, SignatureType}
import xscala.util.simpleClassName

import scala.collection.mutable
import scala.reflect.ClassTag

// TODO: combine with MethodTablesScala
object MethodTables {

  val NO_VNUM = -1 // must be the same as `com.huawei.excelsior.jet.runtime.typedesc.VMTEncoding.NO_VNUM`
  val NO_INUM = -1
  private val EMPTY_INUMS = Array.empty[Int]

  trait Ref {
    def refClass: ClassType
    def method: Method

    private[layout] def isPackagePrivate = !method.isPublic && !method.isPrivate && !method.isProtected
  }

  // TODO-SCALA: for some reason scala code produces unverifiable bytecode if these methods are in interface Ref
  object Ref {
    private[layout] def equals(key1: Ref, key2: Ref) = {
      // always check correctness of method modifiers
      canBeInMethodTable(key1.method) && canBeInMethodTable(key2.method) &&
        // always check name and signature
        key1.method.overridesNameAndSig(key2.method) &&
        // if at least one ref is package-private
        (!(key1.isPackagePrivate || key2.isPackagePrivate) ||
          // then check package name and classloader
          key1.refClass.isSamePackage(key2.refClass))
    }

    private[layout] def hashCode(key: Ref) = key.method.getXName.hashCode
  }

  // TODO: encapsulate in `MethodTables.Ref` object
  // TODO: At this moment it won't work due to Scala's idiosyncratic name mangling (see JET-14219)
  private abstract class RefComparable extends Ref {
    override def equals(obj: Any) = obj match {
      case ref: AnyRef if this eq ref => true
      case ref: Ref => Ref.equals(this, ref)
      case _ => false
    }

    override def hashCode = Ref.hashCode(this)

    override def toString = s"ComparableMethodRef($refClass, $method)"
  }

  def canBeInMethodTable(x: Method) = {
    // TODO: Do not rely on "calculation of the same layout in JIT and AOT"!
    //       Use only symbolic references for to @java-annotated entities.
    // TODO: Assert that MT layout is not accessed for @java-annotated classes when possible.
    !x.isStatic && !x.isPrivate && !x.isConstructor
  }

  def ref(_refClass: ClassType, _method: Method): Ref = new RefComparable() {
    override def refClass: ClassType = _refClass
    override def method: Method = _method
  }

  trait VNumMap {
    def contains(x: Ref): Boolean
    def apply(x: Ref): Int
    def refs: Iterator[Ref]
  }

  // TODO: encapsulate in `MethodTables.VNumMap` object (blocker - JET-14219)
  trait VNumMapBuilder extends VNumMap {
    def update(x: Ref, vnum: Int): Unit
  }

  // TODO: encapsulate in `MethodTables.VNumMap` object (blocker - JET-14219)
  class VNumMapDefault extends VNumMapBuilder {
    /** Note: This map must be linked because equality on the keys is not transitive,
      *       which can lead to different vnums assigned to the same method in two subsequent compilation sessions,
      *       which in turn leads to different code produced by compiler for the same input.
      */
    final private val vnumMap = mutable.LinkedHashMap.empty[Ref, Int]

    override def contains(x: Ref) = vnumMap.contains(comparable(x))

    override def apply(x: Ref) = vnumMap(comparable(x))

    override def refs = vnumMap.keySet.iterator

    override def update(x: Ref, vnum: Int): Unit = vnumMap.put(comparable(x), vnum)

    private def comparable(r: Ref): Ref = r match {
      case _: RefComparable => r
      case _ => ref(r.refClass, r.method)
    }
  }

  abstract class Layout(val size: Int, val inums: Array[Int]) {
    def vnum(x: Ref): Int
    def refs: Iterator[Ref]
  }

  def layout(_size: Int, vnumMap: VNumMap, inums0: Array[Int]): Layout = {
    val _inums = if (inums0 == null) EMPTY_INUMS else inums0

    new Layout(_size, _inums) {
      override def vnum(x: Ref): Int = if (vnumMap.contains(x)) vnumMap(x) else NO_VNUM

      override def refs: Iterator[Ref] = vnumMap.refs

      override def toString: String = {
        s"Layout($size, ${simpleClassName(vnumMap)}, ${inums.mkString("[", ", ", "]")})"
      }
    }
  }

  /** Builds trivial MT layout (for use in JIT).
    *
    * This version should be faster unlike a more advanced [[MethodTablesScala.buildMTLayout]].
    */
  def buildTrivialMTLayoutStraight(`type`: ClassType, typeProvider: TypeProvider): Layout = {
    implicit val tp = typeProvider
    buildTrivialMTLayout(`type`)
  }

  def buildTrivialMTLayout(`type`: ClassType)(implicit typeProvider: TypeProvider): Layout = {
    var size = 0
    var inums: Array[Int] = null
    val vnum = new VNumMapDefault

    val interfs = `type`.allSuperInterfaces.toArray

    val superclass = asClassType(`type`.getSuperClassSig) // TODO should propagate SignatureType?

    // init size and inums
    if (`type`.isInterface) {
      size = 0
      inums = null

    } else if (superclass == null) {
      assert(`type`.isHierarchyRoot)
      size = 0
      inums = new Array[Int](interfs.length)

    } else {
      val superLayout = superclass.getMTLayout

      // inherit size
      size = superLayout.size

      // inherit vnums
      for (ref <- superLayout.refs) {
        vnum.update(ref, superLayout.vnum(ref))
      }

      // inherit inums and expand with own superinterfaces
      val superINums = superLayout.inums
      inums = Array.copyOf(superINums, interfs.length)
      for (i <- superINums.length until inums.length) {
        inums(i) = size
        size += interfs(i).getMTLayout.size
      }
    }

    // assign vnum for declared refs
    for (m <- `type`.getDeclaredMethods if canBeInMethodTable(m)) {
      val r = ref(`type`, m)
      if (!vnum.contains(r)) {
        vnum.update(r, size)
        size += 1
      }
    }

    // assign vnum for interface refs
    for (interf <- interfs; m <- interf.getDeclaredMethods; if canBeInMethodTable(m)) {
      val r = ref(interf, m)
      if (!vnum.contains(r)) {
        vnum.update(r, size)
        size += 1
      }
    }

    layout(size, vnum, inums)
  }

  def buildMT(t: ClassType)(implicit typeProvider: TypeProvider): Array[FindMethodImplResult] = buildMT(t, t.findMethodImplementation : Ref => FindMethodImplResult)

  def buildMT[T >: Null : ClassTag](t: ClassType, f: Ref => T)(implicit typeProvider: TypeProvider): Array[T] = {
    buildMT(t, f, _.getMTLayout, null)
  }

  def buildMT[T >: Null : ClassTag](t: ClassType, f: Ref => T,
                                    makeLayout: ClassType => Layout, noValue: T)
                                   (implicit typeProvider: TypeProvider): Array[T] = {
    val size = makeLayout(t).size
    val mt = Array.fill(size)(noValue)
    buildFor(mt, SignatureType.fromSymType(t), 0, f, makeLayout, noValue)
    mt
  }

  def buildMT(t: ClassType, makeLayout: ClassType => Layout)(implicit typeProvider: TypeProvider): Array[FindMethodImplResult] = {
    buildMT(t, t.findMethodImplementation, makeLayout.apply, null)
  }

  private def buildFor[T](mt: Array[T], t: SignatureType, pos: Int, f: Ref => T,
                          makeLayout: ClassType => Layout, noValue: T)
                         (implicit typeProvider: TypeProvider): Unit = {
    val superclass = t.symType.getSuperClassSig
    if (superclass != null) {
      buildFor(mt, superclass, pos, f, makeLayout, noValue)
    }
    val layout = makeLayout(asClassType(t))

    val inums = layout.inums

    for ((interf, inum) <- asClassType(t).allSuperInterfaces zip inums if inum != NO_INUM) {
      buildFor(mt, SignatureType.fromSymType(interf), pos + inum, f, makeLayout, noValue)
    }

    for (r <- layout.refs) {
      val vnum = layout.vnum(r)
      if (vnum != NO_VNUM) {
        val slot = pos + vnum
        if (mt(slot) == noValue) {
          mt(slot) = f(r)
        }
      }
    }
  }

  def buildMTForUnitTests(t: ClassType, makeLayout: ClassType => Layout, typeProvider: TypeProvider): Array[FindMethodImplResult] = {
    implicit val tp = typeProvider
    buildMT(t, t.findMethodImplementation, makeLayout.apply, null)
  }
}
