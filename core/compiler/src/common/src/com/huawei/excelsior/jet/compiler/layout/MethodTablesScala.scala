/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.layout

import com.huawei.excelsior.jet.compiler.{Environment, TypeProvider}
import com.huawei.excelsior.jet.compiler.layout.MethodTables.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Type}

import scala.collection.mutable

/** Method tables layout builder.
  *
  * TODO: needs proper specification
  *
  * @author liontiger
  */
object MethodTablesScala {

  /** Note: This map must be linked because equality on the keys is not transitive,
    *       which can lead to different vnums assigned to the same method in two subsequent compilation sessions,
    *       which in turn leads to different code produced by compiler for the same input.
    */
  private[layout] class RefMap extends mutable.SeqMap[Ref, Int] with mutable.Growable[(Ref, Int)] with VNumMapBuilder {
    private val vnumMap = new VNumMapDefault

    override def refs: Iterator[Ref] = vnumMap.refs
    override def update(x: Ref, vnum: Int): Unit = vnumMap.update(x, vnum)

    override def addOne(elem: (Ref, Int)) = { update(elem._1, elem._2); this }
    override def get(key: Ref): Option[Int] = if (vnumMap.contains(key)) Some(vnumMap.apply(key)) else None
    override def iterator: Iterator[(Ref, Int)] = refs map { k => (k, vnumMap.apply(k)) }

    override def subtractOne(elem: MethodTables.Ref) = throw new UnsupportedOperationException
}

  /** Note: This set must be linked because equality on the keys is not transitive,
    *       which can lead to different vnums assigned to the same method in two subsequent compilation sessions,
    *       which in turn leads to different code produced by compiler for the same input.
    */
  private[layout] class RefSet extends mutable.Set[Ref] {
    private val vnumMap = new VNumMapDefault

    override def addOne(elem:  Ref): RefSet.this.type = { vnumMap.update(elem, 0); this }
    override def contains(elem: Ref): Boolean = vnumMap.contains(elem)
    override def iterator: Iterator[Ref] = vnumMap.refs

    override def subtractOne(elem: Ref) = throw new UnsupportedOperationException
    override def clear(): Unit = throw new UnsupportedOperationException
}

  def buildMTLayout(t: Type, env: Environment): Layout =
    implicit val tp = env.getTypeProvider
    if (t.isThinClass) {
      // TODO REMOVE THIS BRANCH
      buildThinVMTLayout(asClassType(t))
    } else if (t.isClass) {
      buildVMTLayout(asClassType(t))
    } else if (t.isInterface) {
      buildIMTLayout(asClassType(t))
    } else if (t.isArray) {
      buildVMTLayout(tp.getObjectType)
    } else {
      // empty layout for records (called from Numerate)
      assert(t.isRecord)
      layout(0, new RefMap, Array.empty)
    }

  def buildVMTLayout(c: ClassType)(implicit typeprovider: TypeProvider): Layout = {
    assert(c.isClass)

    var size = 0
    val vnum = new RefMap
    val inums = mutable.ArrayBuilder.make[Int]

    // inherit superclass layout
    val superclass = asClassType(c.getSuperClassSig)
    var superINumSize = 0
    if (superclass != null) {
      val superLayout = superclass.getMTLayout
      size += superLayout.size
      vnum ++= superLayout.refs map (x => (x, superLayout.vnum(x)))
      val superINums = superLayout.inums
      superINumSize = superINums.length
      inums ++= superINums
    }

    // collect newly introduced interfaces
    // Note: (`allInterfs(c)` contains `allInterfs(superclass)` as first `inum.size` elements)
    val interfs = allInterfs(c).drop(superINumSize).toSeq

    def current: Int = {
      val cur = size
      size += 1
      cur
    }

    def addRef(x: Ref): Unit = {
      // Note: refs to final methods can always be called directly, so they do not require a slot in VMT
      //       (except when same ref was already present in superclass).
      //       Such refs, however, are also added to RefMap with `NO_VNUM` value
      //       to ensure that the same refs from superinterfaces and subclasses will not get a slot in VMT.
      def nextVNum: Int = if (x.method.isFinal) NO_VNUM else current
      vnum.getOrElseUpdate(x, nextVNum)
    }

    // add declared refs and refs from superinterfaces without final ones
    // Note: should not add ref if it is already present in map.
    //       This is important to ensure that final methods from `c` will stay in map
    //       so that they can be processed later.
    for (x <- declaredRefs(c, c) ++ interfs.iterator.flatMap(declaredRefs(_, c))) {
      addRef(x)
    }

    // add interface method tables
    for (i <- interfs) {
      inums += size
      size += i.getMTLayout.size
    }

    layout(size, vnum, inums.result())
  }

  def buildThinVMTLayout(c: ClassType)(implicit typeProvider: TypeProvider): Layout = {
    assert(c.isThinClass)

    var size = 0
    val vnum = new RefMap

    if (!c.isPolyThinClass) {
      // empty layout for non-PolyThin classes
      return layout(size, vnum, Array.empty)
    }

    // inherit superclass layout
    val superclass = asClassType(c.getSuperClassSig)
    if (superclass != null) {
      val superLayout = superclass.getMTLayout
      size += superLayout.size
      vnum ++= superLayout.refs map (x => (x, superLayout.vnum(x)))
      assert(superLayout.inums.isEmpty)
    }

    def current: Int = {
      val cur = size
      size += 1
      cur
    }

    def addRef(x: Ref): Unit = {
      // Note: refs to final methods can always be called directly, so they do not require a slot in VMT
      //       (except when same ref was already present in superclass).
      //       Such refs, however, are also added to RefMap with `NO_VNUM` value
      //       to ensure that the same refs from superinterfaces and subclasses will not get a slot in VMT.
      def nextVNum: Int = if (x.method.isFinal) NO_VNUM else current
      vnum.getOrElseUpdate(x, nextVNum)
    }

    for (x <- declaredRefs(c, c)) {
      addRef(x)
    }

    layout(size, vnum, Array.empty)
  }

  def buildIMTLayout(i: ClassType): Layout = {
    assert(i.isInterface)

    var size = 0
    val vnum = new RefMap

    def current: Int = {
      val cur = size
      size += 1
      cur
    }

    // iterator over declared refs and refs from superinterfaces
    val newRefsIterator = declaredRefs(i, i) ++ allInterfs(i).flatMap(declaredRefs(_, i))

    // add declared refs and refs from superinterfaces
    for (x <- newRefsIterator) {
      vnum.getOrElseUpdate(x, current)
    }

    layout(size, vnum, Array.empty)
  }

  private[layout] def allInterfs(t: ClassType) = t.allSuperInterfaces.iterator
  private[layout] def declaredRefs(t: ClassType, scope: ClassType) = {
    // Note: when creating a ref to a method `m` with non-interface declaring class `c`, `refClass` must always be `c`,
    //       because if `m` had package-private access and `scope` was from different package than `c`,
    //       then the pair `(scope, m)` would represent a ref from incorrect package (not the one where `m` was defined).
    val refClass = if (t.isInterface) scope else t
    t.getDeclaredMethods filter canBeInMethodTable map (MethodTables.ref(refClass,_))
  }
}
