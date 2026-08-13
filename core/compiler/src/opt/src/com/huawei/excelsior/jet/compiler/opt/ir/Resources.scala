/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.common.CodeHelpers.{shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.Location.MemBased
import com.huawei.excelsior.jet.compiler.Env.{addressSize, isStandalone, stackPointer, stackSlotSize, targetArch}
import com.huawei.excelsior.jet.compiler.debug.info.DebugLocalVar
import com.huawei.excelsior.jet.compiler.layout.FieldsLayout
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.{Environment, TypeProvider, symlevel}
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, Type}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import xscala.util.simpleClassName

import scala.collection.{immutable, mutable}
import java.lang.Long.{bitCount, lowestOneBit, numberOfTrailingZeros as ntz}

/**
 * Resource is something, that could be occupied or spoiled by a node, when it is processed.
 * Resources are used to express relations between nodes, that could not be expressed in SSA-form.
 * Life ranges of nodes, that occupied or spoiled the same resources, could not be intersected. That means,
 * that one of this node is anti-dependent from usages of other node.
 *
 * @author conwor
 * @author paul
 */
object Resources extends ImplicitSetsAndMaps {

  // TODO: inline typedef after commit in trunk
  type Resource = Location

  /** Location interface implementation for opt specific resources - immediate, flags, frame slots. */
  trait CustomLocation { self: Location.Other =>
    def width = shouldNotCallThis()
  }

  /** Immediate is enumerable resource, that occupied by each constant node. */
  case object Immediate extends Location.Other with CustomLocation

  val InvalidResource = Location.INVALID

  var frameSlotID = 0

  /** FrameSlot is unique resource, parametrized by addressing mode. */
  class FrameSlot(val kind: FrameSlot.Kind, var _offsetFromSP: Int = -1) extends Location.Other with CustomLocation {
    def offsetFromSP: Int = _offsetFromSP

    def mem: MemBased = Location.mem(stackPointer, offsetFromSP ensuring { x => x != -1 })

    def size = kind.size
    def align = kind.alignment
    def zeroed(implicit env: Environment) = kind.zeroed
    def tracedByHeader = kind.tracedByHeader

    def index: Int = shouldNotCallThis("unimplemented")
    
    private val id = frameSlotID
    frameSlotID += 1
    override def toString = s"FrameSlot#${id}{size:$size,align:$align,tracedByHeader:$tracedByHeader}"
  }

  object FrameSlot {

    /** Description of frame slot properties. */
    sealed abstract class Kind {

      /** Size of allocated stack space. */
      def size: Int

      /** Alignment of address of allocated stack space. */
      def alignment: Int

      /** Indicates that stack allocated stack space must be zeroed. */
      final def zeroed(implicit env: Environment) = this match {
        case FrameSlot.Local(t, workaroundForNonZeroedTraceableRecords)
          if t.isRecord && t.hasRefFields(env.getTypeProvider) && !env.enabled(BoolOption.StackAllocZeroingForValueTypes) =>
          // TODO: remove this option and workarounds when zeroValue<T> is reworked in Cangjie (see JET-15124).
          // Also see JET-15875.
          workaroundForNonZeroedTraceableRecords
        case _ => zeroedImpl
      }

      protected def zeroedImpl: Boolean

      /** Indicates that stack allocated memory should be traced by GC as a whole object:
        * with help of the information in its header (the very first slot on the stack).
        *
        * TODO: remove tracedByHeader completely when there will be no dirty frames even for Class GC
        */
      def tracedByHeader: Boolean

      /** Indicates that stack allocated memory contains traced references. */
      def traced: Boolean
    }

    case class Raw(size: Int, alignment: Int) extends Kind {
      protected def zeroedImpl = false
      def tracedByHeader = false
      def traced = false
    }

    sealed abstract class AnyNewOnStack(allocType: SignatureType)(implicit typeProvider: TypeProvider) extends Kind {
      require(allocType.isTraceableReference)

      def alignment = allocType.symType.getObjectAlignment
      protected def zeroedImpl = true
      def tracedByHeader = true
      def traced = true
    }

    case class NewOnStack(allocType: SignatureType)(implicit typeProvider: TypeProvider) extends AnyNewOnStack(allocType) {
      def size = allocType.symType.getHeapObjectSize
    }

    case class NewArrayOnStack(allocType: SignatureType, length: Int)(implicit typeProvider: TypeProvider) extends AnyNewOnStack(allocType) {
      require(allocType.isArray)
      def size = Math.toIntExact(allocType.symType.getArrayObjectSize(length, true))
    }

    sealed abstract class Typed(implicit typeProvider: TypeProvider) extends Kind {
      def allocType: SignatureType

      def symType = allocType.symType

      def size = if (allocType.isPrimitive) {
        if (allocType.isZST) {
          // VOID-typed stack alloc created for debug info generation purposes.
          // For more details look at CangjieLLVMIRParser.allocOneZST.
          0
        } else {
          symType.size
        }
      } else if (allocType.isThinClass) {
        symType.getRawObjectSize
      } else if (allocType.isRecord) {
        if (targetArch == CBC) 0 else symType.getRawObjectSize
      } else {
        assert(allocType.isTraceableReference)
        addressSize
      }

      def alignment = if (allocType.isPrimitive) {
        if (allocType.isZST) {
          // VOID-typed stack alloc created for debug info generation purposes.
          // For more details look at CangjieLLVMIRParser.allocOneZST.
          1
        } else {
          symType.size
        }
      } else if (allocType.isRecord) {
        if (targetArch == CBC) 0 else symType.getObjectAlignment
      } else if (allocType.isInstanceOf[SignatureType.Box]) {
        addressSize
      } else {
        symType.getObjectAlignment
      }

      protected def zeroedImpl = traced
      def tracedByHeader = false
      def traced = allocType.isTraceableReference || (allocType.isRecord && allocType.hasRefFields) || (isStandalone && allocType.isTypeVariable)
    }

    object Typed {
      def unapply(typed: Typed): Option[SignatureType] = Some(typed.allocType)
    }

    // Details about dirty workaround cause - JET-15875.
    // TODO: remove workaround when zeroValue<T> is reworked in Cangjie (see JET-15124 for details).
    case class Local(allocType: SignatureType, workaroundForNonZeroedTraceableRecords: Boolean = false)(implicit typeProvider: TypeProvider) extends Typed {
      override def toString = s"Local($allocType)"
    }

    case class DebugVar(allocType: SignatureType, info: DebugLocalVar)(implicit typeProvider: TypeProvider) extends Typed {
      override def size = if (allocType.isThinClass) {
        // for debug var let's use addr-size slot to keep a var of Thin type
        addressSize
      } else {
        super.size
      }
    }

    sealed abstract class Param extends Kind {
      def size = stackSlotSize
      def alignment = stackSlotSize
      protected def zeroedImpl = false
      def tracedByHeader = false
      def traced = false
    }

    object CallParam extends Param
    object ThisMethodParam extends Param

    /** Memory for variable-sized type in Universal Generic code. */
    case class OffHeapMemory(allocType: SignatureType)(implicit typeProvider: TypeProvider) extends Kind {
      assert(allocType.isVariableSizeType)
      
      def size = stackSlotSize
      def alignment = stackSlotSize
      protected def zeroedImpl = false
      def tracedByHeader = false
      def traced = false
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Sets of resources

  private[ir] class CacheEntry(val r: Resource, val idx: Int, val singleton: ResourceSet)
  private val cache = mutable.Map.empty[Resource, CacheEntry]
  private val cacheIndex = new Array[CacheEntry](64)
  private var firstFreeBit = 0
  private var cacheFrozen = false
  private var cacheFilled = false

  private def findNextFreeBit(from: Int): Int = {
    var i = from
    while (i < cacheIndex.length && cacheIndex(i) != null) i += 1
    if (i == cacheIndex.length) -1 else i
  }

  private[ir] def addToCache(r: Resource, idx0: Int = Int.MinValue): CacheEntry = {
    assert(!cacheFrozen)
    val idx = if (idx0 == Int.MinValue) firstFreeBit else idx0
    val entry = new CacheEntry(r, idx, (new ResourceSet)._rawSingleton(r, idx))
    val was = cache.put(r, entry)
    assert(was.isEmpty)
    if (idx >= 0) {
      assert(cacheIndex(idx) == null)
      cacheIndex(idx) = entry
      if (idx == firstFreeBit) {
        firstFreeBit = findNextFreeBit(idx + 1)
      }
    }
    entry
  }

  def initializeCache(rs: IterableOnce[Resource]): Unit = {
    assert(!cacheFilled && !cacheFrozen)
    for (r <- rs.iterator) addToCache(r)
    cacheFrozen = true
    cacheFilled = true
  }

  def cacheInitialized = cacheFilled

  private def getCacheEntry(r: Resource): CacheEntry = {
    cacheFrozen = true
    cache.get(r).orNull
  }

  private def getCacheEntry(idx: Int): CacheEntry = {
    cacheFrozen = true
    if (idx < 0) null else cacheIndex(idx)
  }

  private def toBitIndex(r: Resource): Int = {
    val e = getCacheEntry(r)
    if (e ne null) e.idx else -1
  }

  /** Marker for special 'resource universe' set which represents all possible resources. */
  private val UniversalSetMarker = new Object { def dummy = 0 }
  private type RefsSet = mutable.Set[Resource]
  private val constTrue: Any => Boolean = { _ => true }

  abstract class AnyResourceSet {
    private[Resources] var bits: Long = 0L
    private[Resources] var refs: AnyRef = _ // contains: null | Resource | RefsSet | UniversalSetMarker

    private[Resources] def _assign(bits: Long, refs: AnyRef): this.type = {
      this.bits = bits
      this.refs = refs
      this
    }

    private[Resources] def _rawSingleton(r: Resource, idx: Int): this.type = {
      if (idx >= 0) _assign(1L << idx, null)
      else _assign(0L, r)
    }

    type ThisType <: AnyResourceSet

    private def isMutable: Boolean = this.isInstanceOf[MutableResourceSet]

    protected def cloneRefs(doClone: Boolean = true): AnyRef =
      if (doClone) normalizedRefs(refs, cloneRS = true) else refs

    private def forkRefs(inPlace: Boolean): AnyRef = cloneRefs(!inPlace && isMutable)

    private def refsAppend(r: Resource, inPlace: Boolean): AnyRef = refs match {
      case null => r
      case r0: Resource =>
        if (r == r0) refs else Sets[Resource].newQSet += r0 += r
      case _s: mutable.Set[_] =>
        val s = _s.asInstanceOf[RefsSet]
        if (s contains r) {
          forkRefs(inPlace)
        } else {
          (if (inPlace) s else s.clone()).addOne(r)
        }
    }

    protected def refsAppend(rs: AnyResourceSet, inPlace: Boolean): AnyRef = {
      if (refs eq rs.refs) forkRefs(inPlace)
      else if (isUniverse || rs.isUniverse) UniversalSetMarker
      else rs.refs match {
        case null => forkRefs(inPlace)
        case r: Resource => refsAppend(r, inPlace)
        case _sy: mutable.Set[_] =>
          val sy = _sy.asInstanceOf[RefsSet]
          this.refs match {
            case null =>
              rs.cloneRefs(inPlace || isMutable || rs.isMutable)
            case r: Resource =>
              Sets[Resource].newQSet += r ++= sy
            case _sx: mutable.Set[_] =>
              val sx = _sx.asInstanceOf[RefsSet]
              val result = if (inPlace) sx else sx.clone()
              result ++= sy
              result
          }
      }
    }

    private def refsRemove(r: Resource, inPlace: Boolean): AnyRef = refs match {
      case null => refs
      case r0: Resource =>
        if (r != r0) refs else null
      case _s: mutable.Set[_] =>
        val s = _s.asInstanceOf[RefsSet]
        if (!(s contains r)) {
          forkRefs(inPlace)
        } else {
          (if (inPlace) s else s.clone()).subtractOne(r)
        }
    }

    protected def refsRemove(rs: AnyResourceSet, inPlace: Boolean): AnyRef = {
      assert(!isUniverse)
      if (refs eq rs.refs) null
      else if (rs.isUniverse) null
      else rs.refs match {
        case null => forkRefs(inPlace)
        case r: Resource => refsRemove(r, inPlace)
        case _s: mutable.Set[_] =>
          val s = _s.asInstanceOf[RefsSet]
          refsFilter(!s(_), inPlace)
      }
    }

    private def refsContain(r: Resource): Boolean = refs match {
      case null => false
      case UniversalSetMarker => true
      case r0: Resource => r == r0
      case _s: mutable.Set[_] => _s.asInstanceOf[RefsSet] contains r
    }

    protected def refsFilter(pred: Resource => Boolean, inPlace: Boolean): AnyRef = {
      assert(!isUniverse)
      refs match {
        case null => null
        case r: Resource =>
          if (pred(r)) r else null
        case _s: mutable.Set[_] =>
          val s = _s.asInstanceOf[RefsSet]
          if (inPlace) s filterInPlace pred else s filter pred
      }
    }

    protected def refsRetain(rs: AnyResourceSet, inPlace: Boolean): AnyRef = {
      if (refs eq rs.refs) forkRefs(inPlace)
      else if (rs.isUniverse) forkRefs(inPlace)
      else if (isUniverse)
        rs.cloneRefs(inPlace || isMutable || rs.isMutable)
      else rs.refs match {
        case null => null
        case r: Resource => if (this.refsContain(r)) r else null
        case _s: mutable.Set[_] =>
          refsFilter(_s.asInstanceOf[RefsSet], inPlace)
      }
    }

    private def refsIsEmpty: Boolean = refs match {
      case null => true
      case s: mutable.Set[_] => s.isEmpty
      case _ => false
    }

    private def refsSize: Int = refs match {
      case null => 0
      case r: Resource => 1
      case UniversalSetMarker => shouldNotReachHere()
      case s: mutable.Set[_] => s.size
    }

    private def refsFindOrNull(pred: Resource => Boolean): Resource = refs match {
      case null => null
      case r: Resource => if (pred(r)) r else null
      case UniversalSetMarker => shouldNotReachHere()
      case _s: mutable.Set[_] =>
        _s.asInstanceOf[RefsSet].find(pred).orNull
    }

    private def refsDisjoint(rs: AnyResourceSet): Boolean = {
      if ((refs eq null) || (rs.refs eq null)) true
      else rs.refs match {
        case r: Resource => !refsContain(r)
        case UniversalSetMarker => refsIsEmpty
        case _s: mutable.Set[_] =>
          val s = _s.asInstanceOf[RefsSet]
          s.isEmpty || (!isUniverse && (refsFindOrNull(s) eq null))
      }
    }

    private def refsSubsetOf(rs: AnyResourceSet): Boolean = {
      if (refs eq rs.refs) true
      else if (rs.isUniverse) true
      else refs match {
        case null => true
        case r: Resource => rs.refsContain(r)
        case UniversalSetMarker => false
        case _s: mutable.Set[_] =>
          _s.asInstanceOf[RefsSet] forall rs.refsContain
      }
    }

    private def findOrNull(pred: Resource => Boolean): Resource = {
      assert(!isUniverse)
      var rest = this.bits
      var idx = -1
      while (rest != 0L) {
        val shift = if ((rest & 1) != 0L) 0 else ntz(rest)
        rest = rest >>> shift >>> 1
        idx += shift + 1
        val r = getCacheEntry(idx).r
        if (pred(r)) return r
      }
      refsFindOrNull(pred)
    }

    protected def bitsFilter(pred: Resource => Boolean): Long = {
      assert(!isUniverse)
      var result = 0L
      var rest = this.bits
      var idx = -1
      while (rest != 0L) {
        val shift = if ((rest & 1) != 0L) 0 else ntz(rest)
        rest = rest >>> shift >>> 1
        idx += shift + 1
        val r = getCacheEntry(idx).r
        if (pred(r)) result |= (1L << idx)
      }
      result
    }

    private[Resources] def _incl(r: Resource): this.type = {
      assert(!isUniverse)
      val idx = toBitIndex(r)
      if (idx >= 0) {
        bits |= (1L << idx)
      } else {
        refs = refsAppend(r, inPlace = true)
      }
      this
    }

    private[Resources] def _excl(r: Resource): this.type = {
      assert(!isUniverse)
      val idx = toBitIndex(r)
      if (idx >= 0) {
        bits &= ~(1L << idx)
      } else {
        refs = refsRemove(r, inPlace = true)
      }
      this
    }

    private[Resources] def _incl(rs: IterableOnce[Resource]): this.type = {
      val it = rs.iterator
      while (it.hasNext) _incl(it.next())
      this
    }

    private[Resources] def _excl(rs: IterableOnce[Resource]): this.type = {
      val it = rs.iterator
      while (it.hasNext) _excl(it.next())
      this
    }

    protected def makeResult(newBits: Long, newRefs: AnyRef): ThisType

    protected def makeResult(mset: MutableResourceSet): ThisType

    def mutableClone(): MutableResourceSet = (new MutableResourceSet)._assign(bits, cloneRefs())

    def apply(r: Resource): Boolean = contains(r)
    def contains(r: Resource): Boolean = {
      val idx = toBitIndex(r)
      if (idx >= 0) (bits & (1L << idx)) != 0L
      else refsContain(r)
    }

    def isEmpty = bits == 0L && refsIsEmpty
    def nonEmpty = !isEmpty
    def size = bitCount(bits) + refsSize

    // PS: conwor said chances I will fail here 1 in a 100000000 and it shouldn't happen more than 1 time per 10 years,
    // thus we don't need assert anywhere near this code.
    // Current counter of fails, starting from October 2024: 9 (including still open JET-15742)
    def head = findOrNull(constTrue).nn
    def headOption = find(constTrue)

    def foreach[U](f: Resource => U): Unit = { findOrNull{ r => f(r); false } }

    def iterator: Iterator[Resource] = new Iterator[Resource] {
      assert(!isUniverse)
      var rest = bits
      var idx = -1
      var r1: Resource = _
      val setIter: Iterator[Resource] = refs match {
        case null => Iterator.empty
        case r: Resource => r1 = r; Iterator.empty
        case s: mutable.Set[_] => s.asInstanceOf[RefsSet].iterator
      }

      def hasNext = (rest != 0L) || (r1 ne null) || setIter.hasNext
      def next() = {
        assert(hasNext)
        if (rest != 0L) {
          val shift = if ((rest & 1) != 0L) 0 else ntz(rest)
          rest = rest >>> shift >>> 1
          idx += shift + 1
          getCacheEntry(idx).r
        } else if (r1 ne null) {
          val r = r1; r1 = null; r
        } else setIter.next()
      }
    }

    def find(pred: Resource => Boolean): Option[Resource] = Option(findOrNull(pred))
    def exists(pred: Resource => Boolean): Boolean = findOrNull(pred) ne null
    def forall(pred: Resource => Boolean): Boolean = !exists(!pred(_))

    def disjointWith(rs: AnyResourceSet): Boolean = ((bits & rs.bits) == 0L) && refsDisjoint(rs)
    def subsetOf(rs: AnyResourceSet): Boolean = ((bits & ~rs.bits) == 0L) && refsSubsetOf(rs)

    def + (r: Resource): ThisType = {
      if (isUniverse) return this.asInstanceOf[ThisType]
      val idx = toBitIndex(r)
      if (idx >= 0) {
        makeResult(bits | (1L << idx), forkRefs(inPlace = false))
      } else {
        makeResult(bits, refsAppend(r, inPlace = false))
      }
    }

    def - (r: Resource): ThisType = {
      assert(!isUniverse)
      val idx = toBitIndex(r)
      if (idx >= 0) {
        makeResult(bits & ~(1L << idx), forkRefs(inPlace = false))
      } else {
        makeResult(bits, refsRemove(r, inPlace = false))
      }
    }

    def ++ (rs: IterableOnce[Resource]): ThisType = {
      val it = rs.iterator
      if (!it.hasNext) return makeResult(bits, refs)
      val r1 = it.next()
      if (!it.hasNext) return this + r1
      makeResult(mutableClone() += r1 ++= it)
    }

    def -- (rs: IterableOnce[Resource]): ThisType = {
      val it = rs.iterator
      if (!it.hasNext) return makeResult(bits, refs)
      val r1 = it.next()
      if (!it.hasNext) return this - r1
      makeResult(mutableClone() -= r1 --= it)
    }

    def | (rs: AnyResourceSet): ThisType =
      makeResult(bits | rs.bits, refsAppend(rs, inPlace = false))

    def & (rs: AnyResourceSet): ThisType =
      makeResult(bits & rs.bits, refsRetain(rs, inPlace = false))

    def &~ (rs: AnyResourceSet): ThisType =
      makeResult(bits & ~rs.bits, refsRemove(rs, inPlace = false))

    def filter(pred: Resource => Boolean): ThisType =
      makeResult(bitsFilter(pred), refsFilter(pred, inPlace = false))

    def isUniverse: Boolean = refs eq UniversalSetMarker

    def isSingleton: Boolean = (bits == lowestOneBit(bits)) && {
      if (bits == 0L) refsSize == 1 else refsIsEmpty
    }

    def single: Resource = { assert(isSingleton); head }

    def count(p: Resource => Boolean) = iterator count p

    def asImmutable: ResourceSet

    def asSeq: Seq[Resource] = immutable.ArraySeq.from(iterator)

    override def equals(that: Any): Boolean = that match {
      case rs: AnyResourceSet =>
        (this eq rs) || {
          bits == rs.bits &&
            normalizedRefs(refs) == normalizedRefs(rs.refs)
        }
      case _ => false
    }

    override def hashCode(): Int = bits.## ^ normalizedRefs(refs).##

    override def toString = s"${simpleClassName(this)}(bits: ${bits.toHexString}, refs: $refs)"
  }

  private def normalizedRefs(refs: AnyRef, cloneRS: Boolean = false): AnyRef = refs match {
    case s0: mutable.Set[_] =>
      val s = s0.asInstanceOf[RefsSet]
      if (s.isEmpty) null
      else if (s.size == 1) s.head
      else if (cloneRS) s.clone()
      else s
    case _ => refs
  }

  private def normalizedSet(bits: Long, refs0: AnyRef, origin: ResourceSet): ResourceSet = {
    val refs = normalizedRefs(refs0)
    if ((refs eq null) && (bits == lowestOneBit(bits))) {
      if (bits == 0L) emptySet else getCacheEntry(ntz(bits)).singleton
    } else if ((origin ne null) && (bits == origin.bits) && (refs eq origin.refs)) {
      origin
    } else {
      (new ResourceSet)._assign(bits, refs)
    }
  }

  final class ResourceSet private[Resources]() extends AnyResourceSet {
    type ThisType = ResourceSet

    protected def makeResult(newBits: Long, newRefs: AnyRef): ResourceSet = {
      normalizedSet(newBits, newRefs, this)
    }

    protected def makeResult(mset: MutableResourceSet): ResourceSet = makeResult(mset.bits, mset.refs)

    def asImmutable: ResourceSet = this
  }

  final class MutableResourceSet private[Resources]() extends AnyResourceSet {
    type ThisType = MutableResourceSet

    protected def makeResult(newBits: Long, newRefs: AnyRef): MutableResourceSet = {
      (new MutableResourceSet)._assign(newBits, newRefs)
    }

    protected def makeResult(mset: MutableResourceSet): MutableResourceSet = mset

    /** Convert this MSet to immutable set and make this set empty. */
    def moveToImmutable: ResourceSet = {
      val bits = this.bits
      val refs = this.refs
      _assign(0L, null)
      normalizedSet(bits, refs, null)
    }

    def asImmutable: ResourceSet = normalizedSet(bits, cloneRefs(), null)

    override def clone(): MutableResourceSet = mutableClone()

    def += (r: Resource): this.type = _incl(r)
    def -= (r: Resource): this.type = _excl(r)

    def ++= (rs: IterableOnce[Resource]): this.type = _incl(rs)
    def --= (rs: IterableOnce[Resource]): this.type = _excl(rs)

    def |= (rs: AnyResourceSet): this.type =
      _assign(bits | rs.bits, refsAppend(rs, inPlace = true))

    def &= (rs: AnyResourceSet): this.type =
      _assign(bits & rs.bits, refsRetain(rs, inPlace = true))

    def &~= (rs: AnyResourceSet): this.type =
      _assign(bits & ~rs.bits, refsRemove(rs, inPlace = true))

    def filterInPlace(pred: Resource => Boolean): this.type =
      _assign(bitsFilter(pred), refsFilter(pred, inPlace = true))

    def update(r: Resource, included: Boolean): Unit =
      if (included) this += r else this -= r
  }

  /** @return immutable set of resources, contains only given resource `r` */
  def setOf(r: Resource): ResourceSet = {
    val cached = getCacheEntry(r)
    if (cached ne null) cached.singleton else (new ResourceSet)._incl(r)
  }

  /** @return immutable set of resources, contains only given resource `r` */
  def setOf(rs: Resource*): ResourceSet = setOf(rs)

  /** @return immutable set of resources, contains all resources from given collection `rs` */
  def setOf(rs: IterableOnce[Resource]): ResourceSet = {
    val it = rs.iterator
    if (!it.hasNext) return emptySet

    val r1 = it.next()
    if (!it.hasNext) return setOf(r1)

    val s = (new ResourceSet)._incl(r1)._incl(it)
    normalizedSet(s.bits, s.refs, s)
  }

  def unionOf(xs: IterableOnce[AnyResourceSet]): ResourceSet = {
    val it = xs.iterator
    if (!it.hasNext) return emptySet

    val s1 = it.next()
    if (!it.hasNext) return s1.asImmutable

    val res = s1.mutableClone()
    while (it.hasNext) res |= it.next()
    res.moveToImmutable
  }

  /** @return empty mutable set of resources */
  def emptyMSet(): MutableResourceSet = new MutableResourceSet
  def mutableSetOf(rs: IterableOnce[Resource]): MutableResourceSet = emptyMSet() ++= rs

  /** Special set which represents all possible resources. */
  val universalSet = (new ResourceSet)._assign(-1L, UniversalSetMarker)
  val emptySet     = new ResourceSet
  val immSet       = addToCache(Immediate, 63).singleton
  val invalidSet   = addToCache(InvalidResource, -1).singleton
}
