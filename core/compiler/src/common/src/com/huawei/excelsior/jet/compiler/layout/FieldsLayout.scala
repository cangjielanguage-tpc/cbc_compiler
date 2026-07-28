/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.layout

import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.jet.compiler.{Env, TypeProvider}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Field, TypeKind}
import com.huawei.excelsior.jet.util.Worklist
import xscala.util.MathUtils.alignUp

import scala.collection.mutable.ArrayBuffer

/** Fields layout implementation.
  *
  * The algorithm arranges fields to minimize two factors:
  * 1) total size of layout
  * 2) permutations from original order
  *
  * The main idea is to place fields in input order as long as they are aligned
  * and rollback the tail when gap occurred to a minimal gap possible.
  *
  * @author ikireev
  */
object FieldsLayout {
  case class FieldOffs(field: Field, offs: Int)

  def instanceFieldsLayout(tpe: ClassType)(implicit typeProvider: TypeProvider): collection.Seq[FieldOffs] = {
    assert(Env.targetArch != Arch.CBC)
    val instanceFields = tpe.getDeclaredFields filterNot (_.isStatic)

    if (instanceFields.isEmpty) {
      return Seq.empty
    }

    val offs = if (tpe.isRecord) {
      0
    } else {
      val superClassSig = tpe.getSuperClassSig
      if (superClassSig != null) {
        superClassSig.getRawObjectSize
      } else {
        tpe.getObjectHeaderSize
      }
    }

    makeFieldsLayout(instanceFields, offs, tpe.hasSequentialLayout)
  }

  def staticFieldsLayout(tpe: ClassType)(implicit typeProvider: TypeProvider): collection.Seq[FieldOffs] = {
    val staticFields = Worklist.from(
      tpe.getDeclaredFields filter (f => f.isStatic && (!f.isAJFlat || f.getType.isRecord))
    )

    if (staticFields.isEmpty) {
      return Seq.empty
    }

    val layout = ArrayBuffer.empty[FieldOffs]
    var offs = 0 // initially static bundle size is 0

    // First, place GC-traced pointer fields
    offs = addFieldsToLayout(staticFields, layout, offs, _.getType.isTraceableReference)

    // Then, place non-GC-traced static fields except records
    offs = addFieldsToLayout(staticFields, layout, offs, !_.getType.isRecord)

    // Finally, place records
    assert(staticFields.iterator forall (_.getType.isRecord))
    layout ++= makeFieldsLayout(staticFields.iterator, offs, tpe.isAJManagedType || tpe.isCangjieType) // TODO: why sequential layout?
  }

  def fieldOffsets(tpe: ClassType, fieldsNum: Int)(implicit typeProvider: TypeProvider): Array[Int] = {
    val arr = new Array[Int](fieldsNum)
    for (FieldOffs(f, offs) <- instanceFieldsLayout(tpe) ++ staticFieldsLayout(tpe)) {
      arr(f.getFieldIndex) = offs
    }
    arr
  }

  private def addFieldsToLayout(staticFields: Worklist[Field], layout: ArrayBuffer[FieldOffs], _offs: Int, matcher: Field => Boolean) = {
    var offs = _offs
    val skipped = Worklist.empty[Field]
    for (f <- staticFields.drain) {
      if (matcher(f)) {
        offs = alignUp(offs, f.alignment)
        layout += FieldOffs(f, offs)
        offs += f.size
      } else {
        skipped += f
      }
    }
    staticFields swap skipped
    offs
  }

  private def makeFieldsLayout(fields: IterableOnce[Field], startOffs: Int, isSequentialLayout: Boolean)(implicit typeProvider: TypeProvider) =
    new FieldsLayout.Builder(fields, startOffs, isSequentialLayout).getLayout

  def getRefFieldOffsets(tpe: ClassType)(implicit typeProvider: TypeProvider): Array[Int] = getRefFieldOffsets(tpe, 0).toArray

  def getRefFieldOffsets(tpe: ClassType, base: Int)(implicit typeProvider: TypeProvider): collection.Seq[Int] = {
    val refFields = ArrayBuffer.empty[Field]
    val refFieldsOffsets = ArrayBuffer.empty[Int]
    var c = tpe
    assert(!c.hasDeferredSuper)
    while (c != null) {
      for (field <- c.getDeclaredFields) {
        assert(!field.getType.hasDeferredSuper)
        if (!field.isStatic) {
          val fieldType = field.getType
          if (fieldType.isRecord) {
            refFieldsOffsets ++= getRefFieldOffsets(asClassType(fieldType), field.getInstanceFieldOffset)
          } else if (fieldType.isTraceableReference) {
            refFields += field
          }
        }
      }
      c = asClassType(c.getSuperClassSig)
    }
    refFieldsOffsets ++= refFields.map(_.getInstanceFieldOffset)
    refFieldsOffsets.sortInPlace()
    refFieldsOffsets.map(_ + base)
  }

  private class Builder(fields: IterableOnce[Field], startOffs: Int, isSequentialLayout: Boolean)(implicit typeProvider: TypeProvider) {
    private val done = ArrayBuffer.empty[FieldOffs]
    private val buffer = ArrayBuffer.empty[Field] // temporary place for fields, which cause misalignment of next fields
    private val worklist = ArrayBuffer.from(fields)
    private val maxAlignment = TypeKind.LONG.alignment

    private var offs = startOffs

    if (isSequentialLayout) {
      buildSequentialLayout()
    } else {
      buildLayout()
    }

    def getLayout = done

    private def buildSequentialLayout(): Unit = {
      while (worklist.nonEmpty) {
        val field = worklist(0)

        val gap = calcGap(offs, field.alignment)
        offs += gap

        done += FieldOffs(field, offs)
        offs += field.size

        worklist.remove(0)
      }
    }

    private def buildLayout(): Unit = {
      var index = 0
      var minGap = maxAlignment // minimal gap from current offs to fields from worklist
      var minGapIndex = 0
      var minAlignment = maxAlignment // minimal field size of fields from worklist

      while (worklist.nonEmpty || buffer.nonEmpty) {
        var allowGap = false
        if (index == worklist.size) { // cannot find aligned field for current offs
          allowGap = !rollbackToMinimizeGap(minGap, minGapIndex, minAlignment)
          index = 0
        }

        val field = worklist(index)

        val gap = calcGap(offs, field.alignment)
        val isAligned = gap == 0
        if (isAligned || allowGap) {
          offs += gap
          done += FieldOffs(field, offs)
          worklist.remove(index)

          // restore hidden elements, that was rolled back, if any
          if (buffer.nonEmpty) {
            worklist ++= buffer
            buffer.clear()
          }

          offs += field.size

          minGap = maxAlignment
          minGapIndex = 0
          minAlignment = maxAlignment
          index = 0
        } else {
          if (gap < minGap) {
            minGap = gap
            minGapIndex = index
          }
          if (field.alignment < minAlignment) {
            minAlignment = field.alignment
          }
          index += 1
        }
      }
    }

    /** Returns `true` if layout was rolled back to a more optimal position (with smaller gap) or `false` otherwise. */
    private def rollbackToMinimizeGap(minGap: Int, minGapIndex: Int, minAlignment: Int): Boolean = {
      if (done.nonEmpty) {
        // search and rollback not optimal tail:
        // go through `done` list in reverse order and try to find more optimal position
        // for the remaining fields in `worklist` (based on alignment of the smallest field in it)
        var cur = done.size - 1
        while (cur >= 0) {
          val curFieldOffs = done(cur).offs
          if (calcGap(curFieldOffs, minAlignment) < minGap) {
            // if such position is found, rollback `done` list to this position
            // and backup removed elements into `buffer`
            offs = curFieldOffs

            while (done.size > cur) {
              buffer += done.remove(done.size - 1).field
            }
            return true
          }
          cur -= 1
        }
      }

      // otherwise restart with the field with minimal gap from the current offs
      if (minGapIndex > 0) {
        worklist.insert(0, worklist.remove(minGapIndex))
      }
      false
    }

    private def calcGap(offs: Int, alignment: Int) = {
      val misalignment = offs % alignment
      if (misalignment == 0) 0 else alignment - misalignment
    }
  }
}
