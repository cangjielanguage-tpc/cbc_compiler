/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.model

import java.lang.reflect.{Field, Modifier}
import java.util.IdentityHashMap

/** Cycle-safe structural comparator for the mutable CHIR model, used by the
  * serializer round-trip harness. Compares objects by walking declared fields
  * with reflection; handles Option / ArrayBuffer / boxed primitives / enums.
  *
  * `compareIds = false` skips the `id` fields: files written from a live
  * compiler package place some defs outside the package def lists (discovered
  * via refs during dispatch, appended after the seeds), while a load+resave
  * of a deserialized package re-groups all defs into the kind-ordered seed
  * lists — so ids legitimately shift on the first round-trip. (This matches
  * C++ deserializer+serializer semantics; patch-gen never resaves loaded
  * packages, so it never observes the shift.)
  */
object ModelCompare {

  /** Returns the first difference as a readable path, or None if equal. */
  def compare(a: AnyRef, b: AnyRef, compareIds: Boolean = true): Option[String] =
    try cmp(a, b, "", new IdentityHashMap[AnyRef, AnyRef](), compareIds)
    catch {
      case e: StackOverflowError => Some("<stack overflow>")
    }

  private def cmp(a: AnyRef, b: AnyRef, path: String, seen: IdentityHashMap[AnyRef, AnyRef],
      compareIds: Boolean): Option[String] = {
    if (a eq b) None
    else if (a == null) Some(s"$path: left is null, right is ${describe(b)}")
    else if (b == null) Some(s"$path: left is ${describe(a)}, right is null")
    else if (a.isInstanceOf[scala.reflect.Enum] || b.isInstanceOf[scala.reflect.Enum]) {
      if (a.getClass != b.getClass) Some(s"$path: enum class differs")
      else if (a ne b) Some(s"$path: enum differs ($a vs $b)")
      else None
    } else a match {
      case s: String =>
        if (s != b) Some(s"$path: '$s' vs '$b'") else None
      case _: java.lang.Boolean | _: java.lang.Byte | _: java.lang.Short | _: java.lang.Integer |
           _: java.lang.Long | _: java.lang.Float | _: java.lang.Double =>
        if (a != b) Some(s"$path: $a vs $b") else None
      case o: Option[_] =>
        b match {
          case b: Option[_] =>
            cmp(o.getOrNull, b.getOrNull, s"$path.get", seen, compareIds)
          case _ => Some(s"$path: Option vs ${b.getClass.getSimpleName}")
        }
      case o: scala.collection.immutable.Seq[_] =>
        b match {
          case b: scala.collection.immutable.Seq[_] => cmpSeqs(o, b, path, seen, compareIds)
          case _ => Some(s"$path: immutable.Seq vs ${b.getClass.getSimpleName}")
        }
      case o: scala.collection.mutable.Seq[_] =>
        b match {
          case b: scala.collection.mutable.Seq[_] => cmpSeqs(o, b, path, seen, compareIds)
          case _ => Some(s"$path: mutable.Seq vs ${b.getClass.getSimpleName}")
        }
      case _ =>
        if (a.getClass != b.getClass) {
          Some(s"$path: class differs ${a.getClass.getSimpleName} vs ${b.getClass.getSimpleName}")
        } else if (seen.containsKey(a)) {
          if (seen.get(a) ne b) Some(s"$path: cycle aliasing mismatch") else None
        } else {
          seen.put(a, b)
          val fields = allFields(a.getClass).sortBy(_.getName).filter(f => compareIds || f.getName != "id")
          var diff: Option[String] = None
          val it = fields.iterator
          while (diff.isEmpty && it.hasNext) {
            val f = it.next()
            f.setAccessible(true)
            val fa = f.get(a)
            val fb = if (b.getClass == a.getClass) { f.setAccessible(true); f.get(b) } else null
            diff = cmp(fa, fb, s"$path.${f.getName}", seen, compareIds)
          }
          diff
        }
    }
  }

  private def cmpSeqs(sa: scala.collection.Seq[_], sb: scala.collection.Seq[_],
      path: String, seen: IdentityHashMap[AnyRef, AnyRef], compareIds: Boolean): Option[String] = {
    if (sa.length != sb.length) Some(s"$path: length ${sa.length} vs ${sb.length}")
    else {
      val pairs = sa.iterator.zip(sb.iterator).zipWithIndex
      var diff: Option[String] = None
      while (diff.isEmpty && pairs.hasNext) {
        val ((va, vb), i) = pairs.next()
        diff = cmp(va.asInstanceOf[AnyRef], vb.asInstanceOf[AnyRef], s"$path[$i]", seen, compareIds)
      }
      diff
    }
  }

  private def describe(v: AnyRef): String =
    if (v == null) "null" else v.getClass.getSimpleName

  private val fieldCache = new java.util.concurrent.ConcurrentHashMap[Class[_], Array[Field]]

  private def allFields(c: Class[_]): Array[Field] =
    fieldCache.computeIfAbsent(c, (cls: Class[_]) => {
      val buf = new java.util.ArrayList[Field]
      var k: Class[_] = cls
      while (k != null && k != classOf[Object]) {
        for (f <- k.getDeclaredFields) {
          val m = f.getModifiers
          if (!Modifier.isStatic(m) && !f.isSynthetic && f.getName != "_bitmap") buf.add(f)
        }
        k = k.getSuperclass
      }
      buf.toArray(new Array[Field](0))
    })

  // Option.get on None throws; map None -> null for comparison
  private implicit class OptionOps(private val o: Option[_]) extends AnyVal {
    def getOrNull: AnyRef = o.getOrElse(null).asInstanceOf[AnyRef]
  }
}
