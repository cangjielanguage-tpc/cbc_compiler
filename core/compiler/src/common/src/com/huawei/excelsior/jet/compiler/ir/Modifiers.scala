/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.ir

import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.*
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Target.*

import scala.annotation.targetName

/** JET modifiers access implementation.
  *
  * Modifiers are bit-flags used to specify classes, methods and fields properties. Many modifiers are based on java
  * bytecode modifiers.
  *
  * @author conwor
  */
class Modifiers(val value: Int) extends AnyVal {
  @inline @targetName("incl") def + (m: Modifier): Modifiers = new Modifiers(value | m.mask)
  @inline @targetName("excl") def - (m: Modifier): Modifiers = new Modifiers(value & ~m.mask)

  @inline @targetName("intersect") def & (ms: Modifiers): Modifiers = new Modifiers(value & ms.value)
  @inline @targetName("union")     def | (ms: Modifiers): Modifiers = new Modifiers(value | ms.value)

  @inline def contains(m: Modifier): Boolean = (value & m.mask) != 0
}

object Modifiers { self =>

  enum Target {
    case CLASS, METHOD, FIELD, ACCESS
  }

  enum Modifier(val bit: Int, val targets: Target*) {
    case PUBLIC       extends Modifier(0,  CLASS, METHOD, FIELD, ACCESS)
    case PRIVATE      extends Modifier(1,  CLASS, METHOD, FIELD)
    case PROTECTED    extends Modifier(2,  CLASS, METHOD, FIELD)
    case STATIC       extends Modifier(3,  CLASS, METHOD, FIELD)
    case FINAL        extends Modifier(4,  CLASS, METHOD, FIELD, ACCESS)
    case SYNCHRONIZED extends Modifier(5,  METHOD)
    case SUPER        extends Modifier(5,  ACCESS)
    case VOLATILE     extends Modifier(6,  FIELD)
    case BRIDGE       extends Modifier(6,  METHOD)
    case TRANSIENT    extends Modifier(7,  FIELD)
    case VARARGS      extends Modifier(7,  METHOD)
    case CJ_FOREIGN   extends Modifier(7,  METHOD)
    case NATIVE       extends Modifier(8,  METHOD)
    case STATINI      extends Modifier(8,  FIELD)
    case INTERFACE    extends Modifier(9,  CLASS, ACCESS)
    case INJECTED     extends Modifier(9,  FIELD)
    case ABSTRACT     extends Modifier(10, CLASS, METHOD, ACCESS)
    case CONSTVAL     extends Modifier(10, FIELD)
    case STRICT       extends Modifier(11, CLASS, METHOD) // TODO-MODIFIERS: according to xjRTS, STRICT is only method flag, but in JDK it is class flag too
    case SYNTHETIC    extends Modifier(12, CLASS, METHOD, FIELD, ACCESS)
    case ANNOTATION   extends Modifier(13, CLASS, ACCESS)
    case DEPRECATED   extends Modifier(13, METHOD, FIELD)
    case ENUM         extends Modifier(14, CLASS, FIELD, ACCESS) // What the field is doing here?
    case CONSTRUCTOR  extends Modifier(14, METHOD)
    case CJ_SEALED    extends Modifier(16, CLASS)
    case CJ_MUT       extends Modifier(17, CLASS)
    case CJ_REDEF     extends Modifier(18, CLASS)
    case CJ_OVERRIDE  extends Modifier(19, CLASS)

    val mask = 1 << bit
  }

  def apply(mask: Int) = new Modifiers(mask)

  /** Returns [[Modifiers]] with set `modifiers`. */
  def apply(modifiers: Modifier*): Modifiers = apply(modifiers.map(_.mask).fold(0)(_ | _))

  private def mask(target: Target): Modifiers =
    apply(Modifier.values filter (_.targets.contains(target)): _*)

  val EMPTY = Modifiers()

  // Masks for all allowed [[Modifier]] for classes/methods/fields
  val classMask       = mask(CLASS)
  val methodMask      = mask(METHOD)
  val fieldMask       = mask(FIELD)
  val accessFlagsMask = mask(ACCESS)


  object JBC {
    private val publicMask  = Modifiers(PUBLIC, PRIVATE, PROTECTED, STATIC, FINAL, SYNCHRONIZED, VOLATILE, TRANSIENT, NATIVE,
      /* INTERFACE, TODO-MODIFIERS: according to xjRTS it is JBC flag, but in JDK it is not */
      ABSTRACT, STRICT)

    private val privateMask = Modifiers(BRIDGE, VARARGS, SYNTHETIC, ANNOTATION, ENUM)
    private val fullMask = publicMask | privateMask

    val publicClassMask     = self.classMask  & publicMask
    val publicInterfaceMask = publicClassMask - FINAL
    val publicMethodMask    = self.methodMask & publicMask
    val publicFieldMask     = self.fieldMask  & publicMask

    val fullClassMask       = self.classMask  & fullMask
    val fullMethodMask      = self.methodMask & fullMask
    val fullFieldMask       = self.fieldMask  & fullMask

    val constructorMask = Modifiers(PUBLIC, PRIVATE, PROTECTED)
  }


  object CJ {
    val classMask  = Modifiers(PUBLIC, FINAL, ABSTRACT, CJ_SEALED)
    val methodMask = Modifiers(PUBLIC, PRIVATE, PROTECTED, STATIC, FINAL, ABSTRACT, CJ_FOREIGN, CJ_MUT, CJ_REDEF, CJ_OVERRIDE)
  }
}
