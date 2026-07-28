/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.patterns

import com.huawei.excelsior.jet.compiler.{PreparationRequired, StatsKind}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType

import scala.PartialFunction.cond

/** Statically compute some [[sun.misc.Unsafe]] invocations. */
trait UnsafeOffsets { self: Universe =>

  def computeUnsafeOffsets(): Boolean = {
    var changed = false
    for (x @ CallMethod(method, _, _) <- all[Call]) {
      // fast path check is outlined
      if (isUnsafeObjectFieldOffsetMethod(method) || isXScalaLazyValsGetOffsetStatic(method)) {
        changed |= tryComputeOneObjectFieldOffset(x)
      }
      if (isUnsafeArrayIndexScale(method)) {
        changed |= tryComputeOneArrayIndexScale(x)
      }
    }
    changed
  }

  /** Optimizes the following pattern of [[sun.misc.Unsafe#objectFieldOffset]] usage by eliminating all calls:
    * <pre>
    *   Unsafe theUnsafe = ...;
    *   Class<?> klass = Foo.class;
    *   String fieldName = "bar";
    *   Field field = klass.getDeclaredField(fieldName);
    *   long offset = theUnsafe.objectFieldOffset(field);
    * </pre>
    *
    * Note that [[sun.misc.Unsafe#staticFieldOffset]] is not optimized because:
    * <ul><li>offsets from static bundle are not known at the moment of compilation</li>
    *     <li>these calls are quite rare in real applications</li></ul>
    */
  private def tryComputeOneObjectFieldOffset(getOffsetCall: Call): Boolean = {
    cond(getOffsetCall.invokeArgs) {
      case Seq(_ /* theUnsafe */, getFieldCall: Call) if isClassGetDeclaredFieldMethod(getFieldCall.targetRef.method) =>
        cond(getFieldCall.invokeArgs) {
          case Seq(ClassObject(klass), ConstString(fieldName)) if klass.isClassOrInterface =>
            val field = asClassType(klass).findDeclaredFieldOrNull(fieldName)
            if (field == null || field.isStatic) {
              return false
            }

            val fieldType = field.getType
            val isPrimitive = fieldType.isPrimitive
            val isInterface = fieldType.isInterface
            replaceByCode(getOffsetCall) {
              val offsetValue = field.getInstanceFieldOffset
              val unsafeRef = RT.FieldOffsetAccessor.fieldOffset(isPrimitive, isInterface)
              ensurePrepared(PreparationRequired.forInvoke(unsafeRef))
              Invoke(unsafeRef)(IConst(offsetValue))
            }

            if (!getFieldCall.hasValueUses) {
              // Its only side effect is to initialize internal caches, may be ignored.
              strikeOut(getFieldCall)
            }

            // klass and fieldName could also be dead, but they are not calls and will be removed later on general basis.

            val methodName = getOffsetCall.targetRef.method.getName

            val kind = if (isPrimitive) "primitive"
              else if (isInterface) "reference, interface"
              else "reference, non-interface"

            stats.count(StatsKind.UnsafeOpt, s"inline $methodName ($kind)", getOffsetCall.pos)
            true
        }
    }
  }

  /** Optimizes the following pattern of [[sun.misc.Unsafe#arrayIndexScale]] usage by eliminating the call:
    * <pre>
    *   Unsafe theUnsafe = ...;
    *   Class<?> array = int[][].class;
    *   int scale = theUnsafe.arrayIndexScale(field);
    * </pre>
    *
    * Note that [[sun.misc.Unsafe#arrayBaseOffset]] is currently implemented in runtime as `@Inline` constant value.
    */
  private def tryComputeOneArrayIndexScale(getScaleCall: Call): Boolean = {
    cond(getScaleCall.invokeArgs) {
      case Seq(_ /* theUnsafe */, ClassObject(array)) if array.isJavaArray =>
        val scaleNode = IConst(array.getArrayElemType.toAsm.sizeInBytes)
        getScaleCall.replaceValueUsesBy(scaleNode)

        val pos = getScaleCall.pos
        strikeOut(getScaleCall)

        stats.count(StatsKind.UnsafeOpt, "compute arrayIndexScale", pos)
        true
    }
  }

  private def isUnsafeObjectFieldOffsetMethod(m: Method) =
    m.getDeclaringClass.isSunMiscUnsafe && m.getName == "objectFieldOffset"

  private def isXScalaLazyValsGetOffsetStatic(m: Method) =
    m.getDeclaringClass.getName == "scala/runtime/LazyVals$" && m.getName == "getOffsetStatic"

  private def isUnsafeArrayIndexScale(m: Method) =
    m.getDeclaringClass.isSunMiscUnsafe && m.getName == "arrayIndexScale"

  private def isClassGetDeclaredFieldMethod(m: Method) =
    (m.getDeclaringClass.getName == "java/lang/Class" || m.getDeclaringClass.getName == "xscala/Class") && m.getName == "getDeclaredField"

}
