/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.util

import scala.NullPointerException

/** Compute a hash code given a nullable object reference. */
def hashCode(obj: AnyRef): Int = if (obj == null) 0 else obj.hashCode()

/** Compute a hash code for a sequence of input objects. */
def hash(values: Any*): Int =
  if (values == null)
    0
  else
    values
      .map(x => hashCode(x.asInstanceOf[AnyRef])) // compute hash code of each element
      .foldLeft(1)(31 * _ + _) // reasonably good way to concatenate hashes

/** Returns `true` if passed object reference is not `null`. */
def nonNull(obj: AnyRef): Boolean = obj != null

// TODO: find better place
def simpleClassName(obj: Any): String = {
  // "foo.Bar$Baz"
  val fullName = obj.getClass.getName

  // "Bar$Baz"
  val className = fullName.substring(fullName.lastIndexOf('.') + 1)

  // "Baz"
  className.substring(className.lastIndexOf('$') + 1)
}
