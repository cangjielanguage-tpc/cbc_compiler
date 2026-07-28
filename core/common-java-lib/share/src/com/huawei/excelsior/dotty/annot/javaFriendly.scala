/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.dotty.annot

import scala.annotation.StaticAnnotation

/** Annotating Scala enum with this annotation allows to
  * use its named values as cases in Java `switch` statements.
  *
  * @note Works only if `java-friendly-enums` plugin is enabled.
  */
// TODO: move to any proper place.
final class javaFriendly extends StaticAnnotation
