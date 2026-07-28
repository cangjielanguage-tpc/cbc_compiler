/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem.cangjie

import com.huawei.excelsior.jet.compiler.TypeProvider
import xscala.io.{DataInput, DataOutput}

import java.io.IOException
import scala.collection.mutable

object FileDescriptors {
  val fileDescriptorName = "fileDescriptor.data"

  @throws[IOException]
  def serialize(out: DataOutput, typeProvider: TypeProvider): Unit = {
    val fileDescriptors = mutable.LinkedHashMap.empty[String, String]

    for (t <- typeProvider.getAllClasses if t.hasFileDescriptor) {
      fileDescriptors.put(t.getName, t.getFileDescriptor)
    }

    out.putW32(fileDescriptors.size)
    for ((key, value) <- fileDescriptors) {
      out.putUTF(key)
      out.putUTF(value)
    }
  }

  @throws[IOException]
  def deserialize(in: DataInput, typeProvider: TypeProvider): Unit = {
    val fileDescriptors = mutable.HashMap.empty[String, String]

    val count = in.getW32()
    for (_ <- 0 until count) {
      fileDescriptors.put(in.getUTF(), in.getUTF())
    }

    for (t <- typeProvider.getAllClasses; fd <- fileDescriptors.get(t.getName)) {
      t.setFileDescriptor(fd)
    }
  }
}