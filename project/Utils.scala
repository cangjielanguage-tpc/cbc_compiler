/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package build

import sbt.{ClasspathDep, Project, ProjectReference}

import java.io.File
import java.nio.file.{Files, Path}

object Utils {
  implicit class ProjectOps(val project: Project) extends AnyVal {
    def dependsOnWhen(cond: => Boolean)(deps: ClasspathDep[ProjectReference]*): Project = {
      if (cond) project.dependsOn(deps *)
      else project
    }

    def aggregateWhen(cond: => Boolean)(refs: ProjectReference*): Project = {
      if (cond) project.aggregate(refs *)
      else project
    }
  }

  def directorySize(dirName: Path): Long = {
    val stream = Files.walk(dirName)
    stream
      .filter(p => Files.isRegularFile(p))
      .mapToLong(p => Files.size(p))
      .sum()
  }

  def showAllFiles(dirName: Path): Seq[File] = {
    Files.walk(dirName).toArray.toSeq.collect {
      case p: Path if Files.isRegularFile(p) => p.toFile
    }
  }
}
