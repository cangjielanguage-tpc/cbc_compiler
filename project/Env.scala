/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package build

import sbt.SettingKey

import java.io.File
import java.util.Properties
import scala.util.{Try, Using}

class Env(props: Properties, val chirVersions: Seq[Int]) {
  private var errors = false

  val os                 = apply("os",                  Some("linux"),       "linux", "windows")
  val arch               = apply("arch",                None,                "amd64", "arm64")
  val mode               = apply("mode",                None,                "work", "enduser")
  val languagePack       = apply("language.pack",       Some("cangjie"),     "none", "java", "cangjie", "scala")
  val flatc              = apply("flatc",               Some("flatc"))
  val graalvm            = apply("graalvm",             Some(""))
  val graalvmParallelism = apply("graalvmParallelism",  Some("2"))
  val chirDefaultVersion = apply("chir.defaultVersion", Some("13002"), chirVersions.map(_.toString)*)

  def apply(name: String, default: Option[String], variants: String*): String = {
    sys.props.get(name)
      .orElse(Option(props.getProperty(name)))
      match {
        case Some(v) if variants.isEmpty || (variants contains v) => v
        case Some(v) =>
          println(s"Unexpected value '$v' for property $name ${variants.mkString("(", ", ", ")")}")
          errors = true
          ""
        case None => default.getOrElse {
          println(s"Missing property $name ${if (variants.isEmpty) "" else variants.mkString("(", ",", ")")}")
          errors = true
          ""
        }
      }
  }

  if (errors) {
    sys.exit(1)
  }

  override def toString: String = props.toString
}

object Env {
  /** Collect supported CHIR format versions from schema dirs (core/chir-lib/schema/v<n>). */
  private def chirSchemaVersions(schemaDir: File): Seq[Int] = {
    val versions = Option(schemaDir.listFiles()).toSeq.flatten
      .filter(_.isDirectory)
      .flatMap(dir => Try(dir.getName.stripPrefix("v").toInt).toOption)
      .distinct
      .sorted
    if (versions.isEmpty) {
      println(s"No CHIR schema versions found in $schemaDir")
      sys.exit(1)
    }
    versions
  }

  /** Load environment values from system properties file. */
  def load(file: File, chirSchemaDir: File): Env = {
    val props = new Properties()
    if (file.exists()) {
      Using.resource(scala.io.Source.fromFile(file)) { source =>
        props.load(source.bufferedReader())
      }
    }
    new Env(props, chirSchemaVersions(chirSchemaDir))
  }

  val settingKey = SettingKey[Env]("env", "Environment values loaded from env.xml and/or system properties.")
}
