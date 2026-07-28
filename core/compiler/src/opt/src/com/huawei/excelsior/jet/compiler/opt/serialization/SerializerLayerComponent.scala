/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.serialization

import com.huawei.excelsior.jet.compiler.PDB2.EntryKind
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.serialization.{SerializationError, SerializationToolbox}
import com.huawei.excelsior.jet.compiler.{Stage, symlevel}
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.util.Names

trait SerializerLayerComponent extends Serialization with Deserialization with IOComponent { self: Universe =>

  val serialization: SerializationLayer = new SerializationLayer

  final class SerializationLayer extends SerializationToolbox {

    private def location(method: Method) = EntryKind.IR.loc(method.getDeclaringClass, Names.shortName(method))

    def serialize(method: symlevel.Method): Unit = {
      stage(Stage.Serialization) {
        writePDBFile(env, location(method)) { stream =>
          assert(IRFormatVersion <= 0xFF)
          stream.putByte(IRFormatVersion)
          serializeWithWriter(new OptWriter(stream, method.getDeclaringClass), method)
        }
      }
    }

    def loadMethod(method: symlevel.Method, args: Seq[Node]): RTPartsInfo = {
      stage(Stage.Deserialization) {
        readPDBFile(env, location(method)) { stream =>
          stream.getByte() match {
            case IRFormatVersion =>
              val info = locallyAnalyzeMethod(method).get // serialized method must have local info

              deserializeWithReader(new OptReader(stream, method.getDeclaringClass), method, args)
              RTPartsInfo(
                info.isDirtyForClassGC,
              )
            case v =>
              throw new SerializationError(s"Invalid serialized IR format version $v, expected $IRFormatVersion")
          }
        }
      }
    }
  }
}
