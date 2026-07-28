/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.serialization

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.NotImplementedFeature.TRANSACTIONAL_PDB_WRITING
import com.huawei.excelsior.jet.compiler.PDB2.Location
import xscala.io.{ByteBuffer, DataInput, DataOutput}

trait SerializationToolbox {

  def readPDBFile[A](env: Environment, loc: Location)(read: DataInput => A): A = {
    val in = env.pdb.getDataInputOrNull(loc)
    assert(in != null)
    try read(in) finally in.close()
  }

  def writePDBFile(env: Environment, loc: Location)(write: DataOutput => Unit): Unit = {
    // There is no way to remove or overwrite PDB file which was opened for writing in case of any errors.
    // So we try to do most of the dangerous job (i.e. writeAction) before opening the real stream.
    // See JET-10335.
    val buffer = new ByteBuffer()
    write(buffer)

    try {
      val out = env.pdb.getDataOutput(loc)
      try out.putBytes(buffer.getBytesPointer, 0, buffer.length) finally out.close()
    } catch {
      case _: UnsupportedOperationException =>
        // This happens if we have already tried to write this PDB file earlier and have crashed (e.g. OOM happened).
        notImplemented(TRANSACTIONAL_PDB_WRITING)
    }
  }
}
