package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.CHIR
import scala.reflect.ClassTag

object CHIRUtils {
  extension (xs: Array[Long]) {
    def toValueSeq[T >: Null <: CHIR.Value: ClassTag](using provider: CHIRItemProvider): Seq[T] =
      PackageImpl.ids(xs).toSeq.map(provider.getValue[T](_).get)
    def toTypeSeq[T >: Null <: CHIR.Type: ClassTag](using provider: CHIRItemProvider): Seq[T] =
      PackageImpl.ids(xs).toSeq.map(provider.getType[T](_).get)
  }
}
