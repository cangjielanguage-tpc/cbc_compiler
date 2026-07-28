/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking

/** Analysis result computed by [[StructuredLockingAnalyzer]]. */
enum StructuredLockingAnalysisResult {

  /** Locking is structured and there is full information about monitor regions. */
  case STRUCTURED

  /** Locking is potentially unstructured due to method structure. */
  case POTENTIALLY_UNSTRUCTURED

  /** Locking is potentially unstructured due to known limitations of our analysis of JSR/RET. */
  case NOT_PAIRED_DUE_TO_JSRS
}
