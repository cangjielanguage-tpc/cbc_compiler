/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.common.CodeHelpers

/** List of features for marking code where they are not implemented.
  *
  * See [[CodeHelpers.notImplemented]].
  */
enum NotImplementedFeature:
  case ERROR_CATCH_TYPE // see JET-7092
  case TRANSACTIONAL_PDB_WRITING
  case CBC
  case CBC_FILE_ONE_REGION_LIMIT_EXCEEDED
