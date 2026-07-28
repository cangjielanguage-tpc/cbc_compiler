/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.options

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind

/** Compiler option with string value.
  *
  * @author paul
  * @author conwor
  */
enum StrOption(defaultValue: String = null,
               defaultLambda: Environment => String = null,
               override val smartKind: SmartKind = SmartKind.Checked) extends Option[String] {

  case Stats                    extends StrOption("")
  case prj                      extends StrOption
  case IrLogsDir                extends StrOption
  case InlineStat               extends StrOption
  case ClinitAnalysisStat       extends StrOption
  case AICOptStat               extends StrOption
  case EscapeStat               extends StrOption
  case ExplosionStat            extends StrOption
  case MarkedRegionsStat        extends StrOption
  case DuplicatePositionMarkers extends StrOption
  case WriteBarriersOptStat     extends StrOption
  case OptStat                  extends StrOption
  case CodeLayout               extends StrOption
  case GNewStat                 extends StrOption
  case TauOptStat               extends StrOption
  case XiTransformStat          extends StrOption
  case FieldsTypeStat           extends StrOption
  case ExtraInfoStat            extends StrOption
  case RMACombiningStat         extends StrOption
  case OutputName               extends StrOption
  case PDBName                  extends StrOption
  case PDBLocation              extends StrOption
  case TargetDir                extends StrOption
  case ShowInlinePlanFor        extends StrOption
  case LogOnlyClass             extends StrOption
  case LogOnlyProc              extends StrOption
  case JProfileDir              extends StrOption
  case JCAdvise                 extends StrOption
  case ConstRTFields            extends StrOption(ConstRTFieldsValue.enabledByDefaultString)
  case UseLibrary               extends StrOption
  case ForeignLibs              extends StrOption
  case CbcAOTDeps               extends StrOption
  case AllCbcAOTDeps            extends StrOption
  case GlobalInitFieldsStat     extends StrOption
  case DynLibs                  extends StrOption
  case SwitchAggregationStat    extends StrOption
  case CangjiePackagesToO1      extends StrOption
  case LibraryName              extends StrOption
  case FrontEnd                 extends StrOption("jbc") // TODO: enum option
  case StackLimit               extends StrOption("900000")
  case STDLib                   extends StrOption
  case JProfile                 extends StrOption
  case DefaultCompactProfile    extends StrOption
  case Locales                  extends StrOption
  case PackExtraItems           extends StrOption
  case TargetZip                extends StrOption
  case NoneLangPackRTClasses    extends StrOption
  case TomcatClasspath          extends StrOption
  case AppType                  extends StrOption

  // Do not use this option. Use ProjectLogic.optRTFiles instead
  case OPTRTFILES               extends StrOption

  override def defaultValueOrNull(env: Environment) = if (defaultValue != null) {
    defaultValue
  } else if (defaultLambda != null) {
    defaultLambda(env)
  } else {
    null
  }

  override def parse(value: String) = value

  locally {
    register(this)
  }
}
