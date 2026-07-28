/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

/* A kind of statistics that compiler collects. */
enum StatsKind(verbose: Boolean = true) {

  /** Temporary stats kind.
    * Must only be used while development or debugging.
    */
  case TMP extends StatsKind

  /** Methods compilation stats. */
  case MethComp extends StatsKind(false)

  /** Compiler crashes stats. */
  case Crashes extends StatsKind(false)

  /** Methods body size stats of not compiled methods with huge size.
    *
    * @see NumOption.HugeMethodsLimit
    */
  case HugeMethodsSize extends StatsKind

  /** Methods body size stats of not compiled clinits with huge size.
    *
    * @see NumOption.HugeClinitsLimit
    */
  case HugeClinitsSize extends StatsKind

  /** Memory optimization stats. */
  case MemOpt extends StatsKind

  /** Implicit checks optimization stat. */
  case ImplicitCheckOptimization extends StatsKind

  /** Allocators optimization: explosion, allocation on stack, inline. */
  case NewOptimization extends StatsKind

  /** Empty segments elimination stat. */
  case EmptySegmentOptimization extends StatsKind

  /** Edge incoming into crossroad optimized stat. */
  case CrossroadsOptimization extends StatsKind

  case TypeFiltersAbsorption extends StatsKind

  /** Cmp(And, 0) to Test optimization stat. */
  case TestsOptimization extends StatsKind

  /** Devirtualization. */
  case Devirt extends StatsKind

  /** Various type optimizations. */
  case TypeOpt extends StatsKind

  /** Non-SSA variables. */
  case Vars extends StatsKind

  /** String optimizations (strconcat, key strings). */
  case StringOpt extends StatsKind

  /** sun.misc.Unsafe optimizations. */
  case UnsafeOpt extends StatsKind

  /** Statistics around loops transformations. */
  case LoopType extends StatsKind

  /** Statistics of how many loops are counted. */
  case CountedLoops extends StatsKind

  /** Xi transformations (peeling, versioning, tau-diamond merge). */
  case XiTransformations extends StatsKind

  /** Statistics about clinit analysis. */
  case ClinitAnalysis extends StatsKind

  /** Refining type of reference return value. */
  case RefinedReturnType extends StatsKind

  /** Wild transfer cleaning. */
  case WildTransfer extends StatsKind

  /** Redundant copy cleaning. */
  case RedundantCopiesElimination extends StatsKind

  /** Pulling up transfers from flag intervals. */
  case FlagIntervalTransfers extends StatsKind

  /** RMA combining. */
  case RMACombining extends StatsKind

  /** Amount of spill code (load/store) in hot loops without calls in PGO hosts. */
  case SpillInPGOLoopsWithoutCalls extends StatsKind

  /** Elimination of synchronization. */
  case SyncElimination extends StatsKind

  /** Frame slots recoloring to float registers. */
  case FrameSlotsColoring extends StatsKind

  /** Statistics about context types optimizations. */
  case ContextTypes extends StatsKind

  /** NewArray + System.arraycopy optimization. */
  case NewArrayCopy extends StatsKind

  /** Interface operations optimizations. */
  case IFaceOps extends StatsKind

  /** Idempotent operations optimizations. */
  case IdempotentOperations extends StatsKind

  /** Logging of dirty frames reasons and numeric statistics. */
  case DirtyFrames extends StatsKind

  /** Logging of preparation checks, generated in code. */
  case LazyPreparation extends StatsKind

  /** Logging of types, marked for bootstrap preparation. */
  case BootstrapPreparation extends StatsKind

  /** Logging of types, marked for preparation from import. */
  case EagerPreparation extends StatsKind

  /** Logging of getClass-related bytecode patterns replacement. */
  case GetClass extends StatsKind

  /** Explicit null check optimization statistics. */
  case ExplicitNullCheckFolding extends StatsKind

  /** Statistics about constant string invocations and its optimizations. */
  case ConstStrings extends StatsKind

  /** Zero iteration loop elimination statistics. */
  case ZeroLoopElimination extends StatsKind

  /** Live ranges optimization. */
  case LiveRangesOptimization extends StatsKind

  /** Compilation mode, used to compile methods. */
  case CompilationMode extends StatsKind

  /** TypeEmpty approximated unreachable code elimination. */
  case TypeEmptyUnreachableElimination extends StatsKind

  /** Array zeroing elimination statistics. */
  case ArrayZeroingElimination extends StatsKind

  /** Cangjie array filling elimination statistics. */
  case CangjieArrayFillingElimination extends StatsKind

  /** Comparison and branch combining operational statistics. */
  case CangjieBranchIfCombining extends StatsKind

  /** Placed evacuation counter. */
  case Evacuation extends StatsKind

  /** Record compensatory zeroing statistics. */
  case CompensatoryZeroingForRecords extends StatsKind

  /** Record no return optimization. */
  case NoReturn extends StatsKind

  /** Record founded MSub patterns. */
  case MSubPattern extends StatsKind

  def isVerbose = verbose
}

object StatsKind {
  def fromString(str: String) = VALUES
    .find(_.productPrefix.equalsIgnoreCase(str))
    .getOrElse(throw new IllegalArgumentException(s"undefined stat \"$str\""))

  private val VALUES = values
}
