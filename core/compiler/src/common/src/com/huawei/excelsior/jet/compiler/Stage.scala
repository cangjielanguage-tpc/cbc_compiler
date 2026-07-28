/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

/** Compiler stages. Used for compilation time profiling.
  */
enum Stage {
  case ReadProject
  case CompileProject
  case Verifying
  case Cha
  case xcMain_CodegenStage
  case ObjFile
  case Linking
  case CBCFileGenerator
  case InitPrimTDs
  case JBCFrontPreprocessClassFile
  case JBCFrontParseClassFile
  case JBCFrontMakeClassImpl
  case pcOfind
  case SymCacheDrop
  case Completer
  case InlinePlanning
  case ProfileGraphBuilding
  case PGOIterationTransition
  case ChainsInlinePlanning
  case StructuredLockingAnalysis
  case CangjieMain
  case CangjieModuleParsing
  case CangjieFunctionParsing
  case OptStartFront
  case OptStartBack
  case OptStartComboFrontBack
  case OptRunPhases
  case OptFront
  case OptBack
  case StaticCallGraphAnalysis
  case PGOStaticAnalysis
  case LoadJBC
  case LoadHLIR
  case LoadCHIR
  case OptimizeLoop
  case ConsistencyChecking
  case InlineAll
  case InlineDecision
  case Devirt
  case PreLowering
  case Lowering
  case LoweringInline
  case Serialization
  case Deserialization
  case GCM
  case DCE
  case Preparation
  case O1CodeOrdering
  case O1RegAlloc
  case O1PostProcess
  case O2CodeOrdering
  case O2RegAlloc
  case O2PostProcess
  case RecolorFrameSlots
  case CFGLiveness
  case Other
  case CFGLayout
  case PDBOpen
  case GCPointsInserting
}
