/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

import build.Build

lazy val all                       = Build.all
lazy val tests                     = Build.tests
lazy val compiler                  = Build.compiler
lazy val compilerAOT               = Build.compilerAOT
lazy val compilerAOTVMDependent    = Build.compilerAOTVMDependent
lazy val xscalaVMDependentShare    = Build.xscalaVMDependentShare
lazy val xscalaVMDependentStub     = Build.xscalaVMDependentStub
lazy val xscalaVMDependentJDK      = Build.xscalaVMDependentJDK
lazy val assembler                 = Build.assembler
lazy val commonJavaLib             = Build.commonJavaLib
lazy val commonRtCompiler          = Build.commonRtCompiler
lazy val compilerCommon            = Build.compilerCommon
lazy val cangjieJavaClassGenImpl   = Build.cangjieJavaClassGenImpl
lazy val lambdaTypeGenImpl         = Build.lambdaTypeGenImpl
lazy val newbaselineCodeGenerator  = Build.newbaselineCodeGenerator
lazy val o2Lib                     = Build.o2Lib
lazy val opt                       = Build.opt
lazy val starterAOT                = Build.starterAOT
lazy val symlevelLight             = Build.symlevelLight
lazy val wrapperCompiler           = Build.wrapperCompiler
lazy val xminizip                  = Build.xminizip
lazy val xpackii                   = Build.xpackii
lazy val chirLib                   = Build.chirLib
lazy val cbcAsm                    = Build.cbcAsm

inThisBuild(Build.thisBuildSettings)
