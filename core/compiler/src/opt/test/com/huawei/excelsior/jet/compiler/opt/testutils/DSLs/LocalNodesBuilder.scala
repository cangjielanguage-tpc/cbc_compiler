/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.testutils.DSLs

import scala.collection.mutable

/**
 * Utilities for local nodes creation (IR without cfg)
 */

trait LocalNodesBuilder extends IRBuilderDSL {

  private var localNodes: mutable.HashMap[Int, NonControlNode] = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    localNodes = new mutable.HashMap[Int, NonControlNode]
  }

  private class BinOp private (proto: BinOp.Proto) extends BinaryOp(proto) with FloatingNode

  private object BinOp {
    case class Proto private[BinOp] (keyType: Type) extends BinaryOp.Floating[BinOp](keyType)(keyType) with PrototypeStrictNodeClass[BinOp, BinOp] {
      def newInstance() = new BinOp(this)
    }

    def apply(tpe: Type) = Prototype.intern(Proto(tpe))
  }

  /**
   * Create unique local nodes (integer args and binops) for non-cfg tests.
   */
  def lNode(f: Int, args: Int*): NonControlNode = {
    localNodes.getOrElseUpdate(f, {
      args.size match {
        case 0 => Param(IntType, f)
        case 2 => BinOp(IntType)(lNode(args(0)), lNode(args(1)))
      }
    })
  }

  def getLNode(x: Int): NonControlNode = {
    localNodes.get(x) should be (defined)
    localNodes(x) should be (Symbol("committed"))
    localNodes(x)
  }

  def checkLNodes(ns: Int*): Unit = {
    ns foreach getLNode

    unreachableBar // evaluate lazy creation of UnreachableBar to make magic constant stable
    val EntryNodesNum = 3 // magic constant to take into account some magic nodes
    allNodesCount should be (ns.size + EntryNodesNum)
  }

  def checkLNodeArgs(f: Int, args: Int*): Unit = {
    getLNode(f).args.toSeq should equal (args map getLNode)
  }

}
