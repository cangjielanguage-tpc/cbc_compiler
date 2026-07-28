/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.testutils.DSLs

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.Primitive
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{InterfaceType, ReferenceType}
import com.huawei.excelsior.jet.compiler.types.References.{OpenCone, ReferenceApprox}
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, TypeKind as TKind}
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.*
import com.huawei.excelsior.jet.compiler.types.TypesToolbox
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.PartialFunction.condOpt
import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.chaining.scalaUtilChainingOps

/**
  * Helper class for creation CFG with nodes.
  * TODO: improve DSL
  *
  * @author conwor
  */
trait GlobalNodesBuilder extends IRBuilderDSL with TypesToolbox { self: CompilerSuite =>

  protected var tieNodesInBackendOrder = false

  private val globalNodes = mutable.LinkedHashMap.empty[String, NodeRef]

  override def extraNodeNameSuffix(n: Node) = {
    super.extraNodeNameSuffix(n) + {
      var names = mutable.Buffer.empty[String]
      for ((name, node) <- globalNodes) {
        if (node == n || (node.isReferentCommitted && node.deref == n)) {
          names += name
        }
      }
      if (names.nonEmpty) names.mkString("<", ",", ">") else ""
    }
  }

  override def beforeEach(): Unit = {
    super[IRBuilderDSL].beforeEach()
    super[TypesToolbox].beforeEach()
    globalNodes.clear()
    testTypesCache.clear()
    attributes.clear()
  }

  /** Last added control node. */
  private var ctrl: UpperPoint = _
  /** Last added memory node. */
  private var mem: MemoryNode = _

  /** Make IR nodes in scope of blocks with implicit control and memory arguments.
    * Action receives callback which must be called before appending nodes to some block.
    */
  def makeNodes[A](action: (Block => Unit) => A): A = {
    def initState(b: Block): Unit = {
      ctrl = b.blockEnd.inCtrl match {
        // Keep handler anchor last node to prevent breaking def-use links in handled blocks, e.g.:
        // 0 @@ ("x=spinal()", "y=xspinal()") -> xb(1) @@ "use(x)"
        case x: HandlerAnchor => x.inCtrl
        case x => x
      }
      mem = b.blockEnd.inMemory
    }

    def tie(n: Node): Unit = {
      n match {
        case n: SpinalNode =>
          val outCtrl = single[ControlNode](ctrl.controlUses filter (_ != n)).asInstanceOf[LowerPoint]
          outCtrl.inCtrl = n
          ctrl = n

          if (n.canThrow && !n.isInstanceOf[HandlerAnchor]) {
            // Note that HandlerAnchor are added to XBlock explicitly in HandlerAnchor.create().
            for (xh <- n.block.spine collectFirst { case anchor: HandlerAnchor => anchor.xHandler }) {
              xh.addArg(n.xpoint)
            }
          }

          n match {
            case n: MemoryNode =>
              // We add memory nodes only to the end of block so replace only blockEnd's inMemory.
              val end = n.block.blockEnd
              assert(outCtrl match {
                case `end` => true
                case a: HandlerAnchor => a.outCtrl == end
                case _ => false
              })
              assert(end.inMemory == mem)
              end.inMemory = n
              mem = n

            case _ =>
          }

        case _: MemoryNode => shouldNotReachHere("Non-spinal memory in tie? Is it Block?")
        case _ =>
      }
    }

    withIncrementalGCM { eliminateCrossBlockMemoryEdges() }

    val res = withPos(rootMethodPos) {
      onCommit.withCallback(tie) {
        assert(!useDefaultArgs)
        useDefaultArgs = true
        try {
          action(initState)
        } finally {
          useDefaultArgs = false
        }
      }
    }

    optimizeBlockMemory()
    checkDefUseDominance()
    res
  }

  private var useDefaultArgs: Boolean = false

  override def getDefaultArgsForNode(node: Node, args: Seq[Node]) = {
    if (useDefaultArgs) defaultArgsFor(ctrl, mem)(node, args) else super.getDefaultArgsForNode(node, args)
  }


  def addNode(tpe: Type = IntType): Node =
    Fake(tpe)

  def addNode(defBlock: Block): Node =
    Fake(IntType).asInstanceOf[FloatingNode] atUpperPoint defBlock

  def addPinnedNode(defBlock: Block): Node =
    FakePinned(IntType)(defBlock)

  def addNode(defBlock: Block, arg1: Node): Node =
    FakeUnary(IntType)(arg1).asInstanceOf[FloatingNode] atUpperPoint defBlock

  def addControlledUnaryNode(inCtrl: Node, value: Node): Node = {
    FakeControlledUnary(value.tpe)(inCtrl, value)
  }

  def addNode(defBlock: Block, arg1: Node, arg2: Node): BinaryOp =
    (FakeBinary(IntType)(arg1, arg2).asInstanceOf[FloatingNode] atUpperPoint defBlock).asInstanceOf[BinaryOp]

  def addPhi(defBlock: Block, args: Node*) = Phi(args.head.tpe)(defBlock +: args: _*)

  def addResult(block: Block, memory: MemoryNode, result: Node) = block.blockEnd match {
    case ret: Return =>
      ret.inMemory = memory
      ret.inValue = result
      result
  }

  def addSomeCtrlNode(block: Block): SpinalNode = makeNodes { at =>
    at(block)
    FakeSpinal(IntType)()
  }

  def setCondition(condition: Node): Unit = {
    ctrl.block.blockEnd.asInstanceOf[Branch].selector = condition
  }

  def addCondition(block: Block, condition: Node): Node = {
    block.blockEnd.asInstanceOf[Branch].selector = condition
    condition
  }

  def addCondition(block: Block, x: Node, y: Node, op: Condition): Node = {
    addCondition(block, Cmp(if (x != null) x.tpe else IntType, op)(x, y))
  }

  def addCatch(): Node = {
    val b = ctrl.block.asInstanceOf[XBlock]
    if (collect[Catch](b.paramNodes).isEmpty) {
      Catch(b)
    } else {
      b.catchNode
    }
  }

  /** Node with ConditionType. */
  def addSomeConditionNode(): Node = {
    Fake(ConditionType)
  }

  def addParam(formalType: ReferenceType, appr: ReferenceApprox) = {
    rootMethod.addParamType(SignatureType.Reference(formalType.symType))
    val param = Param(ValueType(formalType.symType, eopTypeForInterfaces = true, instantiateRich = true), rootMethod.getParamsCount - 1)
    setNodeType(param, appr)
    param
  }

  private lazy val ObjectConeAppr = OpenCone(tObj, mayBeNull = true)

  def addObjNode(appr: ReferenceApprox = ObjectConeAppr) =
    addParam(ReferenceType.javaLangObject, appr)

  def addPinnedObjNode(appr: ReferenceApprox) =
    FakePinned(TRefType)(ctrl.block) tap (setNodeType(_, appr))

  def addPinnedObjNode(defBlock: Block, appr: ReferenceApprox = ObjectConeAppr) =
    FakePinned(TRefType)(defBlock) tap (setNodeType(_, appr))

  /** Create node enriched for given interface.
    * Type approximation is just object cone by default.
    */
  def addRichObjNode(interfType: InterfaceType, appr: ReferenceApprox = ObjectConeAppr) = {
    val obj = addParam(interfType, appr)
    assert(producesRich(obj).toOption contains interfType.symType)
    obj
  }

  def setNodeType(node: Node, appr: ReferenceApprox): Unit = {
    testTypesCache(node) = appr
    invalidateNodeType(node)
  }

  val testTypesCache = mutable.HashMap.empty[Node, ReferenceApprox]
  protected override def calculateOneType(n: Node): ReferenceApprox = {
    testTypesCache.getOrElse(n, super.calculateOneType(n))
  }

  def addGetField() = {
    val field = new FakeField(`type` = Primitive(TKind.INT))
    GetField(field)(addObjNode())
  }

  def addPutField(value: Node) = {
    val field = new FakeField(`type` = Primitive(TKind.INT))
    val pf = PutField(field)(addObjNode(), value)
    pf
  }

  def addBlockEnd(block: Block)(create: (ControlNode, Node) => BlockEnd): BlockEnd = {
    val prevOne = block.blockEnd
    val newOne = create(prevOne.inCtrl, prevOne.inMemory)

    (prevOne.exits.size, newOne.exits.size) match {
      case (0, 0) =>
        // nop

      case (1, 1) =>
        prevOne.exits.head.replaceUsesBy(newOne.exits.head)

      case _ =>
        notImplemented("hard block end replacement, implement if you need it")
    }

    block.blockEnd = newOne
    decommit(prevOne)
    newOne
  }

  def makeCond(blocks: Block*): Unit = {
    for (block <- blocks) {
      addCondition(block, addNode(block), addNode(block), Condition.EQ)
    }
  }

  def addCall() = {
    val methodRef = new FakeMethodReference(new FakeMethod(FakeMethodType.create(TKind.CLASS)))
    DirectCall(methodRef)()
  }

  def addInductiveVariable(start: Node, cond: Condition, limit: Node, step: Node): Node = {
    val tpe = start.tpe
    val phi = Phi.cyclic(tpe)(ctrl.block, phi => Seq(start, Add(phi, step)))
    val cmp = Cmp(tpe, cond)(phi, limit)
    ctrl.block.blockEnd.asInstanceOf[If].selector = cmp
    phi
  }

  def addInductiveVariable(header: Block, start: Node, cond: Condition, limit: Node, step: Node, incrementIsCompared: Boolean = false): Node = {
    val tpe = start.tpe
    var add: Node = null
    val phi = Phi.cyclic(tpe)(header, phi => Seq(start, Add(phi, step) tap (add = _)))
    val cmp = Cmp(tpe, cond)(if (incrementIsCompared) add else phi, limit)
    header.blockEnd.asInstanceOf[If].selector = cmp
    phi
  }

  def addInductiveVariableWithUse(header: Block, use: Block, start: Node, cond: Condition, limit: Node, step: Node, incrementIsCompared: Boolean = false): Node = {
    val tpe = start.tpe
    var add: Node = null
    val phi = Phi.cyclic(tpe)(header, phi => Seq(start, Add(phi, step) tap (add = _)))
    val cmp = Cmp(tpe, cond)(if (incrementIsCompared) add else phi, limit)
    use.blockEnd.asInstanceOf[If].selector = cmp
    phi
  }

  private def findNode(name: String) = globalNodes get name map (_.deref)

  protected implicit def String2Node(name: String): Node = n(name)

  // For ease of typing we want to type `n("x")` instead of `N("x")`.
  // For consistency we want to write `n("x") = ...` instead of `N("x") = ...`.
  // For gods of pattern matching we have to write `case N("x") =>` instead of `case n("x") =>`.
  // But we cannot have two objects with names `n` and `N` because of case-insensitive filesystems.
  // So we do it like this:
  protected object n {
    def apply(name: String) =
      findNode(name) getOrElse { shouldNotReachHere(s"unknown node $name") }

    def update(name: String, value: Node): Unit =
      globalNodes.put(name, value) ensuring (_.isEmpty)
  }
  protected object nExtractor {
    def unapply(x: Node): Option[String] = {
      // First try to find strict match.
      ScalaCollections.singleton(globalNodes collect { case (name, node) if node == x => name }) orElse
        ScalaCollections.singleton(globalNodes collect { case (name, node) if node.isReferentCommitted && node.deref == x => name })
    }
  }
  val N = nExtractor


  protected case class ParsingContext(getNodeByName: String => Option[Node], // find existing node
                                      parse: String => Option[Node], // try to parse new node
                                      linkArgs: (Node, Seq[(String, Int)]) => Unit
                                     )

  /** Attribute is a string of one of the following patterns:
    * {{{
    *   foo
    *   foo=func(bar,baz)
    *   func(bar,baz)
    * }}}
    */
  abstract class Attribute(funcName: String) {

    protected def parseAttribute(attr: String): Option[(Option[String], Option[(String, Seq[String])])] = {
      val Var = """([^=(),]+)""".r
      val Func = """([^=(),]+)=([^=(),]+)\((.*)\)""".r
      val Proc = """([^=(),]+)\((.*)\)""".r

      def split(argsJoined: String): Seq[String] = {
        val argsSplitted = mutable.Buffer.empty[String]

        var idx = 0
        var argStartIdx = 0

        def addArg() =
          argsSplitted += argsJoined.substring(argStartIdx, idx)

        var nesting = 0
        while (idx < argsJoined.length) {
          val ch = argsJoined(idx)
          if (ch == ',' && nesting == 0) {
            addArg()
            argStartIdx = idx + 1
          } else if (ch == '(') {
            nesting += 1
          } else if (ch == ')') {
            nesting -= 1
            require(nesting >= 0)
          }
          idx += 1
        }
        require(nesting == 0)
        if (argStartIdx < idx) {
          addArg()
        }
        argsSplitted.toSeq
      }

      condOpt(attr) {
        case Var(nodeName) => (Some(nodeName), None)
        case Func(nodeName, funcName, args) => (Some(nodeName), Some(funcName, split(args)))
        case Proc(funcName, args) => (None, Some(funcName, split(args)))
      }
    }

    protected def tryParseArgAsName(arg: String): Option[String] = {
      parseAttribute(arg) flatMap {
        case (Some(name), None) => Some(name)
        case _ => None
      }
    }

    protected def parseArgAsNode(ctx: ParsingContext, arg: String): Node = {
      tryParseArgAsName(arg) match {
        case Some(name) =>
          ctx.getNodeByName(name) getOrElse { shouldNotReachHere(s"couldn't resolve node $name") }
        case None =>
          ctx.parse(arg) getOrElse { shouldNotReachHere(s"couldn't parse $arg as node")}
      }
    }

    private[GlobalNodesBuilder] def parse(ctx: ParsingContext, attr: String) = {
      parseAttribute(attr) flatMap {
        case (Some(name), None) if funcName == null => Some((Some(name), handle(ctx, Seq())))
        case (name, Some((`funcName`, args))) => Some((name, handle(ctx, args)))
        case _ => None
      }
    }

    protected def handle(ctx: ParsingContext, args: Seq[String]): Option[Node]
  }

  abstract class ResolvingAttribute(funcName: String) extends Attribute(funcName) {
    protected override final def handle(ctx: ParsingContext, args: Seq[String]) = {
      handleResolved(ctx, args map (parseArgAsNode(ctx, _)))
    }

    protected def handleResolved(ctx: ParsingContext, args: Seq[Node]): Option[Node]
  }

  class SimpleAttribute(func: String)
                       (_makeNode: Seq[Node] => Node) extends ResolvingAttribute(func) {

    protected def handleResolved(ctx: ParsingContext, args: Seq[Node]): Option[Node] = {
      Some(_makeNode(args))
    }
  }

  class UnnamedAttribute(_makeNode: () => Node) extends SimpleAttribute(null)({ case Seq() => _makeNode() })

  class StringAttribute(func: String)(_makeNode: (String, Seq[Node]) => Node) extends Attribute(func) {
    def handle(ctx: ParsingContext, args: Seq[String]) = args match {
      case Seq(strArg, nodeArgs @ _*) => Some(_makeNode(strArg, nodeArgs map (parseArgAsNode(ctx, _))))
      case _ => shouldNotReachHere()
    }
  }

  class InductiveVariableAttribute(cond: Condition, incrementIsCompared: Boolean)
    extends SimpleAttribute(s"iv_$cond${if (incrementIsCompared) "_inc" else ""}")({
    case Seq(start, limit, step) => addInductiveVariable(ctrl.block, start, cond, limit, step, incrementIsCompared)
  })

  private val attributes = mutable.LinkedHashMap.empty[N, Seq[String]]


  override protected final def processAttributes(n: N, attrs: Seq[String]): Unit = {
    assert(!attributes.contains(n))
    attributes(n) = attrs
  }

  def parsableAttributes(): Seq[Attribute] = Seq(
    new UnnamedAttribute(() => addNode()),

    new SimpleAttribute("pinned"  )({ case Seq() => FakePinned(IntType)(ctrl.block) }),
    new SimpleAttribute("spinal"  )({ case Seq() => FakeSpinal(IntType)() }),
    new SimpleAttribute("xspinal" )({ case Seq() => FakeSpinalX(IntType)() }),
    new SimpleAttribute("catch"   )({ case Seq() => addCatch() }),
    new SimpleAttribute("coldcode")({ case Seq() => ColdCodeMarker() }),
    new SimpleAttribute("use"     )({ case Seq(v) => FakeSpinalUnary(v.tpe)(v) }),
    new SimpleAttribute("read"    )({ case Seq() => addGetField() }),
    new SimpleAttribute("write"   )({ case Seq() => addPutField(addNode()) }),
    new SimpleAttribute("cmp"     )({ case Seq(l, r) => Cmp(if (l != null) l.tpe else IntType, Condition.EQ)(l, r) }),
    new SimpleAttribute("cmpne"   )({ case Seq(l, r) => Cmp(if (l != null) l.tpe else IntType, Condition.NE)(l, r) }),
    new SimpleAttribute("if"      )({ case Seq(v) => setCondition(v); ctrl.block.blockEnd }),
    new SimpleAttribute("not"     )({ case Seq(v) => Not(v) }),
    new SimpleAttribute("add"     )({ case Seq(l, r) => Add(l, r) }),

    new SimpleAttribute("controlled")({
      case Seq() => FakeControlled(IntType)()
      case Seq(x) => FakeControlledUnary(IntType)(x)
    }),

    new SimpleAttribute("ret"     )({ case Seq(v) => replaceByReturn(ctrl.block.blockEnd.asInstanceOf[Return], v) }),
    new SimpleAttribute("if"      )({ case Seq(v) => ctrl.block.blockEnd.asInstanceOf[If] tap { _.selector = v } }),
    new SimpleAttribute("halt"    )({ case Seq() => replaceByHalt(ctrl.block.blockEnd) }),

    new StringAttribute("ic"      )({ case (arg, Seq()) => IConst(Integer.decode(arg)) }),
    new StringAttribute("lc"      )({ case (arg, Seq()) => LConst(java.lang.Long.decode(arg)) }),
    new StringAttribute("fc"      )({ case (arg, Seq()) => FConst(java.lang.Float.parseFloat(arg)) }),
    new StringAttribute("bc"      )({ case (arg, Seq()) => ConstCondition(java.lang.Boolean.parseBoolean(arg)) }),

    new SimpleAttribute("true"    )({ case Seq() => True() }),
    new SimpleAttribute("false"   )({ case Seq() => False() }),

    new Attribute("phi") {
      def handle(ctx: ParsingContext, args: Seq[String]) = {
        val argsNames = args map { tryParseArgAsName(_) match {
          case Some(name) => name
          case None => shouldNotReachHere("phi args must be simple vars not expressions")
        }}
        val tpes = argsNames flatMap ctx.getNodeByName map (_.tpe)
        val tpe = tpes reduce (_ | _) ensuring (_ != ValueType, tpes.mkString("[", ",", "]"))
        val node = Phi.raw(tpe)(ctrl.block +: Seq.fill(argsNames.size)(null): _*)
        ctx.linkArgs(node, argsNames.zipWithIndex) // arguments counted from zero because block is the implicit parameter of node
        Some(node)
      }
    },

    new SimpleAttribute("nc"      )({ case Seq(obj) => NullCheck(obj) }),
    new StringAttribute("clinit"  )({ case (t, Seq()) => Clinit(sym(t))() }),
    new StringAttribute("new"     )({ case (t, Seq()) => New(sig(t))() }),
    new StringAttribute("newarr"  )({ case (t, dims) => NewArray(sig(t))(dims: _*) }),
    new StringAttribute("cc"      )({ case (t, Seq(obj)) => CheckCast(sig(t))(obj) }),
    new StringAttribute("wc"      )({ case (t, Seq(obj)) => WeakCast(sym(t))(obj, WeakCast.NoCheck()) }),
    new StringAttribute("wcc"     )({ case (t, Seq(obj, check)) => WeakCast(sym(t))(obj, check) }),

  ) ++ Condition.values.toSeq.flatMap(c => Seq(new InductiveVariableAttribute(c, true), new InductiveVariableAttribute(c, false)))

  def parseAttributes(): Unit = {

    if (tieNodesInBackendOrder) {
      for (b <- all[Block]) {
        CodeOrder.append(b, b)
      }
    }

    if (attributes.nonEmpty) {
      // There would be some nodes with null arguments and some optimizations are not ready for it.
      withDeferredOnCommitOptimizations {

        object Parsing {
          val ctx = ParsingContext(findNode, parseOne(_)._2, linkArgs)

          val toLink = new mutable.LinkedHashMap[Node, collection.Seq[(String, Int)]]
          def linkArgs(node: Node, mapping: Seq[(String, Int)]): Unit = toLink(node) = mapping

          def parseOne(attr: String): (Option[String], Option[Node]) = {
            ScalaCollections.firstElement(parsableAttributes().iterator flatMap (_.parse(ctx, attr))) getOrElse {
              shouldNotReachHere(s"cannot parse attribute '$attr'")
            }
          }
        }

        for ((num, attrs) <- attributes.toSeq.sortBy(_._1)) {
          makeNodes { at =>
            val b: Block = num
            at(b)

            for (attr <- attrs) {
              val (nameOpt, nodeOpt) = Parsing.parseOne(attr)

              nodeOpt match {
                case Some(node) =>
                  for (name <- nameOpt) {
                    n(name) = node
                  }

                  if (tieNodesInBackendOrder) {
                    CodeOrder.append(node, b)
                  }

                case None =>
                  assert(nameOpt.isEmpty)
              }
            }
          }
        }

        for ((node, args) <- Parsing.toLink) {
          assert(!node.isCommitted)
          for ((name, idx) <- args) {
            node.updateArg(idx, n(name))
          }
          commit(node)
        }
        checkDefUseDominance()
      }
    }

    if (tieNodesInBackendOrder) {
      for (b <- all[Block]) {
        CodeOrder.append(b.blockEnd, b)
      }
    }
  }

  override protected final def makeCFG(start: SubGraph): Unit = {
    super.makeCFG(start)

    parseAttributes()
    attributes.clear()

    // This action is optional, we could leave conservative block memories. Add option for this if you need it.
    optimizeBlockMemory()
  }

  // Lazy values DSL

  case class LazyValue[+T](repr: String, value: () => T) extends (() => T) {
    override def apply() = value()
    override def toString = repr
  }
  def lazyValue[T](repr: String, value: => T) = LazyValue[T](repr, () => value)

}
