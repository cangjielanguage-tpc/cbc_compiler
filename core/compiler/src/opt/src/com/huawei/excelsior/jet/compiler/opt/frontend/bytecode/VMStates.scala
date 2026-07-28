/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.bytecode

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind.{DOUBLE, LONG}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.ContextTypesInParsing
import com.huawei.excelsior.jet.util.ScalaCollections


/**
 * Bytecode VMStates and their processing.
 *
 * According to JVM specification, any method has a frame that consists of local variables (an array of locals)
 * and operand stack. During execution of a method, the frame slots are changed by bytecode instructions.
 * For any point of method execution the values of its frame forms so called VM state -- state of local variables and
 * operand stack.
 *
 * During Java bytecode parsing, we do abstract interpretation of bytecode with IR nodes, and thus VM state of Java
 * bytecode abstract interpreter also contains IR nodes.
 *
 * This class provides methods for manipulating abstract interpreter VM state and also provides methods for
 * merging/resolving VM states (implements
 * [[com.huawei.excelsior.jet.compiler.opt.ir.AbstractInterpreterComponent.AbstractInterpreter.State AbstractInterpreter.State]]).
 *
 * @author paul
 * @author conwor
 * @author cypok
 */
trait VMStates { self: Universe =>

  // nodes for special verifier types
  // should not appear anywhere but VMState elements
  private val Long2   = VerificationNode.raw()
  private val Double2 = VerificationNode.raw()

  private def adjustValue(n: Node): Node =
    depriveIfNeeded(n)

  /**
   * VMState represents state of method's frame (bytecode stack & locals)
   * as IR values/nodes.
   *
   * @author paul
   * @author conwor
   */
  class VMState private (private val maxStack: Int,
                         private var locals: Array[NodeRef],
                         private var stack: List[NodeRef],
                         private var _xobj: NodeRef,
                         private var _stackDepth: Int,
                         _memory: NodeRef,
                         _contextTypes: ContextTypesMap)
      extends Scope.State(null, _memory, _contextTypes) {

    protected type This = VMState

    def stackDepth = _stackDepth

    def xobj = _xobj.deref
    def xobj_=(n: Node): Unit = {
      assert(n == Invalid || n.tpe.isTraceableRefType)
      _xobj = adjustValue(n)
    }

    override protected def forkImpl() = {
      new VMState(maxStack, locals, stack, xobj, stackDepth, memory,
        if (contextTypes != null) contextTypes.clone() else null)
    }

    override def makeUnreachableCopy() = {
      def unreachableValue(r: NodeRef) = r.deref match {
        case n: VerificationNode => n // cannot create NoValue(InvalidVerificationType), just use the same node
        case n => NoValue()
      }

      new VMState(maxStack,
        locals map unreachableValue,
        stack map unreachableValue,
        unreachableValue(_xobj),
        stackDepth,
        memory,
        ContextTypesMap.Unreachable)
    }

    /** Creates empty state */
    def this(memory: Node, maxLocals: Int, maxStack: Int) = {
      this(maxStack, Array.fill(maxLocals)(Invalid), Nil, Invalid, 0, memory,
        if (env.enabled(ContextTypesInParsing)) new ContextTypesMap() else null)
    }

    private def get2ndSlot(n: Node): Node = {
      n match
        case StackAlloc.DebugVar(t, info) => // rely on t instead of n.tpe
          if (t.jbcKind == LONG)   Long2 else
          if (t.jbcKind == DOUBLE) Double2 else
          null
        case _ =>
          if (n.tpe == LongType)   Long2 else
          if (n.tpe == DoubleType) Double2 else
          null
    }

    /** Returns `true` if node occupies two slots on local or stack frames.
     */
    private def is2Slots(n: Node): Boolean = (n != null && get2ndSlot(n) != null)

    private def is2ndSlot(n: Node): Boolean = n match {
      case Long2 => true
      case Double2 => true
      case _ => false
    }

    /** Returns `true`, if given node can be loaded from locals. */
    private def localCanBeLoaded(n: Node) = !is2ndSlot(n) && n != Invalid

    /** Get current value of i'th local */
    def apply(i: Int): Node = {
      val r = locals(i).deref
      assert(r != null)
      assert(localCanBeLoaded(r))

      if (genDebug) {
        r match {
          case StackAlloc.DebugVar(t, _) => return LoadMemory(t.toAsm, t, atomic = false)(r)
          case _ => // just skip, some aux locals without debug info and LVT record may appear
        }
      }

      if (is2Slots(r)) {
        val r2 = locals(i + 1)
        assert(is2ndSlot(r2.deref))
      }
      r
    }

    private def update0(i: Int, value: Node): Unit = {
      assert(value != null)
      if (locals(i) != value) {
        copyOnWrite()
        locals(i) = value
      }
    }

    /** Set value of i'th local */
    def update(i: Int, value: Node): Unit = {
      if (genDebug) {
        // If locals(i) is already a StackAlloc.DebugVar
        // then generate storeMem to put value into locals(i)
        val lNode = locals(i).deref
        lNode match {
          case StackAlloc.DebugVar(t, info) =>
            StoreMemory(t.toAsm, t, atomic = false)(lNode, value)
            return
          case _ => // just skip, some aux locals without debug info and LVT record may appear
        }
      }
      update0(i, adjustValue(value))
      if (is2Slots(value)) {
        update0(i + 1, get2ndSlot(value))
      }
      if (i > 0 && is2Slots(locals(i - 1).deref)) {
        update0(i - 1, Invalid)
      }
    }

    /** Pop a value from bytecode stack */
    private def pop0(): Node = {
      assert(stackDepth > 0)
      _stackDepth -= 1
      val head :: tail = stack
      stack = tail
      head.deref
    }

    /** Pop a value from bytecode stack */
    def pop(): Node = {
      val r = pop0()
      assert(r != null)
      assert(!is2ndSlot(r))
      if (is2Slots(r)) {
        val r2 = pop0()
        assert(is2ndSlot(r2))
      }
      r
    }

    /** Drop all values from bytecode stack */
    def dropStack(): this.type = {
      _stackDepth = 0
      stack = Nil
      this
    }

    /** Push value onto bytecode stack */
    def push(value: Node): Unit = {
      assert(value != null)
      if (is2Slots(value)) {
        push(get2ndSlot(value))
      }
      stack = adjustValue(value) :: stack
      assert(stackDepth < maxStack)
      _stackDepth += 1
    }

    /** Push values onto bytecode stack */
    def push(values: Node*): Unit = {
      values foreach push
    }

    override protected def copyOnWriteImpl(): Unit = {
      locals = locals.clone()
    }

    override def mergeFrom(block: Block, states: Seq[VMState], identity: Boolean)(mergeFunc: (Type, Seq[Node]) => Node): VMState = {
      assert(states forall { x =>
        x.locals.length == this.locals.length &&
          x.maxStack == this.maxStack &&
          x.stackDepth == this.stackDepth
      })

      /** Merge sequence of values by computing merged value's type and
        * invoking merge function `doMerge` to do actual merge.
        * Handle special cases when
        * - values are incompatible and cannot be merged
        * - there are any dead values in the sequence
        * - all values are the same and merge function of same values is identity
        * Used to implement both `Phi` and `Proxy` creation.
        */
      def mergeValues(values: Seq[Node]): Node = {
        // TODO: remove copy-paste with VarProcessor.SSACompleter.State.mergeFrom and CHIRParser.CHIRState.mergeFrom
        assert(!(values contains null))
        val tpe = {
          val mergedType = values map (_.tpe) reduce (_ | _)
          // Pretend that result is TRef if all values are null.
          if (mergedType == EopType.Null) EopType.Plain else mergedType
        }
        ScalaCollections.uniqueValue(values) match {
          case None if tpe eq ValueType => Invalid // Incompatible types.
          case Some(value) if tpe eq ValueType => value // Special nodes used during parsing (e.g. Long2, Double2).
          case Some(value) if identity => value
          case Some(value @ StackAlloc.DebugVar(_, _)) => value
          case _ => mergeFunc(tpe, values)
        }
      }

      if (!identity || states.exists(_.locals ne this.locals)) {
        this.copyOnWrite()
        for (i <- 0 until locals.length) {
          this.locals(i) = mergeValues(states map (_.locals(i).deref))
        }
      }

      def mergeStacks(xss: Seq[List[NodeRef]]): List[NodeRef] = xss.head match {
        case Nil => Nil
        case xs => if (identity && xss.forall(_ eq xs)) xs else
          mergeValues(xss map (_.head.deref)) :: mergeStacks(xss map (_.tail))
      }
      this.stack = mergeStacks(states map (_.stack))

      this.xobj = mergeValues(states map (_.xobj))

      super.mergeFrom(block, states, identity)(mergeFunc)
    }

    override def foreachPair(that: VMState)(action: (Node, Node) => Unit): Unit = {
      assert(this.locals.size == that.locals.size)
      assert(this.maxStack == that.maxStack)
      assert(this.stackDepth == that.stackDepth)

      for ((x, y) <- this.locals zip that.locals) action(x.deref, y.deref)
      for ((x, y) <- this.stack zip that.stack) action(x.deref, y.deref)
      action(this.xobj, that.xobj)

      super.foreachPair(that)(action)
    }
  }
}
