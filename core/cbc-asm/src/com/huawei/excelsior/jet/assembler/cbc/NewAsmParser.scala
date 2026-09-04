/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.*
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.BuiltinSignature.Nothing
import com.huawei.excelsior.jet.assembler.cbc.CbcTypeKind.INVALID
import com.huawei.excelsior.jet.assembler.cbc.FieldTag.{SlebConst, U64Const, UlebConst}
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.assembler.cbc.StackSlot.*
import com.huawei.excelsior.jet.assembler.cbc.Token.*
import com.huawei.excelsior.jet.assembler.cbc.Token.KeywordKind.{Type, *}
import com.huawei.excelsior.jet.assembler.cbc.Token.StructuralKind.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessAnalyzer
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.{LoadAccessKind, StoreAccessKind}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.LoadAccessKind.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.StoreAccessKind.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.{Assembler, ForkedAssembler, MemSpace}
import com.huawei.excelsior.jet.assembler.{AsmType, Label, Segment, Width}
import com.huawei.excelsior.jet.codeemitter.BranchOp
import xscala.io.*

import scala.PartialFunction.condOpt
import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import com.huawei.excelsior.jet.util.ScalaCollections

/**
  * Assembler that utilizes per-line tokenization.
 */
class NewAsmParser(builder: CbcFileFormat.Builder, val allLines: Seq[String]) {

  private type LineNumber = Int
  private case class Error(msg: String, ln: LineNumber, begin: Int, end: Int)
  private val errors = mutable.ArrayBuffer.empty[Error]

  private val fieldRefs = mutable.LinkedHashMap.empty[String, FieldReference]
  private val methodRefs = mutable.LinkedHashMap.empty[String, MethodReference]
  private val aotDatas = mutable.LinkedHashMap.empty[String, AotData]

  private var lineNumber: Int = 0

  def parse(): Boolean = {
    val lines = allLines.map(tokenize)

    val parsers = mutable.Stack.empty[Parser]
    parsers.push(TopLevel(builder))

    for ((line, idx) <- lines.zipWithIndex) {
      lineNumber = idx + 1
      try {
        parsers.top.parse(TokenStream(line, lineNumber)) match {
          case Push(p) =>
            parsers.push(p)
          case End =>
            parsers.pop()
            assert(parsers.nonEmpty)
          case Continue =>
        }
      } catch {
        case e: Throwable =>
          dumpErrors()
          stderr.println(s"Unrecoverable error during parsing at line $lineNumber")
          throw e
      }
    }
    if (errors.nonEmpty) {
      dumpErrors()
      false
    } else {
      true
    }
  }

  private def dumpErrors(): Unit = {
    for (error <- errors) {
      stderr.println(s"${error.ln}: ${error.msg} @ ${allLines(error.ln - 1).substring(error.begin, error.end)} @ ${error.begin}:${error.end}")
    }
  }

  private def currentLine: String = allLines(lineNumber - 1)

  private object Kw {
    def unapply(kw: Token): Option[KeywordKind] = condOpt(kw) {
      case x: Keyword => x.kw
    }
  }

  private object Ident {
    def unapply(kw: Token): Option[String] = condOpt(kw) {
      case x: Identifier => x.value
    }
  }

  private object Punct {
    def unapply(kw: Token): Option[StructuralKind] = condOpt(kw) {
      case x: Structural => x.sym
    }
  }

  private class TokenStream(var tokens: Seq[Token], val lineNumber: LineNumber) {

    def newError(msg: String): Error = {
      val (begin, end) = tokens match {
        case Seq(t: Eol) => (t.begin, t.end)
        case t +: tail => tokens = tail; (t.begin, t.end)
      }
      Error(msg, lineNumber, begin, end)
    }

    def newError(msg: String, tok: Token): Error = {
      Error(msg, lineNumber, tok.begin, tok.end)
    }

    def consume(): Token = tokens match {
      case Seq(t: Eol) => t
      case t +: tail => tokens = tail; t
    }

    def current: Token = tokens.head

    private def constructType(name: String, subtypes: Seq[Signature]): Signature = {
      consume() match {
        case Kw(Ref)    => TypeSignature(name, subtypes, true)
        case Kw(AotRef) => AotTypeSignature(name, subtypes, true)
        case Kw(Rec)    => TypeSignature(name, subtypes, false)
        case Kw(AotRec) => AotTypeSignature(name, subtypes, false)
        case Kw(KeywordKind.NullableOption) => OptionSignature(name, subtypes, true)
        case Kw(KeywordKind.UnionOption) => OptionSignature(name, subtypes, false)
        case t =>
          errors += newError("failed to parse type modifier", t)
          BuiltinSignature.Nothing
      }
    }

    def parseTypeSeq(): Seq[Signature] = {
      val list = mutable.ArrayBuffer.empty[Signature]
      while (!current.isEnd) {
        list += parseType()
      }
      list.toSeq
    }

    def parseType(): Signature = tokens match {
      case Kw(Void) +: tail     => tokens = tail; BuiltinSignature.Void
      case Kw(Unit) +: tail     => tokens = tail; BuiltinSignature.Unit
      case Kw(Boolean) +: tail  => tokens = tail; BuiltinSignature.Boolean
      case Kw(I8) +: tail       => tokens = tail; BuiltinSignature.I8
      case Kw(U8) +: tail       => tokens = tail; BuiltinSignature.U8
      case Kw(I16) +: tail      => tokens = tail; BuiltinSignature.I16
      case Kw(U16) +: tail      => tokens = tail; BuiltinSignature.U16
      case Kw(I32) +: tail      => tokens = tail; BuiltinSignature.I32
      case Kw(U32) +: tail      => tokens = tail; BuiltinSignature.U32
      case Kw(UChar32) +: tail  => tokens = tail; BuiltinSignature.UChar32
      case Kw(I64) +: tail      => tokens = tail; BuiltinSignature.I64
      case Kw(U64) +: tail      => tokens = tail; BuiltinSignature.U64
      case Kw(IAddr) +: tail    => tokens = tail; BuiltinSignature.IAddr
      case Kw(UAddr) +: tail    => tokens = tail; BuiltinSignature.UAddr
      case Kw(F16) +: tail      => tokens = tail; BuiltinSignature.F16
      case Kw(F32) +: tail      => tokens = tail; BuiltinSignature.F32
      case Kw(F64) +: tail      => tokens = tail; BuiltinSignature.F64

      case Kw(KeywordKind.Box) +: tail =>
        tokens = tail
        val args = parseTypeList(LBracket, RBracket)
        if (args.length != 1) {
          errors += newError("failed to parse box args", tail.head)
          BuiltinSignature.Nothing
        } else {
          CbcFileFormat.Box(args.head)
        }

      case Punct(LBracket) +: tail => Tuple(parseTypeList(LBracket, RBracket))
      case Punct(LParen) +: tail   => parseFunctional()

      case Punct(Percent) +: Token.IntegerLit(_, _, v) +: tail =>
        tokens = tail
        ClassTypeVariable(v.toInt)

      case Punct(Percent2) +: Token.IntegerLit(_, _, v) +: tail =>
        tokens = tail
        FuncTypeVariable(v.toInt)

      case Seq(Ident(str), Punct(LBracket), xs*) =>
        val subtypes = mutable.ArrayBuffer.empty[Signature]
        tokens = xs

        @tailrec
        def parseSubTypes(): Unit = {
          subtypes += parseType()
          tokens match {
            case Punct(RBracket) +: tail => tokens = tail
            case Punct(Comma) +: tail => tokens = tail; parseSubTypes()
            case _ =>
              errors += newError("failed to parse subtypes of term")
          }
        }

        parseSubTypes()
        constructType(str, subtypes.toSeq)

      case Seq(Ident(str), xs*) =>
        tokens = xs
        constructType(str, Seq.empty)
      case _ =>
        errors += newError("failed to parse type")
        BuiltinSignature.Nothing
    }

    def parseIdent(): String = tokens match {
      case Ident(str) +: tail => tokens = tail; str
      case _ =>
        errors += newError("failed to parse identifier")
        "<invalid>"
    }

    def expect(kind: StructuralKind | KeywordKind): Unit = current match {
      case Kw(k) if k == kind => consume()
      case Punct(k) if k == kind => consume()
      case _ =>
        errors += newError(s"Expected structural token $kind")
    }

    def expectMiddle(sk: StructuralKind | KeywordKind): Unit = {
      if (!current.isEnd) expect(sk)
    }

    private def parseMethodRefFlags(): MethodRefFlags = {
      if (current.is(LBracket)) {
        val flags = parseList(LBracket, RBracket) { () =>
          val t = consume()
          val res = t match {
            case Ident(str) => Flags.methodRefFlags.get(str)
            case t => None
          }
          if (res.isEmpty) {
            errors += newError("Unexpected flag", t)
          }
          res
        }
        MethodRefFlags(flags.flatten)
      } else {
        MethodRefFlags.empty
      }
    }

    private def parseAotData(): Option[AotData] = tokens match {
      case Punct(Hash) +: Ident(aotData) +: tail =>
        tokens = tail
        aotDatas.get(aotData) match {
          case Some(data) => Some(data)
          case _ =>
            errors += newError(s"Aot data $aotData not found")
            None
      }
      case _ => None
    }

    def parseList[T](open: StructuralKind, close: StructuralKind)(parseOne: () => T): Seq[T] = {
      expect(open)
      val args = mutable.ArrayBuffer.empty[T]
      while (!current.isEnd && !current.is(close)) {
        args += parseOne()
        if (!current.is(close)) {
          expect(Comma)
        }
      }
      expect(close)
      args.toSeq
    }

    def parseTypeList(open: StructuralKind, close: StructuralKind): Seq[Signature] = {
      parseList(open, close)(parseType)
    }

    def parseFunctional(): Functional = {
      val args = parseTypeList(LParen, RParen)
      val retType = parseType()
      Functional(args, retType)
    }

    def parseFieldReference(): FieldReference = {
      val refType = parseType()
      val name = parseIdent()
      val fieldType = parseType()
      val aotData = parseAotData()
      SingleFieldReference(refType, name, fieldType, aotData = aotData)
    }

    def parseMethodReference(): MethodReference = {
      val refType = parseType()
      val name = parseIdent()
      val signature = parseFunctional()
      val flags = parseMethodRefFlags()
      val aotData = parseAotData()
      MethodReference(name, refType, signature, flags, aotData = aotData)
    }

    def parse[T](expected: => String, default: => T)(pf: PartialFunction[Token, T]): T = {
      val tok = consume()
      if (pf.isDefinedAt(tok)) {
        pf.apply(tok)
      } else {
        errors += newError(s"failed to parse $expected", tok)
        default
      }
    }

    private def matchTypeKind(kw: KeywordKind) = kw match {
      case Boolean => CbcTypeKind.U8
      case I8 => CbcTypeKind.I8
      case U8 => CbcTypeKind.U8
      case I16 => CbcTypeKind.I16
      case U16 => CbcTypeKind.U16
      case I32 => CbcTypeKind.I32
      case U32 => CbcTypeKind.U32
      case UChar32 => CbcTypeKind.CHAR
      case I64 => CbcTypeKind.I64
      case U64 => CbcTypeKind.U64
      case IAddr => CbcTypeKind.IN
      case UAddr => CbcTypeKind.UN
      case F16 => CbcTypeKind.F16
      case F32 => CbcTypeKind.F32
      case F64 => CbcTypeKind.F64
      case _ => CbcTypeKind.INVALID
    }

    def parseTypeKind(): BuiltinSignature = {
      val tok = current
      parseType() match {
        case x: BuiltinSignature => x
        case _ =>
          errors += newError("Expected only builtin signatures", tok)
          BuiltinSignature.I64
      }
    }

    def parseIReg(): Register.IR = parse("ireg", Register.IR.IR1) {
      case Kw(k) if Conversion.iregs.contains(k) => Conversion.iregMapping(k)
    }

    def parseFReg(): Register.FR = parse("freg", Register.FR.FR0) {
      case Kw(k) if Conversion.fregs.contains(k) => Conversion.fregMapping(k)
    }

    def parseRef(): String = {
      expect(Hash)
      parse("reference", "<invalid>") {
        case Ident(str) => str
      }
    }

    def parseInt(): Long = parse("int", 0L) {
      case t: IntegerLit => t.value
    }

    def parseUntyped(): Untyped = parse("untyped", Untyped(0)) {
      case t: StackSlot => Untyped(t.value.toInt)
    }

    def parseTyped(): Typed = parse("typed", Typed(0)) {
      case t: StackSlot => Typed(t.value.toInt)
    }

    def parseStr(): String = parse("string", "<invalid>") {
      case t: StringLit => t.value
    }

    def parseFloat(): Double = parse("float", 0.0) {
      case t: FloatLit => t.value
    }

    def parseCC(): BranchOp = parse("branch op", BranchOp.EQ) {
      case Kw(kw) if Conversion.branchOpMapping.contains(kw) => Conversion.branchOpMapping(kw)
    }

    def parseAsmType(): AsmType = parse("asm type", AsmType.I64) {
      case Kw(kw) if Conversion.asmTypeMapping.contains(kw) => Conversion.asmTypeMapping(kw)
    }

    def parseStoreAccessKind(): StoreAccessKind = {
      val tok = current
      val ident = parseIdent()
      StoreAccessKind.values.find(_.toString == ident) match {
        case Some(stk) => stk
        case _ =>
          errors += newError("failed to parse load access kind", tok)
          StoreAccessKind.ST_8
      }
    }

    def parseLoadAccessKind(): LoadAccessKind = {
      val tok = current
      val ident = parseIdent()
      LoadAccessKind.values.find(_.toString == ident) match {
        case Some(ldk) => ldk
        case _ =>
          errors += newError("failed to parse load access kind", tok)
          LoadAccessKind.LD_U8
      }
    }
  }

  private def tokenize(s: String): Seq[Token] = AsmTokenizer(s)
    .tokenize()
    .filter(!_.isInstanceOf[Token.Trivia])

  private sealed trait Step
  private case object End extends Step
  private case object Continue extends Step
  private case class Push(p: Parser) extends Step

  private trait Parser {
    def parse(stream: TokenStream): Step
  }

  private class TopLevel(builder: CbcFileFormat.Builder) extends Parser {
    override def parse(stream: TokenStream): Step = stream.consume() match {
      case Kw(Type) =>
        val tb = builder.newTypeBuilder()
        val name = stream.parseIdent()
        tb.setName(name)
        Push(TypeParser(tb))
      case Kw(Cbcdeps) =>
        builder.setCbcDeps(stream.parseStr())
        Continue
      case Kw(Foreignlibs) =>
        builder.setForeignLibs(stream.parseStr())
        Continue
      case Kw(Aotdeps) =>
        builder.setAotDeps(stream.parseStr())
        Continue
      case Kw(Maintype) =>
        builder.setMainTypeName(stream.parseStr())
        Continue
      case Kw(AotDirect) =>
        val name = stream.parseIdent()
        stream.expect(Eq)
        aotDatas(name) = DirectCallAotData(stream.parseStr())
        Continue
      case Kw(AotVirtual) =>
        val name = stream.parseIdent()
        stream.expect(Eq)
        aotDatas(name) = VirtualCallAotData(stream.parseInt().toInt, stream.parseInt().toInt)
        Continue
      case Kw(AotInterface) =>
        val name = stream.parseIdent()
        stream.expect(Eq)
        aotDatas(name) = InterfaceCallAotData(stream.parseInt().toInt)
        Continue
      case Kw(AotStatic) =>
        val name = stream.parseIdent()
        stream.expect(Eq)
        aotDatas(name) = StaticFieldAotData(stream.parseStr())
        Continue
      case Kw(AotInstance) =>
        val name = stream.parseIdent()
        stream.expect(Eq)
        aotDatas(name) = InstanceFieldAotData(stream.parseInt().toInt)
        Continue
      case Kw(Fieldref) =>
        val name = stream.parseIdent()
        stream.expect(Eq)
        fieldRefs(name) = stream.parseFieldReference()
        Continue
      case Kw(Methodref) =>
        val name = stream.parseIdent()
        stream.expect(Eq)
        methodRefs(name) = stream.parseMethodReference()
        Continue
      case Kw(KeywordKind.End) =>
        End
      case t: Eol =>
        Continue // empty line
      case t =>
        errors += stream.newError(s"Unexpected top level token $t")
        Continue
    }
  }

  private class TypeParser(builder: CbcFileFormat.Type.Builder) extends Parser {

    private var constraints: Option[Seq[Signature]] = None

    override def parse(stream: TokenStream): Step = stream.consume() match {
      case Kw(KeywordKind.Flags) =>
        while (!stream.current.isEnd) {
          val t = stream.consume()
          t match {
            case Ident(str) => Flags.typeFlags.get(str) match {
              case Some(f) => builder.addFlag(f)
              case _ =>
                errors += stream.newError("Unexpected flag", t)
            }
            case t =>
              errors += stream.newError("Unexpected flag", t)
          }
        }
        Continue
      case Kw(Constraints) =>
        val constraints = mutable.ArrayBuffer.empty[Signature]
        while (!stream.current.isEnd) {
          constraints += stream.parseType()
        }
        val res = constraints.toSeq
        this.constraints = Some(res)
        builder.setGenericConstraints(res)
        Continue
      case Kw(TypeVars) =>
        val tok = stream.current
        val value = stream.parseInt().toInt
        if (constraints.isEmpty) {
          // do not override constraints written previously
          builder.setGenericConstraints(Seq.fill(value)(BuiltinSignature.Nil))
        } else if (constraints.isDefined && value != constraints.get.length) {
          errors += stream.newError("length mismatch", tok)
        }
        Continue
      case Kw(Super) =>
        builder.setSuperOrEnumType(stream.parseType())
        Continue
      case Kw(Enum) =>
        builder.setSuperOrEnumType(stream.parseType())
        Continue
      case Kw(Interfaces) =>
        builder.setInterfaces(stream.parseTypeSeq())
        Continue
      case Kw(UnionFields) =>
        builder.setUnionFields(stream.parseTypeSeq())
        Continue
      case Kw(EnumKind) =>
        val tok = stream.current
        val name = stream.parseIdent()
        TypeEnumKind.values.find(_.toString == name) match {
          case Some(kind) => builder.setEnumKind(kind)
          case _ => errors += stream.newError("unexpected enum kind", tok)
        }
        Continue
      case Kw(KeywordKind.Method) =>
        val mb = builder.newMethodBuilder()
        mb.setName(stream.parseIdent())
        mb.setSignature(stream.parseFunctional())
        builder.getName.foreach(mb.setTypeName)
        Push(MethodParser(mb))
      case Kw(KeywordKind.Field) =>
        val fb = builder.newFieldBuilder()
        fb.setName(stream.parseIdent())
        fb.setFieldType(stream.parseType())
        Push(FieldParser(fb))
      case Kw(KeywordKind.End) =>
        End
      case t: Eol =>
        Continue // empty line
      case t =>
        errors += stream.newError(s"Unexpected token in type definition $t")
        Continue
    }
  }

  private class MethodParser(builder: CbcFileFormat.Method.Builder) extends Parser {

    private def or(a: Int, b: Int) = a | b

    override def parse(stream: TokenStream) = stream.consume() match {
      case Kw(KeywordKind.Flags) =>
        while (!stream.current.isEnd) {
          val t = stream.consume()
          t match {
            case Ident(str) => Flags.methodFlags.get(str) match {
              case Some(f) => builder.addFlag(f)
              case _ =>
                errors += stream.newError("Unexpected flag", t)
            }
            case t =>
              errors += stream.newError("Unexpected flag", t)
          }
        }
        Continue
      case Kw(KeywordKind.UntypedCount) =>
        val count = stream.consume() match {
          case lit: IntegerLit => lit.value.intValue
          case t =>
            errors += stream.newError(s"Expected integer literal, but found $t")
            0
        }
        builder.getCodeBuilder().setUntypedStackSlotsCount(count)
        Continue
      case Kw(KeywordKind.TypedSlots) =>
        val types = mutable.ArrayBuffer.empty[Signature]
        while (!stream.current.isEnd) {
          types += stream.parseType()
          stream.expectMiddle(Comma)
        }
        builder.getCodeBuilder().setStackAllocatedTypeSigs(types.toSeq)
        Continue
      case Kw(KeywordKind.SavedIregs) =>
        val iregs = mutable.ArrayBuffer.empty[IR]
        while (!stream.current.isEnd) {
          iregs += stream.parseIReg()
          stream.expectMiddle(Comma)
        }
        val mask = iregs.map(1 << _.nonVolIdx).fold(0)(or)
        builder.getCodeBuilder().setUsedNonVolIRegsMask(mask)
        Continue
      case Kw(KeywordKind.SavedFregs) =>
        val fregs = mutable.ArrayBuffer.empty[FR]
        while (!stream.current.isEnd) {
          fregs += stream.parseFReg()
          stream.expectMiddle(Comma)
        }
        val mask = fregs.map(1 << _.nonVolIdx).fold(0)(or)
        builder.getCodeBuilder().setUsedNonVolFRegsMask(mask)
        Continue
      case Kw(KeywordKind.Code) =>
        Push(CodeParser(builder.getCodeBuilder()))
      case Kw(KeywordKind.MethodTypeName) =>
        val name = stream.parseStr()
        builder.setTypeName(name)
        Continue
      case Kw(KeywordKind.Link) =>
        val link = stream.parseStr()
        builder.setLinkageName(link)
        Continue
      case Kw(KeywordKind.End) =>
        End
      case t: Eol =>
        Continue // empty line
      case t =>
        errors += stream.newError(s"Unexpected token in type definition $t")
        Continue
    }
  }

  private final class Arguments(stream: TokenStream, labels: Labels) extends ArgStream {
    private lazy val fakeMethodRef =
      MethodReference("", Nothing, Functional(Seq.empty, Nothing), MethodRefFlags.empty)
    private lazy val fakeFieldRef =
      SingleFieldReference(Nothing, "", Nothing)

    def comma[T](t: T): T = {
      stream.expectMiddle(Comma)
      t
    }

    def freg: Register.FR = comma(stream.parseFReg())
    def ireg: Register.IR = comma(stream.parseIReg())
    def method: MethodReference = comma(methodRefs.getOrElse(stream.parseRef(), fakeMethodRef))
    def field: FieldReference = comma(fieldRefs.getOrElse(stream.parseRef(), fakeFieldRef))
    def tpe: Signature = comma(stream.parseType())
    def int: Long = comma(stream.parseInt())
    def us: Untyped = comma(stream.parseUntyped())
    def ts: Typed = comma(stream.parseTyped())
    def str: String = comma(stream.parseStr())
    def float: Double = comma(stream.parseFloat())
    def label: Label = comma(labels.get(stream.parseIdent()))
    def cc: BranchOp = comma(stream.parseCC())
    def asmType: AsmType = comma(stream.parseAsmType())
    def tk: BuiltinSignature = comma(stream.parseTypeKind())
    def ldk: LoadAccessKind = comma(stream.parseLoadAccessKind())
    def stk: StoreAccessKind = comma(stream.parseStoreAccessKind())

    def fields: Seq[FieldReference] = {
      val frs = mutable.ArrayBuffer.empty[FieldReference]
      while (!stream.current.isEnd) {
        frs += fieldRefs.getOrElse(stream.parseRef(), fakeFieldRef)
        if (stream.current.is(Comma)) stream.consume()
      }
      frs.toSeq
    }
  }

  private trait Labels {
    def get(str: String): Label
  }

  private class LabelsMap(segment: Segment) extends Labels {
    private val map = mutable.LinkedHashMap.empty[String, Label]
    def get(str: String): Label = map.getOrElseUpdate(str, segment.newLabel)
  }

  private class CodeParser(builder: MethodCode.Builder) extends Parser {
    private val segment = new Segment()
    private val analyzer = new LivenessAnalyzer
    private val gen = {
      val gen = new CodeGenerator()
      gen.analyzer = analyzer
      gen.setUp(segment)
      gen
    }

    private val labels = LabelsMap(segment)

    private def slots(stream: TokenStream): Seq[Register.IR | Untyped] = {
      val buf = mutable.ArrayBuffer.empty[Register.IR | Untyped]
      while (!stream.current.isEnd) {
        stream.consume() match {
          case Kw(k) if Conversion.iregs.contains(k) => buf += Conversion.iregMapping(k)
          case Token.StackSlot(_, _, value) => buf += Untyped(value.toInt)
          case t =>
            errors += stream.newError(s"failed to parse ireg or uts", t)
        }
        if (stream.current.is(Comma)) {
          // allow lists without comma separator
          stream.consume()
        }
      }
      buf.toSeq
    }

    override def parse(stream: TokenStream): Step = {
      val tok = stream.consume()
      tok match {
        case Ident("ms.hd.obj") =>
          val args = Arguments(stream, labels)
          Push(MemSpaceCodeParser(MemSpace.Builder().obj(args.ireg), gen))
        case Ident("ms.hd.rec") =>
          val args = Arguments(stream, labels)
          Push(MemSpaceCodeParser(MemSpace.Builder().rec(args.ireg), gen))
        case Ident("ms.hd.typed") =>
          val args = Arguments(stream, labels)
          Push(MemSpaceCodeParser(MemSpace.Builder().typed(args.ts), gen))
        case Ident("ms.hd.static") =>
          val args = Arguments(stream, labels)
          Push(MemSpaceCodeParser(MemSpace.Builder().static(args.field), gen))
        case Ident(labelName) if labelName.endsWith(":") =>
          stream.consume()
          gen.bind(labels.get(labelName.stripSuffix(":")))
          parse(stream)
        case Ident(instrName) =>
          InstructionParser.invoke(instrName, gen, Arguments(stream, labels), onError = () => {
            errors += stream.newError(s"failed to find instruction $instrName", tok)
          })
          Continue
        case Kw(KeywordKind.LivePrim) =>
          analyzer.op(slots(stream).foreach(analyzer.prim))
          Continue
        case Kw(KeywordKind.LiveRec) =>
          analyzer.op(slots(stream).foreach(analyzer.rec))
          Continue
        case Kw(KeywordKind.LiveRef) =>
          analyzer.op(slots(stream).foreach(analyzer.ref))
          Continue
        case Kw(KeywordKind.Dead) =>
          slots(stream).foreach(analyzer.dead)
          Continue
        case Kw(KeywordKind.End) =>
          builder.setSegment(segment)
          builder.setLiveness(gen.collectLiveness)
          End
        case t: Eol =>
          Continue // empty line
        case t =>
          errors += stream.newError(s"Unexpected token in type definition $t")
          Continue
      }
    }
  }

  private class MemSpaceCodeParser(builder: MemSpace.Builder, gen: Assembler) extends Parser {
    private val nolabels = new Labels() {
      def get(str: String): Label = shouldNotReachHere("label usage in memspace operation")
    }

    override def parse(stream: NewAsmParser.this.TokenStream): Step = {
      val tok = stream.consume()
      tok match {
        case Ident(instrName) =>
          val foundTail = InstructionParser.memInvoke(instrName, builder, gen, Arguments(stream, nolabels), onError = () => {
            errors += stream.newError(s"failed to find instruction $instrName", tok)
          })
          if (foundTail) {
            End
          } else {
            Continue
          }
        case t =>
          errors += stream.newError(s"Unexpected token in memspace $t")
          Continue
      }
    }
  }

  private class FieldParser(builder: CbcFileFormat.Field.Builder) extends Parser {
    override def parse(stream: TokenStream) = stream.consume() match {
      case Kw(KeywordKind.Flags) =>
        while (!stream.current.isEnd) {
          val t = stream.consume()
          t match {
            case Ident(str) => Flags.fieldFlags.get(str) match {
              case Some(f) => builder.addFlag(f)
              case _ =>
                errors += stream.newError("Unexpected flag", t)
            }
            case t =>
              errors += stream.newError("Unexpected flag", t)
          }
        }
        Continue
      case Kw(KeywordKind.Fieldval) =>
        val kind = stream.parseIdent()
        val const = stream.consume() match {
          case lit: IntegerLit => lit.value
          case t =>
            errors += Error("Expected const value", lineNumber, t.begin, t.end)
            0
        }
        kind match {
          case "sleb" => builder.setConstValue(SlebConst, const)
          case "uleb" => builder.setConstValue(UlebConst, const)
          case "u64"  => builder.setConstValue(U64Const, const)
        }
        Continue
      case Kw(KeywordKind.End) =>
        End
      case t: Eol =>
        Continue // empty line
      case t =>
        errors += stream.newError(s"Unexpected token in type definition $t")
        Continue
    }
  }
}

private object Flags {
  private def makeFlags[T](flags: Iterable[T]): scala.collection.immutable.Map[String, T] =
    flags.map(t => (t.toString, t)).toMap

  val typeFlags      = TypeFlag.values.map(v => (v.toString, v)).toMap
  val methodFlags    = MethodFlag.values.map(v => (v.toString, v)).toMap
  val methodRefFlags = MethodRefFlag.values.map(v => (v.toString, v)).toMap
  val fieldFlags     = FieldFlag.values.map(v => (v.toString, v)).toMap
}

private object Conversion {
  val iregs = Seq(IRZ, IR1, IR2, IR3, IR4, IR5, IR6, IR7, IR8, IR9, IR10, IR11, IR12, IR13)
  val fregs = Seq(FR0, FR1, FR2, FR3, FR4, FR5, FR6, FR7, FR8, FR9, FR10, FR11, FR12, FR13, FR14, FR15)
  val asmTypes = Seq(I8, U8, I16, U16, I32, U32, I64, U64, F16, F32, F64, PTR)
  val branchOps = Seq(EQ, NE, GE, GT, LT, LE, REQ, RNE, UGE, UGT, ULT, ULE, TESTZ, TESTNZ, TESTBIT, TESTNBIT,
                      FEQ, FNE, FLT, FNLT, FGT, FNGT, FGE, FNGE, FLE, FNLE)

  val iregMapping = ScalaCollections.mapWith(iregs)(x => Register.IR.valueOf(x.str))
  val fregMapping = ScalaCollections.mapWith(fregs)(x => Register.FR.valueOf(x.str))

  val asmTypeMapping = ScalaCollections.mapWith(asmTypes)(x => AsmType.valueOf(x.str))
  val branchOpMapping = ScalaCollections.mapWith(branchOps)(x => BranchOp.valueOf(x.str))
}

private trait ArgStream {
  def freg: Register.FR
  def ireg: Register.IR
  def method: MethodReference
  def field: FieldReference
  def fields: Seq[FieldReference]
  def tpe: Signature
  def int: Long
  def us: Untyped
  def ts: Typed
  def str: String
  def float: Double
  def label: Label
  def cc: BranchOp
  def asmType: AsmType
  def tk: BuiltinSignature
  def ldk: LoadAccessKind
  def stk: StoreAccessKind
}

private object ArgParseError extends Throwable

private object InstructionParser {
  private case class Instruction(name: String, routine: (Assembler, ArgStream) => Unit)
  private case class MemInstruction(name: String, routine: (MemSpace.Builder, ArgStream) => Option[MemSpace.Chain])
  private val instructions = mutable.HashMap.empty[String, Instruction]
  private val memInstructions = mutable.HashMap.empty[String, MemInstruction]

  private def instr(name: String)(routine: (Assembler, ArgStream) => Unit): Unit =
    instructions.put(name, Instruction(name, routine)).ensuring(_.isEmpty, s"duplicate instruction $name")

  private def memInstr(name: String)(routine: (MemSpace.Builder, ArgStream) => Unit): Unit =
    memInstructions.put(name, MemInstruction(name, (b, args) => {
      routine(b, args)
      None
    })).ensuring(_.isEmpty, s"duplicate instruction $name")

  private def tailInstr(name: String)(routine: (MemSpace.Builder, ArgStream) => MemSpace.Chain): Unit =
    memInstructions.put(name, MemInstruction(name, (b, args) => {
      Some(routine(b, args))
    })).ensuring(_.isEmpty, s"duplicate instruction $name")

  def invoke(name: String, asm: Assembler, args: ArgStream, onError: () => Unit): Unit = instructions.get(name) match {
    case Some(instr) => instr.routine(asm, args)
    case _ => onError()
  }

  def memInvoke(name: String, builder: MemSpace.Builder, asm: Assembler, args: ArgStream, onError: () => Unit): Boolean = memInstructions.get(name) match {
    case Some(instr) =>
      instr.routine(builder, args) match {
        case Some(chain) => chain.gen(asm); true
        case _ => false
      }
    case _ => onError(); false
  }

  // movs
  instr("movi.32")  { (a, s) => a.movi32(s.ireg, s.int.toInt) }
  instr("movi.64")  { (a, s) => a.movi64(s.ireg, s.int) }
  instr("fmovi.32") { (a, s) => a.fmovi(s.freg, s.float, Width.W32) }
  instr("fmovi.64") { (a, s) => a.fmovi(s.freg, s.float, Width.W64) }
  instr("mov.32")    { (a, s) => a.mov(s.ireg, s.ireg, W32) }
  instr("fmov.32")   { (a, s) => a.fmov(s.freg, s.freg, W32) }
  instr("movi2f.32") { (a, s) => a.movi2f(s.freg, s.ireg, W32) }
  instr("movf2i.32") { (a, s) => a.movf2i(s.ireg, s.freg, W32) }
  instr("mov.64")    { (a, s) => a.mov(s.ireg, s.ireg, W64) }
  instr("fmov.64")   { (a, s) => a.fmov(s.freg, s.freg, W64) }
  instr("movi2f.64") { (a, s) => a.movi2f(s.freg, s.ireg, W64) }
  instr("movf2i.64") { (a, s) => a.movf2i(s.ireg, s.freg, W64) }
  instr("mov.ref")   { (a, s) => a.movRef(s.ireg, s.ireg) }

  // conversions
  instr("i2i") { (a, s) => a.convertFromTo(s.asmType, s.asmType, s.ireg, s.ireg) }
  instr("i2f") { (a, s) => a.convertFromTo(s.asmType, s.asmType, s.ireg, s.freg) }
  instr("f2i") { (a, s) => a.convertFromTo(s.asmType, s.asmType, s.freg, s.ireg) }
  instr("f2f") { (a, s) => a.convertFromTo(s.asmType, s.asmType, s.freg, s.freg) }

  // 32-bit integer arithmetic
  instr("add.32")  { (a, s) => a.add(Width.W32, s.ireg, s.ireg, s.ireg) }
  instr("sub.32")  { (a, s) => a.sub(Width.W32, s.ireg, s.ireg, s.ireg) }
  instr("mul.32")  { (a, s) => a.mul(Width.W32, s.ireg, s.ireg, s.ireg) }
  instr("and.32")  { (a, s) => a.and(Width.W32, s.ireg, s.ireg, s.ireg) }
  instr("or.32")   { (a, s) => a.or(Width.W32, s.ireg, s.ireg, s.ireg) }
  instr("xor.32")  { (a, s) => a.xor(Width.W32, s.ireg, s.ireg, s.ireg) }
  instr("div.32")  { (a, s) => a.div(Width.W32, s.ireg, s.ireg, s.ireg) }
  instr("rem.32")  { (a, s) => a.rem(Width.W32, s.ireg, s.ireg, s.ireg) }
  instr("udiv.32") { (a, s) => a.udiv(Width.W32, s.ireg, s.ireg, s.ireg) }
  instr("urem.32") { (a, s) => a.urem(Width.W32, s.ireg, s.ireg, s.ireg) }
  instr("lsl.32")  { (a, s) => a.lsl(Width.W32, s.ireg, s.ireg, s.ireg) }
  instr("lsr.32")  { (a, s) => a.lsr(Width.W32, s.ireg, s.ireg, s.ireg) }
  instr("asr.32")  { (a, s) => a.asr(Width.W32, s.ireg, s.ireg, s.ireg) }

  // 64-bit integer arithmetic
  instr("add.64")  { (a, s) => a.add(Width.W64, s.ireg, s.ireg, s.ireg) }
  instr("sub.64")  { (a, s) => a.sub(Width.W64, s.ireg, s.ireg, s.ireg) }
  instr("mul.64")  { (a, s) => a.mul(Width.W64, s.ireg, s.ireg, s.ireg) }
  instr("and.64")  { (a, s) => a.and(Width.W64, s.ireg, s.ireg, s.ireg) }
  instr("or.64")   { (a, s) => a.or(Width.W64, s.ireg, s.ireg, s.ireg) }
  instr("xor.64")  { (a, s) => a.xor(Width.W64, s.ireg, s.ireg, s.ireg) }
  instr("div.64")  { (a, s) => a.div(Width.W64, s.ireg, s.ireg, s.ireg) }
  instr("rem.64")  { (a, s) => a.rem(Width.W64, s.ireg, s.ireg, s.ireg) }
  instr("udiv.64") { (a, s) => a.udiv(Width.W64, s.ireg, s.ireg, s.ireg) }
  instr("urem.64") { (a, s) => a.urem(Width.W64, s.ireg, s.ireg, s.ireg) }
  instr("lsl.64")  { (a, s) => a.lsl(Width.W64, s.ireg, s.ireg, s.ireg) }
  instr("lsr.64")  { (a, s) => a.lsr(Width.W64, s.ireg, s.ireg, s.ireg) }
  instr("asr.64")  { (a, s) => a.asr(Width.W64, s.ireg, s.ireg, s.ireg) }

  // neg
  instr("neg.32") { (a, s) => a.neg(s.ireg, s.ireg, Width.W32) }
  instr("neg.64") { (a, s) => a.neg(s.ireg, s.ireg, Width.W64) }

  // 32-bit integer immediate arithmetic
  instr("addi.32")  { (a, s) => a.addi(Width.W32, s.ireg, s.ireg, s.int) }
  instr("subi.32")  { (a, s) => a.subi(Width.W32, s.ireg, s.ireg, s.int) }
  instr("muli.32")  { (a, s) => a.muli(Width.W32, s.ireg, s.ireg, s.int) }
  instr("andi.32")  { (a, s) => a.andi(Width.W32, s.ireg, s.ireg, s.int) }
  instr("ori.32")   { (a, s) => a.ori(Width.W32, s.ireg, s.ireg, s.int) }
  instr("xori.32")  { (a, s) => a.xori(Width.W32, s.ireg, s.ireg, s.int) }
  instr("divi.32")  { (a, s) => a.divi(Width.W32, s.ireg, s.ireg, s.int) }
  instr("remi.32")  { (a, s) => a.remi(Width.W32, s.ireg, s.ireg, s.int) }
  instr("udivi.32") { (a, s) => a.udivi(Width.W32, s.ireg, s.ireg, s.int) }
  instr("uremi.32") { (a, s) => a.uremi(Width.W32, s.ireg, s.ireg, s.int) }
  instr("lsli.32")  { (a, s) => a.lsli(Width.W32, s.ireg, s.ireg, s.int) }
  instr("lsri.32")  { (a, s) => a.lsri(Width.W32, s.ireg, s.ireg, s.int) }
  instr("asri.32")  { (a, s) => a.asri(Width.W32, s.ireg, s.ireg, s.int) }

  // 64-bit integer immediate arithmetic
  instr("addi.64")  { (a, s) => a.addi(Width.W64, s.ireg, s.ireg, s.int) }
  instr("subi.64")  { (a, s) => a.subi(Width.W64, s.ireg, s.ireg, s.int) }
  instr("muli.64")  { (a, s) => a.muli(Width.W64, s.ireg, s.ireg, s.int) }
  instr("andi.64")  { (a, s) => a.andi(Width.W64, s.ireg, s.ireg, s.int) }
  instr("ori.64")   { (a, s) => a.ori(Width.W64, s.ireg, s.ireg, s.int) }
  instr("xori.64")  { (a, s) => a.xori(Width.W64, s.ireg, s.ireg, s.int) }
  instr("divi.64")  { (a, s) => a.divi(Width.W64, s.ireg, s.ireg, s.int) }
  instr("remi.64")  { (a, s) => a.remi(Width.W64, s.ireg, s.ireg, s.int) }
  instr("udivi.64") { (a, s) => a.udivi(Width.W64, s.ireg, s.ireg, s.int) }
  instr("uremi.64") { (a, s) => a.uremi(Width.W64, s.ireg, s.ireg, s.int) }
  instr("lsli.64")  { (a, s) => a.lsli(Width.W64, s.ireg, s.ireg, s.int) }
  instr("lsri.64")  { (a, s) => a.lsri(Width.W64, s.ireg, s.ireg, s.int) }
  instr("asri.64")  { (a, s) => a.asri(Width.W64, s.ireg, s.ireg, s.int) }

  // 32-bit checked arithmetic
  instr("cadd.32")  { (a, s) => a.cadd(s.ireg, s.ireg, s.ireg, Width.W32) }
  instr("csub.32")  { (a, s) => a.csub(s.ireg, s.ireg, s.ireg, Width.W32) }
  instr("cmul.32")  { (a, s) => a.cmul(s.ireg, s.ireg, s.ireg, Width.W32) }
  instr("cdiv.32")  { (a, s) => a.cdiv(s.ireg, s.ireg, s.ireg, Width.W32) }
  instr("cuadd.32") { (a, s) => a.cuadd(s.ireg, s.ireg, s.ireg, Width.W32) }
  instr("cusub.32") { (a, s) => a.cusub(s.ireg, s.ireg, s.ireg, Width.W32) }
  instr("cumul.32") { (a, s) => a.cumul(s.ireg, s.ireg, s.ireg, Width.W32) }

  // 64-bit checked arithmetic
  instr("cadd.64")  { (a, s) => a.cadd(s.ireg, s.ireg, s.ireg, Width.W64) }
  instr("csub.64")  { (a, s) => a.csub(s.ireg, s.ireg, s.ireg, Width.W64) }
  instr("cmul.64")  { (a, s) => a.cmul(s.ireg, s.ireg, s.ireg, Width.W64) }
  instr("cdiv.64")  { (a, s) => a.cdiv(s.ireg, s.ireg, s.ireg, Width.W64) }
  instr("cuadd.64") { (a, s) => a.cuadd(s.ireg, s.ireg, s.ireg, Width.W64) }
  instr("cusub.64") { (a, s) => a.cusub(s.ireg, s.ireg, s.ireg, Width.W64) }
  instr("cumul.64") { (a, s) => a.cumul(s.ireg, s.ireg, s.ireg, Width.W64) }
  instr("cpow.64")  { (a, s) => a.cpow(s.ireg, s.ireg, s.ireg, Width.W64) }

  // 32-bit checked immediate arithmetic
  instr("caddi.32")  { (a, s) => a.caddi(s.ireg, s.ireg, s.int, Width.W32) }
  instr("csubi.32")  { (a, s) => a.csubi(s.ireg, s.ireg, s.int, Width.W32) }
  instr("cmuli.32")  { (a, s) => a.cmuli(s.ireg, s.ireg, s.int, Width.W32) }
  instr("cuaddi.32") { (a, s) => a.cuaddi(s.ireg, s.ireg, s.int, Width.W32) }
  instr("cusubi.32") { (a, s) => a.cusubi(s.ireg, s.ireg, s.int, Width.W32) }
  instr("cumuli.32") { (a, s) => a.cumuli(s.ireg, s.ireg, s.int, Width.W32) }

  // 64-bit checked immediate arithmetic
  instr("caddi.64")  { (a, s) => a.caddi(s.ireg, s.ireg, s.int, Width.W64) }
  instr("csubi.64")  { (a, s) => a.csubi(s.ireg, s.ireg, s.int, Width.W64) }
  instr("cmuli.64")  { (a, s) => a.cmuli(s.ireg, s.ireg, s.int, Width.W64) }
  instr("cuaddi.64") { (a, s) => a.cuaddi(s.ireg, s.ireg, s.int, Width.W64) }
  instr("cusubi.64") { (a, s) => a.cusubi(s.ireg, s.ireg, s.int, Width.W64) }
  instr("cumuli.64") { (a, s) => a.cumuli(s.ireg, s.ireg, s.int, Width.W64) }
  instr("cpowi.64")  { (a, s) => a.cpowi(s.ireg, s.ireg, s.int, Width.W64) }

  // 32-bit float arithmetic
  instr("fadd.32") { (a, s) => a.fadd(Width.W32, s.freg, s.freg, s.freg) }
  instr("fsub.32") { (a, s) => a.fsub(Width.W32, s.freg, s.freg, s.freg) }
  instr("fmul.32") { (a, s) => a.fmul(Width.W32, s.freg, s.freg, s.freg) }
  instr("fdiv.32") { (a, s) => a.fdiv(Width.W32, s.freg, s.freg, s.freg) }

  // 64-bit float arithmetic
  instr("fadd.64") { (a, s) => a.fadd(Width.W64, s.freg, s.freg, s.freg) }
  instr("fsub.64") { (a, s) => a.fsub(Width.W64, s.freg, s.freg, s.freg) }
  instr("fmul.64") { (a, s) => a.fmul(Width.W64, s.freg, s.freg, s.freg) }
  instr("fdiv.64") { (a, s) => a.fdiv(Width.W64, s.freg, s.freg, s.freg) }

  // float unary
  instr("fneg.32")  { (a, s) => a.fneg(s.freg, s.freg, Width.W32) }
  instr("fabs.32")  { (a, s) => a.fabs(s.freg, s.freg, Width.W32) }
  instr("fsqrt.32") { (a, s) => a.fsqrt(s.freg, s.freg, Width.W32) }
  instr("fneg.64")  { (a, s) => a.fneg(s.freg, s.freg, Width.W64) }
  instr("fabs.64")  { (a, s) => a.fabs(s.freg, s.freg, Width.W64) }
  instr("fsqrt.64") { (a, s) => a.fsqrt(s.freg, s.freg, Width.W64) }

  // ret
  instr("ret.32")  { (a, s) => a.ret(s.ireg, Width.W32) }
  instr("ret.64")  { (a, s) => a.ret(s.ireg, Width.W64) }
  instr("ret.ref") { (a, s) => a.retRef(s.ireg) }
  instr("fret.32") { (a, s) => a.fret(s.freg, Width.W32) }
  instr("fret.64") { (a, s) => a.fret(s.freg, Width.W64) }

  // branches
  instr("bcc.64")  { (a, s) => a.doBcc(s.cc, s.ireg, s.ireg, Width.W64, s.label, wide = false) }
  instr("fbcc.64") { (a, s) => a.doBcc(s.cc, s.freg, s.freg, Width.W64, s.label, wide = false) }
  instr("bcci.64") { (a, s) => a.doBccImm(s.cc, s.ireg, s.int, Width.W64, s.label, wide = false) }
  instr("bcc.32")  { (a, s) => a.doBcc(s.cc, s.ireg, s.ireg, Width.W32, s.label, wide = false) }
  instr("fbcc.32") { (a, s) => a.doBcc(s.cc, s.freg, s.freg, Width.W32, s.label, wide = false) }
  instr("bcci.32") { (a, s) => a.doBccImm(s.cc, s.ireg, s.int, Width.W32, s.label, wide = false) }

  // wide branches
  instr("w.bcc.64")  { (a, s) => a.doBcc(s.cc, s.ireg, s.ireg, Width.W64, s.label, wide = true) }
  instr("w.fbcc.64") { (a, s) => a.doBcc(s.cc, s.freg, s.freg, Width.W64, s.label, wide = true) }
  instr("w.bcci.64") { (a, s) => a.doBccImm(s.cc, s.ireg, s.int, Width.W64, s.label, wide = true) }
  instr("w.bcc.32")  { (a, s) => a.doBcc(s.cc, s.ireg, s.ireg, Width.W32, s.label, wide = true) }
  instr("w.fbcc.32") { (a, s) => a.doBcc(s.cc, s.freg, s.freg, Width.W32, s.label, wide = true) }
  instr("w.bcci.32") { (a, s) => a.doBccImm(s.cc, s.ireg, s.int, Width.W32, s.label, wide = true) }

  // set conditional
  instr("scc.64")  { (a, s) => a.scc(s.cc, s.ireg, s.ireg, s.ireg, Width.W64) }
  // instr("fscc.64") { (a, s) => a.scc(s.cc, s.ireg, s.freg, s.freg, Width.W64) }
  instr("scci.64") { (a, s) => a.scc(s.cc, s.ireg, s.ireg, s.int,  Width.W64) }
  instr("scc.32")  { (a, s) => a.scc(s.cc, s.ireg, s.ireg, s.ireg, Width.W32) }
  // instr("fscc.32") { (a, s) => a.scc(s.cc, s.ireg, s.freg, s.freg, Width.W32) }
  instr("scci.32") { (a, s) => a.scc(s.cc, s.ireg, s.ireg, s.int,  Width.W32) }

  // jmp
  instr("jmp")   { (a, s) => a.doJmp(s.label, wide = false) }
  instr("w.jmp") { (a, s) => a.doJmp(s.label, wide = true) }

  // misc
  instr("new.closure")  { (a, s) => a.newClosure(IR.IR1, s.tpe) }
  instr("newobj")       { (a, s) => a.newobj(s.tpe) }
  instr("throw")        { (a, s) => a.throwEx(s.ireg) }
  instr("catch")        { (a, s) => a.catchEx(s.ireg) }
  instr("div.check")    { (a, s) => a.divisorCheck(s.ireg) }
  instr("arr.len")      { (a, s) => a.lenarr(s.ireg, s.ireg) }
  instr("arr.ic")       { (a, s) => a.arrIC(s.ireg, s.ireg) }
  instr("gcpoint")      { (a, s) => a.gcpoint() }
  instr("iof")          { (a, s) => a.isInstanceOf(s.ireg, s.ireg, s.tpe) }
  instr("prepare.rec")  { (a, s) => a.prepareRecord(s.ts) }
  instr("zero.refs")    { (a, s) => a.prepareRecord(s.ts) }
  instr("ld.stack.rec") { (a, s) => a.ldstackrec(s.ireg, s.ts) }
  instr("nop")          { (a, s) => a.nop() }
  instr("const.string") { (a, s) => a.initConstString(s.ts, StringLiteral(s.str)) }
  instr("tag.g")        { (a, s) => a.tagGeneric(s.ireg, s.ireg, s.ireg, s.tpe) }
  instr("payload.g")    { (a, s) => a.payloadGeneric(s.ireg, s.ireg, s.ireg, s.ireg, s.tpe) }
  instr("new.none.g")   { (a, s) => a.newNoneGeneric(s.ireg, s.ireg, s.ireg, s.tpe) }
  instr("new.some.g")   { (a, s) => a.newSomeGeneric(s.ireg, s.ireg, s.ireg, s.ireg, s.tpe) }

  // memory access - field
  instr("ld.ref.field")   { (a, s) => val dst = s.ireg; MemSpace.Builder().obj(s.ireg).field(s.field).load(dst).gen(a) }
  instr("ld.ref.field.f") { (a, s) => val dst = s.freg; MemSpace.Builder().obj(s.ireg).field(s.field).load(dst).gen(a) }
  instr("st.ref.field")   { (a, s) => val src = s.ireg; MemSpace.Builder().obj(s.ireg).field(s.field).store(src).gen(a) }
  instr("st.ref.field.f") { (a, s) => val src = s.freg; MemSpace.Builder().obj(s.ireg).field(s.field).store(src).gen(a) }
  instr("ld.rec.field")   { (a, s) => val dst = s.ireg; MemSpace.Builder().rec(s.ireg).field(s.field).load(dst).gen(a) }
  instr("ld.rec.field.f") { (a, s) => val dst = s.freg; MemSpace.Builder().rec(s.ireg).field(s.field).load(dst).gen(a) }
  instr("st.rec.field")   { (a, s) => val src = s.ireg; MemSpace.Builder().rec(s.ireg).field(s.field).store(src).gen(a) }
  instr("st.rec.field.f") { (a, s) => val src = s.freg; MemSpace.Builder().rec(s.ireg).field(s.field).store(src).gen(a) }
  instr("ld.static")      { (a, s) => val dst = s.ireg; MemSpace.Builder().static(s.field).load(dst).gen(a) }
  instr("ld.static.f")    { (a, s) => val dst = s.freg; MemSpace.Builder().static(s.field).load(dst).gen(a) }
  instr("st.static")      { (a, s) => val src = s.ireg; MemSpace.Builder().static(s.field).store(src).gen(a) }
  instr("st.static.f")    { (a, s) => val src = s.freg; MemSpace.Builder().static(s.field).store(src).gen(a) }

  // memory access - uslot
  instr("ld.uslot.s8")  { (a, s) => a.loadUntyped(s.ireg, LD_S8, s.us) }
  instr("ld.uslot.s16") { (a, s) => a.loadUntyped(s.ireg, LD_S16, s.us) }
  instr("ld.uslot.u8")  { (a, s) => a.loadUntyped(s.ireg, LD_U8, s.us) }
  instr("ld.uslot.u16") { (a, s) => a.loadUntyped(s.ireg, LD_U16, s.us) }
  instr("ld.uslot.32")  { (a, s) => a.loadUntyped(s.ireg, LD_32, s.us) }
  instr("ld.uslot.u32") { (a, s) => a.loadUntyped(s.ireg, LD_U32, s.us) }
  instr("ld.uslot.64")  { (a, s) => a.loadUntyped(s.ireg, LD_64, s.us) }
  instr("ld.uslot.ref") { (a, s) => a.loadUntyped(s.ireg, LD_REF, s.us) }
  instr("ld.uslot.f32") { (a, s) => a.loadUntyped(s.freg, LD_F32, s.us) }
  instr("ld.uslot.f64") { (a, s) => a.loadUntyped(s.freg, LD_F64, s.us) }

  instr("st.uslot.8")   { (a, s) => a.storeUntyped(s.freg, ST_8, s.us) }
  instr("st.uslot.16")  { (a, s) => a.storeUntyped(s.ireg, ST_16, s.us) }
  instr("st.uslot.32")  { (a, s) => a.storeUntyped(s.ireg, ST_32, s.us) }
  instr("st.uslot.64")  { (a, s) => a.storeUntyped(s.ireg, ST_64, s.us) }
  instr("st.uslot.ref") { (a, s) => a.storeUntyped(s.ireg, ST_REF, s.us) }
  instr("st.uslot.f32") { (a, s) => a.storeUntyped(s.freg, ST_F32, s.us) }
  instr("st.uslot.f64") { (a, s) => a.storeUntyped(s.freg, ST_F64, s.us) }
  instr("st.uslot.imm") { (a, s) => a.storeUntypedImm(s.int, s.us) }

  // memory access - tslot
  instr("ld.tslot")     { (a, s) => val dst = s.ireg; MemSpace.Builder().typed(s.ts).field(s.field).load(dst).gen(a) }
  instr("ld.tslot.f")   { (a, s) => val dst = s.freg; MemSpace.Builder().typed(s.ts).field(s.field).load(dst).gen(a) }
  instr("st.tslot")     { (a, s) => val src = s.ireg; MemSpace.Builder().typed(s.ts).field(s.field).store(src).gen(a) }
  instr("st.tslot.f")   { (a, s) => val src = s.freg; MemSpace.Builder().typed(s.ts).field(s.field).store(src).gen(a) }
  instr("st.tslot.imm") { (a, s) => val src = s.int;  MemSpace.Builder().typed(s.ts).field(s.field).storeImm(src).gen(a) }

  // memory access - raw
  instr("ld.tail") { (a, s) => a.loadTailParam(ldk = s.ldk, dst = s.ireg, tailReg = s.ireg, number = s.int) }

  // calls
  instr("call.direct")    { (a, s) => a.callDirect(s.ireg, s.method) }
  instr("call.virt")      { (a, s) => a.callVirt(s.ireg, s.method) }
  instr("call.interf")    { (a, s) => a.callInterf(s.ireg, s.method) }
  instr("call.closure")   { (a, s) => a.callClosure(IR.IR1, s.tpe) }
  instr("call.closure.g") { (a, s) => a.callClosureGeneric(IR.IR1, s.tpe) }

  // bfx
  instr("bfxs.32.32")  { (a, s) => a.bfx(s.ireg, s.ireg, Width.W32, Width.W32, true,  s.int.toInt, s.int.toInt) }
  instr("bfxs.32.64")  { (a, s) => a.bfx(s.ireg, s.ireg, Width.W32, Width.W64, true,  s.int.toInt, s.int.toInt) }
  instr("bfxs.64.32")  { (a, s) => a.bfx(s.ireg, s.ireg, Width.W64, Width.W32, true,  s.int.toInt, s.int.toInt) }
  instr("bfxs.64.64")  { (a, s) => a.bfx(s.ireg, s.ireg, Width.W64, Width.W64, true,  s.int.toInt, s.int.toInt) }
  instr("bfxz.32.32")  { (a, s) => a.bfx(s.ireg, s.ireg, Width.W32, Width.W32, false, s.int.toInt, s.int.toInt) }
  instr("bfxz.32.64")  { (a, s) => a.bfx(s.ireg, s.ireg, Width.W32, Width.W64, false, s.int.toInt, s.int.toInt) }
  instr("bfxz.64.32")  { (a, s) => a.bfx(s.ireg, s.ireg, Width.W64, Width.W32, false, s.int.toInt, s.int.toInt) }
  instr("bfxz.64.64")  { (a, s) => a.bfx(s.ireg, s.ireg, Width.W64, Width.W64, false, s.int.toInt, s.int.toInt) }

  // thread spawn
  instr("spawn")        { (a, s) => a.spawn(s.ireg, s.tpe) }
  instr("spawn.future") { (a, s) => a.spawnFuture(s.ireg, s.tpe) }

  // generics
  instr("load.type.info")   { (a, s) => a.loadTypeInfoSig(s.ireg, s.tpe) }
  instr("load.type.info.g") { (a, s) => a.loadTypeInfoGeneric(s.ireg, s.tpe) }
  instr("box")              { (a, s) => a.box(s.ireg, s.ireg, s.tpe) }
  instr("box.ts")           { (a, s) => a.box(s.ts, s.ireg) }
  instr("unbox")            { (a, s) => a.unbox(s.ireg, s.ireg, s.tpe) }
  instr("unbox.ts")         { (a, s) => a.unbox(s.ts, s.ireg) }
  instr("type.arg")         { (a, s) => a.typeArg(dst = s.ireg, idx = s.int.toInt, ti = s.ireg) }
  instr("offset")           { (a, s) => a.offset(dst = s.ireg, fr = s.field, ti = s.ireg) }
  instr("add.offset")       { (a, s) => a.addOffset(s.ireg, s.field, s.ireg) } // TODO: name params

  instr("load.type.info.obj") { (a, s) => a.loadTypeInfoObj(s.ireg, s.ireg) }

  // memspace
  memInstr("ms.const.idx") { (b, s) => b.constIndex(s.int.toInt, s.tpe) }
  memInstr("ms.idx")       { (b, s) => b.index(s.ireg, s.tpe) }
  memInstr("ms.field")     { (b, s) => b.field(s.field) }
  memInstr("ms.fseq")      { (b, s) => s.fields.foreach(b.field(_)) }
  memInstr("ms.offset")    { (b, s) => b.offset(s.ireg) }

  memInstr("ms.const.idx.g") { (b, s) => b.constIndexGeneric(s.int.toInt, s.tpe, s.ireg) }
  memInstr("ms.idx.g")       { (b, s) => b.indexGeneric(s.ireg, s.tpe, s.ireg) }
  memInstr("ms.field.g")     { (b, s) => b.fieldGeneric(s.field, s.ireg) }

  tailInstr("ms.ld")     { (b, s) => b.load(s.ireg) }
  tailInstr("ms.ld.f")   { (b, s) => b.load(s.freg) }
  tailInstr("ms.ld.g")   { (b, s) => b.loadGeneric(s.ireg, s.ireg) }
  tailInstr("ms.st")     { (b, s) => b.store(s.ireg) }
  tailInstr("ms.st.f")   { (b, s) => b.store(s.freg) }
  tailInstr("ms.st.imm") { (b, s) => b.storeImm(s.int) }
  tailInstr("ms.st.g")   { (b, s) => b.storeGeneric(s.ireg, s.ireg) }
}
