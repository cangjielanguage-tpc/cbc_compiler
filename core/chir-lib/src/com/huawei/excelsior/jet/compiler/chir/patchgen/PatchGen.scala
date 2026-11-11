package com.huawei.excelsior.jet.compiler.chir.patchgen

import com.huawei.excelsior.jet.compiler.chir.CHIR.*
import com.huawei.excelsior.jet.util.ScalaCollections
import scala.collection.mutable

final class PatchGenException(message: String) extends RuntimeException(message)

/** Mark declarations before editing, then transform the patched package in place.
  * Serialization belongs to the package and is performed once by the caller.
  */
final class PatchGen(source: Option[Package], pkg: Package) {
  private val functions = pkg.values.collect { case f: Func => f }.toVector
  private val globals = pkg.values.collect { case g: GlobalVar => g }.toVector
  private val globalsByName = ScalaCollections.groupMapReduce(globals)(_.srcCodeIdentifier)(identity)((first, _) => first)
  private val functionsByName = functions.groupBy(_.srcCodeIdentifier)
  private val definitions = pkg.typeDefs.toVector
  private val imports = mutable.LinkedHashSet.empty[Long]
  private val deleted = mutable.LinkedHashSet.empty[Long]
  private val importedVars = mutable.ArrayBuffer.empty[GlobalVar]
  private val importedTypes = mutable.ArrayBuffer.empty[CustomTypeDef]
  private val patches = mutable.ArrayBuffer.empty[Func]
  private val fresh = mutable.ArrayBuffer.empty[Func]
  private val prefix = "$" + pkg.name
  private val guardClass = definitions.collectFirst {
    case d: ClassDef if d.srcCodeIdentifier == prefix + "$Patch$GC" => d
  }
  private val patchClass = definitions.collectFirst {
    case d: ClassDef if d.srcCodeIdentifier == prefix + "$PatchImpl$GC" => d
  }
  private val guardVar = globals.find(_.srcCodeIdentifier == prefix + "$GV")
  private val guardInit = functions.find(f => f.srcCodeIdentifier == prefix + "$GVI" && f.body.nonEmpty)

  private def error(condition: Boolean, message: => String): Unit = {
    if (!condition) throw new PatchGenException(message)
  }
  private def imported(value: HasAttributes): Boolean = value.attributes.contains(Attribute.Imported)
  private def addAttribute(value: HasAttributes, attribute: Attribute): Unit =
    value.replaceAttributes((value.attributes :+ attribute).distinct)
  private def isInit(f: Func): Boolean = (f eq pkg.packageInitFunc) || (f eq pkg.packageInitLiteralFunc)
  private def functionKey(f: Func): Product = {
    val parent = f.declaringDef.orElse {
      if (f.attributes.contains(Attribute.Abstract) && !f.attributes.contains(Attribute.Static)) {
        f.tpe.paramTypes.headOption.map(RefType.skip).collect { case t: CustomType => t.typeDef }
      } else None
    }
    (f.identifier, f.srcCodeIdentifier, f.packageName, Equality.typeKey(f.tpe), f.body.nonEmpty, imported(f), parent.map(definitionKey))
  }
  private def definitionKey(d: CustomTypeDef): Product =
    (d.identifier, d.srcCodeIdentifier, d.packageName, d.isInstanceOf[ClassDef], d.isInstanceOf[StructDef])
  private def variableKey(g: GlobalVar): Product = (g.identifier, g.srcCodeIdentifier, g.packageName)

  def createPatch(): Unit = {
    val triple = Seq(guardClass.nonEmpty, guardVar.nonEmpty, guardInit.nonEmpty)
    error(triple.forall(identity) || triple.forall(x => !x),
      "triple (guard class, guard var and guard vars initializer) is expected to be initialized or not initialized")
    error(definitions.exists(d => imported(d) && d.srcCodeIdentifier == "Exception"), "unable to find Exception definition")
    mark()
    retainInternalFunctions()
    removeNewGuards()
    if (source.nonEmpty) {
      generateMethods()
      for (f <- functions if imports(f.id) || deleted(f.id)) {
        f.removeBody()
        addAttribute(f, Attribute.Imported)
      }
      for (g <- importedVars) addAttribute(g, Attribute.Imported)
      for (d <- importedTypes) addAttribute(d, Attribute.Imported)
      removeEmptyInitializers()
      updatePackageInits()
    }
    pkg.setOptimized()
  }

  private def mark(): Unit = {
    source match {
      case Some(source) =>
        error(source.name == pkg.name, "packages differ")
        patches ++= Seq(pkg.packageInitFunc, pkg.packageInitLiteralFunc)
        val currentDefs = definitions.filterNot(imported).map(d => definitionKey(d) -> d).toMap
        for (d <- source.typeDefs if !imported(d) && (d.isInstanceOf[ClassDef] || d.isInstanceOf[StructDef])) {
          val kind = if (d.isInstanceOf[ClassDef]) "class" else "struct"
          val found = currentDefs.get(definitionKey(d))
          error(found.nonEmpty, s"$kind ${d.srcCodeIdentifier} was not found in patched version")
          if (!patchClass.exists(_.srcCodeIdentifier == d.srcCodeIdentifier)) importedTypes += found.get
        }
        val sourceFuncs = source.values.collect { case f: Func => f }.toVector
        val sourceKeys = sourceFuncs.map(f => (f, functionKey(f)))
        val currentKeys = functions.map(f => (f, functionKey(f)))
        val currentFuncs = currentKeys.map((f, key) => key -> f).toMap
        for ((f, key) <- sourceKeys) {
          val initializer = f.packageName == pkg.name &&
            (f.kind == Func.Kind.GlobalVarInit || f.kind == Func.Kind.AnnoFactory || f.srcCodeIdentifier == prefix + "$GVI")
          val stub = f.declaringDef.exists(d => patchClass.exists(_.srcCodeIdentifier == d.srcCodeIdentifier))
          if (!initializer && !stub) {
            val found = currentFuncs.get(key)
            if (found.nonEmpty || !(imported(f) || f.packageName != pkg.name)) {
              error(found.nonEmpty, s"func ${f.identifier.stripPrefix("@")} was not found in patched version")
              markFunction(f, found.get)
            }
          }
        }
        val oldKeys = sourceKeys.iterator.map(_._2).toSet
        fresh ++= currentKeys.collect { case (f, key) if GuardChecks.flag(f).nonEmpty && !oldKeys(key) => f }
        val currentVars = globals.map(g => variableKey(g) -> g).toMap
        for (g <- source.values.collect { case g: GlobalVar => g }) {
          val found = currentVars.get(variableKey(g))
          val kind = if (g.declaringDef.nonEmpty) "static" else "global"
          error(found.nonEmpty, s"$kind var ${g.identifier.stripPrefix("@")} was not found in patched version")
          importedVars += found.get
        }
      case None => fresh ++= functions.filter(f => GuardChecks.flag(f).nonEmpty)
    }
  }

  private def markFunction(old: Func, next: Func): Unit = {
    if (old.body.isEmpty) {
      if (!imported(old)) imports += next.id
      return
    }
    val patchable = GuardChecks.flag(old).nonEmpty
    error(patchable == GuardChecks.flag(next).nonEmpty,
      s"function ${old.srcCodeIdentifier} must be patchable both in source and patched versions")
    error(next.body.nonEmpty, s"function ${old.srcCodeIdentifier} must have body both in source and patched versions")
    error(old.attributes == next.attributes,
      s"attributes of non-patchable function ${old.srcCodeIdentifier} must stay unchanged in patched version")
    if (next.kind == Func.Kind.Lambda) deleted += next.id else imports += next.id
    if (!Equality.sameCode(old.body.get, next.body.get)) {
      error(patchable, s"non-patchable function ${old.srcCodeIdentifier} must stay unchanged in patched version")
      patches += next
    }
  }

  private def retainInternalFunctions(): Unit = {
    val visited = mutable.Set.empty[Long]
    val changed = patches.map(_.id).toSet
    def visit(f: Func): Unit = {
      if (visited.add(f.id)) {
        for (e <- pkg.expressions(f)) e match {
          case e: Apply => e.callee match {
            case f: Func if pkg.isInternal(f) =>
              if (f.kind == Func.Kind.Lambda) deleted -= f.id else if (!changed(f.id)) imports -= f.id
              visit(f)
            case _ => ()
          }
          case _ => ()
        }
      }
    }
    patches.foreach(visit)
  }

  private def removeNewGuards(): Unit = {
    for (f <- fresh) {
      val method = PatchableName.guardMethod(f)
      val flag = GuardChecks.flag(f).get
      GuardChecks.remove(f)
      addAttribute(flag, Attribute.Imported)
      for (d <- guardClass.toSeq ++ patchClass.toSeq) d.replaceMethods(d.methods.filterNot(_.srcCodeIdentifier == method))
      val ctor = PatchableName.syntheticConstructor(f)
      for (name <- (Seq(method) ++ ctor).distinct; g <- functionsByName.getOrElse(name, Seq.empty)) {
        addAttribute(g, Attribute.Unreachable)
      }
    }
  }

  private def generateMethods(): Unit = {
    val generated = mutable.ArrayBuffer.empty[Func]
    val methods = ScalaCollections.groupMapReduce(patchClass.toSeq.flatMap(_.methods))(_.srcCodeIdentifier)(identity)((first, _) => first)
    for (f <- patches.distinct) {
      GuardChecks.remove(f)
      if (!isInit(f)) {
        error(patchClass.nonEmpty, "patch class is expected to be initialized")
        val methodName = PatchableName.guardMethod(f)
        val target = methods.get(methodName)
        error(target.nonEmpty, "patch method is expected to be initialized")
        f.moveBody(target.get)
        generated += target.get
        val flagName = PatchableName.guardFlag(f)
        val flag = globalsByName.get(flagName)
        error(flag.nonEmpty, "guard var flag is expected to be initialized")
        val block = guardInit.get.body.get.entryBlock
        val enabled = pkg.insert(block, Operation.BooleanConstant(true))
        pkg.insert(block, Operation.Store(enabled, flag.get))
      }
    }
    if (generated.nonEmpty) {
      val block = guardInit.get.body.get.entryBlock
      val allocated = pkg.insert(block, Operation.Allocate(patchClass.get.tpe))
      val parentRef = new RefType { def baseType: Type = guardClass.get.tpe }
      val cast = pkg.insert(block, Operation.StaticCast(allocated, parentRef))
      val falseValue = pkg.insert(block, Operation.BooleanConstant(false))
      val tuple = pkg.insert(block, Operation.Tuple(Seq(falseValue, cast), RefType.skip(guardVar.get.tpe)))
      pkg.insert(block, Operation.Store(tuple, guardVar.get))
    }
  }

  private def removeEmptyInitializers(): Unit = {
    def empty(f: Func): Boolean = f.attributes.contains(Attribute.Initializer) && !imported(f) && pkg.isInternal(f) &&
      f.body.exists(g => g.blocks.size == 1 && g.entryBlock.nonTerminatorExpressions.isEmpty && g.entryBlock.terminator.isInstanceOf[Exit])
    for (f <- Seq(pkg.packageInitFunc, pkg.packageInitLiteralFunc); b <- f.body.toSeq.flatMap(_.blocks)) {
      val redundant = b.nonTerminatorExpressions.collect { case e: Apply if e.callee.isInstanceOf[Func] && empty(e.callee.asInstanceOf[Func]) => e }
      val removed = new java.util.IdentityHashMap[Expression, java.lang.Boolean]()
      redundant.foreach(e => removed.put(e, java.lang.Boolean.TRUE))
      b.replaceExpressions(b.nonTerminatorExpressions.filterNot(removed.containsKey), b.terminator)
      for (e <- redundant) {
        val callee = e.callee.asInstanceOf[Func]
        callee.removeBody()
        addAttribute(callee, Attribute.Imported)
      }
    }
  }

  private def updatePackageInits(): Unit = {
    val accessors = Seq((pkg.packageInitFunc, "$has_applied_pkg_init_func", "packageInit"),
      (pkg.packageInitLiteralFunc, "has_invoked_pkg_init_literal", "packageLiteralInit"))
    for ((f, flagName, accessor) <- accessors) {
      val flag = globals.find(_.srcCodeIdentifier == flagName)
      error(flag.nonEmpty, "unable to find package init flag")
      val getter = functions.find(_.srcCodeIdentifier == prefix + accessor + "Getter$GF")
      val setter = functions.find(_.srcCodeIdentifier == prefix + accessor + "Setter$GF")
      error(getter.nonEmpty && setter.nonEmpty, "unable to find package init flag accessor")
      for (e <- pkg.expressions(f)) e match {
        case e: Load if e.location eq flag.get => pkg.replace(e, Operation.Apply(getter.get, Seq.empty))
        case e: Store if e.location eq flag.get => pkg.replace(e, Operation.Apply(setter.get, Seq(e.value)))
        case _ => ()
      }
    }
  }
}
