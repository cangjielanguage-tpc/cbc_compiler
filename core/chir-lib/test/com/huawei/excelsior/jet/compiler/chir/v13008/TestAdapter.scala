package com.huawei.excelsior.jet.compiler.chir.v13008

import com.huawei.excelsior.jet.compiler.chir.{CHIR, EditableCHIR, FunctionMetadata, TestData}
import com.huawei.excelsior.jet.compiler.chir.v13008.CHIRFormat.*

/** Generated-table decoding and field access for the shared test fixtures. */
private[chir] object TestAdapter extends TestData.Schema {
  type WirePackage = CHIRPackageT
  def decode(bytes: Array[Byte]): CHIRPackageT = CHIRPackageT.deserializeFromBinary(bytes)

  def load(bytes: Array[Byte]): TestData.Access = new TestData.Access {
    private val model = PackageImpl.fromBytes(bytes)
    val pkg: EditableCHIR.Package = model
    private def raw: CHIRPackageT = model.p
    def rawBytes: Array[Byte] = raw.serializeToBinary()
    def tpe(id: Long): CHIR.Type = model.getType[CHIR.Type](id).get
    def value(id: Long): CHIR.Value = model.getValue[CHIR.Value](id).get
    def definition(id: Long): CHIR.CustomTypeDef = model.getDef[CHIR.CustomTypeDef](id).get
    def metadata(id: Long): FunctionMetadata = {
      val f = model.valueTable(id).asFunction()
      FunctionMetadata(f.getBody, f.getRetVal, f.getBase.getBase.getType, f.getParams.toSeq,
        f.getLocalId, f.getBlockId, f.getBlockGroupId, f.getIsFastNative, f.getIsCffiwrapper, f.getBase.getRawMangledName)
    }

    def groupOwner(id: Long): Long = model.groupTable(id).getOwnedFunc
    def isReturn(id: Long): Boolean = model.valueTable(id).asLocalVar().getIsRetVal
    def setFunctionBody(id: Long, body: Long): Unit = model.valueTable(id).asFunction().setBody(body)
    def setGroupEntry(id: Long, block: Long): Unit = model.groupTable(id).setEntryBlock(block)
    def setExpressionOwner(id: Long, owner: Long): Unit = model.expressionTable(id).asAllocateBase().getBase.setOwner(owner)
    def setLocalDefinition(id: Long, expression: Long): Unit = model.valueTable(id).asLocalVar().setAssociatedExpr(expression)
    def setOperands(id: Long, operands: Seq[Long]): Unit = model.expressionTable(id).asExpression().setOperands(operands.toArray)
    def setGenericBounds(id: Long, bounds: Seq[Long]): Unit = raw.getTypes()((id - 1).toInt).asGenericType().setUpperBounds(bounds.toArray)
    def setArrayDimension(id: Long, dimension: Long): Unit = raw.getTypes()((id - 1).toInt).asRawArrayType().setDims(dimension)
    def setFixedArraySize(id: Long, size: Long): Unit = raw.getTypes()((id - 1).toInt).asVArrayType().setSize(size)
    def setFunctionTypeArgs(id: Long, args: Seq[Long]): Unit = raw.getTypes()((id - 1).toInt).asFuncType().getBase.setArgTys(args.toArray)
    def setFunctionTypeFlags(id: Long, isC: Boolean, hasVarArg: Boolean): Unit = {
      val t = raw.getTypes()((id - 1).toInt).asFuncType()
      t.setIsCfuncType(isC)
      t.setHasVarArg(hasVarArg)
    }

    def setParameter(id: Long, name: String, tpe: Long): Unit = {
      val p = model.valueTable(id).asParameter().getBase
      p.setIdentifier(name)
      p.setType(tpe)
    }

    def setInteger(id: Long, value: Long): Unit = model.valueTable(id).asIntLiteral().setVal(value)
    def setGlobalPackage(id: Long, name: String): Unit = model.valueTable(id).asGlobalVar().getBase.setPackageName(name)
    def setMemberName(id: Long, index: Int, name: String): Unit = raw.getDefs()((id - 1).toInt).asClassDef().getBase.getInstanceMemberVars()(index).setName(name)
    def setMethodName(id: Long, table: Int, index: Int, name: String): Unit = raw.getDefs()((id - 1).toInt).asClassDef().getBase.getVtable()(table).getVirtualMethods()(index).setFuncName(name)
    def setFunctionAttributes(id: Long, attributes: Long): Unit = model.valueTable(id).asFunction().getBase.getBase.getBase.setAttributes(attributes)
    def setAutoEnvAnnotation(id: Long, index: Int, value: Boolean): Unit = model.valueTable(id).asFunction().getBase.getBase.getBase.getAnnos()(index).asisAutoEnvClass().setValue(value)
    def setOverrideAnnotation(id: Long, index: Int, tpe: Long): Unit = model.valueTable(id).asFunction().getBase.getBase.getBase.getAnnos()(index).asoverrideSrcFuncType().setType(tpe)
    def setSourceFile(id: Long, name: String): Unit = model.valueTable(id).asFunction().getBase.getBase.getBase.getLoc.setFilePath(name)
    def setGenericNames(id: Long, identifier: String, sourceName: String): Unit = {
      val t = raw.getTypes()((id - 1).toInt).asGenericType()
      t.setIdentifier(identifier)
      t.setSrcCodeIdentifier(sourceName)
    }
  }
}
