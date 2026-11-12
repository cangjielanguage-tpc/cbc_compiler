package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.{BodyFault, BodyFixture, EditableCHIR, FunctionMetadata, SharedTestData}
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.*

private[chir] object TestData {
  def fixtureData(): CHIRPackageT = CHIRPackageT.deserializeFromBinary(SharedTestData.fixtureBytes)
  def operationFixtureData(): CHIRPackageT = CHIRPackageT.deserializeFromBinary(SharedTestData.operationFixtureBytes)

  def bodyFixture(): BodyFixture = new BodyFixture {
    private val model = PackageImpl.fromBytes(SharedTestData.fixtureBytes)
    val pkg: EditableCHIR.Package = model
    def rawBytes: Array[Byte] = model.data.serializeToBinary()
    def metadata: FunctionMetadata = {
      val f = model.data.getValues()(0).asFunction()
      FunctionMetadata(f.getBody, f.getRetVal, f.getBase.getBase.getType, f.getParams.toSeq,
        f.getLocalId, f.getBlockId, f.getBlockGroupId, f.getIsFastNative, f.getIsCffiwrapper, f.getBase.getRawMangledName)
    }
    def groupOwner(id: Long): Long = model.groupTable(id).getOwnedFunc
    def isReturn(id: Long): Boolean = model.valueTable(id).asLocalVar().getIsRetVal
    def corrupt(fault: BodyFault): Unit = fault match {
      case BodyFault.SharedBody => model.data.getValues()(7).asFunction().setBody(5L)
      case BodyFault.WrongEntry => model.data.getValues()(4).asBlockGroup().setEntryBlock(3L)
      case BodyFault.WrongExpressionOwner => model.data.getExprs()(2).asAllocateBase().getBase.setOwner(3L)
      case BodyFault.WrongResultDefinition => model.data.getValues()(6).asLocalVar().setAssociatedExpr(1L)
      case BodyFault.ForeignParameter => model.data.getExprs()(3).asExpression().setOperands(Array(11L))
    }
  }
}
