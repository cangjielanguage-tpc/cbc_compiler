# CHIR library tests

From the repository root, run the dedicated sbt task:

```bash
CANGJIE_HOME=~/Projects/software/hotfix-06.11 \
CANGJIE_STDX_PATH=~/Projects/software/hotfix-06.11/stdx/dynamic/stdx \
sbt chirLibTest
```

The task runs only the CHIR library ScalaTest suites and propagates test failures.
Without CANGJIE_HOME, native integration cases are reported as canceled; model
and schema tests still run. The SDK supplies cjc, its hotfix plugin and chir-dis.
The test helper sources the SDK's envsetup.sh for native commands.

For one suite or schema version, use sbt's standard test selection:

```bash
sbt 'chirLib/testOnly *EditingApiSuite'
sbt 'chirLib/testOnly com.huawei.excelsior.jet.compiler.chir.v13008.*'
```

EditingApiSuite exercises shared transformation contracts on all supported
schemas. Version-specific suites cover generated table fields, raw metadata,
serialization and invalid references. EditingApiSuite also checks that synthetic
compiler nodes cannot be edited.

Patch-gen's CLI Main is not a test runner. Its end-to-end fixtures are separate:

```bash
python3 test/patch-gen/run_tests.py --cangjie-home ~/Projects/software/hotfix-06.11
```

## Reading and editing

`CHIR.Package` and its nodes provide the read API. `EditableCHIR.Package`
provides construction and mutation without changing those interfaces:

```scala
val editable = EditableCHIR.fromBytes(inputBytes, version)
val pkg: CHIR.Package = editable
editable.getFunc("from").get.moveBody(pkg.getFunc("to").get)
val outputBytes = editable.toBytes
```

Both APIs share the same backing model, so existing node views observe edits.
The package checks ownership locally during each edit and normalizes and validates
before producing bytes. `CHIR.read` and `EditableCHIR.read` provide common file
loading; `EditableCHIR.Package.write` validates before atomically replacing the output.

Shared tests can use `EditableCHIR.empty(name, version)` and its `newFunction`,
`newGlobalVar`, block and expression constructors. These return
`EditableCHIR.Func`, `EditableCHIR.Block` and other editable node traits extending
their corresponding read interfaces. Generated schema tables remain backend
details. Schema-specific metadata tests may still inspect generated tables directly.
