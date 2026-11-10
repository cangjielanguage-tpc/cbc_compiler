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
serialization and invalid references. NonSerializedImplSuite covers synthetic
compiler nodes and overlays, independently of any serialized schema.

Patch-gen's CLI Main is not a test runner. Its end-to-end fixtures are separate:

```bash
python3 test/patch-gen/run_tests.py --cangjie-home ~/Projects/software/hotfix-06.11
```
