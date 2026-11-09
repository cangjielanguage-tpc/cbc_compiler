# Patch-gen functional tests

The 114 fixture files were copied unchanged from `patch-gen/test/test_data` in
`hotfix_tools` main at `de646aae64edeca575c032dcf4c132908eae95da`.
There are 33 test cases, each run at O0 and O1 by default. O2 can be selected
explicitly and uses the existing O2 expectations where present.

The runner compiles the patchable library, compiles base and patched sources
with the hotfix plugin, invokes the generator, checks the patch with `chir-dis`,
and compares the declaration dump or expected diagnostic. Declaration order is
normalized within each dump section; names and unreachable markers are retained.
A successful generator must produce a patch file. Work files stay in ignored
`work/`, and source fixtures are never compiled in place.

For the Scala generator, the runner passes `--test` to request the declaration
dump. Its implementation and CLI are still pending on this branch.

The native TEST build is useful as a control while implementing Scala:

```bash
python3 test/patch-gen/run_tests.py \
  --cangjie-home ~/Projects/software/hotfix-06.11 \
  --patch-gen ~/Projects/hotfix_tools/output/patch-gen-test/patch-gen --native
```

Use `--filter func1 class1` for a subset. The standard stdx location is
`<cangjie-home>/stdx/dynamic/stdx`.

The Scala guard-removal and body-transfer integration tests compile real plugin
output, edit it using the common CHIR API, write once, reload, and validate it
using native `chir-dis`:

```bash
CANGJIE_HOME=~/Projects/software/hotfix-06.11 \
  sbt 'chirLib/testOnly *GuardChecksSuite'
```

These tests cover ordinary global/member functions. They do not yet generate
complete executable patches or cover constructor return adaptation.
