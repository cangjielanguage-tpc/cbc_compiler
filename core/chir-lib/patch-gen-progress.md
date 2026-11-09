# Patch-gen migration checkpoint

Status: incomplete. There is no complete Scala patch-gen executable on this
branch yet. Native control results must not be reported as Scala port results.

## History and sources

- Working branch: `ilyas/patch-gen`, rebased onto `origin/master` at `08f3bcfb`.
- Original history: `backup/patch-gen-before-api-review-20261109`.
- Replayed the editor/equality commits; omitted the obsolete synthetic-generator
  refactoring, preserving master's `AugmentedPackage` and `IntrinsicsPackage`.
- C++ reference: `~/Projects/hotfix_tools/patch-gen`, main
  `de646aae64edeca575c032dcf4c132908eae95da`. The initially supplied path with
  `software/hotfix_tools` does not exist.
- Also inspected `origin/liontiger/patch-gen-scala-port-review`. Its `model`,
  `ChirOps`, and `Clone` implementation are not imported. Its functional runner
  was reused and corrected; fixtures come directly from C++ main, unchanged.

## Implemented

- Preserved master's configured schema/default-version selection in `CHIR.read`.
- Integrated existing readers and synthetic compiler functions with the common
  CHIR interfaces. Private implementation adapters reject unsupported editing;
  there is no second public editable model. v13008 remains a read-only reader.
- Removed internal table-ID methods and editor-test-only block/group/branch
  factories from the common `CHIR.Package` interface. No public clone API added.
- Invalidated old FlatBuffers generator caches for object-API generation.
- Migrated 114 fixture files into `test/patch-gen`, with an isolated work directory,
  native control mode, native disassembly checking, and successful-output checks.
- Added plugin guard detection and removal through existing CHIR interfaces.
  The implementation retains entry-block setup and jumps to user code, allowing
  existing reachability cleanup to discard dispatch and exception branches.
  It does not copy/clone expressions or merge them between blocks.
- Added native integration tests for global/member `foo`: compile actual plugin
  output, remove guards (including package-init guards), move ordinary function
  bodies into PatchImpl methods, mark source declarations imported, save once,
  reload, and validate with `chir-dis`.

## Validation completed

Using `CANGJIE_HOME=~/Projects/software/hotfix-06.11` and standard
`CANGJIE_HOME/stdx/dynamic/stdx`:

- `chirLib/test`: 96 passed, including both native integration cases.
- `compiler/compile`: passed.
- `sbt test`: passed (3 + 648 + 216 + 1319 + 2824 executed tests;
  three existing ignored tests across the compiler suites).
- Migrated functional suite against the existing C++ TEST binary:
  33 cases at each of O0 and O1, all 66 passed. The runner normalizes declaration
  ordering and outer newlines; expectations were not changed.
- Python runner compiles; `git diff --check` passes.

The complete Scala patch-gen functional suite has NOT passed: its generator is
not implemented yet. End-to-end HLIR/standalone regression validation remains
pending until that generator passes the functional suite. Existing unit tests
include compiler HLIR tests, but do not replace this final end-to-end gate.

## Algorithm and API work remaining

Follow C++ `createPatch`: mark declarations, retain reachable internal functions,
remove guards from new patchable functions, generate changed patch methods,
import old declarations, remove redundant initializers/lambdas, rewrite package
initialization flag access to accessors, then save the package once.

The needed extensions are narrower than CHIRBuilder:

1. Read-only linkage metadata and traversal of nested expression bodies, needed
   to preserve reachable internal generic instances and lambdas during marking.
2. Editing attributes/method membership on definitions and editing call/operand
   references; keep table ownership and result-local references in the package.
3. Creation of the few expressions used to initialize the patch object and guard
   flags: Allocate, static cast, Boolean constant, Tuple, Store, and Apply.
   Replacement should preserve existing result locals where appropriate.
4. Constructor-specific transfer: struct constructors return a newly allocated
   result and redirect receiver uses; class constructors need a Unit return
   allocation. This is different from ordinary `moveBody` compatibility rules.
5. Bind call-context This to the original declaring/extended type after moving
   member bodies. Cover recursive calls, generics and nested bodies explicitly.
6. Mark/import/deletion decisions, package-init accessor rewriting, CLI, package
   OPT phase, and declaration dump expected by the functional runner.

No cloning is needed merely to regenerate local names: the existing body mover
already remaps parameters and renames locals. Keep schema serialization in the
package writer, with a single write after transformations. Avoid a patch-specific
serialization pipeline or duplicating parser nodes in a new mutable model.

## Debugger finding

The first real transfer test selected auto-patchable constructors as well as
ordinary methods. IntelliJ showed source `init`, source type args `[776, 788]`,
and target args `[779, 776, 672]`; the constructor return convention differs.
The test now intentionally selects ordinary `foo` and the PatchImpl owner.
The speculative type-ID comparison change was reverted. The debugger session
and the agent's breakpoint were removed. Constructor support is still required.
