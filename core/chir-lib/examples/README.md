# CHIR editing examples

Run the commands below from the repository root. The Cangjie source lives in
`core/chir-lib/examples`; runnable v13002 examples live in the module’s `src`
directory under the `patchgen` package.

## Editable v13002 package API

`EditableCHIR.read(path)` now returns a real package backed by `CHIRPackageT`.
Functions, blocks and block groups are stable editable views. For example, to
keep the false arm of the first `if` in global `foo` from the demo:

```scala
val pkg = EditableCHIR.read(input)
val f = pkg.values.collectFirst {
  case f: EditableCHIR.Func if f.srcCodeIdentifier == "foo" && f.params.size == 1 => f
}.get
val b = f.body.get.blocks.find(
  _.expressions.lastOption.exists(_.isInstanceOf[CHIR.Branch])
).get
val branch = b.terminator.asInstanceOf[CHIR.Branch]
b.replaceTerminator(pkg.newGoto(branch.falseBlock, branch.debugLoc))
pkg.removeUnreachableBlocks(f)
val destination = pkg.values.collectFirst {
  case f: EditableCHIR.Func if f.srcCodeIdentifier == "foo" && f.params.size == 2 => f
}.get
pkg.moveBody(f, destination)
pkg.write(output) // The only serialization, after both transformations.
```

`MoveFooBodyMain.scala` is the only scenario runner. It contains the `if`
removal and calls the library's `moveBody` operation, then writes once. Run it
with the command below.

The new API implements `replaceBody`, `moveBody`, block expression/terminator replacement,
block-group membership replacement, factories for blocks/groups/gotos/branches,
unreachable-block removal, validation and writing. Factories register nodes;
cross-package or unregistered nodes are rejected. New nodes may be temporarily
unattached during editing but must be attached before writing. Existing orphaned
entries may remain so table IDs do not change.

Writing validates the active graph, then replaces the output through a temporary
file. It retains all schema payloads and the input's file-identifier convention.
Unused records retain valid ownership links in the serialized copy because
native `chir-dis` dereferences those links even for unused records. Removed blocks
are placed in a separate unreferenced storage group, with empty instruction lists,
so native deserialization cannot reattach them to the live body or reconstruct
obsolete predecessor edges. Their table IDs and retained expression/local records
remain allocated. The in-memory graph still detaches removed nodes; writing does
not mutate it. Newly created nodes include the empty debug metadata expected by
the native toolchain.

Validation checks schema ID ranges, union payloads, ownership, definitions,
predecessors/successors, reachable terminators and selected type relationships.
It is not a replacement for the compiler's full type checker. Schema nodes that
lack a specialized CHIR interface are retained and exposed as generic expression
views. The editor currently supports v13002 only; it does not provide factories
for arbitrary new functions, definitions or types.

Run the editor tests with `sbt 'chirLib/test'`.

## End-to-end demo with hotfix-20.10 (v13002)

The runnable main is
`com.huawei.excelsior.jet.compiler.chir.patchgen.MoveFooBodyMain` in `chirLib`.
It uses the v13002 bindings appropriate for `hotfix-20.10`. Run from the repository root:

```bash
sbt 'chirLib/runMain com.huawei.excelsior.jet.compiler.chir.patchgen.MoveFooBodyMain'
```

An optional argument supplies a different absolute toolchain directory; the
default is `~/Projects/software/hotfix-20.10`. The main prints `foo.cj` first,
invokes `cjc --emit-chir=raw`, loads the package through `EditableCHIR.read`, and
prints the two functions' IDs, parameters, and block contents. It removes the
global `foo` conditional and calls `pkg.moveBody(from, to)` on the same editable
package. It checks the updated parameter and generic references through expression
views held before the move, then calls `pkg.write` once. There is no intermediate
CHIR file or serialization between editing and moving. `chir-dis` runs twice,
before editing and after the final write, printing only the two `foo` functions.
The final reload checks that `C.foo` has three blocks and no branch, and global
`foo` is a declaration.

Removing the conditional keeps its false arm, so the body always returns
`"global: " + text`. The unused string comparison remains; this example does not
run dead-code elimination.

Both functions are now generic: `foo<T>(x: T): String where T <: ToString`.
Global `foo` converts `x` to a string, returns `"global: empty"` if it is empty,
and otherwise returns `"global: " + text`. `C.foo` initially returns
`"member: " + x.toString()`. This exercises a conditional branch as well as
generic dispatch when the global body is moved.
The mover maps the two distinct generic type IDs positionally, checks equal
substituted bounds, and remaps body types, including the generic receiver type
stored in the `ToString.toString` invocation. The main verifies that invocation
and the retained constraint after serialization.

Generated artifacts are in `core/chir-lib/target/chir-body-demo/`:

- `bodymove.chir` and `bodymove-moved.chir`: original and final binaries, with
  matching `.chirtxt` full disassemblies.
- `before-foo.chirtxt` and `after-foo.chirtxt`: only the two functions.

The editable package preserves the absence of a FlatBuffers
file identifier in this toolchain's output, remaps parameter IDs, transfers the
return variable and body owner, and renames moved result locals to avoid the new
receiver's function-local naming collisions. Entity-table IDs do not change.
Existing destination-body entries are retained but no longer used by the method.
Source locations continue to point to the original body.

Generic instances (`genericDecl != 0`), nested bodies, lambdas, and spawn remain
unsupported. Substituting a composite type requires the corresponding destination
type to already exist in the package; the helper rejects missing specializations
rather than editing shared type entries. This demonstration does not implement
arbitrary generic specialization or cloning of generic custom definitions.

This is a serialization/disassembly demo, not an executable hotfix: the source
becomes a bodyless declaration, and the example's existing call to it is not
rewritten. Both tool disassemblies and a reload through chirLib were verified.

