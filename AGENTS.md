# AGENTS.md

Guidance for AI coding agents working in this repository.

For repository layout, build instructions and configuration, see [README.md](README.md)
- this file does not repeat it.
For the end-to-end test suites (standalone / hotfix) - toolchain setup and how
to run them - see [test/README.md](test/README.md).

## Verifying changes

Besides `sbt test`, changes to compiler behavior should be verified with the
end-to-end standalone suite (at least the tests related to the change),
especially when CHIR parsing, lowering or ABI conventions are touched:

```bash
$ python3 test/runner.py test --filter standalone/cj/bugs
```

Toolchain setup, suite descriptions and more `runner.py` options are described
in [test/README.md](test/README.md).

## Scala 3 style

These conventions apply to all new code and are enforced by review.

1. **Parenthesize `for` / `while` heads.** Never the bare `for x <- xs do` /
   `while c do` forms. Multi-enumerator heads use `;`:
   `for (x <- xs; y <- ys) { ... }`. A multiline `for` body is always
   enclosed in braces.

2. **Prefer braces; do not rely on significant-indentation syntax.** Do not
   use indentation to delimit anything beyond a single expression: no
   bare-newline statement bodies, no `do`-headed loops, no indent-delimited
   match/if/lambda blocks. Braced bodies are used even for one-line
   definitions like `enum Color { case Red, Green, Blue }`.

3. **Brace any multiline def/val body.** If the body of a def/val spans
   several lines, enclose it in a `{ }` block — not only multi-statement
   bodies:

   ```scala
   def update(x: Int): Unit = {
     cache.clear()
     cache += x
   }
   ```

   Only single-line bodies stay unbraced: `def f(x: Int): Int = x + 1`.
   (A `match` on the `=` line per rule 14 is already braced.) Multiline
   `for` / `if` bodies are always braced as well (rules 1 and 5).

4. **Matches are always braced**: `x match { case ... }`.

5. **No `then`.** Use `if (cond) { ... } else { ... }`; one-liners may omit
   braces: `if (cond) x else y`. Multi-line bodies get braces; else chains
   close on the same line: `} else if (...) {`.

6. **`override` only when overriding a concrete implementation** - never on a
   method that merely implements an abstract member.

7. **Typed case bindings don't invent names.** In `case <binding>: <Type>`,
   the binding is a single letter or shadows the matched variable; avoid
   arbitrary short invented names.

8. **No em/en dashes or unicode symbols in comments and documentation.**
   In comments and docs use plain ASCII punctuation: use `-` instead of em
   or en dashes, write out arrows in words ("to", "->"), and avoid other
   decorative unicode characters.

9. **Test suites are named `*Suite`, not `*Spec`.** Class name and file name
   follow: `class ChirOpsSuite extends AnyFunSuite` in `ChirOpsSuite.scala`.

10. **ScalaTest matcher DSL instead of `assert` in tests.** Use the
    matchers, e.g. `x shouldBe y`, `x should not be y`, `xs shouldBe
    List(1, 2)`, `x shouldBe true`.

11. **No `final` on case classes.** We do not inherit from case classes, so
    `final` adds nothing: write `case class`, not `final case class`.

12. **Partial-function lambdas instead of explicit match lambdas.** Prefer
    `xs.map { case (k, v) => ... }` over `xs.map { kv => kv match { ... } }`.

13. **Qualified names for mutable collections.** Do not import
    `LinkedHashMap`/`LinkedHashSet` unqualified; write
    `mutable.LinkedHashMap` / `mutable.LinkedHashSet` at use sites.

14. **Put `match` on the line with `=`.**
    `def f(x: Int): Int = x match { ... }`.

15. **Align `case` arrows within rows of single-line cases.** Consecutive
    single-line cases in a row are aligned by `=>`. A bodiless default
    (`case _ =>`) is not aligned, and neither are multiline cases.

16. **`foreach` vs `for`.** Single-expression `foreach` is fine
    (`xs.foreach(f)`). Convert to `for` when the lambda is a
    multi-statement block (`xs.foreach { x => s1; s2 }` becomes
    `for (x <- xs) { ... }`) or its body is a lone `if`
    (`xs.foreach { x => if (p(x)) ... }` becomes
    `for (x <- xs if p(x)) { ... }`; no `;` before a guard). When a
    loop's body is just another loop over a different collection, join
    them into a single `for` (`for (x <- xs; y <- f(x)) { ... }`).

17. **Keep blank lines regular.** No stray blank lines before a closing
    brace.
