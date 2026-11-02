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
   `for (x <- xs; y <- ys) { ... }`.

2. **Prefer braces; do not rely on significant-indentation syntax.** Do not
   use indentation to delimit anything beyond a single expression: no
   bare-newline statement bodies, no `do`-headed loops, no indent-delimited
   match/if/lambda blocks. Braced bodies are used even for one-line
   definitions like `enum Color { case Red, Green, Blue }`.

3. **Brace multi-statement bodies.** A def/val whose body consists of more
   than one statement (or expression) uses a `{ }` block:

   ```scala
   def update(x: Int): Unit = {
     cache.clear()
     cache += x
   }
   ```

   Single-expression bodies may stay unbraced: `def f(x: Int): Int = x + 1`.

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
