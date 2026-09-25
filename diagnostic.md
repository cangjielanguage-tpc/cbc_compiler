# Overview

The supplied reproducer deterministically crashes the CBC interpreter with
`Called abstract method` followed by `SIGILL`. The minimized program combines
an interface `A` with a concrete default `foo` implementation and an interface
`B` with an abstract `foo`, then invokes `foo` through `B` on an instance of
`S`.

The root cause is incomplete interface-method resolution in the runtime method
table. The table for `S` retains `B.foo` as abstract instead of satisfying that
interface entry with the compatible concrete default implementation from `A`.
The generated call therefore dispatches to the intentional abstract-method
stub. This is a dispatch-table construction bug (or, if this combination is
invalid by language rules, a missing compiler validation); it is not a random
interpreter failure.

# Error log

Running the supplied `repro-interpreter.sh` produces:

```text
/workspace/cbc-engine/src/runtimesupport/impl/typeinfo_factory.cpp:325: assertion failed: Called abstract method
1552 E CJNative Handle signal: 4.
1552 E Thread "launcher" catched unhandled SIGILL (Illegal instruction) from managed frame.
1552 E   #0  ... in ? from /toolchain/tools/lib/libcbcengine.so
1552 E   #1  ... in ? from /toolchain/tools/lib/libcbcengine.so
./repro-interpreter.sh: line 5:  1551 Illegal instruction (core dumped) launcher default.cbc
```

The prepared GDB run identifies the same call path:

```text
#0  ReportFailure ... typeinfo_factory.cpp:325
#1  RTSupport::AbstractMethodCalled() ... typeinfo_factory.cpp:325
#2  perform_2i_call() ... trampolines.S:602
#3  perform_2i_call() ... trampolines.S:612
#4  perform_2i_call() ... trampolines.S:612
#5  engine_iregs_only_c2i_call() ... trampolines.S:385
```

# Affected tests

# Reproducer

The exact supplied commands are:

```bash
bash ./prepare.sh
bash ./repro-interpreter.sh
```

The minimized source is:

```cangjie
interface A {
    func foo(): Int64 {
        return 0
    }
}
interface B {
    func foo(): Int64
}
class S {}
extend S <: A & B {}
main(): Int64 {
    let b: B = S()
    return b.foo()
}
```

# Analysis

The generated CBC contains both interfaces on `S`, and the generated method
body performs an interface call specifically to `B.foo`:

```text
default:S {
  interfaces {
    @default:A
    @default:B
  }
}
...
0: newobj IR1, @default:S
15: load.ti IR8, @218
21: mov.ref IR1, IR9
25: call.interf.g 2, @1
...
1 - @default:B.foo() -> Int64
```

The serialized definitions confirm that `A.foo` has executable CBC code while
`B.foo` is abstract:

```text
default:A.foo: flags: 1 VIRTUAL ...
  0: mov.W64 IR1, 0
  3: ret.W64 IR1
default:B.foo: flags: 1 ABSTRACT VIRTUAL ...
```

The method-table builder copies each interface table independently. Its
`AddInterface` implementation appends all entries and subtable ranges; it does
not reconcile identical method signatures across interfaces. Its `AddMethod`
override pass only processes methods declared by the current type or extension,
and `S` declares none. Consequently the `B` subtable continues to point at the
abstract `B.foo` entry even though `A` supplies a compatible concrete method.

The runtime then turns abstract entries into an explicit failure stub. The
relevant implementation is:

```text
if (flags.Is(Image::MethodFlag::ABSTRACT)) {
    // TODO: put stub method that throws
    return { nullptr, reinterpret_cast<DYN_FuncPtr>(&AbstractMethodCalled) };
}
```

That stub is exactly what the interface trampoline calls, producing the
assertion and SIGILL. The evidence also shows the runtime method-table log
listing `A.foo` from `A` and `B.foo` from `B`, followed by resolution of
`@default:B.foo`, which rules out an unrelated GC, object-layout, or assembly
failure.

The appropriate fix is to define interface-method conflict/default resolution
when constructing the method table: for matching signatures, a concrete
default implementation must satisfy an abstract interface requirement and be
used in the corresponding interface view, or the compiler must reject the
combination with a proper diagnostic if the language intentionally forbids
that inheritance pattern. At minimum, the compiler/runtime must not emit a
successful program whose interface table dispatches this valid-looking call to
the abstract stub.
