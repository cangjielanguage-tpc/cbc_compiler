#!/usr/bin/env python3
"""Patch-gen functional tests (migrated from hotfix_tools/patch-gen/test).

Pipeline per test (mirrors hotfix_tools build.py `run_patch_gen_tests_at_level`):
  1. build the patchable hotfix library (`lib/hotfix.a`) once per run;
  2. compile the test's base `.cj` (and `_patched.cj` when present) with the
     hotfix plugin, emitting CHIR;
  3. run `patch-gen --test` on the base (and patched) CHIR;
  4. validate the generated patch with `chir-dis`;
  5. compare the patch-gen output (stdout+stderr, combined) against the
     `<test>.expected` file. The exit code of patch-gen is NOT checked: tests
     whose expected outcome is an error compare the error message.

Unlike hotfix_tools this suite runs the Scala patch-gen (deployed as
`tools/bin/patch-gen` in the toolchain), which needs the explicit `--test`
flag to print the TEST-build info dump. Compile levels default to O0/O1
(hotfix_tools default; O2 expected files are stale upstream).
"""

import argparse
import glob
import os
import shutil
import subprocess
import sys
from pathlib import Path

TEST_DIR = Path(__file__).resolve().parent
TOOL_SH = TEST_DIR.parent / "tool.sh"


def run(cmd, cwd, env, capture=False):
    if capture:
        return subprocess.run(cmd, cwd=cwd, env=env, stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT, text=True)
    return subprocess.run(cmd, cwd=cwd, env=env)


def tool_env(toolchain: str) -> dict:
    env = os.environ.copy()
    env.pop("HOTFIX_GEN_THROW_STUBS", None)
    env.setdefault("HOTFIX_PATCH_ALL", "false")
    env["TOOLCHAIN"] = toolchain  # consumed by test/tool.sh
    env["CANGJIE_STDX_PATH"] = os.path.expanduser(
        env.get("CANGJIE_STDX_PATH", f"{toolchain}/stdx/dynamic/stdx"))
    # The plugin loads libstdx.plugin.manager.so from the stdx dir itself.
    env["LD_LIBRARY_PATH"] = os.pathsep.join(filter(None, [
        env["CANGJIE_STDX_PATH"], env.get("LD_LIBRARY_PATH", "")]))
    return env


def in_tool_env(env, cmd):
    # envsetup.sh must be sourced before every toolchain command (cjc, chir-dis,
    # patch-gen); test/tool.sh does exactly that.
    return [str(TOOL_SH)] + cmd


def expected_file(test_data: Path, name: str, level: str):
    level_specific = test_data / f"{name}.expected_{level}"
    if level_specific.is_file():
        print(f"  using {level_specific.name}")
        return level_specific
    generic = test_data / f"{name}.expected"
    if generic.is_file():
        print(f"  no {name}.expected_{level}; using {generic.name}")
        return generic
    return None


def canonicalize_dump(text: str) -> str:
    '''
    Canonical form of the patch-gen `--test` info dump for comparison.

    The dump is a sequence of sections ("global vars", "global funcs",
    "types"), each a flat list of names printed in the patch file's
    package-list order. That order is inherited from the input CHIRs, whose
    internal ordering is nondeterministic in cjc (varies run to run), so it
    cannot be pinned by expected files. Sort the lines within each section;
    keep the section order and each line's content (e.g. the `[unreachable] `
    prefix) intact. Any text before the first section header is kept as-is.
    '''
    sections = []
    current = []
    for line in text.split("\n"):
        if line in ("global vars", "global funcs", "types"):
            if current:
                sections.append(current)
            current = [line]
        elif current:
            current.append(line)
        else:
            sections.append([line])
    if current:
        sections.append(current)
    return "\n".join(
        "\n".join(s[:1] + sorted(s[1:])) for s in sections
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--cangjie-home", default=os.environ.get("CANGJIE_HOME", ""),
                        help="toolchain path (defaults to $CANGJIE_HOME)")
    parser.add_argument("--filter", nargs="*", default=[],
                        help="run only tests whose names start with one of the prefixes")
    parser.add_argument("--compile-level", nargs="+", choices=["O0", "O1", "O2"],
                        default=["O0", "O1"], help="optimization levels (default: O0 O1)")
    parser.add_argument("--patch-gen", default="",
                        help="patch-gen binary to test (default: <toolchain>/tools/bin/patch-gen)")
    args = parser.parse_args()

    toolchain = os.path.expanduser(args.cangjie_home)
    if not toolchain or not Path(toolchain, "envsetup.sh").is_file():
        print(f"Error: toolchain not found at '{toolchain}' (--cangjie-home or $CANGJIE_HOME)")
        return 2
    patch_gen = os.path.expanduser(args.patch_gen) if args.patch_gen \
        else os.path.join(toolchain, "tools", "bin", "patch-gen")
    if not os.path.isfile(patch_gen):
        print(f"Error: patch-gen binary not found: {patch_gen}")
        return 2

    test_data = TEST_DIR / "test_data"
    all_tests = sorted(p.stem for p in test_data.glob("*.cj") if not p.stem.endswith("_patched"))
    tests = [t for t in all_tests if any(t.startswith(f) for f in args.filter)] if args.filter else all_tests
    if args.filter:
        missing = [f for f in args.filter if not any(t.startswith(f) for t in all_tests)]
        if missing:
            print(f"Fail. Unknown tests in --filter: {','.join(missing)}")
            return 2
    if not tests:
        print("No tests matched")
        return 0

    env = tool_env(toolchain)

    # cjc and the plugin write intermediate files into cwd; work on a copy.
    work = TEST_DIR / "work"
    if work.exists():
        shutil.rmtree(work)
    work.mkdir(parents=True)
    for pattern in ("*.cj", "*.expected*"):
        for f in test_data.glob(pattern):
            (work / f.name).write_bytes(f.read_bytes())
    (work / "lib").mkdir()
    for f in (test_data / "lib").glob("*"):
        if f.is_dir():
            shutil.copytree(f, work / "lib" / f.name)
        else:
            (work / "lib" / f.name).write_bytes(f.read_bytes())

    print("Building patchable hotfix lib")
    res = run(in_tool_env(env, ["cjc", "-p", "hotfix", "--output-type=staticlib", "-o", "hotfix.a"]),
              cwd=work / "lib", env=env)
    if res.returncode != 0:
        print("Fail: cannot build lib/hotfix.a")
        return 1

    plugin = os.path.join(toolchain, "tools", "lib", "libhotfix-plugin.so")
    failed = []
    for level in args.compile_level:
        print(f"\nRunning patch-gen functional tests with -{level}")
        for name in tests:
            print(f"  {name}")
            exp = expected_file(test_data, name, level)
            if exp is None:
                print("    Fail. Expected file was not found")
                failed.append(f"{level}:{name}")
                continue

            compile_base = ["cjc", "-Woff", "unused", f"{name}.cj", "lib/hotfix.a",
                            "--import-path", "lib", "--output-type=staticlib",
                            "--plugin", plugin, f"-{level}",
                            "-o", f"lib{name}.chir", "--emit-chir"]
            base_chir = work / f"lib{name}.chir"
            base_chir.unlink(missing_ok=True)
            if run(in_tool_env(env, compile_base), cwd=work, env=env).returncode != 0:
                print("    Fail. Base compilation failed")
                failed.append(f"{level}:{name}")
                continue

            patched_file = work / f"{name}_patched.cj"
            patch_chir = f"difflib{name}.chir"
            generator_command = [patch_gen, "--test", f"lib{name}.chir"]
            if patched_file.is_file():
                patch_chir = f"difflib{name}_patched.chir"
                compile_patched = compile_base.copy()
                compile_patched[3] = patched_file.name
                compile_patched[-2] = f"lib{name}_patched.chir"
                if run(in_tool_env(env, compile_patched), cwd=work, env=env).returncode != 0:
                    print("    Fail. Patched compilation failed")
                    failed.append(f"{level}:{name}")
                    continue
                generator_command.append(f"lib{name}_patched.chir")

            result = run(in_tool_env(env, generator_command), cwd=work, env=env, capture=True)
            generated_patch = work / patch_chir
            if generated_patch.is_file():
                dis = run(in_tool_env(env, ["chir-dis", generated_patch.name]),
                          cwd=work, env=env, capture=True)
                if dis.returncode != 0:
                    print("    Fail. Generated patch is not valid CHIR (chir-dis failed)")
                    print("    " + (dis.stdout or "").strip().replace("\n", "\n    "))
                    failed.append(f"{level}:{name}")
                    continue

            if canonicalize_dump(result.stdout) == canonicalize_dump(exp.read_text()):
                print("    Passed")
            else:
                print("    Fail. Output differs\n    --- expected ---\n    "
                      + exp.read_text().replace("\n", "\n    ")
                      + "\n    --- actual ---\n    "
                      + (result.stdout or "").replace("\n", "\n    "))
                failed.append(f"{level}:{name}")

    print()
    if failed:
        print("Failed tests:")
        for t in failed:
            print(f"  {t}")
        return 1
    print("All tests passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
