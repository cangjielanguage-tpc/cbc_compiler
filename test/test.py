import argparse
import asyncio
import importlib.util
import inspect
import os
import shlex
import shutil
import sys
from os.path import isfile
from pathlib import Path

from runner_tools import MissingArtifacts
from test_identifier import TestIdentifier

ROOT = Path(__file__).resolve().parent
CONTRACT = ("discover", "build", "run", "run_command")


def load_suites():
    suites = []
    for f in sorted(ROOT.glob("**/*_testsuite.py")):
        if "workdir" in f.parts:
            continue
        mod_name = "testsuite_mod_" + f.stem
        spec = importlib.util.spec_from_file_location(mod_name, f)
        if spec is None or spec.loader is None:
            raise SystemExit(f"Cannot load testsuite module from {f}")
        mod = importlib.util.module_from_spec(spec)
        sys.modules[mod_name] = mod
        spec.loader.exec_module(mod)
        for name in CONTRACT:
            fn = getattr(mod, name, None)
            if fn is None or not inspect.iscoroutinefunction(fn):
                raise SystemExit(f"Suite {f.name} does not provide async function '{name}'")
        suites.append((f.stem[: -len("_testsuite")], mod))
    if not suites:
        raise SystemExit("No testsuites found (expected files named {name}_testsuite.py)")
    return suites


def read_id_list(path):
    ids = []
    with open(path) as fh:
        for line in fh:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            ids.append(line)
    return ids


def autodetect_parallelism():
    try:
        limit = len(os.sched_getaffinity(0))
    except OSError:
        limit = os.cpu_count() or 1
    return max(1, limit // 4)


def write_run_sh(tdir, toolchain, cmd):
    run_sh = tdir / "run.sh"
    run_sh.write_text(
        "#!/bin/bash\n"
        f"export TOOLCHAIN={shlex.quote(toolchain)}\n"
        f"exec bash {shlex.quote(str(ROOT / 'tool.sh'))} bash -c {shlex.quote(cmd)}\n"
    )
    run_sh.chmod(0o755)


async def process_testcase(test_id, discovered, workdir, args, toolchain, results):
    mod = discovered[test_id]
    tdir = workdir / test_id.path
    if not args.skip_build:
        rc = await mod.build(test_id, str(tdir))
        if rc != 0:
            results.append((test_id, f"build failed (exit {rc})"))
            return
    rc = await mod.run(test_id, str(tdir))
    cmd = await mod.run_command(test_id, str(tdir))
    write_run_sh(tdir, toolchain, cmd)
    results.append((test_id, "run failed" if rc != 0 else "passed"))


async def run_tests(args):
    workdir = Path(args.work_dir) if args.work_dir else ROOT / "workdir"
    toolchain = os.environ["TOOLCHAIN"]

    discovered = {}
    for name, mod in load_suites():
        for test_id in await mod.discover():
            discovered[test_id] = mod

    explicit = [TestIdentifier.from_str(t) for t in args.test]
    if args.tests:
        explicit += [TestIdentifier.from_str(t) for t in read_id_list(args.tests)]
    if explicit:
        unknown = [t for t in explicit if t not in discovered]
        if unknown:
            print(f"Unknown testcases: {', '.join(str(t) for t in unknown)}", file=sys.stderr)
            return 1
        ids = [t for t in explicit if t in discovered]
    else:
        ids = sorted(discovered)
    if not ids:
        print("No testcases to run")
        return 0

    parallelism = max(1, int(args.parallelism)) if args.parallelism is not None else autodetect_parallelism()
    semaphore = asyncio.Semaphore(parallelism)
    results = []

    async def bounded(test_id):
        async with semaphore:
            await process_testcase(test_id, discovered, workdir, args, toolchain, results)

    tasks = [asyncio.create_task(bounded(t)) for t in ids]
    done, pending = await asyncio.wait(tasks, return_when=asyncio.FIRST_EXCEPTION)
    for task in pending:
        task.cancel()
    await asyncio.gather(*pending, return_exceptions=True)
    for task in done:
        exc = task.exception()
        if exc is None:
            continue
        if isinstance(exc, MissingArtifacts):
            print(f"Hard error: {exc}", file=sys.stderr)
            return 2
        raise exc

    failed = [entry for entry in results if entry[1] != "passed"]
    passed_count = len(ids) - len(failed)
    print(f"[Summary] {passed_count} passed, {len(failed)} failed")
    for test_id, reason in failed:
        print(f"FAILED {test_id} ({reason})")
        if args.describe:
            tdir = workdir / test_id.path
            cmd = await discovered[test_id].run_command(test_id, str(tdir))
            print(f"  run: bash {ROOT / 'tool.sh'} bash -c {shlex.quote(cmd)}")
            print(f"  logs: {tdir / 'run.log'}, {tdir / 'run.err'}, {tdir / 'diff.log'}")

    failed_ids = [test_id for test_id, _ in failed]
    if failed_ids:
        failed_list = Path(args.failed_list) if args.failed_list else workdir / "failed.txt"
        failed_list.parent.mkdir(parents=True, exist_ok=True)
        failed_list.write_text("\n".join(str(t) for t in failed_ids) + "\n")
        print(f"Failed test list written to {failed_list}")
        return 1
    return 0


def cmd_test(args):
    toolchain = args.cangjie_home or os.environ.get("CANGJIE_HOME")
    if not toolchain:
        print("No toolchain path specified. Use --cangjie-home or set CANGJIE_HOME environment variable.", file=sys.stderr)
        return 1
    source = "--cangjie-home flag" if args.cangjie_home else "CANGJIE_HOME env var"
    if not isfile(toolchain + "/envsetup.sh"):
        print(f"There is no envsetup.sh in {toolchain} (set via {source}), path probably points to wrong place", file=sys.stderr)
        return 1
    os.environ["TOOLCHAIN"] = toolchain
    os.environ["CANGJIE_STDX_PATH"] = os.path.expanduser(os.environ.get(
        "CANGJIE_STDX_PATH", f"{toolchain}/stdx/dynamic/stdx"))
    if args.jc_options:
        os.environ["CBC_JC_OPTIONS"] = args.jc_options
    print(f"CANGJIE_TOOLCHAIN: {toolchain}")
    return asyncio.run(run_tests(args))


def cmd_clean(args):
    workdir = Path(args.work_dir) if args.work_dir else ROOT / "workdir"
    if workdir.exists():
        shutil.rmtree(workdir)
        print(f"Removed {workdir}")
    return 0


async def discovery():
    for name, mod in load_suites():
        for test_id in sorted(await mod.discover()):
            print(test_id)


def cmd_discovery_async(args):
    return asyncio.run(discovery())


def main():
    parser = argparse.ArgumentParser(prog="CBC testsuite")
    subparsers = parser.add_subparsers(required=True)

    parser_test = subparsers.add_parser("test", help="Build and run testcases")
    parser_test.set_defaults(func=cmd_test)
    parser_test.add_argument("--cangjie-home", help="Path to Cangjie toolchain (overrides CANGJIE_HOME env var)")
    parser_test.add_argument("--test", action="append", default=[], help="Testcase ID to run (repeatable)")
    parser_test.add_argument("--tests", help="File with testcase IDs, one per line (# comments allowed)")
    parser_test.add_argument("--skip-build", action="store_true", help="Skip build, start running tests")
    parser_test.add_argument("--describe", action="store_true", help="Print run command and log paths for failed testcases")
    parser_test.add_argument("-j", "--parallelism", help="Number of testcases processed in parallel")
    parser_test.add_argument("--work-dir", help="Work directory (default ./workdir)")
    parser_test.add_argument("--failed-list", help="File to dump failed testcase IDs (default <work-dir>/failed.txt)")
    parser_test.add_argument("--jc-options", nargs="?", default="", help="Options for cbc-compiler.jar")

    parser_discovery = subparsers.add_parser("test-discovery", help="Discover all testcases of all test suites")
    parser_discovery.set_defaults(func=cmd_discovery_async)
    parser_discovery.add_argument("--work-dir", help="Work directory (default ./workdir)")

    parser_clean = subparsers.add_parser("clean", help="Remove the work directory")
    parser_clean.set_defaults(func=cmd_clean)
    parser_clean.add_argument("--work-dir", help="Work directory (default ./workdir)")

    args = parser.parse_args()
    sys.exit(args.func(args))


if __name__ == "__main__":
    main()
