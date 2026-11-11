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

from runner_lib import MissingArtifacts, TestIdentifier

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


def auto_status():
    # Default: interactive on a terminal, plain when captured/piped.
    return "interactive" if sys.stdout.isatty() else "plain"


class Permit:
    """One live line of the interactive progress area.

    A task acquires a permit for the duration of its build+run; while held,
    the permit's line shows the current activity. Releasing the permit frees
    both the concurrency slot and its line.
    """

    def __init__(self, reporter, index):
        self.reporter = reporter
        self.index = index
        self.activity: tuple[str, str] | None = None

    def set(self, phase, test_id):
        if self.reporter.interactive:
            self.activity = (phase, test_id)
            self.reporter._render()
        else:
            print(self.reporter._fit(self.reporter._format(phase, test_id)), flush=True)

    def release(self):
        self.activity = None
        self.reporter._pool.put_nowait(self)
        self.reporter._render()


class StatusReporter:
    """Progress display for the run.

    ``plain`` appends one line per phase (building/built/running/done) to stdout.

    ``interactive`` keeps an N-line live area at the bottom of the screen, one
    line per permit (= per parallel worker). A task must acquire a permit
    before running; all changes go through the permit, which triggers a render
    of the whole live area. Results are logged as permanent ``done``/``failed``
    lines (with a ``[finished/total]`` counter) above the live area; live lines
    show ``building``/``running`` without a counter. All prefixes are padded to
    one fixed width (computed from the total test count) so the ``<test>``
    column never shifts as the counter grows. The reporter is the single
    writer: every redraw is an atomic cursor-up + clear + rewrite, and every
    line is truncated to the terminal width so nothing wraps and the cursor
    math stays valid. Falls back to plain when stdout is not a terminal.
    """

    def __init__(self, mode, parallelism, total):
        self.interactive = mode == "interactive" and sys.stdout.isatty()
        self.total = total
        self._finished = 0
        self.n = max(1, parallelism)
        try:
            # Query the terminal directly: shutil.get_terminal_size honours a
            # stale COLUMNS env var, which over-reports and makes lines wrap.
            self.width = max(20, os.get_terminal_size(sys.stdout.fileno()).columns)
        except OSError:
            self.width = 80
        # Widest possible counter is [total/total]; the prefix column is
        # fixed for the whole run so lines never shift as the counter grows.
        ctr_max = f"[{total}/{total}]"
        self.prefix_width = max(len(f"failed {ctr_max}"), len("building"))
        self.permits = [Permit(self, i) for i in range(self.n)]
        self._pool: asyncio.Queue[Permit] = asyncio.Queue()
        for permit in self.permits:
            self._pool.put_nowait(permit)
        self._drawn = False

    def _fit(self, line):
        limit = self.width - 1
        if len(line) <= limit:
            return line
        tail = line[-16:]
        head = line[:max(0, limit - 3 - len(tail))]
        return head + "..." + tail

    def _format(self, verb, test_id, finished=None):
        if finished is None:
            body = verb
        else:
            body = f"{verb} [{finished}/{self.total}]"
        pad = " " * (self.prefix_width - len(body))
        return f"{body}{pad}- {test_id}"

    async def acquire(self):
        return await self._pool.get()

    def _render(self, move_up=True):
        if not self.interactive:
            return
        if move_up and self._drawn and self.n > 1:
            sys.stdout.write(f"\x1b[{self.n - 1}A")
        lines = []
        for permit in self.permits:
            if permit.activity is None:
                text = ""
            else:
                phase, test_id = permit.activity
                text = self._format(phase, test_id)
            lines.append(self._fit(text).ljust(self.width - 1))
        sys.stdout.write("\r" + "\n\r".join(lines))
        sys.stdout.flush()
        self._drawn = True

    def log_result(self, test_id, ok):
        self._finished += 1
        verb = "done" if ok else "failed"
        line = self._format(verb, test_id, self._finished)
        if not self.interactive:
            print(self._fit(line), flush=True)
            return
        if self._drawn:
            # Move to the top of the live area and clear it (not the trail
            # above), write the history line there, then re-render the live
            # area below it. Once the screen fills, the newline scrolls and
            # the trail grows upward.
            # Move to the top of the live area and clear it (not the trail
            # above), write the history line there, then re-render the live
            # area below it. Once the screen fills, the newline scrolls and
            # the trail grows upward.
            if self.n > 1:
                sys.stdout.write(f"\x1b[{self.n - 1}A")
            sys.stdout.write("\r\x1b[0J")
        sys.stdout.write(self._fit(line) + "\n")
        self._render(move_up=False)

    def finish(self):
        if not self.interactive:
            return
        if self._drawn:
            if self.n > 1:
                sys.stdout.write(f"\x1b[{self.n - 1}A")
            sys.stdout.write("\r\x1b[0J")
        sys.stdout.flush()


async def process_testcase(test_id, discovered, workdir, args, toolchain, results, reporter, permit):
    mod = discovered[test_id]
    tdir = workdir / test_id.path
    if not args.skip_build:
        permit.set("building", test_id)
        rc = await mod.build(test_id, str(tdir))
        if rc != 0:
            results.append((test_id, f"build failed (exit {rc})"))
            reporter.log_result(test_id, False)
            return
    permit.set("running", test_id)
    rc = await mod.run(test_id, str(tdir))
    cmd = await mod.run_command(test_id, str(tdir))
    write_run_sh(tdir, toolchain, cmd)
    ok = rc == 0
    results.append((test_id, "passed" if ok else "run failed"))
    reporter.log_result(test_id, ok)


def dump_test_output(tdir, reason):
    names = ["build.log"] if reason.startswith("build") else ["run.log", "run.err"]
    for name in names:
        path = tdir / name
        if not path.is_file():
            continue
        print(f"--- {name} ---")
        text = path.read_text(errors="replace")
        if not text.endswith("\n"):
            text += "\n"
        sys.stdout.write(text)


async def run_tests(args):
    workdir = Path(args.work_dir) if args.work_dir else ROOT / "workdir"
    toolchain = os.environ["TOOLCHAIN"]

    discovered = {}
    suites = load_suites()
    if args.testsuite:
        known = {name for name, _ in suites}
        unknown = [s for s in args.testsuite if s not in known]
        if unknown:
            print(f"Unknown testsuites: {', '.join(unknown)}", file=sys.stderr)
            return 1
        suites = [(name, mod) for name, mod in suites if name in args.testsuite]
    for name, mod in suites:
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
    results = []
    mode = auto_status() if args.status == "auto" else args.status
    reporter = StatusReporter(mode, parallelism, len(ids))

    async def bounded(test_id):
        permit = await reporter.acquire()
        try:
            await process_testcase(test_id, discovered, workdir, args, toolchain, results, reporter, permit)
        finally:
            permit.release()

    tasks = [asyncio.create_task(bounded(t)) for t in ids]
    done, pending = await asyncio.wait(tasks, return_when=asyncio.FIRST_EXCEPTION)
    for task in pending:
        task.cancel()
    await asyncio.gather(*pending, return_exceptions=True)
    reporter.finish()
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
        if args.dump_on_error:
            dump_test_output(workdir / test_id.path, reason)
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
    parser_test.add_argument("--testsuite", action="append", metavar="NAME", default=[],
                             help="Testsuite to run (repeatable); when given, only these suites run")
    parser_test.add_argument("--skip-build", action="store_true", help="Skip build, start running tests")
    parser_test.add_argument("--describe", action="store_true", help="Print run command and log paths for failed testcases")
    parser_test.add_argument("--dump-on-error", action=argparse.BooleanOptionalAction, default=True,
                             help="Dump failed test's stdout/stderr (run.log/run.err, or build.log for build "
                             "failures) below its FAILED line; --no-dump-on-error disables")
    parser_test.add_argument("--status", choices=["auto", "plain", "interactive"], default="auto",
                             help="Progress display: plain (one line per phase) or interactive "
                             "(live N-line area); default: auto - interactive on a TTY, else plain")
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
