import asyncio
import asyncio.subprocess
import difflib
import os
import platform
import re
import shlex
import sys
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path
from typing import Dict

MACOS = sys.platform == "darwin"
DYLIB_EXT = ".dylib" if MACOS else ".so"
DYLIB_PATH_VAR = "DYLD_LIBRARY_PATH" if MACOS else "LD_LIBRARY_PATH"
ENGINE_FRAME = re.compile(r"^[ \t]+at cangjie\.runtime\.cbcengine(?:\.|::)")


class MissingArtifacts(RuntimeError):
    """Raised by a testsuite when a testcase is run without its build artifacts."""
    pass


def remove_engine_frames(text):
    return "".join(
        line for line in text.splitlines(keepends=True)
        if not ENGINE_FRAME.match(line)
    )


def compare_output(expected: Path | None, actual: Path, wdir: Path) -> int:
    """Compares the actual output against the expected file (None = no
    expected file): engine frames are stripped from both sides, a mismatch
    writes diff.log and returns 1."""
    exp_txt = remove_engine_frames(expected.read_text()) if expected is not None and expected.is_file() else None
    act_txt = remove_engine_frames(actual.read_text())
    if exp_txt is None or exp_txt != act_txt:
        diff_lines = difflib.unified_diff(exp_txt.splitlines(keepends=True) if exp_txt is not None else [],
                                          act_txt.splitlines(keepends=True),
                                          "expected", "actual")
        (wdir / "diff.log").write_text("".join(diff_lines))
        return 1
    return 0


def tool_sh():
    return str(Path(__file__).resolve().parent / "tool.sh")


def classify_cmd(cmd) -> str:
    """Timing tag for a command: executable basename, unwrapping shell wrappers."""
    if isinstance(cmd, str):
        cmd = cmd.split()
    if not cmd:
        return "other"
    exe = os.path.basename(str(cmd[0]))
    if exe in ("bash", "sh", "tool.sh") and len(cmd) > 1 and cmd[1] != "-c":
        return classify_cmd(cmd[1:])
    return exe


async def run_async(what, log=None, env=None, shell=False, cwd=None, stderr_log=None,
                    timings=None, tag=None) -> int:
    t0 = time.monotonic()
    try:
        return await _spawn(what, log=log, env=env, shell=shell, cwd=cwd, stderr_log=stderr_log)
    finally:
        if timings is not None:
            key = tag or classify_cmd(what)
            timings[key] = timings.get(key, 0.0) + (time.monotonic() - t0)


async def _spawn(what, log=None, env=None, shell=False, cwd=None, stderr_log=None) -> int:
    async def logger(log_file, stream):
        while not stream.at_eof():
            data = await stream.readline()
            decoded = data.decode() if isinstance(data, bytes) else data
            if log_file is not None:
                log_file.write(decoded)

    kwargs = {}
    if log is not None:
        kwargs["stdout"] = asyncio.subprocess.PIPE
    if stderr_log is not None:
        kwargs["stderr"] = asyncio.subprocess.PIPE
    if env:
        kwargs["env"] = env

    if shell:
        process = await asyncio.create_subprocess_shell(what, cwd=cwd, **kwargs)
    else:
        process = await asyncio.create_subprocess_exec(*what, cwd=cwd, **kwargs)

    stdout_task = None
    stderr_task = None
    if log is not None:
        stdout_task = asyncio.create_task(logger(log, process.stdout))
    if stderr_log is not None:
        stderr_task = asyncio.create_task(logger(stderr_log, process.stderr))

    tasks = [t for t in [stdout_task, stderr_task] if t is not None]
    if tasks:
        await asyncio.gather(*tasks)
    return await process.wait()


async def run_in_toolchain(cmd, cwd=None, log=None, stderr_log=None, env=None,
                           timings=None, tag=None):
    """Runs a command wrapped by tool.sh (sources $TOOLCHAIN/envsetup.sh)."""
    if timings is not None and tag is None:
        tag = classify_cmd(cmd)
    cmd = [tool_sh()] + cmd
    if env is None:
        env = os.environ.copy()
    return await run_async(cmd, shell=False, log=log, env=env, cwd=cwd, stderr_log=stderr_log,
                           timings=timings, tag=tag)


async def run_cjc(input_file, output_file, output_type=None, additional_args=None, log=None,
                  cwd=None, env=None, timings=None):
    args = ["--fobf-layout", "--fno-obf-export-symbols", "--fobf-source-path",
            "--obf-sym-output-mapping", f"{output_file}.obf.map"]
    if output_type is not None:
        args += ["--output-type", output_type]
    args += list(additional_args or [])
    args += ["-o", output_file]
    cmd = ["cjc", "--int-overflow", "wrapping", "-Woff", "unused", input_file] + args
    return await run_in_toolchain(cmd, log=log, cwd=cwd, env=env, timings=timings, tag="cjc")


async def run_cc(input_file: Path, output_file: Path, log=None, timings=None) -> int:
    cmd = ["clang", "-shared", "-fPIC", "-o", str(output_file), str(input_file)]
    if platform.machine().lower() in ("x86_64", "amd64", "x64"):
        cmd.append("-mf16c")
    result = await run_in_toolchain(cmd, log=log, stderr_log=log, timings=timings)
    if result != 0 and log is not None:
        log.write(f"cmd: {shlex.join(cmd)}\n")
    return result


@dataclass(frozen=True, order=True)
class TestIdentifier:
    """A testcase: (suite_name, test_name, mode).

    Serialized as ``suite_name::test_name::mode``. ``::`` separates the three
    values: the first ``::`` ends the suite name, the last ``::`` starts the
    mode, and everything between them is the test name (which may contain
    ``/``). A suite with a single mode has an empty mode (``asm::simple``).
    """
    suite_name: str
    test_name: str
    mode: str

    def __str__(self):
        return f"{self.suite_name}::{self.test_name}::{self.mode}"

    @classmethod
    def from_str(cls, s: str) -> "TestIdentifier":
        suite_name, rest = s.split("::", 1)
        if "::" in rest:
            test_name, mode = rest.rsplit("::", 1)
        else:
            test_name, mode = rest, ""
        return cls(suite_name, test_name, mode)

    @property
    def path(self) -> str:
        # Filesystem-safe form for workdir subpaths: "::" -> "/".
        return str(self).replace("::", "/")


class TestSuite(ABC):
    """A testsuite: discovers its testcases, builds and runs each one."""

    SUITE: str

    def __init__(self):
        self.timings: Dict[str, float] = {}

    @abstractmethod
    async def discover(self) -> list[TestIdentifier]:
        ...

    async def setup(self, workdir: Path) -> None:
        """Prepares suite-wide resources in the work directory.

        Called once in the beginning of the testsuite run, before any testcase
        is built or run, but only for suites that have testcases selected."""


    @abstractmethod
    async def build(self, test_id: TestIdentifier, workdir: Path) -> int:
        ...

    @abstractmethod
    async def run(self, test_id: TestIdentifier, workdir: Path) -> int:
        ...
