import asyncio
import asyncio.subprocess
import os
import sys
import time
from dataclasses import dataclass
from pathlib import Path

MACOS = sys.platform == "darwin"
DYLIB_EXT = ".dylib" if MACOS else ".so"
DYLIB_PATH_VAR = "DYLD_LIBRARY_PATH" if MACOS else "LD_LIBRARY_PATH"


class MissingArtifacts(RuntimeError):
    """Raised by a testsuite when a testcase is run without its build artifacts."""
    pass


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
