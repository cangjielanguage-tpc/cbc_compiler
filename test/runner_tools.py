import os
import shutil
from pathlib import Path


class MissingArtifacts(RuntimeError):
    """Raised by a testsuite when a testcase is run without its build artifacts."""
    pass


def tool_sh():
    return str(Path(__file__).resolve().parent / "tool.sh")


async def run_in_toolchain(cmd, cwd=None, log=None, stderr_log=None):
    """Runs a command wrapped by tool.sh (sources $TOOLCHAIN/envsetup.sh)."""
    cmd = [tool_sh()] + cmd
    return await run_async(cmd, shell=False, log=log, env=os.environ.copy(), cwd=cwd, stderr_log=stderr_log)


async def run_cjc(input_file, output_file, output_type=None, additional_args=None, log=None):
    args = ["--fobf-layout", "--fno-obf-export-symbols", "--fobf-line-number", "--fobf-source-path",
            "--obf-sym-output-mapping", f"{output_file}.obf.map"]
    if output_type is not None:
        args += ["--output-type", output_type]
    args += list(additional_args or [])
    args += ["-o", output_file]
    cmd = ["cjc", "--int-overflow", "wrapping", "-Woff", "unused", input_file] + args
    return await run_in_toolchain(cmd, log=log)


def patched(name):
    return f'{name}-patched'


def dotll(name):
    return f'{name}.ll'


def dotll_diff(name):
    return f'{name}.ll_diff'


def dotbc(name):
    return f'{name}.bc'


def dotcj(name):
    return f'{name}.cj'


def dotcjaot(name):
    return f'{name}.aot.cj'


def dotpdba(name):
    return f'{name}.pdba'


def dotcbc(name):
    return f'{name}.cbc'


def dotactual(name):
    return f'{name}.actual'


def dotexpected(name):
    return f'{name}.expected'


def dotasm(name):
    return f'{name}.asm'


def dotchir(name):
    return f'{name}.chir'


def dotobfdotmap(name):
    return f'{name}.obf.map'


def java_cmd():
    """Resolves the java executable path via JAVA_HOME or system PATH."""
    java_home = os.getenv('JAVA_HOME')
    if java_home:
        return os.path.join(java_home, 'bin', 'java')
    java_exec = shutil.which('java')
    if java_exec:
        return java_exec
    raise EnvironmentError('JAVA_HOME environment variable is not set and java is not found in PATH')


def diff(actual, expected):
    return ['diff', '-u', expected, actual]


import asyncio
import asyncio.subprocess


async def run_async(what, log=None, env=None, shell=False, cwd=None, stderr_log=None) -> int:
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
