import asyncio
import difflib
import fnmatch
import glob
import io
import os
import shutil
from pathlib import Path

from runner_tools import MissingArtifacts, java_cmd, run_cjc, run_in_toolchain

SUITE = "cj"
HERE = Path(__file__).resolve().parent
TESTS_CFG = HERE / "tests.cfg"
MODES = ("default", "O2")
_copy_locks = {}


def _copy_lock(key):
    return _copy_locks.setdefault(key, asyncio.Lock())


def load_excludes():
    patterns = []
    if TESTS_CFG.exists():
        for line in TESTS_CFG.read_text().splitlines():
            line = line.strip()
            if line and not line.startswith("#"):
                patterns.append(line)
    return patterns


def excluded(test_id, patterns):
    base = test_id.split("::")[0]
    for pattern in patterns:
        if "::" in pattern:
            if fnmatch.fnmatch(test_id, pattern):
                return True
        elif fnmatch.fnmatch(base, pattern):
            return True
    return False


def mode_flags(mode):
    return ["-O2"] if mode == "O2" else []


async def discover():
    patterns = load_excludes()
    ids = []
    for f in sorted(glob.glob(str(HERE / "**" / "*.cj"), recursive=True)):
        if f.endswith((".aot.cj", ".int.cj")):
            continue
        base = os.path.relpath(f, HERE)[:-len(".cj")]
        for mode in MODES:
            test_id = f"{SUITE}/{base}::{mode}"
            if not excluded(test_id, patterns):
                ids.append(test_id)
    return ids


async def build(test_id, workdir):
    base, mode = test_id.split("::")
    rel = base.split("/", 1)[1]
    name = Path(rel).name
    test_src = HERE / Path(rel).parent
    test_copy = Path(workdir).parent
    wdir = Path(workdir)
    log_io = io.StringIO()

    toolchain = os.environ["TOOLCHAIN"]
    compiler_jar = toolchain + "/tools/bin/cbc-compiler.jar"
    jc_options = os.environ.get("CBC_JC_OPTIONS", "")

    async with _copy_lock(f"{SUITE}/{base}"):
        if not test_copy.exists():
            shutil.copytree(test_src, test_copy, dirs_exist_ok=True)

        aot_sos = []
        for aot_file in sorted(glob.glob(str(test_copy / "*.aot.cj"))):
            aot_name = os.path.basename(aot_file)[:-len(".aot.cj")]
            aot_so = test_copy / f"lib{aot_name}.so"
            if not aot_so.exists():
                res = await run_cjc(aot_file, str(aot_so), output_type="dylib", log=log_io)
                log_io.write(f"\n[aot {aot_name}] exit {res}\n")
                if res != 0:
                    (wdir / "build.log").write_text(log_io.getvalue())
                    return 1
            aot_sos.append(aot_name)

    os.makedirs(wdir, exist_ok=True)

    import_args = ["--import-path", str(test_copy)] if aot_sos else []

    int_chir_files = []
    int_files = sorted(glob.glob(str(test_copy / "*.int.cj")))
    if int_files:
        temp_dir = wdir / "temp"
        os.makedirs(temp_dir, exist_ok=True)
        import_args += ["--import-path", str(temp_dir)]
        for int_file in int_files:
            int_base = os.path.splitext(os.path.basename(int_file))[0]
            int_base = os.path.splitext(int_base)[0]
            res = await run_cjc(int_file, str(temp_dir / int_base), output_type="staticlib",
                                 additional_args=["--save-temps", str(temp_dir)] + import_args + mode_flags(mode),
                                 log=log_io)
            log_io.write(f"\n[int {int_base}] exit {res}\n")
            if res != 0:
                (wdir / "build.log").write_text(log_io.getvalue())
                return 1
            int_chir_files.append(f"temp/{int_base}.chir")

    res = await run_cjc(str(test_copy / f"{name}.cj"), str(wdir / f"{name}.chir"),
                        additional_args=["--emit-chir"] + import_args + mode_flags(mode), log=log_io)
    log_io.write(f"\n[cjc {mode}] exit {res}\n")
    if res != 0:
        (wdir / "build.log").write_text(log_io.getvalue())
        return 1

    aot_deps = [f"-cbcaotdeps={':'.join(aot_sos)}"] if aot_sos else []
    java_cmd_list = [java_cmd(), "-jar", compiler_jar, f"-outputname={name}", f"{name}.chir"]
    if jc_options:
        java_cmd_list.append(jc_options)
    java_cmd_list += aot_deps + int_chir_files
    cbc_err = io.StringIO()
    res = await run_in_toolchain(java_cmd_list, cwd=str(wdir), log=log_io, stderr_log=cbc_err)
    log_io.write(f"\n[cbc {mode}] exit {res}\n")
    if cbc_err.getvalue().strip():
        log_io.write(cbc_err.getvalue())
    (wdir / "build.log").write_text(log_io.getvalue())
    return 0 if res == 0 else 1


async def run_command(test_id, workdir):
    base = test_id.split("::")[0]
    name = Path(base).name
    wdir = Path(workdir)
    return f'LD_LIBRARY_PATH="{wdir.parent}:$LD_LIBRARY_PATH" launcher "{wdir}/{name}.cbc"'


async def run(test_id, workdir):
    base = test_id.split("::")[0]
    name = Path(base).name
    wdir = Path(workdir)
    cbc = wdir / f"{name}.cbc"
    if not cbc.is_file():
        raise MissingArtifacts(f"{test_id}: {cbc} not found (run without --skip-build)")

    cmd = await run_command(test_id, workdir)
    out_io = io.StringIO()
    err_io = io.StringIO()
    res = await run_in_toolchain(["bash", "-c", cmd], cwd=str(wdir), log=out_io, stderr_log=err_io)
    stdout_txt = out_io.getvalue()
    err_txt = err_io.getvalue()

    (wdir / "run.log").write_text(stdout_txt)
    (wdir / "run.err").write_text(err_txt)
    (wdir / f"{name}.actual").write_text(stdout_txt + (err_txt if err_txt.strip() else f"{res}\n"))
    return verify(wdir, name)


def verify(wdir, name):
    expected = wdir.parent / f"{name}.expected"
    actual = wdir / f"{name}.actual"
    exp_txt = expected.read_text() if expected.is_file() else None
    act_txt = actual.read_text()
    if exp_txt is None or exp_txt != act_txt:
        diff_lines = difflib.unified_diff(exp_txt.splitlines(keepends=True) if exp_txt is not None else [],
                                          act_txt.splitlines(keepends=True),
                                          "expected", "actual")
        (wdir / "diff.log").write_text("".join(diff_lines))
        return 1
    return 0
