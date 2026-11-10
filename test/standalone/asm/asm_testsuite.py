import difflib
import fnmatch
import glob
import io
import os
import shutil
from pathlib import Path

from runner_tools import MissingArtifacts, java_cmd, run_cjc, run_in_toolchain

SUITE = "asm"
HERE = Path(__file__).resolve().parent
TESTS_CFG = HERE / "tests.cfg"


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


async def discover():
    patterns = load_excludes()
    ids = []
    for f in sorted(glob.glob(str(HERE / "**" / "*.asm"), recursive=True)):
        base = os.path.relpath(f, HERE)[:-len(".asm")]
        test_id = f"{SUITE}/{base}"
        if not excluded(test_id, patterns):
            ids.append(test_id)
    return ids


async def build(test_id, workdir):
    base = test_id.split("::")[0]
    rel = base.split("/", 1)[1]
    name = Path(rel).name
    src_dir = HERE / Path(rel).parent
    wdir = Path(workdir)
    log_path = wdir / "build.log"
    shutil.copytree(src_dir, wdir, dirs_exist_ok=True)

    toolchain = os.environ["TOOLCHAIN"]
    asm_jar = toolchain + "/tools/bin/cbc-asm.jar"

    log_io = io.StringIO()
    aot_fail = False
    for aot_file in sorted(glob.glob(str(wdir / "*.aot.cj"))):
        aot_name = os.path.basename(aot_file)[:-len(".aot.cj")]
        res = await run_cjc(aot_file, str(wdir / f"lib{aot_name}.so"), output_type="dylib", log=log_io)
        log_io.write(f"\n[aot {aot_name}] exit {res}\n")
        if res != 0:
            aot_fail = True
            break

    res = 1
    if not aot_fail:
        asm_err = io.StringIO()
        res = await run_in_toolchain([*java_cmd(None), "-jar", asm_jar, str(wdir / f"{name}.asm")],
                                     cwd=str(wdir), log=log_io, stderr_log=asm_err)
        log_io.write(f"\n[asm] exit {res}\n")
        if asm_err.getvalue().strip():
            log_io.write(asm_err.getvalue())

    log_path.write_text(log_io.getvalue())
    return 0 if res == 0 else 1


async def run_command(test_id, workdir):
    base = test_id.split("::")[0]
    name = Path(base).name
    wdir = Path(workdir)
    return f'LD_LIBRARY_PATH="{wdir}:$LD_LIBRARY_PATH" launcher "{wdir}/{name}.cbc"'


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
    (wdir / f"{name}.actual").write_text(stdout_txt + f"{res}\n")
    return verify(test_id, wdir)


def verify(test_id, wdir):
    name = Path(test_id.split("::")[0]).name
    expected = wdir / f"{name}.expected"
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
