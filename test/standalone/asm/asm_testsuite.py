import difflib
import glob
import io
import os
import shutil
from pathlib import Path

from runner_lib import DYLIB_EXT, DYLIB_PATH_VAR, MissingArtifacts, TestIdentifier, TestSuite, run_cjc, run_in_toolchain

HERE = Path(__file__).resolve().parent


class AsmTestSuite(TestSuite):
    SUITE = "asm"

    async def discover(self) -> list[TestIdentifier]:
        ids = []
        for f in sorted(glob.glob(str(HERE / "**" / "*.asm"), recursive=True)):
            rel = os.path.relpath(f, HERE)[:-len(".asm")]
            ids.append(TestIdentifier(self.SUITE, rel, ""))
        return ids

    async def build(self, test_id: TestIdentifier, workdir: Path) -> int:
        rel = test_id.test_name
        name = Path(rel).name
        src_dir = HERE / Path(rel).parent
        wdir = workdir
        log_path = wdir / "build.log"
        shutil.copytree(src_dir, wdir, dirs_exist_ok=True)

        log_io = io.StringIO()
        aot_fail = False
        for aot_file in sorted(glob.glob(str(wdir / "*.aot.cj"))):
            aot_name = os.path.basename(aot_file)[:-len(".aot.cj")]
            res = await run_cjc(aot_file, str(wdir / f"lib{aot_name}{DYLIB_EXT}"), output_type="dylib",
                                log=log_io, timings=self.timings)
            log_io.write(f"\n[aot {aot_name}] exit {res}\n")
            if res != 0:
                aot_fail = True
                break

        res = 1
        if not aot_fail:
            asm_err = io.StringIO()
            res = await run_in_toolchain(["cbc-asm", str(wdir / f"{name}.asm")],
                                         cwd=str(wdir), log=log_io, stderr_log=asm_err,
                                         timings=self.timings)
            log_io.write(f"\n[asm] exit {res}\n")
            if asm_err.getvalue().strip():
                log_io.write(asm_err.getvalue())

        log_path.write_text(log_io.getvalue())
        return 0 if res == 0 else 1

    async def run(self, test_id: TestIdentifier, workdir: Path) -> int:
        name = Path(test_id.test_name).name
        wdir = workdir
        cbc = wdir / f"{name}.cbc"
        if not cbc.is_file():
            raise MissingArtifacts(f"{test_id}: {cbc} not found (run without --skip-build)")

        cmd = f'{DYLIB_PATH_VAR}="{wdir}:${DYLIB_PATH_VAR}" launcher "{wdir}/{name}.cbc"'
        out_io = io.StringIO()
        err_io = io.StringIO()
        res = await run_in_toolchain(["bash", "-c", cmd], cwd=str(wdir), log=out_io, stderr_log=err_io,
                                     timings=self.timings, tag="launcher")
        stdout_txt = out_io.getvalue()
        err_txt = err_io.getvalue()

        (wdir / "run.log").write_text(stdout_txt)
        (wdir / "run.err").write_text(err_txt)
        (wdir / f"{name}.actual").write_text(stdout_txt + f"{res}\n")
        return verify(test_id, wdir)


def verify(test_id, wdir):
    name = Path(test_id.test_name).name
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
