import difflib
import glob
import io
import os
import platform
import shlex
import shutil
from pathlib import Path

from runner_lib import DYLIB_EXT, DYLIB_PATH_VAR, MissingArtifacts, TestIdentifier, TestSuite, run_cc, run_cjc, run_in_toolchain

HERE = Path(__file__).resolve().parent


def runtime_lib_dir() -> Path:
    """The toolchain runtime library directory of the host architecture
    (the one that holds cbc-intrinsics.cbc and cbcengine-helper.chir)."""
    machine = platform.machine().lower()
    arch = "aarch64" if machine in ("arm64", "aarch64") else "x86_64"
    toolchain = os.environ.get("TOOLCHAIN") or os.environ.get("CANGJIE_HOME")
    if not toolchain:
        raise MissingArtifacts("Neither TOOLCHAIN nor CANGJIE_HOME is set")
    dirs = sorted(Path(toolchain).glob(f"runtime/lib/*_{arch}_cjnative"))
    if len(dirs) != 1:
        raise MissingArtifacts(
            f"Expected exactly one runtime/lib/*_{arch}_cjnative directory in the toolchain, found: {dirs}"
        )
    return dirs[0]


async def prepare_helper_cbc(workdir: Path, timings: dict[str, float]) -> Path:
    """Builds cbcengine-helper.cbc in a dedicated tmp directory under the
    work directory and returns the directory to pass to the launcher via
    --cbc-path.

    Hand-written asm images embed no cangjie.runtime.cbcengine.helper package
    with real thrower bodies, which the cbc-intrinsics throwers delegate to;
    without it engine init fails. The launcher gets --cbc-path pointing at the
    tmp directory, so the package gets loaded before cbc-intrinsics.cbc (whose
    own copy of the package is declaration-only). Built unconditionally to
    stay in sync with the toolchain helper chir.
    """
    helper_dir = workdir / "tmp"
    if helper_dir.exists():
        shutil.rmtree(helper_dir)
    helper_dir.mkdir(parents=True)
    chir = runtime_lib_dir() / "cbcengine-helper.chir"
    if not chir.is_file():
        raise MissingArtifacts(f"cbcengine-helper.chir not found in the toolchain: {chir}")
    shutil.copy(chir, helper_dir / chir.name)

    log_io = io.StringIO()
    res = await run_in_toolchain(
        ["cbc-compiler", "-outputname=cbcengine-helper", "+GenLibrary", chir.name, "+IgnoreSourceFileNameForCbc"],
        cwd=str(helper_dir), log=log_io, stderr_log=log_io, timings=timings
    )
    if res != 0 or not (helper_dir / "cbcengine-helper.cbc").is_file():
        raise MissingArtifacts(f"Failed to build cbcengine-helper.cbc from {chir}:\n{log_io.getvalue()}")
    return helper_dir


class AsmTestSuite(TestSuite):
    SUITE = "asm"

    def __init__(self):
        super().__init__()
        self.helper_dir: Path | None = None

    async def setup(self, workdir: Path) -> None:
        self.helper_dir = await prepare_helper_cbc(workdir, self.timings)

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
        deps_failed = False
        for aot_file in sorted(glob.glob(str(wdir / "*.aot.cj"))):
            aot_name = os.path.basename(aot_file)[:-len(".aot.cj")]
            res = await run_cjc(aot_file, str(wdir / f"lib{aot_name}{DYLIB_EXT}"), output_type="dylib",
                                log=log_io, timings=self.timings)
            log_io.write(f"\n[aot {aot_name}] exit {res}\n")
            if res != 0:
                deps_failed = True
                break

        if not deps_failed:
            for c_file in sorted(wdir.glob("*.c")):
                native_so = c_file.with_name(f"lib{c_file.stem}{DYLIB_EXT}")
                res = await run_cc(c_file, native_so, log=log_io, timings=self.timings)
                log_io.write(f"\n[native {c_file.stem}] exit {res}\n")
                if res != 0:
                    deps_failed = True
                    break

        res = 1
        if not deps_failed:
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

        if self.helper_dir is None:
            raise MissingArtifacts(f"{test_id}: helper.cbc was not prepared (suite setup did not run)")
        cmd = f'{DYLIB_PATH_VAR}="{wdir}:${DYLIB_PATH_VAR}" launcher --cbc-path {shlex.quote(str(self.helper_dir))} "{wdir}/{name}.cbc"'
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
