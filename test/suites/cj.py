import glob
import io
import os
import shutil
from pathlib import Path

from runner_lib import (
    DYLIB_EXT, DYLIB_PATH_VAR, MissingArtifacts, TestIdentifier, TestSuite,
    compare_output, run_cc, run_cjc, run_in_toolchain,
)

# Test sources live in standalone/cj (reused by the cjcbc suite as well);
# this module is part of the suites/ package.
SRC = Path(__file__).resolve().parents[1] / "standalone" / "cj"
MODES = ("default", "O2")


def mode_flags(mode):
    return ["-O2"] if mode == "O2" else []


def _import_flags(paths):
    # Flatten a list of Paths into repeated --import-path args for cjc.
    return [arg for path in paths for arg in ("--import-path", str(path))]


def _fail(log_io, wdir):
    # Persist the build log for post-mortem and signal failure.
    (wdir / "build.log").write_text(log_io.getvalue())
    return 1


async def build_aot_deps(src_dir: Path, log_io: io.StringIO, timings: dict[str, float]) -> list[str] | None:
    # AOT packages -> native dylibs next to the sources (later linked via
    # -cbcaotdeps). Returns the lib names, None on failure (logged).
    names = []
    for aot_file in sorted(src_dir.glob("*.aot.cj")):
        aot_name = aot_file.name[:-len(".aot.cj")]
        aot_so = aot_file.with_name(f"lib{aot_name}{DYLIB_EXT}")
        res = await run_cjc(str(aot_file), str(aot_so), output_type="dylib", log=log_io,
                            timings=timings)
        log_io.write(f"\n[aot {aot_name}] exit {res}\n")
        if res != 0:
            return None
        names.append(aot_name)
    return names


async def build_native_deps(src_dir: Path, log_io: io.StringIO, timings: dict[str, float]) -> list[str] | None:
    # Foreign C -> native dylibs next to the sources (later linked via
    # -foreignlibs). Returns the lib names, None on failure (logged).
    names = []
    for c_file in sorted(src_dir.glob("*.c")):
        native_name = c_file.stem
        native_so = c_file.with_name(f"lib{native_name}{DYLIB_EXT}")
        res = await run_cc(c_file, native_so, log=log_io, timings=timings)
        log_io.write(f"\n[native {native_name}] exit {res}\n")
        if res != 0:
            return None
        names.append(native_name)
    return names


async def build_int_chirs(src_dir: Path, wdir: Path, mode: str, import_paths: list[Path],
                          log_io: io.StringIO,
                          timings: dict[str, float]) -> tuple[list[str], list[Path]] | None:
    # Intermediate packages -> staticlibs with --save-temps into <wdir>/temp;
    # their CHIRs are handed to cbc-compiler as extra inputs, and temp becomes
    # an import search path for the later cjc invocations. The packages are
    # compiled against the given search paths plus temp itself (an int
    # package may import AOT packages or earlier-compiled int packages).
    # Returns (chir files, added import paths), None on failure (logged).
    int_files = sorted(src_dir.glob("*.int.cj"))
    if not int_files:
        return [], []
    temp_dir = wdir / "temp"
    temp_dir.mkdir(exist_ok=True)
    search_paths = [*import_paths, temp_dir]
    chir_files = []
    for int_file in int_files:
        pkg = int_file.name[:-len(".int.cj")]
        res = await run_cjc(str(int_file), str(temp_dir / pkg), output_type="staticlib",
                            additional_args=["--save-temps", str(temp_dir)]
                            + _import_flags(search_paths) + mode_flags(mode),
                            log=log_io, timings=timings)
        log_io.write(f"\n[int {pkg}] exit {res}\n")
        if res != 0:
            return None
        chir_files.append(f"temp/{pkg}.chir")
    return chir_files, [temp_dir]


class CjTestSuite(TestSuite):
    SUITE = "cj"

    async def discover(self) -> list[TestIdentifier]:
        ids = []
        for f in sorted(glob.glob(str(SRC / "**" / "*.cj"), recursive=True)):
            if f.endswith((".aot.cj", ".int.cj")):
                continue
            rel = os.path.relpath(f, SRC)[:-len(".cj")]
            for mode in MODES:
                ids.append(TestIdentifier(self.SUITE, rel, mode))
        return ids

    async def build(self, test_id: TestIdentifier, workdir: Path) -> int:
        rel = test_id.test_name
        name = Path(rel).name
        mode = test_id.mode
        wdir = workdir                  # per-mode area: source copy + artifacts
        log_io = io.StringIO()

        # Each mode gets its own copy of the sources, so default and O2 build
        # and run fully concurrently without sharing any files.
        shutil.copytree(SRC / Path(rel).parent, wdir, dirs_exist_ok=True)

        aot_names = await build_aot_deps(wdir, log_io, self.timings)
        if aot_names is None:
            return _fail(log_io, wdir)

        native_names = await build_native_deps(wdir, log_io, self.timings)
        if native_names is None:
            return _fail(log_io, wdir)

        # cjc import search paths; widened as more packages are compiled in.
        import_paths = [wdir] if aot_names else []
        int_pair = await build_int_chirs(wdir, wdir, mode, import_paths, log_io, self.timings)
        if int_pair is None:
            return _fail(log_io, wdir)
        int_chir_files, int_import_paths = int_pair
        import_paths += int_import_paths

        # Compile the entry point to CHIR.
        res = await run_cjc(str(wdir / f"{name}.cj"), str(wdir / f"{name}.chir"),
                            additional_args=["--emit-chir"] + _import_flags(import_paths) + mode_flags(mode),
                            log=log_io, timings=self.timings)
        log_io.write(f"\n[cjc {mode}] exit {res}\n")
        if res != 0:
            return _fail(log_io, wdir)

        # Link CHIR (+ AOT/foreign deps + intermediate CHIRs) into the final CBC.
        return await self._link(wdir, name, mode, log_io, aot_names, native_names, int_chir_files)

    async def _link(self, wdir: Path, name: str, mode: str, log_io: io.StringIO,
                    aot_names: list[str], native_names: list[str], int_chir_files: list[str]) -> int:
        # Suites reusing the cj pipeline override this to change the lowering
        # inputs; the tail persists build.log and reports the result.
        aot_deps = [f"-cbcaotdeps={':'.join(aot_names)}"] if aot_names else []
        foreign_libs = [f"-foreignlibs={':'.join(native_names)}"] if native_names else []
        jc_options = os.environ.get("CBC_JC_OPTIONS", "")
        cbc_cmd = (["cbc-compiler", f"-outputname={name}", f"{name}.chir",
                    f"+IgnoreSourceFileNameForCbc {jc_options}"]
                   + aot_deps + foreign_libs + int_chir_files)
        cbc_err = io.StringIO()
        res = await run_in_toolchain(cbc_cmd, cwd=str(wdir), log=log_io, stderr_log=cbc_err,
                                     timings=self.timings)
        log_io.write(f"\n[cbc {mode}] exit {res}\n")
        if cbc_err.getvalue().strip():
            log_io.write(cbc_err.getvalue())
        (wdir / "build.log").write_text(log_io.getvalue())
        return 0 if res == 0 else 1

    def launcher_cmd(self, wdir: Path, name: str) -> str:
        """The shell command that runs the compiled cbc."""
        return f'{DYLIB_PATH_VAR}="{wdir}:${DYLIB_PATH_VAR}" launcher "{wdir}/{name}.cbc"'

    async def run(self, test_id: TestIdentifier, workdir: Path) -> int:
        name = Path(test_id.test_name).name
        wdir = workdir
        cbc = wdir / f"{name}.cbc"
        if not cbc.is_file():
            raise MissingArtifacts(f"{test_id}: {cbc} not found (run without --skip-build)")

        cmd = self.launcher_cmd(wdir, name)
        out_io = io.StringIO()
        err_io = io.StringIO()
        res = await run_in_toolchain(["bash", "-c", cmd], cwd=str(wdir), log=out_io, stderr_log=err_io,
                                     timings=self.timings, tag="launcher")
        stdout_txt = out_io.getvalue()
        err_txt = err_io.getvalue()

        (wdir / "run.log").write_text(stdout_txt)
        (wdir / "run.err").write_text(err_txt)
        (wdir / f"{name}.actual").write_text(stdout_txt + (err_txt if err_txt.strip() else f"{res}\n"))
        return verify(wdir, name, test_id.mode)


def verify(wdir, name, mode):
    mode_expected = wdir / f"{name}.{mode}.expected"
    expected = mode_expected if mode_expected.is_file() else wdir / f"{name}.expected"
    return compare_output(expected, wdir / f"{name}.actual", wdir)
