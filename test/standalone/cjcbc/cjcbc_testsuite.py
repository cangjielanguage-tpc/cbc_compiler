import glob
import io
import os
import shutil
import sys
from pathlib import Path

from runner_lib import (DYLIB_EXT, DYLIB_PATH_VAR, MissingArtifacts, TestIdentifier, TestSuite,
                        run_cjc, run_in_toolchain)

HERE = Path(__file__).resolve().parent
# Test sources are reused from the cj suite (standalone/cj).
SRC = HERE.parent / "cj"

# cj_testsuite.py lives one level up (standalone/cj); import it for the
# shared mode list and the golden-output verifier.
sys.path.insert(0, str(SRC))
from cj_testsuite import MODES, mode_flags, verify  # noqa: E402  (needs the path above)


def _std_chir_dir() -> Path:
    # Packages prebuilt into CBC form and provided by the toolchain: their
    # CHIRs live in <toolchain>/tools/lib/std-packages/*.chir, so cbc-compiler can
    # lower them together with the test sources and no AOT std library is
    # needed at run time. TOOLCHAIN is exported by runner.py's cmd_test.
    return Path(os.environ["TOOLCHAIN"]) / "tools" / "lib" / "std-packages"

# std packages given to cbc-compiler as CHIR inputs (colon-separated, in
# dependency order). Only these are lowered into the test's cbc; everything
# else stays AOT. Extend via CBC_STD_PKGS (colon-separated package names).
DEFAULT_STD_PKGS = "std.core:std.env"
# Runtime entry points are provided explicitly here. The compiler also records
# stdFFI for CBC std.core's native @C functions (printing, strings, math, etc.).
# That SDK library contains native helpers, not AOT std.core implementations.
DEFAULT_FOREIGN_LIBS = "cangjie-runtime"


def _import_flags(paths):
    # Flatten a list of Paths into repeated --import-path args for cjc.
    return [arg for path in paths for arg in ("--import-path", str(path))]


def _std_chir_files(pkgs: list[str]) -> list[str]:
    std_chir_dir = _std_chir_dir()
    return [str(std_chir_dir / f"{pkg}.chir") for pkg in pkgs]


class CjCbcStdTestSuite(TestSuite):
    """The cj pipeline with the std packages compiled to CBC instead of AOT.

    Sources are reused from standalone/cj: the entry point is compiled to CHIR
    exactly as in the cj suite, then lowered together with the std package
    CHIRs (tools/lib/std-packages) into a single cbc, so std core code comes from CBC
    and not from the AOT std library (libcangjie-std-*.so).

    Runs in the same two modes as cj: ids look like cjcbc::bugs::default.
    """

    SUITE = "cjcbc"

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
        wdir = workdir
        log_io = io.StringIO()
        jc_options = os.environ.get("CBC_JC_OPTIONS", "")

        # Each mode gets its own copy of the sources, so default and O2 build
        # and run fully concurrently without sharing any files.
        shutil.copytree(SRC / Path(rel).parent, wdir, dirs_exist_ok=True)

        # AOT packages -> native dylibs, later linked via -cbcaotdeps.
        aot_names = []
        for aot_file in sorted(wdir.glob("*.aot.cj")):
            aot_name = aot_file.name[:-len(".aot.cj")]
            aot_so = aot_file.with_name(f"lib{aot_name}{DYLIB_EXT}")
            res = await run_cjc(str(aot_file), str(aot_so), output_type="dylib", log=log_io,
                                timings=self.timings)
            log_io.write(f"\n[aot {aot_name}] exit {res}\n")
            if res != 0:
                return self._fail(log_io, wdir)
            aot_names.append(aot_name)

        # Foreign C -> native dylibs, later linked via -foreignlibs.
        native_names = []
        for c_file in sorted(wdir.glob("*.c")):
            native_name = c_file.stem
            native_so = c_file.with_name(f"lib{native_name}{DYLIB_EXT}")
            res = await run_in_toolchain(
                ["cc", "-shared", "-fPIC", "-o", str(native_so), str(c_file)],
                log=log_io, timings=self.timings)
            log_io.write(f"\n[native {native_name}] exit {res}\n")
            if res != 0:
                return self._fail(log_io, wdir)
            native_names.append(native_name)

        # cjc import search paths; widened as more packages are compiled in.
        import_paths = [wdir] if aot_names else []

        # Intermediate packages -> CHIR, handed to cbc-compiler as extra inputs.
        int_chir_files = []
        int_files = sorted(wdir.glob("*.int.cj"))
        if int_files:
            temp_dir = wdir / "temp"
            temp_dir.mkdir(exist_ok=True)
            import_paths.append(temp_dir)
            for int_file in int_files:
                pkg = int_file.name[:-len(".int.cj")]
                res = await run_cjc(str(int_file), str(temp_dir / pkg), output_type="staticlib",
                                    additional_args=["--save-temps", str(temp_dir)]
                                    + _import_flags(import_paths) + mode_flags(mode),
                                    log=log_io, timings=self.timings)
                log_io.write(f"\n[int {pkg}] exit {res}\n")
                if res != 0:
                    return self._fail(log_io, wdir)
                int_chir_files.append(f"temp/{pkg}.chir")

        # Compile the entry point to CHIR (same flags as the cj suite).
        res = await run_cjc(str(wdir / f"{name}.cj"), str(wdir / f"{name}.chir"),
                            additional_args=["--emit-chir"] + _import_flags(import_paths) + mode_flags(mode),
                            log=log_io, timings=self.timings)
        log_io.write(f"\n[cjc {mode}] exit {res}\n")
        if res != 0:
            return self._fail(log_io, wdir)

        # Lower main CHIR + std package CHIRs into one cbc. Packages are
        # listed in dependency order (std.core first).
        std_chir_dir = _std_chir_dir()
        pkgs = os.environ.get("CBC_STD_PKGS", DEFAULT_STD_PKGS).split(":")
        missing = [p for p in pkgs if not (std_chir_dir / f"{p}.chir").is_file()]
        if missing:
            log_io.write(f"Missing std CHIRs in {std_chir_dir}: {', '.join(missing)}\n")
            return self._fail(log_io, wdir)
        foreign_libs = os.environ.get("CBC_STD_FOREIGN_LIBS", DEFAULT_FOREIGN_LIBS)
        # Per-test native foreign libs are appended to the std core ones.
        if native_names:
            foreign_libs += ":" + ":".join(native_names)
        aot_deps = [f"-cbcaotdeps={':'.join(aot_names)}"] if aot_names else []
        cbc_cmd = (["cbc-compiler", f"-outputname={name}", f"{name}.chir",
                    f"+IgnoreSourceFileNameForCbc {jc_options}",
                    f"-foreignlibs={foreign_libs}"]
                   + aot_deps + _std_chir_files(pkgs) + int_chir_files + mode_flags(mode))
        log_io.write(" ".join(cbc_cmd) + "\n")
        res = await run_in_toolchain(cbc_cmd, cwd=str(wdir), log=log_io,
                                     stderr_log=log_io, timings=self.timings)
        log_io.write(f"\n[cbc {mode}] exit {res}\n")
        (wdir / "build.log").write_text(log_io.getvalue())
        return 0 if res == 0 else 1

    async def run(self, test_id: TestIdentifier, workdir: Path) -> int:
        name = Path(test_id.test_name).name
        wdir = workdir
        cbc = wdir / f"{name}.cbc"
        if not cbc.is_file():
            raise MissingArtifacts(f"{test_id}: {cbc} not found (run without --skip-build)")

        # The cbc may dlopen AOT dep dylibs (lib<name>.so) built next to it,
        # so the launcher must find them via the dynamic library path (same
        # as the cj suite).
        cmd = f'{DYLIB_PATH_VAR}="{wdir}:${DYLIB_PATH_VAR}" launcher "{wdir}/{name}.cbc"'
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

    def _fail(self, log_io: io.StringIO, wdir: Path) -> int:
        # Persist the build log for post-mortem and signal failure.
        (wdir / "build.log").write_text(log_io.getvalue())
        return 1
