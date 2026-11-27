import io
import os
from pathlib import Path

from .cj import CjTestSuite, mode_flags
from runner_lib import run_in_toolchain

# std packages given to cbc-compiler as CHIR inputs (colon-separated, in
# dependency order). Only these are lowered into the test's cbc; everything
# else stays AOT. Extend via CBC_STD_PKGS.
DEFAULT_STD_PKGS = "std.core:std.env"
# Foreign libs resolving the @C symbols referenced by the CBC std packages:
# the Cangjie runtime itself. std.core @C externs implemented in the engine
# (e.g. the atexit list hooks) resolve through the engine image, which the
# engine adds to the foreign-libs search base itself.
DEFAULT_FOREIGN_LIBS = "cangjie-runtime"


class CjCbcStdTestSuite(CjTestSuite):
    """The cj pipeline with the std packages compiled to CBC instead of AOT.

    Sources and discovery are reused from standalone/cj; the entry point is
    compiled to CHIR exactly as in the cj suite, then lowered together with
    the std package CHIRs (<toolchain>/tools/lib/std-packages) into a single
    cbc, so std core code comes from CBC and not from the AOT std library
    (libcangjie-std-*.so). Test ids look like cjcbc::fibonacci::default.
    """

    SUITE = "cjcbc"

    def launcher_args(self) -> str:
        # CBC-only mode: the launcher loads no AOT std libraries; all std code
        # comes from the cbc images (the std packages lowered into the test's
        # cbc) and the engine builtins resolve from the loaded cbc images too.
        return "-c"

    async def _link(self, wdir: Path, name: str, mode: str, log_io: io.StringIO,
                    aot_names: list[str], native_names: list[str], int_chir_files: list[str]) -> int:
        # Lower the main CHIR together with the std package CHIRs (in
        # dependency order, std.core first) into one cbc.
        std_chir_dir = Path(os.environ["TOOLCHAIN"]) / "tools/lib/std-packages"
        pkgs = os.environ.get("CBC_STD_PKGS", DEFAULT_STD_PKGS).split(":")
        missing = [p for p in pkgs if not (std_chir_dir / f"{p}.chir").is_file()]
        if missing:
            log_io.write(f"Missing std CHIRs in {std_chir_dir}: {', '.join(missing)}\n")
            (wdir / "build.log").write_text(log_io.getvalue())
            return 1
        foreign_libs = os.environ.get("CBC_STD_FOREIGN_LIBS", DEFAULT_FOREIGN_LIBS)
        # Per-test native foreign libs are appended to the std ones.
        if native_names:
            foreign_libs += ":" + ":".join(native_names)
        aot_deps = [f"-cbcaotdeps={':'.join(aot_names)}"] if aot_names else []
        cbc_cmd = (["cbc-compiler", f"-outputname={name}", f"{name}.chir",
                    f"+IgnoreSourceFileNameForCbc {os.environ.get('CBC_JC_OPTIONS', '')}",
                    f"-foreignlibs={foreign_libs}"]
                   + aot_deps + [str(std_chir_dir / f"{p}.chir") for p in pkgs]
                   + int_chir_files + mode_flags(mode))
        log_io.write(" ".join(cbc_cmd) + "\n")
        res = await run_in_toolchain(cbc_cmd, cwd=str(wdir), log=log_io,
                                     stderr_log=log_io, timings=self.timings)
        log_io.write(f"\n[cbc {mode}] exit {res}\n")
        (wdir / "build.log").write_text(log_io.getvalue())
        return 0 if res == 0 else 1
