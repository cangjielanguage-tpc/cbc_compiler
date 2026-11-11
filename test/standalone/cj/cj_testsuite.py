import asyncio
import difflib
import glob
import io
import os
import shutil
from pathlib import Path

from runner_lib import DYLIB_EXT, DYLIB_PATH_VAR, MissingArtifacts, TestIdentifier, run_cjc, run_in_toolchain

SUITE = "cj"
HERE = Path(__file__).resolve().parent
MODES = ("default", "O2")
_copy_locks = {}


def _copy_lock(key):
    return _copy_locks.setdefault(key, asyncio.Lock())


def mode_flags(mode):
    return ["-O2"] if mode == "O2" else []


def _import_flags(paths):
    # Flatten a list of Paths into repeated --import-path args for cjc.
    return [arg for path in paths for arg in ("--import-path", str(path))]


def _fail(log_io, wdir):
    # Persist the build log for post-mortem and signal failure.
    (wdir / "build.log").write_text(log_io.getvalue())
    return 1


async def discover():
    ids = []
    for f in sorted(glob.glob(str(HERE / "**" / "*.cj"), recursive=True)):
        if f.endswith((".aot.cj", ".int.cj")):
            continue
        rel = os.path.relpath(f, HERE)[:-len(".cj")]
        for mode in MODES:
            ids.append(TestIdentifier(SUITE, rel, mode))
    return ids


async def build(test_id, workdir):
    rel = test_id.test_name
    name = Path(rel).name
    mode = test_id.mode
    test_src = HERE / Path(rel).parent
    test_copy = Path(workdir).parent  # per-test area, shared by default + O2
    wdir = Path(workdir)              # per-mode area
    log_io = io.StringIO()
    jc_options = os.environ.get("CBC_JC_OPTIONS", "")

    # Copy the source dir once (shared across modes), then compile its deps.
    async with _copy_lock(test_id.test_name):
        if not test_copy.exists():
            shutil.copytree(test_src, test_copy, dirs_exist_ok=True)

        # AOT packages -> native dylibs, later linked via -cbcaotdeps.
        aot_names = []
        for aot_file in sorted(test_copy.glob("*.aot.cj")):
            aot_name = aot_file.name[:-len(".aot.cj")]
            aot_so = aot_file.with_name(f"lib{aot_name}{DYLIB_EXT}")
            res = await run_cjc(str(aot_file), str(aot_so), output_type="dylib", log=log_io)
            log_io.write(f"\n[aot {aot_name}] exit {res}\n")
            if res != 0:
                return _fail(log_io, wdir)
            aot_names.append(aot_name)

        # Foreign C -> native dylibs, later linked via -foreignlibs.
        native_names = []
        for c_file in sorted(test_copy.glob("*.c")):
            native_name = c_file.stem
            native_so = c_file.with_name(f"lib{native_name}{DYLIB_EXT}")
            res = await run_in_toolchain(
                ["cc", "-shared", "-fPIC", "-o", str(native_so), str(c_file)], log=log_io)
            log_io.write(f"\n[native {native_name}] exit {res}\n")
            if res != 0:
                return _fail(log_io, wdir)
            native_names.append(native_name)

    wdir.mkdir(parents=True, exist_ok=True)

    # cjc import search paths; widened as more packages are compiled in.
    import_paths = [test_copy] if aot_names else []

    # Intermediate packages -> CHIR, handed to cbc-compiler as extra inputs.
    int_chir_files = []
    int_files = sorted(test_copy.glob("*.int.cj"))
    if int_files:
        temp_dir = wdir / "temp"
        temp_dir.mkdir(exist_ok=True)
        import_paths.append(temp_dir)
        for int_file in int_files:
            pkg = int_file.name[:-len(".int.cj")]
            res = await run_cjc(str(int_file), str(temp_dir / pkg), output_type="staticlib",
                                 additional_args=["--save-temps", str(temp_dir)]
                                 + _import_flags(import_paths) + mode_flags(mode),
                                 log=log_io)
            log_io.write(f"\n[int {pkg}] exit {res}\n")
            if res != 0:
                return _fail(log_io, wdir)
            int_chir_files.append(f"temp/{pkg}.chir")

    # Compile the entry point to CHIR.
    res = await run_cjc(str(test_copy / f"{name}.cj"), str(wdir / f"{name}.chir"),
                        additional_args=["--emit-chir"] + _import_flags(import_paths) + mode_flags(mode),
                        log=log_io)
    log_io.write(f"\n[cjc {mode}] exit {res}\n")
    if res != 0:
        return _fail(log_io, wdir)

    # Link CHIR (+ AOT/foreign deps + intermediate CHIRs) into the final CBC.
    aot_deps = [f"-cbcaotdeps={':'.join(aot_names)}"] if aot_names else []
    foreign_libs = [f"-foreignlibs={':'.join(native_names)}"] if native_names else []
    cbc_cmd = (["cbc-compiler", f"-outputname={name}", f"{name}.chir",
                f"+IgnoreSourceFileNameForCbc {jc_options}"]
               + aot_deps + foreign_libs + int_chir_files)
    cbc_err = io.StringIO()
    res = await run_in_toolchain(cbc_cmd, cwd=str(wdir), log=log_io, stderr_log=cbc_err)
    log_io.write(f"\n[cbc {mode}] exit {res}\n")
    if cbc_err.getvalue().strip():
        log_io.write(cbc_err.getvalue())
    (wdir / "build.log").write_text(log_io.getvalue())
    return 0 if res == 0 else 1


async def run(test_id, workdir):
    name = Path(test_id.test_name).name
    wdir = Path(workdir)
    cbc = wdir / f"{name}.cbc"
    if not cbc.is_file():
        raise MissingArtifacts(f"{test_id}: {cbc} not found (run without --skip-build)")

    cmd = f'{DYLIB_PATH_VAR}="{wdir.parent}:${DYLIB_PATH_VAR}" launcher "{wdir}/{name}.cbc"'
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
