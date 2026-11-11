import asyncio
import difflib
import glob
import io
import os
import shutil
from pathlib import Path

from runner_lib import DYLIB_EXT, DYLIB_PATH_VAR, MissingArtifacts, TestIdentifier, run_cjc, run_in_toolchain

SUITE = "hotfix"
HERE = Path(__file__).resolve().parent
SRC = HERE.parent / "standalone" / "cj"
_copy_locks = {}


def _copy_lock(key):
    return _copy_locks.setdefault(key, asyncio.Lock())


def _import_flags(paths):
    # Flatten a list of Paths into repeated --import-path args for cjc.
    return [arg for path in paths for arg in ("--import-path", str(path))]


def _fail(log_io, wdir):
    # Persist the build log for post-mortem and signal failure.
    (wdir / "build.log").write_text(log_io.getvalue())
    return 1


def _build_env(test_copy):
    # tool.sh re-prepends CANGJIE_STDX_PATH to the dylib search path, so only
    # the per-test dylib dir is added here. CBCOPT is a run-time option and must
    # not leak into the build; HOTFIX_GEN_THROW_STUBS is set per invocation.
    env = os.environ.copy()
    env.pop("HOTFIX_GEN_THROW_STUBS", None)
    env.pop("CBCOPT", None)
    env[DYLIB_PATH_VAR] = os.pathsep.join(filter(None, [
        str(test_copy), os.environ.get(DYLIB_PATH_VAR, "")]))
    return env


async def _chir_dis(chir, wdir, env, log_io):
    res = await run_in_toolchain(["chir-dis", str(chir)], cwd=str(wdir), env=env,
                                 log=log_io, stderr_log=log_io)
    if res != 0:
        log_io.write(f"CHIR disassembly failed ({chir}): {res}\n")
    return res


async def discover():
    ids = []
    for f in sorted(glob.glob(str(SRC / "**" / "*.cj"), recursive=True)):
        if f.endswith((".aot.cj", ".int.cj")):
            continue
        rel = os.path.relpath(f, SRC)[:-len(".cj")]
        ids.append(TestIdentifier(SUITE, rel, "default"))
    return ids


async def build(test_id, workdir):
    rel = test_id.test_name
    name = Path(rel).name
    wdir = Path(workdir).absolute()      # per-mode area
    test_copy = wdir.parent              # per-test area (source copy)
    log_io = io.StringIO()

    # Copy the source dir once, then compile its deps into the copy.
    async with _copy_lock(test_id.test_name):
        if not test_copy.exists():
            shutil.copytree(SRC / Path(rel).parent, test_copy, dirs_exist_ok=True)

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
                                 + _import_flags(import_paths),
                                 log=log_io)
            log_io.write(f"\n[int {pkg}] exit {res}\n")
            if res != 0:
                return _fail(log_io, wdir)
            int_chir_files.append(f"temp/{pkg}.chir")

    # Link inputs shared by both cjc invocations: dylibs, int archives
    # (extensionless staticlibs copied to .a so cjc accepts them) and
    # export-dynamic so the patcher can rebind symbols at run time.
    link_args = []
    for lib in aot_names + native_names:
        link_args += ["-L", str(test_copy), "-l", lib]
    for int_chir in int_chir_files:
        archive = wdir / int_chir[:-len(".chir")]
        shutil.copy2(archive, f"{archive}.a")
        link_args.append(f"{archive}.a")
    common_args = _import_flags(import_paths) + link_args + ["--link-options=--export-dynamic"]

    # cjc and the plugin write intermediate files into cwd; drop stale
    # top-level artifacts so the CHIR scan below sees exactly one file.
    for pattern in ("*.chir", "*.chirtxt", "*.cbc"):
        for path in wdir.glob(pattern):
            path.unlink()
    executable = wdir / name
    if executable.is_file():
        executable.unlink()

    plugin = Path(os.environ["TOOLCHAIN"]) / "tools" / "lib" / f"libhotfix-plugin{DYLIB_EXT}"
    env = _build_env(test_copy)

    # 1) Stub build: the plugin replaces patchable function bodies with throw
    #    stubs; --save-temps keeps the package-named original CHIR.
    stub_env = env.copy()
    stub_env["HOTFIX_GEN_THROW_STUBS"] = "true"
    res = await run_cjc(str(test_copy / f"{name}.cj"), str(executable),
                         additional_args=common_args
                         + ["--plugin", str(plugin), "--save-temps", str(wdir)],
                         cwd=str(wdir), env=stub_env, log=log_io)
    log_io.write(f"\n[cjc stub] exit {res}\n")
    if res != 0:
        return _fail(log_io, wdir)

    chir_files = sorted(wdir.glob("*.chir"))
    if len(chir_files) != 1:
        log_io.write(f"Expected one original CHIR file, found: {chir_files}\n")
        return _fail(log_io, wdir)
    chir = wdir / f"{name}.chir"
    os.replace(chir_files[0], chir)

    res = await _chir_dis(chir, wdir, env, log_io)
    if res != 0:
        return _fail(log_io, wdir)

    # 2) Run the stub binary: every patchable function must throw.
    stub_log = io.StringIO()
    res = await run_in_toolchain([str(executable)], cwd=str(wdir), env=env,
                          log=stub_log, stderr_log=stub_log)
    (wdir / f"{name}.stub.actual").write_text(stub_log.getvalue())
    if res != 1 or "Exception: should not reach here" not in stub_log.getvalue().splitlines():
        log_io.write(f"Expected throw-stub exception with exit status 1, got {res}:\n{stub_log.getvalue()}")
        return _fail(log_io, wdir)

    # 3) Patched build: full CHIR of the same source, plugin still attached.
    patched_chir = wdir / f"{name}_patched.chir"
    res = await run_cjc(str(test_copy / f"{name}.cj"), str(patched_chir),
                         additional_args=common_args + ["--emit-chir", "--plugin", str(plugin)],
                         cwd=str(wdir), env=env, log=log_io)
    log_io.write(f"\n[cjc patched] exit {res}\n")
    if res != 0:
        return _fail(log_io, wdir)
    res = await _chir_dis(patched_chir, wdir, env, log_io)
    if res != 0:
        return _fail(log_io, wdir)

    # 4) patch-gen diffs original vs patched CHIR into the patch CHIR.
    patch_name = f"diff{name}_patched"
    patch_chir = wdir / f"{patch_name}.chir"
    res = await run_in_toolchain(["patch-gen", str(chir), str(patched_chir)],
                          cwd=str(wdir), env=env, log=log_io, stderr_log=log_io)
    log_io.write(f"\n[patch-gen] exit {res}\n")
    if res != 0:
        return _fail(log_io, wdir)
    res = await _chir_dis(patch_chir, wdir, env, log_io)
    if res != 0:
        return _fail(log_io, wdir)

    # 5) Compile the patch CHIR into the CBC binary consumed at run time.
    aot_deps = [f"-cbcaotdeps={':'.join(aot_names)}"] if aot_names else []
    foreign_libs = [f"-foreignlibs={':'.join(native_names)}"] if native_names else []
    jc_options = os.environ.get("CBC_JC_OPTIONS", "")
    cbc_cmd = (["cbc-compiler", f"-outputname={patch_name}", f"{patch_name}.chir",
                f"+IgnoreSourceFileNameForCbc {jc_options}"]
               + aot_deps + foreign_libs + int_chir_files)
    cbc_err = io.StringIO()
    res = await run_in_toolchain(cbc_cmd, cwd=str(wdir), log=log_io, stderr_log=cbc_err)
    log_io.write(f"\n[cbc] exit {res}\n")
    if cbc_err.getvalue().strip():
        log_io.write(cbc_err.getvalue())
    (wdir / "build.log").write_text(log_io.getvalue())
    return 0 if res == 0 else 1


async def run(test_id, workdir):
    name = Path(test_id.test_name).name
    wdir = Path(workdir).absolute()
    cbc = wdir / f"diff{name}_patched.cbc"
    if not cbc.is_file():
        raise MissingArtifacts(f"{test_id}: {cbc} not found (run without --skip-build)")

    test_copy = wdir.parent
    preload = Path(os.environ["TOOLCHAIN"]) / "tools" / "lib" / f"libcbc-preload{DYLIB_EXT}"
    # The stub binary is run directly (no launcher): the preload library
    # restores in-process interpreter initialization for AOT binaries, and
    # CBCOPT points the CBC runtime at the directory holding the patch.
    cmd = (f'{DYLIB_PATH_VAR}="{test_copy}:${DYLIB_PATH_VAR}:{wdir}" '
           f'CBCOPT="cbc.path={wdir}" '
           f'LD_PRELOAD="{preload}" '
           f'"{wdir}/{name}"')
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
    hotfix_expected = wdir.parent / f"{name}.hotfix.expected"
    expected = hotfix_expected if hotfix_expected.is_file() else wdir.parent / f"{name}.expected"
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
