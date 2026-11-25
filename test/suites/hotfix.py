import asyncio
import io
import os
import shutil
from pathlib import Path

from runner_lib import (
    DYLIB_EXT, DYLIB_PATH_VAR, MissingArtifacts, TestIdentifier,
    compare_output, run_cjc, run_in_toolchain,
)
from .cj import (CjTestSuite, _fail, _import_flags, build_aot_deps,
                 build_int_chirs, build_native_deps, mode_flags)

# Reuses the cj suite sources (standalone/cj); this module is part of suites/.
SRC = Path(__file__).resolve().parents[1] / "standalone" / "cj"


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


class HotfixTestSuite(CjTestSuite):
    SUITE = "hotfix"

    def __init__(self):
        super().__init__()
        self._copy_locks = {}

    def _copy_lock(self, key):
        return self._copy_locks.setdefault(key, asyncio.Lock())

    async def _chir_dis(self, chir, wdir, env, log_io):
        res = await run_in_toolchain(["chir-dis", str(chir)], cwd=str(wdir), env=env,
                                     log=log_io, stderr_log=log_io, timings=self.timings)
        if res != 0:
            log_io.write(f"CHIR disassembly failed ({chir}): {res}\n")
        return res

    async def build(self, test_id: TestIdentifier, workdir: Path) -> int:
        rel = test_id.test_name
        name = Path(rel).name
        mode = test_id.mode
        wdir = workdir.absolute()       # per-mode area
        test_copy = wdir / "sources"    # isolated sources and dependencies per mode
        log_io = io.StringIO()

        # Copy the source dir once, then compile its deps into the copy.
        async with self._copy_lock(str(test_id)):
            if not test_copy.exists():
                shutil.copytree(SRC / Path(rel).parent, test_copy, dirs_exist_ok=True)
            aot_names = await build_aot_deps(test_copy, log_io, self.timings)
            if aot_names is None:
                return _fail(log_io, wdir)
            native_names = await build_native_deps(test_copy, log_io, self.timings)
            if native_names is None:
                return _fail(log_io, wdir)

        wdir.mkdir(parents=True, exist_ok=True)

        # cjc import search paths; widened as more packages are compiled in.
        import_paths = [test_copy] if aot_names else []
        int_pair = await build_int_chirs(test_copy, wdir, mode, import_paths, log_io, self.timings)
        if int_pair is None:
            return _fail(log_io, wdir)
        int_chir_files, int_import_paths = int_pair
        import_paths += int_import_paths

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
        common_args = _import_flags(import_paths) + link_args + mode_flags(mode) + ["--link-options=--export-dynamic"]

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
                            cwd=str(wdir), env=stub_env, log=log_io, timings=self.timings)
        log_io.write(f"\n[cjc stub] exit {res}\n")
        if res != 0:
            return _fail(log_io, wdir)

        chir_files = sorted(wdir.glob("*.chir"))
        if len(chir_files) != 1:
            log_io.write(f"Expected one original CHIR file, found: {chir_files}\n")
            return _fail(log_io, wdir)
        chir = wdir / f"{name}.chir"
        os.replace(chir_files[0], chir)

        res = await self._chir_dis(chir, wdir, env, log_io)
        if res != 0:
            return _fail(log_io, wdir)

        # 2) Run the stub binary: every patchable function must throw.
        stub_log = io.StringIO()
        res = await run_in_toolchain([str(executable)], cwd=str(wdir), env=env,
                                     log=stub_log, stderr_log=stub_log,
                                     timings=self.timings, tag="stub-run")
        (wdir / f"{name}.stub.actual").write_text(stub_log.getvalue())
        if res != 1 or "Exception: should not reach here" not in stub_log.getvalue().splitlines():
            log_io.write(f"Expected throw-stub exception with exit status 1, got {res}:\n{stub_log.getvalue()}")
            return _fail(log_io, wdir)

        # 3) Patched build: full CHIR of the same source, plugin still attached.
        patched_chir = wdir / f"{name}_patched.chir"
        res = await run_cjc(str(test_copy / f"{name}.cj"), str(patched_chir),
                            additional_args=common_args + ["--emit-chir", "--plugin", str(plugin)],
                            cwd=str(wdir), env=env, log=log_io, timings=self.timings)
        log_io.write(f"\n[cjc patched] exit {res}\n")
        if res != 0:
            return _fail(log_io, wdir)
        res = await self._chir_dis(patched_chir, wdir, env, log_io)
        if res != 0:
            return _fail(log_io, wdir)

        # 4) patch-gen diffs original vs patched CHIR into the patch CHIR.
        patch_name = f"diff{name}_patched"
        patch_chir = wdir / f"{patch_name}.chir"
        res = await run_in_toolchain(["patch-gen", str(chir), str(patched_chir)],
                                     cwd=str(wdir), env=env, log=log_io, stderr_log=log_io,
                                     timings=self.timings)
        log_io.write(f"\n[patch-gen] exit {res}\n")
        if res != 0:
            return _fail(log_io, wdir)
        res = await self._chir_dis(patch_chir, wdir, env, log_io)
        if res != 0:
            return _fail(log_io, wdir)

        # 5) Compile the patch CHIR into the CBC binary consumed at run time.
        return await self._link(wdir, patch_name, mode, log_io, aot_names, native_names, int_chir_files)

    async def run(self, test_id: TestIdentifier, workdir: Path) -> int:
        name = Path(test_id.test_name).name
        wdir = workdir.absolute()
        cbc = wdir / f"diff{name}_patched.cbc"
        if not cbc.is_file():
            raise MissingArtifacts(f"{test_id}: {cbc} not found (run without --skip-build)")

        test_copy = wdir / "sources"
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
        res = await run_in_toolchain(["bash", "-c", cmd], cwd=str(wdir), log=out_io, stderr_log=err_io,
                                     timings=self.timings, tag="run")
        stdout_txt = out_io.getvalue()
        err_txt = err_io.getvalue()

        (wdir / "run.log").write_text(stdout_txt)
        (wdir / "run.err").write_text(err_txt)
        (wdir / f"{name}.actual").write_text(stdout_txt + (err_txt if err_txt.strip() else f"{res}\n"))
        return verify(wdir, name, test_id.mode)


def verify(wdir, name, mode):
    sources = wdir / "sources"
    candidates = [sources / f"{name}.hotfix.{mode}.expected",
                  sources / f"{name}.hotfix.expected",
                  sources / f"{name}.{mode}.expected",
                  sources / f"{name}.expected"]
    expected = next((p for p in candidates if p.is_file()), candidates[-1])
    return compare_output(expected, wdir / f"{name}.actual", wdir)
