import argparse
import asyncio
import asyncio.subprocess
import glob
import io
import multiprocessing
import shutil
import sys
import json
import platform
import os
from os.path import dirname, realpath, isfile
from typing import Callable, Awaitable, AnyStr
from runner_tools import *


def parse_tests_list(config_file: str, root_dir: str) -> dict[str, list[str]]:
    """
    Parses a file with 'include' and 'exclude' glob patterns and returns a dict mapping matched files
    to a list of explicitly excluded modes.

    If 'exclude' has no mode specified, the test is removed completely.
    """
    include_patterns = []
    exclude_patterns = []

    with open(config_file, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue

            if line.startswith('include '):
                suffix = line.replace('include ', '', 1).strip()
                include_patterns.append(suffix)
            elif line.startswith('exclude '):
                suffix = line.replace('exclude ', '', 1).strip()
                parts = suffix.split(maxsplit=1)
                pattern = parts[0]
                mode = parts[1] if len(parts) > 1 else None
                exclude_patterns.append((pattern, mode))

    matched_files = set()
    for pattern in include_patterns:
        full_pattern = os.path.join(root_dir, pattern)
        files = glob.glob(full_pattern, recursive=True)
        matched_files.update(os.path.relpath(f, root_dir) for f in files if os.path.isfile(f))

    excluded_modes = {}
    for pattern, mode in exclude_patterns:
        full_pattern = os.path.join(root_dir, pattern)
        files = glob.glob(full_pattern, recursive=True)
        for f in files:
            rel_f = os.path.relpath(f, root_dir)
            if mode is None:
                matched_files.discard(rel_f)
            else:
                if rel_f in matched_files:
                    if rel_f not in excluded_modes:
                        excluded_modes[rel_f] = set()
                    excluded_modes[rel_f].add(mode)

    tests = {}
    for f in sorted(list(matched_files)):
        if f.endswith(".aot.cj"):
            continue
        if f.endswith(".int.cj"):
            continue
        if f.endswith(".cj"):
            test_name = f[:-len(".cj")]
        elif f.endswith(".asm"):
            test_name = f[:-len(".asm")]
        else:
            raise ValueError(f"Incorrect test extension: {f}")

        tests[test_name] = list(excluded_modes.get(f, set()))

    return tests


class TestSuite:
    architecture = platform.machine()
    cpu_count = multiprocessing.cpu_count()
    root_dir = dirname(os.path.abspath(__file__))
    _stderr_lock = asyncio.Lock()

    def __init__(self, toolchain_path: str = ""):
        self.toolchain_path = toolchain_path

    async def print_stderr(self, msg: str):
        async with self._stderr_lock:
            print(msg, file=sys.stderr)

    async def print_status(self, msg: str):
        async with self._stderr_lock:
            print(msg, file=sys.stderr)

    def parallelism(self, args) -> int:
        if args.parallelism is not None:
            return max(1, int(args.parallelism))
        return max(1, self.cpu_count // 4)

    async def build_parallel(self, tests: list[str], build: Callable[[str], Awaitable[int]], args, parallelism="auto"):
        if parallelism == "auto":
            parallelism = self.parallelism(args)
        semaphore = asyncio.Semaphore(parallelism)

        async def build_bounded(test: str):
            async with semaphore:
                return test, await build(test)

        tasks = [asyncio.create_task(build_bounded(test)) for test in tests]
        results = await asyncio.gather(*tasks)
        succeeded = [test_name for test_name, err_code in results if err_code == 0]
        return succeeded

    def expected_file(self, test_name: str):
        return dotexpected(test_name)

    async def check_result(self, test_name: str, custom_actual: str = None):
        expected = self.expected_file(test_name)
        actual = custom_actual if custom_actual is not None else dotactual(test_name)
        diff_err = io.StringIO()
        t0 = time.monotonic()
        res = await run_async(" ".join(diff(actual, expected)), shell=True, stderr_log=diff_err)
        await Timings.add('diff', time.monotonic() - t0, timing_chunk)
        return res

    def cjc_env(self):
        return os.environ.copy()

    async def run_cjc(self, file: str, output_file: str, output_type: str = None,
                      additional_args: list[str] = [], use_tool_sh: bool = True, cwd=None, log=None, env=None):
        assert '.' in file
        cjc_args = ["--fobf-layout", "--fno-obf-export-symbols", "--fobf-source-path",
                    "--obf-sym-output-mapping", dotobfdotmap(output_file)]
        cjc_args += additional_args

        if output_type is not None:
            cjc_args += ["--output-type", output_type]

        _, file_extension = file.rsplit('.', 1)
        assert file_extension == "cj"

        cjc_args += ["-o", output_file]

        cjc_cmd_list = ['cjc', '--int-overflow', 'wrapping', '-Woff', 'unused', file, *cjc_args]

        env = self.cjc_env() if env is None else env.copy()
        return await run_in_env(use_tool_sh, env, cjc_cmd_list, cwd=cwd, log=log)

    def clean(self):
        print("Cleaning up...")
        generated_patterns = ["*.out", "*.cbc", "*.bchir", "*.bchir2", "*.cjo", "*.pdba", "*.chir.fb",
                              "*.actual", "*.dasm", "*.a", "*.so"]
        generated_dirs = ["default/", "O2/", "jetpdb/", "hotfix-default/"]
        for pattern in generated_patterns:
            for f in glob.glob(os.path.join(self.root_dir, "**", pattern), recursive=True):
                if os.path.isfile(f):
                    os.remove(f)
                elif os.path.isdir(f):
                    shutil.rmtree(f)
        for dir_pattern in generated_dirs:
            for f in glob.glob(os.path.join(self.root_dir, "**", dir_pattern), recursive=True):
                if os.path.isdir(f):
                    shutil.rmtree(f)


class StandaloneTestSuite(TestSuite):
    suite_mode = "standalone"
    compilation_modes = [("default", []), ("O2", ["-O2"])]
    standalone_tests = parse_tests_list(os.path.join(TestSuite.root_dir, "tests-list-standalone.cfg"), root_dir=TestSuite.root_dir)

    def __init__(self, toolchain_path: str):
        super().__init__(toolchain_path)
        self.asm_jar = self.toolchain_path + "/tools/bin/cbc-asm.jar"
        self.asm_jar_cds = dotjsa(self.asm_jar)
        self.compiler_jar = self.toolchain_path + "/tools/bin/cbc-compiler.jar"
        self.compiler_jar_cds = dotjsa(self.compiler_jar)
        self.compilation_failures = []
        self.built_modes = set()
        # Reset cds cache if it exists
        if os.path.isfile(self.asm_jar_cds):
            os.remove(self.asm_jar_cds)
        if os.path.isfile(self.compiler_jar_cds):
            os.remove(self.compiler_jar_cds)

    def mode_work_dir(self, test_work_dir, mode, name):
        return f"{test_work_dir}/{mode}"

    async def prepare_tools(self, args):
        return 0

    async def compile_cj(self, test_name, mode_work_dir, name, import_args, opt_flags, aot_names, native_names, log):
        res = await self.run_cjc(dotcj(test_name),
                                 output_file=dotchir(f"{mode_work_dir}/{name}"),
                                 additional_args=["--emit-chir"] + import_args + opt_flags,
                                 log=log)
        return res

    def cbc_name(self, name):
        return name

    def cj_run_command(self, mode_work_dir, name, env):
        return ["launcher", f"{mode_work_dir}/{name}.cbc"]

    async def build_test(self, test_name: str):
        test_work_dir, name = test_name.rsplit('/', 1)
        in_mode = test_name[test_name.find('standalone'):].split('/')[1]
        env = os.environ.copy()

        excluded_modes = self.standalone_tests.get(test_name, [])

        await self.print_status(f"Building test {test_name}")

        import_args = []
        aot_so_names = []
        for aot_file in sorted(glob.glob(os.path.join(test_work_dir, "*.aot.cj"))):
            aot_name = os.path.basename(aot_file)[:-len(".aot.cj")]
            aot_so = f"{test_work_dir}/lib{aot_name}.so"
            import_args = ["--import-path", test_work_dir]
            aot_log = io.StringIO()
            res = await self.run_cjc(aot_file,
                                     output_file=aot_so,
                                     output_type="dylib",
                                     use_tool_sh=True,
                                     log=aot_log)
            if res != 0:
                err_msg = f"{self.suite_mode.capitalize()} test AOT part compilation error ({aot_name}): {res}\n"
                if aot_log.getvalue().strip():
                    err_msg += aot_log.getvalue()
                await self.print_stderr(err_msg)
                self.compilation_failures.append((test_name, in_mode, f"during AOT compilation ({aot_name})"))
                return 1
            aot_so_names.append(aot_name)

        native_lib_names = []
        for c_file in sorted(glob.glob(os.path.join(test_work_dir, "*.c"))):
            native_name = os.path.basename(c_file)[:-len(".c")]
            native_so = f"{test_work_dir}/lib{native_name}.so"
            native_log = io.StringIO()
            native_err = io.StringIO()
            res = await run_in_env(True, env, ["cc", "-shared", "-fPIC", "-o", native_so, c_file],
                                  log=native_log, stderr_log=native_err)
            if res != 0:
                err_msg = f"{self.suite_mode.capitalize()} test native library compilation error ({native_name}): {res}\n"
                if native_log.getvalue().strip():
                    err_msg += native_log.getvalue()
                if native_err.getvalue().strip():
                    err_msg += native_err.getvalue()
                await self.print_stderr(err_msg)
                self.compilation_failures.append((test_name, in_mode, f"during native library compilation ({native_name})"))
                return 1
            native_lib_names.append(native_name)

        match in_mode:
            case "asm":
                compile_asm_to_obj = java_cmd(self.asm_jar_cds) + ['-jar', self.asm_jar, dotasm(test_name)]

                with open(f"{test_work_dir}/asm.out", "w+") as asm_log:
                    asm_err = io.StringIO()
                    res = await run_in_env(True, env, compile_asm_to_obj, log=asm_log, stderr_log=asm_err)
                    if res != 0:
                        asm_log.flush()
                        asm_log.seek(0)
                        asm_out = asm_log.read()
                        asm_msg = f"{self.suite_mode.capitalize()} test asm error: {res}"
                        if asm_out.strip():
                            asm_msg += asm_out
                        if asm_err.getvalue().strip():
                            asm_msg += asm_err.getvalue()
                        await self.print_stderr(asm_msg)
                        self.compilation_failures.append((test_name, in_mode, "during asm compilation"))
                        return 1

            case "cj":
                any_mode_succeeded = False
                attempted_modes = 0
                for mode, opt_flags in self.compilation_modes:
                    if mode in excluded_modes:
                        await self.print_status(f"Skipping {test_name} compilation ({mode} mode) due to config exclusion.")
                        continue

                    attempted_modes += 1
                    mode_work_dir = self.mode_work_dir(test_work_dir, mode, name)
                    os.makedirs(mode_work_dir, exist_ok=True)

                    int_chir_files = []
                    int_cj_files = sorted(glob.glob(os.path.join(test_work_dir, "*.int.cj")))

                    if int_cj_files:
                        int_temp_dir = f"{mode_work_dir}/temp"
                        import_args += ["--import-path", int_temp_dir]
                        os.makedirs(int_temp_dir, exist_ok=True)
                        for int_cj_file in int_cj_files:
                            int_fullname = os.path.splitext(os.path.basename(int_cj_file))[0]
                            int_basename = os.path.splitext(int_fullname)[0]
                            int_chir_files.append(f"temp/{int_basename}.chir")
                            int_log = io.StringIO()
                            res = await self.run_cjc(int_cj_file,
                                                     output_file=f"{int_temp_dir}/{int_basename}",
                                                     additional_args=["--save-temps", int_temp_dir, "--output-type", "staticlib"] +
                                                                     import_args + opt_flags,
                                                     use_tool_sh=True,
                                                     log=int_log)
                            if res != 0:
                                err_msg = f"{self.suite_mode.capitalize()} test INT package compilation error ({int_basename}): {res}\n"
                                if int_log.getvalue().strip():
                                    err_msg += int_log.getvalue()
                                await self.print_stderr(err_msg)
                                self.compilation_failures.append((test_name, in_mode, f"during INT package compilation ({int_basename})"))
                                return 1


                    chir_log = io.StringIO()
                    cbc_name = self.cbc_name(name)
                    res = await self.compile_cj(test_name, mode_work_dir, name,
                                                import_args, opt_flags, aot_so_names,
                                                native_lib_names, chir_log)
                    if res != 0:
                        err_msg = f"{self.suite_mode.capitalize()} test compilation error ({test_name} - {mode}): {res}\n"
                        if chir_log.getvalue().strip():
                            err_msg += chir_log.getvalue()
                        await self.print_stderr(err_msg)
                        self.compilation_failures.append((test_name, in_mode, f"during compilation ({mode})"))
                        continue

                    aot_deps_args = [f"-cbcaotdeps={':'.join(aot_so_names)}"] if aot_so_names else []
                    foreign_libs_args = [f"-foreignlibs={':'.join(native_lib_names)}"] if native_lib_names else []
                    jc_options = f"+IgnoreSourceFileNameForCbc {self.jc_options}"
                    chir_to_cbc = java_cmd(self.compiler_jar_cds) + ['-jar', self.compiler_jar, f"-outputname={cbc_name}", f"{cbc_name}.chir", jc_options] + aot_deps_args + foreign_libs_args + int_chir_files
                    cbc_log = io.StringIO()
                    cbc_err = io.StringIO()
                    res = await run_in_env(True, env, chir_to_cbc, cwd=mode_work_dir, log=cbc_log, stderr_log=cbc_err)
                    if res != 0:
                        err_detail = f"{self.suite_mode.capitalize()} test cbc-compiler.jar error ({test_name} - {mode}): {res}\n cmd: {chir_to_cbc}\n"
                        if cbc_log.getvalue().strip():
                            err_detail += cbc_log.getvalue()
                        if cbc_err.getvalue().strip():
                            err_detail += cbc_err.getvalue()
                        await self.print_stderr(err_detail)
                        self.compilation_failures.append((test_name, in_mode, f"during compilation ({mode})"))
                        continue

                    self.built_modes.add((test_name, mode))
                    any_mode_succeeded = True

                if attempted_modes > 0 and not any_mode_succeeded:
                    return 1

            case _:
                raise ValueError(f"Unknown standalone test mode: {in_mode}")

        return 0

    async def run_test(self, test_name: str):
        test_work_dir, name = test_name.rsplit('/', 1)
        in_mode = test_name[test_name.find('standalone'):].split('/')[1]
        env = os.environ.copy()

        current_ld_path = env.get("LD_LIBRARY_PATH", "")
        env["LD_LIBRARY_PATH"] = f"{test_work_dir}:{current_ld_path}" if current_ld_path else test_work_dir

        if in_mode == "asm":
            await self.print_status(f'Running {test_name} in standalone mode (asm)')
            cmd = ["launcher", dotcbc(test_name)]
            launcher_err = io.StringIO()
            with open(dotactual(test_name), "w") as test_output:
                res = await run_in_env(True, env, cmd, log=test_output, stderr_log=launcher_err)
                test_output.write(f"{res}\n")
            if await self.check_result(test_name) != 0:
                err = launcher_err.getvalue().strip()
                if err:
                    await self.print_stderr(err)
                msg = test_failed(test_name, in_mode, msg="diff (mode: standalone asm)")
                await self.print_stderr(msg)
                return True

        elif in_mode == "cj":
            excluded_modes = self.standalone_tests.get(test_name, [])
            for mode, _ in self.compilation_modes:
                if mode in excluded_modes or (test_name, mode) not in self.built_modes:
                    continue

                mode_work_dir = self.mode_work_dir(test_work_dir, mode, name)
                mode_cbc = f"{mode_work_dir}/{self.cbc_name(name)}.cbc"

                if not os.path.isfile(mode_cbc):
                    continue

                await self.print_status(f'Running {test_name} in {self.suite_mode} mode (cj - {mode})')
                cmd = self.cj_run_command(mode_work_dir, name, env)

                actual_file = f"{mode_work_dir}/{name}.actual"
                launcher_err = io.StringIO()
                with open(actual_file, "w") as test_output:
                    res = await run_in_env(True, env, cmd, log=test_output, stderr_log=launcher_err)
                    err_res = launcher_err.getvalue()
                    test_output.write(f"{res}\n" if not err_res else err_res)
                if await self.check_result(test_name, custom_actual=actual_file) != 0:
                    err = launcher_err.getvalue().strip()
                    if err:
                        await self.print_stderr(err)
                    msg = test_failed(test_name, in_mode, msg=f"diff (mode: {self.suite_mode} cj - {mode})")
                    await self.print_stderr(msg)
                    continue

        else:
            raise ValueError(f"Unknown standalone test mode: {in_mode}")

        return 0

    async def run(self, args):
        print(f"{self.suite_mode.capitalize()} testsuite")
        self.jc_options = args.jc_options

        tests = list(filter(lambda name: enabled(name, args), self.standalone_tests.keys()))
        if not tests:
            print("Tests set is empty")
            return

        if await self.prepare_tools(args) != 0:
            failedTests.append(f"{self.suite_mode} tool preparation failed")
            return

        self.compilation_failures = []
        self.built_modes.clear()
        global timing_chunk
        timing_chunk = 'build'
        succeeded: list[str] = await self.build_parallel(tests, self.build_test, args)
        timing_chunk = 'other'

        for test_name, in_mode, msg in self.compilation_failures:
            full_msg = test_failed(test_name, in_mode, msg=msg)
            await self.print_stderr(full_msg)

        timing_chunk = 'run'
        for test in succeeded:
            await self.run_test(test)
        timing_chunk = 'other'


class HotfixTestSuite(StandaloneTestSuite):
    suite_mode = "hotfix"
    compilation_modes = [("default", [])]
    standalone_tests = parse_tests_list(os.path.join(TestSuite.root_dir, "test-list-hotfix.cfg"),
                                       root_dir=TestSuite.root_dir)

    def expected_file(self, test_name: str):
        hotfix_expected = dotexpected(f"{test_name}.hotfix")
        return hotfix_expected if os.path.isfile(hotfix_expected) else super().expected_file(test_name)

    def cjc_env(self):
        return self.hotfix_env.copy()

    def mode_work_dir(self, test_work_dir, mode, name):
        return f"{test_work_dir}/hotfix-{mode}/{name}"

    def cbc_name(self, name):
        return f"diff{name}_patched"

    async def disassemble_chir(self, chir, mode_work_dir, env, log):
        res = await run_in_env(True, env.copy(), ["chir-dis", chir],
                               cwd=mode_work_dir, log=log, stderr_log=log)
        if res != 0:
            log.write(f"CHIR disassembly failed ({chir}): {res}\n")
        return res

    async def prepare_tools(self, args):
        self.plugin = os.path.join(self.toolchain_path, "tools/lib/libhotfix-plugin.so")
        env = os.environ.copy()
        env.pop('HOTFIX_GEN_THROW_STUBS', None)
        env['CANGJIE_STDX_PATH'] = os.path.expanduser(env.get(
            'CANGJIE_STDX_PATH', f"{self.toolchain_path}/stdx/dynamic/stdx"))
        env['LD_LIBRARY_PATH'] = os.pathsep.join(filter(None, [
            env['CANGJIE_STDX_PATH'], env.get('LD_LIBRARY_PATH', '')]))
        self.hotfix_env = env
        return 0

    async def compile_cj(self, test_name, mode_work_dir, name, import_args, opt_flags, aot_names, native_names, log):
        mode_work_dir = os.path.abspath(mode_work_dir)
        # cjc and the plugin write intermediate files into cwd. Isolate each test.
        for pattern in ("*.chir", "*.chirtxt", "*.cbc"):
            for path in glob.glob(os.path.join(mode_work_dir, pattern)):
                os.remove(path)
        executable = os.path.join(mode_work_dir, name)
        if os.path.isfile(executable):
            os.remove(executable)
        import_args = [os.path.abspath(arg) if i > 0 and import_args[i - 1] == '--import-path'
                       else arg for i, arg in enumerate(import_args)]
        test_work_dir = os.path.abspath(os.path.dirname(test_name))
        link_args = []
        for lib in aot_names + native_names:
            link_args += ["-L", test_work_dir, "-l", lib]
        # INT packages are bytecode inputs for CBC and static inputs for the native host.
        for int_file in sorted(glob.glob(os.path.join(test_work_dir, "*.int.cj"))):
            int_name = os.path.basename(int_file)[:-len(".int.cj")]
            archive = os.path.join(mode_work_dir, "temp", int_name)
            # Standalone writes extensionless archives; cjc accepts .a inputs.
            shutil.copy2(archive, f"{archive}.a")
            link_args.append(f"{archive}.a")
        common_args = import_args + link_args + ["--emit-chir", "--link-options=--export-dynamic"]
        stub_env = self.cjc_env()
        stub_env['HOTFIX_GEN_THROW_STUBS'] = 'true'
        res = await self.run_cjc(os.path.abspath(dotcj(test_name)), output_file=executable,
                                 additional_args=common_args + ["--plugin", self.plugin, "--continue"],
                                 cwd=mode_work_dir, log=log, env=stub_env)
        if res != 0:
            return res
        # --continue names CHIR after the package rather than the executable.
        chir_files = glob.glob(os.path.join(mode_work_dir, "*.chir"))
        if len(chir_files) != 1:
            log.write(f"Expected one original CHIR file, found: {chir_files}\n")
            return 1
        chir = dotchir(executable)
        os.replace(chir_files[0], chir)
        env = self.hotfix_env.copy()
        env['LD_LIBRARY_PATH'] = os.pathsep.join(filter(None, [test_work_dir, env.get('LD_LIBRARY_PATH')]))
        env.pop('ENABLE_INTERPRETER', None)
        env.pop('CBCOPT', None)
        res = await self.disassemble_chir(chir, mode_work_dir, env, log)
        if res != 0:
            return res
        stub_log = io.StringIO()
        res = await run_in_env(True, env.copy(), [executable], cwd=mode_work_dir,
                               log=stub_log, stderr_log=stub_log)
        with open(f"{executable}.stub.actual", "w") as output:
            output.write(stub_log.getvalue())
        if res != 1 or "Exception: should not reach here" not in stub_log.getvalue().splitlines():
            log.write(f"Expected throw-stub exception with exit status 1, got {res}:\n{stub_log.getvalue()}")
            return 1
        patched_chir = dotchir(f"{executable}_patched")
        res = await self.run_cjc(os.path.abspath(dotcj(test_name)), output_file=patched_chir,
                                 additional_args=common_args + ["--plugin", self.plugin],
                                 cwd=mode_work_dir, log=log)
        if res == 0:
            res = await self.disassemble_chir(patched_chir, mode_work_dir, env, log)
        if res == 0:
            res = await run_in_env(True, env, ["patch-gen", chir, patched_chir],
                                   cwd=mode_work_dir, log=log, stderr_log=log)
        if res == 0:
            patch_chir = dotchir(os.path.join(mode_work_dir, self.cbc_name(name)))
            res = await self.disassemble_chir(patch_chir, mode_work_dir, env, log)
        return res

    def cj_run_command(self, mode_work_dir, name, env):
        mode_work_dir = os.path.abspath(mode_work_dir)
        env['CANGJIE_STDX_PATH'] = self.hotfix_env['CANGJIE_STDX_PATH']
        env['LD_LIBRARY_PATH'] = os.pathsep.join(filter(None, [env.get('LD_LIBRARY_PATH'), mode_work_dir]))
        env['CBCOPT'] = f"cbc.path={mode_work_dir}"
        env['ENABLE_INTERPRETER'] = 'true'
        return [os.path.join(mode_work_dir, name)]


def enabled(name, args):
    if args.filter_file:
        return name in enabled_exact_tests
    return all(name.startswith(prefix) for prefix in args.filter)


enabled_exact_tests: dict = {}


def collect_enabled_exact_tests(args):
    global enabled_exact_tests
    enabled_exact_tests = {}
    if args.filter_file:
        enabled_exact_tests = parse_tests_list(args.filter_file, dirname(os.path.abspath(__file__)))


async def run_in_env(use_tool_sh: bool, env: dict[str, str], cmd: list[str], cwd=None, log=None, stderr_log=None):
    if use_tool_sh:
        env['TOOLCHAIN'] = toolchain_path
        cmd = [f'{TestSuite.root_dir}/tool.sh'] + cmd

    t0 = time.monotonic()
    res = await run_async(cmd, shell=False, log=log, env=env, cwd=cwd, stderr_log=stderr_log)
    await Timings.add(classify_cmd(cmd), time.monotonic() - t0, timing_chunk)
    return res


toolchain_path: str = ""

# Current chunk ('build' or 'run') for timing attribution; set by run().
timing_chunk: str = 'other'


def test_failed(name, in_mode="cj", msg=None):
    global failedTests

    if in_mode == "asm":
        fname = dotasm(name)
    elif in_mode == "cj":
        fname = dotcj(name)
    else:
        raise ValueError(f"Unknown test mode: {in_mode}")

    full_msg = f'{fname} failed ' + str(msg)
    failedTests.append(full_msg)
    return full_msg


failedTests: list = []


async def run_async(what, log=None, env=None, shell=False, cwd=None, stderr_log=None) -> int:
    async def logger(log_file, stream):
        while not stream.at_eof():
            data = await stream.readline()
            decoded = data.decode() if isinstance(data, bytes) else data
            if log_file is not None:
                log_file.write(decoded)

    kwargs = {}
    if log is not None:
        kwargs["stdout"] = asyncio.subprocess.PIPE
    if stderr_log is not None:
        kwargs["stderr"] = asyncio.subprocess.PIPE
    if env:
        kwargs["env"] = env

    if shell:
        process = await asyncio.create_subprocess_shell(what, cwd=cwd, **kwargs)
    else:
        process = await asyncio.create_subprocess_exec(*what, cwd=cwd, **kwargs)

    stdout_task = None
    stderr_task = None
    if log is not None:
        stdout_task = asyncio.create_task(logger(log, process.stdout))
    if stderr_log is not None:
        stderr_task = asyncio.create_task(logger(stderr_log, process.stderr))

    tasks = [t for t in [stdout_task, stderr_task] if t is not None]
    if tasks:
        await asyncio.gather(*tasks)
    return await process.wait()


def test(args):
    global toolchain_path
    global failedTests

    res = 0
    if args.filter_file:
        args.filter_file = os.path.abspath(args.filter_file)
    if args.cangjie_home:
        args.cangjie_home = os.path.abspath(os.path.expanduser(args.cangjie_home))
    os.chdir(TestSuite.root_dir)
    collect_enabled_exact_tests(args)

    toolchain_path = args.cangjie_home or os.environ.get("CANGJIE_HOME")
    if not toolchain_path:
        print("No toolchain path specified. Use --cangjie-home or set CANGJIE_HOME environment variable.", file=sys.stderr)
        exit(1)
    toolchain_path = os.path.abspath(os.path.expanduser(toolchain_path))
    source = "--cangjie-home flag" if args.cangjie_home else "CANGJIE_HOME env var"
    envsetup = toolchain_path + "/envsetup.sh"
    if not isfile(envsetup):
        print(f"There is no envsetup.sh in {toolchain_path} (set via {source}), path probably points to wrong place", file=sys.stderr)
        exit(1)

    print(f"CANGJIE_TOOLCHAIN: {toolchain_path}")

    suite = HotfixTestSuite if args.mode == "hotfix" else StandaloneTestSuite
    asyncio.run(suite(toolchain_path).run(args))

    print()
    print(Timings.report())

    if failedTests:
        print()
        print(f"[Summary] ({len(failedTests)} failed tests)")
        for fail in failedTests:
            print(fail)
        res += 10

    exit(res)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="CBC testsuite")
    subparsers = parser.add_subparsers(required=True)
    parser_test = subparsers.add_parser('test', help="Test all test cases")
    parser_test.set_defaults(func=test)
    parser_test.add_argument('--mode', choices=['standalone', 'hotfix'], default='standalone',
                             help="Test execution mode (default: standalone)")
    parser_test.add_argument('--cangjie-home', help="Path to Cangjie toolchain (overrides CANGJIE_HOME env var)")
    parser_test.add_argument('--filter', nargs='+', default=[], help="Run only test cases with given prefixes")
    parser_test.add_argument('--filter-file', help="Run only test cases listed in given file")
    parser_test.add_argument('-j', '--parallelism', nargs='?', help="Count of tests built in parallel")
    parser_test.add_argument('--jc-options', nargs='?', default='', help="Options for cbc compiler")

    parser_clean = subparsers.add_parser('clean', help="Clean up intermediate files")
    parser_clean.set_defaults(func=lambda _: TestSuite().clean())
    args = parser.parse_args()

    failedTests = []

    args.func(args)
