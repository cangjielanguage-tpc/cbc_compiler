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

    def __init__(self, toolchain_path: str = ""):
        self.toolchain_path = toolchain_path

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

    async def check_result(self, test_name: str, custom_actual: str = None):
        expected = dotexpected(test_name)
        actual = custom_actual if custom_actual is not None else dotactual(test_name)
        return await run_async(" ".join(diff(actual, expected)), shell=True)

    async def run_cjc(self, file: str, output_file: str, output_type: str = None,
                      additional_args: list[str] = [], use_tool_sh: bool = True, cwd=None):
        assert '.' in file
        cjc_args = []
        cjc_args += additional_args

        if output_type is not None:
            cjc_args += ["--output-type", output_type]

        _, file_extension = file.rsplit('.', 1)
        assert file_extension == "cj"

        cjc_args += ["-o", output_file]

        cjc_cmd_list = ['cjc', '--int-overflow', 'wrapping', '-Woff', 'unused', file, *cjc_args]

        return await run_in_env(use_tool_sh, os.environ.copy(), cjc_cmd_list, cwd=cwd)

    def clean(self):
        print("Cleaning up...")
        generated_patterns = ["*.out", "*.cbc", "*.bchir", "*.bchir2", "*.cjo", "*.pdba", "*.chir.fb",
                              "*.actual", "*.dasm", "*.a", "*.so"]
        generated_dirs = ["default/", "O2/", "jetpdb/"]
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
    standalone_tests = parse_tests_list("tests-list-standalone.cfg", root_dir=TestSuite.root_dir)

    def __init__(self, toolchain_path: str):
        super().__init__(toolchain_path)
        self.asm_jar = self.toolchain_path + "/tools/bin/cbc-asm.jar"
        self.compiler_jar = self.toolchain_path + "/tools/bin/cbc-compiler.jar"
        self.compilation_failures = []

    @staticmethod
    def c_sources(test_work_dir: str) -> list[str]:
        return sorted(glob.glob(os.path.join(test_work_dir, "*.c")))

    @staticmethod
    def c_shared_library_name(c_source: str) -> str:
        return f"lib{os.path.splitext(os.path.basename(c_source))[0]}.so"

    @staticmethod
    def shared_library_aot_dep(shared_library: str) -> str:
        library_name = os.path.basename(shared_library)
        if library_name.startswith("lib"):
            library_name = library_name[len("lib"):]
        return os.path.splitext(library_name)[0]

    def c_shared_libraries(self, test_work_dir: str) -> list[str]:
        return [
            os.path.join(test_work_dir, self.c_shared_library_name(c_source))
            for c_source in self.c_sources(test_work_dir)
        ]

    async def build_c_shared_libraries(self, test_work_dir: str, env: dict[str, str]) -> int:
        for c_source in self.c_sources(test_work_dir):
            source_name = os.path.basename(c_source)
            shared_library_name = self.c_shared_library_name(c_source)
            compile_c_to_so = ["gcc", "-shared", "-fPIC", source_name, "-o", shared_library_name]

            res = await run_in_env(True, env, compile_c_to_so, cwd=test_work_dir)
            if res != 0:
                return res

        return 0

    def c_link_args(self, test_work_dir: str) -> list[str]:
        c_link_args = []
        for shared_library in self.c_shared_libraries(test_work_dir):
            if os.path.isfile(shared_library):
                c_link_args += ["-L", test_work_dir, "-l", self.shared_library_aot_dep(shared_library)]
        return c_link_args

    def cbc_aot_deps_args(self, test_work_dir: str) -> list[str]:
        cbc_aot_deps = [
            self.shared_library_aot_dep(shared_library)
            for shared_library in self.c_shared_libraries(test_work_dir)
            if os.path.isfile(shared_library)
        ]
        return [f"-cbcaotdeps={':'.join(cbc_aot_deps)}"] if cbc_aot_deps else []

    async def build_test(self, test_name: str):
        test_work_dir, name = test_name.rsplit('/', 1)
        in_mode = test_name[test_name.find('standalone'):].split('/')[1]
        env = os.environ.copy()

        excluded_modes = self.standalone_tests.get(test_name, [])

        print(f"Building test {test_name}")

        res = await self.build_c_shared_libraries(test_work_dir, env)
        if res != 0:
            print(f"Standalone test C shared library compilation error: {res}", file=sys.stderr)
            self.compilation_failures.append((test_name, in_mode, "during C shared library compilation"))
            return 1

        import_args = []
        if os.path.isfile(dotcjaot(test_name)):
            import_args = ["--import-path", test_work_dir]
            res = await self.run_cjc(dotcjaot(test_name),
                                     output_file=f"{test_work_dir}/libaot.so",
                                     output_type="dylib",
                                     use_tool_sh=True)
            if res != 0:
                print(f"Standalone test AOT part compilation error: {res}", file=sys.stderr)
                self.compilation_failures.append((test_name, in_mode, "during AOT compilation"))
                return 1

        match in_mode:
            case "asm":
                compile_asm_to_obj = [java_cmd(), '-jar', self.asm_jar, dotasm(test_name)]

                with open(f"{test_work_dir}/asm.out", "w") as asm_log:
                    res = await run_in_env(True, env, compile_asm_to_obj, log=asm_log)
                    if res != 0:
                        print(f"Standalone test asm error: {res}", file=sys.stderr)
                        self.compilation_failures.append((test_name, in_mode, "during asm compilation"))
                        return 1

            case "cj":
                any_mode_succeeded = False
                attempted_modes = 0
                for mode, opt_flags in [("default", []), ("O2", ["-O2"])]:
                    if mode in excluded_modes:
                        print(f"Skipping {test_name} compilation ({mode} mode) due to config exclusion.")
                        continue

                    attempted_modes += 1
                    mode_work_dir = f"{test_work_dir}/{mode}"
                    os.makedirs(mode_work_dir, exist_ok=True)

                    output_chir = dotchir(f"{mode_work_dir}/{name}")

                    res = await self.run_cjc(dotcj(test_name),
                                             output_file=output_chir,
                                             additional_args=["--emit-chir"] + import_args +
                                                             self.c_link_args(test_work_dir) + opt_flags,
                                             use_tool_sh=True)
                    if res != 0:
                        print(f"Standalone test compilation error ({test_name} - {mode}): {res}", file=sys.stderr)
                        self.compilation_failures.append((test_name, in_mode, f"during compilation ({mode})"))
                        continue

                    chir_to_cbc = [java_cmd(), '-jar', self.compiler_jar, f"{name}.chir",
                                   *self.cbc_aot_deps_args(test_work_dir), args.jc_options]
                    res = await run_in_env(True, env, chir_to_cbc, cwd=mode_work_dir)
                    if res != 0:
                        print(f"Standalone test cbc-compiler.jar error ({test_name} - {mode}): {res}\n cmd: {chir_to_cbc}", file=sys.stderr)
                        self.compilation_failures.append((test_name, in_mode, f"during compilation ({mode})"))
                        continue

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
            print(f'Running {test_name} in standalone mode (asm)')
            cmd = ["launcher", dotcbc(test_name)]
            with open(dotactual(test_name), "w") as test_output:
                res = await run_in_env(True, env, cmd, log=test_output)
                test_output.write(f"{res}\n")

            if await self.check_result(test_name) != 0:
                return test_failed(test_name, in_mode, msg="diff (mode: standalone asm)")

        elif in_mode == "cj":
            excluded_modes = self.standalone_tests.get(test_name, [])
            for mode in ["default", "O2"]:
                if mode in excluded_modes:
                    continue

                mode_work_dir = f"{test_work_dir}/{mode}"
                mode_cbc = f"{mode_work_dir}/{name}.cbc"

                if not os.path.isfile(mode_cbc):
                    continue

                print(f'Running {test_name} in standalone mode (cj - {mode})')
                cmd = ["launcher", mode_cbc]

                actual_file = f"{mode_work_dir}/{name}.actual"
                with open(actual_file, "w") as test_output:
                    res = await run_in_env(True, env, cmd, log=test_output)
                    test_output.write(f"{res}\n")

                if await self.check_result(test_name, custom_actual=actual_file) != 0:
                    return test_failed(test_name, in_mode, msg=f"diff (mode: standalone cj - {mode})")

        else:
            raise ValueError(f"Unknown standalone test mode: {in_mode}")

        return 0

    async def run(self, args):
        print("Standalone testsuite")

        tests = list(filter(lambda name: enabled(name, args), self.standalone_tests.keys()))
        if not tests:
            print("Tests set is empty")
            return

        self.compilation_failures = []
        succeeded: list[str] = await self.build_parallel(tests, self.build_test, args)

        for test_name, in_mode, msg in self.compilation_failures:
            test_failed(test_name, in_mode, msg=msg)

        for test in succeeded:
            await self.run_test(test)


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


async def run_in_env(use_tool_sh: bool, env: dict[str, str], cmd: list[str], cwd=None, log=None):
    if use_tool_sh:
        env['TOOLCHAIN'] = toolchain_path
        cmd = [f'{TestSuite.root_dir}/tool.sh'] + cmd

    cmd_to_run = " ".join(cmd)
    return await run_async(cmd_to_run, shell=True, log=log, env=env, cwd=cwd)


toolchain_path: str = ""


def test_failed(name, in_mode="cj", msg=None, log=None):
    global failedTests

    if in_mode == "asm":
        fname = dotasm(name)
    elif in_mode == "cj":
        fname = dotcj(name)
    else:
        raise ValueError(f"Unknown test mode: {in_mode}")

    full_msg = f'{fname} failed ' + str(msg)
    failedTests.append(full_msg)
    print(full_msg, file=log, flush=True if not log else None)
    return True


failedTests: list = []


async def run_async(what, log=None, env=None, shell=False, cwd=None) -> int:
    async def logger(log_file, stream):
        while not stream.at_eof():
            data = await stream.readline()
            if isinstance(log_file, (io.TextIOBase, io.TextIOWrapper)):
                log.write(data.decode())
            else:
                log.write(data)

    kwargs = {}
    if log:
        kwargs["stdout"] = asyncio.subprocess.PIPE
    if env:
        kwargs["env"] = env

    if shell:
        process = await asyncio.create_subprocess_shell(what, cwd=cwd, **kwargs)
    else:
        process = await asyncio.create_subprocess_exec(*what, cwd=cwd, **kwargs)

    if log:
        await logger(log, process.stdout)
    return await process.wait()


def test(args):
    global toolchain_path
    global failedTests

    res = 0
    collect_enabled_exact_tests(args)

    toolchain_path = args.toolchain
    envsetup = toolchain_path + "/envsetup.sh"
    if not isfile(envsetup):
        print(f"There is no envsetup.sh in {toolchain_path}, path probably points to wrong place", file=sys.stderr)
        exit(1)

    print(f"CANGJIE_TOOLCHAIN: {toolchain_path}")

    asyncio.run(StandaloneTestSuite(toolchain_path).run(args))

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
    parser_test.add_argument('toolchain', help="Path to Cangjie toolchain")
    parser_test.add_argument('--filter', nargs='+', default=[], help="Run only test cases with given prefixes")
    parser_test.add_argument('--filter-file', help="Run only test cases listed in given file")
    parser_test.add_argument('-j', '--parallelism', nargs='?', help="Count of tests built in parallel")
    parser_test.add_argument('--jc-options', nargs='?', default='', help="Options for cbc compiler")

    parser_clean = subparsers.add_parser('clean', help="Clean up intermediate files")
    parser_clean.set_defaults(func=lambda _: TestSuite().clean())
    args = parser.parse_args()

    failedTests = []

    args.func(args)
