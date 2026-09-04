import os
import sys
import argparse
import shutil
import asyncio
import subprocess
import traceback
from pathlib import Path

SEMAPHORE = asyncio.Semaphore(os.cpu_count() or 4)

def group_files_by_tags(master_test_dir: Path, search_root: Path, tags_only: set[str]) -> dict[str, list[Path]]:
    """
    Recursively scans from the repository search_root to group files.
    - Expected files are targeted flatly inside the identified master module directory.
    - Source files (*.cj, *.toml) are discovered across all subdirectories.
      If tag-specific version of file is absent, release version of file (without suffix) is added.
    """
    tags_file_path = master_test_dir / "tags"
    tags = set(split_first_line(tags_file_path))
    if not tags:
        print("[ERROR] The first line of the 'tags' file is empty.")
        return False

    tags_excluded_file_path = master_test_dir / "tags_excluded"
    tags_excluded = set(split_first_line(tags_excluded_file_path))
    if tags_excluded is None:
        print("[ERROR] The first line of the 'tags_excluded' file is empty.")
        return False

    filtered_tags = tags - tags_excluded & tags_only if tags_only else tags - tags_excluded
    all_groups = list(filtered_tags)
    if "release" in all_groups:
        print("[ERROR] Release tag is reserved for the files with tag prefix.")
        return {}
    all_groups.append("release")

    grouped_data = {tag: [] for tag in all_groups}

    extensions = ["*.cj", "*.toml", "result*.expected"]
    all_source_files = []
    for ext in extensions:
        all_source_files.extend(list(search_root.rglob(ext)))

    for file_path in all_source_files:
        stem = file_path.stem
        ext = file_path.suffix

        exclude_file = False
        for tag in tags_excluded:
            if stem.endswith(f"_{tag}"):
                exclude_file = True
                break
        if exclude_file:
            continue

        matched_a_tag = False
        for tag in tags - tags_excluded:
            if stem.endswith(f"_{tag}"):
                if (not tags_only or tag in tags_only):
                    grouped_data[tag].append(file_path.relative_to(search_root))
                matched_a_tag = True
                break

        if not matched_a_tag:
            grouped_data["release"].append(file_path.relative_to(search_root))
            for tag in filtered_tags:
                tag_specific_sibling = file_path.with_name(f"{stem}_{tag}{ext}")
                if not tag_specific_sibling.exists():
                    grouped_data[tag].append(file_path.relative_to(search_root))

    return grouped_data

async def build_and_test_patch(release_dir: Path, tag_dir: Path, log_prefix: str) -> str | None:
    """
    Asynchronously creates a patch from differences between tagged and release version,
    executes the patched binary, and returns its runtime standard output string.
    Returns the stripped actual output string on success, or None on failure.
    """
    if not release_dir.exists():
        print(f"{log_prefix} [ERROR] Release directory not found in .hotfix target: '{release_dir}'")
        return None

    if not tag_dir.exists():
        print(f"{log_prefix} [ERROR] Tag directory not found in .hotfix target: '{tag_dir}'")
        return None

    release_chirs = {f.name: f for f in release_dir.glob("*.chir")}
    tag_chirs = {f.name: f for f in tag_dir.glob("*.chir")}

    files_to_patch = {name: path for name, path in tag_chirs.items() if name in release_chirs}
    new_files = {name: path for name, path in tag_chirs.items() if name not in release_chirs}

    if new_files:
        print(f"{log_prefix} [INFO] Discovered new files: {list(new_files.keys())}")
    else:
        print(f"{log_prefix} [INFO] No unique new .chir files detected in this tag variant.")

    async def patch_in_temp(tmp_path: Path) -> str | None:
        diff_tasks = []
        diff_files = []

        for tag_chir_name, tag_chir_path in files_to_patch.items():
            release_chir_path = release_chirs[tag_chir_name]

            diff_file = tmp_path / f"diff{tag_chir_name}"
            diff_files.append(diff_file)

            async def execute_diff(tag_chir_name: str, release_chir_path: Path, tag_chir_path: Path, tmp_path: Path, log_prefix: str):
                async with SEMAPHORE:
                    print(f"{log_prefix} [RUN] patch-gen {release_chir_path.name} ➔ {tag_chir_path.name}")
                    proc = await asyncio.create_subprocess_exec(
                        "patch-gen", str(release_chir_path.resolve()), str(tag_chir_path.resolve()),
                        cwd=str(tmp_path),
                        stdout=asyncio.subprocess.PIPE,
                        stderr=asyncio.subprocess.PIPE
                    )
                    _, stderr_bytes = await proc.communicate()
                    return proc.returncode, stderr_bytes, tag_chir_name

            diff_tasks.append(execute_diff(tag_chir_name, release_chir_path, tag_chir_path, tag_dir, log_prefix))

        if diff_tasks:
            print(f"{log_prefix} [INFO] Processing {len(diff_tasks)} patch-gen jobs concurrently...")
            processes = await asyncio.gather(*diff_tasks)
            for diff_ret_code, diff_stderr_bytes, chir_name in processes:
                if diff_ret_code != 0:
                    err_msg = diff_stderr_bytes.decode().strip() or "No details."
                    print(f"{log_prefix} [ERROR] Diff failed for {tag_chir_name} (code {diff_ret_code})")
                    print(f"{log_prefix} [DEBUG] Log details: {err_msg}")
                    return None

            print(f"{log_prefix} [SUCCESS] All patch-gen jobs passed successfully.")

        valid_diff_files = [f for f in diff_files if f.is_file()]
        # TODO use patch-gen for new files as well
        valid_new_files = [path for path in new_files.values() if path.is_file()]

        compiled_targets = valid_diff_files + valid_new_files

        if not compiled_targets:
            print(f"{log_prefix} [ERROR] No patchable differential or new source files generated.")
            return None

        print(f"{log_prefix} [INFO] Found {len(compiled_targets)} files ({len(valid_diff_files)} diffs, {len(valid_new_files)} new) ready to compile.")

        patch_name = "patch"

        cbc_args = ["cbc-compiler", f"-outputname={patch_name}", "+genlibrary"]
        cbc_args.extend([str(f.resolve()) for f in compiled_targets])

        async def execute_jc(cbc_args: list[str], tmp_path: Path, log_prefix: str):
            async with SEMAPHORE:
                print(f"{log_prefix} [RUN] {' '.join(cbc_args)}")
                proc = await asyncio.create_subprocess_exec(
                    *cbc_args,
                    cwd=str(tmp_path),
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE
                )
                stdout_bytes, stderr_bytes = await proc.communicate()
                return proc.returncode, stdout_bytes, stderr_bytes

        jc_ret_code, jc_stdout_bytes, jc_stderr_bytes = await execute_jc(cbc_args, tmp_path, log_prefix)

        if jc_ret_code != 0:
            print(f"{log_prefix} [ERROR] cbc-compiler toolchain failure (code {jc_ret_code})")
            print(f"{log_prefix} [DEBUG] std output: {jc_stdout_bytes.decode().strip()}")
            print(f"{log_prefix} [DEBUG] std err: {jc_stderr_bytes.decode().strip()}")
            return None

        cbc_file = tmp_path / f"{patch_name}.cbc"
        if not cbc_file.exists():
            print(f"{log_prefix} [ERROR] Target distribution anomaly: '{patch_name}.cbc' was missing from output pool.")
            return None
        print(f"{log_prefix} [SUCCESS] Hotfix patch bundle successfully compiled: '{cbc_file.resolve()}'")

        cangjie_home = os.environ.get("CANGJIE_HOME")
        if not cangjie_home:
            print(f"{log_prefix} [ERROR] $CANGJIE_HOME is undefined in env context.")
            return None

        source_engine_so = Path(cangjie_home) / "tools" / "lib" / "libcbcengine.so"
        if not source_engine_so.is_file():
            print(f"{log_prefix} [ERROR] Toolchain dependency library not found at: '{source_engine_so}'")
            return None

        test_env = os.environ.copy()
        test_env["LD_LIBRARY_PATH"] = f"{release_dir}:{test_env.get('LD_LIBRARY_PATH', '')}"
        test_env["CBCOPT"] = f"cbc.path={tmp_path} {test_env.get('CBCOPT', '')}"
        test_env["ENABLE_INTERPRETER"] = "true"

        async def execute_patch(binary_path: Path, tmp_path: Path, test_env: dict[str, str], log_prefix: str):
            async with SEMAPHORE:
                print(f"{log_prefix} [RUN] LD_LIBRARY_PATH=\"{test_env['LD_LIBRARY_PATH']}\" CBCOPT=\"{test_env['CBCOPT']}\" ENABLE_INTERPRETER=\"{test_env['ENABLE_INTERPRETER']}\" {binary_path}")
                proc = await asyncio.create_subprocess_exec(
                    binary_path,
                    cwd=str(tmp_path.resolve()),
                    env=test_env,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE
                )
                stdout_bytes, stderr_bytes = await proc.communicate()
                return proc.returncode, stdout_bytes, stderr_bytes

        try:
            patch_ret_code, patch_stdout_bytes, patch_stderr_bytes = await execute_patch(f"{release_dir}/main",
                tmp_path, test_env, log_prefix)
            actual_test_output = patch_stdout_bytes.decode(encoding="utf-8", errors="ignore").strip()
            if patch_ret_code != 0:
                err_log = patch_stderr_bytes.decode(encoding="utf-8", errors="ignore").strip()
                print(f"{log_prefix} [ERROR] Patched binary execution crashed with exit code {patch_ret_code}")
                print(f"{log_prefix} [DEBUG] Error details: {err_log if err_log else actual_test_output}")
                return None
            return actual_test_output

        except Exception as test_err:
            print(f"{log_prefix} [ERROR] Unhandled exception occurred during running patch: {test_err}")
            traceback.print_exc()
            return None

    return await patch_in_temp(tag_dir)

async def build_and_test_tag_version(tag: str, tag_target_root: Path, exec_subdir_name: Path, release_hotfix_dir: Path) -> bool:
    """
    Executes 'cjpm clean && cjpm run' asynchronously inside the test module folder.
    Isolates .chir objects, builds hotfix patches, and compares runtime output vs result.expected.
    """
    log_prefix = f"[TAG-{tag.upper()}]"
    exec_dir = tag_target_root / exec_subdir_name
    print(f"{log_prefix} [INFO] Initializing execution runtime in: {exec_dir}")

    try:
        async def exec_cjpm_clean():
            async with SEMAPHORE:
                proc = await asyncio.create_subprocess_exec(
                    "cjpm", "clean",
                    cwd=exec_dir,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE
                )
                stdout_bytes, stderr_bytes = await proc.communicate()
                return proc.returncode, stdout_bytes, stderr_bytes

        cjpm_clean_ret_code, _, cjpm_clean_stderr_bytes = await exec_cjpm_clean()
        if cjpm_clean_ret_code != 0:
            err_msg = cjpm_clean_stderr_bytes.decode(encoding="utf-8", errors="ignore").strip()
            print(f"{log_prefix} [ERROR] Process execution failed (code {cjpm_clean_ret_code}). Error: {err_msg}")
            return False

        async def exec_cjpm_run():
            async with SEMAPHORE:
                proc = await asyncio.create_subprocess_exec(
                    "cjpm", "run",
                    cwd=exec_dir,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE
                )
                stdout_bytes, stderr_bytes = await proc.communicate()
                return proc.returncode, stdout_bytes, stderr_bytes

        cjpm_run_ret_code, cjpm_run_stdout_bytes, cjpm_run_stderr_bytes = await exec_cjpm_run()
        if cjpm_run_ret_code != 0:
            err_msg = cjpm_run_stderr_bytes.decode(encoding="utf-8", errors="ignore").strip()
            print(f"{log_prefix} [ERROR] Process execution failed (code {cjpm_run_ret_code}). Error: {err_msg}")
            return False

        actual_output = cjpm_run_stdout_bytes.decode(encoding="utf-8", errors="ignore").replace("cjpm run finished", "").strip()

        expected_file_path = exec_dir / "result.expected"
        if expected_file_path.is_file():
            expected_output = expected_file_path.read_text(encoding="utf-8").strip()
            if actual_output == expected_output:
                print(f"{log_prefix} [SUCCESS] Output matches 'result.expected'.")
            else:
                print(f"{log_prefix} [ERROR] Assertion Mismatch Failure!\n   Expected: '{expected_output}'\n   Got:      '{actual_output}'")
                return False
        else:
            print(f"{log_prefix} [ERROR] No expected file found (result.expected).")
            return False

        expected_output = await asyncio.to_thread(expected_file_path.read_text, encoding="utf-8")
        normalized_expected = expected_output.strip()

        target_dir = exec_dir / "target"
        if target_dir.is_dir():
            chir_files = await asyncio.to_thread(lambda: list(target_dir.rglob("*.chir")))

            if chir_files:
                hotfix_dir = exec_dir / ".hotfix"
                await asyncio.to_thread(hotfix_dir.mkdir, parents=True, exist_ok=True)

                copy_tasks = [
                    asyncio.to_thread(shutil.copy2, item, hotfix_dir / item.name)
                    for item in chir_files
                ]
                await asyncio.gather(*copy_tasks)
                print(f"{log_prefix} [SUCCESS] Processed hotfix artifacts (Captured {len(chir_files)} .chir files).")

                patched_runtime_output = await build_and_test_patch(release_hotfix_dir, hotfix_dir, log_prefix)

                if patched_runtime_output is None:
                    return False

                normalized_patched_actual = patched_runtime_output.strip()

                if normalized_patched_actual == normalized_expected:
                    print(f"{log_prefix} [SUCCESS] Patched binary output matches 'result.expected'.")
                else:
                    print(f"{log_prefix} [ERROR] Patched Runtime Assertion Mismatch Error!")
                    print(f"{log_prefix} [DEBUG]  Expected: '{normalized_expected}'")
                    print(f"{log_prefix} [DEBUG]  Got:      '{normalized_patched_actual}'")
                    return False

    except Exception as e:
        print(f"{log_prefix} [ERROR] Unhandled error inside async worker task layer: {e}")
        traceback.print_exc()
        return False
    return True


def build_and_test_release_version(workspace_dir: Path, exec_subdir_name: Path) -> str | None:
    """
    Executes 'cjpm clean && cjpm run' synchronously inside the release module folder.
    Asserts standard outputs and copies BOTH .chir and .exe files into .hotfix/.
    Returns the Path to the .hotfix directory if successful, or None on failure.
    """
    log_prefix = "[RELEASE]"
    exec_dir = workspace_dir / exec_subdir_name
    print(f"{log_prefix} Initializing execution runtime in: {exec_dir}")

    try:
        subprocess.run(["cjpm", "clean"], cwd=exec_dir, capture_output=True, check=True)

        result = subprocess.run(
            ["cjpm", "run"], cwd=exec_dir,
            text=True, capture_output=True
        )

        actual_output = result.stdout.replace("cjpm run finished", "").strip() if result.stdout else ""

        if result.returncode != 0:
            print(f"{log_prefix} [ERROR] Process failed (code {result.returncode}). Logs: {result.stderr.strip()}")
            return None

        expected_file_path = exec_dir / "result.expected"
        if expected_file_path.is_file():
            expected_output = expected_file_path.read_text(encoding="utf-8").strip()
            if actual_output == expected_output:
                print(f"{log_prefix} Output matches 'result.expected'.")
            else:
                print(f"{log_prefix} Assertion Mismatch Failure!\n   Expected: '{expected_output}'\n   Got:      '{actual_output}'")
                return None
        else:
            print(f"{log_prefix} [ERROR] No expected file found (result.expected). Skipping assertion.")
            return None

        hotfix_dir = exec_dir / ".hotfix"

        target_dir = exec_dir / "target"
        if target_dir.is_dir():
            artifacts = list(target_dir.rglob("*.chir")) + list(target_dir.rglob("main")) + list(target_dir.rglob("*.so"))
            if artifacts:
                hotfix_dir.mkdir(parents=True, exist_ok=True)
                for item in artifacts:
                    shutil.copy2(item, hotfix_dir / item.name)
                print(f"{log_prefix} Processed hotfix artifacts (Captured {len(artifacts)} files).")

        return hotfix_dir

    except Exception as e:
        print(f"{log_prefix} [ERROR] Unhandled error inside sync runner environment: {e}")
        traceback.print_exc()
        return None

def run_tests(search_root: Path, tag_groups: dict[str, list[Path]], tmp_dir: Path, master_test_subdir_name: str) -> bool:
    """
    Synchronously creates sandbox directories, copies layout dependencies,
    builds a release version of app synchronously and each of tag versions of app asynchronously.

    Returns True if the entire tests pass successfully, False otherwise.
    """
    tags_to_execute = []
    release_root = None

    for tag, relative_files in tag_groups.items():
        tag_target_root = tmp_dir / tag
        tag_target_root.mkdir(parents=True, exist_ok=True)

        for rel_path in relative_files:
            source_file = search_root / rel_path
            dest_file_dir = tag_target_root / rel_path.parent
            dest_file_dir.mkdir(parents=True, exist_ok=True)

            original_name = source_file.name
            stem = source_file.stem
            suffix = source_file.suffix

            tag_ending = f"_{tag}"

            if suffix in (".cj", ".cjpm", ".expected", ".toml") and stem.endswith(tag_ending):
                clean_stem = stem[:-len(tag_ending)]
                dest_filename = f"{clean_stem}{suffix}"
            else:
                dest_filename = original_name

            dest_file = dest_file_dir / dest_filename
            shutil.copy2(source_file, dest_file)

        if tag == "release":
            release_root = tag_target_root
        else:
            tags_to_execute.append((tag, tag_target_root))

    if not release_root:
        print("[ERROR] 'release' tag was not found. ")
        return False

    print(f"\n--- Phase 1: Building release version of app ---")
    release_hotfix_dir = build_and_test_release_version(release_root, master_test_subdir_name)

    if release_hotfix_dir is None:
        return False

    print(f"[SUCCESS] Release version of app: {release_hotfix_dir}")

    if tags_to_execute:
        print(f"\n--- Phase 2: Building tag version of app ---")

        async def run_gathered_tasks():
            async_tasks = [
                build_and_test_tag_version(tag, path, master_test_subdir_name, release_hotfix_dir)
                for tag, path in tags_to_execute
            ]
            return await asyncio.gather(*async_tasks)

        pipeline_results = asyncio.run(run_gathered_tasks())

        if pipeline_results and False in pipeline_results:
            print("[ERROR] One or more asynchronous tag test tasks failed. Preserving workspaces for debugging.")
            return False

    return True

def split_first_line(path: Path) -> list[str] | None:
    try:
        with path.open("r", encoding="utf-8") as f:
            first_line = f.readline().strip()
        return [t.strip() for t in first_line.split(",") if t.strip()]
    except Exception as e:
        print(f"[ERROR] Error parsing '{path}' configuration file: {e}")
        return None

def test_command(test_root_dir: Path, tmp_dir_arg: str, tags_only: set[str]) -> bool:
    master_test_dir = None
    if (test_root_dir / "cjpm.toml").exists() and (test_root_dir / "tags").exists():
        # one module
        master_test_dir = test_root_dir
    else :
        # several modules
        for item in test_root_dir.iterdir():
            if item.is_dir():
                if (item / "cjpm.toml").exists() and (item / "tags").exists():
                    master_test_dir = item
                    break

    if not master_test_dir:
        print("[ERROR] Could not locate a first-level module subdirectory containing both 'cjpm.toml' and 'tags'.")
        return False

    print(f"\nREPOSITORY ROOT:         {test_root_dir.name}")
    print(f"MASTER MODULE:           {master_test_dir.name}/")

    tmp_dir = Path(test_root_dir / tmp_dir_arg).resolve() if tmp_dir_arg else master_test_dir / "temp"
    if tmp_dir.is_dir():
        print(f"[INFO] Clearing environment in temp directory: {tmp_dir.name}/")
        try:
            shutil.rmtree(tmp_dir)
            print("[INFO] Clean environment initialized successfully.")
        except Exception as pre_cleanup_err:
            print(f"[WARN] Non-breaking initialization cleanup anomaly: {pre_cleanup_err}")

    tag_groups = group_files_by_tags(master_test_dir, test_root_dir, tags_only)
    if not tag_groups:
        return False

    pipeline_success = run_tests(test_root_dir, tag_groups, tmp_dir,
        master_test_dir.name if master_test_dir != test_root_dir else "")

    if not tmp_dir_arg and pipeline_success and tmp_dir.is_dir():
        print(f"[SUCCESS] All pipelines passed successfully. Purging temporary workspaces in: {tmp_dir.name}/")
        try:
            shutil.rmtree(tmp_dir)
            print("[INFO] Workspace directory tree cleaned completely.")
        except Exception as cleanup_err:
            print(f"[WARN] Error during cleanup: {cleanup_err}")

    if pipeline_success:
        return True
    else:
        print(f"[DEBUG] Failed environment preserved inside '{tmp_dir.name}/' directory.")
        return False

def main():
    parser = argparse.ArgumentParser(description="One-file Python Hotfix Test Framework")
    subparsers = parser.add_subparsers(dest="command", required=True)

    test_parser = subparsers.add_parser("test", help="Scan and run tests")

    test_parser.add_argument(
        "--save-temps",
        type=str,
        help="Custom path to save temp test outputs",
    )

    test_parser.add_argument(
        '--tests-only',
        nargs='+',
        default=[],
        help="Run only test cases with given prefixes")

    test_parser.add_argument(
        '--tags-only',
        nargs='+',
        default=[],
        help="Run test cases with given tags")

    # TODO the task that creates empty test project

    args = parser.parse_args()

    if args.command == "test":
        script_dir = Path(__file__).resolve().parent

        tests_excluded = split_first_line(script_dir / "tests_excluded")
        if tests_excluded is None:
            print("[ERROR] The first line of the 'tests_excluded' file is empty.")
            sys.exit(1)

        failed_tests = []
        for item in script_dir.iterdir():
            if item.is_dir() and item.name not in tests_excluded and (not args.tests_only or item.name in args.tests_only):
                passed = test_command(item, args.save_temps, set(args.tags_only))
                if passed is False:
                    print(f"[ERROR] Test '{item.name}' execution failed.")
                    failed_tests.append(item.name)

        if failed_tests:
            failed_tests_str = "\n".join(failed_tests)
            print(f"\n[ERROR] Failed tests:\n{failed_tests_str}")
            sys.exit(1)
        else:
            print("\n[SUCCESS] All tests passed completely.")

if __name__ == "__main__":
    main()
