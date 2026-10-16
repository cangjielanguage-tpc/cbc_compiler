#!/usr/bin/env bash

set -euo pipefail

readonly SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
readonly WORKSPACE_DIR="$(cd -- "${SCRIPT_DIR}/../../../../.." && pwd)"
readonly ENGINE_DIR="${ENGINE_DIR:-${WORKSPACE_DIR}/cbc-engine-initial-stage}"
readonly CANGJIE_SDK_DIR="${CANGJIE_SDK_DIR:-${WORKSPACE_DIR}/cangjie}"
readonly BUILD_ROOT="${BUILD_ROOT:-${ENGINE_DIR}/output/profiler-benchmarks}"
readonly RESULTS_DIR="${RESULTS_DIR:-${SCRIPT_DIR}/profiler_results}"
readonly JOBS="${JOBS:-$(nproc)}"
readonly BUILD_TYPE="${BUILD_TYPE:-Release}"
readonly TOOLCHAIN_FILE="${TOOLCHAIN_FILE:-${ENGINE_DIR}/cmake/toolchains/x86_64-linux-gnu-clang.cmake}"
readonly COMPILER_JAR="${CANGJIE_SDK_DIR}/tools/bin/cbc-compiler.jar"
readonly SDK_LIBRARY="${CANGJIE_SDK_DIR}/tools/lib/libcbcengine.so"
readonly COMPILE_COMMANDS="${ENGINE_DIR}/compile_commands.json"
readonly MEASUREMENT_SCRIPT="${SCRIPT_DIR}/run_measurements.sh"
readonly ANALYZER="${SCRIPT_DIR}/analyze_results.py"
readonly TEST_SOURCE="${TEST_SOURCE:-${SCRIPT_DIR}/main.cj}"
readonly -a IMPLEMENTATIONS=(profiler double_buffer atomic)

if [[ ! -f "${CANGJIE_SDK_DIR}/envsetup.sh" ]]; then
    echo "Cangjie envsetup.sh not found: ${CANGJIE_SDK_DIR}/envsetup.sh" >&2
    exit 1
fi

if [[ ! -f "${TOOLCHAIN_FILE}" ]]; then
    echo "CMake toolchain not found: ${TOOLCHAIN_FILE}" >&2
    exit 1
fi

if [[ ! -f "${COMPILER_JAR}" ]]; then
    echo "CBC compiler not found: ${COMPILER_JAR}" >&2
    echo "Copy the current CBC compiler build into the Cangjie SDK." >&2
    exit 1
fi

if [[ ! -f "${SDK_LIBRARY}" ]]; then
    echo "SDK library not found: ${SDK_LIBRARY}" >&2
    exit 1
fi

if [[ ! -f "${TEST_SOURCE}" ]]; then
    echo "Test source not found: ${TEST_SOURCE}" >&2
    exit 1
fi

if [[ ! -x "${MEASUREMENT_SCRIPT}" ]]; then
    echo "Measurement script is not executable: ${MEASUREMENT_SCRIPT}" >&2
    exit 1
fi

readonly BACKUP_DIR="$(mktemp -d)"
readonly SDK_LIBRARY_BACKUP="${BACKUP_DIR}/libcbcengine.so"
readonly TEST_BUILD_DIR="${BACKUP_DIR}/test"
readonly TEST_CBC="${TEST_BUILD_DIR}/main.cbc"
cp -p -- "${SDK_LIBRARY}" "${SDK_LIBRARY_BACKUP}"

original_compile_commands_target=""
if [[ -L "${COMPILE_COMMANDS}" ]]; then
    original_compile_commands_target="$(readlink -- "${COMPILE_COMMANDS}")"
fi

restore_sdk_library() {
    local status=$?
    trap - EXIT
    cp -p -- "${SDK_LIBRARY_BACKUP}" "${SDK_LIBRARY}"
    if [[ -n "${original_compile_commands_target}" ]]; then
        ln -sfn -- "${original_compile_commands_target}" "${COMPILE_COMMANDS}"
    fi
    rm -rf -- "${BACKUP_DIR}"
    exit "${status}"
}
trap restore_sdk_library EXIT

# shellcheck source=/dev/null
source "${CANGJIE_SDK_DIR}/envsetup.sh"

mkdir -p -- "${BUILD_ROOT}" "${RESULTS_DIR}" "${TEST_BUILD_DIR}"

echo "=== Compiling ${TEST_SOURCE} ===" >&2
cjc \
    --int-overflow wrapping \
    -Woff unused \
    --emit-chir \
    -o "${TEST_BUILD_DIR}/main.chir" \
    "${TEST_SOURCE}"

(
    cd "${TEST_BUILD_DIR}"
    java -jar "${COMPILER_JAR}" \
        -outputname=main \
        main.chir \
        "+IgnoreSourceFileNameForCbc"
)

for implementation in "${IMPLEMENTATIONS[@]}"; do
    build_dir="${BUILD_ROOT}/${implementation}"

    echo "=== Configuring ${implementation} ===" >&2
    cmake \
        -S "${ENGINE_DIR}" \
        -B "${build_dir}" \
        -DBUILD_TESTING=OFF \
        -DCMAKE_BUILD_TYPE="${BUILD_TYPE}" \
        -DCMAKE_TOOLCHAIN_FILE="${TOOLCHAIN_FILE}" \
        -DCBC_PROFILER_IMPLEMENTATION="${implementation}"

    echo "=== Building ${implementation} ===" >&2
    cmake --build "${build_dir}" --target cbcengine --parallel "${JOBS}"

    cp -- "${build_dir}/libcbcengine.so" "${SDK_LIBRARY}"

    echo "=== Measuring ${implementation} ===" >&2
    CBC_FILE="${TEST_CBC}" "${MEASUREMENT_SCRIPT}"

    cp -- "${SCRIPT_DIR}/profiler_true.txt" "${RESULTS_DIR}/${implementation}_true.txt"
    cp -- "${SCRIPT_DIR}/profiler_false.txt" "${RESULTS_DIR}/${implementation}_false.txt"
done

echo "=== Results ==="
"${ANALYZER}" \
    "${RESULTS_DIR}/profiler_false.txt" \
    "${RESULTS_DIR}/profiler_true.txt" \
    "${RESULTS_DIR}/double_buffer_false.txt" \
    "${RESULTS_DIR}/double_buffer_true.txt" \
    "${RESULTS_DIR}/atomic_false.txt" \
    "${RESULTS_DIR}/atomic_true.txt"

echo "Raw results: ${RESULTS_DIR}" >&2
