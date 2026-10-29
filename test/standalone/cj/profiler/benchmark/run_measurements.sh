#!/usr/bin/env bash

set -euo pipefail

readonly RUNS=3
readonly SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
readonly CBC_FILE="${CBC_FILE:-${SCRIPT_DIR}/default.cbc}"
readonly PROFILER_ON_FILE="${SCRIPT_DIR}/profiler_true.txt"
readonly PROFILER_OFF_FILE="${SCRIPT_DIR}/profiler_false.txt"
readonly LAUNCHER_BIN="${LAUNCHER:-launcher}"

if [[ ! -f "${CBC_FILE}" ]]; then
    echo "CBC file not found: ${CBC_FILE}" >&2
    exit 1
fi

if ! command -v "${LAUNCHER_BIN}" >/dev/null 2>&1; then
    echo "Launcher not found: ${LAUNCHER_BIN}" >&2
    echo "Source envsetup.sh or set LAUNCHER to an absolute path." >&2
    exit 1
fi

run_series() {
    local profiler_enabled="$1"
    local output_file="$2"

    : >"${output_file}"

    for ((run = 1; run <= RUNS; ++run)); do
        echo "cbc.profiler=${profiler_enabled}: ${run}/${RUNS}" >&2

        local elapsed
        elapsed="$(CBCOPT="cbc.profiler=${profiler_enabled}" \
            "${LAUNCHER_BIN}" "${CBC_FILE}")"

        if [[ ! "${elapsed}" =~ ^[0-9]+$ ]]; then
            echo "Expected one integer on stdout, got: ${elapsed}" >&2
            exit 1
        fi

        printf '%s\n' "${elapsed}" >>"${output_file}"
    done
}

run_series true "${PROFILER_ON_FILE}"
run_series false "${PROFILER_OFF_FILE}"

echo "Results written to:" >&2
echo "  ${PROFILER_ON_FILE}" >&2
echo "  ${PROFILER_OFF_FILE}" >&2
