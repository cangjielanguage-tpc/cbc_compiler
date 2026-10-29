#!/usr/bin/env bash

set -euo pipefail

readonly RUNS=10
readonly SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
readonly REGULAR_CBC="${REGULAR_CBC:-${SCRIPT_DIR}/regular/default.cbc}"
readonly ABS_INLINED_CBC="${ABS_INLINED_CBC:-${SCRIPT_DIR}/abs-inlined/default.cbc}"
readonly FULLY_INLINED_CBC="${FULLY_INLINED_CBC:-${SCRIPT_DIR}/abs-compute-inlined/default.cbc}"
readonly LAUNCHER_BIN="${LAUNCHER:-launcher}"

for cbc_file in \
    "${REGULAR_CBC}" \
    "${ABS_INLINED_CBC}" \
    "${FULLY_INLINED_CBC}"; do
    if [[ ! -f "${cbc_file}" ]]; then
        echo "CBC file not found: ${cbc_file}" >&2
        exit 1
    fi
done

if ! command -v "${LAUNCHER_BIN}" >/dev/null 2>&1; then
    echo "Launcher not found: ${LAUNCHER_BIN}" >&2
    echo "Source cangjie/envsetup.sh or set LAUNCHER to an absolute path." >&2
    exit 1
fi

run_once() {
    local name="$1"
    local cbc_file="$2"
    local elapsed

    elapsed="$("${LAUNCHER_BIN}" "${cbc_file}")"
    if [[ ! "${elapsed}" =~ ^[0-9]+$ ]]; then
        echo "Expected one integer from ${name}, got: ${elapsed}" >&2
        exit 1
    fi

    printf '%s' "${elapsed}"
}

regular_total=0
abs_inlined_total=0
fully_inlined_total=0

for ((run = 1; run <= RUNS; ++run)); do
    regular_time="$(run_once regular "${REGULAR_CBC}")"
    regular_total=$((regular_total + regular_time))
    printf 'run %2d/%d  regular:           %s ms\n' "${run}" "${RUNS}" "${regular_time}"

    abs_inlined_time="$(run_once abs-inlined "${ABS_INLINED_CBC}")"
    abs_inlined_total=$((abs_inlined_total + abs_inlined_time))
    printf 'run %2d/%d  abs-inlined:       %s ms\n' "${run}" "${RUNS}" "${abs_inlined_time}"

    fully_inlined_time="$(run_once fully-inlined "${FULLY_INLINED_CBC}")"
    fully_inlined_total=$((fully_inlined_total + fully_inlined_time))
    printf 'run %2d/%d  fully-inlined:     %s ms\n' "${run}" "${RUNS}" "${fully_inlined_time}"
done

regular_average="$(awk -v total="${regular_total}" -v runs="${RUNS}" \
    'BEGIN { printf "%.2f", total / runs }')"
abs_inlined_average="$(awk -v total="${abs_inlined_total}" -v runs="${RUNS}" \
    'BEGIN { printf "%.2f", total / runs }')"
fully_inlined_average="$(awk -v total="${fully_inlined_total}" -v runs="${RUNS}" \
    'BEGIN { printf "%.2f", total / runs }')"

printf '\naverage regular:       %s ms\n' "${regular_average}"
printf 'average abs-inlined:   %s ms\n' "${abs_inlined_average}"
printf 'average fully-inlined: %s ms\n' "${fully_inlined_average}"
