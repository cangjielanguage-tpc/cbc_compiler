#!/usr/bin/env python3

import math
import statistics
import sys
from pathlib import Path


EXPECTED_SAMPLES = 30
# Two-sided 95% Student's t critical value for 29 degrees of freedom.
T_CRITICAL_95 = 2.045229642132703


def read_samples(path: Path) -> list[float]:
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as error:
        raise SystemExit(f"Cannot read {path}: {error}") from error

    try:
        samples = [float(line.strip()) for line in lines if line.strip()]
    except ValueError as error:
        raise SystemExit(f"{path} contains a non-numeric value: {error}") from error

    if len(samples) != EXPECTED_SAMPLES:
        raise SystemExit(
            f"{path} contains {len(samples)} samples; expected {EXPECTED_SAMPLES}"
        )

    return samples


def summarize(path: Path) -> tuple[str, float, float, float]:
    samples = read_samples(path)
    total = math.fsum(samples)
    mean = statistics.mean(samples)
    margin = T_CRITICAL_95 * statistics.stdev(samples) / math.sqrt(len(samples))
    return path.name, total, mean, margin


def main() -> None:
    script_dir = Path(__file__).resolve().parent
    paths = (
        [Path(argument) for argument in sys.argv[1:]]
        if len(sys.argv) > 1
        else [script_dir / "profiler_true.txt", script_dir / "profiler_false.txt"]
    )
    rows = [summarize(path) for path in paths]

    headers = ("File", "Sum", "Mean", "95% CI for mean")
    formatted_rows = [
        (name, f"{total:.3f}", f"{mean:.3f}", f"{mean:.3f} ± {margin:.3f}")
        for name, total, mean, margin in rows
    ]
    widths = [
        max(len(headers[index]), *(len(row[index]) for row in formatted_rows))
        for index in range(len(headers))
    ]

    def print_row(row: tuple[str, ...]) -> None:
        print("  ".join(value.ljust(width) for value, width in zip(row, widths)))

    print_row(headers)
    print_row(tuple("-" * width for width in widths))
    for row in formatted_rows:
        print_row(row)


if __name__ == "__main__":
    main()
