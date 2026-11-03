# cbc-compiler: jar vs jar+AppCDS vs GraalVM native image

Full standalone testsuite, `runner.py -j 1` (single-threaded), measured pass after a discarded
warmup pass per config. Averages of two full runs (2026-11-02, idle machine, 20 cores,
OpenJDK 21.0.12.1). Toolchain: `hotfix-sdk.tar.gz` @ `01a18c21-9421-741d-a3ee-6a61f4d98c90`.
Zero test failures in all 6 passes. Raw logs: `bench-results/*-full1`, `bench-results/*-full2`.

| Phase | jar | jar-cds | native | noise | cds vs jar | native vs jar |
|---|---:|---:|---:|---:|---:|---:|
| cbc-compiler — CHIR→CBC (431 calls) | 920.0 s | 478.1 s | 150.8 s | 0.2% | 1.92x | 6.10x |
| cbc-asm — assembler (46 calls) | 14.6 s | 14.8 s | 4.5 s | 4.8% | 1.0x | 3.24x |
| cjc — CHIR emission (583 calls) | 103.2 s | 102.4 s | 99.8 s | 1.0% | 1.01x | 1.03x |
| launcher — test execution (477 calls) | 88.4 s | 87.7 s | 87.9 s | 1.7% | 1.01x | 1.01x |
| diff + cc (484 calls) | 1.9 s | 1.9 s | 1.9 s | — | — | — |
| **build chunk total (1067 cmds)** | **1038.3 s** | **595.7 s** | **255.5 s** | 0.1% | **1.74x** | **4.06x** |
| **run chunk total (954 cmds)** | **89.9 s** | **89.1 s** | **89.4 s** | 1.7% | 1.01x | 1.01x |
| wall total (incl. runner overhead) | 1128.5 s | 685.1 s | 345.3 s | 0.1% | 1.65x | 3.27x |

Notes:

- Configs switched by rotating files in `toolchain/cangjie/tools/bin` only (see
  `bench-compiler-modes.sh`): jar = plain `java -jar`; jar-cds = `USE_JAVA_STARTUP_OPTIMIZATION=1`
  (C1-only JIT + auto-created AppCDS archive, pre-warmed); native = wrapper execs the
  md5-validated `<jar>-native` image.
- cbc-compiler per-call average: jar 2.14 s → jar-cds 1.11 s → native 0.35 s — the win is
  JVM startup/JIT elimination on short-lived processes.
- cjc and launcher are control groups (same binaries in all configs, deltas ≤1%).
- cbc-asm has no CDS branch in its wrapper, hence jar ≈ jar-cds there.
- Run-to-run noise: 0.1–4.8% (worst: cbc-asm native, 4.5 s bucket), well under 5–10% tolerance.
