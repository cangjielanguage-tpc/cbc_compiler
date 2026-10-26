# Optional dependency: flatbuffers
FROM docker.io/ubuntu:18.04 AS flatbuffers-builder

ARG DEBIAN_FRONTEND=noninteractive
ARG FLATBUFFERS_VERSION

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates \
      git \
      cmake \
      make \
      g++ && \
    rm -rf /var/lib/apt/lists/*

RUN git clone --branch "${FLATBUFFERS_VERSION}" --depth 1 https://github.com/google/flatbuffers.git /tmp/flatbuffers && \
    mkdir /tmp/flatbuffers/build && cd /tmp/flatbuffers/build && \
    cmake .. -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release && \
    make -j"$(nproc)" flatc && \
    cp /tmp/flatbuffers/build/flatc /usr/local/bin/flatc

# Optional dependency: graalvm
FROM docker.io/ubuntu:18.04 AS graalvm-builder

ARG DEBIAN_FRONTEND=noninteractive
ARG GRAALVM_VERSION
ARG TARGETARCH

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates \
      curl && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p /opt/graalvm && \
    case "${TARGETARCH}" in \
      amd64) graalvm_arch="x64" ;; \
      arm64) graalvm_arch="aarch64" ;; \
      *) echo "Error: Unsupported architecture '${TARGETARCH}'." >&2; exit 1 ;; \
    esac && \
    i="$(echo "${GRAALVM_VERSION}" | cut -d. -f2)" && \
    jdk="25.0.${GRAALVM_VERSION#*.*.}" && \
    (curl -fsSL "https://github.com/graalvm/graalvm-ce-builds/releases/download/graal-${GRAALVM_VERSION}/graalvm-community-jdk-25i${i}-${jdk}_linux-${graalvm_arch}_bin.tar.gz" | \
    tar -xz -C /opt/graalvm --strip-components=1 --no-same-owner || true) && \
    chmod +x /opt/graalvm/bin/* && \
    rm -rf /opt/graalvm/lib/src.zip /opt/graalvm/jmods && \
    /opt/graalvm/bin/native-image --version

# cbc-compiler-dev
FROM docker.io/ubuntu:18.04

ARG DEBIAN_FRONTEND=noninteractive
ARG TARGETARCH
ARG MUSL_TOOLCHAIN_VERSION

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates \
      curl \
      wget \
      git \
      gnupg \
      unzip \
      zip \
      jq \
      xz-utils \
      python3 \
      build-essential \
      zlib1g-dev \
      gcc \
      openjdk-11-jdk && \
    rm -rf /var/lib/apt/lists/*

# GraalVM's official musl toolchain for --static --libc=musl (bundles musl libc,
# libz.a and a modern gcc/binutils; bionic's binutils 2.30 is too old to link
# GraalVM 25's musl static archives). Needed on amd64 only: GraalVM does not
# support --libc=musl on aarch64 and Build.scala falls back to --static-nolibc there.
# https://www.graalvm.org/latest/reference-manual/native-image/guides/build-static-executables/
RUN if [ "${TARGETARCH}" = "amd64" ]; then \
      curl -fsSL "https://gds.oracle.com/download/bfs/archive/musl-toolchain-${MUSL_TOOLCHAIN_VERSION}-linux-amd64.tar.gz" | \
      tar -xz -C /opt --no-same-owner && \
      /opt/musl-toolchain/bin/x86_64-linux-musl-gcc --version; \
    fi

# sbt 1.12.8, pinned to match project/build.properties (https://www.scala-sbt.org/download/)
RUN curl -fsSL "https://github.com/sbt/sbt/releases/download/v1.12.8/sbt-1.12.8.tgz" | tar -xz -C /usr/local && \
    ln -s /usr/local/sbt/bin/sbt /usr/local/bin/sbt && \
    sbt --script-version

COPY --from=flatbuffers-builder /usr/local/bin/flatc /usr/local/bin/flatc
COPY --from=graalvm-builder     /opt/graalvm/        /opt/graalvm/

ENV GRAALVM_HOME=/opt/graalvm
ENV PATH="/opt/musl-toolchain/bin:${PATH}"

# Pre-populate the sbt/coursier caches (~/.sbt, ~/.cache/coursier) with the
# project's build definition and dependencies so that container runs don't
# re-download them from Maven.
COPY build.sbt .sbtopts /tmp/warmup/
COPY project/ /tmp/warmup/project/
COPY core/ /tmp/warmup/core/
COPY scripts/ /tmp/warmup/scripts/
RUN cd /tmp/warmup && \
    sbt \
     -Darch="${TARGETARCH}" \
     -Dmode=work \
     -Dgraalvm=/opt/graalvm/bin/native-image \
     compiler/nativeImage && \
    rm -rf /tmp/warmup

WORKDIR /workspace

CMD ["/bin/bash"]
