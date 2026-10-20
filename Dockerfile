FROM ubuntu:26.04 AS flatbuffers-builder

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
    cmake -S /tmp/flatbuffers -B /tmp/flatbuffers/build -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release && \
    cmake --build /tmp/flatbuffers/build --parallel "4" && \
    cp /tmp/flatbuffers/build/flatc /usr/local/bin/flatc

FROM ubuntu:26.04

ARG DEBIAN_FRONTEND=noninteractive

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
      openjdk-21-jdk && \
    rm -rf /var/lib/apt/lists/*

COPY --from=flatbuffers-builder /usr/local/bin/flatc /usr/local/bin/flatc

RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | gpg --dearmor -o /etc/apt/keyrings/scalasbt.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/scalasbt.gpg] https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
      sbt && \
    rm -rf /var/lib/apt/lists/*

ARG GRAALVM_VERSION
ARG TARGETARCH
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

# Pre-populate the sbt/coursier caches (~/.sbt, ~/.cache/coursier) with the
# project's build definition and dependencies so that container runs don't
# re-download them from Maven.
COPY build.sbt .sbtopts /tmp/warmup/
COPY project/ /tmp/warmup/project/
COPY core/ /tmp/warmup/core/
RUN cd /tmp/warmup && \
    sbt -Darch="${TARGETARCH}" -Dmode=work -batch compiler/compile && \
    rm -rf /tmp/warmup

ENV GRAALVM_HOME=/opt/graalvm

WORKDIR /workspace

CMD ["/bin/bash"]
