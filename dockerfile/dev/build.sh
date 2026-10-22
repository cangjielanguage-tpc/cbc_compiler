#!/bin/bash
set -euxo pipefail

usage() {
    echo "Usage: ./build.sh <arch>"
    echo "  arch: x86_64, x64, amd64, aarch64, arm64"
    echo "Builds the dev image for the given architecture and pushes it along with"
    echo "the multi-arch manifest list, e.g.:"
    echo "  ./build.sh amd64"
    echo "  ./build.sh arm64"
}

INPUT_ARCH=$(echo "${1:-}" | tr '[:upper:]' '[:lower:]')

case "$INPUT_ARCH" in
  amd64|x86_64|x64)
    ARCH="amd64"
    ;;
  arm64|aarch64)
    ARCH="arm64"
    ;;
  *)
    usage >&2
    echo "Error: Unsupported architecture '${INPUT_ARCH}'." >&2
    exit 1
    ;;
esac

REGISTRY="s3.excelsior.internal:5000"
IMAGE_NAME="cbc-compiler-dev"
IMAGE_TAG="v1"
SHORT_TAG="${REGISTRY}/${IMAGE_NAME}:${IMAGE_TAG}"
TAG="${SHORT_TAG}-${ARCH}"

# The build context is the repository root; the Dockerfile picks the sbt build
# definition and sources from it to pre-populate the dependency caches (see
# .dockerignore for what is actually sent).
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CONTEXT="$(cd "$SCRIPT_DIR/../.." && pwd)"

podman build --network=host --pull --no-cache \
  $(xargs -I{} echo --build-arg {} < "$SCRIPT_DIR/versions.env") \
  --build-arg=TARGETARCH=${ARCH} \
  -f "$SCRIPT_DIR/Dockerfile" -t "$TAG" --platform "linux/${ARCH}" "$CONTEXT"
podman push "$TAG"

# Create manifest list if it doesn't exist, or add the image to it
if ! podman manifest inspect "$SHORT_TAG" >/dev/null 2>&1; then
  podman manifest create "$SHORT_TAG" "$TAG"
else
  podman manifest add "$SHORT_TAG" "$TAG"
fi

podman push "$SHORT_TAG"
