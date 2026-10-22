#!/bin/sh
set -eu

case "${1:-}" in
    amd64|arm64)
        platform="$1"
        # Remove the platform selector so that "$@" contains only compiler arguments.
        shift
        compiler_jar="cbc-compiler-$platform.jar"
        ;;
    *)
        compiler_jar="cbc-compiler.jar"
        ;;
esac

dir="$(dirname "$0")"
jar="$dir/$compiler_jar"
native="${jar%.jar}-native"
info="$native.info"
base="$(basename "$jar" .jar)"

die() {
    echo "cbc-compiler.sh: $*" >&2
    exit 1
}

md5_of() {
    if command -v md5sum > /dev/null 2>&1; then
        md5sum "$1" | cut -d' ' -f1
    else
        md5 -q "$1"
    fi
}

# Prints the value of the `<key> = <md5>` entry ($1) in the info file ($2);
# dies if the entry is missing.
info_md5() {
    key="$1"
    while read -r k eq value; do
        if [ "$k" = "$key" ]; then
            printf '%s\n' "$value"
            return 0
        fi
    done < "$2"
    die "Cannot find '$key' entry in $2"
}

# Intercepts its own invocation replacing `java -jar <cbc-compiler.jar>` with
# the GraalVM native image `<cbc-compiler>-native`, validating that the jar and
# the native image match using the md5 checksums recorded in `<...>-native.info`.
if [ -e "$info" ]; then
    [ -f "$jar" ] || die "$jar missing"
    [ -f "$native" ] || die "$native missing"
    [ -x "$native" ] || die "$native not executable"

    jar_md5="$(md5_of "$jar")"
    if [ "$jar_md5" != "$(info_md5 "$base-jar.md5" "$info")" ]; then
        die "md5 mismatch: $jar $jar_md5 != '$base-jar.md5' recorded in $info"
    fi

    native_md5="$(md5_of "$native")"
    if [ "$native_md5" != "$(info_md5 "$base-native.md5" "$info")" ]; then
        die "md5 mismatch: $native $native_md5 != '$base-native.md5' recorded in $info"
    fi

    exec "$native" "$@"
fi

# JVM startup optimization for repeated short-lived runs: C1-only JIT plus an
# auto-created AppCDS archive (<jar>.jsa) written next to the jar. Requires java 21+.
if [ "${USE_JAVA_STARTUP_OPTIMIZATION:-}" = "1" ]; then
    exec java \
        -XX:TieredStopAtLevel=1 \
        -XX:+AutoCreateSharedArchive \
        "-XX:SharedArchiveFile=$jar.jsa" \
        -jar "$jar" "$@"
fi

exec java -jar "$jar" "$@"
