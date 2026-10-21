#!/usr/bin/env bash

source "$TOOLCHAIN/envsetup.sh"

set -euo pipefail

[ "$#" -gt 0 ] || exit 0

################ Utilities ################
die() {
    echo "tool.sh: $*" >&2
    exit 1
}

md5_of() {
    if command -v md5sum > /dev/null 2>&1; then
        md5sum "$1" | cut -d' ' -f1
    else
        md5 -q "$1"
    fi
}

# Compares the md5 of `$1` with the `<base>-<kind>.md5` value recorded in the
# info file ($3); dies on mismatch or missing entry.
check_md5() {
    local file="$1" kind="$2" info="$3" base="$4"
    local key="$base-$kind.md5" k eq value
    value=""
    while read -r k eq value; do
        if [ "$k" = "$key" ]; then
            if [ "$(md5_of "$file")" != "$value" ]; then
                die "md5 mismatch: $base $kind $(md5_of "$file") != '$key' ($value) recorded in $info"
            fi
            return 0
        fi
    done < "$info"
    die "Cannot find '$key' entry in $info"
}

check_native_image_available() {
    local jar="$1" native="$2" info="$3" base

    [ -f "$jar" ] || die "$jar missing"

    # No info -> use jar
    [ -e "$info" ] || return 1

    [ -f "$native" ] || die "$native missing"
    [ -x "$native" ] || die "$native not executable"

    base="$(basename "$jar" .jar)"
    check_md5 "$jar" jar "$info" "$base"
    check_md5 "$native" native "$info" "$base"
    return 0
}
################ Utilities ################

args=("$@")
if [ "$(basename "${args[0]}")" = "java" ]; then
    # Intercepts `java [<jvm-opts>] -jar <xxx.jar> [<args>...]` invocations
    # replacing with GraalVM native image `xxx-native`
    # Validates that *-native and *-jar match using md5 checksums
    jar_idx=-1
    for i in "${!args[@]}"; do
        if [ "${args[$i]}" = "-jar" ]; then jar_idx=$((i + 1)); break; fi
    done

    if [ "$jar_idx" -ge 0 ]; then
        jar="${args[$jar_idx]}"
        rest=("${args[@]:$((jar_idx + 1))}")
        native="$(dirname "$jar")/$(basename "$jar" .jar)-native"
        info="$native.info"

        if check_native_image_available "$jar" "$native" "$info"; then
            exec "$native" "${rest[@]}"
        fi
    fi
fi

exec "${args[@]}"
