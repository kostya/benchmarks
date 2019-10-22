#!/bin/bash
# Usage: combinejars <destination> <jars...>
#
# Combines contents of jars into a single destination jar. The
# destination jar will have all it entries dates set to the unix
# epoch, hence making it reproducible for the same input.
#
# Contents at same paths are overwritten, the latter overwriting the earlier.

destination="$(realpath "$1")"
shift
sources=("$@")

workdir="$(mktemp -d)"

cleanup() {
    rm -rf "$workdir"
}
trap cleanup EXIT

for s in "${sources[@]}"; do
    absolute=$(realpath "$s")
    (cd "$workdir" || exit 1; unzip -uo "$absolute" > /dev/null)
done
find "$workdir" -exec touch -t 197001010000 {} \;
jar -cMf "$destination" -C "$workdir" .
