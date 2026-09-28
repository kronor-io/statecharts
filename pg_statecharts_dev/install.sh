#!/bin/sh
# Installs the extension without needing make, a compiler or the PostgreSQL
# server headers. The extension is pure SQL, so "building" it is concatenating
# the files in sql/parts.txt and "installing" it is copying the result next to
# the control file in PostgreSQL's extension directory.
#
# The same files work on every PostgreSQL version, architecture and operating
# system.
#
#   ./install.sh                      install for the pg_config on $PATH
#   ./install.sh /path/to/pg_config   install for a specific PostgreSQL
#
# Set DEST to write the files somewhere else entirely, which is how the release
# packaging stages them without needing a PostgreSQL to point at:
#
#   DEST=/tmp/stage ./install.sh
#
set -eu

cd "$(dirname "$0")"

# The control file names the extension, so a renamed checkout still works and
# this script stays identical for both extensions.
CONTROL=
for f in ./*.control; do CONTROL=$f; done
if [ ! -f "$CONTROL" ]; then
  echo "error: no .control file next to $0" >&2
  exit 1
fi
EXTENSION=$(basename "$CONTROL" .control)

PG_CONFIG=${1:-pg_config}
VERSION=$(sed -n "s/^default_version *= *'\([^']*\)'.*/\1/p" "$CONTROL")

if [ -z "${DEST:-}" ]; then
  if ! command -v "$PG_CONFIG" >/dev/null 2>&1; then
    echo "error: $PG_CONFIG not found." >&2
    echo "Pass the path to pg_config as the first argument, e.g." >&2
    echo "  ./install.sh /usr/lib/postgresql/18/bin/pg_config" >&2
    exit 1
  fi

  DEST="$("$PG_CONFIG" --sharedir)/extension"
fi

if [ ! -d "$DEST" ]; then
  echo "error: extension directory $DEST does not exist" >&2
  exit 1
fi

if [ ! -w "$DEST" ]; then
  echo "error: cannot write to $DEST -- try again with sudo" >&2
  exit 1
fi

BUILT=$(mktemp)
trap 'rm -f "$BUILT"' EXIT

printf '%s\n' "\\echo Use \"CREATE EXTENSION $EXTENSION\" to load this file. \\quit" > "$BUILT"
printf '%s\n' "-- $EXTENSION $VERSION" >> "$BUILT"
printf '%s\n' '-- GENERATED FILE, DO NOT EDIT. Concatenated from the files in sql/parts.txt.' >> "$BUILT"

while IFS= read -r part; do
  [ -n "$part" ] || continue
  printf '\n-- %s\n' "$part" >> "$BUILT"
  cat "$part" >> "$BUILT"
done < sql/parts.txt

install -m 644 "$BUILT" "$DEST/$EXTENSION--$VERSION.sql"
install -m 644 "$EXTENSION.control" "$DEST/$EXTENSION.control"

for upgrade in sql/upgrade/*.sql; do
  [ -e "$upgrade" ] || continue
  install -m 644 "$upgrade" "$DEST/$(basename "$upgrade")"
done

echo "installed $EXTENSION $VERSION into $DEST"
