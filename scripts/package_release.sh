#!/bin/bash
# Builds the release artifacts.
#
# Both extensions are pure SQL, so there is nothing to compile and nothing that
# varies by architecture or operating system. That leaves:
#
#   pg_statecharts-<version>.tar.gz       both extensions, with an install.sh
#                                         that installs the runtime and, given
#                                         --dev, the dev tooling too. Works on
#                                         any PostgreSQL, anywhere.
#   pg-statecharts-<PG>_<version>_all.deb the runtime extension, one per
#                                         PostgreSQL major version, only because
#                                         Debian puts extensions under a
#                                         versioned path. Architecture: all.
#   pg-statecharts-dev-<PG>_<version>_all.deb
#                                         the dev tooling, depending on the
#                                         runtime package of the same version.
#
# The dev tooling reads and writes files on the database host, which is why it
# is a separate package: production servers install the runtime package and
# never have the dev files on disk at all.
#
# Usage: scripts/package_release.sh [outdir]
set -euo pipefail

cd "$(dirname "$0")/.."

OUT=${1:-dist}
VERSION=$(sed -n "s/^default_version *= *'\([^']*\)'.*/\1/p" pg_statecharts/pg_statecharts.control)
DEV_VERSION=$(sed -n "s/^default_version *= *'\([^']*\)'.*/\1/p" pg_statecharts_dev/pg_statecharts_dev.control)

if [ "$VERSION" != "$DEV_VERSION" ]; then
  echo "error: pg_statecharts is $VERSION but pg_statecharts_dev is $DEV_VERSION" >&2
  echo "The two are released together and must carry the same version." >&2
  exit 1
fi

# Debian package names cannot contain underscores in the version, and sqitch
# style versions are already fine, but be explicit about what we support.
case "$VERSION" in
  [0-9]*.[0-9]*.[0-9]*) ;;
  *) echo "error: '$VERSION' does not look like a release version" >&2; exit 1 ;;
esac

rm -rf "$OUT"
mkdir -p "$OUT"

echo "packaging pg_statecharts $VERSION"

# ---------------------------------------------------------------- tarball ---
STAGE=$(mktemp -d)
trap 'rm -rf "$STAGE"' EXIT

PKG="$STAGE/pg_statecharts-$VERSION"
mkdir -p "$PKG"

for ext in pg_statecharts pg_statecharts_dev; do
  mkdir -p "$PKG/$ext"
  cp -r "$ext/sql" "$PKG/$ext/"
  cp "$ext/$ext.control" "$ext/install.sh" "$ext/README.md" "$PKG/$ext/"
done
cp README.md LICENSE "$PKG/" 2>/dev/null || cp README.md "$PKG/"

cat > "$PKG/install.sh" <<'INSTALLER'
#!/bin/sh
# Installs pg_statecharts, the runtime extension. Pass --dev to also install
# pg_statecharts_dev, the tooling for turning .scxml files into statecharts and
# migrations. It reads and writes files on the database host, so leave it out
# on production servers.
#
#   ./install.sh                                      runtime only
#   ./install.sh --dev                                runtime and dev tooling
#   ./install.sh /usr/lib/postgresql/18/bin/pg_config a specific PostgreSQL
#   ./install.sh --dev /usr/lib/postgresql/18/bin/pg_config
set -eu
cd "$(dirname "$0")"

dev=false
if [ "${1:-}" = "--dev" ]; then
  dev=true
  shift
fi

./pg_statecharts/install.sh "$@"
if [ "$dev" = true ]; then
  ./pg_statecharts_dev/install.sh "$@"
fi
INSTALLER
chmod +x "$PKG/install.sh"

tar -czf "$OUT/pg_statecharts-$VERSION.tar.gz" -C "$STAGE" "pg_statecharts-$VERSION"
echo "  $OUT/pg_statecharts-$VERSION.tar.gz"

# ---------------------------------------------------------------- debs -----
# Only build these where dpkg-deb exists; the tarball is the portable artifact.
if ! command -v dpkg-deb >/dev/null 2>&1; then
  echo "dpkg-deb not found, skipping .deb packages"
  exit 0
fi

# build_deb <extension dir> <debian package name> <PG major> <control body>
#
# install.sh is the single implementation of "assemble and lay out the
# extension files"; DEST points it at the staging tree instead of at a real
# PostgreSQL, so the packaged files cannot drift from the installed ones.
build_deb() {
  local ext=$1 name=$2 pg=$3 control=$4
  local deb="$STAGE/$name"
  local share="$deb/usr/share/postgresql/$pg/extension"

  mkdir -p "$deb/DEBIAN" "$share"
  DEST="$share" "./$ext/install.sh" >/dev/null

  printf '%s\n' "$control" > "$deb/DEBIAN/control"

  dpkg-deb --build --root-owner-group "$deb" "$OUT/${name}_${VERSION}_all.deb" >/dev/null
  echo "  $OUT/${name}_${VERSION}_all.deb"
}

# 0.0.0, the Rust build, was packaged as plain "pg-statecharts" and installed
# the same control file. Conflicts + Replaces is the dpkg idiom for "this
# package takes over that one": a plain dpkg -i removes the old package,
# including its .so, and leaves only the 0.1.0 files. Breaks would refuse to
# install, and Replaces alone would leave both packages installed.
for PG in 16 17 18; do
  build_deb pg_statecharts "pg-statecharts-$PG" "$PG" "\
Package: pg-statecharts-$PG
Version: $VERSION
Architecture: all
Maintainer: Kronor <dev@kronor.io>
Depends: postgresql-$PG
Conflicts: pg-statecharts
Replaces: pg-statecharts
Section: database
Priority: optional
Description: Statecharts (hierarchical state machines) in PostgreSQL
 Tables, types, triggers and functions for defining and running statecharts
 inside PostgreSQL. This is the runtime half, the one to install on production
 servers; pg-statecharts-dev-$PG holds the development tooling.
 .
 Implemented entirely in SQL and PL/pgSQL, so the package is architecture
 independent."

  # The generated migrations must match the runtime they target, and the two
  # extensions are released together anyway, so pin the exact version.
  build_deb pg_statecharts_dev "pg-statecharts-dev-$PG" "$PG" "\
Package: pg-statecharts-dev-$PG
Version: $VERSION
Architecture: all
Maintainer: Kronor <dev@kronor.io>
Depends: pg-statecharts-$PG (= $VERSION)
Section: database
Priority: optional
Description: Development tooling for pg_statecharts
 Imports .scxml files as statecharts and generates sqitch migrations from
 them. Both read files on the database host and one writes them, which is
 why this is a separate package: install it on development machines, not on
 production servers.
 .
 Implemented entirely in SQL and PL/pgSQL, so the package is architecture
 independent."
done
