#!/bin/bash
# Builds the release artifacts.
#
# Both extensions are pure SQL, so there is nothing to compile and nothing that
# varies by architecture or operating system. That leaves:
#
#   pg_statecharts-<version>.tar.gz   everything, with install.sh. Works on any
#                                     PostgreSQL, anywhere.
#   pg-statecharts_<version>_all.deb  one per PostgreSQL major version, only
#                                     because Debian puts extensions under a
#                                     versioned path. Architecture: all.
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
# Installs both extensions. Pass a pg_config path to target a specific
# PostgreSQL, e.g. ./install.sh /usr/lib/postgresql/18/bin/pg_config
set -eu
cd "$(dirname "$0")"
./pg_statecharts/install.sh "$@"
./pg_statecharts_dev/install.sh "$@"
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

for PG in 16 17 18; do
  DEB="$STAGE/deb$PG"
  SHARE="$DEB/usr/share/postgresql/$PG/extension"
  mkdir -p "$DEB/DEBIAN" "$SHARE"

  # install.sh is the single implementation of "assemble and lay out the
  # extension files"; DEST points it at the staging tree instead of at a real
  # PostgreSQL, so the packaged files cannot drift from the installed ones.
  for ext in pg_statecharts pg_statecharts_dev; do
    DEST="$SHARE" "./$ext/install.sh" >/dev/null
  done

  cat > "$DEB/DEBIAN/control" <<CONTROL
Package: pg-statecharts-$PG
Version: $VERSION
Architecture: all
Maintainer: Kronor <dev@kronor.io>
Depends: postgresql-$PG
Section: database
Priority: optional
Description: Statecharts (hierarchical state machines) in PostgreSQL
 Tables, types, triggers and functions for defining and running statecharts
 inside PostgreSQL, plus optional development tooling for importing .scxml
 files and generating sqitch migrations.
 .
 Implemented entirely in SQL and PL/pgSQL, so the package is architecture
 independent.
CONTROL

  dpkg-deb --build --root-owner-group "$DEB" "$OUT/pg-statecharts-${PG}_${VERSION}_all.deb" >/dev/null
  echo "  $OUT/pg-statecharts-${PG}_${VERSION}_all.deb"
done
