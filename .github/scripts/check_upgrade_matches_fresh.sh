#!/bin/bash
# Checks that a database that reached the current version by upgrading is the
# same as one that reached it with CREATE EXTENSION.
#
# The upgrade scripts are handwritten, and they carry their own copies of the
# function bodies in sql/function and sql/trigger. Nothing used to compare the
# two. A copy that falls behind its source is invisible: both databases report
# the same extversion, the fresh install is the one that gets tested, and the
# upgraded one quietly keeps running the previous release's code until some
# behaviour nobody connected to the upgrade turns out to be wrong.
#
# Older releases are installed from their tags, so this needs the repository's
# history and tags, not just a tarball of the working tree. It also needs to
# write into the extension directory, and the extension already installed.
set -eu

cd "$(dirname "$0")/../.."
. .github/scripts/lib/compare_extension.sh

PSQL="psql -v ON_ERROR_STOP=1 -X -q"
VERSION=$(sed -n "s/^default_version *= *'\([^']*\)'.*/\1/p" pg_statecharts/pg_statecharts.control)
EXTDIR="$(pg_config --sharedir)/extension"

# Under GitHub Actions the checkout is mounted into the job's container and is
# not owned by the user the step runs as, which git refuses to read without
# being told. Said per invocation rather than written into whoever runs this
# locally.
GIT="git -c safe.directory=$PWD"

WORK=$(mktemp -d)
INSTALLED=""
cleanup() {
  rm -rf "$WORK"
  # Leave the extension directory as it was found, so that a later step in the
  # same job does not see a version this script put there.
  for f in $INSTALLED; do rm -f "$f"; done
}
trap cleanup EXIT

# Which releases can be installed from scratch and then upgraded.
#
# 0.0.0 is excluded: it was the pgrx implementation, and its install script
# creates C functions backed by a pg_statecharts.so that this repository no
# longer builds. The route out of that era is the sqitch adoption, which
# check_sqitch_adoption.sh covers and which ends up on the same upgrade path
# from 0.0.0 onwards.
OLD_VERSIONS=$($GIT tag --list 'v*' \
  | sed -n 's/^v\([0-9]\+\.[0-9]\+\.[0-9]\+\)$/\1/p' \
  | grep -vxF '0.0.0' \
  | grep -vxF "$VERSION" \
  | sort -V)

if [ -z "$OLD_VERSIONS" ]; then
  echo "error: no released version to upgrade from; are the tags fetched?" >&2
  exit 1
fi

echo "--- installing a fresh $VERSION ---"
$PSQL -c "drop database if exists upgrade_fresh"
$PSQL -c "create database upgrade_fresh"
$PSQL -d upgrade_fresh -c "create extension pg_statecharts cascade"

for old in $OLD_VERSIONS; do
  echo "--- building $old from its tag ---"
  mkdir -p "$WORK/$old"
  $GIT archive "v$old" pg_statecharts | tar -x -C "$WORK/$old"
  make -C "$WORK/$old/pg_statecharts" >/dev/null

  # Only the install script. The control file and the upgrade scripts stay the
  # ones this working tree ships, which is the point: an old database is being
  # brought forward by today's upgrade path.
  cp "$WORK/$old/pg_statecharts/pg_statecharts--$old.sql" "$EXTDIR/"
  INSTALLED="$INSTALLED $EXTDIR/pg_statecharts--$old.sql"

  echo "--- installing $old and upgrading it to $VERSION ---"
  db="upgrade_from_${old//./_}"
  $PSQL -c "drop database if exists $db"
  $PSQL -c "create database $db"
  $PSQL -d "$db" -c "create extension pg_statecharts version '$old' cascade"
  $PSQL -d "$db" -c "alter extension pg_statecharts update"

  at=$($PSQL -tA -d "$db" -c "select extversion from pg_extension where extname = 'pg_statecharts'")
  if [ "$at" != "$VERSION" ]; then
    echo "error: updating from $old left the extension at $at, not $VERSION" >&2
    exit 1
  fi

  compare_extension upgrade_fresh "$db" "a database upgraded from $old"
done

echo "all upgrade paths end up where a fresh install does"
