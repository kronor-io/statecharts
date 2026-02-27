#!/bin/bash

set -e

DEB_DIR=deb
PG_VERSION=$(sed -n 's/^default *= *\["pg\([0-9]\+\)"\]$/\1/p' Cargo.toml)
VERSION=$(cat Cargo.toml | sed -n 's/^version *= *"\([0-9.]*\)"$/\1/p')

cargo pgrx package --pg-config $(cargo pgrx info pg-config $PG_VERSION) --out-dir ./target/packaged

rm -rf $DEB_DIR
mkdir -p $DEB_DIR/DEBIAN

echo "
Package: pg-statecharts
Version: $VERSION
Architecture: $(dpkg --print-architecture)
Maintainer: Kronor <dev@kronor.io>
Description: Functionality for using statecharts in PostgreSQL
" > $DEB_DIR/DEBIAN/control

echo "2.0" > $DEB_DIR/debian-binary

LIB_DIR=$DEB_DIR/usr/lib/postgresql/$PG_VERSION/lib/
SHARE_DIR=$DEB_DIR/usr/share/postgresql/$PG_VERSION/extension/

mkdir -p $LIB_DIR $SHARE_DIR

cp "$(find target/packaged/ -name pg_statecharts.so)" $LIB_DIR
cp "$(find target/packaged/ -name pg_statecharts--$VERSION.sql)" $SHARE_DIR
cp "$(find target/packaged/ -name pg_statecharts.control)" $SHARE_DIR

dpkg-deb --build $DEB_DIR pg_statecharts_"$VERSION"_pg"$PG_VERSION"_$(uname -s)_$(uname -m).deb
