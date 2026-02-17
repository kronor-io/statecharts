#!/bin/bash

set -e

DEB_DIR=deb
VERSION=18

cargo pgrx package --pg-config $(cargo pgrx info pg-config $VERSION) --out-dir ./target/packaged

rm -r $DEB_DIR
mkdir -p $DEB_DIR/DEBIAN

echo "
Package: pg-statecharts
Version: 1.0.0
Architecture: $(dpkg --print-architecture)
Maintainer: Kronor <dev@kronor.io>
Description: Functionality for using statecharts in PostgreSQL
" > $DEB_DIR/DEBIAN/control

echo "2.0" > $DEB_DIR/debian-binary

LIB_DIR=$DEB_DIR/usr/lib/postgresql/$VERSION/lib/
SHARE_DIR=$DEB_DIR/usr/share/postgresql/$VERSION/extension/

mkdir -p $LIB_DIR $SHARE_DIR

cp target/packaged/home/axel/.pgrx/*/pgrx-install/lib/postgresql/* $LIB_DIR
cp target/packaged/home/axel/.pgrx/*/pgrx-install/share/postgresql/extension/* $SHARE_DIR

dpkg-deb --build $DEB_DIR pg_statecharts.deb
