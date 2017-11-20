#!/bin/bash

TMPDIR=$1
SRCDIR=$2

mkdir -p $TMPDIR/usr/bin
mkdir -p $TMPDIR/etc/init
mkdir -p $TMPDIR/etc/paradox/
mkdir -p $TMPDIR/var/www/
test -r $SRCDIR/pkg/debian/upstart.conf && cp $SRCDIR/pkg/debian/upstart.conf $TMPDIR/etc/init/paradox.conf
cp $SRCDIR/pkg/config.json $TMPDIR/etc/paradox/config
cp $SRCDIR/dist/build/paradox/paradox $TMPDIR/usr/bin/paradox
cp -r $SRCDIR/files $TMPDIR/var/www/files
cp -r $SRCDIR/fonts $TMPDIR/var/www/files/.

cp $SRCDIR/pkg/debian/preinst $TMPDIR/../paradox.preinst

echo usr/bin/paradox
