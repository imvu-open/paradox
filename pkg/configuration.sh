#!/usr/bin/env bash

set -e
set -o pipefail

BASEDIR=$(cd "$(dirname "$0")"/..; pwd -P)
cd $BASEDIR

PROJECT_TAG="$1"
shift;
STACK_CMD="$@"

STAGEBASE=$BASEDIR/dist/staging
OUTBASE=$STAGEBASE/workdir-$RANDOM
REVISION=$(git log --pretty=oneline -1 | awk '{ print $1 }')

rm -rf $STAGEBASE

mk_tar() {
    cd $BASEDIR
    local OUTDIR=$1
    echo 'creating tarball '$OUTDIR
    mkdir -p $OUTDIR
    mkdir -p $OUTDIR/bin

    cp -R \
        files \
        $OUTDIR
    cp -R fonts $OUTDIR/files/.

    cp -R $($STACK_CMD path --local-install-root)/bin/paradox $OUTDIR/bin/paradox

    cd $OUTDIR

    tar cafv $STAGEBASE/${PROJECT_TAG}-$REVISION.tar.bz2 files bin
}

mk_tar $OUTBASE/prod

rm -rf $OUTBASE

