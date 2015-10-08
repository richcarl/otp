#!/bin/bash

set -e

if [ -z "${RELEASE_NUMBER}" ]; then
    echo Error: RELEASE_NUMBER is required
    exit 1
fi

# Change to one level above the script directory, verify rpm directory exists
cd $(dirname ${BASH_SOURCE[0]})/.. 
test -d rpm 
RBHOME=`/bin/pwd`

# Set version and revision based on git describe output
# Example values (based on agreed Klarna OTP tag format of <OTP_VSN>+kred<N>)
#   GIT_DESCRIBE=16.3.1+kred5 => version=16.3.1 revision=kred5_3
#   GIT_DESCRIBE=16.3.1+kred5-7-g4747477 => version=16.3.1 revision=kred5_g4747477_3
# NB We remove the middle integer component number from the describe output

GIT_DESCRIBE=$(git describe --tags | sed -r 's/-[0-9]+(-g.{7})$/\1/')
DESCRIBE=$(echo $GIT_DESCRIBE | tr - _ | tr + -)
VERSION=$(echo $DESCRIBE | cut -d '-' -f1)
KLARNA_VERSION=$(echo $DESCRIBE | cut -d '-' -f2)
REVISION="${KLARNA_VERSION}_${RELEASE_NUMBER}"
GIT_REV=$(git log -n 1 '--pretty=%h')

mkdir -p rpmbuild/{SPECS,SOURCES}

# Create tarball
# FULLNAME must match what kred_erlang.spec looks for in its prep phase
FULLNAME=kred_erlang-${VERSION}-${REVISION}
git archive --format=tar --prefix=${FULLNAME}/ --output=rpmbuild/SOURCES/${FULLNAME}.tar HEAD
gzip -q rpmbuild/SOURCES/${FULLNAME}.tar

cp rpm/kred_erlang.spec rpmbuild/SPECS
cd rpmbuild/SPECS

RPMBUILD="env GZIP= HOME=${RBHOME} rpmbuild "
RPM_DEFINES="--define 'version ${VERSION}' --define 'revision ${REVISION}' --define 'git_rev ${GIT_REV}'"
eval $RPMBUILD -bb --clean ${RPM_DEFINES} kred_erlang.spec
eval $RPMBUILD -bb --clean --with developer ${RPM_DEFINES} kred_erlang.spec

cd ../..

rm -f rpmbuild/SPECS/* rpmbuild/SOURCES/*

echo done

