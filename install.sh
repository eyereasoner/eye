#!/bin/bash
# --------------------------
# installing EYE in /opt/eye
# --------------------------
SCRIPT_DIR=$( cd "$( dirname "$0" )" && pwd )
mkdir -p /opt/eye/src
cp -a $SCRIPT_DIR/eye.prolog /opt/eye/src
mkdir -p /opt/eye/lib
pushd /opt/eye/lib
swipl -q -f ../src/eye.prolog -g main -- --image eye.pvm
popd
mkdir -p /opt/eye/bin
cp -a $SCRIPT_DIR/eye.sh /opt/eye/bin
