#!/bin/sh
# Full build script

JV_SRCROOT=`dirname $0`
JV_SRCROOT=`realpath $JV_SRCROOT`
export JV_SRCROOT
mvn clean -Dmaven.repo.local=$JV_SRCROOT/m2/repository $*
mvn install -Dmaven.test.skip=true -e -Dmaven.repo.local=$JV_SRCROOT/m2/repository $*
