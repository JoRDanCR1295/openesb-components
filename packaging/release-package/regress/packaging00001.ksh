#packaging00001 - check the manifest and jbi.xml  of each ojc installer artifact

testname=packaging00001

. ./regress_defs.ksh

rm -rf $TESTDIR
mkdir -p $TESTDIR

#the following command does, in sequence:
# 0.  pull out the manifest file (-m) and jbi.xml (-j) file if present, with verbose marking.
# 1.  translate CR to NL to get rid of CR's in manifests
# 2.  squeeze NL+ -> NL.  this ensures that jbi.xml files checked in with DOS EOL's will be standardized.
# 3.  eliminate tab characters to normalize output display in editors
# 4.  join ojc-artifact-url lines in manifest as they exceed 80 chars.
# $.  final cat is just to terminate the list of pipes.
#NOTE - we save the intermediate result for further testing.
jartoc -v -m -j $COMPONENTDIR/*.jar |\
    tr '\015' '\012' |\
    tr -s '\012' '\012' |\
    expand -4 |\
    sed -e :a -e '/^ojc-artifact-url:/N; s/\n *//' |\
    cat > $JARTOC_OUTPUT

#the following command does, in sequence:
# 1+. substitute variable data with standard tokens so we don't have to update the ref file all the time.
# $.  final cat is just to terminate the list of pipes.
cat $JARTOC_OUTPUT |\
    sed -e '/^Built-By:/s/:.*/: {=USER=}/' |\
    sed -e '/^Build-Jdk:/s/:.*/: {=JAVAVERS=}/' |\
    sed -e '/^ojc-artifact-url:/s/:.*/: {=ARTIFACT_URL=}/' |\
    sed -e '/^ojc-artifact-version:/s/:.*/: {=ARTIFACT_VERSION=}/' |\
    sed -e '/^ojc-build-number:/s/:.*/: {=BUILD_NUMBER=}/' |\
    sed -e '/^ojc-cvs-branch:/s/:.*/: {=CVS_BRANCH_NAME=}/' |\
    sed -e '/^ojc-cvs-timestamp:/s/:.*/: {=CVS_TIMESTAMP=}/' |\
    sed -e '/^ojc-release-version:/s/:.*/: {=RELEASE_VERSION=}/' |\
    sed -e '/<identification:VersionInfo/s/component-version=[^ ]* /component-version="{=RELEASE_VERSION=}" /' |\
    sed -e '/<identification:VersionInfo/s/build-number=[^\/]*\//build-number="{=BUILD_NUMBER=}"\//' |\
    cat > $JARTOC_SUBSTITUTED

##### Substitutions:
# <identification:VersionInfo component-version="{=RELEASE_VERSION=}" build-number="{=BUILD_NUMBER=}"/>
# Built-By: {=USER=}
# Build-Jdk: {=JAVAVERS=}
# Component-Name: aspectse-installer
# ojc-artifact-url: http://download.java.net/maven/esb/open-jbi-components/aspectse-installer/{=ARTIFACT_VERSION=}/aspectse-installer-{=ARTIFACT_VERSION=}.jar
# ojc-artifact-version: {=ARTIFACT_VERSION=}
# ojc-build-number: {=BUILD_NUMBER=}
# ojc-cvs-branch: {=CVS_BRANCH_NAME=}
# ojc-cvs-timestamp: {=CVS_TIMESTAMP=}
# ojc-cvsroot: :pserver:guest@cvs.dev.java.net:/cvs
# ojc-release-version: {=RELEASE_VERSION=}


#create the raw output for the next test:
egrep 'build-number=| ----|^Component-Name:|^ojc-|<name>' $JARTOC_OUTPUT > $JARTOC_REDUCED

egrep 'build-number=| ----|^Component-Name:|^ojc-|<name>' $JARTOC_SUBSTITUTED
exit $?
