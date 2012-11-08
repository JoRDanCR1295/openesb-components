#
#common definitions for packaging regression tests.
#warning - this file is shared for now between codegen and shell tests.
#statements are therefore limited to simple assignments.
#

#this are normally set by antbld/regress/common_defs.ksh:
SVC_LOC=packaging/release-package
SVC_ROOT=$SRCROOT/$SVC_LOC
SVC_REGRESS=$SVC_ROOT/regress
SVC_BLD=$SVC_ROOT/bld

#maven repo groupId for this project:
PRODUCT_GROUP_ID=open-jbi-components
VERSION_PROPS=$SRCROOT/bld/version.properties

#shared data files.  packaging00001 cleans and creates:
TESTDIR=../bld/testpackagedata
JARTOC_OUTPUT=$TESTDIR/jartoc.out
JARTOC_SUBSTITUTED=$TESTDIR/jartoc_substituted.out
JARTOC_REDUCED=$TESTDIR/jartoc_reduced.out

#where component jars live:
COMPONENTDIR=../bld/components

