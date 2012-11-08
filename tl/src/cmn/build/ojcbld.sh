#!/bin/sh
#
# BEGIN_HEADER - DO NOT EDIT
#
# The contents of this file are subject to the terms
# of the Common Development and Distribution License
# (the "License").  You may not use this file except
# in compliance with the License.
#
# You can obtain a copy of the license at
# https://open-esb.dev.java.net/public/CDDLv1.0.html.
# See the License for the specific language governing
# permissions and limitations under the License.
#
# When distributing Covered Code, include this CDDL
# HEADER in each file and include the License file at
# https://open-esb.dev.java.net/public/CDDLv1.0.html.
# If applicable add the following below this CDDL HEADER,
# with the fields enclosed by brackets "[]" replaced with
# your own identifying information: Portions Copyright
# [year] [name of copyright owner]
#

#
# @(#)ojcbld.sh
#
# Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
#
# END_HEADER - DO NOT EDIT
#

# ojcBuild - production build script for ojc product
#
# Conventions:  Lowercase names are generally local vars (not exported).
#               Uppercase names are generally exported vars.
#


################################ USAGE ROUTINES ################################

usage()
{
    status=$1

    cat << EOF

Usage:  $p [options...] [var=def...] [-Dprop=val] [target(s)]

 Master build script for JBI products.

Options:
 -help           Display this message.
 -helpflow       Display the build stages.
 -test           Run all scripts in non-destructive test mode.
 -keepon         Keep running, even if compile fails. RELEASE DEFAULT
 -nokeepon       Don't run tests if it doesn't compile.  DEVELOPER DEFAULT.
 -clean          Do a clean build (call ant with clean options). DEFAULT.
 -noclean        Do not do a clean build.
 -update         Update (or create) working directory. DEFAULT FOR RELEASE BUILD.
 -tagsourcedate  Tag sources with current timestamp.  DEFAULT FOR RELEASE BUILD.
 -cleansrc       Clean out the source code before building.
                 Implies "-update" for development builds unless explicitly turned off.
 -cleanmavenrepo Remove maven local repository, and require build to redownload all maven dependencies
 -updatemaven    Update external dependencies in local maven repository.  DEFAULT IF -update
 -noupdatemaven  Do not update external dependencies in local maven repository.
                 (pass --offline option to all mvn commands.)
 -mavendeploy    Deploy maven artifacts to \$MAVEN_DISTROOT.
 -nomavendeploy  Do not Deploy maven artifacts. (DEFAULT)
 -mavenarchive   Archive local maven build repository. PRIMARY RELEASE DEFAULT
 -nomavenarchive Do not archive local maven build repository. DEVELOPER DEFAULT
 -noupdate       Do not checkout or update source code from CVS. DEVELOPER DEFAULT.
 -fast|-fastupdate
                 Short-hand for:  $p -update -noregress -nojavadoc
 -integreport    Create integration report. DEFAULT FOR RELEASE BUILD.
 -nointegreport  Do not create integration report. DEFAULT FOR DEVELOPER BUILD.
 -nobuild        Do not build.
 -javadoc        Create javadoc. DEFAULT FOR RELEASE BUILD.
 -nojavadoc      Do not create javadoc. DEVELOPER DEFAULT.
 -srcbundle      Create source-code bundles. DEFAULT FOR PRIMARY RELEASE BUILD.
 -nosrcbundle    Do not create source-code bundles. DEVELOPER DEFAULT.
 -xref           Create cross-referenced sources. DEFAULT FOR RELEASE BUILD.
 -noxref         Do not create cross-referenced sources. DEVELOPER DEFAULT.
 -archive_logs   Save test output with permanant log. DEFAULT.
 -noarchive_logs Do not save test output and servers logs.
 -review         Run the code review tool.
 -noreview       Do not run code review tool.
 -regress        Run all unit and system tests (DEFAULT).
 -noregress      Do not run any tests.
 -jregress       Run the jregress tests (DEFAULT)
 -nojregress     Do not run the jregress tests.
 -junit          Run the junit tests (DEFAULT)
 -nojunit        Do not run the junit tests.
 -release        Build all release kits.  DEFAULT FOR RELEASE BUILD.
 -norelease      Do not build any release kits.  DEVELOPER DEFAULT.
 -install        Build the installer.  DEFAULT FOR RELEASE BUILD.
 -noinstall      Do not build the installer.  DEVELOPER DEFAULT.
 -addons         Create and release the addon component installer bundle  DEFAULT FOR RELEASE BUILD.
 -noaddons       Do not create and release the addon component installer bundle.  DEVELOPER DEFAULT.
 -pushkit        Push kits to a location other than the usual kitroot.
 -nopushkit      Do not push kits to a location other than the usual kitroot (DEFAULT)
 -junitsystests  Run the junit system tests.
 -nojunitsystests Do not run the junit system tests. (DEFAULT)
 -verbose        Build with verbose messages.
 -bldnum num     use <num> for BLDNUM instead of generating a new one.
                 useful for re-releasing kits without a new build.
 var=def...      Substitute <def> for each <var>, and export to the environment.
 -Dprop=val...   Pass the given propery/value to the main build script.
 target(s)...    Build the given targets in the main build.xml file.

Environment:
 LOGDIR          Where to put all log files.
 LOCKDIR         Where to look for and write files used to communicate
                 completion of various build steps.

 KITROOT         The root of all product kits.
 KIT_DISTROOT    Where to distribute kits.  Normally set to \$KITROOT/\$PRODUCT

 DEVELOPER_BUILD Set this to 1 if you are building for development.

 RELEASE_BUILD   Set this to 1 if for official release builds only.
                 DO NOT SET THIS ENVIRONMENT VARIABLE IN A DEVELOPMENT PATH.

 INTEG_REPORT    Set to the name of script to create the integration report.

Developer Build Example:
 $p -update

Release Build Example:
 $p -bldnum \$BLDNUM

EOF

    exit $status
}

help_flow()
{
    cat << EOF
 Build flow is:
    1. clean
    2. update_src
    3. build_$PRODUCT
    4  BACKGROUND TASKS (in parallel with tests):
        4a. code_review
        4b. javadoc
        4c. make_release
        4d. make_installer
        4e. xref
        4f. archive_doc
    5. run_tests
    6. endbuild

EOF
}

parse_args()
{
    ### set option defaults according to builder profile.
    bld_set_builder_profile

    ARG_PROPS=
    BLDNUM_ARG=NULL
    CALLEDFROMWRAPPER=0
    DEBUG=0
    DOBUILD=0
    DOCLEAN=0
    DOCLEANSRC=0
    DOCODEREVIEW=0
    DOHELP=0
    DOINSTALL=0
    DOINTEGREPORT=0
    DOJAVADOC=0
    DOJREGRESS=0
    DOJUNIT=0
    DOREGRESS=0
    DOMAVENARCHIVE=0
    DOMAVENCLEAN=0
    DOMAVENDEPLOY=0
    DOMAVEN_UPDATE=0
    DORELEASE=0
    DOSRCBUNDLES=0
    DOUPDATE=0
    had_explicit_noupdate=0
    DOXREF=0
    KEEPON=0
    TESTMODE=0
    testarg=
    TAG_SOURCE_DATE=0
    VERBOSE_ARG=
    REGRESS_PRODUCT=ojc

    DOADDONS=0
    DOAPPSERVER_INSTALL=0
    DOARCHIVELOGS=0
    DOFIXCVSROOT=0
    DOJUNITSYSTESTS=0
    DOPUSHKIT=0
    INSTALLAS_ARGS=

    I_AM_PRIMARY=0
    if [ "$FORTE_PORT" = "$PRIMARY_PORT" ]; then
        I_AM_PRIMARY=1
    fi

    #
    #default for all options is 0, so only set options specific to the
    #three build profiles here:  developer, release slave build, release primary build.
    #

    if [ $I_AM_PRIMARY -eq 1 -a $RELEASE_BUILD -eq 1 ]; then
        #release primary build profile:
        DOCLEAN=1
        DOBUILD=1
        DOJREGRESS=1
        DOJUNIT=1
        DOREGRESS=1
        DOUPDATE=1
        DOMAVEN_UPDATE=1
        DOMAVENARCHIVE=1
        DOINTEGREPORT=1
        DORELEASE=1
        TAG_SOURCE_DATE=1
        DOSRCBUNDLES=1
        DOADDONS=1
    elif [ $RELEASE_BUILD -eq 1 ]; then
        #release slave build profile:
        DOCLEAN=1
        DOBUILD=1
        DOJREGRESS=1
        DOJUNIT=1
        DOREGRESS=1
        DOUPDATE=1
        DOMAVEN_UPDATE=1
        TAG_SOURCE_DATE=1
    else
        #developer build profile:
        DOCLEAN=1
        DOBUILD=1
        DOJREGRESS=1
        DOJUNIT=1
        DOREGRESS=1
        DOMAVEN_UPDATE=1
    fi

    while [ $# -gt 0 -a "$1" != "" ]
    do
        arg=$1; shift

        case $arg in
#ojc custom options follow:
        -addons )
            DOADDONS=1
            DORELEASE=1
            ;;
        -noaddons )
            DOADDONS=0
            ;;
        -pushkit )
            DOPUSHKIT=1
            ;;
        -nopushkit )
            DOPUSHKIT=0
            ;;
        -junitsystests )
            DOJUNITSYSTESTS=1
            ;;
        -nojunitsystests )
            DOJUNITSYSTESTS=0
            ;;
        -updateas|-updateappserver|-updateas8 )
            DOAPPSERVER_INSTALL=1
            ;;
        -noupdateas|-noupdateappserver|-noupdateas8 )
            DOAPPSERVER_INSTALL=0
            ;;
        -pe )
            INSTALLAS_ARGS="-pe"
            DOAPPSERVER_INSTALL=1
            ;;
        -ee )
            INSTALLAS_ARGS="-ee"
            DOAPPSERVER_INSTALL=1
            ;;
#end ojc custom options
#maven related options:
        -cleanmavenrepo )
            DOMAVENCLEAN=1
            DOMAVEN_UPDATE=1
            ;;
        -mavenarchive )
            DOMAVENARCHIVE=1
            ;;
        -nomavenarchive )
            DOPMAVENARCHIVE=0
            ;;
        -mavendeploy )
            DOMAVENDEPLOY=1
            ;;
        -nomavendeploy )
            DOMAVENDEPLOY=0
            ;;
        -updatemaven )
            DOMAVEN_UPDATE=1
            ;;
        -noupdatemaven )
            DOMAVEN_UPDATE=0
            ;;
#end of maven related options.
        -helpf* )
            help_flow
            exit 0
            ;;
        -h* )
            usage 0
            ;;
        -debug )
            DEBUG=1
            ;;
        -t* )
            TESTMODE=1
            testarg="-test"
            ;;
        -calledFromWrapper )
            #true if called from runojcBuild wrapper:
            CALLEDFROMWRAPPER=1
            ;;
        -clean )
            DOCLEAN=1
            ;;
        -noclean )
            DOCLEAN=0
            ;;
        -cleansrc )
            DOCLEANSRC=1
            DOUPDATE=1
            DOAPPSERVER_INSTALL=1
            ;;
        -bldnum )
            if [ $# -gt 0 ]; then
                BLDNUM_ARG=$1; shift
            else
                echo "${p}: -bldnum requires an argument"
                usage 1
            fi
            ;;
        -keepon )
            KEEPON=1
            ;;
        -nokeepon )
            KEEPON=0
            ;;
        -regress )
            #useful with -fast option.
            DOREGRESS=1
            DOJUNIT=1
            DOJREGRESS=1
            ;;
        -noregress )
            DOREGRESS=0
            DOJUNIT=0
            DOJREGRESS=0
            ;;
        -jregress )
            DOJREGRESS=1
            ;;
        -nojregress )
            DOJREGRESS=0
            ;;
        -junit )
            DOJUNIT=1
            ;;
        -nojunit )
            DOJUNIT=0
            ;;
        -javadoc )
            DOJAVADOC=1
            ;;
        -nojavadoc )
            DOJAVADOC=0
            ;;
        -archive_logs )
            DOARCHIVELOGS=1
            ;;
        -noarchive_logs )
            DOARCHIVELOGS=0
            ;;
        -xref )
            DOXREF=1
            ;;
        -noxref )
            DOXREF=0
            ;;
        -release )
            DORELEASE=1
            ;;
        -norelease )
            DORELEASE=0
            ;;
        -install )
            DOINSTALL=1
            ;;
        -noinstall )
            DOINSTALL=0
            ;;
        -srcbundle )
            DOSRCBUNDLES=1
            ;;
        -nosrcbundle )
            DOSRCBUNDLES=0
            ;;
        -nocodereview|-noreview )
            DOCODEREVIEW=0
            ;;
        -nobuild )
            DOBUILD=0
            #don't clean if we're not building
            DOCLEAN=0
            ;;
        -update )
            #update product sources:
            DOUPDATE=1
            DOMAVEN_UPDATE=1
            #DOAPPSERVER_INSTALL=1
            ;;
        -noupdate )
            DOUPDATE=0
            DOAPPSERVER_INSTALL=0
            had_explicit_noupdate=1
            ;;
        -tagsourcedate )
            #checkout sources with current timestamp
            TAG_SOURCE_DATE=1
            ;;
        -fast|-fastupdate )
            DOUPDATE=1
            DOAPPSERVER_INSTALL=0
            #only run junit tests (this will force use to compile tests):
            DOREGRESS=1
            DOJUNIT=1
            DOJREGRESS=0
            DOJAVADOC=0
            ;;
#INF related:
        -integreport )
            DOINTEGREPORT=1
            ;;
        -nointegreport )
            DOINTEGREPORT=0
            ;;
#end INF related
        -verbose )
            VERBOSE_ARG="-verbose"
            ;;
        -product )
            if [ $# -gt 0 ]; then
                REGRESS_PRODUCT=$1; shift
            else
                echo ${p}: -product requires the name of the top-level bom
                usage 1
            fi
            ;;
        -D*=* )
            if [ -z "$ARG_PROPS" ]; then
                ARG_PROPS="$arg"
            else
                ARG_PROPS="$ARG_PROPS $arg"
            fi
            ;;
        -D* )
            echo "${p}: illegal option, $arg"
            usage 1
            ;;
        *=* )
            tmp=`echo $arg|sed -e 's/"/\\\\"/g'`
            #echo A arg=.$arg. tmp is .$tmp.
            tmp=`echo $tmp|sed -e 's/^\([^=][^=]*\)=\(.*\)/\1="\2"; export \1/'`
            #echo B tmp is .$tmp.
            eval $tmp
            ;;
        -* )
            echo "${p}: unknown option, $arg"
            usage 1
            ;;
        * )
            ;;
        esac
    done

    # reset DOCLEAN if it is not needed.
    if [ $DOCLEAN -eq 1 ]; then
        if [ $DOCLEANSRC -eq 1 ]; then
            #we don't need to run the clean step if we are removing the src tree:
            DOCLEAN=0
            bldmsg -p $p -warn Ignoring -clean because -cleansrc is also specified.
        elif [ ! -d "$SRCROOT/global-common" ]; then
            #we don't need to run the clean step because source has not been checked out yet:
            DOCLEAN=0
            bldmsg -p $p -warn Ignoring -clean because global-common is missing - assume first build.
        fi
    fi

    #!junit and !jregress => !regress
    if [ $DOJUNIT -eq 0 -a $DOJREGRESS -eq 0 ]; then
        DOREGRESS=0
    fi

    #converse is also true:
    if [ $DOREGRESS -eq 0 ]; then
        DOJUNIT=0
        DOJREGRESS=0
    fi
}

############################### INFO ROUTINES #################################

show_options()
{
    bldmsg -mark Running $p $saveargs

    cat << EOF
OPTION SETTINGS FOR $p -
    DOHELP is          $DOHELP
    TESTMODE is        $TESTMODE
    DOCLEAN is         $DOCLEAN
    DOMAVENCLEAN is    $DOMAVENCLEAN
    DOMAVEN_UPDATE is  $DOMAVEN_UPDATE
    DOMAVENARCHIVE is  $DOMAVENARCHIVE
    DOCLEANSRC is      $DOCLEANSRC

    DOUPDATE is        $DOUPDATE
    DOFIXCVSROOT is    $DOFIXCVSROOT
    TAG_SOURCE_DATE is $TAG_SOURCE_DATE

    DOBUILD is         $DOBUILD
    DOCODEREVIEW is    $DOCODEREVIEW
    DOREGRESS is       $DOREGRESS
    DOJREGRESS is      $DOJREGRESS
    DOJUNIT is         $DOJUNIT

    DOJAVADOC is       $DOJAVADOC
    DOXREF is          $DOXREF
    DOARCHIVELOGS is   $DOARCHIVELOGS

    DORELEASE is       $DORELEASE
    DOINSTALL is       $DOINSTALL

    DOINTEGREPORT is   $DOINTEGREPORT

    REGRESS_PRODUCT is $REGRESS_PRODUCT

    ARG_PROPS are      $ARG_PROPS
EOF

}

show_build_environment()
{
    #show general environment:
    bld_show_env

    #show local additions:
    cat << EOF

    I_AM_PRIMARY is $I_AM_PRIMARY

    UPDATELOG is    $UPDATELOG
    CLEANLOG is     $CLEANLOG
    BUILDLOG is     $BUILDLOG
    JAVADOCLOG is   $JAVADOCLOG
    SRCBUNDLELOG is $SRCBUNDLELOG
    ADDONSLOG is    $ADDONSLOG
    XREFLOG is      $XREFLOG
    RELEASELOG is   $RELEASELOG
    MAVENARCHIVELOG is   $MAVENARCHIVELOG
    UNITTESTLOG is  $UNITTESTLOG
    REGRESSLOG is   $REGRESSLOG
    INSTALLOG is    $INSTALLOG
    CODEREVIEWLOG is   $CODEREVIEWLOG
    INTEGRATIONLOG is  $INTEGRATIONLOG

    JBI_CVSROOT is  $JBI_CVSROOT
    CVS_OPTS is     $CVS_OPTS

    HTML_SRCROOT is $HTML_SRCROOT

    BLDNUM is       $BLDNUM
    KIT_REV is      $KIT_REV

    DEVELOPER_BUILD is $DEVELOPER_BUILD
    RELEASE_BUILD is   $RELEASE_BUILD

    JBI_BRANCH_NAME is $JBI_BRANCH_NAME

EOF
}

################################## UTILITIES ##################################

require()
#import external shell routines - fatal error if we can't find it.
{
    libname=$1

    if [ x$libname = x ]; then
        echo "BUILD_ERROR: ${p}:require:  missing file name - ABORT"
        exit 1
    fi

    #look in a couple of familiar places:
    if [ -f "$TOOLROOT/lib/cmn/$libname" ]; then
        libname=$TOOLROOT/lib/cmn/$libname
    elif [ -f "./$libname" ]; then
        #we assume this is a test env!
        echo "$p - BUILD_WARNING: loading $libname from current directory."
        libname=./$libname
    fi

    . $libname
    if [ $? -ne 0 ]; then
        echo "BUILD_ERROR: ${p}:require: errors sourcing $libname - ABORT"
        exit 1
    fi
}

has_ant_errors()
#search for ant task errors:
#   [javac] 60 errors
#   [javadoc] 1 error
#   [junit] TEST com.sun.iis.ebxml.internal.support.logger.TestGlobalLogger FAILED
{
    if [ "$1" = "" ]; then
        bldmsg -error -p $p "Usage:  has_ant_errors LOGFILE"
        return 0
    fi

    #check for javac/javadoc errors:
    grep '\] [0-9]* error'  $1 > /dev/null
    if [ $? -eq 0 ]; then
        return 1
    fi

    #check for junit errors:
    grep 'Tests run:' $1 | grep -v 'Failures: 0, Errors: 0,' > /dev/null
    if [ $? -eq 0 ]; then
        #we found something matching, which means we had failures:
        return 1
    fi

    #check for jregress errors:
    egrep '\*TIMED OUT\*|\*FAILED\*| Connection refused' $1 > /dev/null
    if [ $? -eq 0 ]; then
        return 1
    fi

    return 0
}

filter_maven_log()
#funtion to reduce the maven download progress strings to just the final download total.
#INPUT:  stdin
#OUTPUT:  stdout
{
    perl -n -a -e 's|^.*\r([0-9][0-9Kb]* downloaded)$|$1|g; print;'
}

filter_maven_log_in_place()
#Usage:  filter_maven_log_in_place fn
#replace contents of <fn> with results of filter_maven_log
{
    if [ "$1" != "" ]; then
        filter_maven_log < "$1" > "$TMPA"
        if [ $? -eq 0 ]; then
            cmp -s "$1" "$TMPA"
            if [ $? -ne 0 ]; then
                #files are different:
                mv "$TMPA" "$1"
                return $?
            fi

            ## files are the same
            rm -f "$TMPA"
            return 0
        fi
        rm -f "$TMPA"
        return 1
    fi

    #FAILED:
    return 1
}

############################### INITIALIZATION ################################

setup_wrapper_env()
#setup up variables that are normally set by wrapper.
{
    #we require a LINKROOT separate from SRCROOT:
    if [ x$FORTE_LINKROOT = x ]; then
        if [ "$FORTE_PORT" != "nt" ]; then
            bld_fatal_error "FORTE_LINKROOT must be set to build OJC - ABORT"
        fi
    fi

    bld_setup_logdir
    if [ $? -ne 0 ]; then
        bld_fatal_error "failed to set up log directory - ABORT"
    fi

    bld_setup_lockdir

    #set up port-class vars:
    export IS_UNIX IS_NT

    IS_NT=0; bldhost -is_nt $FORTE_PORT
    if [ $? -eq 0 ]; then
        IS_NT=1
    fi

    IS_UNIX=0; bldhost -is_unix $FORTE_PORT
    if [ $? -eq 0 ]; then
        IS_UNIX=1
    fi
}

set_global_vars()
{
    p=`basename $0`
    TMPA=/tmp/${p}A.$$

    ###############
    # BUILD CONTROL
    ###############

    export MAVEN_OPTIONS KITROOT_SMB KITROOT_HTTP GFESB_KITROOT SMVN_PROPS

    MAVEN_OPTIONS="--fail-at-end -DrunFullBuild=1"
    if [ $DOMAVEN_UPDATE -eq 0 ]; then
        MAVEN_OPTIONS="$MAVEN_OPTIONS --offline"
    fi

    SMVN_PROPS='-DSRCROOT="$JV_SRCROOT" -Dmaven.repo.local="$JV_MAVEN_REPO_LOCAL" -DBUILD_NUMBER="$BLDNUM" -DCVS_BRANCH_NAME="$CVS_BRANCH_NAME" -DCVS_TIMESTAMP="$BLDTIME"'

    export GF_DOWNLOAD_URL
    if [ "$GF_DOWNLOAD_URL" = "" ]; then
        GF_DOWNLOAD_URL="http://download.java.net/javaee5/v2ur1/promoted"
    else
        bldmsg -warn -p $p "Setting GF_DOWNLOAD_URL from environment."
    fi

    if [ "$KITROOT_SMB" = "" ]; then
        KITROOT_SMB="NEED_TO_SET_VAR"
        bldmsg -warn -p $p "defaulting KITROOT_SMB for jcaps pickup to '$KITROOT_SMB'"
    fi

    if [ "$KITROOT_HTTP" = "" ]; then
        KITROOT_HTTP="NEED_TO_SET_VAR"
        bldmsg -warn -p $p "defaulting KITROOT_HTTP for jcaps pickup to '$KITROOT_HTTP'"
    fi

    if [ "$GFESB_KITROOT" = "" ]; then
        GFESB_KITROOT="NEED_TO_SET_VAR"
        bldmsg -warn -p $p "defaulting GFESB_KITROOT for gfesb pickup to '$GFESB_KITROOT'"
    fi


    if [ $DOPUSHKIT -eq 1 ]; then
        #make sure we do the release step:
        if [ $DORELEASE -eq 0 ]; then
            DORELEASE=1
            bldmsg -warn -p $p "turning on -release because -pushkit requires it."
        fi

        #Make sure required environment is defined:
        if [ x$PUSHKIT_IDENTITY = x ]; then
            DOPUSHKIT=0
            bldmsg -error -p $p "turning off -pushkit because PUSHKIT_IDENTITY is not set"
        fi
        if [ x$PUSHKIT_DEST = x ]; then
            DOPUSHKIT=0
            bldmsg -error -p $p "turning off -pushkit because PUSHKIT_DEST is not set"
        fi
        if [ x$PUSHKIT_SRC = x ]; then
            DOPUSHKIT=0
            bldmsg -error -p $p "turning off -pushkit because PUSHKIT_SRC is not set"
        fi

        #these variables are used to generate the external kit report:
        if [ x$HTTP_PUSHKIT_ROOT = x ]; then
            DOPUSHKIT=0
            bldmsg -error -p $p "turning off -pushkit because HTTP_PUSHKIT_ROOT is not set"
        fi
        if [ x$PUSHKIT_REL_PATH = x ]; then
            DOPUSHKIT=0
            bldmsg -error -p $p "turning off -pushkit because PUSHKIT_REL_PATH is not set"
        fi
    fi

    if [ $DOINSTALL -eq 1 ]; then
        #make sure we do the release step:
        if [ $DORELEASE -eq 0 ]; then
            DORELEASE=1
            bldmsg -warn -p $p "turning on -release because -install requires it."
        fi
    fi

    #note - I_AM_PRIMARY is set by parse_args.
    if [ $I_AM_PRIMARY -eq 0 ]; then
        if [ $RELEASE_BUILD -eq 1 ]; then
            #we don't build & release on non-primary machines during RE builds:
            if [ $DORELEASE -eq 1 ]; then
                #user specified -release on non-primary - issue usage warning and unset
                bldmsg -warn -p $p "-release has no effect on non-primary RE build machines."
                DORELEASE=0
            fi
        fi
    fi

    ##############
    # BRANCH NAMES
    ##############
    export JBI_BRANCH_NAME
    if [ "$JBI_BRANCH_NAME" = "trunk" ]; then
        JBI_BRANCH_NAME="main"
        bldmsg -warn -p $p "JBI_BRANCH_NAME 'trunk' is deprecated; changing to '$JBI_BRANCH_NAME'"
    elif [ "$JBI_BRANCH_NAME" = "" ]; then
        JBI_BRANCH_NAME="main"
        bldmsg -warn -p $p "defaulting JBI_BRANCH_NAME to '$JBI_BRANCH_NAME'"
    fi

    #########
    # JAVADOC
    # Warning:  if you change $JAVADOC_BASE here, change it in common_defs.bom and antbld/inc/prolog.ant
    #########
    export JAVADOC_BASE LASSEN_JAVADOC WHITNEY_JAVADOC SHASTA_JAVADOC ARCHIVE_JAVADOC ALASKA_KITDIR
    ARCHIVE_JAVADOC=$SRCROOT/$JBI_BRANCH_NAME/javadoc_stable
    JAVADOC_BASE=$SRCROOT/antbld/bld/doc
    LASSEN_JAVADOC=$JAVADOC_BASE/lassen
    WHITNEY_JAVADOC=$JAVADOC_BASE/whitney
    SHASTA_JAVADOC=$JAVADOC_BASE/shasta
    ALASKA_KITDIR=$SRCROOT/antbld/bld/alaska

    if [ $DOXREF -eq 1 ]; then
        #make sure we do the javadoc step:
        if [ $DOJAVADOC -eq 0 ]; then
            DOJAVADOC=1
            bldmsg -warn -p $p "turning on -javadoc because -xref requires it."
        fi
    fi

    ###########
    # X-DISPLAY
    ###########
    export DISPLAY
    DISPLAY=$REGRESS_DISPLAY

    ###########
    # LOG FILES
    ###########

    set -a
    ADDONSLOG=$LOGDIR/makeAddons.log
    ANTLOGDIR=$LOGDIR/antlogs
    BUILDLOG=$LOGDIR/javaBuild.log
    CLEANLOG=$LOGDIR/javaClean.log
    CODEREVIEWLOG=$LOGDIR/checkStyle.log
    INSTALLOG=$LOGDIR/makeInstall.log
    INTEGRATIONLOG=$LOGDIR/integ_${PRODUCT}.txt
    JAVADOCLOG=$LOGDIR/javadoc.log
    PUSHKITLOG=$LOGDIR/pushkit.log
    REGRESSLOG=$LOGDIR/runSystemTests.log
    RELEASELOG=$LOGDIR/makeRelease.log
    MAVENARCHIVELOG=$LOGDIR/mavenArchive.log
    MAVENDEPLOYLOG=$LOGDIR/mavenDeploy.log
    SRCBUNDLELOG=$LOGDIR/srcbundle.log
    UNITTESTLOG=$LOGDIR/runUnitTests.log
    UPDATELOG=$LOGDIR/cvs_update.log
    XREFLOG=$LOGDIR/xref.log
    set +a

    #init vars for summary reports:
    export TEST_SUMMARY_REPORT KIT_SUMMARY_REPORT
    TEST_SUMMARY_REPORT=$LOGDIR/runRegress.rpt
    KIT_SUMMARY_REPORT=$LOGDIR/kitSummary.rpt

    ### setup file to tell testResults which suite we are testing:
    REGSTAT=$LOGDIR/../regstat.ok
    rm -f $REGSTAT

    if [ "$PRI_BLD_LOCATION" = "" ]; then
      PRI_BLD_LOCATION=$SRCROOT
    fi

    ############
    # LOCK FILES
    ############
    KITSREADY=$LOCKDIR/kits.rdy
    SETRELEASEREADY=$LOCKDIR/release.rdy

    export MAX_KIT_WAIT
    if [ "$MAX_KIT_WAIT" = "" ]; then
        #wait up to 30 minutes (1800 seconds) for primary build:
        MAX_KIT_WAIT=1800
    fi

    #if we have a -bldnum arg, then reset BLDNUM (set by bld_setup_logdir).
    if [ $BLDNUM_ARG != NULL ]; then
        BLDNUM=$BLDNUM_ARG
    fi

    ###############
    # JBI_CVSROOT - this is the product CVSROOT used by this script.
    #                 You can override it if you are calling this script from
    #                 a special env. where CVSROOT means something else.
    ###############
    export JBI_CVSROOT
    if [ "$JBI_CVSROOT" = "" ]; then
        JBI_CVSROOT=$CVSROOT
    else
        bldmsg -warn -p $p "Setting JBI_CVSROOT from environment."
    fi

    #############
    # CVS OPTIONS
    #############
    export CVS_OPTS
    if [ "$CVS_OPTS" = "" ]; then
        #default is quiet, ignore $HOME/.cvsrc, read-only, compression:
        CVS_OPTS="-q -f -r -z6"
    else
        bldmsg -warn -p $p "Setting CVS_OPTS from environment."
    fi

    #####################
    # CVS CHECKOUT TARGET
    #####################
    # Use module lists from the environment, if specified. #
    if [ ! -z "$JBI_MODULES" ]; then
        CVS_CO_LIST="$JBI_MODULES"
    else
        CVS_CO_LIST="\
            open-jbi-components/global-common\
            open-jbi-components/ojc-core\
            open-jbi-components/pom.xml\
            open-jbi-components/nbactions.xml\
            open-jbi-components/rl\
            open-jbi-components/packaging\
            open-jbi-components/installers\
            open-jbi-components/contrib-gestalt\
            open-jbi-components/contrib-imola\
            open-jbi-components/contrib-stort\
            open-jbi-components/contrib-zaz\
        "
    fi

    ####################
    # INTEGRATION REPORT
    ####################
    if [ $DOINTEGREPORT -eq 1 ]; then
        export INTEG_REPORT
        if [ "$INTEG_REPORT" = "" ]; then
            INTEG_REPORT=integrationReport
            bldmsg -warn -p $p "defaulting INTEG_REPORT to '$INTEG_REPORT'"
        fi
    fi

    ##############
    # HTTP_KITROOT
    ##############
    export HTTP_KITROOT
    if [ "$HTTP_KITROOT" = "" ]; then
        HTTP_KITROOT="file://$KIT_DISTROOT"
        bldmsg -warn -p $p "defaulted HTTP_KITROOT to '$HTTP_KITROOT'"
    fi

    ##########
    # JXTOHTML
    ##########
    export HTML_SRCROOT EXCEPTION_INDEX
    if [ "$HTML_SRCROOT" = "" ]; then
        if [ "$HTTP_LOGROOT" != "" ]; then
            HTML_SRCROOT="$HTTP_LOGROOT/$CODELINE/javadoc/regress_javadoc/src-html"
        else
            HTML_SRCROOT="http://iis.sfbay/open-jbi-components/main/javadoc/regress_javadoc/src-html"
        fi

        bldmsg -warn -p $p "defaulted HTML_SRCROOT to '$HTML_SRCROOT'"
    fi
    EXCEPTION_INDEX=$LOGDIR/_EXCEPTIONS.html

    #########
    # REGRESS PRODUCT TARGET
    #########
    case $REGRESS_PRODUCT in
    ojc )
        REGRESS_PRODUCT=ojc
        ;;
    * )
        bldmsg -error -p $p "Unrecognized -product arg, '$REGRESS_PRODUCT' - defaulting to 'ojc'"
        REGRESS_PRODUCT=ojc
        ;;
    esac

    #########
    # ASADMIN TIMEOUT:
    #########
    #how long to wait for asadmin commands:
    export MAX_ASADMIN_WAIT
    if [ "$MAX_ASADMIN_WAIT" = "" ]; then
        #max wait, in seconds:
        MAX_ASADMIN_WAIT=90
    fi

    return 0
}

check_local_vars()
#This routine is largely obsolete.  however you can
#include checks for variables that must be set in the
#environment and are not checked earlier in set_global_vars()
{
    localvarerrs=0

    if [ $localvarerrs -ne 0 ]; then
        return 1
    fi

    return 0
}

clear_local_locks()
{
    #place-holder for now.
    echo ""
}

################################# HOUSEKEEPING #################################

cleanup()
{
    if [ x$TMPA != x ]; then
        rm -f $TMPA
    fi

    #remove the pid file if we were not called from wrapper:
    if [ $CALLEDFROMWRAPPER -eq 0 ]; then
        rm -f $RUNBLD_PIDFILE
    fi

}

rec_signal()
{
    cleanup
    bldmsg -error -p $p Interrupted
    bldmsg -markend -status 2 $p

    exit 2
}

################################### INTERNAL ###################################

update_repo()
# Usage:  eval update_repo [-export] [-D date_tag] [-r alpha_tag] cvsroot dest_dir repos_name co_target
# you must quote date arg if it has spaces.
{
    _datearg=
    _tagarg=
    _prunearg=
    _cvs_cmd=checkout
    _export_sources=0
    _urepo_arg=

    while [ $# -gt 0 -a "$1" != "" ]
    do
        _urepo_arg=$1; shift

        case $_urepo_arg in
        -export )
            _export_sources=1
            _cvs_cmd=export
            ;;
        -D )
            _datearg="-D '$1'"
            shift
            ;;
        -r )
            _tagarg="-r '$1'"
            shift
            ;;
        -* )
            echo "update_repo: unknown option, $_urepo_arg"
            return 1
            ;;
        * )
            break
            ;;
        esac
    done

    cvsroot=$_urepo_arg        #already did shift in loop
    _dest_dir=$1; shift
    repo_name="$1"; shift
    co_target="$1"

#echo update_repos: cvsroot=$cvsroot _dest_dir=$_dest_dir repo_name=$repo_name co_target=$co_target _datearg=$_datearg _tagarg=$_tagarg

    installdir $SRCROOT/$_dest_dir
    ret=$?
    if [ $ret -ne 0 ]; then
        bldmsg -error -p $p/update_repo "cannot create $SRCROOT/$_dest_dir - aborting $_cvs_cmd  of $_dest_dir."
        return $ret
    fi

    bldmsg -markbeg -p $p update $repo_name source

    if [ "$_datearg" = "" ]; then
        _prunearg="-P"
    else
        #Note that -D <date> implies -P, so we don't need it:
        #Note also that export requires a date tag
        _prunearg=
    fi

    #hack to install top-level CVS dir
    if [ $_export_sources -eq 0 -a ! -r $SRCROOT/CVS/Root ]; then
        rm -rf  $SRCROOT/CVS
        rm -rf $SRCROOT/tmp/$CVS_SRCROOT_PREFIX

        mkdir -p $SRCROOT/tmp
        cd $SRCROOT/tmp

        bldmsg -mark "Create top-level CVS dir: `echo $cmd`"
        cmd="cvs $CVS_OPTS -d $cvsroot checkout -l -A $_prunearg $_tagarg $_datearg $CVS_SRCROOT_PREFIX"
        eval $cmd
        ret=$?

        if [ $ret -ne 0 ]; then
            bldmsg -error -p $p Could not checkout top-level CVS directory, $CVS_SRCROOT_PREFIX/CVS
            bldmsg -markend -p $p -status $ret update $repo_name source
            return $ret
        else
            mv $SRCROOT/tmp/$CVS_SRCROOT_PREFIX/CVS $SRCROOT
            rm -rf $SRCROOT/tmp/$CVS_SRCROOT_PREFIX
        fi
    fi

    cd $SRCROOT/$_dest_dir
    cwd=`pwd`

    _dashA="-A"
    if [ $_export_sources -eq 1 ]; then
        _dashA=
    fi

    # Use 'eval' to delay interpretation of embedded quotes:
    cmd="cvs $CVS_OPTS -d $cvsroot $_cvs_cmd $_dashA $_prunearg $_tagarg $_datearg $co_target"
    bldmsg -mark "In $cwd: `echo $cmd`"
    eval $cmd
    ret=$?

    bldmsg -markend -p $p -status $ret update $repo_name source

    return $ret
}

get_product_services()
#return the list of services used in the product build (exclude tools services)
{
    echo "$CVS_CO_LIST"
}

get_test_services()
#return the driver-test services
{
    #testing:
    #echo "open-jbi-components/nbbuild open-jbi-components/driver-tests/filebc"

    echo "open-jbi-components/nbbuild open-jbi-components/driver-tests"
}

set_update_time()
{
    export BLDTIME UCVSUPDATETIME JBI_SNAPSHOT_TIME UCVSUPDATETIMEDOT

    BLDTIME=""
    touch $SRCROOT/bldlock/bldenv.sh

    UCVSUPDATETIME=`bld_gmttime`

    if [ $RELEASE_BUILD -eq 1 ]; then
        if [ $I_AM_PRIMARY -eq 1 ]; then
            #always set the build time for primary
            if [ $DOUPDATE -eq 1 ]; then
                shprops -set $SRCROOT/bldlock/bldenv.sh LASTUPDATETIME=$UCVSUPDATETIME
            else
                tmpPRODUCT=jbi
                eval `shprops -get $LASTBLDPARMS ${tmpPRODUCT}_last_update`
                cmd="echo \$${tmpPRODUCT}_last_update"
                LOCAL_LASTUPDATETIME=`eval $cmd`
                shprops -set $SRCROOT/bldlock/bldenv.sh LASTUPDATETIME=$LOCAL_LASTUPDATETIME
            fi
        else
            if [ ! -f $PRIPATHREF/bldlock/bldenv.sh ]; then
                bldmsg -mark -error -p $p "RELEASE BUILD:  Cannot access \$PRIPATHREF/bldlock/bldenv.sh - has primary build fired? PRIPATHREF='$PRIPATHREF'"
                return 1
            fi

            eval `shprops -get $PRIPATHREF/bldlock/bldenv.sh LASTUPDATETIME`
            UCVSUPDATETIME=$LASTUPDATETIME
            echo "LASTUPDATETIME=${LASTUPDATETIME}" > $SRCROOT/bldlock/bldenv.sh
        fi
    fi

    #BLDTIME=`bld_tocvstime $UCVSUPDATETIME`
    UCVSUPDATETIMEDOT=`echo $UCVSUPDATETIME | sed "s/\(....\)\(..\)\(..\)\(..\)\(..\)\(..\)/\1.\2.\3.\4.\5.\6/"`
    #change BLDTIME to use dot format, which works with later versions of cvs.
    BLDTIME="$UCVSUPDATETIMEDOT"

    JBI_SNAPSHOT_TIME=`echo $UCVSUPDATETIME | perl -n -e '$xx=$_; printf "%s.%s-1", substr($xx,0,8), substr($xx,8,6);'`
    bldmsg "JBI_SNAPSHOT_TIME=$JBI_SNAPSHOT_TIME"
    return 0
}

update_src()
#Usage:  update_src [-testsources] [-export]
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    #default is to update product sources:
    get_test_sources=0
    export_sources=0
    export_arg=
    _usrc_arg=

    while [ $# -gt 0 -a "$1" != "" ]
    do
        _usrc_arg=$1; shift

        case $_usrc_arg in
        -testsources )
            get_test_sources=1
            ;;
        -export )
            export_sources=1
            export_arg=-export
            ;;
        -* )
            echo "update_src: unknown option, $_usrc_arg"
            return 1
            ;;
        * )
            break
            ;;
        esac
    done

    cd "$SRCROOT"
    if [ "$_usrc_arg" != "" ]; then
        #already did shift in loop
        destdir="$_usrc_arg"
        if [ $get_test_sources -eq 1 ]; then
            rm -rf "$destdir"
            mkdir -p "$destdir"
        fi
    else
        destdir=..
    fi

    bldmsg "BUILDRESULTS_TYPE=cvsupdate"

    # Print the time in both formats. *
    bldmsg -mark -p $p/update_src BLDTIME in CVS format: \"$BLDTIME\"

    #######
    # setup the source update:
    #######
    #default branch to env name if it is not mainline:
    if [ "$JBI_BRANCH_NAME" != "main" ]; then
        jbi_revarg="-r '$JBI_BRANCH_NAME'";
    else
        jbi_revarg="";
    fi

    #setup the date arg. default, which is BLDTIME for RE builds:
    #NOTE:  if we are exporting instead of checking-out, must supply date.
    if [ $export_sources -eq 1 -o $TAG_SOURCE_DATE -eq 1 ]; then
        jbi_datearg="-D '$BLDTIME'"
    else
        jbi_datearg=""
    fi

    #if -testsource option, then get test sources:
    if [ $get_test_sources -eq 1 ]; then
        srcdirs="`get_test_services`"
    else
        srcdirs="`get_product_services`"
    fi

    #######
    # start the source update:
    #######
    if [ $get_test_sources -eq 0 -a $DOCLEANSRC -eq 1 ]; then
        #also remove local bdb and release files:
        cleandirs="$srcdirs $SRCROOT/bdb $RELEASE_DISTROOT"

        bldmsg -mark -p $p/update_src "Removing source: $cleandirs"
        cd $SRCROOT/..
        bld_killall -bg $cleandirs
        if [ $? -ne 0 ]; then
            bldmsg -warn -p $p/update_src "had trouble removing source, continuing"
        fi
        cd $SRCROOT
    fi

    ############
    #update_repo to checkout or export
    ############
    cd "$SRCROOT"
    update_src_errs=0
    if [ $get_test_sources -eq 1 ]; then
        eval update_repo $export_arg $jbi_datearg $jbi_revarg $JBI_CVSROOT "$destdir" test "'$srcdirs'"
    else
        eval update_repo $export_arg $jbi_datearg $jbi_revarg $JBI_CVSROOT "$destdir" product "'$srcdirs'"
    fi
    c_status=$?

    if [ $get_test_sources -eq 1 ]; then
        if [ $c_status -eq 0 ]; then
            bldmsg -p $p/update_src -mark $PRODUCT test source update SUCCESSFUL.
        else
            update_src_errs=1
            bldmsg -p $p/update_src -error $PRODUCT test source update FAILED.
        fi
    else
        if [ $c_status -eq 0 ]; then
            bldmsg -p $p/update_src -mark $PRODUCT source update SUCCESSFUL.

            bldmsg -mark -p $p "Setting jbi_last_update to '$UCVSUPDATETIME' in '$BLDPARMS'"
            shprops -set $BLDPARMS jbi_last_update=$UCVSUPDATETIME
        else
            update_src_errs=1
            bldmsg -p $p/update_src -error $PRODUCT source update FAILED.
            bldmsg -warn -p $p "Not setting jbi_last_update in '$BLDPARMS' because the CVS update failed"
        fi
    fi

    return $update_src_errs
}

build_product()
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    bldmsg "BUILDRESULTS_TYPE = ant_generic"

    cd $SRCROOT

    if [ $DOAPPSERVER_INSTALL -eq 1 ]; then
        #refresh the app-server installation, which we compile and test against:
        bldmsg -markbeg -p $p/build_product Install appserver 8
        installas8 $INSTALLAS_ARGS
        _status=$?
        if [ $_status -ne 0 ]; then
            bldmsg -error -p $p/build_product Appserver installation FAILED
            bldmsg -markend -p $p/build_product -status $_status Install appserver 8
            return 1
        fi

        bldmsg -markend -p $p/build_product -status $_status Install appserver 8
    fi

    bldmsg "BUILDRESULTS_TYPE = jbi_javabuild"

    #if -update, then rerun clean task to ensure consistency.
    #note - always clean junit system-tests projects as well.  RT 8/2/07.
    if [ $DOUPDATE -eq 1 ]; then
        MAVEN_GOALS="clean"
        cmd="mvn $MAVEN_OPTIONS $ARG_PROPS $SMVN_PROPS -Dmaven.test.skip=true -DrunSystemTests $MAVEN_GOALS"
        bldmsg -mark -p $p/build_product `echo $cmd`

        eval $cmd
        if [ $? -ne 0 ]; then
            bldmsg -error -p $p/build_product FAILED during post-source update clean
            return 1
        fi
    fi

    MAVEN_GOALS="install"
    cmd="mvn $MAVEN_OPTIONS $ARG_PROPS $SMVN_PROPS -Dmaven.test.skip=true $MAVEN_GOALS"
    bldmsg -mark -p $p/build_product `echo $cmd`

    eval $cmd
    return $?
}

maven_deploy()
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    cd $SRCROOT

    bldmsg "BUILDRESULTS_TYPE = jbi_javabuild"

    #currently, we only deploy in ojc-core:
    MAVEN_GOALS="-DrunFullBuild=0 -DdeployRemote=1 deploy"

    cmd="mvn $MAVEN_OPTIONS $ARG_PROPS $SMVN_PROPS -Dmaven.test.skip=true $MAVEN_GOALS"
    bldmsg -mark -p $p/maven_deploy `echo $cmd`

    eval $cmd
    return $?
}

build_xref_maven()
#use maven to generate xref'd java source.
#not currently generating with javadoc correctly.
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    bldmsg "BUILDRESULTS_TYPE=ant_generic"

    cd $SRCROOT

    #note - xref target below runs its own javadoc plugin, and does not use
    #the javadoc generated by build_javadoc.

    cmd="mvn $MAVEN_OPTIONS $ARG_PROPS $SMVN_PROPS -Dmaven.test.skip=true site:site"
    bldmsg -mark -p $p/build_xref_maven `echo $cmd`

    eval $cmd
    return $?
}

make_src_bundles()
#create source zips.
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    bld_reset_watchdog
    make_src_prod_bundles
    _msbprodzip_stat=$?

    bld_reset_watchdog
    make_test_src_bundles
    _msbtestzip_stat=$?

    if [ $_msbprodzip_stat -ne 0 -o $_msbtestzip_stat -ne 0 ]; then
        return 1
    fi

    return 0
}

make_test_src_bundles()
#create test source zips.
{
    bldmsg "BUILDRESULTS_TYPE = jbi_javabuild"
    cd "$SRCROOT"

    _src_bundles_status=0
    _src_bundles_reldir="bld/src/test"
    _src_bundles_fulldir="$SRCROOT/$_src_bundles_reldir"
    _src_bundles_outdir="$_src_bundles_fulldir/open-jbi-components"
    _src_bundles_zipfile="$_src_bundles_fulldir/driver-tests.zip"

    bldmsg -mark -p $p/make_src_bundles checking out test sources
    update_src -testsources -export $_src_bundles_reldir
    if [ $? -ne 0 ]; then
        bldmsg -error -p $p/make_src_bundles checking out test sources FAILED.
        _src_bundles_status=1
    elif [ -d "$_src_bundles_outdir" ]; then
        #zip it up:
        cd "$_src_bundles_outdir"
        jar cMf "$_src_bundles_zipfile" *
        if [ $? -ne 0 ]; then
            bldmsg -error -p $p/make_src_bundles "could not create '$_src_bundles_zipfile' from '$_src_bundles_outdir'"
            _src_bundles_status=2
        fi
    else
        bldmsg -error -p $p/make_src_bundles "output directory '$_src_bundles_outdir' does not exist!"
        _src_bundles_status=3
    fi

    cd "$SRCROOT"

    return $_src_bundles_status
}

make_src_prod_bundles()
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    bldmsg "BUILDRESULTS_TYPE = jbi_javabuild"
    cd "$SRCROOT"

    _src_prod_bundles_status=0
    _src_prod_bundles_reldir="bld/src/product"
    _src_prod_bundles_fulldir="$SRCROOT/$_src_prod_bundles_reldir"
    _src_prod_bundles_outdir="$_src_prod_bundles_fulldir/open-jbi-components"
    _src_prod_bundles_zipfile="$_src_prod_bundles_fulldir/ojc-src.zip"

    bldmsg -mark -p $p/make_src_prod_bundles checking out product sources
    update_src -export $_src_prod_bundles_reldir
    if [ $? -ne 0 ]; then
        bldmsg -error -p $p/make_src_prod_bundles checking out test sources FAILED.
        _src_prod_bundles_status=1
    elif [ -d "$_src_prod_bundles_outdir" ]; then
        #zip it up:
        cd "$_src_prod_bundles_outdir"
        jar cMf "$_src_prod_bundles_zipfile" *
        if [ $? -ne 0 ]; then
            bldmsg -error -p $p/make_src_prod_bundles "could not create '$_src_prod_bundles_zipfile' from '$_src_prod_bundles_outdir'"
            _src_prod_bundles_status=2
        fi
    else
        bldmsg -error -p $p/make_src_prod_bundles "output directory '$_src_prod_bundles_outdir' does not exist!"
        _src_prod_bundles_status=3
    fi

    cd "$SRCROOT"

    return $_src_prod_bundles_status
}

build_javadoc()
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    bldmsg "BUILDRESULTS_TYPE = jbi_javabuild"

    bldmsg -warn Javadoc build tbd.

    return $?
}

archive_logs()
{
    _status=0

    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    bldmsg "BUILDRESULTS_TYPE=ant_generic"

    cd $SRCROOT
    installdir $ANTLOGDIR

    cmd="ant $VERBOSE_ARG -Dregress_product=$REGRESS_PRODUCT -Dlog.dir.name=$ANTLOGDIR -Dfailonerror=false -Dbuildnumber=$BLDNUM -Dbranch=$JBI_BRANCH_NAME $ARG_PROPS archive_logs"
    cmd="echo archive_logs"
    bldmsg -mark -p $p/archive_logs `echo $cmd`

    eval $cmd
    [ $? -ne 0 ] && _status=1

    #now scan the logs for java exception traces:
    cmd="ant $VERBOSE_ARG -Dregress_product=$REGRESS_PRODUCT -Dlog.dir.name=$ANTLOGDIR -Dfailonerror=false -Dbuildnumber=$BLDNUM -Dbranch=$JBI_BRANCH_NAME $ARG_PROPS jxtohtml"
    cmd="echo jxtohtml"
    bldmsg -mark -p $p/archive_logs `echo $cmd`
    eval $cmd

    [ $? -ne 0 ] && _status=1

    #if we created any _X* files (html logs with hyperlinked exception traces),
    #then find them and collect them in a top-level index file:

    return $_status
}

clean_maven()
{
  rm -rf "$SRCROOT/m2"
  return $?
}


clean_build_tree()
#NOTE:  maven always loads dependencies prior to running a target.
#Therefore, we have to do a clean:build stepwise for each sub-project.
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    bldmsg "BUILDRESULTS_TYPE = ant_generic"

    cd $SRCROOT

    bldmsg "BUILDRESULTS_TYPE = jbi_javabuild"

    MAVEN_GOALS="clean"
    cmd="mvn $MAVEN_OPTIONS $ARG_PROPS $SMVN_PROPS -Dmaven.test.skip=true -DrunSystemTests $MAVEN_GOALS"

    #note:  we always clean junit system test projects.  RT 8/2/07
    bldmsg -mark -p $p/clean_build_tree `echo $cmd`

    eval $cmd
    return $?
}

map_regress_port()
#
# Usage: realport=`map_regress_port $kitport`
#
#NOTE:  this is to make sure <FORTE_PORT> macros in the boms are analyzed
#       correctly.
{
    if [ "$bldnum" = "" ]; then
        bldmsg -error -p $p/map_regress_port "Usage: map_regress_port regress_port_name" 1>&2
        echo "NULL_PORT"
        return 1
    fi

    case $1 in
    redhat )
        echo linux
        ;;
    sollassen )
        echo solsparc
        ;;
    nt5 )
        echo nt
        ;;
    xp )
        echo nt
        ;;
    * )
        echo $1
        ;;
    esac

    return 0
}

make_release()
#Example settings:
# KITREV=main
# BLDNUM=001114
#this routine is normally only called on primary port
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    kiterrs=0

    bldmsg "BUILDRESULTS_TYPE=general"

    ######
    #build release dir:
    ######
    #makedrv -auto -q -c bdb

    kitbase=$KIT_DISTROOT/$KIT_REV
    bldnum=Build$BLDNUM
    bldnumfile=$kitbase/.bldnum
    prod="ojc"
    relstage="$SRCROOT/bld/release"

    #######
    #release each port on primary build machines...
    #######
    if [ $I_AM_PRIMARY -eq 1 ]; then
        kitports="`bldhost -a -kitports -port`"
    else
        kitports=$REGRESS_FORTE_PORT
    fi

    if [ ! -d $kitbase ]; then
        installdir -m 0775 $kitbase
    fi

    if [ ! -f $bldnumfile ]; then
        touch $bldnumfile
    fi

    #create release staging area:
    rm -rf $relstage
    installdir -m 0775 $relstage

    #copy in the cvs_update.log if it exists
    if [ -r $UPDATELOG ]; then
        cmd="cp $UPDATELOG ${relstage}"
        bldmsg -mark -p $p/make_release $cmd
        eval $cmd
        if [ $? -ne 0 ]; then
           bldmsg -error -p $p/make_release "'$cmd' FAILED"
           kiterrs=1
        fi
    else
        echo "cvs -update was not run" > ${relstage}/cvs_update.log
        if [ $? -ne 0 ]; then
           bldmsg -error -p $p/make_release "fake cvs update creation FAILED"
           kiterrs=1
        fi
    fi

    #create version files:
    versionfile="${relstage}/version.txt"
    echo $bldnum  >> $versionfile 2>&1

    #create $PRODUCT.ver file:
    ojcverfile="${relstage}/$PRODUCT.ver"

    #create jcaps_download file:
    jcapsfile="${relstage}/jcaps_download"

    #create gfesb_download file:
    gfesbfile="${relstage}/gfesb_download"

    bldhost -versionproperties > $TMPA
    #read in version properties:
    eval `shprops -q -get $TMPA`

    #output selected fields:
    cat << EOF_VER >> "$ojcverfile"
FULL_PRODUCT_NAME="$FULL_PRODUCT_NAME"
SHORT_PRODUCT_NAME="$SHORT_PRODUCT_NAME"
FULL_VERSION="$FULL_VERSION"
MAJOR_VERSION="$MAJOR_VERSION"
MINOR_VERSION="$MINOR_VERSION"
BUILD_NUMBER="$BLDNUM"
CODELINE="$CODELINE"
TIMESTAMP="$UCVSUPDATETIMEDOT"
EOF_VER

    #output jcapsfile 
    cat << EOF_JCAPS >> "$jcapsfile"
component.build.save.dir=$KITROOT_SMB/ojc/${CODELINE}/Build${BLDNUM}/solsparc/ojc
component.build.save.file=$KITROOT_HTTP/ojc/${CODELINE}/Build${BLDNUM}/solsparc/ojc/ojc-components.zip
OJC_BUILD=$BLDNUM
OJC_VERSION=$FULL_VERSION
httpbc.jar=$KITROOT_SMB/ojc/${CODELINE}/Build${BLDNUM}/solsparc/ojc/httpbc.jar
bpelserviceengine.jar=$KITROOT_SMB/ojc/${CODELINE}/Build${BLDNUM}/solsparc/ojc/bpelserviceengine.jar
etlserviceengine.jar=$KITROOT_SMB/ojc/${CODELINE}/Build${BLDNUM}/solsparc/ojc/etlserviceengine.jar
sharedutillib.jar=$KITROOT_SMB/ojc/${CODELINE}/Build${BLDNUM}/solsparc/ojc/sharedutillib.jar
wsdlextlib.jar=$KITROOT_SMB/ojc/${CODELINE}/Build${BLDNUM}/solsparc/ojc/wsdlextlib.jar
etlse-monitor.war=$KITROOT_SMB/ojc/${CODELINE}/Build${BLDNUM}/solsparc/ojc/etlse-monitor.war 
EOF_JCAPS

    #output gfesbfile 
    cat << EOF_GFESB >> "$gfesbfile"
OJC_FULL_PRODUCT_NAME="$FULL_PRODUCT_NAME"
OJC_SHORT_PRODUCT_NAME="$SHORT_PRODUCT_NAME"
OJC_FULL_VERSION="$FULL_VERSION"
OJC_MAJOR_VERSION="$MAJOR_VERSION"
OJC_MINOR_VERSION="$MINOR_VERSION"
OJC_BUILD_NUMBER="$BLDNUM"
OJC_CODELINE="$CODELINE"
OJC_TIMESTAMP="$UCVSUPDATETIMEDOT"
ojc-components.zip=$GFESB_KITROOT/Build${BLDNUM}/ojc/ojc-components.zip
EOF_GFESB

    TEMPLATE="THEFILE=${GFESB_KITROOT}/Build${BLDNUM}/ojc/THEFILE"
    for theFile in `ppbom -useexternal -flat -q -bomloc $SRCROOT/rl/src/cmn/bom ojc.bom | grep -v index.html`
    do
        echo $TEMPLATE | sed -e "s/THEFILE/$theFile/g" >> "$gfesbfile"
    done

    rm -f $TMPA

    #create index.html file:
    indexfile="${relstage}/index.html"
    full_name=`asadmin version -t -v | sed 's/^[^(]*(//' | sed 's/build //'  | sed 's/)[.]*//'`
    number_only=`echo $full_name | sed 's/-[^-]*//'`

    cat << INDEX_EOF >> "$indexfile"
<html>
<head>
<title>${bldnum}</title>
</head>
<body>
<h3>${bldnum}</h3>
Before installing these Open JBI Components you need to make sure that you have Project Glassfish v2 installed on your machine.
<br>
Project Glassfish v2 $full_name  can be downloaded here: <a href="$GF_DOWNLOAD_URL">$GF_DOWNLOAD_URL</a>
<br><br>
Open JBI Components downloads from ${bldnum}:
<ul>
INDEX_EOF

    TEMPLATE='<li><a href="THEFILE">THEFILE</a>'
    for theFile in `ppbom -useexternal -flat -q -bomloc $SRCROOT/rl/src/cmn/bom ojc.bom | grep -v index.html`
    do
        echo $TEMPLATE | sed -e "s/THEFILE/$theFile/g" >> "$indexfile"
    done

    cat << INDEX_EOF >> "$indexfile"
</ul>
</body>
</html>
INDEX_EOF


    #create kit summary header:
    cat << MSG_EOF >> "$KIT_SUMMARY_REPORT"
===========
KITS POSTED:
===========

MSG_EOF

    ########
    #release components to release staging area:
    ########
    bom=components.bom
    release -nolog -nochecksum -bomloc $SRCROOT/rl/src/cmn/bom -w $relstage/components -port cmn $bom
    status=$?
    if [ $status -ne 0 ]; then
        bldmsg -error -p $p/make_release Release of $bom to $relstage FAILED
        kiterrs=1
    else
        #create zip of all components:
        component_list=`ppbom -useexternal -flat -q -bomloc $SRCROOT/rl/src/cmn/bom components.bom`
        (cd $relstage/components && jar cMf ../ojc-components.zip $component_list)
        status=$?
        if [ $status -ne 0 ]; then
            bldmsg -error -p $p/make_release creation of $relstage/ojc-components.zip FAILED
            kiterrs=1
        fi
    fi

    ########
    #release all kits:
    ########

    kitdir=$kitbase/$bldnum/cmn/$prod
    bom=$prod.bom

    bldmsg -mark -p $p/make_release Release $bom to $kitdir

    release -bomloc $SRCROOT/rl/src/cmn/bom -ver $BLDNUM -log -checksum -w $kitdir $bom
    status=$?

    if [ $status -ne 0 ]; then
        bldmsg -error -p $p/make_release Release of $bom to $KIT_DISTROOT FAILED
        kiterrs=1
    else
        echo "    $HTTP_KITROOT/$KIT_REV/$bldnum/cmn"  >> "$KIT_SUMMARY_REPORT"
    fi

    for kitport in $kitports
    do
        realport=`map_regress_port $kitport`
        cd $kitbase/$bldnum
        ln -s cmn $kitport
        status=$?
        if [ $status -ne 0 ]; then
            bldmsg -error -p $p/make_release creation of $realport symlink FAILED
            kiterrs=1
        else
            echo "    $HTTP_KITROOT/$KIT_REV/$bldnum/$kitport"  >> "$KIT_SUMMARY_REPORT"
        fi
    done

    if [ $kiterrs -ne 0 ]; then
        echo "    ERRORS POSTING KITS - ONE OR MORE KITS INVALID"  >> "$KIT_SUMMARY_REPORT"
    fi

    echo " "  >> "$KIT_SUMMARY_REPORT"

    ##note the build number, status, etc in top level dir:
    touch -f "$bldnumfile" && chmod +w "$bldnumfile"
    if [ $? -eq 0 ]; then
        #tmp1=`date '+%Y%m%d%H%M%S'`
        tmp1=$UCVSUPDATETIME
        tmp2=`logname`
        #example:  Build070502_1 0 20070502120050
        echo $bldnum $kiterrs $tmp1 $tmp2 >> "$bldnumfile"
        bldmsg -mark Updated bldnum file, $bldnumfile

    else
        bldmsg -error -p $p/make_release "Failed to update bldnum file, '$bldnumfile'."
        kiterrs=1
    fi

    if [ $kiterrs -eq 0 ]; then
        #put the bldnum in the release.ready file
        bldmsg -mark "Creating $SETRELEASEREADY file."
        echo $bldnum > $SETRELEASEREADY
    fi


    return $kiterrs
}

make_addons()
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    kitbase=$KIT_DISTROOT/$KIT_REV
    bldnum=Build$BLDNUM
    make_addons_errs=0

    ##create jbi-components-installer.jar
    bldmsg -mark -p $p/make_addons create addon installer

    cd $SRCROOT/installers/addons

    cmd="mvn $MAVEN_OPTIONS $ARG_PROPS $SMVN_PROPS -Dmaven.test.skip=true clean package"
    bldmsg -mark -p $p/make_addons $cmd
    eval $cmd
    if [ $? -ne 0 ]; then
        bldmsg -error -p $p/make_addons "'$cmd' FAILED"
        make_addons_errs=1
    fi

    cd $SRCROOT/installers/addons/bld

    cmd="zip -r addons.zip jbi_components_installer.jar"
    bldmsg -mark -p $p/make_addons $cmd
    eval $cmd
    if [ $? -ne 0 ]; then
        bldmsg -error -p $p/make_addons "'$cmd' FAILED"
        make_addons_errs=1
    fi

    cmd="jar cvfM addons-0.2-${BLDNUM}.jar addons.zip"
    bldmsg -mark -p $p/make_addons $cmd
    eval $cmd
    if [ $? -ne 0 ]; then
        bldmsg -error -p $p/make_addons "'$cmd' FAILED"
        make_addons_errs=1
    fi

    cd $SRCROOT/installers/addons

    ########
    #release addon installer jar:
    ########
    bom=addons.bom
    addonsdir=$kitbase/$bldnum/installers
    cmd="release -bomloc $SRCROOT/rl/src/cmn/bom -ver $BLDNUM -nolog -checksum -w $addonsdir $bom"
    bldmsg -mark -p $p/make_addons $cmd
    eval $cmd
    if [ $? -ne 0 ]; then
        bldmsg -error -p $p/make_addons "'$cmd' FAILED"
        make_addons_errs=1
    else
       echo "    $HTTP_KITROOT/$KIT_REV/$bldnum/installers"  >> "$KIT_SUMMARY_REPORT"

       #create a symlink to latest kit:
       if [ -r $kitbase/latest ]; then
           mv $kitbase/latest $kitbase/.previouslatest
       else
           rm -f $kitbase/latest
       fi
       bldmsg -mark ln -s $bldnum "$kitbase/latest"
       ln -s $bldnum "$kitbase/latest"
    fi

    return $make_addons_errs
}

push_kit()
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    push_kit_errs=0

    pushkitsrc="$PUSHKIT_SRC/$bldnum/cmn/ojc"

    if [ $DOADDONS -eq 1 ]; then
        pkinstallersrc="$PUSHKIT_SRC/$bldnum/installers"
    else
        pkinstallersrc=
    fi

    #Make sure source location is reachable
    if [ ! -d "$pushkitsrc" ]; then
        bldmsg -error -p $p/pushkit "can not access source dir '$pushkitsrc' - ABORT"
        push_kit_errs=1
        return $push_kit_errs
    fi

    #####
    #test the ssh connection, by copying a file from local /tmp to remote /tmp:
    #####
    _pushkit_test=/tmp/pushkit_sshtest.$$
    touch -f $_pushkit_test
    scp -B $_pushkit_test ${PUSHKIT_IDENTITY}:$_pushkit_test
    if [ $? -ne 0 ]; then
        bldmsg -error -p $p/push_kit "Cannot copy test file '$_pushkit_test' using ssh identity '$PUSHKIT_IDENTITY' - ABORT"
        push_kit_errs=1
        rm -f $_pushkit_test
        return $push_kit_errs
    fi


    ###########
    #connection okay - remove local & remote test files, create destination directory:
    ###########
    rm -f $_pushkit_test
    cmd="ssh $PUSHKIT_IDENTITY 'rm -f $_pushkit_test && mkdir -p $PUSHKIT_DEST/$bldnum'"
    bldmsg -mark -p $p/pushkit "$cmd"
    eval "$cmd"
    status=$?
    if [ $status -ne 0 ]; then
        bldmsg -error -p $p/push_kit "$cmd FAILED"
        push_kit_errs=1
        return $push_kit_errs
    fi

    #copy kit.  note -B option is for "batch", and prevents looping for password entry:
    cmd="scp -B -pr $pushkitsrc $pkinstallersrc ${PUSHKIT_IDENTITY}:$PUSHKIT_DEST/$bldnum"
    bldmsg -mark -p $p/pushkit "$cmd"
    eval $cmd
    status=$?
    if [ $status -ne 0 ]; then
        bldmsg -error -p $p/push_kit "$cmd FAILED"
        push_kit_errs=1
        return $push_kit_errs
    else
        #copy was successful. Add kit locations to the kits summary report:
        cat << MSG_EOF >> "$KIT_SUMMARY_REPORT"

EXTERNAL:

    $HTTP_PUSHKIT_ROOT/$PUSHKIT_REL_PATH/$bldnum
MSG_EOF
    fi

    ####
    # Create latest symlink in PUSHKIT_DEST directory
    ####
    cmd="ssh $PUSHKIT_IDENTITY 'cd $PUSHKIT_DEST && rm -f .previouslatest && ((test -d latest &&  mv latest .previouslatest) || rm -f latest) && ln -s $bldnum latest'"
    bldmsg -mark -p $p/pushkit "$cmd"
    eval $cmd
    status=$?
    if [ $status -ne 0 ]; then
        bldmsg -error -p $p/push_kit "$cmd FAILED"
        push_kit_errs=1
        return $push_kit_errs
    fi

    return $push_kit_errs
}

make_installer()
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    make_installer_errs=0

    #primary port builds all installers:
    if [ $I_AM_PRIMARY -eq 1 ]; then
        kitports="`bldhost -a -kitports -port`"
    else
        kitports=$REGRESS_FORTE_PORT
    fi

    products=$REGRESS_PRODUCT

    #remove the installer build area:
    bldmsg -p $p/make_installer -mark removing shasta installer build area in $SRCROOT/installers/gui/bld
    rm -rf $SRCROOT/installers/gui/bld

    kitbase=$KIT_DISTROOT/$KIT_REV
    bundle_status=0

    for kitport in $kitports
    do
        for prod in $products
        do
            kitdir=$kitbase/$bldnum/$kitport/$prod
            cdromdir=$kitbase/$bldnum/CDROM/$kitport/$prod
            bundledir=$kitbase/$bldnum/BUNDLES/$kitport/$prod
            packagedir=$kitbase/$bldnum/NATIVE-PACKAGES/$kitport/$prod

            make_gui_installer "$prod" "$kitport" "$kitdir" "$cdromdir"
            if [ $? -ne 0 ]; then
                bldmsg -error -p $p/make_installer make_gui_installer FAILED for $kitport
                make_installer_errs=1
            fi

            make_bundle "$prod" "$kitport" "$kitdir" "$cdromdir" "$bundledir"
            if [ $? -ne 0 ]; then
                bldmsg -error -p $p/make_installer make_bundle FAILED for $kitport
                make_installer_errs=1
            fi

        done
    done

    ####
    # Create latest symlink in kits directory
    ####
    cmd="rm ${kitbase}/.previouslatest"
    bldmsg -mark removing .previouslatest symlink - `echo $cmd`
    eval $cmd
    cmd="mv ${kitbase}/latest ${kitbase}/.previouslatest"
    bldmsg -mark moving latest symlink to .previouslatest symlink - `echo $cmd`
    eval $cmd
    cmd="ln -s ${kitbase}/${bldnum} ${kitbase}/latest"
    bldmsg -mark adding new latest symlink - `echo $cmd`
    eval $cmd
    ####

    return $make_installer_errs
}

make_gui_installer()
#Usage:  make_gui_installer kit_port kitdir cdromdir
{
    product="$1"
    kitport="$2"
    kitdir="$3"
    cdromdir="$4"
    if [ "$product" = "" -o "$kitport" = "" -o "$kitdir" = "" -o "$cdromdir" = "" ]; then
        bldmsg -error -p $p/make_gui_installer "Usage: make_gui_installer product kit_port kitdir cdromdir"
        return 1
    fi

    make_gui_installer_errs=0

    cd $SRCROOT/installers/gui
    cmd="ant $VERBOSE_ARG -Dregress_product=$prod -Dkitport=$kitport -Dkitlocation=$kitdir build-kit"
    # cmd="echo build-kit"
    bldmsg -mark -p $p/make_gui_installer `echo $cmd`

    eval $cmd
    status=$?
    if [ $status -ne 0 ]; then
        bldmsg -error -p $p/make_gui_installer Building installer for $prod FAILED
        make_gui_installer_errs=1
    fi

    ########
    #release installer to cdrom directory:
    ########
    bom=${product}_installer.bom

    bldmsg -mark -p $p/make_gui_installer Release $bom to $cdromdir

    release -bomloc $SRCROOT/rl/src/cmn/bom -ver $BLDNUM -log -checksum -w $cdromdir -port $kitport $bom
    status=$?

    if [ $status -ne 0 ]; then
        bldmsg -error -p $p/make_gui_installer Release of $bom to $cdromdir FAILED
        make_gui_installer_errs=1
    fi

    return $make_gui_installer_errs
}

make_mavenarchive()
#Usage:  make_mavenarchive
{

    make_mavenarchive_errs=0

    kitbase=${KIT_DISTROOT}/${KIT_REV}
    bldnum=Build${BLDNUM}
    maven_kitbase=${kitbase}/${bldnum}/maven


    bldmsg -mark -p $p/make_mavenarchive Archiving m2 to bld/release/m2.zip using maven_archive.bom
    cd $SRCROOT
    rm -rf $SRCROOT/bld/release/m2.zip
    zip -r $SRCROOT/bld/release/m2.zip m2

    bldmsg -mark -p $p/make_mavenarchive Release maven_archive.bom to $maven_kitbase
    installdir $maven_kitbase
    release -bomloc $SRCROOT/rl/src/cmn/bom -ver $BLDNUM -nolog -checksum -w $maven_kitbase maven_archive.bom
    status=$?
    if [ $status -ne 0 ]; then
        bldmsg -error -p $p/make_mavenarchive releasing m2.zip via maven_archive.bom FAILED
        make_mavenarchive_errs=1
    fi

    return $make_mavenarchive_errs
}


make_bundle()
#Usage:  make_bundle  "$prod" "$kitport" "$kitdir" "$cdromdir" "$bundledir"
{
    product="$1"
    kitport="$2"
    kitdir="$3"
    cdromdir="$4"
    bundledir="$5"
    if [ -z "$product" -o -z "$kitport" -o -z "$kitdir" -o -z "$cdromdir" -o -z "$bundledir" ]; then
        bldmsg -error -p $p/make_bundle "Usage: make_bundle product kit_port kitdir cdromdir bundledir"
        return 1
    fi

    if [ $DEBUG -ne 0 ]; then
        cat << EOF
IN make_bundle:
    product=>$product<
    kitport=>$kitport<
    kitdir=>$kitdir<
    cdromdir=>$cdromdir<
    bundledir=>$bundledir<
EOF
    fi

    # This command sets version variables loaded from codeline.pl that are needed
    export FULL_VERSION_UL SHORT_PRODUCT_NAME
    eval `bldhost -product $product -versionproperties | egrep '^FULL_VERSION_UL=|^SHORT_PRODUCT_NAME=' `

    make_bundle_errs=0

    #clean output directory:
    stagedir=$SRCROOT/installers/gui/bld/installer/$product
    installdir $stagedir

    realport=`map_regress_port $kitport`
    bundleport=$realport

    cd $cdromdir
    if [ $realport = 'nt' ]; then
        rm -f $stagedir/${SHORT_PRODUCT_NAME}_${FULL_VERSION_UL}_windows.zip
        jar -Mcf $stagedir/${SHORT_PRODUCT_NAME}_${FULL_VERSION_UL}_windows.zip *
    else
        [ "$realport" = "solsparc" ] && bundleport="solaris-sparc"
        [ "$realport" = "solx86" ] && bundleport="solaris-x86"

        rm -f $stagedir/${SHORT_PRODUCT_NAME}_${FULL_VERSION_UL}_${bundleport}.tar\
            ${SHORT_PRODUCT_NAME}_${FULL_VERSION_UL}_${bundleport}.tar.zip

        if [ "$realport" = "solsparc" -o "$realport" = "solx86" ]; then
            #use solaris tar:
            if [ "$FORTE_PORT" != "solsparc" -a "$FORTE_PORT" != "solsparc" ]; then
                bldmsg -warn -p $p/make_bundle "creating solaris tar on non-solaris primary build host.  Output is invalid.  We assume you are testing."
            fi
            /bin/tar -cf $stagedir/${SHORT_PRODUCT_NAME}_${FULL_VERSION_UL}_${bundleport}.tar *
        else
            #use gnu tar ($TOOLROOT version):
            tar -cf $stagedir/${SHORT_PRODUCT_NAME}_${FULL_VERSION_UL}_${bundleport}.tar *
        fi

        cd $stagedir
        jar -Mcf ${SHORT_PRODUCT_NAME}_${FULL_VERSION_UL}_${bundleport}.tar.zip ${SHORT_PRODUCT_NAME}_${FULL_VERSION_UL}_${bundleport}.tar
    fi

    bom=${product}_bundle.bom
    bldmsg -mark -p $p/make_bundle Release $bom to $bundledir

    release -bomloc $SRCROOT/rl/src/cmn/bom -ver $BLDNUM -log -checksum -w $bundledir -port $realport $bom
    status=$?

    if [ $status -ne 0 ]; then
        bldmsg -error -p $p/make_bundle Release of $bom to $KIT_DISTROOT FAILED
        make_bundle_errs=1
    fi

    #maven_kitbase=$bundledir/../../..

    #if [ "$realport" = "solsparc" ]; then
    #  bldmsg -mark -p $p/make_bundle Archiving m2 to bld/m2.zip
    #  cd $SRCROOT
    #  rm -rf $SRCROOT/bld/m2.zip
    #  jar -Mcf $SRCROOT/bld/m2.zip m2
    #  bldmsg -mark -p $p/make_bundle Release maven_archive.bom to $maven_kitbase
    #  installdir $maven_kitbase
    #  release -bomloc $SRCROOT/rl/src/cmn/bom -ver $BLDNUM -log -checksum -w $maven_kitbase -port $realport maven_archive.bom
    #fi


    return $make_bundle_errs
}

run_unit_tests()
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    bldmsg "BUILDRESULTS_TYPE=jbi_regress"

    cd $SRCROOT
    run_tests_status=0

    runSystemTests=; [ $DOJUNITSYSTESTS -eq 1 ] && runSystemTests=-DrunSystemTests

    ######
    #junit (and junitreport happens automatically here)
    ######
    bldmsg -markbeg ${p}:junit
    #NOTE:  changing target to install to work-around maven lifecycle bug that evokes
    #errors in the maven-dependency plugin in packaging projects when running "test" or "test-compile".
    #RT 2/2/07
    #
    #Changing target to package, to see if we can avoid writing on local repos again while
    #potentially other tasks are running.  RT 2/13/07
    #
    cmd="mvn $MAVEN_OPTIONS $ARG_PROPS $SMVN_PROPS -Dmaven.test.skip=false $runSystemTests package"
    bldmsg -mark -p $p/run_tests `echo $cmd`
    eval $cmd
    status=$?
    if [ $status -ne 0 ]; then
        run_tests_status=1
        bldmsg -error -p $p/run_tests Junit test step FAILED
    fi
    bldmsg -markend -status $status ${p}:junit
    bld_reset_watchdog

    ############
    #junitreport (archival; report is done above, in "test" step)
    ############

    #archive junit report:
    if [ -d "$SRCROOT/bld/junit" ]; then
        bldmsg -mark -p $p/run_tests Archive junit test results
        cp -rp $SRCROOT/bld/junit $LOGDIR
        if [ $? -ne 0 ]; then
            run_tests_status=1
            bldmsg -error -p $p/run_tests Archive junit test results FAILED
        fi
    fi
    bld_reset_watchdog

    return $run_tests_status
}

run_system_tests()
{
    if [ $TESTMODE -eq 1 ]; then
        return 0
    fi

    bldmsg "BUILDRESULTS_TYPE=jbi_regress"

    bldmsg -markbeg ${p}:jregress

    cd $SRCROOT
    run_tests_status=0

    #########
    #jregress
    #########

    MAVEN_GOALS="-DrunFullBuild=0 -Djregress=1 integration-test"

    cmd="mvn $MAVEN_OPTIONS $ARG_PROPS $SMVN_PROPS -Dmaven.test.skip=true $MAVEN_GOALS"
    bldmsg -mark -p $p/run_system_tests `echo $cmd`

    eval $cmd
    run_tests_status=$?

    bldmsg -markend -status $run_tests_status ${p}:jregress

    return $run_tests_status
}

build_summary()
#create a summary status of the test step, if tests were run
{
    rm -f "$TEST_SUMMARY_REPORT"
    touch "$TEST_SUMMARY_REPORT"

    jdk_version=`2>&1 java -version | grep 'Java(TM)'`
    appserver_version=`2>&1 asadmin version --verbose=true --terse=true`
    date=`date`
    host=`uname -n`
    oesb_version=`2>&1 grep BUILD_NUMBER $GFBASE/jbi/jbi.ver`
    ojc_version=`2>&1 grep BUILD_NUMBER $GFBASE/jbi/ojc.ver`

    #write the summary header:
    cat >> "$TEST_SUMMARY_REPORT" << EOF
TEST RESULTS $date ($host/$FORTE_PORT/${BLDNUM}):
    $jdk_version
    Appserver $appserver_version
    Open-ESB $oesb_version

EOF

    # if no tests were run ...
    if [ $DOREGRESS -eq 0 ]; then
        cat >> "$TEST_SUMMARY_REPORT" << EOF
NO TESTS RUN

EOF
    ##
    else
        #... we ran some tests:
        junit_summary="NO JUNIT TEST RESULTS"
        if [ -r "$UNITTESTLOG"  ]; then
            junit_summary=`sed -n -e "/junit_summary/,/junit.failure.list/p" "$UNITTESTLOG" | grep 'junit[\._]'`
            [ "$junit_summary" = "" ] && junit_summary="NO JUNIT TEST RESULTS"
        fi

        jregress_summary=`get_jregress_summary`

        cat >> "$TEST_SUMMARY_REPORT" << EOF
$junit_summary

$jregress_summary

EOF
    ##
    fi
}

get_jregress_summary()
{
    echo jregress_summary:

    ant -f $JV_SRCROOT/global-common/m2.ant -DSRCROOT=$JV_SRCROOT jregress_report | grep '[0-9] tests '
    if [ ! -r "$SRCROOT/bld/regress.summary" ]; then
        echo "    NO JREGRESS TEST RESULTS"
        return 0
    fi
}

run_background_tasks()
#start a thread to run supplemental build tasks in parallel with regression
#WARNING:  we cannot set any vars in parent process, so we use shprops to
#communicate results.
{
    #note - we add the begin/end marks here
    #so we know the actual time the background tasks take,
    #(otherwise, get the run_tests times).
    bldmsg -markbeg ${p}:background tasks

    BG_BUILD_STATUS=0
    shprops -set $BG_RESULTS BG_BUILD_STATUS=$BG_BUILD_STATUS

    if [ $DOSRCBUNDLES -eq 1 ]; then
        bldmsg -mark Making source bundles - Log is $SRCBUNDLELOG
        bldmsg -markbeg ${p}:make_src_bundles
        make_src_bundles >> $SRCBUNDLELOG 2>&1
        status=$?
        if [ $status -ne 0 ]; then
            bldmsg -error -p $p make_src_bundles failed. Check $SRCBUNDLELOG for errors.
            BG_BUILD_STATUS=1
            shprops -set $BG_RESULTS BG_BUILD_STATUS=$BG_BUILD_STATUS
        fi
        bldmsg -markend -status $status ${p}:make_src_bundles
    fi

    if [ $DOJAVADOC -eq 1 ]; then
        bldmsg -mark Building javadoc - Log is $JAVADOCLOG
        bldmsg -markbeg ${p}:javadoc
        build_javadoc >> $JAVADOCLOG 2>&1
        status=$?
        if [ $status -ne 0 ]; then
            bldmsg -error -p $p build_javadoc failed. Check $JAVADOCLOG for errors.
            BG_BUILD_STATUS=1
            shprops -set $BG_RESULTS BG_BUILD_STATUS=$BG_BUILD_STATUS
        fi
        bldmsg -markend -status $status ${p}:javadoc
        bld_reset_watchdog
    fi

    release_failed=0
    if [ $DORELEASE -eq 1 -a $BUILD_FAILED -eq 0 ]; then
        bldmsg -mark Releasing $PRODUCT - Log is $RELEASELOG
        bldmsg -markbeg ${p}:make_release
        make_release >> $RELEASELOG 2>&1
        status=$?
        if [ $status -ne 0 ]; then
            bldmsg -error -p $p make_release failed. Check $RELEASELOG for errors.
            release_failed=1
            BG_BUILD_STATUS=1
            shprops -set $BG_RESULTS BG_BUILD_STATUS=$BG_BUILD_STATUS
        fi
        bldmsg -markend -status $status ${p}:make_release
        bld_reset_watchdog
    elif [ $DORELEASE -eq 1 -a $BUILD_FAILED -ne 0 ]; then
        bldmsg -error Skipping make_release because build step failed
    fi

    addons_failed=0
    if [ $DOADDONS -eq 1 ]; then
        if [ $release_failed -eq 0 -a $BUILD_FAILED -eq 0 ]; then
            bldmsg -mark creating jbi-components-installer.jar - Log is $ADDONSLOG
            bldmsg -markbeg ${p}:make_addons
            make_addons >> $ADDONSLOG 2>&1
            status=$?
            if [ $status -ne 0 ]; then
                bldmsg -error -p $p make_addons failed. Check $ADDONSLOG for errors.
                addons_failed=1
                BG_BUILD_STATUS=1
                shprops -set $BG_RESULTS BG_BUILD_STATUS=$BG_BUILD_STATUS
            fi
            bldmsg -markend -status $status ${p}:make_addons
        else
             bldmsg -error Skipping make_addons because release or build step failed
        fi
        bld_reset_watchdog
    fi

    pushkit_failed=0
    if [ $DOPUSHKIT -eq 1 -a $release_failed -eq 0 -a $BUILD_FAILED -eq 0 ]; then
        bldmsg -mark Pushing kits of $PRODUCT to mirror- Log is $PUSHKITLOG
        bldmsg -markbeg ${p}:push_kit
        push_kit >> $PUSHKITLOG 2>&1
        status=$?
        if [ $status -ne 0 ]; then
            bldmsg -error -p $p push_kit failed. Check $PUSHKITLOG for errors.
            pushkit_failed=1
            BG_BUILD_STATUS=1
            shprops -set $BG_RESULTS BG_BUILD_STATUS=$BG_BUILD_STATUS
        fi
        bldmsg -markend -status $status ${p}:push_kit
        bld_reset_watchdog
    elif [ $DOPUSHKIT -eq 1 -a $DORELEASE -eq 1 -a $BUILD_FAILED -ne 0 ]; then
        bldmsg -error Skipping push_kit because build or release step failed
    fi

    mavendeploy_failed=0
    if [ $DOMAVENDEPLOY -eq 1 -a $release_failed -eq 0 -a $BUILD_FAILED -eq 0 ]; then
        bldmsg -mark Deploying maven artifacts to mirror - Log is $MAVENDEPLOYLOG
        bldmsg -markbeg ${p}:maven_deploy
        maven_deploy >> $MAVENDEPLOYLOG 2>&1
        status=$?
        if [ $status -ne 0 ]; then
            bldmsg -error -p $p maven_deploy failed. Check $MAVENDEPLOYLOG for errors.
            mavendeploy_failed=1
            BG_BUILD_STATUS=1
            shprops -set $BG_RESULTS BG_BUILD_STATUS=$BG_BUILD_STATUS
        fi
        bldmsg -markend -status $status ${p}:maven_deploy
        bld_reset_watchdog
    elif [ $DOMAVENDEPLOY -eq 1 -a $DORELEASE -eq 1 -a $BUILD_FAILED -ne 0 ]; then
        bldmsg -error Skipping maven_deploy because build or release step failed
    fi

    mavenarchive_failed=0
    if [ $DOMAVENARCHIVE -eq 1 -a $release_failed -eq 0 -a $BUILD_FAILED -eq 0 ]; then
        bldmsg -mark Doing maven archive for $PRODUCT - Log is $MAVENARCHIVELOG
        bldmsg -markbeg ${p}:make_mavenarchive
        make_mavenarchive >> $MAVENARCHIVELOG 2>&1
        status=$?
        if [ $status -ne 0 ]; then
            bldmsg -error -p $p make_mavenarchive failed. Check $MAVENARCHIVELOG for errors.
            mavenarchive_failed=1
            BG_BUILD_STATUS=1
            shprops -set $BG_RESULTS BG_BUILD_STATUS=$BG_BUILD_STATUS
        fi
        bldmsg -markend -status $status ${p}:make_mavenarchive
        bld_reset_watchdog
    elif [ $DOMAVENARCHIVE -eq 1 -a $DORELEASE -eq 1 -a $BUILD_FAILED -ne 0 ]; then
        bldmsg -error Skipping make_mavenarchive because build step failed
    fi

    # note - do this AFTER the installer build.  We may want to package javadoc,
    # but xref is for internal use only.
    if [ $DOXREF -eq 1 ]; then
        bldmsg -mark Creating cross-referenced source trees - Log is $XREFLOG
        bldmsg -markbeg ${p}:xref
        build_xref_maven >> $XREFLOG 2>&1
        status=$?
        if [ $status -ne 0 ]; then
            bldmsg -error -p $p build_xref_maven failed. Check $XREFLOG for errors.
            BG_BUILD_STATUS=1
            shprops -set $BG_RESULTS BG_BUILD_STATUS=$BG_BUILD_STATUS
        fi
        bldmsg -markend -status $status ${p}:xref
        bld_reset_watchdog
    fi

    bldmsg -markend -status $BG_BUILD_STATUS ${p}:background tasks
}

#################################### MAIN #####################################

p=`basename $0`
saveargs="$@"
BUILD_STATUS=0

#source common build routines or die.
#NOTE - all "bld_<name>" routines come from this module:
require bldcmn.sh

parse_args "$@"
if [ $? -ne 0 ]; then
    echo
fi

#if asking for help, display help of all scripts called by this one.
if [ $DOHELP -eq 1 ]; then
    #none written yet...
    exit 0
fi

show_options

#make sure we have a /tmp dir on nt:
bld_check_create_tmp

bld_check_usepath
if [ $? -ne 0 ]; then
    bld_fatal_error "one or more usePathDef variables not set - ABORT"
fi

if [ $CALLEDFROMWRAPPER -eq 0 ]; then
    setup_wrapper_env
fi

set_global_vars
if [ $? -ne 0 ]; then
    bld_fatal_error "Error setting global variables.  Message should have been provided. - ABORT"
fi

check_local_vars
if [ $? -ne 0 ]; then
    bld_fatal_error "one or more required environment variables not set - ABORT"
fi

show_build_environment

#this will get updated at the end of the build.
#if it does not, then program was interrupted or crashed:
shprops -set $BLDPARMS BUILD_STATUS=1

bldmsg -markbeg $p
#don't trap interrupts until initial start message.
trap rec_signal 2 15

clear_local_locks

#this updates the runBuild wrapper watchdog timer if it exists:
bld_reset_watchdog

if [ $DOMAVENCLEAN -eq 1 ]; then
    bldmsg -mark Removing maven local repository in $SRCROOT/m2
    bldmsg -markbeg ${p}:clean_maven
    clean_maven
    status=$?
    if [ $status -ne 0 ]; then
        bldmsg -error -p $p clean_maven failed.
        BUILD_STATUS=1
    fi
    bldmsg -markend -status $status ${p}:clean_maven
    bld_reset_watchdog
fi

if [ $DOCLEAN -eq 1 ]; then
    bldmsg -mark Cleaning build areas - log is $CLEANLOG
    bldmsg -markbeg ${p}:clean_build_tree
    clean_build_tree >> $CLEANLOG 2>&1
    status=$?
    if [ $status -ne 0 ]; then
        bldmsg -error -p $p clean_build_tree failed. Check $CLEANLOG for errors.
        BUILD_STATUS=1
    fi

    #reduce download noise:
    filter_maven_log_in_place $CLEANLOG

    bldmsg -markend -status $status ${p}:clean_build_tree
    bld_reset_watchdog
fi

#set the update time, which is used by the update task and the integration report task
set_update_time
status=$?

if [ $status -ne 0 ]; then
    bld_fatal_error "CANNOT set CVS update time - ABORT"
fi

if [ $DOUPDATE -eq 1 ]; then
    bldmsg -mark Updating source code - log is $UPDATELOG
    bldmsg -markbeg ${p}:update_src
    update_src >> $UPDATELOG 2>&1
    status=$?
    bldmsg -markend -status $status ${p}:update_src
    if [ $status -ne 0 ]; then
        bldmsg -error -p $p update_src failed. Check $UPDATELOG for errors.
        BUILD_STATUS=1

        ### abort entire build if RE build, since we cannot issue kits
        ### built from a corrupted working directory:
        if [ $RELEASE_BUILD -eq 1 ]; then
            bldmsg -error -p $p BUILD ABORTED DUE TO CVS UPDATE ERRORS.
            bldmsg -markend -status $BUILD_STATUS $p
            exit $BUILD_STATUS
        fi
    else
        #temporarily ignore m2 files, so we can get meaningful buildResults.  RT 7/24/06
        mv $UPDATELOG ${UPDATELOG}.tmp
        egrep -v '/pom.xml$|/settings.xml$|/m2.ant$' ${UPDATELOG}.tmp > $UPDATELOG
        rm -f ${UPDATELOG}.tmp
    fi
    bld_reset_watchdog
fi

#run integration report *after* source update, to give commitd time to catch up.
if [ $DOINTEGREPORT -eq 1 ]; then
    #run integration report:
    bldmsg -mark Creating integration report - output is $INTEGRATIONLOG
    bldmsg -markbeg ${p}:$INTEG_REPORT
    $INTEG_REPORT -o $INTEGRATIONLOG $UCVSUPDATETIME
    if [ $? -ne 0 ]; then
        bldmsg -error ${p}:$INTEG_REPORT failed for $PRODUCT repostiorty.
        status=1
    fi

    $INTEG_REPORT -tools -o $TMPA $UCVSUPDATETIME
    if [ $? -ne 0 ]; then
        bldmsg -error ${p}:$INTEG_REPORT failed for devtools repostiorty.
        status=2
    else
        cat $TMPA >> $INTEGRATIONLOG
        rm -f $TMPA
    fi

    bldmsg -markend -status $status ${p}:$INTEG_REPORT


    bld_reset_watchdog
fi

BUILD_FAILED=0
if [ $DOBUILD -eq 1 ]; then
    bldmsg -mark Building $PRODUCT - Log is $BUILDLOG
    bldmsg -markbeg ${p}:build_product
    build_product >> $BUILDLOG 2>&1
    status=$?

    #reduce download noise:
    filter_maven_log_in_place $BUILDLOG

    #this additional check is necessary because we tell ant to ignore various errors,
    #so ant always returns a zero status.
    if [ $status -eq 0 ]; then
        has_ant_errors $BUILDLOG
        status=$?
    fi

    if [ $status -ne 0 ]; then
        bldmsg -error -p $p build_product failed. Check $BUILDLOG for errors.
        BUILD_STATUS=1
        BUILD_FAILED=1

        #if -keepon specified ...
        if [ $KEEPON -eq 1 ]; then
            #.. then pretend the compile didn't fail:
            BUILD_FAILED=0
        fi
    fi
    bldmsg -markend -status $status ${p}:build_product
    bld_reset_watchdog
fi

###########
#BACKGROUND misc. tasks while we run tests:
###########
export BG_LOG BG_RESULTS
BG_LOG=$LOGDIR/background_tasks.log
BG_RESULTS=$LOGDIR/background_results.sh

#track total time for all tests:
if [ $DOREGRESS -eq 1 -a $BUILD_FAILED -eq 0 ]; then
    bldmsg -mark Testing $PRODUCT - Log is $REGRESSLOG
    bldmsg -markbeg ${p}:run_tests
fi

#
#NOTE:  we can now run junit while background javadoc is running
#       moving these tasks to before junit per RT 5/23/07
#

#####
#FORK background tasks:
#####
run_background_tasks > $BG_LOG &
bgpid=$!

#########
#jregress
#  note - for this build we run jregress first because there are few tests.  RT 10/14/08
#########
if [ $DOJREGRESS -eq 1 -a $BUILD_FAILED -eq 0 ]; then
    run_system_tests >> $REGRESSLOG 2>&1
    status=$?
    bld_reset_watchdog

    #scan for ignored ant task errors:
    if [ $status -eq 0 ]; then
        has_ant_errors $REGRESSLOG
        status=$?
    fi

    if [ $status -ne 0 ]; then
        bldmsg -error -p $p One or more system tests failed. Check $REGRESSLOG for errors.
        BUILD_STATUS=1
    fi
elif [ $DOJREGRESS -eq 1  -a $BUILD_FAILED -ne 0 ]; then
    bldmsg -error Skipping system tests because build step failed
fi

######
#junit
######
if [ $DOJUNIT -eq 1 -a $BUILD_FAILED -eq 0 ]; then
    run_unit_tests >> $UNITTESTLOG 2>&1
    status=$?
    bld_reset_watchdog

    #scan for ignored ant task errors:
    if [ $status -eq 0 ]; then
        has_ant_errors $UNITTESTLOG
        status=$?
    fi

    if [ $status -ne 0 ]; then
        bldmsg -error -p $p One or more unit tests failed. Check $UNITTESTLOG for errors.
        BUILD_STATUS=1
    fi
elif [ $DOJUNIT -eq 1  -a $BUILD_FAILED -ne 0 ]; then
    bldmsg -error Skipping unit tests because build step failed
fi


if [ $DOREGRESS -eq 1 -a $BUILD_FAILED -eq 0 ]; then
    bldmsg -markend -status $status ${p}:run_tests
fi

if [ $DOARCHIVELOGS -eq 1 -a $DOREGRESS -eq 1 -a $BUILD_FAILED -eq 0 ]; then
    bldmsg -markbeg ${p}:archive_logs
    #not much output - so just let it go to main log:
    archive_logs
    status=$?
    if [ $status -ne 0 ]; then
        bldmsg -error -p $p archive_logs failed.
        BUILD_STATUS=1
    fi
    bldmsg -markend -status $status ${p}:archive_logs
    bld_reset_watchdog
fi

#########
#WAIT FOR background tasks if necessary:
#########
wait $bgpid
eval `shprops -get $BG_RESULTS BG_BUILD_STATUS`

bldmsg -mark Results of run_background_tasks tasks follows:
cat $BG_LOG
bldmsg -mark "##### EOF (run_background_tasks)"

#rm -f $BG_LOG $BG_RESULTS

if [ $BG_BUILD_STATUS -ne 0 ]; then
    BUILD_STATUS=1
fi

if [ $BUILD_STATUS -eq 0 ]; then
    #update the last good build time in the build parameters file:
    shprops -set $BLDPARMS ULASTGOODBLDTIME=$UBLDSTARTTIME
fi

shprops -set $BLDPARMS BUILD_STATUS=$BUILD_STATUS

#collect logs and run ant summary report:
build_summary

bldmsg -markend -status $BUILD_STATUS $p

cleanup

exit $BUILD_STATUS
