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
# @(#)needbld.sh - ver 1.1 - 01/04/2006
#
# Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
# 
# END_HEADER - DO NOT EDIT
#

#
# needBuild - script that decides whether or not we need to build.
#   Used by -autobuild function of the runBuild wrapper.
#
#  17-Jun-2002 (russt)
#       Initial revision

################################ USAGE ROUTINES ################################

usage()
{
    status=$1

    cat << EOF
Usage:  $p [-help] [-tools]

SYNOPSIS
  Determine if a build is needed.  If it is, exit with status 0.
  If there are not updates to process, exit with status 1.
  If there were errors, exit with status > 1.

OPTIONS
 -help     Display this message.
 -tools    Wrapper script is building and updating tools repositories.

ENVIRONMENT:

\$LASTBLDPARMS   Build parameter file for this build. (set by wrapper).

EOF

    exit $status

}

parse_args()
{
    DOTOOLS=0
    CALLEDFROMWRAPPER=0

    while [ $# -gt 0 ]
    do
        arg=$1; shift

        case $arg in
        -h* )
            usage 0
            ;;
        -tools )
            DOTOOLS=1
            ;;
        -calledFromWrapper )
            #true if called from runjbiBuild wrapper:
            CALLEDFROMWRAPPER=1
            ;;
        -* )
            echo "${p}: WARNING unknown option, $arg - ignored."
            ;;
        esac
    done
}

################################ INITIALIZATION ################################

check_environment()
{
    localvarerrs=0

    if [ "$LASTBLDPARMS" = "" ]; then
        bldmsg -error -p $p LASTBLDPARMS not set.
        localvarerrs=1
    fi

    if [ "x$RELEASE_BUILD" != "x" ]; then
        if [ $RELEASE_BUILD -eq 1 -a  "$PATHREF" = "" ]; then
            bldmsg -error -p $p PATHDEF not set.
            localvarerrs=1
        fi
    fi

    if [ "$PRODUCT" = "" ]; then
        bldmsg -error -p $p PRODUCT not set.
        localvarerrs=1
    fi

    if [ "$CVS_BRANCH_NAME" = "" ]; then
        bldmsg -error -p $p CVS_BRANCH_NAME not set.
        localvarerrs=1
    fi
    
    if [ $localvarerrs -ne 0 ]; then
        return 1
    fi

    return 0
}

################################# HOUSEKEEPING #################################

cleanup()
{
    rm -f /tmp/${p}*.$$
}

rec_signal()
{
    cleanup
    bldmsg -error -p $p Interrupted
    exit 2
}

################################### INTERNAL ###################################

gen_query_script()
#routine to generate an ant script to query the INF database
{

#select max(f.transtime)

    cat << 'EOF' > $1
<project name="infquery" default="runquery">

<target name="runquery" depends="init">

<sql
    driver="${inf_jdbc_driver}"
    url="${inf_jdbc_url}"
    userid="${inf_jdbc_userid}"
    password="${inf_jdbc_password}"
    print="true"
    showheaders="false"
    output="${integration_report}"
><![CDATA[

    select count(distinct f.noticeId)
    from cvs_filelist f, cvs_repos r, branch b, notice n
        where r.reposId = f.reposId
        and r.repos = '${repos}'
        and b.branchId = n.branchId
        and f.noticeId = n.noticeId
        and b.Name = '${branch}'
        and f.transtime >= '${gmt_begtime}'
        ;
]]>

</sql>

<!--
<echo message="output is in ${integration_report}" />
-->

</target>

<target name="init">
    <!--
    # you must override these properties on the command line
    -->
    <property name="integration_report" value="integrations.txt" />
    <property name="gmt_begtime" value="20030618080000" />
    <!-- this is the INF scm value: -->
    <property name="repos" value="/java.net/cvs/open-jbi-components" />
    <property name="branch" value="ojc{trunk}" />

    <!--
     # read in build database connection properties:
     #  inf_jdbc_driver
     #  inf_jdbc_url
     #  inf_jdbc_userid
     #  inf_jdbc_password
    -->
    <property environment="env"/>
    <property name="srcroot" value="${env.JV_SRCROOT}" />
    <property file="${srcroot}/.build_db_properties"/>

<echo>
    inf_jdbc_driver=${inf_jdbc_driver}
    inf_jdbc_url=${inf_jdbc_url}
    inf_jdbc_userid=${inf_jdbc_userid}
    inf_jdbc_password=${inf_jdbc_password}
</echo>

</target>

</project>
EOF

    return $?
}

##################################### MAIN #####################################

p=`basename $0`

parse_args "$@"

tmpA=/tmp/${p}_A.$$
tmpB=/tmp/${p}_B.$$
tmpC=/tmp/${p}_C.$$
queryscript="$tmpB"
errfile="$tmpC"

#trap interrupts until initial start message.
trap rec_signal 2 15

check_environment
if [ $? -ne 0 ]; then
    bldmsg -error -p $p "One or more required environment variables are not set - ABORT."
    exit 3
fi

gen_query_script $queryscript
if [ $? -ne 0 ]; then
    bldmsg -error -p $p cannot generate query script
    exit 1
fi

PRODUCT_REPOS="/java.net/cvs/open-jbi-components"
product_repos_base="ojc"
TOOLS_REPOS=iis/devtools
tools_repos_base="`basename $TOOLS_REPOS`"

#these times must be in GMT
#NOTE: we use LASTBLDPARMS file because wrapper hasn't created the new one yet.
eval `shprops -get $LASTBLDPARMS`
if [ $? -ne 0 ]; then
    bldmsg -warn -p $p "problem retrieving repository update times from $LASTBLDPARMS"
    bldmsg -warn -p $p "Assuming this is the first -autobuild run."
    cleanup
    exit 0
fi

if [ "$CVS_BRANCH_NAME" = "main" ]; then
    branchname="trunk"
else
    branchname="$CVS_BRANCH_NAME"
fi

if [ $DOTOOLS -eq 1 ]; then
    reposlist="$PRODUCT_REPOS $TOOLS_REPOS "
else
    reposlist="$PRODUCT_REPOS "
fi

# if it's slave build we base our decision to build on primary bldnum
if [ "x$RELEASE_BUILD" != "x" ]; then
    if [ $PRIMARY_PORT != $FORTE_PORT -a $RELEASE_BUILD -eq 1 ]; then
        if [ -f $PRIPATHREF/bldlock/bldenv.sh ]; then
            eval `shprops -get $PRIPATHREF/bldlock/bldenv.sh LASTUPDATETIME`
            export LASTUPDATETIME
            base_repos=`basename $PRODUCT_REPOS`
            if [ $PRODUCT_REPOS = "/java.net/cvs/open-jbi-components" ]; then
              base_repos="jbi"
            fi
            cmd="echo \$${base_repos}_last_update"
            LOCAL_LASTUPDATETIME=`eval $cmd`
            if [ "$LOCAL_LASTUPDATETIME" = "$LASTUPDATETIME" ]; then
                echo "Slave and Primary insync - NO BUILD REQUIRED"
                cleanup
                exit 1
            else
                echo "BUILD REQUIRED"
                cleanup
                exit 0
            fi
        else
            bldmsg -mark -warn -p $p "bldenv.sh file not present in PRIMARY bldlock path"
            exit 2
        fi
    fi
fi

#foreach repos ...
for repos  in $reposlist
do
    inf_repos=$repos
    base_repos=`basename $repos`

    if [ $repos =  "/java.net/cvs/open-jbi-components" ]; then
        base_repos="jbi"
        inf_branch="ojc{trunk}"
    else
        inf_branch="`echo $base_repos{$branchname}`"
    fi

    #this var is set from LASTBUILDPARMS file eval above:
    if [ "$repos" = "$PRODUCT_REPOS" ]; then
        STARTTIME="$jbi_last_update"
    else
        cmd="echo \$${base_repos}_last_update"
        STARTTIME=`eval $cmd`
    fi


    cmd="ant -quiet -Dintegration_report=$tmpA -Dgmt_begtime=$STARTTIME -Drepos=$inf_repos -Dbranch=$inf_branch -f $queryscript"
    bldmsg -mark -p $p Running $cmd
    eval $cmd > $errfile 2>&1
    status=$?

    if [ $status -eq 0 ]; then
        changecount=`head -1 $tmpA`
        echo Integration count for repos $inf_repos, branch $inf_branch is $changecount
        if [ "$changecount" -gt 0 ]; then
            echo BUILD REQUIRED
            cleanup
            exit 0
        fi
    else
        #if there are errors, we exit with status 2
        bldmsg -p $p -error -status $status ant sql query failed for repos $inf_repos
        cat $errfile
        cleanup
        exit 2
    fi
done

cleanup

#no updates to process, and no errors running queries - exit with status 1.
exit 1
