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
# @(#)integreport.sh - ver 1.1 - 01/04/2006
#
# Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
# 
# END_HEADER - DO NOT EDIT
#

#
# integrationReport - retrieve integration data over update interval for named
#                     repository.
#
#  02-Aug-2002 (russt)
#       Initial revision

################################ USAGE ROUTINES ################################

usage()
{
    status=$1

    cat << EOF
Usage:  $p [-help] [-tools] [-o outfile] [to_gmt]

SYNOPSIS
  Retrieve integration data over update interval for named repository.
  output is generated in comma separated format.  If the <to_gmt> is given,
  then the report is generated for updates from the last update time
  to the time given, which must be GMT in the form:  yyyymmddhhmmss.


OPTIONS
 -help     Display this message.
 -tools    generate report for tools repository instead of product repository.

ENVIRONMENT:

\$BLDPARMS       Build parameter file for this build. (set by wrapper).
\$LASTBLDPARMS   Build parameter file for last build. (set by wrapper).

EOF

    exit $status

}

parse_args()
{
    DOTOOLS=0
    DOOUTPUT=0
    OUTPUTFILE=""
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
        -o )
            DOOUTPUT=1
            if [ $# -gt 0 ]; then
                OUTPUTFILE=$1; shift
            else
                echo "${p}: -o requires the name of an output file."
                usage 1
            fi
            ;;
        -calledFromWrapper )
            #true if called from runjbiBuild wrapper:
            CALLEDFROMWRAPPER=1
            ;;
        -* )
            echo "${p}: WARNING unknown option, $arg - ignored."
            ;;
        * )
            ENDTIME=$arg
            ;;
        esac
    done

#echo ENDTIME is $ENDTIME

}

################################ INITIALIZATION ################################

check_environment()
{
    localvarerrs=0

    if [ "$LASTBLDPARMS" = "" ]; then
        bldmsg -error -p $p LASTBLDPARMS not set.
        localvarerrs=1
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

gen_integration_query()
#routine to generate an ant script to query for integrations
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

    select
        n.noticeId,
        u.name,
        u.emailAddr,
        r.repos,
        b.name,
        f.transtime,
        f.old_rev,
        f.new_rev,
        f.opt_type,
        f.cvstype,
        f.cvsdir,
        f.cvsfile,
        n.Subject
    from cvs_filelist f, cvs_repos r, branch b, notice n, user_profile u
        where r.reposId = f.reposId
        and r.repos = '${repos}'
        and b.branchId = n.branchId
        and f.noticeId = n.noticeId
        and u.userId = n.userId
        and b.Name = '${branch}'
        and f.transtime >= '${gmt_begtime}'
        and f.transtime <= '${gmt_endtime}'
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
    <property name="gmt_endtime" value="20030618080000" />
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

if [ "$CVS_BRANCH_NAME" = "main" ]; then
    branchname="trunk"
else
    branchname="$CVS_BRANCH_NAME"
fi

PRODUCT_REPOS="/java.net/cvs/open-jbi-components"
TOOLS_REPOS=iis/devtools

if [ $DOTOOLS -eq 1 ]; then
    theRepos="`basename $TOOLS_REPOS`"
    inf_repos="$TOOLS_REPOS"
    inf_branch="$theRepos{$branchname}"
else
    inf_repos="$PRODUCT_REPOS"
    inf_branch="ojc{trunk}"

    #this is used to get the update time:
    if [ $PRODUCT_REPOS = "/java.net/cvs/open-jbi-components" ]; then
        theRepos="jbi"
    fi
fi

#NOTE:  shprops returns a var=value pair, which is why the eval.
if [ "$ENDTIME" = "" ]; then
    #get the current update time in GMT:
    eval `shprops -get $BLDPARMS ${theRepos}_last_update`
    if [ $? -ne 0 ]; then
        bldmsg -error -p $p "problem retrieving repository update times from $LASTBLDPARMS"
        cleanup
        exit 1
    else
        #success - copy the update time into local var:
        #NOTE: this is done in two steps to avoid bug in redhat bash 2.05.
        cmd="echo \$${theRepos}_last_update"
        ENDTIME=`eval $cmd`
    fi
fi

#get last previous build's update times:
eval `shprops -get $LASTBLDPARMS ${theRepos}_last_update`
if [ $? -ne 0 ]; then
    bldmsg -warn -p $p "problem retrieving repository update times from $LASTBLDPARMS"
    bldmsg -warn -p $p "Assuming this is the first build."
    BEGTIME=$ENDTIME
else
    #success - copy the update time into local var.
    #NOTE: this is done in two steps to avoid bug in redhat bash 2.05.
    cmd="echo \$${theRepos}_last_update"
    BEGTIME=`eval $cmd`
fi

#########
#GENERATE query:
#########

gen_integration_query $queryscript
if [ $? -ne 0 ]; then
    bldmsg -error -p $p cannot generate query script
    exit 1
fi

####
#RUN query:
####

cmd="ant -quiet -Dintegration_report=$tmpA -Dgmt_begtime=$BEGTIME -Dgmt_endtime=$ENDTIME -Drepos=$inf_repos -Dbranch=$inf_branch -f $queryscript"
bldmsg -mark -p $p Running $cmd
eval $cmd > $errfile 2>&1
status=$?

#######
#FORMAT query results:
#######

if [ $status -eq 0 ]; then
    if [ $DOOUTPUT -eq 1 ]; then
        rm -f $OUTPUTFILE
        grep -v '^$' $tmpA >  $OUTPUTFILE
        status=$?
        if [ $? -ne 0 ]; then
            bldmsg -p $p -error -status $status could not create output file $OUTPUTFILE
            exit 1
        fi
        echo Report for repository $inf_repos, branch $inf_branch from $BEGTIME to $ENDTIME saved to $OUTPUTFILE
    else
        #if output file not specified, then it goes to stdout:
        echo Report for repository $inf_repos, branch $inf_branch from $BEGTIME to $ENDTIME
        cat $tmpA
    fi
else
    bldmsg -p $p -error -status $status ant sql query failed for repos $inf_repos
    cat $errfile
    cleanup
    exit 2
fi

cleanup

exit 0
