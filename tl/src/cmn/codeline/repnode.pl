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
# @(#)repnode.pl - ver 1.1 - 01/04/2006
#
# Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
# 
# END_HEADER - DO NOT EDIT
#

package repnode;    #info about codeline build machines.

require "codeline.pl";
require "oscmn.pl";

$p = $'p;       #$main'p is the program name set by the skeleton

sub main
{
    local(*ARGV, *ENV) = @_;

    #set global flags:
    return (1) if (&parse_args(*ARGV, *ENV) != 0);
    return (0) if ($HELPFLAG);

    my $regroot = &cl'regress_root;

    #for each regress platform...
    for $port (@PORTS) {
        my $regdir = &cl'regress_dir($port);
        next if ($regdir eq "NULL");

        $regdir .= "/regress";

        my $linkdir = "$regroot/$port";
        my $linkname = "$linkdir/regress";

        if ($TEST_MODE) {
            printf ("mkdir -p %s %s\n", $linkdir);
        } elsif (&os'mkdir_p($linkdir, 0775) != 0) {
            printf STDERR ("%s:  could not create '%s' (platform regression dir).\n", $p, $linkdir);
            return(1);
        }

        &create_link($regdir, $linkname);

        #now we have the current regress dirs for this platform.
        #next, read the .date files and create links for the current build logs
        #and current test results:

        my $datefile = "$regdir/.date";
        if (!open(INFILE, $datefile)) {
            printf STDERR ("%s: WARNING: can't open datefile: '%s' - skipping\n", $datefile);
            next;
        }

        my $thedate = <INFILE>;

        #note - chomp is unsafe here because we are expecting results from DOS/UNIX.
        #chomp only works for EOL format of current platform.  RT 5/13/02
        $thedate =~ s/[\r\n]*$//;

        my $loglinksrc = "$regdir/log/$thedate";
        my $loglinkdst = "$regroot/$port/current_log";

        &create_link($loglinksrc, $loglinkdst);
    }

    return 0;
}

sub create_link
#create or display the symlink that would be created
{
    my($src, $dst) = @_;

    if ($TEST_MODE) {
        print "ln -s $src $dst\n";
    } elsif ($UPDATE_MODE && &symlink_okay($src, $dst)) {
        printf STDERR ("okay:  %s --> %s\n", $dst, $src) if ($VERBOSE);
    } else {
        unlink $dst;

        if (&os'symlink($src, $dst) != 0) {
            printf STDERR
            ("%s: WARNING:  could not create link: %s --> %s\n",
                $p, $dst, $src);
        } else {
            printf ("%s --> %s\n", $dst, $src);
        }
    }
}

sub symlink_okay
#true if existing link already contains the correct value
{
    my ($linkvalue, $linkname) = @_;

    my ($tmp) = readlink($linkname);
    return(0) if (!defined($tmp));

    return($tmp eq $linkvalue);
}


sub parse_args
#proccess command-line aguments
{
    local(*ARGV, *ENV) = @_;
    local ($flag, $arg);

    $ALL_PORTS = 0;
    $HELPFLAG = 0;
    $TEST_MODE = 1;     #default is non-destructive

    #eat up flag args:
    while ($#ARGV+1 > 0 && $ARGV[0] =~ /^-/) {
        $flag = shift(@ARGV);

        if ($flag eq '-a') {
            $ALL_PORTS = 1;
            @PORTS = (&cl'regress_ports());
        } elsif ($flag =~ '^-unix') {
            $ALL_PORTS = 0;
            @PORTS = (&cl'unix_ports());
        } elsif ($flag =~ '^-f') {
            $TEST_MODE = 0; #will attempt to create dirs & links
        } elsif ($flag =~ '^-v') {
            $VERBOSE = 1;
        } elsif ($flag =~ '^-u') {
            $UPDATE_MODE = 1;
            $TEST_MODE = 0; #update mode => recreate dirs & links out of date
        } elsif ($flag =~ '^-h') {
            $HELPFLAG = 1;
            return(&usage(0));
        } else {
            return(&usage(1));
        }
    }

    #add remaining args:
    if ($#ARGV >= 0) {
        for (@ARGV) {
            next if ($_ eq "");
            @PORTS = (@PORTS, $_);
        }
    }

    if ($#PORTS < 0) {
        $ALL_PORTS = 1;
        @PORTS = (&cl'regress_ports());
    }

    return(0);
}

sub usage
{
    local($status) = @_;
    local($CODELINE) = &cl'codeline;
    local($regports) = join("\n\t", &cl'regress_ports);

    print STDERR <<"!";
Usage:  $p [-h] [-v] [-f] [-u] [-a] [-unix] [regress_port_names...]

Synopsis:

Displays the commands to create a regress and build results
reporting structure for the $CODELINE codeline.

With -f option, recreates the structure.

Options:
  -h      display usage message.
  -f      remove the old reporting structure and re-create it
  -v      verbose messages
  -u      update symlinks only if they are out of date
  -a      build reporting structure for all ports
  -unix   build reporting structure for unix ports only

NOTE:

Valid regress port names for $CODELINE are:
    $regports
!
    return($status);
}

sub cleanup
{
}
1;
