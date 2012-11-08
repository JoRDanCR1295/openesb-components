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
# @(#)addon00002.ksh
# Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
#
# END_HEADER - DO NOT EDIT
#

#addon00002 - JBI Addon Components Test

. ./regress_defs.ksh

asadmin stop-domain $TEST_DOMAIN 2>&1
mv $AS_INSTALL/domains/$TEST_DOMAIN/config/domain-registry $AS_INSTALL/domains/$TEST_DOMAIN/config/domain-registry.bak
sed -e 's/true/false/g' $AS_INSTALL/domains/$TEST_DOMAIN/config/domain-registry.bak > $AS_INSTALL/domains/$TEST_DOMAIN/config/domain-registry
asadmin start-domain --user admin --passwordfile $AS_INSTALL/passwords $TEST_DOMAIN 2>&1
asadmin list-jbi-binding-components --port $AS_ADMIN_PORT --user admin --passwordfile $AS_INSTALL/passwords 2>&1
asadmin list-jbi-service-engines    --port $AS_ADMIN_PORT --user admin --passwordfile $AS_INSTALL/passwords 2>&1
asadmin list-jbi-shared-libraries   --port $AS_ADMIN_PORT --user admin --passwordfile $AS_INSTALL/passwords 2>&1
asadmin stop-domain $TEST_DOMAIN 2>&1
asadmin uninstall-addon jbi_components 2>&1
