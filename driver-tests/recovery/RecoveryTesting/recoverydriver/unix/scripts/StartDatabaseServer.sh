#!/bin/bash
source ./scripts/SetEnvVars.sh

DERBY_INSTALL=$AS_HOME/javadb; export DERBY_INSTALL

$DERBY_INSTALL/frameworks/NetworkServer/bin/setNetworkServerCP.ksh
$DERBY_INSTALL/frameworks/NetworkServer/bin/startNetworkServer.ksh $DB_HOST
