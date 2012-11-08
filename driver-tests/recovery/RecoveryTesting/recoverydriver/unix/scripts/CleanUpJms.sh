#!/bin/bash
source ./scripts/SetEnvVars.sh

./imq/bin/imqcmd purge dst -javahome $JAVA_HOME -n $1 -t q -passfile scripts/passfile.txt -u admin -b $JMS_HOST:$JMS_PORT
