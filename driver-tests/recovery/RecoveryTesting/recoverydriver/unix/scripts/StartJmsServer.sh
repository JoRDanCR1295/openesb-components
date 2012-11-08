#!/bin/bash
source ./scripts/SetEnvVars.sh

./imq/bin/imqbrokerd -loglevel DEBUG -Dimq.debug.com.sun.messaging.jmq.jmsserver.data.handlers.TransactionHandler=true -javahome $JAVA_HOME -port $JMS_PORT
