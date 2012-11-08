#!/bin/bash
source ./scripts/SetEnvVars.sh

rm -r ../build
rm -r ../dist

mkdir ../build
mkdir ../build/classes
mkdir ../dist

$JAVA_HOME/bin/javac -cp $AS_HOME/lib/activation.jar:$AS_HOME/lib/appserv-admin.jar:$AS_HOME/lib/install/applications/jmsra/imqjmsra.jar:$AS_HOME/lib/appserv-rt.jar:$AS_HOME/lib/j2ee.jar:$AS_HOME/lib/javaee.jar:$AS_HOME/jbi/lib/jbi_rt.jar:$AS_HOME/lib/mail.jar:$JAVA_HOME/lib/tools.jar ../src/com/sun/jbi/recoverytest/*.java -d ../build/classes

$JAVA_HOME/bin/jar cf ../dist/recoverydriver.jar -C ../build/classes .

$JAVA_HOME/bin/java -cp ../dist/recoverydriver.jar:$AS_HOME/javadb/lib/derbyclient.jar:$ORACLE_DRIVER_PATH com.sun.jbi.recoverytest.CleanUp test.properties
