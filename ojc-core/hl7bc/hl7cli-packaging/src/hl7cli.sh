if [ "$JAVA_HOME" = "" ] ; then

  echo Please set JAVA_HOME first before trying again.

else

   $JAVA_HOME/bin/java -cp lib/hl7cli.jar:lib/commons-cli.jar:lib/hl7api.jar:lib/jbi-admin-common.jar com.sun.jbi.hl7.cli.HL7CLI  $1 $2 $3 $4 $5 $6 $7 $8 $9 

fi


