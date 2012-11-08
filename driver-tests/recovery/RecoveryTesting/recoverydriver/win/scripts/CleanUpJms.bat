call scripts/SetEnvVars.bat

call imq\bin\imqcmd.exe purge dst -javahome %JAVA_HOME% -n %1 -t q -passfile scripts/passfile.txt -u admin -b %JMS_HOST%:%JMS_PORT%
