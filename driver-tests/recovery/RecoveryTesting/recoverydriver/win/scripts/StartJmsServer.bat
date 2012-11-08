call scripts/SetEnvVars.bat

call imq\bin\imqbrokerd.exe -javahome %JAVA_HOME% -port %JMS_PORT%
