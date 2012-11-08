call scripts/SetEnvVars.bat

set DERBY_HOME=%AS_HOME%/javadb
"%JAVA_HOME%\bin\java" -cp "%DERBY_HOME%\lib\derby.jar;%DERBY_HOME%\lib\derbytools.jar;%DERBY_HOME%\lib\derbynet.jar" -Dderby.system.home=%DB_LOCATION% org.apache.derby.drda.NetworkServerControl start -h %DB_HOST% -p 1527

