echo .
echo STARTING WSTESTCLIENT

REM Example settings
REM set JAVA_HOME="C:\Program Files\Java\jdk1.5.0_09"
REM set APPSERVER_HOME=${SRCROOT}/Sun\AppServer

IF NOT DEFINED JAVA_HOME GOTO ERROR_NOJH
IF NOT DEFINED APPSERVER_HOME GOTO ERROR_NOASH
if "" == "%1" ( set THREAD=1 ) else ( set THREAD=%1 )
if "" == "%2" ( set COUNT=1 ) else ( set COUNT=%2 )
if "" == "%3" ( VERIFY="true" ) else ( set VERIFY=%3 )
if "" == "%4" ( goto Run )

:Run
java -cp bin;%APPSERVER_HOME%\lib\appserv-ws.jar;%APPSERVER_HOME%\lib\javaee.jar test.Test http://localhost:18181/purchaseOrderCoordinatorService/purchaseOrderCoordinatorPort %THREAD% %COUNT% %VERIFY%
GOTO SUCCESS

:ERROR_NOJH
echo JAVA_HOME not defined
GOTO ERROR_END

:ERROR_NOASH
echo APPSERVER_HOME not defined
GOTO ERROR_END

:ERROR_END
echo TEST FAILED

:SUCCESS
ECHO .
