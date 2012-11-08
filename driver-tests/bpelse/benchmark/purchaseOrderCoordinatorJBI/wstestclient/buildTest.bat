echo .
echo BUILDING_WSTESTCLIENT
REM Example settings
REM set JAVA_HOME="C:\Program Files\Java\jdk1.5.0_09"
REM set APPSERVER_HOME=${SRCROOT}/Sun\AppServer

IF NOT DEFINED JAVA_HOME GOTO ERROR_NOJH
IF NOT DEFINED APPSERVER_HOME GOTO ERROR_NOASH
set PATH=%APPSERVER_HOME%\bin;%path%
copy ..\..\purchaseOrderCoordinator\src\*.wsdl . 
copy ..\..\purchaseOrderCoordinator\src\*.xsd .

call wscompile -gen Config.xml -keep

del *.wsdl
del *.xsd

echo on
copy Test.java test\Test.java
mkdir bin
%JAVA_HOME%\bin\javac -cp %APPSERVER_HOME%\lib\appserv-ws.jar;%APPSERVER_HOME%\lib\javaee.jar test\*.java -d bin test\PurchaseOrderCoordinator\*.java
echo BUILD SUCCESSFUL
GOTO SUCCESS

:ERROR_NOJH
echo JAVA_HOME not defined
GOTO ERROR_END

:ERROR_NOASH
echo APPSERVER_HOME not defined
GOTO ERROR_END

:ERROR_END
echo BUILD FAILED

:SUCCESS
ECHO .
