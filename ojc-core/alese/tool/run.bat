@echo off
@REM Populate ALE Config Tables Script. Usage: run.bat <ConfigPropFile>
@REM Set JAVA_HOME to point to a jdk version 1.5 or higher ...
@REM For example, JAVA_HOME="C:\j2sdk1.5.0_10"
@if [%1] == [] goto help

REM Please do not modify the code below this line.

if %JAVA_HOME% == "" GOTO error1

rem set CLASSPATH=.;lib\ale-core-2.3-SNAPSHOT.jar;lib\common-util-2.3-SNAPSHOT.jar;lib\derbyclient.jar;lib\jbi.jar

set CLASSPATH=./lib/mysql-connector-java-5.1.6-bin.jar;./bld/ale-tool-2.3-SNAPSHOT.jar

:continue
%JAVA_HOME%\bin\java -classpath "%CLASSPATH%" com.sun.jbi.engine.ale.core.util.PopulateALEDBUtil %1
GOTO exit

:error1
echo Please set the JAVA_HOME variable in run.bat .....
GOTO exit

:help
@echo To run the ALE Database Populate script, type in
@echo.
@echo "run.bat <ConfigPropFile>"
@echo eg.run Config.Properties 
@echo.

:exit