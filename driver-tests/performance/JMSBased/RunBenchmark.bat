@echo off
call setEnv.bat
set JBBD_DIST=C:\open-jbi-components\driver-tests\performance\JMSBased\jmsBasedBenchmarkDriver\dist
set JBBD_LIB=C:\open-jbi-components\driver-tests\performance\JMSBased\jmsBasedBenchmarkDriver\lib
rem
set CLASSPATH=%JBBD_DIST%\jmsBasedBenchmarkDriver.jar
set CLASSPATH=%CLASSPATH%;%JBBD_LIB%\iepcore.jar
set CLASSPATH=%CLASSPATH%;%JBBD_LIB%\imqjmsra.jar
set CLASSPATH=%CLASSPATH%;%JBBD_LIB%\javaee.jar
if "%1" == "-Xdebug" GOTO Xdebug
%JAVA_HOME%\bin\java com.sun.jbi.performance.jmsbd.BenchmarkDriver %*
goto end

:Xdebug
%JAVA_HOME%/bin/java -Xdebug -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=y com.sun.jbi.performance.jmsbd.BenchmarkDriver %2 %3 %4 %5 %6 %7 %8 %9

goto end

:end

