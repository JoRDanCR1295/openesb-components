@echo off
set JAVA_HOME=C:\Alaska\build-tools\jdk1.5.0_12
set IEP_CORE_DIST=C:\Alaska\jbicomps\iepse\iepcore\dist
set IEP_LIB=C:\Alaska\jbicomps\iepse\iepcore\scripts\lib
set JBBD_DIST=C:\open-jbi-components\driver-tests\performance\JMSBased\jmsBasedBenchmarkDriver\dist
set JBBD_LIB=C:\open-jbi-components\driver-tests\performance\JMSBased\jmsBasedBenchmarkDriver\lib
rem
set CLASSPATH=%IEP_CORE_DIST%\iepcore.jar
set CLASSPATH=%CLASSPATH%;%IEP_CORE_DIST%\iepclient.jar
set CLASSPATH=%CLASSPATH%;%IEP_LIB%\ojdbc14.jar
set CLASSPATH=%CLASSPATH%;%IEP_LIB%\derby.jar
set CLASSPATH=%CLASSPATH%;%IEP_LIB%\derbyclient.jar
set CLASSPATH=%CLASSPATH%;%IEP_LIB%\mysql-connector-java-5.1.7-bin.jar
set CLASSPATH=%CLASSPATH%;%IEP_LIB%\componentsl.jar
set CLASSPATH=%CLASSPATH%;%IEP_LIB%\j2ee.jar
set CLASSPATH=%CLASSPATH%;%IEP_LIB%\saaj.jar
set CLASSPATH=%CLASSPATH%;%IEP_LIB%\saaj-impl.jar
set CLASSPATH=%CLASSPATH%;%JBBD_DIST%\jmsBasedBenchmarkDriver.jar
set CLASSPATH=%CLASSPATH%;%JBBD_LIB%\imqjmsra.jar
set CLASSPATH=%CLASSPATH%;%JBBD_LIB%\javaee.jar

set propertyFile=benchmarks\iepse\echo\event-processor.properties

if "%1" == "-Xdebug" GOTO Xdebug
%JAVA_HOME%\bin\java com.sun.jbi.performance.jmsbd.EventProcessor %propertyFile%
goto end

:Xdebug
%JAVA_HOME%/bin/java -Xdebug -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=y com.sun.jbi.performance.jmsbd.EventProcessor %propertyFile%

goto end

:end

