set JAVA_HOME=C:\Alaska\root\build-tools\jdk1.5.0_09
set Path=%JAVA_HOME%\jre\bin;

REM java -Xdebug -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=y -jar ScalabilityStandAloneTestClient.jar
java -jar ScalabilityStandAloneTestClient.jar
