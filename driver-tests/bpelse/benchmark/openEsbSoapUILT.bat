@echo off

set JAVA=%JAVA_HOME%\bin\java

if not "%JAVA_HOME%" == "" goto SET_CLASSPATH

set JAVA=java

echo JAVA_HOME is not set, unexpected results may occur.
echo Set JAVA_HOME to the directory of your local JDK to avoid this message.

:SET_CLASSPATH

rem init classpath

set CLASSPATH=%SOAP_UI_HOME%\bin\soapui-1.7.5.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\activation-1.1.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\javamail-1.4.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\wsdl4j-1.6.2.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\junit-3.8.1.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\log4j-1.2.14.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\looks-2.1.2.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\forms-1.0.7.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\commons-logging-1.1.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\not-yet-commons-ssl-0.3.8.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\commons-cli-1.0.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\commons-beanutils-1.7.0.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\commons-httpclient-3.0.1-soapui.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\swingx-SNAPSHOT.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\l2fprod-common-fontchooser-0.2-dev.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\commons-codec-1.3.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\groovy-all-1.0.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\jetty-6.1.4.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\jetty-util-6.1.4.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\servlet-api-2.5-6.1.4.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\xbean-2.3.0.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\xbean_xpath-2.3.0.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\xmlpublic-2.3.0.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\jsr173_1.0_api-xmlbeans-2.3.0.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\soapui-xmlbeans-1.7.5.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\soap-xmlbeans-1.2.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\j2ee-xmlbeans-1.4.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\ext-xmlbeans-1.0.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\saxon-8.8.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\saxon-dom-8.8.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\xmlunit-1.1.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\xmlsec-1.2.1.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\xalan-2.6.0.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\wss4j-1.5.0.jar
set CLASSPATH=%CLASSPATH%;%SOAP_UI_HOME%\lib\bcprov-jdk15-133.jar

rem JVM parameters, modify as appropriate
set JAVA_OPTS=%JAVA_OPTS% -Xms128m -Xmx512m

rem ********* run soapui loadtest runner ***********

"%JAVA%" %JAVA_OPTS% -cp "%CLASSPATH%" com.eviware.soapui.tools.SoapUILoadTestRunner %*
