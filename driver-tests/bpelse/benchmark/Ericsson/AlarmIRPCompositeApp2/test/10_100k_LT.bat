@echo off
if not "%SOAP_UI_HOME%" == "" goto RUN_TEST

set SOAP_UI_HOME=\tools\soapui-1.7.5
echo
echo SOAP_UI_HOME is not set, unexpected results may occur.
echo Set SOAP_UI_HOME to the directory of your local SOAP_UI to avoid this message.

:RUN_TEST
set SOAP_UI_HOME=\tools\soapui-1.7.5
call ..\..\..\openEsbSoapUILT.bat -lWarmup_10_100k_LT openESB-soapui-project.xml
call ..\..\..\openEsbSoapUILT.bat -l10_100k_LT -r openESB-soapui-project.xml