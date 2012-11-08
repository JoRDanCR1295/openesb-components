set caps.appserver.home=${SRCROOT}/glassfish
call ${alaska_root}/env.bat
set PATH=%caps.appserver.home%\BIN;%PATH%
set CLASSPATH=%CLASSPATH%;bin;%caps.appserver.home%\lib\javaee.jar;%caps.appserver.home%\lib\webservices-rt.jar;
call mkdir bin
#uncomment this line and comment line following this if you need source code call wsimport ..\..\CorrelationSample\src\CorrelatedBpelClient.wsdl -keep -s src -d bin -p corrSample
call wsimport ..\..\CorrelationSample\src\CorrelatedBpelClient.wsdl -d bin -p corrSample
call javac src\corrSample\*.java -d bin
