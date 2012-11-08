set caps.appserver.home=${SRCROOT}/glassfish
call ${alaska_root}/env.bat
set PATH=%caps.appserver.home%\BIN;%PATH%
set CLASSPATH=bin;%CLASSPATH%;%caps.appserver.home%\lib\javaee.jar;%caps.appserver.home%\lib\webservices-rt.jar;
#First parameter number of threads
#Second parameter number of iterations
java corrSample.Main %1 %2
