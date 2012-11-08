cd ${alaska_root}
call env.bat
cd ${alaska_root}/jbi\bpelse\jbiadapter
call ant -Dlibs.junit.classpath=${SRCROOT}/netbeans\nb5\netbeans\ide6\modules\ext\junit-3.8.1.jar -DrunArgs="-Xdebug -Xrunjdwp:transport=dt_socket,address=13001,suspend=n,server=y -Dbpelse.properties=bpelse2.properties -Dtest.properties=bpdebugger/test2.properties" -lib ${SRCROOT}/netbeans\nb5\netbeans\ide6\modules\ext\junit-3.8.1.jar debuggerRunner
