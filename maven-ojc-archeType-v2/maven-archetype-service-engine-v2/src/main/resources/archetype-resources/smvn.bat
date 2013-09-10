@echo off
set JAVA_HOME=C:\Program Files (x86)\Java\jdk1.6.0_38
set path=%path%;C:\Program Files (x86)\Java\jdk1.6.0_38\bin
set JV_SRCROOT=D:\codes\logicoyOpenESB\openesb-components
set JV_GFBASE=D:\softwares\openesb_logicoy_last_working57\glassfish
mvn -Dmaven.test.skip=true -Dmaven.repo.local=C:\Users\logicoyparam\m2\repository %*