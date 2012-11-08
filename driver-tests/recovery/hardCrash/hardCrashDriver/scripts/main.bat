cd ${SRCROOT}/recovery
if EXIST flag. (
 echo "appserver installed".
) else (
 echo "appserver needs to be installed".
 cd ${SRCROOT}/recovery
 CALL alaska-doit.bat
 
 @rem stop the appserver so that the test batch script can restart it
 cd ${SRCROOT}/Sun\appserver\bin
 call asadmin stop-domain domain1

 @rem flag directory that keeps from re-creating the environment during the restarts of the machime
 cd ${SRCROOT}/recovery
 mkdir flag
)
cd ${SRCROOT}/recovery
CALL test
