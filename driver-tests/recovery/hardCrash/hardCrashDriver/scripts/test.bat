call random.bat

echo reboot time is 5%Random%

cd ${SRCROOT}/Sun\appserver\bin
call asadmin start-domain domain1

:: invoke the test targets here

cd ${SRCROOT}/recovery
psshutdown.exe -r -f -c -t 5%Random%
:: psshutdown.exe -r -t 5%Random%
