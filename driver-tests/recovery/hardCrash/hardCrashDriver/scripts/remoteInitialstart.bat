@rem remove markup directory
cd ${SRCROOT}/recovery
rmdir /s /q flag
echo shutting down recovery test machine
psshutdown.exe -r -f -t 00
