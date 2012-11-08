
rem Use this script to generate all HL7 Version 2 XSDs from the Access database


rem set CLSPATH=./bld/encoder-hl7-1.0.jar;../encoder-fw/bld/encoder-fw-1.0.jar
set CLSPATH=./bld/encoder-hl7-2.4-SNAPSHOT.jar;../encoder-fw/bld/encoder-fw-2.4-SNAPSHOT.jar
set MAINCLS=com.sun.encoder.hl7.xsdbuilder.MainGenerator
set METADATA=./metadata/hl7_56.mdb
set METADATA_V251=./metadata/hl7_251.mdb
set METADATA_V26=./metadata/hl7_v26.mdb

rmdir /s /q ./bld/xsd

java -classpath %CLSPATH% %MAINCLS% -i %METADATA% -o ./bld/xsd/2.1 -v 2.1
java -classpath %CLSPATH% %MAINCLS% -i %METADATA% -o ./bld/xsd/2.2 -v 2.2
java -classpath %CLSPATH% %MAINCLS% -i %METADATA% -o ./bld/xsd/2.3 -v 2.3
java -classpath %CLSPATH% %MAINCLS% -i %METADATA% -o ./bld/xsd/2.3.1 -v 2.3.1
java -classpath %CLSPATH% %MAINCLS% -i %METADATA% -o ./bld/xsd/2.4 -v 2.4
java -classpath %CLSPATH% %MAINCLS% -i %METADATA% -o ./bld/xsd/2.5 -v 2.5
java -classpath %CLSPATH% %MAINCLS% -i %METADATA_V251% -o ./bld/xsd/2.5.1 -v 2.5.1
java -classpath %CLSPATH% %MAINCLS% -i %METADATA_V26% -o ./bld/xsd/2.6 -v 2.6
