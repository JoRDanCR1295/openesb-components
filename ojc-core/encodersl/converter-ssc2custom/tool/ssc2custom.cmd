@rem ETD to Custom Encoder Converter
@rem
@rem Converts Monk ETDs to Custom Encoders

@echo off

if "%ENCODER_LIB%" EQU "" (set ENCODER_LIB=%~dp0lib)

set cp=
set cp=%cp%;%ENCODER_LIB%\xbean.jar
set cp=%cp%;%ENCODER_LIB%\encoderfrmwk-xsdext.jar
set cp=%cp%;%ENCODER_LIB%\customencoder-xsdext.jar
set cp=%cp%;%ENCODER_LIB%\encoder-fw.jar
set cp=%cp%;%ENCODER_LIB%\converter-ssc2custom.jar
set cp=%cp%;%ENCODER_LIB%\jsr173_api.jar

java -classpath "%cp%" com.sun.encoder.converter.Ssc2Custom %*
