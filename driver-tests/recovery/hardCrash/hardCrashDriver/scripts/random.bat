@ECHO OFF
ECHO.
IF NOT [%1]==[] GOTO Syntax

:: Change the FIND string and the tokens/delims
:: parameters for non-US language and settings:
FOR /F "tokens=2 delims=." %%A IN ('VER ^| TIME ^| FIND "current"') DO SET Random=%%A
SET Random
GOTO End

:Syntax
ECHO Random, Version 1.00 for Windows NT
ECHO Generate a semi-random number between 0 and 99 by "capturing"
ECHO the hundredths of seconds of the execution time.
ECHO.
ECHO Written by Rob van der Woude
ECHO http://www.robvanderwoude.com
ECHO.
ECHO Usage:  %~n0
ECHO.
ECHO Note :  Written for US version of Windows NT 4
ECHO         with standard US International settings.
ECHO         Adjust the parameters of the FOR and FIND commands
ECHO         for other languages and International settings.

:End


