000010*********************************************************** 
000020* OUT FILE 2 - MESSAGES ONLY                              *
000030***********************************************************
000040* DATA-OUTFILE-2.
000050 01  OUT2-RECORD.
000060    03 OUT2-FILE-ID                 PIC 9.
000070    03 OUT2-RECORD-ID               PIC 9999999.
000080    03 OUT2-DATE.                  
000090        05  OUT2-MM                    PIC 99.
000100        05  OUT2-DD                    PIC 99.
000110        05  OUT2-YEAR.                 
000120        07  OUT2-CC                  PIC 99.
000130        07  OUT2-YY                  PIC 99.
000140    03 OUT2-TIME.                    
000150        05  OUT2-HOURS                 PIC 99.
000160        05  OUT2-MINUTES               PIC 99.
000170        05  OUT2-SECONDS               PIC 99.
000180    03 OUT2-ALPHA-1-M1              PIC X(16).
000190    03 OUT2-VAR-X-OCCURS            PIC 9.
000200*    03 OUT2-VARIABLE.   
000210*       05  OUT2-VAR-X OCCURS 3 TIMES.
000220     03 OUT2-VAR-X-MESSAGE1      PIC X(20).
000220     03 OUT2-VAR-X-MESSAGE2      PIC X(20).
000220     03 OUT2-VAR-X-MESSAGE3      PIC X(20).
000230*DEPENDING ON OUT2-VAR-X-OCCURS
