      ***************************************************************** 
      * OUTPUT FILE 3 - DATA ONLY WITH CALCULATIONS                   *
      *****************************************************************
      * FD  DATA-OUTFILE-3.
        01  OUT3-RECORD.
            03  OUT3-FILE-ID                 PIC 9.
            03  OUT3-RECORD-ID               PIC 9999999.
            03  OUT3-DATE.              
              05  OUT3-MM                    PIC 99.
              05  OUT3-DD                    PIC 99.
              05  OUT3-YEAR.     
                07  OUT3-CC                  PIC 99.
                07  OUT3-YY                  PIC 99.
            03  OUT3-TIME.           
              05  OUT3-HOURS                 PIC 99.
              05  OUT3-MINUTES               PIC 99.
              05  OUT3-SECONDS               PIC 99.
            03  OUT3-ALPHA-1                 PIC X(16).
            03  OUT3-S9999-X2                PIC S9999.
            03  OUT3-9999-X3                 PIC 9999.
            03  OUT3-S9999-X-9999            PIC 99999999.
            03  OUT3-S99999999-SUM           PIC S99999999.
            03  OUT3-99999999-BIN            PIC 99999999.
            03  OUT3-S9999999                PIC S9999999 COMP-3.
            03  OUT3-S99V99-X2               PIC S99V99.
            03  OUT3-99V99-D2                PIC 99V99. 
            03  OUT3-S9999-X100              PIC S9999.
            03  OUT3-SV9999-D100             PIC SV9999.
            03  OUT3-S9999                   PIC S9999.
            03  OUT3-S9V999                  PIC S9V999.
            03  OUT3-S999V9                  PIC S999V9.
            03  OUT3-S9999V99-SUM            PIC S9999V99.
            03  OUT3-S9999999V99-HASH        PIC S9999999V99.
