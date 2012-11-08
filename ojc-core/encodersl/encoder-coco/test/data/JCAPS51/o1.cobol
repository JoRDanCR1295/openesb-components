      ***************************************************************** 
      * OUTPUT FILE 1 - COMBINED DATA AND MESSAGES                    *
      *****************************************************************
      *  FD  DATA-OUTFILE-1.
        01  OUT1-RECORD.
            03  OUT1-FILE-ID                 PIC 9.
            03  OUT1-RECORD-ID               PIC 9999999.
            03  OUT1-DATE.
              05  OUT1-MM                    PIC 99.
              05  OUT1-DD                    PIC 99.
              05  OUT1-YEAR.
                07  OUT1-CC                  PIC 99.
                07  OUT1-YY                  PIC 99.
            03  OUT1-TIME.
              05  OUT1-HOURS                 PIC 99.
              05  OUT1-MINUTES               PIC 99.
              05  OUT1-SECONDS               PIC 99.
            03  OUT1-ALPHA-1                 PIC X(16).
            03  OUT1-S9999                   PIC S9999.
            03  OUT1-9999                    PIC 9999.
            03  OUT1-S9999-BIN               PIC S9999 BINARY.
            03  OUT1-S9999-COMP              PIC S9999 COMP.
            03  OUT1-S9999-COMP-4            PIC S9999 COMP-4.
            03  OUT1-9999-BIN                PIC 9999 BINARY.
            03  OUT1-9999-COMP               PIC 9999 COMP.
            03  OUT1-9999-COMP-4             PIC 9999 COMP-4.
            03  OUT1-S9999-PACK              PIC S9999 PACKED-DECIMAL.
            03  OUT1-S9999-COMP-3            PIC S9999 COMP-3.
            03  OUT1-S99V99                  PIC S99V99.
            03  OUT1-99V99                   PIC 99V99.
            03  OUT1-S99V99-BIN              PIC S99V99 BINARY.
            03  OUT1-S99V99-COMP             PIC S99V99 COMP.
            03  OUT1-S99V99-COMP-4           PIC S99V99 COMP-4.
            03  OUT1-99V99-BIN               PIC 99V99 BINARY.
            03  OUT1-99V99-COMP              PIC 99V99 COMP.
            03  OUT1-99V99-COMP-4            PIC 99V99 COMP-4.
            03  OUT1-S99V99-PACK             PIC S99V99 PACKED-DECIMAL.
            03  OUT1-S99V99-COMP-3           PIC S99V99 COMP-3.
            03  OUT1-RED-1                   PIC X(16).
            03  OUT1-RED-2 REDEFINES OUT1-RED-1.
              05  OUT1-RED-2-X4              PIC X(4). 
              05  OUT1-RED-2-9999            PIC 9999.
              05  OUT1-RED-2-PD              PIC 9999999 COMP-3.
              05  OUT1-RED-2-BIN             PIC 9999999 BINARY.
            03  OUT1-VAR-1.
              05  OUT1-VAR-1-FORMAT          PIC X.
              05  OUT1-VAR-1-X7              PIC X(7). 
              05  OUT1-VAR-1-9999            PIC 9999.
              05  OUT1-VAR-1-PD              PIC 9999999 COMP-3.
              05  OUT1-VAR-1-BIN             PIC 9999999 BINARY. 
            03  OUT1-VAR-2.                 
              05  OUT1-VAR-2-FORMAT          PIC X.
              05  OUT1-VAR-2-X7              PIC X(7). 
              05  OUT1-VAR-2-9999            PIC 9999.
              05  OUT1-VAR-2-PD              PIC 9999999 COMP-3.
              05  OUT1-VAR-2-BIN             PIC 9999999 BINARY.      
            03  OUT1-VAR-3.                
              05  OUT1-VAR-3-FORMAT          PIC X.
              05  OUT1-VAR-3-X7              PIC X(7). 
              05  OUT1-VAR-3-9999            PIC 9999.
              05  OUT1-VAR-3-PD              PIC 9999999 COMP-3.
              05  OUT1-VAR-3-BIN             PIC 9999999 BINARY. 
            03  OUT1-VAR-4.                
              05  OUT1-VAR-4-FORMAT          PIC X.
              05  OUT1-VAR-4-X7              PIC X(7). 
              05  OUT1-VAR-4-9999            PIC 9999.
              05  OUT1-VAR-4-PD              PIC 9999999 COMP-3.
              05  OUT1-VAR-4-BIN             PIC 9999999 BINARY. 
            03  OUT1-VAR-5.
              05  OUT1-VAR-5-FORMAT          PIC X.
              05  OUT1-VAR-5-X7              PIC X(7). 
              05  OUT1-VAR-5-9999            PIC 9999.
              05  OUT1-VAR-5-PD              PIC 9999999 COMP-3.
              05  OUT1-VAR-5-BIN             PIC 9999999 BINARY. 
            03  OUT1-VAR-6.           
              05  OUT1-VAR-6-FORMAT          PIC X.
              05  OUT1-VAR-6-X7              PIC X(7). 
              05  OUT1-VAR-6-9999            PIC 9999.
              05  OUT1-VAR-6-PD              PIC 9999999 COMP-3.
              05  OUT1-VAR-6-BIN             PIC 9999999 BINARY. 
            03  OUT1-ALPHA-1-M1              PIC X(16).
            03  OUT1-ALPHA-1-M2              PIC X(16).
            03  OUT1-VAR-1-MESSAGE           PIC X(20).
            03  OUT1-VAR-2-MESSAGE           PIC X(20).
            03  OUT1-VAR-3-MESSAGE           PIC X(20).
            03  OUT1-VAR-4-MESSAGE           PIC X(20).
            03  OUT1-VAR-5-MESSAGE           PIC X(20).
            03  OUT1-VAR-6-MESSAGE           PIC X(20).
