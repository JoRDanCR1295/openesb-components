       01 TESTCOB.
           05  REPETITION-COUNTS.
               10  COUNT1 PIC 9(2) USAGE BINARY.
               10  COUNT2 PIC 9(2) USAGE BINARY.
           05  RPI-ENT.
               10  RPIE-CODENR PIC X(2) OCCURS 3 TIMES.
               10  RPIE-CODENR1 PIC G(4) USAGE DISPLAY-1
                                    OCCURS 2 TIMES.
                 88  CODENRENT VALUE '03'.
               10  RPIE-NSIRET PIC X(14).
               10  RPIE-NSIRETN REDEFINES RPIE-NSIRET PIC 9(14).
               10  FILLER PIC X(100).
               10  RPIE-REFREM OCCURS 1 TO 99 TIMES DEPENDING ON
                                 COUNT1 OF REPETITION-COUNTS.
                 15  RPIE-REFOPE PIC X(7).
                 15  RPIE-REFOPEN REDEFINES RPIE-REFOPE PIC 9(7).
                 15  FILLER PIC X(9).
               10  RPIE-CODBIC PIC X(11).
               10  RPIE-CODBICN REDEFINES RPIE-CODBIC PIC 9(11).
