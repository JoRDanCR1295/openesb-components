      * I/O AREA FOR DATA BASE HANDLING                                 01330000
                                                                        01340000
       01  IOAREA.                                                      01350000
           02  IO-LINE PICTURE X(37) OCCURS 5.                          01360000
           02  IO-DATA REDEFINES IO-LINE.                               01370000
               04  IO-LAST-NAME    PIC X(10).                           01380000
               04  IO-FIRST-NAME   PIC X(10).                           01390000
               04  IO-EXTENSION    PIC X(10).                           01400000
               04  IO-ZIP-CODE     PIC X(7).                            01410000
           02  IO-FILLER       PIC X(3) VALUE SPACES.                   01420000
           02  IO-COMMAND      PIC X(8) VALUE SPACES.                   01430000