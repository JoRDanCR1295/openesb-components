       01  AUTO-RECORD-IN.
           05  CNO-IN.
               10  ACCT-IN                PIC X(09).
               10  FORM-IN                PIC X(03).
               10  SEQ-IN                 PIC X(01).
           05  INSD-INFO-IN.
               10  INSD-FIRST-NAME-IN     PIC X(10).
	       10  INSD-MID-INIT-IN       PIC X(01).
               10  INSD-LAST-NAME-IN      PIC X(15).
    ***       05  INSD-INFO-REDEF1-IN REDEFINES INSD-INFO-IN.
    ***           10  INSD-FIRST-1-IN        PIC X(01).
    ***           10  FILLER                 PIC X(10).
    ***           10  INSD-LAST-1-IN         PIC X(01).
    ***               88  LAST-INIT-A        VALUE 'A'.
    ***           10  FILLER                 PIC X(14).
           05  INSD-INFO-REDEF2-IN REDEFINES INSD-INFO-IN.
               10  INSD-FIRST-MID-IN      PIC X(11).
               10  INSD-LAST-SHORT-IN     PIC X(04).
               10  FILLER                 PIC X(11).
           05  VEHICLE-IN  OCCURS 4 TIMES.
               10  YEAR-IN                PIC X(02).
               10  MAKE-IN                PIC X(05).
               10  MODEL-IN               PIC X(10).
           05  VEHICLE-NUMBER-IN          PIC 9(02).
           05  VEH-NUM-ALPHA-IN           PIC x(02).
           05  VEH-NUM-IN 
               REDEFINES VEH-NUM-ALPHA-IN PIC 9(02).
               