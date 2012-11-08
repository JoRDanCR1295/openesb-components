       01  SAF-HO-POLICY-MASTER-REC. 
         02  HEADERS.                                
           05  CDE-HEADER.                                   
              10  RECONCILIATION-KEY     PIC  X(16). 
              10  LOGGING-LEVEL          PIC  X(1).  
              10  FILLER                 PIC  X(23). 
           05  RESPONSE-HEADER.                              
              10 TPH-LLBB-LL             PIC X(2).          
              10 TPH-LLBB-BB             PIC X(2).          
              10 HEX-17-FOR-TEN          PIC X(10).         
011100         10  MUND-UND-ACTIONS-TBL.
011100           12  MUND-UND-ACTIONS                 OCCURS 8 TIMES
011200                                                INDEXED BY
011200                                                MUND-ACT-INDEX.
011300             15  MUND-ACT-ACTION-TYPE           PIC X.
011400                 88  MUND-ACT-INITIAL-VALUE         VALUE ' '.
011500                 88  MUND-ACT-ADD-ONLY              VALUE 'A'.
011600                 88  MUND-ACT-CHANGE-ONLY           VALUE 'C'.
011700                 88  MUND-ACT-ELIMINATE-ONLY        VALUE 'E'.
011800             15  MUND-UND-ACTION-NUMBER         PIC S9     COMP-3.
011900             15  MUND-UND-ACTION-TAKEN-TBL.
011900               17  MUND-UND-ACTION-TAKEN        OCCURS 3 TIMES
012000                                                INDEXED BY
012000                                                MUND-TAK-INDEX1
012000                                                MUND-TAK-INDEX2
012000                                                PIC S99    COMP-3.
012100                 88  MUND-NO-ACTION                 VALUE +11.
012200                 88  MUND-CANCEL                    VALUE +13.
012300                 88  MUND-NOT-TO-BE-RENEWED         VALUE +14.
012400                 88  MUND-CONTD-FOR-AGCY-BUSINESS   VALUE +15.
012500                 88  MUND-REINSPECTION              VALUE +16.
012600                 88  MUND-DISCUSSED-WITH-AGENT      VALUE +17.
012700                 88  MUND-REDUCE-EXPOSURE           VALUE +18.
012800                 88  MUND-OTHER-ACT                 VALUE +19.
012900             15  MUND-UND-ACT-UND-INITIALS      PIC XXX.
013000             15  MUND-DATE-UND-ACT-TAKEN        PIC S9(5)  COMP-3.
