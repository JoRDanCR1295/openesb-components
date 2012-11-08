000580**** 05  LG-LOG-LENTH                    PIC X(4).                        
000580**** 05  LG-LOG-PREFIX.                                                   
000640****     10  LG-B-A                      PIC X.                           
000990****     10  LG-TXN-ID                   PIC X(4).                        
000810****     10  LG-SUB-FUNCTION             PIC X.                          1
003140     05  CL-KEY.                                                         1
003150         10  CL-PZ-ID                        PIC XX.                     1
003170         10  CL-CI-ID                        PIC XX.                     3
003180         10  CL-REC-ID                       PIC XX.                     5
003200         10  CL-KEY1.                                                    7
003210             15  CL-PA-KEY1.                                             7
003220                 20  CL-ME-KEY1.                                         7
003230                     25  CL-PA-GROUP         PIC X(08).                  7
003240                     25  CL-PA-ID            PIC X(09).                 15
003250                 20  CL-PA-REL-NAME.                                    24
003260                     25  CL-PA-REL           PIC X.                     24
003270                     25  CL-PA-ID-NAME       PIC X(06).                 25
003280             15  CL-SYS-ID                   PIC X.                     31
003290                 88  CL-DENTAL               VALUE 'D'.                   
003300                 88  CL-DISABILITY           VALUE 'K' 'L'.               
003310                 88  CL-STD                  VALUE 'K'.                   
003320                 88  CL-LTD                  VALUE 'L'.                   
003330                 88  CL-MEDICAL              VALUE 'M'.                   
003340             15  CL-CL-KEY1.                                            32
003350                 20  CL-CL-ID                PIC X(09).                 32
003360                 20  CL-CL-GEN-X.                                       41
003370                     25  CL-CL-GEN           PIC 99.                    41
003380         10  CL-KEY-FIL                      PIC XX.                    43
003390     05  CL-ODO-CTRS.                                                   45
003400         10  CL-C-ODO-CTR                    PIC S9(4)    COMP.         45
003410         10  CL-NFD-ODO-CTR                  PIC S9(4)    COMP.         47
003420         10  CL-FILL-ODO-CTR                 PIC S9(4)    COMP.         49
003430         10  CL-CUST-ODO-CTR                 PIC S9(4)    COMP.         51
003440     05  CL-FIXED-DATA.                                                 53
003450         10  CL-LAST-UPD-DATA.                                          53
Y2K  A             15  CL-LAST-UPD-DT-CYMD            PIC 9(08).        00430000
Y2K  A             15  CL-LAST-UPD-DT-CYMD-X REDEFINES                  00440000
Y2K  A                  CL-LAST-UPD-DT-CYMD.                            00440000
Y2K  A                20  CL-LAST-UPD-CCYY        PIC 9(04).                  45
Y2K  A                20  FILLER                  PIC X(04).                  45
Y2K  A             15  CL-LAST-UPD-DATE-X REDEFINES                     00490000
Y2K  A                  CL-LAST-UPD-DT-CYMD.                            00490000
Y2K  A               20  CL-LAST-UPD-CC              PIC 9(02).         00500000
Y2K  C               20  CL-LAST-UPD-DT              PIC 9(6).                53
Y2K  C               20  CL-LAST-UPD-DT-X                                     53
Y2K  C                   REDEFINES  CL-LAST-UPD-DT.                             
Y2K  C                   25  CL-LAST-UPD-YY          PIC 99.                  53
Y2K  C                   25  CL-LAST-UPD-MM          PIC 99.                  55
Y2K  C                   25  CL-LAST-UPD-DD          PIC 99.                  57
003520         10  CL-USER-FIELD-G.                                           59
003530             15  CL-USER-FIELD               PIC X(10)   OCCURS 3.      59
003540         10  CL-STATUS                       PIC XX.                    89
003550             88  CL-PAY-CLAIM                VALUE '01'.                  
003560             88  CL-PAID-CLAIM               VALUE '02'.                  
003570             88  CL-LOGGED-CLAIM             VALUE '10'.                  
003580             88  CL-HOLD-CLAIM               VALUE '11'.                  
003590             88  CL-HELD-CLAIM               VALUE '12'.                  
003600             88  CL-ERROR-CLAIM              VALUE '15'.                  
003610             88  CL-BATCH-CLAIM              VALUE '16'.                  
003620             88  CL-ADJUSTED-CLAIM           VALUE '91'.                  
003630             88  CL-CLOSEOUT                 VALUE '99'.                  
003640         10  FILLER REDEFINES CL-STATUS.                                89
003650             15  CL-STATUS-1                 PIC X.                     89
003660                 88  CL-PAID-STATUS          VALUE '0'.                   
003670                 88  CL-HOLD-STATUS          VALUE '1'.                   
003680                 88  CL-ADJ-STATUS           VALUE '9'.                   
003690             15  CL-STATUS-2                 PIC X.                     90
003700         10  CL-SEG-PTRS.                                               91
003710             15  CL-CL-SEG-NEXT              PIC XX.                    91
003720             15  CL-CL-SEG-PREV              PIC XX.                    93
003730         10  CL-OVERRIDE-DATA.                                          95
003740             15  CL-OR-CODE                  PIC XXX.                   95
003750*****    EACH BIT IS USED AS OVERRIDE IND    *****                        
003760             15  CL-OR-IND                   PIC XXX.                   98
003770*    SKIP2                                                                
003780         10  CL-PROCESS-DATA.                                          101
003790             15  CL-CI-OPTIONS.                                        101
003800                 20  CL-CI-GROUP-OPT4        PIC X.                    101
003810                     88  CL-CI-GROUP             VALUE 'G' 'P'.           
003820                     88  CL-CI-PI-GROUP          VALUE 'P'.               
003830                 20  FILLER                  PIC XXX.                  102
003840             15  CL-PI-ID                    PIC X(06).                105
003850             15  CL-PI-COV                   PIC XX.                   111
003860             15  CL-PI-OPTIONS.                                        113
003870                 20  CL-PI-CHK-OPT           PIC X.                    113
003880                     88  CL-PI-CHK-ADDL-COPY     VALUE 'A' 'B'.           
003890                     88  CL-PI-CHK-PLAN-COPY     VALUE 'P' 'B'.           
003900                 20  FILLER                  PIC XXX.                  114
003910             15  CL-PI-CHK-VERSION           PIC X(02).                117
003920             15  CL-PC-ID                    PIC X(02).                119
003930             15  CL-PPP                      PIC X.                    121
003940             15  CL-ELIG-EVENT               PIC X.                    122
003950             15  CL-ELIG-COV                 PIC XX.                   123
003960             15  CL-COBRA-IND                PIC X.                    125
003970                 88  CL-COBRA                    VALUE 'A' 'Y'.           
003980             15  CL-SITE                     PIC XX.                   126
003990             15  CL-PO-ID                    PIC X(11).                128
004000             15  CL-ME-LANG-IND              PIC X(01).                139
004010                 88  CL-ME-LANG-ENGLISH          VALUE ' '.               
004020                 88  CL-ME-LANG-FRENCH           VALUE 'F'.               
004030             15  CL-BATCH-PC-ID              PIC X(02).                140
004040             15  CL-ME-STATE                 PIC X(02).                142
004050*    SKIP2                                                                
004060         10  CL-PREFIXES.                                              144
004070             15  CL-BC-KEY1.                                           144
004080                 20  CL-BC-PFX                 PIC X(06).              144
004090                 20  CL-BC-COV                 PIC XX.                 150
004100             15  CL-AI-PFX                     PIC X(04).              152
004110             15  CL-PL-PFX                     PIC X(04).              156
004120             15  CL-DB-PFX REDEFINES CL-PL-PFX PIC X(04).              156
004130             15  CL-ST-PFX REDEFINES CL-DB-PFX PIC X(04).              156
004140             15  CL-LT-PFX REDEFINES CL-DB-PFX PIC X(04).              156
004150             15  CL-DE-PFX                     PIC X(04).              160
004160             15  CL-MS-PFX                     PIC X(04).              164
004170             15  CL-MD-PFX REDEFINES CL-MS-PFX PIC X(04).              164
004180             15  CL-MC-PFX                     PIC X(04).              168
004190             15  CL-HP-PFX REDEFINES CL-MC-PFX PIC X(04).              168
004200             15  CL-PO-PFX                     PIC X(04).              172
004210             15  CL-PY-PFX                     PIC X(04).              176
004220             15  CL-UT-PFX                     PIC X(04).              180
004230             15  CL-MU-PFX REDEFINES CL-UT-PFX PIC X(04).              180
004240             15  CL-RT-PFX                     PIC X(04).              184
004250             15  CL-AR-PFX REDEFINES CL-RT-PFX PIC X(04).              184
004260             15  CL-RC-PFX REDEFINES CL-RT-PFX PIC X(04).              184
004270             15  CL-TP-PFX                     PIC X(04).              188
004280             15  CL-RA-PFX                     PIC X(04).              192
004290             15  CL-IM-PFX REDEFINES CL-RA-PFX PIC X(04).              192
004300             15  CL-IA-PFX                     PIC X(04).              196
004310             15  CL-SP-PFX REDEFINES CL-IA-PFX PIC X(04).              196
004320             15  CL-EA-PFX                     PIC X(04).              200
004330         10  CL-USDATA.                                                204
004340             15  CL-US-ID-PEND               PIC X(10).                204
004350             15  CL-US-ID-ACCEPT             PIC X(10).                214
004360*    SKIP2                                                                
004370         10  CL-ME-PA-DATA.                                            224
004380             15  CL-PA-ACCT-NO   OCCURS  3   PIC X(15).                224
004390             15  CL-PA-AGE                   PIC S9(03)  COMP-3.       269
004400             15  CL-PA-SEX                   PIC X.                    271
004410             15  CL-FB-ID                    PIC X(09).                272
004420             15  CL-ACCUM-SUF                PIC X(06).                281
004430             15  CL-ME-TYPE                  PIC X(04).                287
004440             15  CL-ME-DEPT                  PIC X(08).                291
004450             15  CL-ME-LOC                   PIC X(08).                299
004460*    SKIP2                                                                
004470         10  CL-CT-DATA.                                               307
004480             15  CL-CT-CS-ID                 PIC X(9).                 307
004490             15  FILLER                      PIC X(6).                 316
004500*    SKIP2                                                                
004510         10  CL-TF-DATA.                                               322
004520             15  CL-TF-RF-ID                 PIC X(9).                 322
004530             15  CL-TF-REF-AUDIT-CODE        PIC X.                    331
004540             15  CL-TF-REF-PR-ID             PIC X(11).                332
004550             15  CL-TF-RF-SEG-X.                                       343
004560                 20  CL-TF-RF-SEG            PIC 9(2).                 343
004570             15  FILLER                      PIC X(2).                 345
004580         10  CL-MM-DATA REDEFINES CL-TF-DATA.                          322
004590             15  CL-MM-AUTH-NO               PIC X(9).                 322
004600             15  CL-MM-AUTH-AUDIT-CODE       PIC X.                    331
004610             15  CL-MM-AUTH-PR-ID            PIC X(11).                332
004620             15  CL-MM-AUTH-SEG-X.                                     343
004630                 20  CL-MM-AUTH-SEG          PIC 9(2).                 343
004640             15  CL-MM-INTERFACE-IND         PIC X(1).                 345
004650                 88  CL-MM-INTERFACE         VALUE 'A'.                   
004660             15  FILLER                      PIC X(1).                 346
004670         10  CL-DATES.                                                 347
Y2K  A             15  CL-CL-CCYY                PIC 9(04).             00322700
Y2K  A             15  CL-CL-CCYY-X  REDEFINES CL-CL-CCYY.              00322800
Y2K  A                20   CL-CL-CC-X.                                  00322900
Y2K  A                   25   CL-CL-CC              PIC 9(02).          00322900
Y2K  C                20   CL-CL-YY-X.                                       347
Y2K  C                   25  CL-CL-YY                PIC 99.                 347
Y2K  A             15  CL-REC-DT-CYMD            PIC 9(08).             00430000
Y2K  A             15  CL-REC-DT-CYMD-X REDEFINES                       00440000
Y2K  A                   CL-REC-DT-CYMD.                                00440000
Y2K  A                20  CL-RD-CCYY        PIC 9(04).                        45
Y2K  A                20  FILLER            PIC X(04).                        45
Y2K  A             15  CL-REC-DATE-X REDEFINES                          00490000
Y2K  A                  CL-REC-DT-CYMD.                                 00490000
Y2K  A                20  CL-RD-CC                    PIC 9(02).        00500000
Y2K  C                20  CL-REC-DT                   PIC 9(6).              349
Y2K  C                20  CL-REC-DT-X                                        349
Y2K  C                   REDEFINES   CL-REC-DT.                                 
Y2K  C                   25  CL-RD-YY                PIC 99.                 349
Y2K  C                   25  CL-RD-MM                PIC 99.                 351
Y2K  C                   25  CL-RD-DD                PIC 99.                 353
Y2K  A             15  CL-INPUT-DT-CYMD            PIC 9(08).           00430000
Y2K  A             15  CL-INPUT-DT-CYMD-X REDEFINES                     00440000
Y2K  A                  CL-INPUT-DT-CYMD.                               00440000
Y2K  A                20  CL-ID-CCYY        PIC 9(04).                        45
Y2K  A                20  FILLER            PIC X(04).                        45
Y2K  A             15  CL-INPUT-DATE-X REDEFINES                        00490000
Y2K  A                  CL-INPUT-DT-CYMD.                               00490000
Y2K  A                20  CL-ID-CC                    PIC 9(02).        00500000
Y2K  C                20  CL-INPUT-DT                 PIC 9(6).              355
Y2K  C                20  CL-INPUT-DT-X                                      355
Y2K  C                    REDEFINES   CL-INPUT-DT.                              
Y2K  C                   25  CL-ID-YY                PIC 99.                 355
Y2K  C                   25  CL-ID-MM                PIC 99.                 357
Y2K  C                   25  CL-ID-DD                PIC 99.                 359
Y2K  A             15  CL-PAID-DT-CYMD            PIC 9(08).            00430000
Y2K  A             15  CL-PAID-DT-CYMD-X REDEFINES                      00440000
Y2K  A                  CL-PAID-DT-CYMD.                                00440000
Y2K  A                20  CL-PD-CCYY        PIC 9(04).                        45
Y2K  A                20  FILLER            PIC X(04).                        45
Y2K  A             15  CL-PAID-DATE-X REDEFINES                         00490000
Y2K  A                  CL-PAID-DT-CYMD.                                00490000
Y2K  A                20  CL-PD-CC                    PIC 9(02).        00500000
Y2K  C                20  CL-PAID-DT                  PIC 9(6).              361
Y2K  C                20  CL-PAID-DT-X                                       361
Y2K  C                   REDEFINES   CL-PAID-DT.                                
Y2K  C                   25  CL-PD-YY                PIC 99.                 361
Y2K  C                   25  CL-PD-MM                PIC 99.                 363
Y2K  C                   25  CL-PD-DD                PIC 99.                 365
Y2K  A             15  CL-NEXT-REV-DT-CYMD            PIC 9(08).        00430000
Y2K  A             15  CL-NEXT-REV-DT-CYMD-X REDEFINES                  00440000
Y2K  A                  CL-NEXT-REV-DT-CYMD.                            00440000
Y2K  A                20  CL-NR-CCYY        PIC 9(04).                        45
Y2K  A                20  FILLER            PIC X(04).                        45
Y2K  A             15  CL-NEXT-REV-DATE-X REDEFINES                     00490000
Y2K  A                  CL-NEXT-REV-DT-CYMD.                            00490000
Y2K  A                20  CL-NR-CC                    PIC 9(02).        00500000
Y2K  C                20  CL-NEXT-REV-DT              PIC 9(6).              367
Y2K  C                20  CL-NEXT-REV-DT-X                                   367
Y2K  C                    REDEFINES   CL-NEXT-REV-DT.                           
Y2K  C                   25  CL-NR-YY                PIC 99.                 367
Y2K  C                   25  CL-NR-MM                PIC 99.                 369
Y2K  C                   25  CL-NR-DD                PIC 99.                 371
Y2K  A             15  CL-MISC-DT-CYMD            PIC 9(08).            00430000
Y2K  A             15  CL-MISC-DT-CYMD-X REDEFINES                      00440000
Y2K  A                   CL-MISC-DT-CYMD.                               00440000
Y2K  A                20  CL-MD-CCYY        PIC 9(04).                        45
Y2K  A                20  FILLER            PIC X(04).                        45
Y2K  A             15  CL-MISC-DATE-X REDEFINES                         00490000
Y2K  A                  CL-MISC-DT-CYMD.                                00490000
Y2K  A                20  CL-MD-CC                    PIC 9(02).        00500000
Y2K  C                20  CL-MISC-DT                  PIC 9(6).              373
Y2K  C                20  CL-MISC-DT-X                                       373
Y2K  C                   REDEFINES   CL-MISC-DT.                                
Y2K  C                  25  CL-MD-YY                PIC 99.                  373
Y2K  C                  25  CL-MD-MM                PIC 99.                  375
Y2K  C                  25  CL-MD-DD                PIC 99.                  377
005000*    SKIP2                                                                
005010         10  CL-LE-G.                                                  379
005020           15  CL-LE-ENTRY                 OCCURS 2.                   379
Y2K  A                 20  CL-LE-DT-CYMD       PIC 9(08).               00430000
Y2K  A                 20  CL-LE-DT-CYMD-X REDEFINES                    00440000
Y2K  A                      CL-LE-DT-CYMD.                              00440000
Y2K  A                    25  CL-LD-CCYY        PIC 9(04).                    45
Y2K  A                    25  FILLER            PIC X(04).                    45
Y2K  A                 20  CL-LE-DATE-X REDEFINES                       00490000
Y2K  A                      CL-LE-DT-CYMD.                              00490000
Y2K  A                    25  CL-LD-CC                PIC 9(02).        00500000
Y2K  C                    25  CL-LE-DT                PIC 9(6).              379
Y2K  C                    25  CL-LE-DT-X                                     379
Y2K  C                        REDEFINES   CL-LE-DT.                             
Y2K  C                       30  CL-LD-YY            PIC 99.                 379
Y2K  C                       30  CL-LD-MM            PIC 99.                 381
Y2K  C                       30  CL-LD-DD            PIC 99.                 383
005090                 20  CL-LE-NO                PIC 9.                    385
005100                 20  CL-LE-CODE              PIC X.                    386
005110                     88  CL-LE-CODE-VALID    VALUES                       
005120                            ' ' 'A' 'C' 'D' 'E' 'H' 'L' 'O'               
005130                                'P' 'Q' 'R' 'S' 'M'.                      
005140                     88  CL-LE-HA-VALID      VALUES                       
005150                                ' ' 'C' 'H' 'L' 'M' 'O' 'P'.              
005160                     88  CL-LE-PAY-VALID     VALUES                       
005170                                        ' ' 'A' 'E' 'Q' 'R'.              
005180                     88  CL-LE-HOLD-VALID    VALUES                       
005190                                'C' 'D' 'H' 'L' 'O' 'P'.                  
005200                     88  CL-LE-NO-ACT        VALUE ' '.                   
005210                     88  CL-LE-CLOSEOUT      VALUE 'C'.                   
005220                     88  CL-LE-DCI           VALUE 'D'.                   
005230                     88  CL-LE-DCI-RECD      VALUE 'E'.                   
005240                     88  CL-LE-HOLD          VALUE 'H'.                   
005250                     88  CL-LE-LETTER        VALUE 'A' 'L'.               
005260                     88  CL-LE-OUTSTAND      VALUE 'O' 'S'.               
005270                     88  CL-LE-PRE-CERT      VALUE 'P'.                   
005280                     88  CL-LE-PRE-CERT-RECD VALUE 'Q'.                   
005290                     88  CL-LE-RECVD         VALUE 'R'.                   
005300                     88  CL-LE-MANUAL-LTR    VALUE 'M'.                   
005310                 20  CL-LE-ID                PIC XX OCCURS 3.          387
005320         10  CL-MEMO.                                                  407
005330             15  CL-MEMO-ID                  PIC X.                    407
005340             15  CL-MEMO-TEXT                PIC X(69).                408
005350*    SKIP1                                                                
005360         10  CL-COB-ACD-DATA.                                          477
005370             15  CL-COB-DATA.                                          477
005380                 20  CL-COB-TYPE             PIC X.                    477
005390                     88  CL-COB-COB          VALUE 'C'.                   
005400                     88  CL-COB-MED          VALUE 'M'.                   
005410                 20  CL-COB-CODE             PIC X.                    478
005420                 20  CL-COB-OTHER            PIC S9(5)V99 COMP-3.      479
005430                 20  CL-COB-ALLOW-MAX        PIC S9(5)V99 COMP-3.      483
005440                 20  CL-COB-DISALLOW         PIC S9(5)V99 COMP-3.      487
005450                 20  CL-COB-SAVINGS          PIC S9(5)V99 COMP-3.      491
005460                 20  CL-COB-APPLIED          PIC S9(5)V99 COMP-3.      495
005470                 20  CL-COB-COP-DED          PIC S9(5)V99 COMP-3.      499
005480*    SKIP1                                                                
005490             15  CL-ACD-DATA.                                          503
005500                 20  CL-ACD-TYPE             PIC X.                    503
005510                     88  CL-ACD-UNUSED       VALUE ' '.                   
005520                     88  CL-ACD-ACCIDENT     VALUE 'A'.                   
005530                     88  CL-ACD-EMERGENCY    VALUE 'O'.                   
005540                 20  CL-ACD-AMT              PIC S9(5)V99 COMP-3.      504
Y2K  A                 20  CL-ACD-DT-CYMD       PIC 9(08).              00430000
Y2K  A                 20  CL-ACD-DT-CYMD-X REDEFINES                   00440000
Y2K  A                       CL-ACD-DT-CYMD.                            00440000
Y2K  A                    25  CL-ACD-CCYY        PIC 9(04).                   45
Y2K  A                    25  FILLER             PIC X(04).                   45
Y2K  A                 20  CL-ACD-DATE-X REDEFINES                      00490000
Y2K  A                      CL-ACD-DT-CYMD.                             00490000
Y2K  A                    25  CL-ACD-CC               PIC 9(02).        00500000
Y2K  C                    25  CL-ACD-DT               PIC 9(6).              508
Y2K  C                    25  CL-ACD-DT-X                                    508
Y2K  C                       REDEFINES   CL-ACD-DT.                             
Y2K  C                       30  CL-ACD-YY           PIC 99.                 508
Y2K  C                       30  CL-ACD-MM           PIC 99.                 510
Y2K  C                       30  CL-ACD-DD           PIC 99.                 512
005610*    SKIP2                                                                
005620         10  CL-MISC-MED-DEN-DATA.                                     514
005630             15  CL-TOT-CHG                  PIC S9(5)V99 COMP-3.      514
005640             15  CL-EOB-AMT-G.                                         518
005650                 20  CL-EOB-AMT   PIC S9(7)V99 COMP-3  OCCURS 8.       518
005660             15  CL-PY-STATUS                PIC X.                    558
005670                 88  CL-PY-NO-AUDIT          VALUE ' '.                   
005680                 88  CL-PY-AUDIT-ACCEPTED    VALUE '1'.                   
005690                 88  CL-PY-AUDIT-HOLD        VALUE '2' '4'.               
005700                 88  CL-PY-AUDIT-RELEASED    VALUE '3'.                   
005710                 88  CL-PY-AUDIT-HOLD-MSG    VALUE '4' '6'.               
005720                 88  CL-PY-AUDIT-PEND        VALUE '5' '6'.               
005730                 88  CL-PY-AUDIT-PEND-NO-MSG VALUE '5'.                   
005740                 88  CL-PY-AUDIT-PEND-MSG    VALUE '6'.                   
005750             15  CL-PY-EXPL                  PIC X(3).                 559
Y2K  A             15  CL-DRAG-DT-CYMD            PIC 9(08).            00430000
Y2K  A             15  CL-DRAG-DT-CYMD-X REDEFINES                      00440000
Y2K  A                    CL-DRAG-DT-CYMD.                              00440000
Y2K  A                20  CL-DRAG-DT-CCYY        PIC 9(04).                   45
Y2K  A                20  FILLER                 PIC X(04).                   45
Y2K  A             15  CL-DRAG-DATE-X REDEFINES                         00490000
Y2K  A                  CL-DRAG-DT-CYMD.                                00490000
Y2K  A                20  CL-DRAG-DT-CC               PIC 9(02).        00500000
Y2K  C                20  CL-DRAG-DT                  PIC 9(6).              562
Y2K  C                20  CL-DRAG-DT-X                                       562
Y2K  C                   REDEFINES   CL-DRAG-DT.                                
Y2K  C                   25  CL-DRAG-DT-YY           PIC 99.                 562
Y2K  C                   25  CL-DRAG-DT-MM           PIC 99.                 564
Y2K  C                   25  CL-DRAG-DT-DD           PIC 99.                 566
005820             15  CL-ALT-COV-IND              PIC X.                    568
005830                 88  CL-ALT-COV-PO           VALUE 'P'.                   
005840                 88  CL-ALT-COV-HP           VALUE 'H'                    
005850                                               'N' 'S' 'O' 'V'.           
005860                 88  CL-ALT-COV-CERT     VALUE 'N' 'S' 'O' 'V'.           
005870                 88  CL-ALT-COV-PO-HP        VALUE 'B'.                   
005880                 88  CL-ALT-COV-RA           VALUE 'R'.                   
005890                 88  CL-ALT-COV-MEDMGT       VALUE 'A' 'E' 'L'.           
005900             15  CL-DRAG-OVERRIDE-CODE       PIC X.                    569
005910                 88  CL-AI-DRAG-OVERRIDABLE  VALUE 'O'.                   
005920                 88  CL-CM-DRAG-OVERRIDE     VALUE 'C'.                   
005930                 88  CL-CNTL-DRAG-OVERRIDE   VALUE 'B'.                   
005940             15  CL-COB-PRORATE-IND          PIC X.                    570
005950                 88  CL-COB-SYS-PRORATE      VALUE 'P'.                   
005960                 88  CL-COB-MANUAL-CALC      VALUE 'C'.                   
005970             15  FILLER                      PIC X(03).                571
005980         10  CL-L-MAX                        PIC S9(3)    COMP-3.      574
005990         10  CL-C-MAX                        PIC S9(3)    COMP-3.      576
006000         10  CL-CODES-G.                                               578
006010             15  CL-SOURCE-CODE              PIC X.                    578
006020                 88  CL-SOURCE-OL-ADJUD      VALUE SPACE.                 
006030                 88  CL-SOURCE-OL-ENTRY      VALUE 'O'.                   
006040                 88  CL-SOURCE-BATCH-ENTRY   VALUE 'B'.                   
006050             15  CL-PAID-CODE                PIC X.                    579
006060                 88  CL-PAID-OL              VALUE SPACE.                 
006070                 88  CL-PAID-BATCH           VALUE 'B'.                   
006080         10  CL-BATCH-ONLY-FIELDS.                                     580
006090             15  CL-BATCH-ACTION-CODE        PIC X.                    580
006100                 88  CL-BATCH-ACTION-LOG     VALUE SPACE.                 
006110                 88  CL-BATCH-ACTION-PAY     VALUE 'P'.                   
006120                 88  CL-BATCH-ACTION-HOLD    VALUE 'H'.                   
006130                 88  CL-BATCH-ACTION-ERR     VALUE 'E'.                   
006140*    SKIP1                                                                
006150*****    EACH BIT IS USED AS AN ERROR IND    *****                        
006160             15  CL-BATCH-ADJUD-ERR-REASONS  PIC X(2).                 581
006170*    SKIP1                                                                
006180             15  CL-BATCH-ADJUD-PEND-REASON  PIC X.                    583
006190                 88  CL-PI-PEND-REASON       VALUE 'A'.                   
006200                 88  CL-ID-PEND-REASON       VALUE 'D'.                   
006210                 88  CL-EX-PEND-REASON       VALUE 'E'.                   
006220                 88  CL-IP-PEND-REASON       VALUE 'I'.                   
006230                 88  CL-ME-PEND-REASON       VALUE 'M'.                   
006240                 88  CL-PA-PEND-REASON       VALUE 'P'.                   
006250                 88  CL-PR-PEND-REASON       VALUE 'R'.                   
006260                 88  CL-SE-PEND-REASON       VALUE 'S'.                   
006270*    SKIP2                                                                
006280         10  CL-TF-CAPITATED-CLAIM-SW        PIC X(01).                584
006290             88  CL-TF-CAPITATED-CLAIM                  VALUE 'Y'.        
006300*    SKIP2                                                                
006310         10  CL-CE-FIXED-DATA.                                         585
006320             15  CL-CE-STATUS.                                         585
006330                 88  CL-CE-NO-CHG            VALUE '00'.                  
006340                 20  CL-CE-STATUS-1          PIC X.                    585
006350                     88  CL-CE-FORMAT          VALUE '1' '3'.             
006360                     88  CL-CE-RULE            VALUE '2' '3'.             
006370                     88  CL-CE-COMB            VALUE '4'.                 
006380                 20  CL-CE-STATUS-2          PIC X.                    586
006390                     88  CL-CE-DIS             VALUE '1' '3'.             
006400                     88  CL-CE-WARN            VALUE '2' '3'.             
006410             15  CL-CE-ID-PFX                PIC X(02).                587
006420*    SKIP1                                                                
006430         10  CL-SUPPRESS-EOBS-IND            PIC X(01).                589
006440             88  CL-SUPPRESS-EOBS              VALUE '6'.                 
006450*     SKIP1                                                                
006460         10  CL-SUP-ACD-DIS-X.                                         590
006470             15  CL-SUP-ACD-DIS              PIC 9(5)V99 COMP-3.       590
006480*    SKIP1                                                                
006490         10  FILLER                          PIC X(7).                 594
006500*    SKIP2                                                                
006510**************************************************                        
006520*    C H E C K   F I E L D S                     *                        
006530**************************************************                        
006540     05  CL-C-INFO-G.                                                  601
006550*    SKIP1                                                                
006560         10  CL-C-INFO                    OCCURS  3  TIMES             601
006570                                          INDEXED BY CL-C-IDX.            
006580             15  CL-C-STATUS                 PIC XX.                   601
006590                 88  CL-C-NOACTION           VALUE '00'.                  
006600                 88  CL-C-ALL-WAIT           VALUES                       
006610                                   '01' '05' '07' '31' '35' '41'.         
006620                 88  CL-C-WAITING            VALUE '01'.                  
006630                 88  CL-C-PRINTED            VALUE '02'.                  
006640                 88  CL-C-PAID               VALUE '03'.                  
006650                 88  CL-C-CASHED             VALUE '04' '08'.             
006660                 88  CL-C-STALE-CASHED       VALUE '08'.                  
006670                 88  CL-C-COMBINED           VALUES                       
006680                                   '05' '06' '07' '35' '36'.              
006690                 88  CL-C-COMB-WAITING       VALUE '05'.                  
006700                 88  CL-C-COMB-PRINTED       VALUE '06'.                  
006710                 88  CL-C-COMB-PPP-WAITING   VALUE '07'.                  
006720                 88  CL-C-EXTERNAL           VALUES                       
006730                                         '31' '32' '35' '36'              
006740                                         '41' '42'.                       
006750                 88  CL-C-EXT-WAITING        VALUE '31'.                  
006760                 88  CL-C-EXT-PRINTED        VALUE '32'.                  
006770                 88  CL-C-COMB-EXT-WAIT      VALUE '35'.                  
006780                 88  CL-C-COMB-EXT-PRINTED   VALUE '36'.                  
006790                 88  CL-C-CAPITATED-WAITING  VALUE '41'.                  
006800                 88  CL-C-CAPITATED-PRINTED  VALUE '42'.                  
006810                 88  CL-C-MANUAL             VALUE '81'.                  
006820                 88  CL-C-MANUAL-SUPPRESS    VALUE '82'.                  
006830                 88  CL-C-REISSUE-EOB        VALUE '83'.                  
006840                 88  CL-C-MANUAL-CASHED      VALUE '84' '88'.             
006850                 88  CL-C-MANUAL-STALE-CASHED VALUE '88'.                 
006860                 88  CL-C-ADJUSTED           VALUE '91'.                  
006870                 88  CL-C-CANCELLED          VALUE '92'.                  
006880                 88  CL-C-VOID               VALUE '93'.                  
006890                 88  CL-C-STOPPED            VALUE '94'.                  
006900                 88  CL-C-RETURNED           VALUE '95'.                  
006910                 88  CL-C-LOST               VALUE '96'.                  
006920                 88  CL-C-DESTROYED          VALUE '97'.                  
006930                 88  CL-C-MANUAL-VOID        VALUE '98'.                  
006940                 88  CL-C-CLOSEOUT           VALUE '99'.                  
Y2K  A             15  CL-C-ACTION-DT-CYMD            PIC 9(08).        00430000
Y2K  A             15  CL-C-ACTION-DT-CYMD-X REDEFINES                  00440000
Y2K  A                  CL-C-ACTION-DT-CYMD.                            00440000
Y2K  A                20  CL-C-A-CCYY        PIC 9(04).                       45
Y2K  A                20  FILLER             PIC X(04).                       45
Y2K  A             15  CL-C-ACTION-DATE-X REDEFINES                     00490000
Y2K  A                  CL-C-ACTION-DT-CYMD.                            00490000
Y2K  A                20  CL-C-A-CC                   PIC 9(02).        00500000
Y2K  C                20  CL-C-ACTION-DT              PIC 9(6).              603
Y2K  C                20  CL-C-ACTION-DT-X                                   603
Y2K  C                   REDEFINES   CL-C-ACTION-DT.                            
Y2K  C                   25  CL-C-A-YY               PIC 99.                 603
Y2K  C                   25  CL-C-A-MM               PIC 99.                 605
Y2K  C                   25  CL-C-A-DD               PIC 99.                 607
007010             15  CL-C-PAYEE                  PIC X.                    609
007020                 88  CL-C-PAY-MEM            VALUE 'M' 'N'.               
007030                 88  CL-C-PAY-ALT-MEM        VALUE 'N'.                   
007040                 88  CL-C-PAY-PROV           VALUE 'P' 'Q'.               
007050                 88  CL-C-PAY-ALT-PROV       VALUE 'Q'.                   
007060                 88  CL-C-PAY-ALT-PAYEE      VALUE 'A'.                   
007070             15  CL-C-PAYEE-ID.                                        610
007080                 20  CL-C-PAYEE-SSN          PIC X(9).                 610
007090                 20  CL-C-PAYEE-ID-SUF       PIC XX.                   619
007100             15  CL-C-COMPUTED               PIC S9(5)V99 COMP-3.      621
007110             15  CL-C-ACCUMULATED            PIC S9(5)V99 COMP-3.      625
007120             15  CL-C-COB-ADJ                PIC S9(5)V99 COMP-3.      629
007130             15  CL-C-ADJ-AMT                PIC S9(5)V99 COMP-3.      633
007140             15  CL-C-ADJ-EXPL               PIC X(03).                637
007150             15  CL-C-SYS-ADJ-AMT            PIC S9(5)V99 COMP-3.      640
007160             15  CL-C-SYS-ADJ-EXPL.                                    644
007170                 20  CL-C-SYS-ADJ-PFX        PIC X.                    644
007180                     88  CL-C-SYS-PO-ADJ     VALUE '›' '@'.               
007190                     88  CL-C-SYS-PO-DISC    VALUE '@'.                   
007200                     88  CL-C-SYS-ACD-ADJ    VALUE '$'.                   
007210                 20  CL-C-SYS-ADJ-SFX        PIC X.                    645
007220                 20  FILLER                  PIC X.                    646
007230             15  CL-C-SEG-AMT                PIC S9(5)V99 COMP-3.      647
007240             15  CL-C-COMB-CL-ID             PIC X(11).                651
007250             15  FILLER REDEFINES CL-C-COMB-CL-ID.                     651
007260                 20  CL-C-NO                 PIC X(8).                 651
007270                 20  CL-C-NO-FIL             PIC XXX.                  659
007280             15  CL-C-CHK-AMT                PIC S9(7)V99 COMP-3.      662
007290             15  CL-C-OVERPYMT               PIC S9(5)V99 COMP-3.      667
007300             15  CL-C-REC-AMT                PIC S9(5)V99 COMP-3.      671
007310             15  CL-C-WITHHOLD-AMT           PIC S9(5)V99 COMP-3.      675
007320             15  CL-C-WITHHOLD-EXPL.                                   679
007330                 20  CL-C-WITHHOLD-PFX       PIC X.                    679
007340                     88  CL-C-WITHHOLD-RW    VALUE '#'.                   
007350                 20  CL-C-WITHHOLD-SFX       PIC X.                    680
007360                 20  FILLER                  PIC X.                    681
007370             15  CL-C-TYPE                   PIC X(1).                 682
007380                 88  CL-C-EFT                VALUE 'E'.                   
007390                 88  CL-C-EFT-CANCELLED      VALUE 'C'.                   
007400             15  CL-C-SUBF-AND-EXPL.                                   683
007410                 20  CL-C-SUBF               PIC X.                    683
007420                     88  CL-C-SUBF-VALID     VALUES ' ' 'A' 'C'           
007430                                                    'D' 'M' 'N'           
007440                                                    'R' 'S' 'V'.          
007450                     88  CL-C-SUBFA          VALUE  'A'.                  
007460                     88  CL-C-SUBFC          VALUE  'C'.                  
007470                     88  CL-C-SUBFD          VALUE  'D'.                  
007480                     88  CL-C-SUBFM          VALUE  'M'.                  
007490                     88  CL-C-SUBFN          VALUE  'N'.                  
007500                     88  CL-C-SUBFR          VALUE  'R'.                  
007510                     88  CL-C-SUBFS          VALUE  'S'.                  
007520                     88  CL-C-SUBFV          VALUE  'V'.                  
007530                 20  CL-C-EXPL               PIC X(3).                 684
007540             15  FILLER                      PIC X(14).                687
Y2K  C     05  CL-NON-FIXED-DATA                   PIC X(3262).              901
007560**************************************************                        
007570*    M E D I C A L   O N L Y   F I E L D S       *                        
007580**************************************************                        
007590*    SKIP1                                                                
007600     05  CL-MED-NON-FIXED-DATA                                         901
007610         REDEFINES CL-NON-FIXED-DATA.                                     
007620*    SKIP2                                                                
007630         10  CL-MED-MISC-NON-FIXED.                                    901
007640             15  CL-MED-ADMIT-PR             PIC X(11).                901
007650             15  CL-MED-ID-G.                                          912
007660                 20 CL-MED-ID-ID             PIC X(6) OCCURS 5.        912
007670*****  HC-PROC (5 BYTE FIELD) FROM CFERTHC IS        *****                
007680*****  STORED IN CL-MED-IP-ID (7 BYTE FIELD).        *****                
007690             15  CL-MED-IP-G.                                          942
007700                 20 CL-MED-IP-ID             PIC X(7) OCCURS 3.        942
007710             15  CL-MED-PROC-ID              PIC X(7).                 963
007720             15  CL-MED-DC-STAT              PIC XX.                   970
007730                 88  CL-MED-DC-STAT-VALID    VALUES                       
007740                         '01' '02' '03' '04' '05' '06'                    
007750                         '07' '20' '30'.                                  
007760             15  CL-MED-DC-STAT-TYPE         PIC X(03).                972
007770             15  CL-MED-PRE-AUTHO-DAYS       PIC S9(03)   COMP-3.      975
Y2K  A             15  CL-MED-PRE-AUTHO-DT-CYMD    PIC 9(08).           00430000
Y2K  A             15  CL-MED-PRE-AUTHO-DT-CYMD-X REDEFINES             00440000
Y2K  A                   CL-MED-PRE-AUTHO-DT-CYMD.                      00440000
Y2K  A                20  CL-MED-PA-CCYY        PIC 9(04).                    45
Y2K  A                20  FILLER                PIC X(04).                    45
Y2K  A             15  CL-MED-PRE-AUTHO-DATE-X REDEFINES                00490000
Y2K  A                  CL-MED-PRE-AUTHO-DT-CYMD.                       00490000
Y2K  A                20  CL-MED-PA-CC                PIC 9(02).        00500000
Y2K  C                20  CL-MED-PRE-AUTHO-DT         PIC 9(06).             977
Y2K  C                20  CL-MED-PRE-AUTHO-DT-X                              977
Y2K  C                   REDEFINES  CL-MED-PRE-AUTHO-DT.                        
Y2K  C                   25  CL-MED-PA-YY            PIC 99.                 977
Y2K  C                   25  CL-MED-PA-MM            PIC 99.                 979
Y2K  C                   25  CL-MED-PA-DD            PIC 99.                 981
007840             15  CL-MED-SYS-IND              PIC X.                    983
007850                 88  CL-MED-MED              VALUE ' ' 'M'.               
007860                 88  CL-MED-HOSP           VALUE ' ' 'H' 'R' 'A'.         
007870                 88  CL-MED-HC               VALUE 'H' 'R'.               
007880                 88  CL-MED-HA               VALUE 'A'.                   
007890                 88  CL-MED-HC-NO-RC         VALUE 'H'.                   
007900                 88  CL-MED-HC-RC            VALUE 'R'.                   
007910             15  CL-MED-RC-KEY1.                                       984
007920                 20  CL-MED-RC-ID.                                     984
007930                     25  CL-MED-CI-OL-OPT3   PIC X.                    984
007940                     25  CL-MED-CAUSE-CODE   PIC X.                    985
007950             15  CL-MED-DRG-CODE             PIC X(5).                 986
007960             15  CL-MED-CALC-IND             PIC X.                    991
007970                 88  CL-MED-CALC-DRG         VALUE 'D'.                   
007980                 88  CL-MED-CALC-UP-PR       VALUE 'E'.                   
007990                 88  CL-MED-CALC-PER-DIEM    VALUE 'P'.                   
008000             15  CL-MED-UP-BYPASS-IND        PIC X.                    992
008010                 88  CL-MED-UP-BYPASS        VALUE '@'.                   
008020             15  CL-MED-UP-CAT-OVR           PIC XX.                   993
008030             15  CL-MED-UP-DAYS-OVR-X.                                 995
008040                 20  CL-MED-UP-DAYS-OVR      PIC S9(03) COMP-3.        995
008050*    SKIP2                                                                
008060             15  FILLER                      PIC X(14).                997
008070*    SKIP2                                                                
008080         10  CL-H-ITEM-G.                                             1011
008090             15  CL-H-ITEM                   OCCURS 18 TIMES          1011
008100                                             INDEXED BY CL-H-IDX          
008110                                                        CL-H-IDX2.        
008120                 20  CL-H-FROM-MMDD          PIC 9(4).                1011
008130                 20  CL-H-FROM-MMDD-X                                 1011
008140                     REDEFINES   CL-H-FROM-MMDD.                          
008150                     25  CL-H-F-MM           PIC 99.                  1011
008160                     25  CL-H-F-DD           PIC 99.                  1013
008170                 20  CL-H-TO-MMDD            PIC 9(4).                1015
008180                 20  CL-H-TO-MMDD-X                                   1015
008190                     REDEFINES   CL-H-TO-MMDD.                            
008200                     25  CL-H-T-MM           PIC 99.                  1015
008210                     25  CL-H-T-DD           PIC 99.                  1017
008220                 20  CL-H-EXPL.                                       1019
008230                     25  CL-H-EXPL-PFX       PIC X(01).               1019
008240                     25  CL-H-EXPL-SFX       PIC X(02).               1020
008250                 20  CL-H-SE-POS.                                     1022
008260                     25  CL-H-SE-ID          PIC X(03).               1022
008270                     25  CL-H-POS            PIC X.                   1025
008280                 20  CL-H-SE-RULE            PIC X(03).               1026
008290                 20  CL-H-PR-KEY1.                                    1029
008300                     25  CL-H-PR-ID.                                  1029
008310                         30  CL-H-PR-SSN     PIC X(09).               1029
008320                         30  CL-H-PR-SUF     PIC XX.                  1038
008330                     25  FILLER REDEFINES CL-H-PR-ID.                 1029
008340                         30  CL-H-PR-SPEC    PIC X.                   1029
008350                             88  CL-H-PR-DMY VALUE '%'.                   
008360                         30  CL-H-PR-SPEC-ID PIC X(10).               1030
008370                 20  CL-H-PR-SPECIALTY       PIC X(04).               1040
008380                 20  CL-H-ROOM-TYPE          PIC X(02).               1044
008390                 20  CL-H-IP-ID-PFX          PIC XX.                  1046
008400                 20  CL-H-IP-ID.                                      1048
008410                     25  CL-H-RC-CODE        PIC X(3).                1048
008420                     25  FILLER              PIC X(4).                1051
008430                 20  CL-H-CE-IP-ID-G  REDEFINES  CL-H-IP-ID.          1048
008440                     25  CL-H-CE-IP-ID       PIC X(05).               1048
008450                     25  CL-H-CE-IP-SFX      PIC X(02).               1053
008460                 20  CL-H-REL-ID             PIC X(6).                1055
008470                 20  CL-H-ID-IDX             PIC 9.                   1061
008480                 20  CL-H-LE-IDX             PIC 9.                   1062
008490                 20  CL-H-PAN-IDX            PIC 9.                   1063
008500*    SKIP1                                                                
008510                 20  CL-H-PAYEE              PIC X.                   1064
008520                     88  CL-H-PAY-MEM        VALUE 'M' ' ' 'N'.           
008530                     88  CL-H-PAY-ALT-MEM    VALUE 'N'.                   
008540                     88  CL-H-PAY-PROV       VALUE 'P' 'Q'.               
008550                     88  CL-H-ALT-PAYEE      VALUE 'A'.                   
008560                     88  CL-H-PAY-ALT-PROV   VALUE 'Q'.                   
008570*    SKIP1                                                                
008580                 20  CL-H-OR-CODE            PIC XXX.                 1065
008590*****            EACH BIT IS USED AS OVERRIDE IND         *****           
008600                 20  CL-H-OR-IND             PIC XXX.                 1068
008610*    SKIP1                                                                
008620                 20  CL-H-OR-X-AMT           PIC S9(5)V99 COMP-3.     1071
008630                 20  CL-H-OR-P-AMT           PIC S9(5)V99 COMP-3.     1075
008640*    SKIP1                                                                
008650                 20  CL-H-ALT-RULE-IND       PIC X.                   1079
008660                     88  CL-H-ALT-RULE-SSO   VALUE 'S' 'B'.               
008670                     88  CL-H-ALT-RULE-OPS   VALUE 'O' 'B'.               
008680                     88  CL-H-ALT-RULE-SSO-OPS VALUE 'B'.                 
008690*    SKIP2                                                                
008700                 20  CL-H-PF-IND             PIC X.                   1080
008710                     88  CL-H-PF-USED        VALUE 'P'.                   
008720                     88  CL-H-EXT-PRICE      VALUE 'O'.                   
008730                     88  CL-H-UP-PRICE       VALUE 'E'.                   
008740*    SKIP1                                                                
008750                 20  CL-H-CAP-IND            PIC X.                   1081
008760                     88  CL-H-CAPITATED      VALUE 'C'.                   
008770*    SKIP1                                                                
008780                 20  CL-H-CHG                PIC S9(5)V99 COMP-3.     1082
008790                 20  CL-H-CONSIDERED-CHG     PIC S9(5)V99 COMP-3.     1086
008800                 20  CL-H-CTR                PIC S9(3)    COMP-3.     1090
008810                 20  CL-H-RATE               PIC S9(5)V99 COMP-3.     1092
008820                 20  CL-H-DISALLOW           PIC S9(5)V99 COMP-3.     1096
008830                 20  CL-H-UE-DISALLOW        PIC S9(5)V99 COMP-3.     1100
008840                 20  CL-H-UE-DISALLOW-EXPL   PIC X(03).               1104
008850                 20  CL-H-UE-AMT1            PIC S9(5)V99 COMP-3.     1107
008860                 20  CL-H-UE-AMT1-EXPL       PIC X(03).               1111
008870                 20  CL-H-ADJ-AMT            PIC S9(5)V99 COMP-3.     1114
008880                 20  CL-H-COB-OTHER REDEFINES                         1114
008890                     CL-H-ADJ-AMT            PIC S9(5)V99 COMP-3.         
008900                 20  CL-H-ADJ-IND            PIC X.                   1118
008910                 20  CL-H-ADJ-EXPL           PIC X(03).               1119
008920                 20  CL-H-COB-IND-G REDEFINES                         1119
008930                     CL-H-ADJ-EXPL.                                       
008940                     25  CL-H-COB-OTHER-IND     PIC X.                1119
008950                     25  CL-H-COB-ALLOW-MAX-IND PIC X.                1120
008960                     25  CL-H-COB-DISALLOW-IND  PIC X.                1121
008970                 20  CL-H-CHK-SEG-AMT        PIC S9(5)V99 COMP-3.     1122
008980                 20  CL-H-COB-ALLOW-MAX REDEFINES                     1122
008990                     CL-H-CHK-SEG-AMT        PIC S9(5)V99 COMP-3.         
009000                 20  CL-H-B-MM          OCCURS  2  TIMES.             1126
009010                     25  CL-H-ALLOW          PIC S9(5)V99 COMP-3.     1126
009020                     25  CL-H-CTR-ALLOW      PIC S9(3)    COMP-3.     1130
009030                     25  CL-H-DEDUCT         PIC S9(5)V99 COMP-3.     1132
009040                     25  CL-H-SE-OPT         PIC X.                   1136
009050*****                    ONLY FOR BASIC TIER                *****         
009060                         88  CL-H-EXC-BASIC-FROM-CHK VALUE 'B'.           
009070*****                    ONLY FOR MAJOR TIER                *****         
009080                         88  CL-H-BASIC-CARVEOUT     VALUE 'B'.           
009090                         88  CL-H-EBP                VALUE 'E'.           
009100                     25  CL-H-DED-T          PIC S9       COMP-3.     1137
009110                         88  CL-H-DED-PERCAUSE VALUE +9.                  
009120                     25  CL-H-COPAY          PIC S9(5)V99 COMP-3.     1138
009130                     25  CL-H-COB-ADJ        PIC S9(5)V99 COMP-3.     1142
009140                     25  CL-H-PAID           PIC S9(5)V99 COMP-3.     1146
009150                 20  CL-H-REF-LINE-PTR       PIC 9.                   1174
009160                 20  CL-H-AUTH-LINE-PTR REDEFINES                     1174
009170                     CL-H-REF-LINE-PTR       PIC 9.                       
009180                 20  CL-H-PROC-OPTION8  OCCURS 2 TIMES.               1175
009190                     25  CL-H-PROC-OPT8      PIC X(01).               1175
009200                 20  CL-H-CE-ACT-IND         PIC X(01).               1177
009210                     88  CL-H-CE-ACT-NONE      VALUE SPACE.               
009220                     88  CL-H-CE-ACT-DIS       VALUE 'D'.                 
009230                     88  CL-H-CE-ACT-RULE      VALUE 'R'.                 
009240                     88  CL-H-CE-ACT-WARN      VALUE 'W'.                 
009250***                  REASON-IND SAME AS WARN NO.  *****                   
009260                 20  CL-H-CE-REAS-IND        PIC X(02).               1178
009270                     88  CL-H-CE-REAS-NONE     VALUE SPACE.               
009280                 20  CL-H-CE-LINE-PTR        PIC X(01).               1180
009290                     88  CL-H-CE-CLAIM-LINE    VALUE '1' '2' '3'          
009300                                                     '4' '5' '6'.         
009310                     88  CL-H-CE-CLAIM-SEG     VALUE 'C'.                 
009320                     88  CL-H-CE-CLAIM-HIST    VALUE 'H'.                 
009330                 20  CL-H-COB-DISALLOW       PIC S9(5)V99 COMP-3.     1181
009340                 20  FILLER                  PIC X(01).               1185
009350**************************************************                        
009360*    D E N T A L     O N L Y    F I E L D S      *                        
009370**************************************************                        
009380     05  CL-DEN-NON-FIXED-DATA                                         901
009390         REDEFINES  CL-NON-FIXED-DATA.                                    
009400         10  CL-DEN-MISC-NON-FIXED.                                    901
009410             15  CL-DEN-PR-KEY1.                                       901
009420                 20  CL-DEN-PR-ID.                                     901
009430                     25  CL-DEN-PR-SSN     PIC X(9).                   901
009440                     25  CL-DEN-PR-SUF     PIC XX.                     910
009450                 20  FILLER  REDEFINES  CL-DEN-PR-ID.                  901
009460                     25  CL-DEN-PR-SPEC    PIC X.                      901
009470                         88  CL-DEN-PR-DMY VALUE '%'.                     
009480                     25  CL-DEN-PR-SPEC-ID PIC X(10).                  902
009490             15  CL-DEN-PAYEE              PIC X.                      912
009500                 88  CL-DEN-PAY-MEM        VALUE 'M' 'N'.                 
009510                 88  CL-DEN-PAY-ALT-MEM    VALUE 'N'.                     
009520                 88  CL-DEN-PAY-PROV       VALUE 'P' 'Q'.                 
009530                 88  CL-DEN-PAY-ALT-PROV   VALUE 'Q'.                     
009540                 88  CL-DEN-PAY-ALT-PAYEE  VALUE 'A'.                     
009550             15  CL-DEN-PR-SPEC-CODE       PIC X.                      913
009560                 88  CL-DEN-PR-SPEC-CODE-VALID                            
009570                                          VALUES ' ' '1' '2' '3'.         
009580                 88  CL-DEN-PART-PROVIDER VALUES ' ' '1'.                 
009590             15  CL-DEN-OR-P-AMT           PIC S9(5)V99 COMP-3.        914
009600             15  FILLER                    PIC X(23).                  918
009610         10  CL-D-ITEM-G.                                              941
009620             15  CL-D-ITEM      OCCURS  8  TIMES                       941
009630                                INDEXED BY CL-D-IDX                       
009640                                           CL-D-IDX2.                     
009650                 20  CL-D-FROM-MMDD        PIC 9(4).                   941
009660                 20  CL-D-FROM-MMDD-X                                  941
009670                     REDEFINES   CL-D-FROM-MMDD.                          
009680                     25  CL-D-F-MM         PIC 99.                     941
009690                     25  CL-D-F-DD         PIC 99.                     943
009700                 20  CL-D-EXPL             PIC X(03).                  945
009710                 20  CL-D-CG-ID            PIC XX.                     948
009720                 20  CL-D-CG-RULE          PIC X(03).                  950
009730                 20  CL-D-DP-ID-ORIG       PIC X(05).                  953
009740                 20  CL-D-DP-ID            PIC X(05).                  958
009750                 20  CL-D-DP-ID-REL        PIC X(05).                  963
009760                 20  CL-D-DP-RULE          PIC X(03).                  968
009770                 20  CL-D-IP-ID-PFX        PIC XX.                     971
009780*    SKIP1                                                                
009790                 20  CL-D-LE-IDX-X.                                    973
009800                     25  CL-D-LE-IDX       PIC 9.                      973
009810                 20  CL-D-OR-CODE          PIC XXX.                    974
009820*****            EACH BIT IS USED AS OVERRIDE IND         *****           
009830                 20  CL-D-OR-IND           PIC XXX.                    977
009840*    SKIP1                                                                
009850                 20  CL-D-OR-X-AMT         PIC S9(5)V99 COMP-3.        980
009860*    SKIP1                                                                
009870                 20  CL-D-R-C-AMT          PIC S9(5)V99   COMP-3.      984
009880*    SKIP1                                                                
009890                 20  CL-D-TC-CODE          PIC X.                      988
009900                 20  CL-D-UT-CODE          PIC X.                      989
009910                 20  CL-D-TOOTH            PIC XX.                     990
009920*****                SURF CODES ARE 'ALL ' FOR 5,6,7 SURFACES             
009930*****                               M O D L F B V                         
009940*****                M-MESIAL,O-OCUSAL,D-DISTAL,L-LINGUAL                 
009950*****                F- FACIAL,B-BUCCAL,V-VESTIBULAR,I-INCISAL            
009960                 20  CL-D-SURF.                                        992
009970                     25  CL-D-SURF-E       PIC X  OCCURS 4.            992
009980                 20  CL-D-TOOTH-RANGE.                                 996
009990                     25  CL-D-LOW-TOOTH    PIC XX.                     996
010000                     25  CL-D-HIGH-TOOTH   PIC XX.                     998
010010*    SKIP1                                                                
010020                 20  CL-D-PF-IND           PIC X.                     1000
010030                     88  CL-D-PF-USED      VALUE 'P'.                     
010040                     88  CL-D-EXT-PRICE    VALUE 'O'.                     
010050*    SKIP1                                                                
010060                 20  CL-D-CAP-IND          PIC X.                     1001
010070                     88  CL-D-CAPITATED    VALUE 'C'.                     
010080*    SKIP2                                                                
010090                 20  CL-D-CHG              PIC S9(5)V99   COMP-3.     1002
010100                 20  CL-D-CONSIDERED-CHG   PIC S9(5)V99   COMP-3.     1006
010110                 20  CL-D-CTR              PIC S9(3)      COMP-3.     1010
010120                 20  CL-D-DISALLOW         PIC S9(5)V99   COMP-3.     1012
010130                 20  CL-D-UE-DISALLOW      PIC S9(5)V99   COMP-3.     1016
010140                 20  CL-D-UE-DISALLOW-EXPL PIC X(03).                 1020
010150                 20  CL-D-UE-AMT1          PIC S9(5)V99   COMP-3.     1023
010160                 20  CL-D-UE-AMT1-EXPL     PIC X(03).                 1027
010170                 20  CL-D-ADJ-AMT          PIC S9(5)V99   COMP-3.     1030
010180                 20  CL-D-COB-OTHER REDEFINES                         1030
010190                     CL-D-ADJ-AMT            PIC S9(5)V99 COMP-3.         
010200                 20  CL-D-ADJ-IND          PIC X.                     1034
010210                 20  CL-D-ADJ-EXPL         PIC X(03).                 1035
010220                 20  CL-D-COB-IND-G REDEFINES                         1035
010230                     CL-D-ADJ-EXPL.                                       
010240                     25  CL-D-COB-OTHER-IND     PIC X.                1035
010250                     25  CL-D-COB-ALLOW-MAX-IND PIC X.                1036
010260                     25  CL-D-COB-DISALLOW-IND  PIC X.                1037
010270                 20  CL-D-CHK-SEG-AMT      PIC S9(5)V99   COMP-3.     1038
010280                 20  CL-D-COB-ALLOW-MAX REDEFINES                     1038
010290                     CL-D-CHK-SEG-AMT        PIC S9(5)V99 COMP-3.         
010300                 20  CL-D-ALLOW            PIC S9(5)V99   COMP-3.     1042
010310                 20  CL-D-CTR-ALLOW        PIC S9(3)      COMP-3.     1046
010320                 20  CL-D-DEDUCT           PIC S9(5)V99   COMP-3.     1048
010330                 20  CL-D-DED-T            PIC S9         COMP-3.     1052
010340                 20  CL-D-COPAY            PIC S9(5)V99   COMP-3.     1053
010350                 20  CL-D-COB-ADJ          PIC S9(5)V99   COMP-3.     1057
010360                 20  CL-D-PAID             PIC S9(5)V99   COMP-3.     1061
010370                 20  CL-D-COB-DISALLOW     PIC S9(5)V99 COMP-3.       1065
010380                 20  FILLER                PIC X(12).                 1069
010390*****************************************************                     
010400*    D I S A B I L I T Y    O N L Y    F I E L D S  *                     
010410*****************************************************                     
010420     05  CL-DIS-NON-FIXED-DATA                                         901
010430         REDEFINES CL-NON-FIXED-DATA.                                     
010440         10  CL-DIS-DB-PARM-DATA.                                      901
010450             15  CL-DIS-BEN-OPTIONS.                                   901
010460                 20  CL-DIS-BEN-OPT2              PIC X.               901
010470                     88  CL-DIS-MIN-ALLOW         VALUE ' ' 'P'.          
010480                     88  CL-DIS-MIN-PYMT          VALUE 'P'.              
010490                 20  CL-DIS-BEN-OPT5              PIC X.               902
010500                     88  CL-DIS-VAR-WEEK          VALUE 'V' 'W'.          
010510                     88  CL-DIS-VAR-WEEK-FIX      VALUE 'W'.              
010520                 20  CL-DIS-DB-OPT8               PIC X.               903
010530                     88  CL-DIS-DB-OPT8-VALID     VALUES                  
010540                                  SPACE '1' 'J' '6' 'N' 'B' 'C'.          
010550                     88  CL-DIS-DB-COLA-NO        VALUE SPACES.           
010560                     88  CL-DIS-DB-COLA-AFTER-BA                          
010570                                              VALUE '1' '6' 'B'.          
010580                     88  CL-DIS-DB-COLA-AFTER-2BA VALUE 'C'.              
010590                     88  CL-DIS-DB-COLA-JAN       VALUE '1' 'J'.          
010600                     88  CL-DIS-DB-COLA-0101      VALUE  '1'.             
010610                     88  CL-DIS-DB-COLA-FIRST-JAN VALUE 'J'.              
010620                     88  CL-DIS-DB-COLA-JUN       VALUE '6' 'N'.          
010630                     88  CL-DIS-DB-COLA-0601      VALUE '6'.              
010640                     88  CL-DIS-DB-COLA-FIRST-JUN VALUE 'N'.              
010650                     88  CL-DIS-DB-COLA-BA        VALUE 'B'.              
010660                     88  CL-DIS-DB-COLA-2BA       VALUE 'C'.              
010670                 20  CL-DIS-DB-OPT10              PIC X(01).           904
010680                     88  CL-DIS-DB-COLA-NET       VALUE 'N'.              
010690                 20  FILLER                       PIC X(04).           905
010700             15  CL-DIS-DB-PYMT-OPTIONS.                               909
010710                 20  CL-DIS-DB-PYMT-OPT2          PIC X.               909
010720                     88  CL-DIS-DB-PYMT-BIWEEK-FST-CHK                    
010730                                                  VALUE 'F'.              
010740                 20  CL-DIS-DB-PYMT-OPT3          PIC X.               910
010750                     88  CL-DIS-PYMT-LAST-END-PER                         
010760                                                  VALUE SPACE.            
010770                     88  CL-DIS-PYMT-LAST-END-BEN VALUE 'B'.              
010780                 20  FILLER                       PIC X(06).           911
010790             15  CL-DIS-DT-ID                     PIC X(04).           917
010800             15  CL-DIS-EOB-GEN-IND               PIC X.               921
010810                 88  CL-DIS-EOB-GEN               VALUE 'E'.              
010820             15  CL-DIS-DB-COLA-TYPE              PIC X.               922
010830                 88  CL-DIS-DB-COLA-SIMPLE        VALUE 'S'.              
010840                 88  CL-DIS-DB-COLA-COMPOUND      VALUE 'C'.              
010850             15  CL-DIS-DB-COLA-PCT        PIC S9(03)V99  COMP-3.      923
010860             15  CL-DIS-DB-CLOSE-DAYS      PIC S9(03)     COMP-3.      926
010870             15  CL-DIS-DB-COLA-ID         PIC X(02).                  928
010880             15  FILLER                    PIC X(02).                  930
010890         10  CL-DIS-PAY-PARM.                                          932
010900             15  CL-DIS-HOURS-PER-DAY      PIC S9(03)     COMP-3.      932
010910             15  CL-DIS-DAYS-PER-WEEK      PIC S9(03)     COMP-3.      934
010920             15  CL-DIS-DAYS-PER-MONTH     PIC S9(03)     COMP-3.      936
010930             15  CL-DIS-PYMT-FREQ          PIC X.                      938
010940                 88  CL-DIS-PYMT-WEEK      VALUE 'W'.                     
010950                 88  CL-DIS-PYMT-BIWEEK    VALUE 'B'.                     
010960                 88  CL-DIS-PYMT-MONTH     VALUE 'M'.                     
010970                 88  CL-DIS-PYMT-SEMI-MONTH VALUE 'S'.                    
010980             15  CL-DIS-PYMT-DAY           PIC XX.                     939
010990             15  CL-DIS-PYMT-ADVANCE       PIC S9(03)     COMP-3.      941
011000             15  CL-DIS-ER-CONT-PCT        PIC S9(3)V99   COMP-3.      943
011010             15  CL-DIS-ER-CONT-PCT-F   REDEFINES                      943
011020                 CL-DIS-ER-CONT-PCT        PIC S9V9(4)    COMP-3.         
011030             15  CL-DIS-FW-PCT             PIC S9(5)V99   COMP-3.      946
011040             15  CL-DIS-FW-PCT-F        REDEFINES                      946
011050                 CL-DIS-FW-PCT             PIC S9(3)V9(4) COMP-3.         
011060             15  CL-DIS-MIN-PYMT-AMT       PIC S9(5)V99   COMP-3.      950
011070             15  CL-DIS-PENSION-PCT        PIC S9(3)V99   COMP-3.      954
011080             15  CL-DIS-PENSION-PCT-F   REDEFINES                      954
011090                 CL-DIS-PENSION-PCT        PIC S9V9(4)    COMP-3.         
011100             15  CL-DIS-PAY-FUTURE-FIELDS.                             957
Y2K110***              20  CL-DIS-PAY-FILL-DATE-1  PIC 9(6).                 957
Y2K  C                 20  CL-DIS-PAY-FILL-DATE-CYMD-1  PIC 9(8).            957
011120                 20  CL-DIS-PAY-FILL-CTR-1   PIC S9(5)    COMP-3.      963
011130                 20  CL-DIS-PAY-FILL-AMT-1   PIC S9(7)V99 COMP-3.      966
011140                 20  FILLER                  PIC X(09).                971
011150         10  CL-DIS-SCREEN-PARM.                                       980
011160             15  CL-DIS-NWD-G.                                         980
011170                 20  CL-DIS-NWD            PIC XX   OCCURS 4.          980
011180             15  CL-DIS-ID-ID-G.                                       988
011190                 20 CL-DIS-ID-ID           PIC X(6) OCCURS 2.          988
011200                 20 CL-DIS-REL-ID          PIC X(6) OCCURS 2.         1000
011210             15  CL-DIS-CAUSE              PIC X.                     1012
011220                 88  CL-DIS-ILLNESS        VALUE 'I'.                     
011230                 88  CL-DIS-HOSP           VALUE 'H'.                     
011240                 88  CL-DIS-ACCIDENT       VALUE 'A'.                     
011250                 88  CL-DIS-OCCU           VALUE 'O'.                     
011260                 88  CL-DIS-NO-FAULT       VALUE 'N'.                     
011270             15  CL-DIS-DUR-BASE           PIC X.                     1013
011280                 88  CL-DIS-DUR-WEEK       VALUE 'W'.                     
011290                 88  CL-DIS-DUR-MONTH      VALUE 'M'.                     
011300             15  CL-DIS-DUR                PIC S999       COMP-3.     1014
011310             15  CL-DIS-SAL-BASE           PIC X.                     1016
011320                 88  CL-DIS-SAL-WEEK       VALUE 'W'.                     
011330                 88  CL-DIS-SAL-MONTH      VALUE 'M'.                     
011340             15  CL-DIS-SAL                PIC S9(7)V99   COMP-3.     1017
011350             15  CL-DIS-BEN-PCT-IND-X.                                1022
011360                 20  CL-DIS-BEN-PCT-IND    PIC 9.                     1022
011370             15  CL-DIS-SDR                PIC S999       COMP-3.     1023
011380             15  CL-DIS-SVC-YRS            PIC S999       COMP-3.     1025
011390             15  CL-DIS-PR-KEY1.                                      1027
011400                 20  CL-DIS-PR-ID.                                    1027
011410                     25  CL-DIS-PR-SSN     PIC X(9).                  1027
011420                     25  CL-DIS-PR-SUF     PIC XX.                    1036
011430                 20  FILLER REDEFINES CL-DIS-PR-ID.                   1027
011440                     25  CL-DIS-PR-SPEC    PIC X.                     1027
011450                         88  CL-DIS-PR-DMY VALUE '%'.                     
011460                     25  CL-DIS-PR-SPEC-ID PIC X(10).                 1028
011470             15  CL-DIS-PAYEE              PIC X.                     1038
011480                 88  CL-DIS-PAY-MEM        VALUE  ' ' 'M' 'N'.            
011490                 88  CL-DIS-PAY-ALT-MEM    VALUE 'N'.                     
011500                 88  CL-DIS-PAY-EXTERNAL   VALUE 'E'.                     
011510                 88  CL-DIS-PAY-PROV       VALUE 'P' 'Q'.                 
011520                 88  CL-DIS-PAY-ALT-PROV   VALUE 'Q'.                     
011530                 88  CL-DIS-PAY-ALT-PAYEE  VALUE 'A'.                     
011540             15  CL-DIS-CONT-CL-KEY1.                                 1039
011550                 20  CL-DIS-CONT-CL-ID     PIC X(9).                  1039
011560                 20  CL-DIS-CONT-CL-GEN-X.                            1048
011570                     25  CL-DIS-CONT-CL-GEN PIC 99.                   1048
011580             15  CL-DIS-AP-CODE            PIC X.                     1050
011590                 88  CL-DIS-AP-CL-NO       VALUE ' ' 'C'.                 
011600                 88  CL-DIS-AP-ME-ID       VALUE 'M'.                     
011610             15  CL-DIS-EFT-CODE           PIC X.                     1051
011620                 88  CL-DIS-EFT            VALUE 'E'.                     
011630             15  FILLER                    PIC X(10).                 1052
011640         10  CL-DIS-DATES.                                            1062
Y2K  A             15  CL-DIS-ACD-DT-CYMD    PIC 9(08).                 00430000
Y2K  A             15  CL-DIS-ACD-DT-CYMD-X REDEFINES                   00440000
Y2K  A                   CL-DIS-ACD-DT-CYMD.                            00440000
Y2K  A                20  CL-DIS-ACD-CCYY        PIC 9(04).                   45
Y2K  A                20  FILLER                 PIC X(04).                   45
Y2K  A             15  CL-DIS-ACD-DATE-X REDEFINES                      00490000
Y2K  A                  CL-DIS-ACD-DT-CYMD.                             00490000
Y2K  A                20  CL-DIS-ACD-CC             PIC 9(02).          00500000
Y2K  C                20  CL-DIS-ACD-DT             PIC 9(6).               1062
Y2K  C                20  CL-DIS-ACD-DT-X REDEFINES CL-DIS-ACD-DT.          1062
Y2K  C                   25  CL-DIS-ACD-YY         PIC 99.                  1062
Y2K  C                   25  CL-DIS-ACD-MM         PIC 99.                  1064
Y2K  C                   25  CL-DIS-ACD-DD         PIC 99.                  1066
Y2K  A             15  CL-DIS-LDW-DT-CYMD    PIC 9(08).                 00430000
Y2K  A             15  CL-DIS-LDW-DT-CYMD-X REDEFINES                   00440000
Y2K  A                    CL-DIS-LDW-DT-CYMD.                           00440000
Y2K  A                20  CL-DIS-LDW-CCYY        PIC 9(04).                   45
Y2K  A                20  FILLER                 PIC X(04).                   45
Y2K  A             15  CL-DIS-LDW-DATE-X REDEFINES                      00490000
Y2K  A                  CL-DIS-LDW-DT-CYMD.                             00490000
Y2K  A                20  CL-DIS-LDW-CC             PIC 9(02).          00500000
Y2K  C                20  CL-DIS-LDW-DT             PIC 9(6).               1068
Y2K  C                20  CL-DIS-LDW-DT-X REDEFINES CL-DIS-LDW-DT.          1068
Y2K  C                   25  CL-DIS-LDW-YY         PIC 99.                  1068
Y2K  C                   25  CL-DIS-LDW-MM         PIC 99.                  1070
Y2K  C                   25  CL-DIS-LDW-DD         PIC 99.                  1072
Y2K  A             15  CL-DIS-IPV-DT-CYMD    PIC 9(08).                 00430000
Y2K  A             15  CL-DIS-IPV-DT-CYMD-X REDEFINES                   00440000
Y2K  A                   CL-DIS-IPV-DT-CYMD.                            00440000
Y2K  A                20  CL-DIS-IPV-CCYY        PIC 9(04).                   45
Y2K  A                20  FILLER                 PIC X(04).                   45
Y2K  A             15  CL-DIS-IPV-DATE-X REDEFINES                      00490000
Y2K  A                  CL-DIS-IPV-DT-CYMD.                             00490000
Y2K  A                20  CL-DIS-IPV-CC             PIC 9(02).          00500000
Y2K  C                20  CL-DIS-IPV-DT             PIC 9(6).               1074
Y2K  C                20  CL-DIS-IPV-DT-X REDEFINES CL-DIS-IPV-DT.          1074
Y2K  C                   25  CL-DIS-IPV-YY         PIC 99.                  1074
Y2K  C                   25  CL-DIS-IPV-MM         PIC 99.                  1076
Y2K  C                   25  CL-DIS-IPV-DD         PIC 99.                  1078
Y2K  A             15  CL-DIS-LPV-DT-CYMD    PIC 9(08).                 00430000
Y2K  A             15  CL-DIS-LPV-DT-CYMD-X REDEFINES                   00440000
Y2K  A                   CL-DIS-LPV-DT-CYMD.                            00440000
Y2K  A                20  CL-DIS-LPV-CCYY        PIC 9(04).                   45
Y2K  A                20  FILLER                 PIC X(04).                   45
Y2K  A             15  CL-DIS-LPV-DATE-X REDEFINES                      00490000
Y2K  A                  CL-DIS-LPV-DT-CYMD.                             00490000
Y2K  A                20  CL-DIS-LPV-CC             PIC 9(02).          00500000
Y2K  C                20  CL-DIS-LPV-DT             PIC 9(6).               1080
Y2K  C                20  CL-DIS-LPV-DT-X REDEFINES CL-DIS-LPV-DT.          1080
Y2K  C                   25  CL-DIS-LPV-YY         PIC 99.                  1080
Y2K  C                   25  CL-DIS-LPV-MM         PIC 99.                  1082
Y2K  C                   25  CL-DIS-LPV-DD         PIC 99.                  1084
Y2K  A             15  CL-DIS-RTW-DT-CYMD    PIC 9(08).                 00430000
Y2K  A             15  CL-DIS-RTW-DT-CYMD-X REDEFINES                   00440000
Y2K  A                  CL-DIS-RTW-DT-CYMD.                             00440000
Y2K  A                20  CL-DIS-RTW-CCYY        PIC 9(04).                   45
Y2K  A                20  FILLER                 PIC X(04).                   45
Y2K  A             15  CL-DIS-RTW-DATE-X REDEFINES                      00490000
Y2K  A                  CL-DIS-RTW-DT-CYMD.                             00490000
Y2K  A                20  CL-DIS-RTW-CC             PIC 9(02).          00500000
Y2K  C                20  CL-DIS-RTW-DT             PIC 9(6).               1086
Y2K  C                20  CL-DIS-RTW-DT-X REDEFINES CL-DIS-RTW-DT.          1086
Y2K  C                   25  CL-DIS-RTW-YY         PIC 99.                  1086
Y2K  C                   25  CL-DIS-RTW-MM         PIC 99.                  1088
Y2K  C                   25  CL-DIS-RTW-DD         PIC 99.                  1090
Y2K  A             15  CL-DIS-CONT-FROM-DT-CYMD    PIC 9(08).           00430000
Y2K  A             15  CL-DIS-CONT-FROM-DT-CYMD-X REDEFINES             00440000
Y2K  A                   CL-DIS-CONT-FROM-DT-CYMD.                      00440000
Y2K  A                20  CL-DIS-CONT-F-CCYY        PIC 9(04).                45
Y2K  A                20  FILLER                    PIC X(04).                45
Y2K  A             15  CL-DIS-CONT-FROM-DATE-X REDEFINES                00490000
Y2K  A                  CL-DIS-CONT-FROM-DT-CYMD.                       00490000
Y2K  A                20  CL-DIS-CONT-F-CC          PIC 9(02).          00500000
Y2K  C                20  CL-DIS-CONT-FROM-DT       PIC 9(6).               1092
Y2K  C                20  CL-DIS-CONT-FROM-DT-X                             1092
Y2K  C                    REDEFINES   CL-DIS-CONT-FROM-DT.                      
Y2K  C                   25  CL-DIS-CONT-F-YY      PIC 99.                  1092
Y2K  C                   25  CL-DIS-CONT-F-MM      PIC 99.                  1094
Y2K  C                   25  CL-DIS-CONT-F-DD      PIC 99.                  1096
Y2K  C             15  CL-DIS-DATES-FIL1-CYMD    PIC 9(8).                  1098
Y2K  A             15  CL-DIS-DATES-FIL1-CYMD-X REDEFINES                   1098
Y2K  A                   CL-DIS-DATES-FIL1-CYMD.                            1098
Y2K  A               20  CL-DIS-DATES-FIL1-CC      PIC 9(2).                    
Y2K  C               20  CL-DIS-DATES-FIL1         PIC 9(6).                1098
011970         10  CL-DIS-LE-G.                                             1104
011980             15  CL-DIS-LE-DATA  OCCURS  2  TIMES.                    1104
Y2K  A                 20  CL-DIS-LE-DT-CYMD    PIC 9(08).              00430000
Y2K  A                 20  CL-DIS-LE-DT-CYMD-X REDEFINES                00440000
Y2K  A                      CL-DIS-LE-DT-CYMD.                          00440000
Y2K  A                    25  CL-DIS-LE-CCYY        PIC 9(04).                45
Y2K  A                    25  FILLER                PIC X(04).                45
Y2K  A                 20  CL-DIS-LE-DATE-X REDEFINES                   00490000
Y2K  A                     CL-DIS-LE-DT-CYMD.                           00490000
Y2K  A                    25  CL-DIS-LE-CC          PIC 9(02).          00500000
Y2K  C                    25  CL-DIS-LE-DT          PIC 9(6).               1104
Y2K  C                    25  CL-DIS-LE-DT-X REDEFINES CL-DIS-LE-DT.        1104
Y2K  C                       30  CL-DIS-LE-YY      PIC 99.                  1104
Y2K  C                       30  CL-DIS-LE-MM      PIC 99.                  1106
Y2K  C                       30  CL-DIS-LE-DD      PIC 99.                  1108
012040                 20  CL-DIS-LE-NO          PIC 9.                     1110
012050                 20  CL-DIS-LE-CODE        PIC X.                     1111
012060                     88  CL-DIS-LE-CODE-VALID VALUES                      
012070                                           ' ' 'A' 'L' 'E' 'R'.           
012080                     88  CL-DIS-LE-LETTER     VALUE 'A' 'L'.              
012090                     88  CL-DIS-LE-EXCEPT     VALUE 'E'.                  
012100                     88  CL-DIS-LE-OUTSTAND   VALUE 'O'.                  
012110                     88  CL-DIS-LE-RECVD      VALUE 'R'.                  
012120                 20  CL-DIS-LE-ID          PIC XX OCCURS 3.           1112
012130                 20  CL-DIS-LE-OPT-G.                                 1118
012140                     25  CL-DIS-LE-OPT1    PIC X.                     1118
012150                         88 CL-DIS-LE-DAYS-PRIOR    VALUE ' '.            
012160                         88 CL-DIS-LE-WEEKS-FROM    VALUE 'W'.            
012170                         88 CL-DIS-LE-MONTHS-FROM   VALUE 'M'.            
012180                         88 CL-DIS-LE-MONTHS-REPEAT VALUE 'R'.            
012190                     25  CL-DIS-LE-OPT2    PIC X.                     1119
012200                     25  CL-DIS-LE-OPT3    PIC X.                     1120
012210                     25  CL-DIS-LE-OPT4    PIC X.                     1121
012220                 20  CL-DIS-LE-EXPL        PIC X(03).                 1122
012230         10  CL-DIS-OR-FIELDS.                                        1146
012240             15  CL-DIS-OR-IND-G.                                     1146
012250                 20 CL-DIS-OR-PLAN-IND     PIC X.                     1146
012260                    88 CL-DIS-OR-PLAN      VALUE 'E'.                     
012270                 20 CL-DIS-OR-COV-IND      PIC X.                     1147
012280                    88 CL-DIS-OR-COV       VALUE 'F'.                     
012290                 20 CL-DIS-OR-HIST-IND     PIC X.                     1148
012300                    88 CL-DIS-OR-HIST      VALUE 'H'.                     
012310                 20 CL-DIS-OR-SITE-IND     PIC X.                     1149
012320                    88 CL-DIS-OR-SITE      VALUE 'S'.                     
012330                 20 CL-DIS-OR-TYPE-IND     PIC X.                     1150
012340                    88 CL-DIS-OR-TYPE      VALUE 'T'.                     
012350                 20 CL-DIS-OR-VOID-REPAY-IND PIC X.                   1151
012360                    88 CL-DIS-OR-VOID-REPAY  VALUE 'T'.                   
012370                 20 CL-DIS-OR-FICA1-IND    PIC X.                     1152
012380                    88 CL-DIS-OR-FICA1     VALUE '1'.                     
012390                 20 CL-DIS-OR-FICA2-IND    PIC X.                     1153
012400                    88 CL-DIS-OR-FICA2     VALUE '2'.                     
012410                 20 CL-DIS-OR-DT-IND       PIC X.                     1154
012420                    88 CL-DIS-OR-PTD       VALUE 'D'.                     
012430                    88 CL-DIS-OR-BTD-PTD   VALUE 'B'.                     
012440                 20  FILLER                PIC X(03).                 1155
012450             15  CL-DIS-OR-CAN-EXEMPT      PIC S9(5)V99   COMP-3.     1158
012460             15  CL-DIS-OR-PROV-EXEMPT     PIC S9(5)V99   COMP-3.     1162
Y2K  A             15  CL-DIS-OR-DT1-CYMD    PIC 9(08).                 00430000
Y2K  A             15  CL-DIS-OR-DT1-CYMD-X REDEFINES                   00440000
Y2K  A                  CL-DIS-OR-DT1-CYMD.                             00440000
Y2K  A                20  CL-DIS-OR-DT1-CCYY        PIC 9(04).                45
Y2K  A                20  FILLER                    PIC X(04).                45
Y2K  A             15  CL-DIS-OR-DATE1-X REDEFINES                      00490000
Y2K  A                  CL-DIS-OR-DT1-CYMD.                             00490000
Y2K  A                 20  CL-DIS-OR-DT1-CC          PIC 9(02).         00500000
Y2K  C                 20  CL-DIS-OR-DT1             PIC 9(06).             1166
Y2K  C                 20  CL-DIS-OR-DT1-X  REDEFINES CL-DIS-OR-DT1.        1166
Y2K  C                   25  CL-DIS-OR-DT1-YY      PIC 99.                  1166
Y2K  C                   25  CL-DIS-OR-DT1-MM      PIC 99.                  1168
Y2K  C                   25  CL-DIS-OR-DT1-DD      PIC 99.                  1170
Y2K  A             15  CL-DIS-OR-DT2-CYMD    PIC 9(08).                 00430000
Y2K  A             15  CL-DIS-OR-DT2-CYMD-X REDEFINES                   00440000
Y2K  A                    CL-DIS-OR-DT2-CYMD.                           00440000
Y2K  A                20  CL-DIS-OR-DT2-CCYY        PIC 9(04).                45
Y2K  A                20  FILLER                    PIC X(04).                45
Y2K  A             15  CL-DIS-OR-DATE2-X REDEFINES                      00490000
Y2K  A                  CL-DIS-OR-DT2-CYMD.                             00490000
Y2K  A                 20  CL-DIS-OR-DT2-CC          PIC 9(02).         00500000
Y2K  C                 20  CL-DIS-OR-DT2             PIC 9(06).             1172
Y2K  C                 20  CL-DIS-OR-DT2-X REDEFINES CL-DIS-OR-DT2.         1172
Y2K  C                   25  CL-DIS-OR-DT2-YY      PIC 99.                  1172
Y2K  C                   25  CL-DIS-OR-DT2-MM      PIC 99.                  1174
Y2K  C                   25  CL-DIS-OR-DT2-DD      PIC 99.                  1176
012570         10  CL-DIS-FICA-PARM-DATA.                                   1178
012580*****        CONTAINS 2 YEARS FROM DIS-FROM DATE       *****              
012590             15  FILLER                    PIC X(04).                 1178
012600             15  CL-DIS-FICA-PARM     OCCURS 2  TIMES.                1182
Y2K  A                 20  CL-DIS-FICA-CCYY           PIC 9(04).        00430000
Y2K  A                 20  CL-DIS-FICA-CCYY-X  REDEFINES                00440000
Y2K  A                           CL-DIS-FICA-CCYY.                      00440000
Y2K  A                    25  CL-DIS-FICA-CC-X.                               45
Y2K  A                       30  CL-DIS-FICA-CC          PIC 99.              45
Y2K  C                    25  CL-DIS-FICA-YY-X.                             1182
Y2K  C                       30  CL-DIS-FICA-YY    PIC 99.                  1182
012630                 20  CL-DIS-FICA-MAX-ALLOW PIC S9(7)      COMP-3.     1184
012640                 20  CL-DIS-FICA-PCT       PIC S99V999    COMP-3.     1188
012650                 20  CL-DIS-FICA-PCT-F REDEFINES                      1188
012660                     CL-DIS-FICA-PCT       PIC SV9(5)     COMP-3.         
012670*    SKIP2                                                                
012680         10  CL-DIS-CURR-PYMT-LINE.                                   1200
012690*****    ACCUMULATED ON ZERO SEGMENT                   *****              
012700*****    IT IS ADJUSTED ON-ZERO FOR NON ZERO RECALL    *****              
012710*****        RUN  DATE        *****                                       
Y2K  A             15  CL-DIS-PER-RD-DT-CYMD    PIC 9(08).              00430000
Y2K  A             15  CL-DIS-PER-RD-DT-CYMD-X REDEFINES                00440000
Y2K  A                   CL-DIS-PER-RD-DT-CYMD.                         00440000
Y2K  A                20  CL-DIS-PER-RD-CCYY        PIC 9(04).                45
Y2K  A                20  FILLER                    PIC X(04).                45
Y2K  A             15  CL-DIS-PER-RD-DATE-X REDEFINES                   00490000
Y2K  A                  CL-DIS-PER-RD-DT-CYMD.                          00490000
Y2K  A                 20  CL-DIS-PER-RD-CC          PIC 9(02).         00500000
Y2K  C                 20  CL-DIS-PER-RD-DT          PIC 9(6).              1200
Y2K  C                 20  CL-DIS-PER-RD-DT-X     REDEFINES                 1200
Y2K  C                     CL-DIS-PER-RD-DT.                                    
Y2K  C                    25  CL-DIS-PER-RD-YY      PIC 99.                 1200
Y2K  C                    25  CL-DIS-PER-RD-MM      PIC 99.                 1202
Y2K  C                    25  CL-DIS-PER-RD-DD      PIC 99.                 1204
012780             15  CL-DIS-PER-BASE           PIC X.                     1206
012790*****            BASE IN WEEK FOR STD AND MONTHS FOR LTD                  
012800                 88  CL-DIS-PER-BASE-WEEK    VALUE 'W'.                   
012810                 88  CL-DIS-PER-BASE-MONTH   VALUE 'M'.                   
012820             15  CL-DIS-PERIOD             PIC S999V9(8)  COMP-3.     1207
012830             15  CL-DIS-PERIOD-F        REDEFINES                     1207
012840                 CL-DIS-PERIOD             PIC S9(11) COMP-3.             
Y2K  A             15  CL-DIS-PER-FROM-DT-CYMD    PIC 9(08).            00430000
Y2K  A             15  CL-DIS-PER-FROM-DT-CYMD-X REDEFINES              00440000
Y2K  A                   CL-DIS-PER-FROM-DT-CYMD.                       00440000
Y2K  A                20  CL-DIS-PER-F-CCYY        PIC 9(04).                 45
Y2K  A                20  FILLER                   PIC X(04).                 45
Y2K  A             15  CL-DIS-PER-FROM-DATE-X REDEFINES                 00490000
Y2K  A                  CL-DIS-PER-FROM-DT-CYMD.                        00490000
Y2K  A                 20  CL-DIS-PER-F-CC           PIC 9(02).         00500000
Y2K  C                 20  CL-DIS-PER-FROM-DT        PIC 9(6).              1213
Y2K  C                 20  CL-DIS-PER-FROM-DT-X   REDEFINES                 1213
Y2K  C                     CL-DIS-PER-FROM-DT.                                  
Y2K  C                    25  CL-DIS-PER-F-YY       PIC 99.                 1213
Y2K  C                    25  CL-DIS-PER-F-MM       PIC 99.                 1215
Y2K  C                    25  CL-DIS-PER-F-DD       PIC 99.                 1217
Y2K  A             15  CL-DIS-PER-TO-DT-CYMD     PIC 9(8).                  1217
Y2K  A             15  CL-DIS-PER-TO-DT-CYMD-X REDEFINES                    1217
Y2K  A                 CL-DIS-PER-TO-DT-CYMD.                               1217
Y2K  C               20  CL-DIS-PER-TO-DT-CC       PIC 99.                  1219
Y2K  C               20  CL-DIS-PER-TO-DT          PIC 9(6).                1221
Y2K  C               20  CL-DIS-PER-TO-DT-X     REDEFINES                   1221
Y2K  C                   CL-DIS-PER-TO-DT.                                      
Y2K  C                 25  CL-DIS-PER-TO-YY      PIC 99.                    1221
Y2K  C                 25  CL-DIS-PER-TO-MM      PIC 99.                    1223
Y2K  C                 25  CL-DIS-PER-TO-DD      PIC 99.                    1225
012980             15  CL-DIS-PER-ALLOW          PIC S9(7)V99   COMP-3.     1227
012990             15  CL-DIS-PER-FICA-ALLOW     PIC S9(7)V99   COMP-3.     1232
013000             15  CL-DIS-PER-FICA           PIC S9(7)V99   COMP-3.     1237
013010             15  CL-DIS-PER-OFF-ITEM   OCCURS 6 TIMES.                1242
013020                 20  CL-DIS-PER-OFF-ID.                               1242
013030                     25  CL-DIS-PER-OFF-ID-PRE  PIC X.                1242
013040                     25  CL-DIS-PER-OFF-ID-SUF  PIC X.                1243
013050                         88 CL-DIS-PER-NEG-OFF        VALUE '+'.          
013060                 20  CL-DIS-PER-OFF        PIC S9(7)V99   COMP-3.     1244
013070             15  CL-DIS-PER-DED-ITEM   OCCURS 5 TIMES.                1284
013080                 20  CL-DIS-PER-DED-ID     PIC XX.                    1284
013090                 20  CL-DIS-PER-DED        PIC S9(7)V99   COMP-3.     1286
013100             15  FILLER.                                              1319
013110                 20  CL-DIS-PER-MISC-ID    PIC XX.                    1319
013120                 20  CL-DIS-PER-MISC       PIC S9(7)V99   COMP-3.     1321
013130             15  CL-DIS-PER-PENSION        PIC S9(7)V99   COMP-3.     1326
013140             15  CL-DIS-PER-PAID           PIC S9(7)V99   COMP-3.     1331
013150             15  CL-DIS-PER-COLA-PCT-USED  PIC S9(3)V99   COMP-3.     1336
013160             15  CL-DIS-PER-COLA-PCT-USED-F REDEFINES                 1336
013170                 CL-DIS-PER-COLA-PCT-USED  PIC S9V9(4)    COMP-3.         
013180             15  CL-DIS-PER-COLA           PIC S9(7)V99   COMP-3.     1339
013190             15  CL-DIS-PER-FIL-AMT1       PIC S9(7)V99   COMP-3.     1344
013200             15  FILLER                    PIC X(14).                 1349
013210*    SKIP2                                                                
013220         10  CL-DIS-TOT-G.                                            1363
013230*****    THESE ARE CUMULATIVE TOTALS ON ZERO AND NON-ZERO SEG.            
013240*****    AFTER ADJUSTMENT OF NON-ZERO THESE TOTALS ON NON-ZERO            
013250*****    ARE AS OF LAST PAYMENT (GAGA)                                    
013260*    SKIP1                                                                
013270             15  CL-DIS-TOT-STAT           PIC X.                     1363
013280                 88  CL-DIS-TOT-OK         VALUE ' ' .                    
013290                 88  CL-DIS-TOT-GAGA       VALUE 'G'.                     
013300*    SKIP2                                                                
013310             15  CL-DIS-YTD-TOTALS.                                   1364
Y2K  A                 20  CL-DIS-YTD-CCYY           PIC 9(04).         00322700
Y2K  A                 20  CL-DIS-YTD-CCYY-X  REDEFINES                 00322800
Y2K  A                     CL-DIS-YTD-CCYY.                             00322800
Y2K  A                    25  CL-DIS-YTD-CC-X.                          00322900
Y2K  A                       30  CL-DIS-YTD-CC         PIC 9(02).       00322900
Y2K  C                    25  CL-DIS-YTD-YY-X.                              1364
Y2K  C                       30  CL-DIS-YTD-YY     PIC 99.                  1364
013340                 20  CL-DIS-YTD-ALLOW      PIC S9(7)V99   COMP-3.     1366
013350                 20  CL-DIS-YTD-FICA-ALLOW PIC S9(7)V99   COMP-3.     1371
013360                 20  CL-DIS-YTD-FICA       PIC S9(7)V99   COMP-3.     1376
013370                 20  CL-DIS-YTD-OFF        PIC S9(7)V99   COMP-3.     1381
013380                 20  CL-DIS-YTD-DED        PIC S9(7)V99   COMP-3.     1386
013390                 20  CL-DIS-YTD-MISC       PIC S9(7)V99   COMP-3.     1391
013400                 20  CL-DIS-YTD-PENSION    PIC S9(7)V99   COMP-3.     1396
013410                 20  CL-DIS-YTD-PAID       PIC S9(7)V99   COMP-3.     1401
013420                 20  CL-DIS-YTD-COLA       PIC S9(7)V99   COMP-3.     1406
013430                 20  CL-DIS-YTD-FIL-AMT1   PIC S9(7)V99   COMP-3.     1411
013440*    SKIP2                                                                
013450             15  CL-DIS-DTD-TOTALS.                                   1416
013460                 20  CL-DIS-DTD-ALLOW      PIC S9(7)V99   COMP-3.     1416
013470                 20  CL-DIS-DTD-FICA-ALLOW PIC S9(7)V99   COMP-3.     1421
013480                 20  CL-DIS-DTD-FICA       PIC S9(7)V99   COMP-3.     1426
013490                 20  CL-DIS-DTD-OFF        PIC S9(7)V99   COMP-3.     1431
013500                 20  CL-DIS-DTD-DED        PIC S9(7)V99   COMP-3.     1436
013510                 20  CL-DIS-DTD-MISC       PIC S9(7)V99   COMP-3.     1441
013520                 20  CL-DIS-DTD-PENSION    PIC S9(7)V99   COMP-3.     1446
013530                 20  CL-DIS-DTD-PAID       PIC S9(7)V99   COMP-3.     1451
013540                 20  CL-DIS-DTD-COLA       PIC S9(7)V99   COMP-3.     1456
013550                 20  CL-DIS-DTD-FIL-AMT1   PIC S9(7)V99   COMP-3.     1461
013560*    SKIP1                                                                
013570         10  CL-DIS-COMM-B-INFO.                                      1466
013580             15  CL-DIS-LAST-CL-KEY1.                                 1466
013590                 20  CL-DIS-LAST-CL-ID     PIC X(9).                  1466
013600                 20  CL-DIS-LAST-CL-GEN-X.                            1475
013610                     25  CL-DIS-LAST-CL-GEN PIC 99.                   1475
013620             15  CL-DIS-BEN-EXPL           PIC X(3).                  1477
013630                 88 CL-DIS-BEN-DISALLOW              VALUE 'S8'           
013640                                                           'S9'.          
Y2K  A             15  CL-DIS-BEN-FROM-DT-CYMD    PIC 9(08).            00430000
Y2K  A             15  CL-DIS-BEN-FROM-DT-CYMD-X REDEFINES              00440000
Y2K  A                   CL-DIS-BEN-FROM-DT-CYMD.                       00440000
Y2K  A                20  CL-DIS-BEN-F-CCYY        PIC 9(04).                 45
Y2K  A                20  FILLER                   PIC X(04).                 45
Y2K  A             15  CL-DIS-BEN-FROM-DATE-X REDEFINES                 00490000
Y2K  A                  CL-DIS-BEN-FROM-DT-CYMD.                        00490000
Y2K  A                 20  CL-DIS-BEN-F-CC           PIC 9(02).         00500000
Y2K  C                 20  CL-DIS-BEN-FROM-DT        PIC 9(6).              1480
Y2K  C                 20  CL-DIS-BEN-FROM-DT-X                             1480
Y2K  C                    REDEFINES CL-DIS-BEN-FROM-DT.                         
Y2K  C                    25  CL-DIS-BEN-F-YY       PIC 99.                 1480
Y2K  C                    25  CL-DIS-BEN-F-MM       PIC 99.                 1482
Y2K  C                    25  CL-DIS-BEN-F-DD       PIC 99.                 1484
Y2K  A             15  CL-DIS-BEN-TO-DT-CYMD     PIC  9(8).                 1484
Y2K  A             15  CL-DIS-BEN-TO-DT-CYMD-X  REDEFINES                   1484
Y2K  A                 CL-DIS-BEN-TO-DT-CYMD.                               1484
Y2K  C               20  CL-DIS-BEN-TO-DT-CC       PIC 99.                  1486
Y2K  C               20  CL-DIS-BEN-TO-DT          PIC 9(6).                1488
Y2K  C               20  CL-DIS-BEN-TO-DT-X REDEFINES CL-DIS-BEN-TO-DT.     1488
Y2K  C                 25  CL-DIS-BEN-TO-YY      PIC 99.                    1488
Y2K  C                 25  CL-DIS-BEN-TO-MM      PIC 99.                    1490
Y2K  C                 25  CL-DIS-BEN-TO-DD      PIC 99.                    1492
013770             15  CL-DIS-BEN-TO-DT-MAX-CC   PIC 99.                    1494
01378              15  CL-DIS-BEN-TO-DT-MAX      PIC 9(6).                  1496
01379              15  CL-DIS-BEN-TO-DT-MAX-X    REDEFINES                  1496
01380                  CL-DIS-BEN-TO-DT-MAX.                                    
01381                 20  CL-DIS-BEN-TO-MAX-YY  PIC 99.                     1496
01382                 20  CL-DIS-BEN-TO-MAX-MM  PIC 99.                     1498
01383                 20  CL-DIS-BEN-TO-MAX-DD  PIC 99.                     1500
013840             15  CL-DIS-BEN-BASE           PIC X.                     1502
013850*****            BASE IN WEEK FOR STD AND MONTHS FOR LTD                  
013860                 88  CL-DIS-BEN-BASE-WEEK  VALUE 'W'.                     
013870                 88  CL-DIS-BEN-BASE-MONTH VALUE 'M'.                     
013880             15  CL-DIS-BEN-FIL-AMT1       PIC S9(5)V99   COMP-3.     1503
013890             15  CL-DIS-BEN-FIL-AMT2       PIC S9(5)V99   COMP-3.     1507
013900             15  FILLER                    PIC X(14).                 1511
013910*    SKIP2                                                                
013920         10  CL-B-MAX                      PIC S999       COMP-3.     1525
013930*    SKIP2                                                                
013940         10  FILLER                        PIC X(34).                 1527
013950*    SKIP2                                                                
013960         10  CL-B-ITEM-G.                                             1561
013970*****        ON NON-ZERO SEG THIS IS COPY OF NON-ZERO SEG DATA            
013980*****        BUT ON RECALLED CLAIM IT IS DATA ON SCREEN                   
013990             15  CL-B-ITEM  OCCURS 16 TIMES                           1561
014000                            INDEXED BY CL-B-IDX.                          
014010                 20  CL-B-ITEM-TYPE        PIC XX.                    1561
014020                     88  CL-B-BEN-AMT      VALUE 'B '.                    
014030                     88  CL-B-BEN-ADJ      VALUE 'BA'.                    
014040                     88  CL-B-COLA         VALUE 'CC' 'CP'.               
014050                     88  CL-B-COLA-CURR    VALUE 'CC'.                    
014060                     88  CL-B-COLA-PREV    VALUE 'CP'.                    
014070                     88  CL-B-FICA         VALUE 'F '.                    
014080                     88  CL-B-FICA-ADJ     VALUE 'FA'.                    
014090                     88  CL-B-MEDICARE     VALUE 'M '.                    
014100                     88  CL-B-OFF          VALUE 'O '.                    
014110                     88  CL-B-DED          VALUE 'D '.                    
Y2K  A                 20  CL-B-FROM-DT-CYMD    PIC 9(08).              00430000
Y2K  A                 20  CL-B-FROM-DT-CYMD-X REDEFINES                00440000
Y2K  A                       CL-B-FROM-DT-CYMD.                         00440000
Y2K  A                    25  CL-B-F-CCYY        PIC 9(04).                   45
Y2K  A                    25  FILLER             PIC X(04).                   45
Y2K  A                 20  CL-B-FROM-DATE-X REDEFINES                   00490000
Y2K  A                      CL-B-FROM-DT-CYMD.                          00490000
Y2K  A                    25  CL-B-F-CC             PIC 9(02).          00500000
Y2K  C                    25  CL-B-FROM-DT          PIC 9(6).               1563
Y2K  C                    25  CL-B-FROM-DT-X   REDEFINES                    1563
Y2K  C                        CL-B-FROM-DT.                                     
Y2K  C                       30  CL-B-F-YY         PIC 99.                  1563
Y2K  C                       30  CL-B-F-MM         PIC 99.                  1565
Y2K  C                       30  CL-B-F-DD         PIC 99.                  1567
Y2K  A                 20  CL-B-TO-DT-CYMD      PIC 9(08).              00430000
Y2K  A                 20  CL-B-TO-DT-CYMD-X   REDEFINES                00440000
Y2K  A                       CL-B-TO-DT-CYMD.                           00440000
Y2K  A                     25  CL-B-TO-CCYY      PIC 9(4).                  1571
Y2K  A                     25  FILLER            PIC X(4).                  1571
Y2K  A                 20  CL-B-TO-DT-CYMD-R   REDEFINES                00440000
Y2K  A                       CL-B-TO-DT-CYMD.                           00440000
Y2K  C                     25  CL-B-TO-DT-CC         PIC 99.                1569
Y2K  C                     25  CL-B-TO-DT            PIC 9(6).              1571
Y2K  C                     25  CL-B-TO-DT-X     REDEFINES                   1571
Y2K  C                        CL-B-TO-DT.                                       
Y2K  C                       30  CL-B-TO-YY        PIC 99.                  1571
Y2K  C                       30  CL-B-TO-MM        PIC 99.                  1573
Y2K  C                       30  CL-B-TO-DD        PIC 99.                  1575
014250                 20  CL-B-CD-ID.                                      1577
014260                     25 CL-B-CD-ID-PRE     PIC X.                     1577
014270                     25 CL-B-CD-ID-SUF     PIC X.                     1578
014280                        88 CL-B-NEG-OFF           VALUE '+'.              
014290                     25  FILLER            PIC X.                     1579
014300                 20  CL-B-ALLOW            PIC S9(5)V99   COMP-3.     1580
014310                 20  CL-B-OPT-G.                                      1584
014320                     25  CL-B-OPT1         PIC X.                     1584
014330                         88 CL-B-ALLOW-FLAT VALUE 'F'.                    
014340                         88 CL-B-CHK-ADJ   VALUE 'C'.                     
014350                     25  CL-B-OPT2         PIC X.                     1585
014360***                         OVER ON PREV COLA, NO ROLL                    
014370***                         OVER ON CURR COLA, NO FUTURE COLA             
014380                         88 CL-B-OVERRIDE  VALUE 'R'.                     
014390                     25  CL-B-OPT3         PIC X.                     1586
014400                     25  CL-B-OPT4         PIC X.                     1587
014410                 20  FILLER                PIC X(13).                 1588
014420**********************************************************                
014430*           ***** I M P O R T A N T *****                *                
014440*                                                        *                
014450*     IF YOU DEFINE FIELDS IN THIS FILLER AREA AT THE    *                
014460*     END OF THIS RECORD, BE SURE TO INSERT THE CODE     *                
014470*     TO INITIALIZE THE NEW FIELDS IN THE EXPANSION      *                
014480*     ROUTINE IN THE COPYBOOK (CFERPCLC). YOU MUST ALSO  *                
014490*     CHANGE THE VALUE OF THE APPROPRIATE FILLER USED    *                
014500*     FIELD (K-CL-XXX-FILL-USED) IN COPYBOOK (CFERKCL0). *                
014510*                                                        *                
014520**********************************************************                
014530*    SKIP2                                                                
014540*    THE FOLLOWING FIELDS ADDED AFTER 4.0                                 
014550*    SKIP2                                                                
014560     05  CL-FILL-AREA-G.                                              4161
Y2K  C         10  FILLER                        PIC X(402).                4161
014580*    SKIP1                                                                
014590     05  CL-MED-FILL-AREA-G     REDEFINES                             4161
014600         CL-FILL-AREA-G.                                                  
Y2K  C         10  FILLER                        PIC X(402).                4161
014620*    SKIP2                                                                
014630     05  CL-DEN-FILL-AREA-G     REDEFINES                             4161
014640         CL-FILL-AREA-G.                                                  
Y2K  C         10  FILLER                        PIC X(402).                4161
014660*    SKIP1                                                                
014670     05  CL-DISAB-FILL-AREA-G   REDEFINES                             4161
014680         CL-FILL-AREA-G.                                                  
014690***  TLC - THE FIELDS ARE FOR THE FICA/MEDICARE CHANGES AND               
014700***  MUST BE MOVED UP TO THE PROPER PLACE IN RELEASE 5.0.                 
014710         10  CL-DIS-FICA-FIELDS.                                      4161
014720             15  CL-DIS-FICA2-PARM    OCCURS 2 TIMES.                 4161
014730                 20  CL-DIS-FICA2-MAX-ALLOW PIC S9(7)    COMP-3.      4161
014740                 20  CL-DIS-FICA2-PCT       PIC S99V999  COMP-3.      4165
014750                 20  CL-DIS-FICA2-PCT-F REDEFINES                     4165
014760                     CL-DIS-FICA2-PCT       PIC SV9(5)   COMP-3.          
014770             15  CL-DIS-PER-TOTALS1-2.                                4175
014780                 20  CL-DIS-PER-FICA1-ALLOW PIC S9(7)V99 COMP-3.      4175
014790                 20  CL-DIS-PER-FICA2-ALLOW PIC S9(7)V99 COMP-3.      4180
014800                 20  CL-DIS-PER-FICA1       PIC S9(7)V99 COMP-3.      4185
014810                 20  CL-DIS-PER-FICA2       PIC S9(7)V99 COMP-3.      4190
014820             15  CL-DIS-YTD-TOTALS1-2.                                4195
014830                 20  CL-DIS-YTD-FICA1-ALLOW PIC S9(7)V99 COMP-3.      4195
014840                 20  CL-DIS-YTD-FICA2-ALLOW PIC S9(7)V99 COMP-3.      4200
014850                 20  CL-DIS-YTD-FICA1       PIC S9(7)V99 COMP-3.      4205
014860                 20  CL-DIS-YTD-FICA2       PIC S9(7)V99 COMP-3.      4210
014870             15  CL-DIS-DTD-TOTALS1-2.                                4215
014880                 20  CL-DIS-DTD-FICA1-ALLOW PIC S9(7)V99 COMP-3.      4215
014890                 20  CL-DIS-DTD-FICA2-ALLOW PIC S9(7)V99 COMP-3.      4220
014900                 20  CL-DIS-DTD-FICA1       PIC S9(7)V99 COMP-3.      4225
014910                 20  CL-DIS-DTD-FICA2       PIC S9(7)V99 COMP-3.      4230
014920*    SKIP1                                                                
014930             15  CL-DIS-TRACKING-SW         PIC X(01).                4235
014940                 88  CL-DIS-TRACKING-SW-VALID   VALUE ' ' 'C' 'N'.        
014950                 88  CL-DIS-NOT-CONVERTED-CLAIM VALUE ' ' .               
014960                 88  CL-DIS-CONVERTED-CLAIM     VALUE 'C'.                
014970                 88  CL-DIS-NEW-CLAIM           VALUE 'N'.                
014980*    SKIP1                                                                
Y2K  A             15  CL-DIS-CONV-DT-CYMD    PIC 9(08).                00430000
Y2K  A             15  CL-DIS-CONV-DT-CYMD-X REDEFINES                  00440000
Y2K  A                 CL-DIS-CONV-DT-CYMD.                             00440000
Y2K  A                20  CL-DIS-CONV-CCYY        PIC 9(04).                  45
Y2K  A                20  FILLER                  PIC X(04).                  45
Y2K  A             15  CL-DIS-CONV-DATE-X REDEFINES                     00490000
Y2K  A                  CL-DIS-CONV-DT-CYMD.                            00490000
Y2K  A                20  CL-DIS-CONV-CC        PIC 9(02).              00500000
Y2K  C                20  CL-DIS-CONV-DT        PIC 9(6).                   4236
Y2K  C                20  CL-DIS-CONV-DT-X                                  4236
Y2K  C                   REDEFINES  CL-DIS-CONV-DT.                             
Y2K  C                  25  CL-DIS-CONV-YY             PIC 99.              4236
Y2K  C                  25  CL-DIS-CONV-MM             PIC 99.              4238
Y2K  C                  25  CL-DIS-CONV-DD             PIC 99.              4240
015050             15  FILLER                     PIC X(19).                4242
015060         10  FILLER                         PIC X(300).               4261
015070     05  CL-CE-EXTERNAL-AREA-G  REDEFINES                             4161
015080         CL-FILL-AREA-G.                                                  
015090         10  CL-CE-EXT-FIXED-DATA.                                    4161
015100             15  CL-CE-EXT-LINE-CTR          PIC S9(03) COMP-3.       4161
015110             15  CL-CE-EXT-FIL               PIC X(14).               4163
015120         10  CL-CE-EXT-ACT-TBL-G.                                     4177
015130             15  CL-CE-EXT-ACT-TBL-E    OCCURS 6 TIMES                4177
015140                                INDEXED BY CL-CE-EXT-IDX.                 
015150                 20  CL-CE-EXT-FROM-MMDD-X   PIC X(04).               4177
015160                 20  CL-CE-EXT-TO-MMDD-X     PIC X(04).               4181
015170                 20  CL-CE-EXT-SE-POS.                                4185
015180                     25  CL-CE-EXT-SE-ID     PIC X(03).               4185
015190                     25  CL-CE-EXT-POS       PIC X(01).               4188
015200                 20  CL-CE-EXT-CHG           PIC S9(05)V99 COMP-3.    4189
015210                 20  CL-CE-EXT-CTR           PIC S9(03)    COMP-3.    4193
015220                 20  CL-CE-EXT-PR-KEY1       PIC X(11).               4195
015230                 20  CL-CE-EXT-PAYEE         PIC X(01).               4206
015240                 20  CL-CE-EXT-IP-ID         PIC X(07).               4207
015250                 20  CL-CE-EXT-ID-IDX        PIC 9.                   4214
015260                 20  CL-CE-EXT-LE-IDX        PIC 9.                   4215
015270                 20  CL-CE-EXT-PAN-IDX       PIC 9.                   4216
015280****                 FOLLOWING GENERATED BY CLINICAL EDITS                
015290                 20  CL-CE-EXT-SUGG-IP-ID    PIC X(07).               4217
015300                 20  CL-CE-EXT-BUND-TO-LNO   PIC 9(02).               4224
015310                 20  CL-CE-EXT-SWITCHES.                              4226
015320                     25  CL-CE-EXT-DEL-SW     PIC X(01).              4226
015330                         88  CL-CE-EXT-DEL    VALUE 'Y'.                  
015340                     25  CL-CE-EXT-BI-SW      PIC X(01).              4227
015350                         88  CL-CE-EXT-BI     VALUE 'Y'.                  
015360                     25  CL-CE-EXT-REBUND-SW  PIC X(01).              4228
015370                         88  CL-CE-EXT-REBUND VALUE 'Y'.                  
015380                     25  CL-CE-EXT-SGLB-SW    PIC X(01).              4229
015390                         88  CL-CE-EXT-SGLB   VALUE 'Y'.                  
015400                 20  FILLER                   PIC X(11).              4230
015410     05  CL-CUST-AREA-G.                                              4561
015420         10  FILLER                        PIC X(100).                4561
015430**********************************************************                
015440*            END OF ***  C F E R R C L 0 ***             *                
015450**********************************************************                
