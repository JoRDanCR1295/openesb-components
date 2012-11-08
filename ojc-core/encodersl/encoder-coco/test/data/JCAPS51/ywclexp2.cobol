000010**********************************************************
000020*    EXPANDED CLAIM RECORD      : EXPCLAIM               *
000030*        KEY LENGTH             : K-EC-KEY-LENGTH (44)   *
000040*        RECORD LENGTH          : K-EC-REC-LENGTH (16098)*
000050*                                                        *
000060*    CONTAINS CLAIM RELATED INFORMATION.                 *
000070*    09/24/99 ADDED 2 FIELDS: 'CL-NM-PFX PIC X(4)' AND   *
      *    'CL-DEN-USER-FIELD PIC X(10) OCCURS 3'              *
      *    THESE CHANGES DID NOT CHANGE THE LENGHT OF THE FILE *
      *    07/27/2000 ADDED EC-ME-KEY-CHG-CD PIC X(01)         *
      *    TAKING THE POSITION FROM EXISTING FILLER            *
      *    11/07/2000 CL-EOB-AMT-9 - 12  ARE ADDED TO          *
      *    CL-CUST-AREA-G.                                     *
001070**********************************************************
      **** EC-LENGHT FIELD COMMENTED FOR POC TEST ONLY'*********
001080**** 05  EC-LENGTH                            PIC X(4).                  1
001080     05  EC-CL-REC.                                                      1
001090         10  EC-KEY.                                                     1
001100             15  EC-PZ-ID                      PIC X(02).                1
001110             15  EC-CI-ID                      PIC X(02).                3
001120             15  EC-REC-ID                     PIC X(02).                5
001130                 88  EC-CLAIM-REC                VALUE 'CL'.
001140                 88  EC-BACKOUT-REC              VALUE 'CB'.
001150             15  EC-KEY1.                                                7
001160                 20  EC-PA-KEY1.                                         7
001170                     25  EC-ME-KEY1.                                     7
001180                         30  EC-PA-GROUP       PIC X(08).                7
001190                         30  EC-PA-ID          PIC X(09).               15
001200                     25  EC-PA-REL-NAME.                                24
001210                         30  EC-PA-REL         PIC X(01).               24
001220                         30  EC-PA-ID-NAME     PIC X(06).               25
001230                 20  EC-SYS-ID                 PIC X(01).               31
001240                     88  EC-DENTAL               VALUE 'D'.
001250                     88  EC-DISABILITY           VALUE 'K' 'L'.
001260                     88  EC-STD                  VALUE 'K'.
001270                     88  EC-LTD                  VALUE 'L'.
001280                     88  EC-MEDICAL              VALUE 'M'.
001290                 20  EC-CL-KEY1.                                        32
001300                     25  EC-CL-ID              PIC X(09).               32
001310                     25  EC-CL-GEN-X.                                   41
001320                         30  EC-CL-GEN         PIC 9(02).               41
001330             15  EC-KEY-FIL                    PIC XX.                  43
001340*******  10  EC-REMAINDER-CL-REC               PIC X(4616).             45
003390     05  CL-ODO-CTRS.                                                   45
003400         10  CL-C-ODO-CTR                    PIC S9(4)    COMP.         45
003410         10  CL-NFD-ODO-CTR                  PIC S9(4)    COMP.         47
003420         10  CL-FILL-ODO-CTR                 PIC S9(4)    COMP.         49
003430         10  CL-CUST-ODO-CTR                 PIC S9(4)    COMP.         51
003440     05  CL-RFIND-DATA.                                                 53
003450         10  CL-LAST-UPD-DATA.                                          53
003460             15  CL-LAST-UPD-DT              PIC 9(8).                  53
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
004680             15  CL-CL-YY-X.                                           347
004690                 20  CL-CL-YY                PIC 9(4).                 347
004700             15  CL-REC-DT                   PIC 9(8).                 349
004760             15  CL-INPUT-DT                 PIC 9(8).                 355
004820             15  CL-PAID-DT                  PIC 9(8).                 361
004880             15  CL-NEXT-REV-DT              PIC 9(8).                 367
004940             15  CL-MISC-DT                  PIC 9(8).                 373
005000*    SKIP2
005010         10  CL-LE-G.                                                  379
005020             15  CL-LE-ENTRY                 OCCURS 2.                 379
005030                 20  CL-LE-DT                PIC 9(8).                 379
005040                 20  CL-LE-DT-X                                        379
005050                     REDEFINES   CL-LE-DT.
005060                     25  CL-LD-CC            PIC 99.                   379
005060                     25  CL-LD-YY            PIC 99.                   379
005070                     25  CL-LD-MM            PIC 99.                   381
005080                     25  CL-LD-DD            PIC 99.                   383
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
005550                 20  CL-ACD-DT               PIC 9(8).                 508
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
005760             15  CL-DRAG-DT                  PIC 9(8).                 562
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
006450*    SKIP1
006460         10  CL-SUP-ACD-DIS-X.                                         590
006470             15  CL-SUP-ACD-DIS              PIC 9(5)V99 COMP-3.       590
006480*    SKIP1
IK0924         10  CL-NM-PFX                       PIC X(4).                 594
IK0924         10  FILLER                          PIC X(3).                 594
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
006950             15  CL-C-ACTION-DT              PIC 9(8).                 603
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
007550     05  CL-NON-FIXED-DATA                   PIC X(3362).              901
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
007780             15  CL-MED-PRE-AUTHO-DT         PIC 9(08).                977
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
                       20  CL-H-ITEM1.
008120                     25 CL-H-FROM-MMDD       PIC 9(4).                1011
008130                     25 CL-H-FROM-MMDD-X                              1011
008140                         REDEFINES CL-H-FROM-MMDD.
008150                         30 CL-H-F-MM        PIC 99.                  1011
008160                         30 CL-H-F-DD        PIC 99.                  1013
008170                     25 CL-H-TO-MMDD         PIC 9(4).                1015
008180                     25 CL-H-TO-MMDD-X                                1015
008190                         REDEFINES CL-H-TO-MMDD.
008200                         30 CL-H-T-MM        PIC 99.                  1015
008210                         30 CL-H-T-DD        PIC 99.                  1017
008220                     25 CL-H-EXPL.                                    1019
008230                         30 CL-H-EXPL-PFX    PIC X(01).               1019
008240                         30 CL-H-EXPL-SFX    PIC X(02).               1020
008250                     25 CL-H-SE-POS.                                  1022
008260                         30 CL-H-SE-ID       PIC X(03).               1022
008270                         30 CL-H-POS         PIC X.                   1025
008280                     25 CL-H-SE-RULE         PIC X(03).               1026
008290                     25 CL-H-PR-KEY1.                                 1029
008300                         30 CL-H-PR-ID.                               1029
008310                             35 CL-H-PR-SSN  PIC X(09).               1029
008320                             35 CL-H-PR-SUF  PIC XX.                  1038
008330                         30 FILLER REDEFINES CL-H-PR-ID.              1029
008340                             35 CL-H-PR-SPEC PIC X.                   1029
008350                                 88 CL-H-PR-DMY VALUE '%'.
008360                             35 CL-H-PR-SPEC-ID PIC X(10).            1030
008370                     25 CL-H-PR-SPECIALTY    PIC X(04).               1040
008380                     25 CL-H-ROOM-TYPE       PIC X(02).               1044
008390                     25 CL-H-IP-ID-PFX       PIC XX.                  1046
008400                     25 CL-H-IP-ID.                                   1048
008410                         30 CL-H-RC-CODE     PIC X(3).                1048
008420                         30 FILLER           PIC X(4).                1051
008430                     25 CL-H-CE-IP-ID-G REDEFINES CL-H-IP-ID.         1048
008440                         30 CL-H-CE-IP-ID    PIC X(05).               1048
008450                         30 CL-H-CE-IP-SFX   PIC X(02).               1053
008460                     25 CL-H-REL-ID          PIC X(6).                1055
008470                     25 CL-H-ID-IDX          PIC 9.                   1061
008480                     25 CL-H-LE-IDX          PIC 9.                   1062
008490                     25 CL-H-PAN-IDX         PIC 9.                   1063
008500*        SKIP1
008510                     25 CL-H-PAYEE           PIC X.                   1064
008520                         88 CL-H-PAY-MEM     VALUE 'M' ' ' 'N'.
008530                         88 CL-H-PAY-ALT-MEM VALUE 'N'.
008540                         88 CL-H-PAY-PROV    VALUE 'P' 'Q'.
008550                         88 CL-H-ALT-PAYEE   VALUE 'A'.
008560                         88 CL-H-PAY-ALT-PROV VALUE 'Q'.
008570*        SKIP1
008580                     25 CL-H-OR-CODE         PIC XXX.                 1065
008590*****                EACH BIT IS USED AS OVERRIDE IND     *****
008600                     25 CL-H-OR-IND          PIC XXX.                 1068
008610*        SKIP1
008620                     25 CL-H-OR-X-AMT        PIC S9(5)V99 COMP-3.     1071
008630                     25 CL-H-OR-P-AMT        PIC S9(5)V99 COMP-3.     1075
008640*        SKIP1
008650                     25 CL-H-ALT-RULE-IND    PIC X.                   1079
008660                         88 CL-H-ALT-RULE-SSO VALUE 'S' 'B'.
008670                         88 CL-H-ALT-RULE-OPS VALUE 'O' 'B'.
008680                         88 CL-H-ALT-RULE-SSO-OPS VALUE 'B'.
008690*        SKIP2
008700                     25 CL-H-PF-IND          PIC X.                   1080
008710                         88 CL-H-PF-USED     VALUE 'P'.
008720                         88 CL-H-EXT-PRICE   VALUE 'O'.
008730                         88 CL-H-UP-PRICE    VALUE 'E'.
008740*        SKIP1
008750                     25 CL-H-CAP-IND         PIC X.                   1081
008760                         88 CL-H-CAPITATED   VALUE 'C'.
008770*        SKIP1
008780                     25 CL-H-CHG             PIC S9(5)V99 COMP-3.     1082
008790                     25 CL-H-CONSIDERED-CHG  PIC S9(5)V99 COMP-3.     1086
008800                     25 CL-H-CTR             PIC S9(3)    COMP-3.     1090
008810                     25 CL-H-RATE            PIC S9(5)V99 COMP-3.     1092
008820                     25 CL-H-DISALLOW        PIC S9(5)V99 COMP-3.     1096
008830                     25 CL-H-UE-DISALLOW     PIC S9(5)V99 COMP-3.     1100
008840                     25 CL-H-UE-DISALLOW-EXPL PIC X(03).              1104
008850                     25 CL-H-UE-AMT1         PIC S9(5)V99 COMP-3.     1107
008860                     25 CL-H-UE-AMT1-EXPL    PIC X(03).               1111
008870                     25 CL-H-ADJ-AMT         PIC S9(5)V99 COMP-3.     1114
008880                     25 CL-H-COB-OTHER REDEFINES                      1114
008890                         CL-H-ADJ-AMT        PIC S9(5)V99 COMP-3.
008900                     25 CL-H-ADJ-IND         PIC X.                   1118
008910                     25 CL-H-ADJ-EXPL        PIC X(03).               1119
008920                     25 CL-H-COB-IND-G REDEFINES                      1119
008930                         CL-H-ADJ-EXPL.
008940                         30 CL-H-COB-OTHER-IND  PIC X.                1119
008950                         30 CL-H-COB-ALLOW-MAX-IND PIC X.             1120
008960                         30 CL-H-COB-DISALLOW-IND PIC X.              1121
008970                     25 CL-H-CHK-SEG-AMT     PIC S9(5)V99 COMP-3.     1122
008980                     25 CL-H-COB-ALLOW-MAX REDEFINES                  1122
008990                         CL-H-CHK-SEG-AMT    PIC S9(5)V99 COMP-3.
                       20  CL-H-ITEM2.
009000                  25  CL-H-B-MM          OCCURS  2  TIMES.            1126
009010                     30  CL-H-ALLOW          PIC S9(5)V99 COMP-3.     1126
009020                     30  CL-H-CTR-ALLOW      PIC S9(3)    COMP-3.     1130
009030                     30  CL-H-DEDUCT         PIC S9(5)V99 COMP-3.     1132
009040                     30  CL-H-SE-OPT         PIC X.                   1136
009050*****                    ONLY FOR BASIC TIER                *****
009060                         88  CL-H-EXC-BASIC-FROM-CHK VALUE 'B'.
009070*****                    ONLY FOR MAJOR TIER                *****
009080                         88  CL-H-BASIC-CARVEOUT     VALUE 'B'.
009090                         88  CL-H-EBP                VALUE 'E'.
009100                     30  CL-H-DED-T          PIC S9       COMP-3.     1137
009110                         88  CL-H-DED-PERCAUSE VALUE +9.
009120                     30  CL-H-COPAY          PIC S9(5)V99 COMP-3.     1138
009130                     30  CL-H-COB-ADJ        PIC S9(5)V99 COMP-3.     1142
009140                     30  CL-H-PAID           PIC S9(5)V99 COMP-3.     1146
                       20  CL-H-ITEM3.
009150                     25 CL-H-REF-LINE-PTR    PIC 9.                   1174
009160                     25 CL-H-AUTH-LINE-PTR REDEFINES                  1174
009170                         CL-H-REF-LINE-PTR   PIC 9.
009180                     25 CL-H-PROC-OPTION8 OCCURS 2 TIMES.             1175
009190                         30 CL-H-PROC-OPT8   PIC X(01).               1175
009200                     25 CL-H-CE-ACT-IND      PIC X(01).               1177
009260                     25 CL-H-CE-REAS-IND     PIC X(02).               1178
009280                     25 CL-H-CE-LINE-PTR     PIC X(01).               1180
009330                     25 CL-H-COB-DISALLOW    PIC S9(5)V99 COMP-3.     1181
009340                     25  FILLER                  PIC X(01).           1185
      *        10 FILLER                               PIC X(98).
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
IK0924         10  CL-DEN-MISC-NON-FIXED-OTHER.
IK0924             15  CL-DEN-USER-FIELD-G.
IK0924                 20  CL-DEN-USER-FIELD     PIC X(10) OCCURS 3.
010380*        10  FILLER                        PIC X(100).                1069
010390*****************************************************
010400*    D I S A B I L I T Y    O N L Y    F I E L D S  *
010410*****************************************************
010420     05  CL-DIS-NON-FIXED-DATA                                         901
010430         REDEFINES CL-NON-FIXED-DATA.
             07 CL-DIS-CLAIM.
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
011110                 20  CL-DIS-PAY-FILL-DATE-1  PIC 9(8).                 957
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
011650             15  CL-DIS-ACD-DT             PIC 9(8).                  1062
011700             15  CL-DIS-LDW-DT             PIC 9(8).                  1068
011750             15  CL-DIS-IPV-DT             PIC 9(8).                  1074
011800             15  CL-DIS-LPV-DT             PIC 9(8).                  1080
011850             15  CL-DIS-RTW-DT             PIC 9(8).                  1086
011900             15  CL-DIS-CONT-FROM-DT       PIC 9(8).                  1092
011960             15  CL-DIS-DATES-FIL1         PIC 9(8).                  1098
011970         10  CL-DIS-LE-G.                                             1104
011980             15  CL-DIS-LE-DATA  OCCURS  2  TIMES.                    1104
011990                 20  CL-DIS-LE-DT          PIC 9(8).                  1104
012000                 20  CL-DIS-LE-DT-X REDEFINES CL-DIS-LE-DT.           1104
012010                     25  CL-DIS-LE-CC      PIC 99.                    1104
012010                     25  CL-DIS-LE-YY      PIC 99.                    1104
012020                     25  CL-DIS-LE-MM      PIC 99.                    1106
012030                     25  CL-DIS-LE-DD      PIC 99.                    1108
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
012470             15  CL-DIS-OR-DT1             PIC 9(08).                 1166
012520             15  CL-DIS-OR-DT2             PIC 9(08).                 1172
012570         10  CL-DIS-FICA-PARM-DATA.                                   1178
012580*****        CONTAINS 2 YEARS FROM DIS-FROM DATE       *****
012590             15  FILLER                    PIC X(04).                 1178
012600             15  CL-DIS-FICA-PARM     OCCURS 2  TIMES.                1182
012610                 20  CL-DIS-FICA-YY-X.                                1182
012620                     25  CL-DIS-FICA-YY    PIC 9(4).                  1182
012630                 20  CL-DIS-FICA-MAX-ALLOW PIC S9(7)      COMP-3.     1184
012640                 20  CL-DIS-FICA-PCT       PIC S99V999    COMP-3.     1188
012650                 20  CL-DIS-FICA-PCT-F REDEFINES                      1188
012660                     CL-DIS-FICA-PCT       PIC SV9(5)     COMP-3.
012670*    SKIP2
012680         10  CL-DIS-CURR-PYMT-LINE.                                   1200
012690*****    ACCUMULATED ON ZERO SEGMENT                   *****
012700*****    IT IS ADJUSTED ON-ZERO FOR NON ZERO RECALL    *****
012710*****        RUN  DATE        *****
012720             15  CL-DIS-PER-RD-DT          PIC 9(8).                  1200
012780             15  CL-DIS-PER-BASE           PIC X.                     1206
012820             15  CL-DIS-PERIOD             PIC S999V9(8)  COMP-3.     1207
012830             15  CL-DIS-PERIOD-F        REDEFINES                     1207
012840                 CL-DIS-PERIOD             PIC S9(11) COMP-3.
012850             15  CL-DIS-PER-FROM-DT        PIC 9(8).                  1213
012910*            15  CL-DIS-PER-TO-DT-CC       PIC 99.                    1219
012920             15  CL-DIS-PER-TO-DT          PIC 9(8).                  1221
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
013320                 20  CL-DIS-YTD-YY-X.                                 1364
013330                     25  CL-DIS-YTD-YY     PIC 9(4).                  1364
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
013650             15  CL-DIS-BEN-FROM-DT        PIC 9(8).                  1480
013710*            15  CL-DIS-BEN-TO-DT-CC       PIC 99.                    1486
013720             15  CL-DIS-BEN-TO-DT          PIC 9(8).                  1488
013770             15  CL-DIS-BEN-TO-DT-MAX-CC   PIC 99.                    1494
013780             15  CL-DIS-BEN-TO-DT-MAX      PIC 9(6).                  1496
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
013950       07 CL-DIS-ITEM.
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
014120                 20  CL-B-FROM-DT          PIC 9(8).                  1563
014180*                20  CL-B-TO-DT-CC         PIC 99.                    1569
014190                 20  CL-B-TO-DT            PIC 9(8).                  1571
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
014570         10  FILLER                        PIC X(402).                4161
014580*    SKIP1
014590     05  CL-MED-FILL-AREA-G     REDEFINES                             4161
014600         CL-FILL-AREA-G.
014610         10  FILLER                        PIC X(402).                4161
014620*    SKIP2
014630     05  CL-DEN-FILL-AREA-G     REDEFINES                             4161
014640         CL-FILL-AREA-G.
014650         10  FILLER                        PIC X(402).                4161
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
014990             15  CL-DIS-CONV-DT                 PIC 9(8).             4236
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
015400*        10  FILLER                           PIC X(2).               4230
015410     05  CL-CUST-AREA-G.                                              4561
015420         10  FILLER                        PIC X(100).                4561
ECONO      05  CL-EOB-AMT-9-12-AREA-G  REDEFINES
               CL-CUST-AREA-G.
               10  CL-EOB-AMT-9-12.
                   15  CL-EOB-AMT-9              PIC S9(7)V99 COMP-3.
                   15  CL-EOB-AMT-10             PIC S9(7)V99 COMP-3.
                   15  CL-EOB-AMT-11             PIC S9(7)V99 COMP-3.
                   15  CL-EOB-AMT-12             PIC S9(7)V99 COMP-3.
               10  FILLER                        PIC X(80).
015430**********************************************************
015440*            END OF ***  C F E R R C L 0 ***             *
015450**********************************************************
001350     05  EC-FIXED-DATA.                                               4661
001360         10  EC-AP-DATA.                                              4661
001370             15  EC-AP-NAME                    PIC X(30).             4661
001380             15  EC-AP-ADDR1                   PIC X(30).             4691
001390             15  EC-AP-ADDR2                   PIC X(30).             4721
001400             15  EC-AP-ADDR3                   PIC X(30).             4751
001410             15  EC-AP-ADDR4.                                         4781
001420                 20  EC-AP-CITY                PIC X(19).             4781
001430                 20  EC-AP-STATE               PIC X(02).             4800
001440                 20  EC-AP-ZIP                 PIC X(09).             4802
001450         10  EC-LE-DATA                      OCCURS  2 TIMES.         4811
001460             15  EC-LE-DESC                  OCCURS  3 TIMES          4811
001470                                               PIC X(16).
001480             15  EC-LE-ADDRESSEE.                                     4859
001490                 20  EC-LE-ADDRESSEE-1         PIC X(01).             4859
001500                 20  EC-LE-ADDRESSEE-2         PIC X(01).             4860
001510                 20  EC-LE-ADDRESSEE-3         PIC X(01).             4861
001520             15  EC-LE-FOLLOW-UP1              PIC S9(3)   COMP-3.    4862
001530             15  EC-LE-FOLLOW-UP2              PIC S9(3)   COMP-3.    4864
001540             15  EC-LE-FINAL-DISP-DAYS         PIC S9(3)   COMP-3.    4866
001550             15  EC-LE-FINAL-DISP              PIC X(02).             4868
001560         10  EC-ME-DATA.                                              4929
001570             15  EC-ME-EMP-ID                  PIC X(10).             4929
001580             15  EC-ME-SEX                     PIC X(01).             4939
001590             15  EC-ME-BIRTH-DT                PIC 9(08).             4940
001670             15  EC-ME-NAME                    PIC X(30).             4948
001680             15  EC-ME-ADDRESS.                                       4978
001690                 20  EC-ME-ADDR1               PIC X(30).             4978
001700                 20  EC-ME-ADDR2               PIC X(30).             5008
001710                 20  EC-ME-ADDR3               PIC X(30).             5038
001720                 20  EC-ME-ADDR4.                                     5068
001730                     25  EC-ME-CITY            PIC X(19).             5068
001740                     25  EC-ME-STATE           PIC X(02).             5087
001750                     25  EC-ME-ZIP             PIC X(09).             5089
001760             15  EC-ME-GF-ME-ID                PIC X(09).             5098
001770             15  EC-ME-GF-GROUP                PIC X(08).             5107
001780             15  EC-ME-GF-DIV                  PIC X(04).             5115
001790             15  EC-ME-GEN-ID                  PIC X(12).             5119
001800             15  EC-ME-OCC-CODE                PIC X(02).             5131
001810             15  EC-ME-USER-FIELD-G.                                  5133
001820                 20  EC-ME-USER-FIELD        OCCURS  3 TIMES          5133
001830                                               PIC X(10).
001840             15  EC-ME-KEY-CHG-CD              PIC X(01).             5163
                       88 EC-ME-KC-KEY-CHG             VALUE '0'.
                       88 EC-ME-KC-REGULAR             VALUE '1'.
                       88 EC-ME-KC-PENDED              VALUE ' '.
001840             15  FILLER                        PIC X(09).             5163
001850         10  EC-PA-DATA.                                              5173
001860             15  EC-PA-NAME                    PIC X(30).             5173
001870             15  EC-PA-SEX                     PIC X(01).             5203
001880                 88  EC-PA-MALE                  VALUE 'M'.
001890                 88  EC-PA-FEMALE                VALUE 'F'.
001900             15  EC-PA-BIRTH-DT                PIC 9(08).             5204
001980             15  EC-PA-SSN                     PIC X(09).             5212
001990             15  EC-PA-USER-FIELD-G.                                  5221
002000                 20  EC-PA-USER-FIELD        OCCURS  3 TIMES          5221
002010                                               PIC X(10).
002020             15  EC-PA-COB-DATA-G.                                    5251
002030                 20  EC-PA-COB-DATA          OCCURS  4 TIMES.         5251
002040                     25  EC-PA-COB-CODE        PIC X(02).             5251
002050                         88  EC-PA-COB-OCCUR1-VALID  VALUE '  '
002060                                                      'CP' 'CS'
002070                                                      'C?' 'MP'
002080                                                      'MS' 'M?'.
002090                         88  EC-PA-COB-OCCUR2-VALID  VALUE '  '
002100                                                 'DP' 'DS' 'D?'.
002110                         88  EC-PA-COB-PRIMARY       VALUE 'CP'
002120                                                           'DP'.
002130                         88  EC-PA-MEDICARE-PRIMARY  VALUE 'MP'.
002140                         88  EC-PA-COB-UNKNOWN       VALUE 'C?'
002150                                                           'D?'.
002160                         88  EC-PA-MEDICARE-UNKNOWN  VALUE 'M?'.
002170                     25  EC-PA-COB-CARRIER-ID  PIC X(09).             5253
002180                     25  EC-PA-COB-GR-POL      PIC X(10).             5262
002190                     25  FILLER                PIC X(04).             5272
002200                     25  EC-PA-COB-EFF-DT      PIC 9(08).             5276
002260                     25  EC-PA-COB-TERM-DT     PIC 9(08).             5282
002320                     25  EC-PA-COB-LAST-INV-DT PIC 9(08).             5288
002380             15  FILLER                        PIC X(10).             5423
002390         10  EC-PLAN-DATA.                                            5433
002400             15  EC-AI-GF-EOB-AMT-PTR          PIC S9(04) COMP.       5433
002410             15  EC-PI-POLICY-NO               PIC X(10).             5435
002420             15  EC-PI-NAME                    PIC X(30).             5445
002430             15  EC-PI-USER-FIELD-G.                                  5475
002440                 20  EC-PI-USER-FIELD        OCCURS  3 TIMES          5475
002450                                               PIC X(10).
002460             15  EC-PO-USER-FIELD-G.                                  5505
002470                 20  EC-PO-USER-FIELD        OCCURS  3 TIMES          5505
002480                                               PIC X(10).
002490             15  EC-GR-USER-FIELD-G.                                  5535
002500                 20  EC-GR-USER-FIELD        OCCURS 3 TIMES           5535
002510                                               PIC X(10).
002520******  THE FOLLOWING FIELDS REPRESENT THE EXPANDED
002530******     BITS OF THE "CLAIM-LEVEL" OVERRIDE IND
002540*    SKIP2
002550         10  EC-CL-OR-DATA.                                           5565
002560             15  EC-CL-STORED-IND-G.                                  5565
002570                 20  EC-CL-BEL-IND             PIC X.                 5565
002580                     88  EC-CL-BEL                 VALUE 'B'.
002590                 20  EC-CL-PLAN-IND            PIC X.                 5566
002600                     88  EC-CL-PLAN                VALUE 'E'.
002610                 20  EC-CL-COV-IND             PIC X.                 5567
002620                     88  EC-CL-COV                 VALUE 'F'.
002630                 20  EC-CL-LMT-IND             PIC X.                 5568
002640                     88  EC-CL-LMT                 VALUE 'L'.
002650                 20  EC-CL-PAID-IND            PIC X.                 5569
002660                     88  EC-CL-PAID                VALUE 'P'.
002670                 20  EC-CL-READ-IND            PIC X.                 5570
002680                     88  EC-CL-READ                VALUE 'R'.
002690                 20  EC-CL-SITE-IND            PIC X.                 5571
002700                     88  EC-CL-SITE                VALUE 'S'.
002710                 20  EC-CL-TYPE-IND            PIC X.                 5572
002720                     88  EC-CL-TYPE                VALUE 'T'.
002730                 20  EC-CL-PY-RELEASE-IND      PIC X.                 5573
002740                     88  EC-CL-PY-RELEASE          VALUE 'Y'.
002750                 20  EC-CL-DISC-PR-IND         PIC X.                 5574
002760                     88  EC-CL-DISC-PR             VALUE 'J'.
002770                 20  EC-CL-DISC-PR-BYP-IND     PIC X.                 5575
002780                     88  EC-CL-DISC-PR-BYP         VALUE 'K'.
002790                 20  EC-CL-CS-ID-MATCH-IND     PIC X.                 5576
002800                     88  EC-CL-CS-ID-MATCH         VALUE 'C'.
002810                 20  EC-CL-CS-ID-NO-MATCH-IND  PIC X.                 5577
002820                     88  EC-CL-CS-ID-NO-MATCH      VALUE 'V'.
002830                 20  EC-CL-SUP-ACD-DIS-IND     PIC X.                 5578
002840                     88  EC-CL-SUP-ACD-DIS         VALUE 'G'.
002850                 20  EC-CL-COV-HP-PO-IND       PIC X.                 5579
002860                     88  EC-CL-COV-HP-PO           VALUE 'M'.
002870                 20  EC-CL-COB-AMT-IND         PIC X.                 5580
002880                     88  EC-CL-COB-AMT             VALUE 'H'.
002890                 20  EC-CL-OVERRIDE-REF-ID-IND PIC X.                 5581
002900                     88  EC-OVERRIDE-REF-ID        VALUE 'Z'.
002910                 20  EC-CL-VOID-REFERRAL-IND   PIC X.                 5582
002920                     88  EC-CL-VOID-REFERRAL       VALUE 'X'.
002930                 20  FILLER                    PIC X(06).             5583
002940*    SKIP1
002950         10  EC-CL-L-MAX                       PIC S9(3)   COMP-3.    5589
002960         10  EC-TF-DATA.                                              5591
002970             15  EC-TF-PR-SPECIALTY            PIC X(04).             5591
002980             15  EC-TF-PR-USER-FIELD           PIC X(10).             5595
002990         10  FILLER                            PIC X(96).             5605
003000     05  EC-NON-FIXED-DATA                     PIC X(9636).           5701
003010     05  EC-MED-NON-FIXED-DATA              REDEFINES                 5701
003020                                             EC-NON-FIXED-DATA.
003030         10  EC-MED-MISC-NON-FIXED.                                   5701
003040             15  EC-MED-ADMIT-PR-NAME          PIC X(30).             5701
003050             15  EC-MED-ID-DESC-G            OCCURS  5 TIMES.         5731
003060                 20  EC-MED-ID-DESC            PIC X(70).             5731
003070             15  FILLER                        PIC X(220).            6081
003080*    SKIP1
003090         10  EC-H-ITEM                       OCCURS  18  TIMES.       6301
003100             15  EC-H-IP-DESC                  PIC X(40).             6301
003110             15  EC-H-PR-STATUS                PIC X(02).             6341
003120             15  EC-H-PR-PO-ID                 PIC X(11).             6343
003130             15  EC-H-PR-TYPE                  PIC X(04).             6354
003140             15  EC-H-PR-SPECIALTY-G.                                 6358
003150                 20  EC-H-PR-SPECIALTY       OCCURS  4  TIMES         6358
003160                                               PIC X(04).
003170             15  EC-H-PR-SSN                   PIC X(09).             6374
003180             15  EC-H-PR-TAX-ID                PIC X(09).             6383
003190             15  EC-H-PR-NAME                  PIC X(30).             6392
003200             15  EC-H-PR-ADDR1                 PIC X(30).             6422
003210             15  EC-H-PR-ADDR2                 PIC X(30).             6452
003220             15  EC-H-PR-ADDR3                 PIC X(30).             6482
003230             15  EC-H-PR-ADDR4.                                       6512
003240                 20  EC-H-PR-CITY              PIC X(19).             6512
003250                 20  EC-H-PR-STATE             PIC X(02).             6531
003260                 20  EC-H-PR-ZIP.                                     6533
003270                     25  EC-H-PR-ZIP13.                               6533
003280                         30  EC-H-PR-ZIP13-9   PIC 9(03).             6533
003290                     25  EC-H-PR-ZIP49         PIC X(06).             6536
003300             15  EC-H-PR-USER-FIELD-G.                                6542
003310                 20  EC-H-PR-USER-FIELD        OCCURS  3  TIMES       6542
003320                                               PIC X(10).
003330             15  EC-H-PR-UPD-FIELDS.                                  6572
003340                 20  EC-H-PR-LAST-UPD-US-ID    PIC X(10).             6572
003350                 20  EC-H-PR-LAST-UPD-DT       PIC X(08).             6582
003410             15  EC-H-PR-GEN-ID                PIC X(12).             6588
003420             15  EC-H-SE-GL-ACCT-NO            PIC X(12).             6600
003430             15  EC-H-SE-GF-PLAN               PIC X(06).             6612
003440             15  EC-H-SE-GF-BEN-ID             PIC X(02).             6618
003450             15  FILLER                        PIC X(08).             6620
003460             15  EC-H-SE-DESC                  PIC X(40).             6628
003470             15  EC-H-EXPL                     PIC X(40).             6668
003480             15  EC-H-OR-EXPL                  PIC X(40).             6708
003490*    SKIP1
003500******  THE FOLLOWING FIELDS REPRESENT THE EXPANDED
003510******     BITS OF THE "LINE-LEVEL" OVERIDE IND
003520*    SKIP1
003530             15  EC-H-STORED-IND-G.                                   6748
003540                 20  EC-H-ALLOW-IND            PIC X.                 6748
003550                     88  EC-H-ALLOW              VALUE 'A'.
003560                 20  EC-H-BASIC-IND            PIC X.                 6749
003570                     88  EC-H-BASIC              VALUE 'B'.
003580                 20  EC-H-M-COP-IND            PIC X.                 6750
003590                     88  EC-H-M-COP              VALUE 'C'.
003600                 20  EC-H-M-DED-IND            PIC X.                 6751
003610                     88  EC-H-M-DED              VALUE 'D'.
003620                 20  EC-H-ELIG-IND             PIC X.                 6752
003630                     88  EC-H-ELIG               VALUE 'E'.
003640                 20  EC-H-CG-RULE-IND          PIC X.                 6753
003650                     88  EC-H-CG-RULE            VALUE 'G'.
003660                 20  EC-H-MAJOR-IND            PIC X.                 6754
003670                     88  EC-H-MAJOR              VALUE 'M'.
003680                 20  EC-H-PAID-IND             PIC X.                 6755
003690                     88  EC-H-PAID               VALUE 'P'.
003700                 20  EC-H-SE-DP-RULE-IND       PIC X.                 6756
003710                     88  EC-H-SE-DP-RULE         VALUE 'S'.
003720                 20  EC-H-REL-IND              PIC X.                 6757
003730                     88  EC-H-REL                VALUE 'W'.
003740                 20  EC-H-EXCL-IND             PIC X.                 6758
003750                     88  EC-H-EXCL               VALUE 'X'.
003760                 20  EC-H-B-COP-IND            PIC X.                 6759
003770                     88  EC-H-B-COP              VALUE 'Y'.
003780                 20  EC-H-B-DED-IND            PIC X.                 6760
003790                     88  EC-H-B-DED              VALUE 'Z'.
003800                 20  FILLER                    PIC X(11).             6761
003810             15  EC-H-IP-USER-FIELD            PIC X(10).             6772
003820             15  FILLER                        PIC X(19).             6782
003830*    EJECT
003840     05  EC-DEN-NON-FIXED-DATA             REDEFINES                  5701
003850                                             EC-NON-FIXED-DATA.
003860         10  EC-DEN-MISC-NON-FIXED.                                   5701
003870             15  EC-DEN-PR-STATUS              PIC X(02).             5701
003880             15  EC-DEN-PR-PO-ID               PIC X(11).             5703
003890             15  EC-DEN-PR-TYPE                PIC X(04).             5714
003900             15  EC-DEN-PR-SPECIALTY-G.                               5718
003910                 20  EC-DEN-PR-SPECIALTY     OCCURS  4  TIMES         5718
003920                                               PIC X(04).
003930             15  EC-DEN-PR-GEN-ID              PIC X(12).             5734
003940             15  EC-DEN-PR-SSN                 PIC X(09).             5746
003950             15  EC-DEN-PR-TAX-ID              PIC X(09).             5755
003960             15  EC-DEN-PR-NAME                PIC X(30).             5764
003970             15  EC-DEN-PR-ADDR1               PIC X(30).             5794
003980             15  EC-DEN-PR-ADDR2               PIC X(30).             5824
003990             15  EC-DEN-PR-ADDR3               PIC X(30).             5854
004000             15  EC-DEN-PR-ADDR4.                                     5884
004010                 20  EC-DEN-PR-CITY            PIC X(19).             5884
004020                 20  EC-DEN-PR-STATE           PIC X(02).             5903
004030                 20  EC-DEN-PR-ZIP.                                   5905
004040                     25  EC-DEN-PR-ZIP13.                             5905
004050                         30  EC-DEN-PR-ZIP13-9 PIC 9(03).             5905
004060                     25  EC-DEN-PR-ZIP49       PIC X(06).             5908
004070             15  EC-DEN-PR-USER-FIELD-G.                              5914
004080                 20  EC-DEN-PR-USER-FIELD      OCCURS  3  TIMES       5914
004090                                               PIC X(10).
004100             15  EC-DEN-PR-UPD-FIELDS.                                5944
004110                 20  EC-DEN-PR-LAST-UPD-US-ID  PIC X(10).             5944
004120                 20  EC-DEN-PR-LAST-UPD-DT     PIC X(08).             5954
004180*    SKIP1
004190             15  FILLER                        PIC X(221).            5960
004200*    SKIP1
004210         10  EC-D-ITEM                       OCCURS  8 TIMES.         6181
004220             15  EC-D-CG-DESC                  PIC X(40).             6181
004230             15  EC-D-DP-GL-ACCT-NO            PIC X(12).             6221
004240             15  EC-D-DP-GF-PLAN               PIC X(06).             6233
004250             15  EC-D-DP-GF-BEN-ID             PIC X(02).             6239
004260             15  EC-D-DP-DESC                  PIC X(40).             6241
004270             15  EC-D-EXPL                     PIC X(40).             6281
004280             15  EC-D-OR-EXPL                  PIC X(40).             6321
004290*    EJECT
004300******  THE FOLLOWING FIELDS REPRESENT THE EXPANDED
004310******     BITS OF THE "LINE-LEVEL" OVERIDE IND
004320*    SKIP1
004330             15  EC-D-STORED-IND-G.                                   6361
004340                 20  EC-D-ALLOW-IND            PIC X.                 6361
004350                     88  EC-D-ALLOW              VALUE 'A'.
004360                 20  EC-D-BASIC-IND            PIC X.                 6362
004370                     88  EC-D-BASIC              VALUE 'B'.
004380                 20  EC-D-M-COP-IND            PIC X.                 6363
004390                     88  EC-D-M-COP              VALUE 'C'.
004400                 20  EC-D-M-DED-IND            PIC X.                 6364
004410                     88  EC-D-M-DED              VALUE 'D'.
004420                 20  EC-D-ELIG-IND             PIC X.                 6365
004430                     88  EC-D-ELIG               VALUE 'E'.
004440                 20  EC-D-CG-RULE-IND          PIC X.                 6366
004450                     88  EC-D-CG-RULE            VALUE 'G'.
004460                 20  EC-D-MAJOR-IND            PIC X.                 6367
004470                     88  EC-D-MAJOR              VALUE 'M'.
004480                 20  EC-D-PAID-IND             PIC X.                 6368
004490                     88  EC-D-PAID               VALUE 'P'.
004500                 20  EC-D-SE-DP-RULE-IND       PIC X.                 6369
004510                     88  EC-D-SE-DP-RULE         VALUE 'S'.
004520                 20  EC-D-REL-IND              PIC X.                 6370
004530                     88  EC-D-REL                VALUE 'W'.
004540                 20  EC-D-EXCL-IND             PIC X.                 6371
004550                     88  EC-D-EXCL               VALUE 'X'.
004560                 20  EC-D-B-COP-IND            PIC X.                 6372
004570                     88  EC-D-B-COP              VALUE 'Y'.
004580                 20  EC-D-B-DED-IND            PIC X.                 6373
004590                     88  EC-D-B-DED              VALUE 'Z'.
004600                 20  FILLER                    PIC X(11).             6374
004610*    SKIP1
004620             15  FILLER                        PIC X(36).             6385
               10 FILLER                             PIC X(34).

004630*    EJECT
004640     05  EC-DISB-NON-FIXED-DATA             REDEFINES                 5701
004650                                             EC-NON-FIXED-DATA.
004660         10  EC-DIS-DB-GL-ACCT-NO              PIC X(12).             5701
004670         10  EC-DIS-DB-CP-PLAN                 PIC X(06).             5713
004680         10  EC-DIS-DB-GF-PLAN                 PIC X(06).             5719
004690         10  EC-DIS-DB-GF-BEN-ID               PIC X(02).             5725
004700         10  EC-DIS-DD-FICA-NAME               PIC X(30).             5727
004710         10  EC-DIS-PER-DD-NAME              OCCURS  5 TIMES          5757
004720                                               PIC X(30).
004730         10  EC-DIS-PER-DO-NAME              OCCURS  6 TIMES          5907
004740                                               PIC X(30).
004750         10  EC-DIS-EX-EXPL                    PIC X(40).             6087
004760         10  EC-DIS-EX-BEN-EXPL                PIC X(40).             6127
004770         10  EC-DIS-ID-DESC                    PIC X(40).             6167
004780         10  EC-DIS-PR-STATUS                  PIC X(02).             6207
004790         10  EC-DIS-PR-PO-ID                   PIC X(11).             6209
004800         10  EC-DIS-PR-TYPE                    PIC X(04).             6220
004810         10  EC-DIS-PR-SPECIALTY-G.                                   6224
004820             15  EC-DIS-PR-SPECIALTY         OCCURS  4  TIMES         6224
004830                                               PIC X(04).
004840         10  EC-DIS-PR-SSN                     PIC X(09).             6240
004850         10  EC-DIS-PR-TAX-ID                  PIC X(09).             6249
004860         10  EC-DIS-PR-NAME                    PIC X(30).             6258
004870         10  EC-DIS-PR-ADDRESS.                                       6288
004880             15  EC-DIS-PR-ADDR1               PIC X(30).             6288
004890             15  EC-DIS-PR-ADDR2               PIC X(30).             6318
004900             15  EC-DIS-PR-ADDR3               PIC X(30).             6348
004910             15  EC-DIS-PR-ADDR4.                                     6378
004920                 20  EC-DIS-PR-CITY            PIC X(19).             6378
004930                 20  EC-DIS-PR-STATE           PIC X(02).             6397
004940                 20  EC-DIS-PR-ZIP.                                   6399
004950                     25  EC-DIS-PR-ZIP13.                             6399
004960                         30  EC-DIS-PR-ZIP13-9 PIC 9(03).             6399
004970                     25  EC-DIS-PR-ZIP49       PIC X(06).             6402
004980         10  EC-DIS-PR-USER-FIELD-G.                                  6408
004990             15  EC-DIS-PR-USER-FIELD        OCCURS  3  TIMES         6408
005000                                               PIC X(10).
005010         10  EC-DIS-PR-UPD-FIELDS.                                    6438
005020             15  EC-DIS-PR-LAST-UPD-US-ID      PIC X(10).             6438
005030             15  EC-DIS-PR-LAST-UPD-DT         PIC X(08).             6448
005090         10  EC-DIS-PR-GEN-ID                  PIC X(12).             6454
005100*    SKIP1
005110         10  FILLER                            PIC X(235).            6466
      *        10 FILLER                             PIC X(34).
005120*    EJECT
005130**********************************************************
005140*           ***** I M P O R T A N T *****                *
005150*                                                        *
005160*     IF YOU DEFINE FIELDS IN THIS FILLER AREA AT THE    *
005170*     END OF THIS RECORD, BE SURE TO INSERT THE CODE     *
005180*     TO INITIALIZE THE NEW FIELDS IN THE EXPANSION      *
005190*     ROUTINE IN THE COPYBOOK (CFERPECR). YOU MUST ALSO  *
005200*     CHANGE THE VALUE OF THE APPROPRIATE FILLER USED    *
005210*     FIELD (K-EC-XXX-FILL-USED) IN COPYBOOK (CFERKCL0). *
005220*                                                        *
005230**********************************************************
005240*    THE FOLLOWING FIELDS ADDED AFTER 4.0
005250     05  EC-FILL-AREA-G.                                             15301
005260         10  FILLER                        PIC X(600).               15301
005270     05  EC-CE-EXTERNAL-AREA-G REDEFINES                             15301
005280         EC-FILL-AREA-G.
005290         10  EC-CE-EXT-IP-TBL-E    OCCURS 6 TIMES.                   15301
005300             15  EC-CE-EXT-IP-DESC         PIC X(40).                15301
005310             15  EC-CE-EXT-SUGG-IP-DESC    PIC X(40).                15341
005320         10  FILLER                        PIC X(120).               15781
005330     05  EC-CUST-AREA-G.                                             15901
005340         10  FILLER                        PIC X(100).               15901
005350**********************************************************
005360*            END OF ***  C F E R R E C 0 ***             *
005370**********************************************************
