000010**********************************************************                
000020*    EXPANDED CLAIM RECORD      : CFERREC0               *                
000030*        KEY LENGTH             : K-EC-KEY-LENGTH (44)   *                
Y2K****        RECORD LENGTH          : K-EC-REC-LENGTH (16098)*                
000050*                                                        *                
000060*    CONTAINS CLAIM RELATED INFORMATION.                 *                
000070*                                                        *                
000080*        ** PROPRIETARY PROPERTY OF ERISCO, INC. **      *                
000090*        **            COPYRIGHT 1984            **      *                
000100**********************************************************                
000110*    SKIP2                                                                
000120**********************************************************                
000130*    -----    RELEASE  4.00  MODIFICATIONS    --------   *                
000140*                                                        *                
000150*    -  KEY LENGTH CHANGED FROM   40 TO 44               *                
000160*    -  REC LENGTH CHANGED FROM 5000 TO 16000            *                
000170*                                                        *                
000180*    -  NEW FIELDS:                                      *                
000190*            EC-PZ-ID                                    *                
000200*            EC-AP-ADDR4                                 *                
000210*            EC-ME-ADDR4                                 *                
000220*            EC-ME-OCC-CODE                              *                
000230*            EC-PO-USER-FIELD-G                          *                
000240*            EC-PO-USER-FIELD                            *                
000250*            EC-H-PR-ADDR4                               *                
000260*            EC-H-PR-LAST-UPD-US-ID                      *                
000270*            EC-DEN-PR-ADDR4                             *                
000280*            EC-DEN-PR-LAST-UPD-US-ID                    *                
000290*            EC-D-EXPL                                   *                
000300*            EC-D-OR-EXPL                                *                
000310*            EC-DIS-PR-ADDR4                             *                
000320*            EC-DIS-PR-LAST-UPD-US-ID                    *                
000330*                                                        *                
000340*    -  DELETED:                                         *                
000350*            EC-MED-IP-DESC-G                            *                
000360*            EC-MED-IP-DESC                              *                
000370*            EC-H-PR-UPD-ORIGIN                          *                
000380*            EC-H-PR-FIL-AMT1                            *                
000390*            EC-H-PR-FIL-AMT2                            *                
000400*            EC-DEN-PR-UPD-ORIGIN                        *                
000410*            EC-DEN-PR-FIL-AMT1                          *                
000420*            EC-DEN-PR-FIL-AMT2                          *                
000430*            EC-D-OR-CODE                                *                
000440*            EC-DIS-PR-UPD-ORIGIN                        *                
000450*            EC-DIS-PR-FIL-AMT1                          *                
000460*            EC-DIS-PR-FIL-AMT2                          *                
000470*            EC-DIS-PER-DO-NAME-4                        *                
000480*                                                        *                
000490**********************************************************                
000500*    EJECT                                                                
000510**********************************************************                
000520*                                                        *                
000530*    -  CHANGED:                                         *                
000540*            EC-KEY              FROM  40  TO  44  BYTES *                
000550*            EC-REMAINDER-CL-REC FROM 1360 TO 4616 BYTES *                
000560*            EC-ME-US-FIELD-G    TO   EC-ME-USER-FIELD-G *                
000570*            EC-ME-US-FIELD      TO   EC-ME-USER-FIELD   *                
000580*            EC-PA-US-FIELD-G    TO   EC-PA-USER-FIELD-G *                
000590*            EC-PA-US-FIELD      TO   EC-PA-USER-FIELD   *                
000600*            EC-PA-COB-DATA      FROM 2    TO 4    OCC   *                
000610*            EC-PI-US-FIELD-G    TO   EC-PI-USER-FIELD-G *                
000620*            EC-PI-US-FIELD      TO   EC-PI-USER-FIELD   *                
000630*            EC-GR-US-FIELD-G    TO   EC-GR-USER-FIELD-G *                
000640*            EC-GR-US-FIELD      TO   EC-GR-USER-FIELD   *                
000650*            EC-MED-CL-L-MAX     TO   EC-CL-L-MAX        *                
000660*            EC-NON-COMMON-DATA  TO   EC-NON-FIXED-DATA  *                
000670*            EC-MED-ID-DESC      FROM 3    TO 5    OCC   *                
000680*            EC-H-ITEM           FROM 6    TO 18   OCC   *                
000690*            EC-H-PR-SPECIALTY   FROM 1    TO 4    OCC   *                
000700*            EC-H-PR-US-FIELD    TO   EC-H-PR-USER-FIELD *                
000710*            EC-H-PR-US-FIELD    FROM 1    TO 3    OCC   *                
000720*            EC-H-PR-UPD-ORIGIN  TO                      *                
000730*            EC-H-PR-LAST-UPD-US-ID                      *                
000740*            EC-H-STORED-IND-G   FROM 16   TO 24   BYTES *                
000750*            EC-DEN-PR-SPECIALTY FROM 1    TO 4    OCC   *                
000760*            EC-DEN-PR-US-FIELD  TO EC-DEN-PR-USER-FIELD *                
000770*            EC-DEN-PR-US-FIELD  FROM 1    TO 3    OCC   *                
000780*            EC-DEN-PR-UPD-ORIGIN  TO                    *                
000790*            EC-DEN-PR-LAST-UPD-US-ID                    *                
000800*            EC-D-STORED-IND-G   FROM 16   TO 24   BYTES *                
000810*            EC-DIS-PER-DD-NAME  FROM 3    TO 5    OCC   *                
000820*            EC-DIS-PER-DO-NAME  FROM 3    TO 6    OCC   *                
000830*            EC-DIS-PR-SPECIALTY FROM 1    TO 4    OCC   *                
000840*            EC-DIS-PR-US-FIELD  TO EC-DIS-PR-USER-FIELD *                
000850*            EC-DIS-PR-US-FIELD  FROM 1    TO 3    OCC   *                
000860*            EC-DIS-PR-UPD-ORIGIN  TO                    *                
000870*            EC-DIS-PR-LAST-UPD-US-ID                    *                
000880*                                                        *                
000890*    -  CONVERSION NOTES:                                *                
000900*                                                        *                
000910**********************************************************                
000920*    SKIP2                                                                
000930**********************************************************                
000940*    ----   MODIFICATIONS AFTER RELEASE 4.00   -------   *                
000950*                                                        *                
000960*    REL    DATE   PGMR DESCRIPTION                      *                
000970*    4.30 10/05/89 BTM  ADDED EC-TF-PR-SPECIALTY,        *                
000980*                 EC-TF-PR-USER-FIELD, EC-H-IP-USER-FIELD*                
000990*                                                        *                
001000*    4.31 12/28/89 REP  ADDED EC-FILL-AREA-G             *                
001010*                             EC-CUST-AREA-G             *                
001020*                             EC-CE-EXTERNAL-AREA-G      *                
001030*                             EC-CE-EXT-IP-DESC          *                
001040*                             EC-CE-EXT-SUGG-IP-DESC     *                
001050*                       CHANGED THE LENGTH OF THE CLAIM  *                
001060*                               RECORD.                  *                
000990*                                                        *                
000990*         09/24/97 TL   Y2K THE FOLLOWING FIELDS WERE EX-*                
000990*                       PANDED ON 2 BYTES EACH BY EDITING*                
000990*                       -CC- SUBFIELDS:                  *                
000990*                       EC-PA-COB-EFF-DT                 *                
000990*                       EC-PA-COB-TERM-DT                *                
000990*                       EC-PA-LAST-INV-DT                *                
000990*                       EC-H-PR-LAST-UPD-DT              *                
000990*                       EC-DEN-PR-LAST-UPD-DT            *                
000990*                       EC-DIS-PR-LAST-UPD-DT.           *                
000990*                       THE FIELD EC-NON-FIXED-DATA WAS  *                
000990*                       CHANGED FROM 9600 TO 9636 BYTES. *                
000990*                       RECORD LENGTH WAS CHANGED FROM   *                
000990*                       16000 TO 16098 BYTES. THE FIELD  *                
000990*                       EC-REMAINDER-CL-REC WAS CHANGED  *                
000990*                       FROM 4616 TO 4654 BYTES.         *                
000990*                                                        *                
AK0700*         07/26/00 AK   ADD EC-ME-KEY-CHG-CD             *                
000990*                                                        *                
001070**********************************************************                
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
Y2K            10  EC-REMAINDER-CL-REC               PIC X(4654).             45
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
001600             15  EC-ME-BIRTH-DT-X            REDEFINES                4940
001610                                             EC-ME-BIRTH-DT.              
001620                 20  EC-ME-B-CC                PIC 9(02).             4940
001630                 20  EC-ME-DOB-YMD.                                   4942
001640                     25  EC-ME-B-YY            PIC 9(02).             4942
001650                     25  EC-ME-B-MM            PIC 9(02).             4944
001660                     25  EC-ME-B-DD            PIC 9(02).             4946
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
AK0700             15  EC-ME-KEY-CHG-CD              PIC X(01).             5163
AK0700                 88  EC-ME-KC-KEY-CHG              VALUE '0'.         5163
AK0700                 88  EC-ME-KC-REGULAR              VALUE '1'.         5163
AK0700                 88  EC-ME-KC-PENDED               VALUE ' '.         5163
AK0700             15  FILLER                        PIC X(09).             5163
001850         10  EC-PA-DATA.                                              5173
001860             15  EC-PA-NAME                    PIC X(30).             5173
001870             15  EC-PA-SEX                     PIC X(01).             5203
001880                 88  EC-PA-MALE                  VALUE 'M'.               
001890                 88  EC-PA-FEMALE                VALUE 'F'.               
001900             15  EC-PA-BIRTH-DT                PIC 9(08).             5204
001910             15  EC-PA-BIRTH-DT-X            REDEFINES                5204
001920                                             EC-PA-BIRTH-DT.              
001930                 20  EC-PA-B-CC                PIC 9(02).             5204
001940                 20  EC-PA-DOB-YMD.                                   5206
001950                     25  EC-PA-B-YY            PIC 9(02).             5206
001960                     25  EC-PA-B-MM            PIC 9(02).             5208
001970                     25  EC-PA-B-DD            PIC 9(02).             5210
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
Y2K  A                     25  EC-PA-COB-EFF-DT-CYMD  PIC 9(8).             5276
Y2K  A                     25  EC-PA-COB-EFF-DT-CYMD-X                      5276
Y2K  A                         REDEFINES EC-PA-COB-EFF-DT-CYMD.             5276
Y2K  A                       30  EC-PA-COB-EFF-DT-CCYY PIC 9(4).            5276
Y2K  A                       30  FILLER                PIC X(4).            5276
Y2K  A                     25  EC-PA-COB-EFF-DT-OLD                         5276
Y2K  A                         REDEFINES EC-PA-COB-EFF-DT-CYMD.             5276
Y2K  A                         30  EC-PA-COB-EFF-CC    PIC 9(02).           5276
Y2K                            30  EC-PA-COB-EFF-DT    PIC 9(06).           5276
Y2K                            30  EC-PA-COB-EFF-DT-X  REDEFINES            5276
Y2K                                EC-PA-COB-EFF-DT.                            
Y2K                                35  EC-PA-COB-E-YY  PIC 9(02).           5276
Y2K                                35  EC-PA-COB-E-MM  PIC 9(02).           5278
Y2K                                35  EC-PA-COB-E-DD  PIC 9(02).           5280
Y2K  A                     25  EC-PA-COB-TERM-DT-CYMD   PIC 9(8).           5282
Y2K  A                     25  EC-PA-COB-TERM-DT-CYMD-X                     5282
Y2K  A                         REDEFINES EC-PA-COB-TERM-DT-CYMD.            5282
Y2K  A                       30  EC-PA-COB-TERM-DT-CCYY PIC 9(4).           5282
Y2K  A                       30  FILLER                 PIC X(4).           5282
Y2K  A                     25  EC-PA-COB-TERM-DT-OLD                        5282
Y2K  A                         REDEFINES EC-PA-COB-TERM-DT-CYMD.            5282
Y2K  A                         30  EC-PA-COB-TERM-CC    PIC 99.             5282
Y2K                            30  EC-PA-COB-TERM-DT    PIC 9(06).          5282
Y2K                            30  EC-PA-COB-TERM-DT-X REDEFINES            5282
Y2K                                EC-PA-COB-TERM-DT.                           
Y2K                                35  EC-PA-COB-T-YY   PIC 9(02).          5282
Y2K                                35  EC-PA-COB-T-MM   PIC 9(02).          5284
Y2K                                35  EC-PA-COB-T-DD   PIC 9(02).          5286
Y2K                        25  EC-PA-COB-LAST-INV-DT-CYMD                   5288
Y2K                                                     PIC 9(08).          5288
Y2K  A                     25  EC-PA-COB-LAST-INV-DT-CYMD-X                 5288
Y2K  A                         REDEFINES EC-PA-COB-LAST-INV-DT-CYMD.        5288
Y2K  A                         30  EC-PA-COB-LAST-INV-DT-CCYY               5288
Y2K  A                                                  PIC 9(04).          5288
Y2K  A                         30  FILLER               PIC X(04).          5288
Y2K  A                     25  EC-PA-COB-LAST-INV-DT-OLD                    5288
Y2K  A                         REDEFINES EC-PA-COB-LAST-INV-DT-CYMD.        5288
Y2K  A                         30  EC-PA-COB-LAST-INV-CC                    5288
Y2K                                                    PIC 9(02).           5288
Y2K                            30  EC-PA-COB-LAST-INV-DT                    5288
Y2K                                                    PIC 9(06).           5288
Y2K                            30  EC-PA-COB-LAST-INV-DT-X REDEFINES        5288
Y2K                                EC-PA-COB-LAST-INV-DT.                       
Y2K                                35  EC-PA-COB-L-YY  PIC 9(02).           5288
Y2K                                35  EC-PA-COB-L-MM  PIC 9(02).           5290
Y2K                                35  EC-PA-COB-L-DD  PIC 9(02).           5292
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
Y2K        05  EC-NON-FIXED-DATA                     PIC X(9636).           5701
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
Y2K  A                 20  EC-H-PR-LAST-UPD-DT-CYMD  PIC 9(08).             6582
Y2K  A                 20  EC-H-PR-LAST-UPD-DT-CYMD-X                       6582
Y2K  A                     REDEFINES EC-H-PR-LAST-UPD-DT-CYMD.              6582
Y2K  A                   25  EC-H-PR-LAST-UPD-DT-CCYY PIC 9(4).             6582
Y2K  A                   25  FILLER                   PIC X(4).             6582
Y2K  A                 20  EC-H-PR-LAST-UPD-DT-OLD                          6582
Y2K  A                     REDEFINES EC-H-PR-LAST-UPD-DT-CYMD.              6582
Y2K  A                     25  EC-H-PR-LAST-UPD-CC   PIC X(02).             6582
Y2K                        25  EC-H-PR-LAST-UPD-DT   PIC X(06).             6582
Y2K                        25  EC-H-PR-LAST-UPD-DT-X                        6582
Y2K                            REDEFINES EC-H-PR-LAST-UPD-DT.                   
Y2K                            30  EC-H-PR-LU-YY     PIC 9(02).             6582
Y2K                            30  EC-H-PR-LU-MM     PIC 9(02).             6584
Y2K                            30  EC-H-PR-LU-DD     PIC 9(02).             6586
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
Y2K  A                 20  EC-DEN-PR-LAST-UPD-DT-CYMD                       5954
Y2K  A                                               PIC 9(08).             5954
Y2K  A                 20  EC-DEN-PR-LAST-UPD-DT-CYMD-X                     5954
Y2K  A                     REDEFINES EC-DEN-PR-LAST-UPD-DT-CYMD.            5954
Y2K  A                     25  EC-DEN-PR-LAST-UPD-DT-CCYY                   5954
Y2K  A                                               PIC 9(04).             5954
Y2K  A                     25  FILLER                PIC X(04).             5954
Y2K  A                 20  EC-DEN-PR-LAST-UPD-DT-OLD                        5954
Y2K  A                     REDEFINES EC-DEN-PR-LAST-UPD-DT-CYMD.            5954
Y2K  A                     25  EC-DEN-PR-LAST-UPD-CC PIC 9(02).             5954
Y2K                        25  EC-DEN-PR-LAST-UPD-DT PIC 9(06).             5954
Y2K                        25  EC-DEN-PR-LAST-UPD-DT-X                      5954
Y2K                            REDEFINES EC-DEN-PR-LAST-UPD-DT.                 
Y2K                            30  EC-DEN-PR-LU-YY   PIC 9(02).             5954
Y2K                            30  EC-DEN-PR-LU-MM   PIC 9(02).             5956
Y2K                            30  EC-DEN-PR-LU-DD   PIC 9(02).             5958
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
Y2K  A             15  EC-DIS-PR-LAST-UPD-DT-CYMD    PIC 9(08).             6448
Y2K  A             15  EC-DIS-PR-LAST-UPD-DT-CYMD-X                         6448
Y2K  A                 REDEFINES EC-DIS-PR-LAST-UPD-DT-CYMD.                6448
Y2K  A               20  EC-DIS-PR-LAST-UPD-DT-CCYY  PIC 9(04).             6448
Y2K  A               20  FILLER                      PIC X(04).             6448
Y2K  A             15  EC-DIS-PR-LAST-UPD-DT-OLD                            6448
Y2K  A                 REDEFINES EC-DIS-PR-LAST-UPD-DT-CYMD.                6448
Y2K  A                 20  EC-DIS-PR-LAST-UPD-CC     PIC 9(02).             6448
Y2K                    20  EC-DIS-PR-LAST-UPD-DT     PIC 9(06).             6448
Y2K                    20  EC-DIS-PR-LAST-UPD-DT-X                          6448
Y2K                        REDEFINES EC-DIS-PR-LAST-UPD-DT.                     
Y2K                        25  EC-DIS-PR-LU-YY       PIC 9(02).             6448
Y2K                        25  EC-DIS-PR-LU-MM       PIC 9(02).             6450
Y2K                        25  EC-DIS-PR-LU-DD       PIC 9(02).             6452
005090         10  EC-DIS-PR-GEN-ID                  PIC X(12).             6454
005100*    SKIP1                                                                
005110         10  FILLER                            PIC X(235).            6466
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
