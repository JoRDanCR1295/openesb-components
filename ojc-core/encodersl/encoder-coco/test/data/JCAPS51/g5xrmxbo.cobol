000010****************************************************************
000020*    GF MEMBER INDEX RECORD       :  G5XRMXB0                  *
000030*    KEY LENGTH                   :  K-MX-KEY-LENGTH (32)      *
000040*    RECORD LENGTH                :  K-MX-REC-LENGTH (550)     *
000050*                                                              *
000060*                                                              *
000070*   RECORD IS REQUIRED FOR EVERY MEMBER ON FILE. IT CONTAINS   *
000080*   POINTERS TO MEMBER RECORD SUB-TYPES,  BILLING DATA FOR     *
000090*   ASSOCIATIONS, STATUS AND SOME SIGNIFICANT DATES.           *
000100*                                                              *
000110*   ANY CHANGES MADE TO G5XRMXB0 SHOULD BE SYNCHRONIZED WITH   *
000120*   G5XRMXN0 ('NEW-' VERSION OF G5XRMXB0).                     *
000130*                                                              *
000140*                                                              *
000150****************************************************************
000160*    SKIP2
000170*    SKIP2
000180****************************************************************
000190*         **  PROPRIETARY PROPERTY OF ERISCO, INC.  **         *
000200*         **        COPYRIGHT 1984 - 1997           **         *
000210****************************************************************
000220*    SKIP2
000230****************************************************************
000240*       -----    RELEASE 6.0 MODIFICATION NOTES     -----      *
000250*                                                              *
000260*    -   REC LENGTH CHANGED FROM  600   TO   550               *
000270*                                                              *
000280*    -   CHANGED:                                              *
000290*        MX-UNUSED-FIL-1               FROM 115  TO 34   BYTES *
000300*                                                              *
000310*    -   CONVERSION NOTES:                                     *
000320*        ALL DATE FIELDS WERE EXPANDED TO INCLUDE CENTURY      *
000330*                                                              *
000340****************************************************************
000350*    SKIP2
000360****************************************************************
000370*       -----    MODIFICATIONS AFTER RELEASE 6.0    -----      *
000380****************************************************************
000390*                                                              *
000400*--->GF80825A   ADDED MX-TERM-IN-PROG-IND.                     *
000390*                                                              *
      *                                                              *
GLM#1 *--->GLM#1   CHANGES MADE 09/>>/1999 - R.SCHULMAN              *
GLM#1 *            MODIFIED MX-STS TO ADD 'U' AND 'R' AS VALID VALUES*
GLM#1 *            VALUES IN THE MX-STS-VALID LEVEL 88 FIELD         *
      *                                                              *
      *ADD -->GF19990713002 GF75                                     *
      *ADD -->     1. ADDED MX-EARLIEST-RATE-DT.                     *
      *                                                              *
000400*--->REF#001 RLSE 6.00  CHGS MADE 05/24/96 THRU 08/18/97       *
000410*    ENH483  GF09 SY03 SY07                                    *
000420*     1. UPDATED FOR RELEASE 6.0                               *
000430*                                                              *
000440****************************************************************
000450*    SKIP2
000460****************************************************************
000470*       -----          RELEASE STAMP                -----      *
000480*                                                              *
000490*     G5XRMXB0  REL.6.00  08/18/97  12:54:38                   *
000500****************************************************************
000880     05  I-OR-D                          PIC X(01).                      1
000880     05                                  PIC X(46).                      1
000510     05  MX-KEY.
000520         10  MX-PZ-ID                    PIC X(02).
000530             88  MX-GROUPFACTS             VALUE 'GF'.
000540         10  MX-CI-ID                    PIC X(02).
000550         10  MX-REC-ID                   PIC X(02).
000560             88  MX-MEMBER-INDEX-REC        VALUE 'MX'.
000570         10  MX-KEY1.
000580             15  MX-GR-DV-MM.
000590                 20  MX-GR-ID.
000600                     25  MX-GR-ID-PFX    PIC X(02).
000610                     25  MX-GR-ID-SFX    PIC X(06).
000620                 20  MX-DV-ID            PIC X(04).
000630                 20  MX-MM-ID            PIC X(09).
000640             15  MX-KEY1-FIL             PIC X(01).
000650         10  MX-KEY-FIL                  PIC X(04).
000660*    SKIP1
000670     05  MX-ODO-CTRS             COMP.
000680         10  MX-FILL-ODO-CTR             PIC S9(04).
000690         10  MX-CUST-ODO-CTR             PIC S9(04).
000700*    SKIP1
000710     05  MX-SYSTEM-DATA.
000720         10  MX-LAST-UPD-DATA.
000730             15  MX-LAST-UPD-ONLINE-DATA.
000740                  20  MX-LAST-UPD-DATE               PIC 9(08).
000750                  20  MX-LAST-UPD-DATE-X REDEFINES
000760                     MX-LAST-UPD-DATE.
000770                      25  MX-LAST-UPD-CC             PIC 9(02).
000780                      25  MX-LAST-UPD-DT             PIC 9(06).
000790                      25  MX-LAST-UPD-DT-X
000800                          REDEFINES MX-LAST-UPD-DT.
000810                          30  MX-LAST-UPD-YY         PIC X(02).
000820                          30  MX-LAST-UPD-MM         PIC X(02).
000830                          30  MX-LAST-UPD-DD         PIC X(02).
000840                 20  MX-GF-LAST-UPD-DT
000850                      REDEFINES MX-LAST-UPD-DATE     PIC 9(08).
000860                 20  MX-GF-LAST-UPD-DT-X
000870                     REDEFINES MX-LAST-UPD-DATE.
000880                     25  MX-GF-LAST-UPD-YY          PIC 9(04).
000890                     25  MX-GF-LAST-UPD-YY-X
000900                         REDEFINES MX-GF-LAST-UPD-YY.
000910                         30  MX-GF-LAST-UPD-CC      PIC 9(02).
000920                         30  MX-GF-LAST-UPD-YY2     PIC 9(02).
000930                     25  MX-GF-LAST-UPD-MM          PIC 9(02).
000940                     25  MX-GF-LAST-UPD-DD          PIC 9(02).
000950                 20  MX-LAST-UPD-TIME               PIC 9(06).
000960                 20  MX-LAST-UPD-TIME-X
000970                     REDEFINES MX-LAST-UPD-TIME.
000980                     25  MX-LAST-UPD-TIME-HH        PIC 9(02).
000990                     25  MX-LAST-UPD-TIME-MM        PIC 9(02).
001000                     25  MX-LAST-UPD-TIME-SS        PIC 9(02).
001010                 20  MX-LAST-UPD-US-ID              PIC X(10).
001020                 20  MX-LOCK-SW                     PIC X(01).
001030                 20  MX-LAST-UPD-PC-ID              PIC X(02).
001040                 20  MX-LAST-UPD-REL                PIC X(03).
001050             15  MX-LAST-UPD-BATCH-DATA.
001060                 20  MX-LAST-UPD-BATCH-DT           PIC 9(08).
001070                 20  MX-LAST-UPD-BATCH-DT-X
001080                     REDEFINES MX-LAST-UPD-BATCH-DT.
001090                     25  MX-LAST-UPD-BATCH-YY       PIC 9(04).
001100                     25  MX-LAST-UPD-BATCH-YY-X
001110                         REDEFINES MX-LAST-UPD-BATCH-YY.
001120                         30  MX-LAST-UPD-BATCH-CC   PIC 9(02).
001130                         30  MX-LAST-UPD-BATCH-YY2  PIC 9(02).
001140                     25  MX-LAST-UPD-BATCH-MM       PIC 9(02).
001150                     25  MX-LAST-UPD-BATCH-DD       PIC 9(02).
001160                 20  MX-LAST-UPD-BATCH-PROG-ID      PIC X(08).
001170                 20  MX-LAST-UPD-BATCH-FIL          PIC X(04).
001180*    SKIP1
001190         10  MX-USER-AREA-TBL.
001200             15  MX-USER-AREA-ITEM       PIC X(10)  OCCURS 3.
001210*    SKIP1
001220     05  MX-INDIC-DATA.
001230*    SKIP1
001240         10  MX-ORIG-KEY.
001250             15  MX-ORIG-GR-DV-MM.
001260                 20  MX-ORIG-GR-ID.
001270                     25  MX-ORIG-GR-ID-PFX   PIC X(02).
001280                     25  MX-ORIG-GR-ID-SFX   PIC X(06).
001290                 20  MX-ORIG-DV-ID           PIC X(04).
001300                 20  MX-ORIG-MM-ID           PIC X(09).
001310             15  MX-ORIG-FIL                 PIC X(01).
001320*    SKIP1
001330         10  MX-MG-DATA.
001340             15  MX-MG-POL-TYPE              PIC X(01).
001350                 88  MX-MG-POL-TYPE-VALID    VALUE ' ' 'M' 'S'.
001360                 88  MX-MG-POL-NOT-ASSOC     VALUE ' '.
001370                 88  MX-MG-POL-MASTER        VALUE 'M'.
001380                 88  MX-MG-POL-SUBSIDIARY    VALUE 'S'.
001390*    SKIP1
001400             15  MX-MG-KEY.
001410                 20  MX-MG-GR-DV-MM.
001420                     25  MX-MG-GR-ID.
001430                         30  MX-MG-GR-ID-PFX PIC X(02).
001440                         30  MX-MG-GR-ID-SFX PIC X(06).
001450                     25  MX-MG-DV-ID         PIC X(04).
001460                     25  MX-MG-MM-ID         PIC X(09).
001470                 20  MX-MG-FIL               PIC X(01).
001480*    SKIP1
001490         10  MX-P-KEYS-G.
001500             15  MX-P-KEY      OCCURS  8 TIMES.
001510                 20  MX-P-CI                 PIC X(02).
001520                 20  MX-P-KEY1.
001530                     25  MX-P-GR-DV-MM.
001540                         30  MX-P-GR-ID-PFX  PIC X(02).
001550                         30  MX-P-GR-ID-SFX  PIC X(06).
001560                         30  MX-P-DV-ID      PIC X(04).
001570                         30  MX-P-MM-ID      PIC X(09).
001580                     25  MX-P-KEY1-FIL       PIC X(01).
001590                     25  MX-P-SUB-REC-ID     PIC X(02).
001600                 20  MX-P-KEY-FIL            PIC X(02).
001610*    SKIP1
001620         10  MX-POINTER-KEYS REDEFINES MX-P-KEYS-G.
001630             15  MX-MA-KEY.
001640                 20  MX-MA-CI                 PIC X(02).
001650                 20  MX-MA-KEY1.
001660                     25  MX-MA-GR-DV-MM.
001670                         30  MX-MA-GR-ID-PFX  PIC X(02).
001680                         30  MX-MA-GR-ID-SFX  PIC X(06).
001690                         30  MX-MA-DV-ID      PIC X(04).
001700                         30  MX-MA-MM-ID      PIC X(09).
001710                     25  MX-MA-KEY1-FIL       PIC X(01).
001720                     25  MX-MA-SUB-REC-ID     PIC X(02).
001730                 20  MX-MA-KEY-FIL            PIC X(02).
001740*    SKIP1
001750             15  MX-MB-KEY.
001760                 20  MX-MB-CI                 PIC X(02).
001770                 20  MX-MB-KEY1.
001780                     25  MX-MB-GR-DV-MM.
001790                         30  MX-MB-GR-ID-PFX  PIC X(02).
001800                         30  MX-MB-GR-ID-SFX  PIC X(06).
001810                         30  MX-MB-DV-ID      PIC X(04).
001820                         30  MX-MB-MM-ID      PIC X(09).
001830                     25  MX-MB-KEY1-FIL       PIC X(01).
001840                     25  MX-MB-SUB-REC-ID     PIC X(02).
001850                 20  MX-MB-KEY-FIL            PIC X(02).
001860*    SKIP1
001870             15  MX-MD-KEY.
001880                 20  MX-MD-CI                 PIC X(02).
001890                 20  MX-MD-KEY1.
001900                     25  MX-MD-GR-DV-MM.
001910                         30  MX-MD-GR-ID-PFX  PIC X(02).
001920                         30  MX-MD-GR-ID-SFX  PIC X(06).
001930                         30  MX-MD-DV-ID      PIC X(04).
001940                         30  MX-MD-MM-ID      PIC X(09).
001950                     25  MX-MD-KEY1-FIL       PIC X(01).
001960                     25  MX-MD-SUB-REC-ID     PIC X(02).
001970                 20  MX-MD-KEY-FIL            PIC X(02).
001980*    SKIP1
001990             15  MX-MF-KEY.
002000                 20  MX-MF-CI                 PIC X(02).
002010                 20  MX-MF-KEY1.
002020                     25  MX-MF-GR-DV-MM.
002030                         30  MX-MF-GR-ID-PFX  PIC X(02).
002040                         30  MX-MF-GR-ID-SFX  PIC X(06).
002050                         30  MX-MF-DV-ID      PIC X(04).
002060                         30  MX-MF-MM-ID      PIC X(09).
002070                     25  MX-MF-KEY1-FIL       PIC X(01).
002080                     25  MX-MF-SUB-REC-ID     PIC X(02).
002090                 20  MX-MF-KEY-FIL            PIC X(02).
002100*    SKIP1
002110             15  MX-ML-KEY.
002120                 20  MX-ML-CI                 PIC X(02).
002130                 20  MX-ML-KEY1.
002140                     25  MX-ML-GR-DV-MM.
002150                         30  MX-ML-GR-ID-PFX  PIC X(02).
002160                         30  MX-ML-GR-ID-SFX  PIC X(06).
002170                         30  MX-ML-DV-ID      PIC X(04).
002180                         30  MX-ML-MM-ID      PIC X(09).
002190                     25  MX-ML-KEY1-FIL       PIC X(01).
002200                     25  MX-ML-SUB-REC-ID     PIC X(02).
002210                 20  MX-ML-KEY-FIL            PIC X(02).
002220*    SKIP1
002230             15  MX-MM-KEY.
002240                 20  MX-MM-CI                 PIC X(02).
002250                 20  MX-MM-KEY1.
002260                     25  MX-MM-GR-DV-MM.
002270                         30  MX-MM-GR-ID-PFX  PIC X(02).
002280                         30  MX-MM-GR-ID-SFX  PIC X(06).
002290                         30  MX-MM-DV-ID      PIC X(04).
002300                         30  MX-MM-MM-ID      PIC X(09).
002310                     25  MX-MM-KEY1-FIL       PIC X(01).
002320                     25  MX-MM-SUB-REC-ID     PIC X(02).
002330                 20  MX-MM-KEY-FIL            PIC X(02).
002340*    SKIP1
002350             15  MX-MO-KEY.
002360                 20  MX-MO-CI                 PIC X(02).
002370                 20  MX-MO-KEY1.
002380                     25  MX-MO-GR-DV-MM.
002390                         30  MX-MO-GR-ID-PFX  PIC X(02).
002400                         30  MX-MO-GR-ID-SFX  PIC X(06).
002410                         30  MX-MO-DV-ID      PIC X(04).
002420                         30  MX-MO-MM-ID      PIC X(09).
002430                     25  MX-MO-KEY1-FIL       PIC X(01).
002440                     25  MX-MO-SUB-REC-ID     PIC X(02).
002450                 20  MX-MO-KEY-FIL            PIC X(02).
002460*    SKIP1
002470             15  MX-MP-KEY.
002480                 20  MX-MP-CI                 PIC X(02).
002490                 20  MX-MP-KEY1.
002500                     25  MX-MP-GR-DV-MM.
002510                         30  MX-MP-GR-ID-PFX  PIC X(02).
002520                         30  MX-MP-GR-ID-SFX  PIC X(06).
002530                         30  MX-MP-DV-ID      PIC X(04).
002540                         30  MX-MP-MM-ID      PIC X(09).
002550                     25  MX-MP-KEY1-FIL       PIC X(01).
002560                     25  MX-MP-SUB-REC-ID     PIC X(02).
002570                 20  MX-MP-KEY-FIL            PIC X(02).
002580*    SKIP1
002590         10  MX-STS                       PIC X(01).
GLM#1              88  MX-STS-VALID      VALUE 'A' 'P' 'R' 'T' 'U'.
002610             88  MX-STS-ACTIVE               VALUE 'A'.
002620             88  MX-STS-PENDED               VALUE 'P'.
002630             88  MX-STS-TERMINATED           VALUE 'T'.
002640*    SKIP1
002650         10  MX-PAYMENT-TYPE-CODE        PIC X(01).
002660             88  MX-PAYMENT-TYPE-VALID       VALUE ' ' 'A' 'B'
002670                                                   'C'.
002680             88  MX-PAYMENT-TYPE-UNUSED      VALUE ' '.
002690             88  MX-PAYMENT-TYPE-AUTO-CHECK  VALUE 'A'.
002700             88  MX-PAYMENT-TYPE-DIRECT-BILL VALUE 'B'.
002710             88  MX-PAYMENT-TYPE-CREDIT-CARD VALUE 'C'.
002720*    SKIP1
002730         10  MX-BIL-DATA-AREA.
002740             15  MX-BIL-MODE             PIC X(02).
002750                 88  MX-BIL-MODE-VALID  VALUE '  ' '01' '02'
002760                                              '04' '12'.
002770                 88  MX-BIL-MODE-NO-ASSOC     VALUE '  '.
002780                 88  MX-BIL-MODE-ANNUAL       VALUE '01'.
002790                 88  MX-BIL-MODE-SEMI-AN      VALUE '02'.
002800                 88  MX-BIL-MODE-QRTRLY       VALUE '04'.
002810                 88  MX-BIL-MODE-MONTHLY      VALUE '12'.
002820             15  MX-BIL-BILLS-PER-YEAR   REDEFINES
002830                 MX-BIL-MODE             PIC 9(02).
002840             15  MX-BIL-SYNC-MNTH        PIC X(01).
002850             15  MX-BIL-DUE-DAY          PIC S9(03) COMP-3.
002860             15  MX-BIL-COMB-CD          PIC X(01).
002870                 88  MX-BIL-COMB-VALID        VALUE ' ' 'M' 'A'.
002880                 88  MX-BIL-COMB-ASSOC-VALID     VALUE 'M' 'A'.
002890                 88  MX-BIL-COMB-NOT-ASSOC-VALID VALUE ' '.
002900                 88  MX-BIL-COMB-WITH-MAST       VALUE 'M'.
002910                 88  MX-BIL-COMB-ALONE           VALUE 'A'.
002920             15  MX-BIL-FINAL            PIC X(01).
002930                 88 MX-BIL-FINAL-VALID        VALUES ' ' 'N' 'Y'.
002940                 88 MX-BIL-FINAL-DONE         VALUE 'Y'.
002950                 88 MX-BIL-FINAL-NOT-DONE     VALUE 'N'.
002960                 88 MX-BIL-FINAL-NOT-INDIV    VALUE ' '.
002970*    SKIP1
002980         10  MX-ORIG-EFF-DT.
002990             15  MX-ORIG-EFF-YY.
003000                 20  MX-ORIG-EFF-CC      PIC X(02).
003010                 20  MX-ORIG-EFF-YY2     PIC X(02).
003020             15  MX-ORIG-EFF-MM          PIC X(02).
003030             15  MX-ORIG-EFF-DD          PIC X(02).
003040*    SKIP1
003050         10  MX-TERMN8-DT.
003060             15  MX-TERMN8-YY.
003070                 20  MX-TERMN8-CC        PIC X(02).
003080                 20  MX-TERMN8-YY2       PIC X(02).
003090             15  MX-TERMN8-MM            PIC X(02).
003100             15  MX-TERMN8-DD            PIC X(02).
003110*    SKIP1
003120         10  MX-TERMN8-REASON            PIC X(02).
003130             88  MX-TERMN8-REASON-VALID      VALUE '  ' THRU '99'.
003140             88  MX-TERMN8-TRM-EMP           VALUE 'TE'.
003150             88  MX-TERMN8-DEATH             VALUE 'DT'.
003160             88  MX-TERMN8-OVER-AGE          VALUE 'OA'.
003170             88  MX-TERMN8-DECLINE           VALUE 'DC'.
003180             88  MX-TERMN8-LEAVE-OF-ABSENCE  VALUE 'LA'.
003190             88  MX-TERMN8-INELIGIBLE        VALUE 'IN'.
003200             88  MX-TERMN8-RESCISSION        VALUE 'RC'.
003210             88  MX-TERMN8-KEY-CHANGE        VALUE 'MK'.
003220             88  MX-TERMN8-UNUSED            VALUE '  '.
003230*    SKIP1
003240         10  MX-REINSTATE-DT.
003250             15  MX-REINSTATE-YY.
003260                 20  MX-REINSTATE-CC         PIC X(02).
003270                 20  MX-REINSTATE-YY2        PIC X(02).
003280             15  MX-REINSTATE-MM             PIC X(02).
003290             15  MX-REINSTATE-DD             PIC X(02).
003300*    SKIP1
003310         10  MX-DUE-DT-LAST-PD.
003320             15  MX-DUE-DT-LAST-PD-YY.
003330                 20  MX-DUE-DT-LAST-PD-CC    PIC X(02).
003340                 20  MX-DUE-DT-LAST-PD-YY2   PIC X(02).
003350             15  MX-DUE-DT-LAST-PD-MM        PIC X(02).
003360             15  MX-DUE-DT-LAST-PD-DD        PIC X(02).
003370*    SKIP1
003380         10 MX-DUE-DT-LAST-BILL.
003390             15 MX-DUE-DT-LAST-BILL-YY.
003400                 20 MX-DUE-DT-LAST-BILL-CC   PIC X(02).
003410                 20 MX-DUE-DT-LAST-BILL-YY2  PIC X(02).
003420             15 MX-DUE-DT-LAST-BILL-MM       PIC X(02).
003430             15 MX-DUE-DT-LAST-BILL-DD       PIC X(02).
003440*    SKIP1
003450         10 MX-END-DT-LAST-BILL.
003460             15 MX-END-DT-LAST-BILL-YY.
003470                 20 MX-END-DT-LAST-BILL-CC   PIC X(02).
003480                 20 MX-END-DT-LAST-BILL-YY2  PIC X(02).
003490             15 MX-END-DT-LAST-BILL-MM       PIC X(02).
003500             15 MX-END-DT-LAST-BILL-DD       PIC X(02).
003510*    SKIP1
003520         10  MX-PURGE-DT.
003530             15  MX-PURGE-YY.
003540                 20  MX-PURGE-CC         PIC X(02).
003550                 20  MX-PURGE-YY2        PIC X(02).
003560             15  MX-PURGE-MM             PIC X(02).
003570             15  MX-PURGE-DD             PIC X(02).
003580*    SKIP1
003590         10  MX-SYSTEM-DT.
003600             15  MX-SYSTEM-YY.
003610                 20  MX-SYSTEM-CC        PIC X(02).
003620                 20  MX-SYSTEM-YY2       PIC X(02).
003630             15  MX-SYSTEM-MM            PIC X(02).
003640             15  MX-SYSTEM-DD            PIC X(02).
003650*    SKIP1
003660         10  MX-CERT-DT.
003670             15  MX-CERT-YY.
003680                 20  MX-CERT-CC          PIC X(02).
003690                 20  MX-CERT-YY2         PIC X(02).
003700             15  MX-CERT-MM              PIC X(02).
003710             15  MX-CERT-DD              PIC X(02).
003720*    SKIP1
003730         10  MX-TRANS-DT.
003740             15  MX-TRANS-YY.
003750                 20  MX-TRANS-CC         PIC X(02).
003760                 20  MX-TRANS-YY2        PIC X(02).
003770             15  MX-TRANS-MM             PIC X(02).
003780             15  MX-TRANS-DD             PIC X(02).
003790*    SKIP1
003800         10  MX-TRANS-DT-CALC-IND        PIC X(01).
003810             88  MX-TRANS-DT-CALC-IND-VALID
003820                                           VALUES ' ' 'Y'
003830                                                  'M' 'D' 'G'.
003840             88  MX-TRANS-DT-CALCED-Y      VALUES 'Y'.
003850             88  MX-TRANS-DT-CALCED        VALUES 'M' 'D' 'G'.
003860             88  MX-TRANS-DT-CALCED-MM     VALUES 'M'.
003870             88  MX-TRANS-DT-CALCED-DV     VALUES 'D'.
003880             88  MX-TRANS-DT-CALCED-GR     VALUES 'G'.
003890*    SKIP1
003900         10  MX-ASSOC-DATA.
003910             15  MX-ANNIV-DT.
003920                 20 MX-ANNIV-MM          PIC X(02).
003930*    SKIP1
003940             15 MX-APP-DT.
003950                 20  MX-APP-YY.
003960                     25  MX-APP-CC       PIC X(02).
003970                     25  MX-APP-YY2      PIC X(02).
003980                 20  MX-APP-MM           PIC X(02).
003990                 20  MX-APP-DD           PIC X(02).
004000*    SKIP1
004010             15 MX-UND-DT.
004020                 20  MX-UND-YY.
004030                     25  MX-UND-CC       PIC X(02).
004040                     25  MX-UND-YY2      PIC X(02).
004050                 20  MX-UND-MM           PIC X(02).
004060                 20  MX-UND-DD           PIC X(02).
004070*    SKIP1
004080             15  MX-POL-NUMBER           PIC X(15).
004090*    SKIP1
004100         10  MX-RETRO-ADJ-CD             PIC X(01).
004110             88  MX-RETRO-ADJ-VALID        VALUES ' ' 'Y' 'N'.
004120             88  MX-RETRO-ADJ-YES          VALUE  ' ' 'Y'.
004130             88  MX-RETRO-ADJ-NO           VALUE  'N'.
004140*    SKIP1
004150         10  MX-EV-PRESENT-SW            PIC X(01).
004160             88  MX-EV-PRESENT             VALUE 'Y'.
004170*    SKIP1
004180         10  MX-EV-ABSENT-SW             PIC X(01).
004190             88  MX-EV-ABSENT              VALUE 'Y'.
004200*    SKIP1
004210         10  MX-EV-INCREASE-ABSENT-SW    PIC X(01).
004220             88  MX-EV-INCREASE-ABSENT     VALUE 'Y'.
004230*    SKIP1
004240         10  MX-EV-DENY-SW               PIC X(01).
004250             88  MX-EV-DENIED              VALUE 'Y'.
004260*    SKIP1
004270         10  MX-REBILL-IN-PROG-CD            PIC X(01).
004280             88  MX-REBILL-IN-PROG-VALID     VALUE 'Y' ' '.
004290             88  MX-REBILL-IN-PROG           VALUE 'Y'.
004300             88  MX-REBILL-NOT-IN-PROG       VALUE ' '.


               10  MX-KEY-CHG-IND              PIC X(01).
                   88  MX-KEY-CHGED                VALUE 'K'.
      *    SKIP1
               10  MX-REINS-IN-PROG-IND        PIC X(01).
                   88  MX-REINS-IN-PROG            VALUE 'Y'.
               10  MX-TERM-IN-PROG-IND REDEFINES
                   MX-REINS-IN-PROG-IND        PIC X(01).
                   88  MX-TERM-IN-PROG             VALUE 'T'.
      *    SKIP1
ADD-->         10  MX-EARLIEST-RATE-DT         PIC X(08).
ADD-->*    SKIP1
ADD-->         10  MX-UNUSED-FIL-1             PIC X(24).
DELETE*        10  MX-UNUSED-FIL-1             PIC X(32).
004330*    SKIP2
004340****************************************************************
004350*        END OF  ***  G 5 X R M X B 0  ***                     *
004360****************************************************************
