000010****************************************************************
000020*    GF MEMBER FIXED-DATA RECORD  :  G5XRMFB0                  *
000030*    KEY LENGTH                   :  K-MF-KEY-LENGTH (32)      *
000040*    RECORD LENGTH                :  K-MF-REC-LENGTH (170)     *
000050*                                                              *
000060*   RECORD CONTAINS FIXED MEMBER INDICATIVE INFORMATION NOT    *
000070*   INCLUDED IN OTHER MEMBER RECORD TYPES. THIS RECORD WILL    *
000080*   WILL BE REQUIRED FOR EVERY MEMBER AND CONTAINS DATA THAT   *
000090*   GENERALLY IS NOT CHANGED.  FOR THOSE DATA ITEMS THAT       *
000100*   MUST CHANGE, ELECTIONS WILL BE UTILIZED.                   *
000110*                                                              *
000120*   ANY CHANGES MADE TO G5XRMFB0 SHOULD BE SYNCHRONIZED WITH   *
000130*   G5XRMFN0 ('NEW-' VERSION OF G5XRMFB0).                     *
000140*                                                              *
000150****************************************************************
000160*     SKIP2
000170****************************************************************
000180*         **  PROPRIETARY PROPERTY OF ERISCO, INC.  **         *
000190*         **        COPYRIGHT 1984 - 1997           **         *
000200****************************************************************
000210*     SKIP2
000220****************************************************************
000230*       -----    RELEASE 6.0 MODIFICATION NOTES     -----      *
000240*                                                              *
000250*    -   REC LENGTH CHANGED FROM 200 TO 170                    *
000260*                                                              *
000270*    -   DELETED:                                              *
000280*        MF-UNUSED-FIL                                         *
000290*                                                              *
000300*    -   CHANGED:                                              *
000310*        MF-FIL-1                       FROM 43  TO 21  BYTES  *
000320*                                                              *
000330*    -   CONVERSION NOTES:                                     *
000340*        ALL DATE FIELDS WERE EXPANDED TO INCLUDE CENTURY      *
000350*                                                              *
000360****************************************************************
000370*     SKIP2
000380****************************************************************
000390*       -----    MODIFICATIONS AFTER RELEASE 6.0    -----      *
000400****************************************************************
000410*                                                              *
000420*--->REF#001 RLSE 6.00  CHGS MADE 05/24/96 THRU 08/18/97       *
000430*    ENH483  GF09 SY07                                         *
000440*     1. UPDATED FOR RELEASE 6.0                               *
000450*                                                              *
000460****************************************************************
000470*     SKIP2
000480****************************************************************
000490*       -----          RELEASE STAMP                -----      *
000500*                                                              *
000510*     G5XRMFB0  REL.6.00  08/18/97  12:29:04                   *
000520****************************************************************
000525 01 G5XRMFB0.
000530     05  MF-KEY.
000540         10  MF-PZ-ID                    PIC X(02).
000550             88  MF-GROUPFACTS             VALUE 'GF'.
000560         10  MF-CI-ID                    PIC X(02).
000570         10  MF-REC-ID                   PIC X(02).
000580             88  MF-MEMBER-DATA-REC        VALUE 'MM'.
000590         10  MF-KEY1.
000600             15  MF-GR-DV-MM.
000610                 20  MF-GR-ID.
000620                     25  MF-GR-ID-PFX    PIC X(02).
000630                     25  MF-GR-ID-SFX    PIC X(06).
000640                 20  MF-DV-ID            PIC X(04).
000650                 20  MF-MM-ID            PIC X(09).
000660             15  MF-KEY1-FIL             PIC X(01).
000670             15  MF-SUB-REC-ID           PIC X(02).
000680                 88  MF-FIXED-SUB-REC    VALUE 'MF'.
000690         10  MF-KEY-FIL                  PIC X(02).
000700*     SKIP1
000710     05  MF-ODO-CTRS             COMP.
000720         10  MF-FILL-ODO-CTR             PIC S9(04).
000730         10  MF-CUST-ODO-CTR             PIC S9(04).
000740*     SKIP1
000750     05  MF-SYSTEM-DATA.
000760         10  MF-LAST-UPD-DATA.
000770             15  MF-LAST-UPD-ONLINE-DATA.
000780                  20  MF-LAST-UPD-DATE               PIC 9(08).
000790                  20  MF-LAST-UPD-DATE-X REDEFINES
000800                     MF-LAST-UPD-DATE.
000810                      25  MF-LAST-UPD-CC             PIC 9(02).
000820                      25  MF-LAST-UPD-DT             PIC 9(06).
000830                      25  MF-LAST-UPD-DT-X
000840                          REDEFINES MF-LAST-UPD-DT.
000850                          30  MF-LAST-UPD-YY         PIC X(02).
000860                          30  MF-LAST-UPD-MM         PIC X(02).
000870                          30  MF-LAST-UPD-DD         PIC X(02).
000880                 20  MF-GF-LAST-UPD-DT
000890                      REDEFINES MF-LAST-UPD-DATE     PIC 9(08).
000900                 20  MF-GF-LAST-UPD-DT-X
000910                     REDEFINES MF-LAST-UPD-DATE.
000920                     25  MF-GF-LAST-UPD-YY          PIC 9(04).
000930                     25  MF-GF-LAST-UPD-YY-X
000940                         REDEFINES MF-GF-LAST-UPD-YY.
000950                         30  MF-GF-LAST-UPD-CC      PIC 9(02).
000960                         30  MF-GF-LAST-UPD-YY2     PIC 9(02).
000970                     25  MF-GF-LAST-UPD-MM          PIC 9(02).
000980                     25  MF-GF-LAST-UPD-DD          PIC 9(02).
000990                 20  MF-LAST-UPD-TIME               PIC 9(06).
001000                 20  MF-LAST-UPD-TIME-X
001010                     REDEFINES MF-LAST-UPD-TIME.
001020                     25  MF-LAST-UPD-TIME-HH        PIC 9(02).
001030                     25  MF-LAST-UPD-TIME-MM        PIC 9(02).
001040                     25  MF-LAST-UPD-TIME-SS        PIC 9(02).
001050                 20  MF-LAST-UPD-US-ID              PIC X(10).
001060                 20  MF-LOCK-SW                     PIC X(01).
001070                 20  MF-LAST-UPD-PC-ID              PIC X(02).
001080                 20  MF-LAST-UPD-REL                PIC X(03).
001090*     SKIP1
001100             15  MF-LAST-UPD-BATCH-DATA.
001110                 20  MF-LAST-UPD-BATCH-DT           PIC 9(08).
001120                 20  MF-LAST-UPD-BATCH-DT-X
001130                     REDEFINES MF-LAST-UPD-BATCH-DT.
001140                     25  MF-LAST-UPD-BATCH-YY       PIC 9(04).
001150                     25  MF-LAST-UPD-BATCH-YY-X
001160                         REDEFINES MF-LAST-UPD-BATCH-YY.
001170                         30  MF-LAST-UPD-BATCH-CC   PIC 9(02).
001180                         30  MF-LAST-UPD-BATCH-YY2  PIC 9(02).
001190                     25  MF-LAST-UPD-BATCH-MM       PIC 9(02).
001200                     25  MF-LAST-UPD-BATCH-DD       PIC 9(02).
001210                 20  MF-LAST-UPD-BATCH-PROG-ID      PIC X(08).
001220                 20  MF-LAST-UPD-BATCH-FIL          PIC X(04).
001230*     SKIP1
001240     05  MF-INDIC-INFO.
001250         10  MF-USE-CTR                  PIC S9(03) COMP-3.
001260*     SKIP1
001270     05  MF-FIXED-DATA.
001280*     SKIP1
001290         10  MF-GEN-ID                   PIC X(12).
001300             88  MF-GEN-PSC-DUMMY-MM       VALUE 'PSC-DUMMY-MM'.
001310         10  MF-NAME                     PIC X(30).
001320*     SKIP1
001330         10  MF-SEX                      PIC X(01).
001340             88  MF-SEX-VALID              VALUE 'M' 'F'.
001350             88  MF-MALE                   VALUE 'M'.
001360             88  MF-FEMALE                 VALUE 'F'.
001370*     SKIP1
001380         10  MF-SSN                      PIC X(09).
001390*     SKIP1
001400         10  MF-SSN-UNUSED-FIL           PIC X(01).
001410*     SKIP1
001420         10  MF-DOB.
001430             15  MF-DOB-YY.
001440                 20 MF-DOB-CC            PIC X(02).
001450                 20 MF-DOB-YY2           PIC X(02).
001460             15  MF-DOB-MM               PIC X(02).
001470             15  MF-DOB-DD               PIC X(02).
001480         10  MF-FIL-1                    PIC X(21).
001490*     SKIP1
001500****************************************************************
001510*            END  OF  G 5 X R M F B 0                          *
001520****************************************************************
