000010**********************************************************
000020*    CARRIER RECORD             : CFERRCR0               *
000030*        KEY LENGTH             : K-CR-KEY-LENGTH (24)   *
000040*        RECORD LENGTH          : K-CR-REC-LENGTH (352)  *
000050*                                                        *
000060*    CONTAINS OTHER CARRIER  INFORMATION.                *
000070*                                                        *
000080*        ** PROPRIETARY PROPERTY OF ERISCO, INC. **      *
000090*        **            COPYRIGHT 1984            **      *
000100**********************************************************
000110*    SKIP2
000120**********************************************************
000130*    -----    RELEASE  4.00  MODIFICATIONS    --------   *
000140*                                                        *
000150*    -  KEY LENGTH CHANGED FROM   16 TO 24               *
000160*    -  REC LENGTH CHANGED FROM  300 TO 350              *
000170*                                                        *
000180*    -  NEW FIELDS:                                      *
000190*       CR-ADDR4                                         *
000200*       CR-PZ-ID (KEY)                                   *
000210*       CR-ODO-CTRS                                      *
000220*       CR-FILL-ODO-CTR                                  *
000230*       CR-CUST-ODO-CTR                                  *
000240*       CR-LAST-UPD-DATA                                 *
000250*       CR-LAST-UPD-DT                                   *
000260*       CR-LAST-UPD-TIME                                 *
000270*       CR-LAST-UPD-US-ID                                *
000280*       CR-LOCK-SW                                       *
000290*                                                        *
000300*    -  DELETED:                                         *
000310*            NONE                                        *
000320*                                                        *
000330*    -  CHANGED:                                         *
000340*            ---- FROM ----      ----  TO  ----          *
000350*            NONE                                        *
000360*                                                        *
000370*    -  CONVERSION NOTES:                                *
000380*            MOVE CR-ADDR3       CR-ADDR4                *
000390*            MOVE SPACES         CR-ADDR3                *
000400*                                                        *
000410**********************************************************
000420*    SKIP2
000430**********************************************************
000440*    ----   MODIFICATIONS AFTER RELEASE 4.00   -------   *
000450*                                                        *
000460*    REL    DATE   PGMR DESCRIPTION                      *
000470*         06/10/97 GI   Y2K CHANGES, LENGTH CHANGED FROM *
000480*                       350   TO  352.                   *
000490**********************************************************
000500*    SKIP2
000510     05  CR-KEY.                                                         1
000520         10  CR-PZ-ID                    PIC X(02).                      1
000530             88  CR-CLAIMFACTS           VALUE 'CF'.
000540         10  CR-CI-ID                    PIC XX.                         3
000550         10  CR-REC-ID                   PIC XX.                         5
000560             88  CR-CARRIER-REC          VALUE 'CR'.
000570         10  CR-KEY1.                                                    7
000580             15  CR-ID                   PIC X(9).                       7
000590         10  CR-KEY-FIL                  PIC X(9).                      16
000600*    SKIP1
000610     05  CR-ODO-CTRS           COMP.                                    25
000620         10  CR-FILL-ODO-CTR             PIC S9(4).                     25
000630         10  CR-CUST-ODO-CTR             PIC S9(4).                     27
000640*    SKIP1
000650     05  CR-LAST-UPD-DATA.                                              29
Y2K  A         10  CR-LAST-UPD-DT-CYMD        PIC 9(08).                00430000
Y2K  A         10  CR-LAST-UPD-DT-CYMD-X REDEFINES                      00440000
Y2K  A              CR-LAST-UPD-DT-CYMD.                                00440000
Y2K  A            15  CR-LAST-UPD-CCYY        PIC 9(04).                      45
Y2K  A            15  FILLER                  PIC X(04).                      45
Y2K  A         10  CR-LAST-UPD-DATE-X REDEFINES                         00490000
Y2K  A               CR-LAST-UPD-DT-CYMD.                               00490000
Y2K  A            15  CR-LAST-UPD-CC              PIC 9(02).            00500000
Y2K  C            15  CR-LAST-UPD-DT              PIC 9(6).                   29
Y2K  C            15  CR-LAST-UPD-DT-X                                        29
Y2K  C               REDEFINES  CR-LAST-UPD-DT.
Y2K  C               20  CR-LAST-UPD-YY          PIC 99.                      29
Y2K  C               20  CR-LAST-UPD-MM          PIC 99.                      31
Y2K  C               20  CR-LAST-UPD-DD          PIC 99.                      33
000720         10  CR-LAST-UPD-TIME            PIC 9(6).                      35
000730         10  CR-LAST-UPD-TIME-X                                         35
000740             REDEFINES  CR-LAST-UPD-TIME.
000750             15  CR-LAST-UPD-TIME-HH     PIC 99.                        35
000760             15  CR-LAST-UPD-TIME-MM     PIC 99.                        37
000770             15  CR-LAST-UPD-TIME-SS     PIC 99.                        39
000780         10  CR-LAST-UPD-US-ID           PIC X(10).                     41
000790         10  CR-LOCK-SW                  PIC X.                         51
000800         10  FILLER                      PIC X(07).                     52
000810*    SKIP1
000820     05  FILLER.                                                        59
000830         10  FILLER                      PIC X(10)   OCCURS 3.          59
000840*    SKIP1
000850     05  CR-GEN-ID                       PIC X(12).                     89
000860*    SKIP1
000870     05  CR-MEMO                         PIC X(70).                    101
000880*    SKIP1
000890     05  CR-NAME                         PIC X(30).                    171
000900*    SKIP1
000910     05  CR-ADDRESS.                                                   201
000920         10  CR-ADDR1                    PIC X(30).                    201
000930         10  CR-ADDR2                    PIC X(30).                    231
000940         10  CR-ADDR3                    PIC X(30).                    261
000950         10  CR-ADDR4.                                                 291
000960             15  CR-CITY                 PIC X(19).                    291
000970             15  CR-STATE                PIC XX.                       310
000980             15  CR-ZIP                  PIC X(9).                     312
000990*    SKIP1
001000     05  CR-PHONE.                                                     321
001010         10 FILLER                       PIC X.                        321
001020         10 CR-AREA                      PIC XXX.                      322
001030         10 CR-EXCH                      PIC XXX.                      325
001040         10 CR-NMBR                      PIC X(4).                     328
001050*    SKIP2
001060     05  FILLER                          PIC X(19).                    332
001070*    SKIP1
001080**********************************************************
001090*     END OF ***  C F E R R C R 0 ***                    *
001100**********************************************************
