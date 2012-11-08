000010**************************************************************
000020*             COMPRESSED                                     *
000030*    EXPANDED CLAIM RECORD (INPUT)  : CFERRECR               *
000040*            KEY LENGTH             : K-EC-KEY-LENGTH (44)   *
Y2K****            MAX RECORD LENGTH      : K-EC-REC-LENGTH (16098)*
000060*                                                            *
000070*    CONTAINS COMPRESSED CLAIM RELATED INFORMATION.          *
000080*                                                            *
000090*    NOTE - THIS IS THE COMPRESSED VERSION OF THE            *
000100*           EXPANDED CLAIM RECORD USED WHEN READING          *
000110*           AS INPUT.  CHANGES MADE TO THIS COPYBOOK         *
000120*           MAY REQUIRE CHANGES IN THE COPYBOOKS FOR         *
000130*           THE OUTPUT VERSION 'CFERRECW' AND THE            *
000140*           EXPANDED VERSION 'CFERREC0' AND VICE-VERSA.      *
000150*                                                            *
000160*           THIS COPYBOOK SHOULD BE USED IN CONJUNCTION      *
000170*           WITH THE COPYBOOK 'CFERPECR' WHICH               *
000180*           EXPANDS THE EXPANDED CLAIM RECORD.               *
000190*                                                            *
000200*                                                            *
000210*        ** PROPRIETARY PROPERTY OF ERISCO, INC. **          *
000220*        **            COPYRIGHT 1984            **          *
000230**************************************************************
000240*    SKIP1
000250**************************************************************
000260*    -----    RELEASE  4.00  MODIFICATIONS    --------       *
000270*                                                            *
000280*    -  NEW RECORD                                           *
000290*                                                            *
000300**************************************************************
000310*    SKIP1
000320**************************************************************
000330*    ----   MODIFICATIONS AFTER RELEASE 4.00   -------       *
000340*                                                            *
000350*    REL    DATE   PGMR DESCRIPTION                          *
000360*    4.31 12/28/89 REP  ADDED   ECR-CL-FILL-ODO-CTR          *
000370*                       ADDED   ECR-CL-CUST-ODO-CTR          *
000380*                       CHANGED ECR-FILL-ODO-CTR             *
000390*                       CHANGED ECR-CUST-ODO-CTR             *
000400*                       ADDED   ECR-CL-FILL-AREA-G           *
000410*                       ADDED   ECR-CL-CUST-AREA-G           *
000420*                       CHANGED ECR-FILL-AREA-G              *
000430*                       CHANGED ECR-CUST-AREA-G              *
000431*                                                            *
000432*         06/13/97 MPET Y2K RECORD LENGTH CHANGED FROM 16000 *
000433*                       TO 16098 BYTES. THE FIELD ECR-FIXED- *
000434*                       DATA WAS CHANGED FROM 1040 TO 1064   *
000435*                       BYTES. OCCURENCES OF THE FIELD ECR-  *
000436*                       NFD-BYTE WAS CHANGED FROM 9600 TO    *
000437*                       9636.                                *
000438*                       ACCORDING TO THE CHANGES MADE IN COPY-
000439*                       BOOK CFERRCL0 THE FOLLOWING FIELDS   *
000440*                       WERE CHANGED:                        *
000441*                       ECR-CL-FIXED-DATA(FROM 548 TO 570 BYTES)
000442*                       ECR-CL-INFO-G (FROM 100 TO 102 BYTES)*
000443*                       ECR-CL-NON-FIXED-DATA(THE NUMBER OF  *
000444*                       OCCURENCES WAS CHANGED FROM 3260 TO  *
000445*                       3262 TIMES)                          *
000446*                       ECR-CL-FILL-BYTE(THE NUMBER OF OCCURE-
000447*                       NCES WAS CHANGED FROM 400 TO 402 TIMES).
000448*                                                            *
000450**************************************************************
000460     05  ECR-KEY.                                                        1
000470         10  ECR-PZ-ID                       PIC XX.                     1
000480             88  ECR-CLAIMFACTS              VALUE 'CF'.
000490         10  ECR-CI-ID                       PIC XX.                     3
000500         10  ECR-REC-ID                      PIC XX.                     5
000510             88  ECR-CLAIM-REC               VALUE 'CL'.
000520         10  ECR-KEY1.                                                   7
000530             15  ECR-PA-KEY1.                                            7
000540                 20  ECR-ME-KEY1.                                        7
000550                     25  ECR-PA-GROUP        PIC X(08).                  7
000560                     25  ECR-PA-ID           PIC X(09).                 15
000570                 20  ECR-PA-REL-NAME.                                   24
000580                     25  ECR-PA-REL          PIC X.                     24
000590                     25  ECR-PA-ID-NAME      PIC X(06).                 25
000600             15  ECR-SYS-ID                  PIC X.                     31
000610                 88  ECR-DENTAL              VALUE 'D'.
000620                 88  ECR-DISABILITY          VALUE 'K' 'L'.
000630                 88  ECR-STD                 VALUE 'K'.
000640                 88  ECR-LTD                 VALUE 'L'.
000650                 88  ECR-MEDICAL             VALUE 'M'.
000660             15  ECR-CL-KEY1.                                           32
000670                 20  ECR-CL-ID               PIC X(09).                 32
000680                 20  ECR-CL-GEN-X.                                      41
000690                     25  ECR-CL-GEN          PIC 99.                    41
000700         10  ECR-KEY-FIL                     PIC XX.                    43
000710     05  ECR-ODO-CTRS.                                                  45
000720         10  ECR-C-ODO-CTR                   PIC S9(4)    COMP.         45
000730         10  ECR-CL-NFD-ODO-CTR              PIC S9(4)    COMP.         47
000740         10  ECR-CL-FILL-ODO-CTR             PIC S9(4)    COMP.         49
000750         10  ECR-CL-CUST-ODO-CTR             PIC S9(4)    COMP.         51
000760         10  ECR-NFD-ODO-CTR                 PIC S9(4)    COMP.         53
000770         10  ECR-FILL-ODO-CTR                PIC S9(4)    COMP.         55
000780         10  ECR-CUST-ODO-CTR                PIC S9(4)    COMP.         57
********   05  ECR-CL-FIXED-DATA                   PIC X(548).                59
Y2K        05  ECR-CL-FIXED-DATA                   PIC X(570).                59
000800     05  ECR-C-INFO-G.                                                 607
********       10  ECR-C-INFO                      PIC X(100)                607
Y2K            10  ECR-C-INFO                      PIC X(102)                607
000820                           OCCURS  0  TO  3  TIMES
000830                           DEPENDING ON ECR-C-ODO-CTR.
000840     05  ECR-CL-NON-FIXED-DATA.                                          1
000850         10  ECR-CL-NFD-BYTE                 PIC X                       1
**********                       OCCURS  0  TO  3260  TIMES
Y2K                              OCCURS  0  TO  3262  TIMES
000870                           DEPENDING ON ECR-CL-NFD-ODO-CTR.
000880     05  ECR-CL-FILL-AREA-G.                                          4097
000890         10  ECR-CL-FILL-BYTE                PIC X                    4097
**********                       OCCURS  0  TO  400  TIMES
Y2K                              OCCURS  0  TO  402  TIMES
000910                           DEPENDING ON ECR-CL-FILL-ODO-CTR.
000920     05  ECR-CL-CUST-AREA-G.                                          8193
000930         10  ECR-CL-CUST-BYTE                   PIC X                 8193
000940                           OCCURS  0  TO  100  TIMES
000950                           DEPENDING ON ECR-CL-CUST-ODO-CTR.
Y2K        05  ECR-FIXED-DATA                      PIC X(1064).            12289
000970     05  ECR-NON-FIXED-DATA.                                         13329
Y2K            10  ECR-NFD-BYTE                    PIC X                   13329
Y2K                              OCCURS  0  TO  9636  TIMES
001000                           DEPENDING ON ECR-NFD-ODO-CTR
001010                           INDEXED   BY ECR-NFD-ODO-IDX.
001020     05  ECR-FILL-AREA-G.                                            24577
001030         10  ECR-FILL-BYTE                   PIC X                   24577
001040                           OCCURS  0  TO  600  TIMES
001050                           DEPENDING ON ECR-FILL-ODO-CTR.
001060     05  ECR-CUST-AREA-G.                                            28673
001070         10  ECR-CUST-BYTE                   PIC X                   28673
001080                           OCCURS  0  TO  100  TIMES
001090                           DEPENDING ON ECR-CUST-ODO-CTR.
001100**********************************************************
001110*            END OF ***  C F E R R E C R ***             *
001120**********************************************************
