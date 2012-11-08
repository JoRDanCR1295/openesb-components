001000******************************************************************
001100*GDS7001B                                                        *
001200*THIS IS THE GLOBAL DIRECTORY SYSTEM OUTPUT BUFFER.              *
001300******************************************************************
001400
001500 01  GDS-OUTPUT-BUFFER.
001600     03  GDSO-HEADER-RECORD.
001700         05  GDSO-BUFFER-ID                      PIC X(06).
001800         05  GDSO-RESULT-TYPE                    PIC 9.
001900         05  GDSO-RESULT-CODE                    PIC S9(09).
002000         05  GDSO-RESULT-MODULE                  PIC X(08).
002100         05  GDSO-RESULT-TEXT                    PIC X(80).
002200     03  GDSO-DATA-AREA.
002300         05  GDSO-PU-PRIMARY-KEY.
002400             10  GDSO-PU-COUNTRY                 PIC X(02).
002500             10  GDSO-PU-STATE                   PIC X(02).
002600             10  GDSO-PU-CITY                    PIC X(03).
002700             10  GDSO-PU-LOC                     PIC S9(04).
002800             10  GDSO-PU-LOC-R  REDEFINES GDSO-PU-LOC.
002900                 15  GDSO-PU-LOC-FILL            PIC X(02).       26 OCT94
003000                 15  GDSO-PU-LOC-NBR             PIC S9(02).
003100         05  GDSO-PU-GEO-CD                      PIC X(05).
003200         05  GDSO-PU-RAW-AREA-NBR                PIC S9(09).      26 OCT94
003300         05  GDSO-PU-AREA-GRP REDEFINES                           26 OCT94
003400             GDSO-PU-RAW-AREA-NBR.                                26 OCT94
003500             10  GDSO-PU-AREA-FILL               PIC X(04).       26 OCT94
003600             10  GDSO-PU-AREA-NBR-GRP.                            26 OCT94
003700                 15  GDSO-PU-AREA-TYPE-IND       PIC 9(01).       26 OCT94
003800                 15  GDSO-PU-AREA-NBR            PIC S9(04).      26 OCT94
003900         05  GDSO-PU-GEO-POOL-NBR                PIC S9(04).
004000         05  GDSO-PU-GEO-POOL-IND                PIC X(02).
004100         05  GDSO-PU-NTRL-LOC-NBR                PIC S9(09).
004200         05  GDSO-PU-SYS-DATE-USED               PIC S9(09).
004300         05  GDSO-PU-IC-NBR                      PIC S9(09).
004400         05  GDSO-PU-RTCY-NTRL-LOC-NBR           PIC S9(09).
004500         05  GDSO-PU-REF2-NTRL-LOC-NBR           PIC S9(09).
004600         05  GDSO-PU-CITY-BANDING-TBL.
004700             10  GDSO-PU-CITY-BANDING-CD     OCCURS 24 TIMES
004800                                                 PIC X(01).
004900         05  GDSO-PU-LOCAL-CRNCY-CD              PIC X(03).
005000         05  GDSO-PU-FORGN-CRNCY-CD              PIC X(03).
005100         05  GDSO-PU-US-CRNCY-RATE               PIC S9(4)V9(6).
005200         05  GDSO-PU-IC-DROP-TBL-KEY             PIC X(06).
005300         05  GDSO-PU-TIME-ZONE                   PIC S9(04).      26 OCT94
005400         05  GDSO-PU-TIME-ZONE-HH-MM REDEFINES                    26 OCT94
005500             GDSO-PU-TIME-ZONE.                                   26 OCT94
005600             10  GDSO-PU-TIME-ZONE-HH            PIC 9(02).       26 OCT94
005700             10  GDSO-PU-TIME-ZONE-MM            PIC S9(02).      26 OCT94
005800         05  GDSO-GRACE-PERIOD                   PIC S9(04).      24 JAN96
005900         05  GDSO-GRACE-PERIOD-HH-MM REDEFINES                    24 JAN96
006000             GDSO-GRACE-PERIOD.                                   24 JAN96
006100             10  GDSO-GRACE-PERIOD-HH            PIC 9(02).       24 JAN96
006200             10  GDSO-GRACE-PERIOD-MM            PIC S9(02).      24 JAN96
006300         05  GDSO-PU-VEH-CD.
006400             10  GDSO-PU-VEH-CLASS               PIC X(01).
006500             10  GDSO-PU-VEH-DOORS               PIC S9(04).
006600         05  GDSO-PU-UPG-PROMO-VEH-GRP.
006700             10  GDSO-PU-UPG-PROMO-VEH-TBL OCCURS 5 TIMES.
006800                 15  GDSO-PU-UPG-PROMO-VEH-CD.
006900                     20  GDSO-PU-UPG-PROMO-VEH-CLASS  PIC X(01).
007000                     20  GDSO-PU-UPG-PROMO-VEH-DOORS  PIC S9(04).
007100                 15  GDSO-PU-UPG-PROMO-VEH-IND  OCCURS 10 TIMES
007200                                                 PIC X(03).
007300         05  GDSO-PU-VEH-SIPP-CLASS              PIC X(04).
007400         05  GDSO-PU-VEH-MIN-AGE-1               PIC S9(04).
007500         05  GDSO-PU-VEH-MIN-AGE-2               PIC S9(04).
007600         05  GDSO-PU-VEH-MAX-AGE                 PIC S9(04).
007700         05  GDSO-DP-PRIMARY-KEY.
007800             10  GDSO-DP-COUNTRY                 PIC X(02).
007900             10  GDSO-DP-STATE                   PIC X(02).
008000             10  GDSO-DP-CITY                    PIC X(03).
008100             10  GDSO-DP-LOC                     PIC S9(04).      12 SEP94
008200             10  GDSO-DP-LOC-R  REDEFINES GDSO-DP-LOC.            12 SEP94
008300                 15  GDSO-DP-LOC-FILL            PIC X(02).       26 OCT94
008400                 15  GDSO-DP-LOC-NBR             PIC S9(02).      12 SEP94
008500         05  GDSO-DP-GEO-CD                      PIC X(05).
008600         05  GDSO-DP-RAW-AREA-NBR                PIC S9(09).      26 OCT94
008700         05  GDSO-DP-AREA-GRP REDEFINES                           26 OCT94
008800             GDSO-DP-RAW-AREA-NBR.                                26 OCT94
008900             10  GDSO-DP-AREA-FILL               PIC X(04).       26 OCT94
009000             10  GDSO-DP-AREA-NBR-GRP.                            26 OCT94
009100                 15  GDSO-DP-AREA-TYPE-IND       PIC 9(01).       26 OCT94
009200                 15  GDSO-DP-AREA-NBR            PIC S9(04).      26 OCT94
009300         05  GDSO-DP-GEO-POOL-NBR                PIC S9(04).
009400         05  GDSO-DP-GEO-POOL-IND                PIC X(02).
009500         05  GDSO-DP-NTRL-LOC-NBR                PIC S9(09).
009600         05  GDSO-DP-IC-NBR                      PIC S9(09).
009700         05  GDSO-DP-RTCY-NTRL-LOC-NBR           PIC S9(09).
009800         05  GDSO-DP-REF2-NTRL-LOC-NBR           PIC S9(09).
009900         05  GDSO-DP-TIME-ZONE                   PIC S9(04).      26 OCT94
010000         05  GDSO-DP-TIME-ZONE-HH-MM REDEFINES                    26 OCT94
010100             GDSO-DP-TIME-ZONE.                                   26 OCT94
010200             10  GDSO-DP-TIME-ZONE-HH            PIC 9(02).       26 OCT94
010300             10  GDSO-DP-TIME-ZONE-MM            PIC S9(02).      26 OCT94
010400         05  GDSO-OKC-TIME-ZONE                  PIC S9(04).      14 NOV94
010500         05  GDSO-OKC-TIME-ZONE-HH-MM REDEFINES                   14 NOV94
010600             GDSO-OKC-TIME-ZONE.                                  14 NOV94
010700             10  GDSO-OKC-TIME-ZONE-HH           PIC 9(02).       14 NOV94
010800             10  GDSO-OKC-TIME-ZONE-MM           PIC S9(02).      14 NOV94
010900     03  GDSO-VERBIAGE-AREA.                                      24 JAN96
011000         05  GDSO-VERBIAGE-TBL OCCURS 5 TIMES.                    24 JAN96
011100             10  GDSO-VERBIAGE-KEY              PIC X(03).        24 JAN96
011200             10  GDSO-QUOTE-ON-RES              PIC X(01).        24 JAN96
011300             10  GDSO-DISPLAY-NUMBER            PIC X(07).        21 MAY97
011400         05  GDSO-FILLER                        PIC X(45).        21 MAY97
011500     03  GDSO-FREEMILE-BUFFER.
011600         05  GDSO-FM-DAY-NBR                    PIC S9(09).
011700         05  GDSO-FM-WEEK-NBR                   PIC S9(09).
011800         05  GDSO-FM-MONTH-NBR                  PIC S9(09).
011900         05  GDSO-FM-TRAN-NBR                   PIC S9(09).
012000         05  GDSO-FM-EX-HH-NBR                  PIC S9(09).
012100         05  GDSO-FM-EX-DD-NBR                  PIC S9(09).
012200         05  GDSO-FM-EX-WK-NBR                  PIC S9(09).
012300     03  GDSO-ESTMILE-BUFFER.
012400         05  GDSO-EM-RENTAL-DAYS                PIC S9(04).
012500         05  GDSO-EM-RENTAL-MILES               PIC S9(04).
012600     03  GDSO-PU-LOCATION-IND-TBL.
012700         05  GDSO-PU-LOCATION-IND OCCURS 30     PIC X(03).
012800     03  GDSO-DP-LOCATION-IND-TBL.
012900         05  GDSO-DP-LOCATION-IND OCCURS 30     PIC X(03).
013000     03  GDSO-VEH-IND-TBL.
013100         05  GDSO-VEH-IND         OCCURS 10     PIC X(03).
013200     03  GDSO-LOCAL-RENTER-FG                   PIC X(01).        27 JAN99
013300     03  GDSO-HLE-LOC-FG                        PIC X(01).        29 JAN99
013400     03  GDSO-TVL-LOC-FG                        PIC X(01).        29 JAN99
013410     03  GDSO-LOCAL-ACFR-FG                     PIC X(01).        12 Dec02
013500     03  GDSO-FILLER                            PIC X(488).       12 Dec02
013600     03  GDSO-CHG-AREA.                                            5 FEB96
013700         05  GDSO-CHG-TBL-CNT                   PIC 9(2).          8 FEB96
013800         05  GDSO-CHG-TBL OCCURS 0 to 35 TIMES DEPENDING ON
                        GDSO-CHG-TBL-CNT.                                8 FEB96
013900             10  GDSO-PU-CHG-GRP.                                  5 FEB96
014000                 15  GDSO-CHARGE-TYPE           PIC X(01).         8 FEB96
014100                     88  GDSO-FIXED          VALUE 'F'.            8 FEB96
014200                     88  GDSO-PCT            VALUE 'P'.           12 FEB96
014300                     88  GDSO-TAX            VALUE 'T'.            8 FEB96
014400                 15  GDSO-GROUP-NBR             PIC S9(04).       20 FEB96
014500                 15  GDSO-TAXABLE               PIC X(01).         8 FEB96
014600                 15  GDSO-CHG-KEY               PIC X(03).         8 FEB96
014700                 15  GDSO-CHG-RTRN-SQLCODE      PIC S9(09).        5 FEB96
014800                 15  GDSO-AGGREGATE-FG          PIC X(01).         5 FEB96
014900                 15  GDSO-AGGREGATE-SW          PIC X(01).         5 FEB96
015000                 15  GDSO-AGG-START-DAY-NBR     PIC S9(03).        8 FEB96
015100                 15  GDSO-AGG-STOP-DAY-NBR      PIC S9(03).        8 FEB96
015110                 15  GDSO-MIN-AGE               PIC S9(04).       22 Dec03
015120                 15  GDSO-MAX-AGE               PIC S9(04).       22 Dec03
015200                 15  GDSO-CHARGE-SW             PIC X(01).         5 FEB96
015300                 15  GDSO-FIXED-UNIT            PIC S9(09)V99.     5 FEB96
015400                 15  GDSO-PERCENTAGE REDEFINES GDSO-FIXED-UNIT.   20 FEB96
015500                     20  FILLER                 PIC X(03).         8 FEB96
015600                     20  GDSO-PERCENT           PIC S9(03)V9(05).  8 FEB96
015700                 15  GDSO-EX-HH-FEE             PIC S9(09)V99.     5 FEB96
015800                 15  GDSO-MAX-AMT               PIC S9(09)V99.     5 FEB96
015900                 15  GDSO-FLAT-FEE              PIC S9(09)V99.     5 FEB96
016000                 15  GDSO-MAX-DAYS              PIC S9(04).        5 FEB96
016100                 15  GDSO-RESUME-DD             PIC S9(04).       19 Jul02
016200                 15  GDSO-WEEK-FEE              PIC S9(09)V99.     5 FEB96
016300                 15  GDSO-MONTH-CHG             PIC S9(09)V99.     5 FEB96
016400                 15  GDSO-REPLACEMENT           PIC S9(09)V99.     5 FEB96
016500                 15  GDSO-AIRPORT-MIN-AMOUNT                       5 FEB99
016600                                     REDEFINES GDSO-REPLACEMENT.   5 FEB99
016700                     20  GDSO-ARPT-FEE-MIN-AMT  PIC S9(09)V99.     5 FEB99
016800                 15  GDSO-PERC-CD-TBL      OCCURS 8 TIMES.         8 FEB96
016900                     20  GDSO-PERC-XREF-CD      PIC X(03).         8 FEB96
