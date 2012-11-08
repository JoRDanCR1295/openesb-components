004400 01          REC.                                                 LZCDIS00
004500     05         LAENGE-GES PIC S9(4) COMP.                        LZCDIS00
004600     05         IMS                        PIC XX.                LZCDIS00
004700     05         LAST-TRC                   PIC X(8).              LZCDIS00
004800     05         SCHLUESSEL.                                       LZCDIS00
004900        10         EMPFAENGER.                                    LZCDIS00
005000           15         EMP-UN               PIC 99.                LZCDIS00
005100           15         EMP-BST              PIC 999.               LZCDIS00
005200           15         EMP-KZ-MASCHINE      PIC X.                 LZCDIS00
005300        10         ABSENDER.                                      LZCDIS00
005400           15         ABS-UN               PIC 99.                LZCDIS00
005500           15         ABS-BST              PIC 999.               LZCDIS00
005600           15         ABS-KZ-MASCHINE      PIC X.                 LZCDIS00
005700        10         SA-KEY.                                        LZCDIS00
005800           15         SA                   PIC 999.               LZCDIS00
005900           15         UEB-GR               PIC 9.                 LZCDIS00
006000        10         LAENGENFELDER.                                 LZCDIS00
006100           15         LAENGE-LRN           PIC S9(4) COMP         LZCDIS00
006200                                           VALUE +1.              LZCDIS00
006300           15         LAENGE-NLRN          PIC S9(4) COMP         LZCDIS00
006400                                           VALUE +1.              LZCDIS00
006500           15         LAENGE-LRA           PIC S9(4) COMP         LZCDIS00
006600                                           VALUE +1.              LZCDIS00
006700           15         LAENGE-NLRA          PIC S9(4) COMP         LZCDIS00
006800                                           VALUE +1.              LZCDIS00
006900     05         DATENTEIL.                                        LZCDIS00
007000        10         DATEN-NEU-LAGER.                               LZCDIS00
007100           15         LAGERREL-NEU                                LZCDIS00
007200                 OCCURS 1 TO 1000                                 LZCDIS00
007300                     DEPENDING ON         LAENGE-LRN              LZCDIS00
007400                                           PIC X.                 LZCDIS00
007500        10         DATEN-NEU-NICHT-LAGER.                         LZCDIS00
007600           15         NICHT-NEU                                   LZCDIS00
007700                 OCCURS 1 TO 1000                                 LZCDIS00
007800                     DEPENDING ON         LAENGE-NLRN             LZCDIS00
007900                                           PIC X.                 LZCDIS00
008000        10         DATEN-ALT-LAGER.                               LZCDIS00
008100           15         LAGERREL-ALT                                LZCDIS00
008200                 OCCURS 1 TO 1000                                 LZCDIS00
008300                     DEPENDING ON         LAENGE-LRA              LZCDIS00
008400                                           PIC X.                 LZCDIS00
008500        10         DATEN-ALT-NICHT-LAGER.                         LZCDIS00
008600           15         NICHT-LAGER-ALT                             LZCDIS00
008700                 OCCURS 1 TO 1000                                 LZCDIS00
008800                     DEPENDING ON         LAENGE-NLRA             LZCDIS00
008900                                           PIC X.                 LZCDIS00
