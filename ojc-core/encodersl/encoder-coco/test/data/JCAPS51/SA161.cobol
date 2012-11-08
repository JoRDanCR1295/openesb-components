         01 SA161.
           02 IBM-161.
             03 LIEFP-KEY.
               04 EMPFAENGER.
                 05 UN-BST-ABT-EMPF.
                   06 UN-BST.
                     07 UN                  PIC 9(02).
                     07 BST                 PIC 9(03).
                   06 ABT                   PIC 9(03).
               04 LIEFERANT-NR.
                 05 LIEF-BBN                PIC 9(08).
                 05 LIEF-REST               PIC X(05).
               04 ILN REDEFINES LIEFERANT-NR.
                 05 BBN-OHNE-PRUEFZIFFER    PIC X(7).
                 05 ILN-REST                PIC X(5).
                 05 ILN-PRUEFZIFFER         PIC X(1).
               04 LIEFER-AVIS               PIC X(10).
               04 DAT-ERSTELLUNG.
                 05 JAHRHUNDERT             PIC 9(02).
                 05 JJMMTT.
                   06 JAHR                  PIC 9(02).
                   06 MONAT                 PIC 9(02).
                   06 TAG                   PIC 9(02).
               04 LIEFP-ALT-KEY-1.
                 05 S-UN-BST-ABT.
                   06 UN-BST.
                     07 UN                  PIC 9(02).
                     07 BST                 PIC 9(03).
                   06 ABT                   PIC 9(03).
                 05 BEPO.
                   06 WG                    PIC X(01).
                   06 ART-GR-9              PIC 9(01).
                   06 BEPO-NR               PIC 9(04).
                   06 BEPO-PRUEFZIFFER      PIC 9(01).
                 05 GROESSE                 PIC 9(07).
             03 LIEFP-ALT-KEY-2.
               04 EAN                       PIC 9(13).
             03 MENGE-VE-LIEFERSCHEIN       PIC 9(07)V9(02).
             03 VK                          PIC S9(07)V9(02) 
             SIGN LEADING SEPARATE.
             03 AUFTRAGSNR                  PIC X(12).
             03 AVIS-TYP.
               04 KENNZ-AVIS-TYP            PIC X(01).
                 88 CROSSDOCKING-AVIS       VALUE IS "C".
                 88 DEPOT-AVIS              VALUE IS "D".
                 88 LAGERWARE               VALUE IS "D".
                 88 RUECKRUF-AVIS           VALUE IS "R".
                 88 TRANSIT-AVIS            VALUE IS "T".
             03 EK                          PIC 9(07)V9(03).
             03 AP                          PIC 9(07)V9(03).
             03 FIL-VK                      PIC 9(07)V9(03).
             03 NVE.
               04 KZ-VERPACKUNG             PIC 9(01).
               04 BBN-OHNE-PRUEFZIFFER      PIC 9(07).
               04 NVE-LFD-NR                PIC 9(09).
               04 PRUEF-ZIFF                PIC 9(01).
             03 KZ-HAENGEWARE.
               04 KENNZ-HAENGE-LIEGEWARE    PIC X(01).
                 88 LIEGEWARE               VALUE IS " ".
                 88 HAENGEWARE              VALUE IS "H".
             03 LAENGE-HAENGEWARE           PIC 9(04).
             03 KZ-PREISBINDUNG             PIC X.
               88 KEINE-PREISBINDUNG        VALUE IS " ".
               88 KARTELLAMT-PREISBINDUNG   VALUE IS "1".
               88 ZE-PREISBINDUNG           VALUE IS "2".
               88 NECKERMANN                VALUE IS "3".
             03 KONTONUMMER                 PIC X(06).
             03 BEPO-BEZ                    PIC X(20).
             03 TEXT-BEPO-LIEFER            PIC X(20).
             03 ART-BEZ                     PIC X(20).
             03 TEXT-ART-LIEFER             PIC X(20).
             03 SERIEN-BEZ                  PIC X(20).
             03 TEXT-SERIE-LIEFER           PIC X(20).
             03 GROESSEN-BEZ                PIC X(20).
             03 KOMMISSIONSWARE.
               04 KOMMISSIONSWARE-DEF       PIC X(01).
                 88 KEIN-KOMMISSION         VALUE IS "0".
                 88 DEPOT-NS-LIEFERANT      VALUE IS "1".
                 88 DEPOT-NS-LAGER          VALUE IS "3".
                 88 KOMMISSION              VALUE IS "1".
                 88 KOMMISSION-EIGEN        VALUE IS "2".
                 88 FILIALDEPOT-WARE        VALUE IS "4".
             03 VE-EINHEIT-JE-LHM           PIC 9(06).
             03 VE-LAENGE                   PIC 9(04).
             03 VE-BREITE                   PIC 9(04).
             03 VE-HOEHE                    PIC 9(04).
             03 VE-GEWICHT                  PIC 9(07).
             03 AUSZEICHNER-BEPO            PIC X(01).
             03 AUSZEICHNER-PREIS           PIC X(01).
             03 AUSZEICHNER-EAN             PIC X(01).
             03 AUSZEICHNER-PLU             PIC X(01).
             03 KZ-BERLIN-NBG               PIC X(01).
             03 VORGANGSART.
               04 VORGANGSART-DEF           PIC 9(02).
                 88 EXTRABESTELLUNG         VALUE IS 62.
             03 KZ-HERKUNFT                 PIC 9(02).
             03 MENGE-BEST-VE               PIC 9(07)V9(02).
             03 LOT-NR                      PIC X(13).
             03 VK-M-ST-E                   PIC 9(07)V9(02).
             03 EK-E                        PIC 9(07)V9(03).
             03 AP-E                        PIC 9(07)V9(03).
             03 FIL-VK-E                    PIC 9(07)V9(02).
             03 LADESEQUENZ                 PIC X(06).
             03 FILLER                      PIC X(243).
