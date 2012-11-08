         01 SA160.
           02 IBM-160.
             03 LIEFAK-KEY.
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
               04 LIEFAK-ALT-KEY-1.
                 05 LIEFER-AVIS             PIC X(10).
                 05 DAT-ERSTELLUNG.
                   06 JAHRHUNDERT           PIC 9(02).
                   06 JJMMTT.
                     07 JAHR                PIC 9(02).
                     07 MONAT               PIC 9(02).
                     07 TAG                 PIC 9(02).
             03 LIEFAK-ALT-KEY-2.
               04 LIEF-KURZ-BEZ             PIC X(15).
             03 V-UN-BST-ABT.
               04 UN-BST.
                 05 UN                      PIC 9(02).
                 05 BST                     PIC 9(03).
               04 ABT                       PIC 9(03).
             03 AUFTRAGSNR                  PIC X(12).
             03 SAMMELRECHNUNGS-NR          PIC X(12).
             03 AVIS-TYP.
               04 KENNZ-AVIS-TYP            PIC X(01).
                 88 CROSSDOCKING-AVIS       VALUE IS "C".
                 88 DEPOT-AVIS              VALUE IS "D".
                 88 LAGERWARE               VALUE IS "D".
                 88 RUECKRUF-AVIS           VALUE IS "R".
                 88 TRANSIT-AVIS            VALUE IS "T".
             03 LIEFERTERMIN.
               04 JAHRHUNDERT               PIC 9(02).
               04 JJMMTT.
                 05 JAHR                    PIC 9(02).
                 05 MONAT                   PIC 9(02).
                 05 TAG                     PIC 9(02).
             03 S-UN-BST-ABT.
               04 UN-BST.
                 05 UN                      PIC 9(02).
                 05 BST                     PIC 9(03).
               04 ABT                       PIC 9(03).
             03 CONTAINER-NR                PIC X(17).
             03 CONTAINER-TYP               PIC X(09).
             03 ANZ-KARTON                  PIC 9(08).
             03 ANZ-VE-CONTAINER            PIC S9(15)     
             SIGN LEADING SEPARATE.
