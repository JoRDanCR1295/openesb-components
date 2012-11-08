      ********************************* Top of Data **********************************
       05  810-EDI-INV-SO-FILE.
      ****************************************************
      ** CBLY810G S/O FRM ST P BY GAG4 01/03/02 15:10:07 *
      ** REQUEST NUMBER: 428CR501 (SINGLE)               *
      *****************************************************
      *** TOTAL FILE LENGTH: 300                          *
      ***        KEY FIELDS:  30                          *
      *****************************************************
      ***  TOTAL REC TYPE 1: 270                          *
      ***       DATA FIELDS: 184                          *
      ***            FILLER:  86                          *
      *****************************************************
      ***  TOTAL REC TYPE 2: 270                          *
      ***       DATA FIELDS:  30                          *
      ***            FILLER: 240                          *
      *****************************************************
      ***  TOTAL REC TYPE 3: 270                          *
      ***       DATA FIELDS: 167                          *
      ***            FILLER: 103                          *
      ******************************************************
            10 810-EDI-INV-H        PIC X(22).
            10 810-FILLER-06        PIC 9(6).
            10 810-EDI-REC-TYPE-C   PIC X(02).
                88 810-EDI-REC-TYPE-1-B
                                    VALUE '01'.
                88 810-EDI-REC-TYPE-2-B
                                    VALUE '02'.
                88 810-EDI-REC-TYPE-3-B
                                    VALUE '03'.
            10 810-EDI-REC-TYPE-1-G.
             15 810-INV-CUR-D       PIC X(08).
             15 810-EDI-TRANS-ID-H  PIC X(30).
             15 810-EDI-INV-TRANS-TYPE-C
                                    PIC X(02).
             15 810-EDI-TRANS-PURP-C
                                    PIC X(02).
             15 810-REP-TOT-INV-A   PIC -------------V99.
             15 810-CAN-TRANS-ORIG-INV-H
                                    PIC X(22).
             15 810-EDI-ESI-ID-H    PIC X(17).
             15 810-REP-LONG-N      PIC X(60).
             15 810-REP-ID-G.
              20 810-REP-ID-H       PIC X(09).
              20 810-REP-ID-4-H     PIC X(04).
             15 810-INV-PYMT-DUE-D  PIC X(08).
             15 810-DIST-DOE-C      PIC X(01).
                88 810-TXU-DOE-C-B  VALUE 'T'.
                88 810-SESCO-DOE-C-B
                                    VALUE 'S'.
             15 810-DIST-DUNS-C     PIC X(01).
                88 810-DIST-DUNS-C  VALUE 'S'.
                88 810-TXU-DUNS-C-B VALUE 'T'.
                88 810-SESCO-DUNS-C-B
                                    VALUE 'S'.
             15 810-CODE-VER-H      PIC X(04).
             15 810-PRTD-NONMTR-F   PIC X(1).
             15 810-FILLER-86       PIC X(86).
            10 810-EDI-REC-TYPE-2-G REDEFINES 810-EDI-REC-TYPE-1-G.
             15 810-PROD-SERV-ID-C  PIC X(10).
                88 810-PROD-SERV-ID-ACCT-B
                                    VALUE 'ACCOUNT '.
                88 810-PROD-SERV-ID-RATE-B
                                    VALUE 'RATE   '.
                88 810-PROD-SERV-ID-B2B-B
                                    VALUE 'B2B    '.
             15 810-DISCO-RATE-SUB-CLS-C
                                    PIC X(2).
             15 810-DISCO-RATE-CLASS-C
                                    PIC 99.
                88 810-DISCO-RATE-CLASS-1-B
                                    VALUE 01.
                88 810-DISCO-RATE-CLASS-2-B
                                    VALUE 02.
                88 810-DISCO-RATE-CLASS-3-B
                                    VALUE 03.
                88 810-DISCO-RATE-CLASS-4-B
                                    VALUE 04.
                88 810-DISCO-RATE-CLASS-5-B
                                    VALUE 05.
                88 810-DISCO-RATE-CLASS-6-B
                                    VALUE 06.
                88 810-DISCO-RATE-RES-B
                                    VALUE 00 01 02 03.
                88 810-DISCO-RATE-GD-LT-B
                                    VALUE 62 63 64 65 66 67 68 69 70 71
                   72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89
                   90 91 92 93 94 95 96 97.
                88 810-DISCO-RATE-GEN-SERV-B
                                    VALUE 08 09 10 11 14 15 16 17 22 24
                   26 28.
                88 810-DISCO-RATE-MUN-B
                                    VALUE 08 14 53 54 56 59.
                88 810-DISCO-RATE-OTH-B
                                    VALUE 08 09 41 51 83 95.
                88 810-DISCO-RATE-IDR-B
                                    VALUE 21 23 25 27 31 33 35 37 41 42
                   43 44.
                88 810-DISCO-RATE-LTG-SERV-B
                                    VALUE 57 60.
             15 810-SERV-PER-END-D  PIC X(08).
             15 810-SERV-PER-BEG-D  PIC X(08).
             15 810-FILLER-240      PIC X(240).
            10 810-EDI-REC-TYPE-3-G REDEFINES 810-EDI-REC-TYPE-1-G.
             15 810-SERV-ORD-H      PIC X(30).
             15 810-DISCO-CSO-COMPL-D
                                    PIC X(08).
             15 810-CHRG-ID-C       PIC X(01).
             15 810-CHRG-TYPE-C     PIC X(10).
             15 810-EDI-CHRG-A      PIC -------------V99.
             15 810-EDI-CHRG-P      PIC ----------.9(08).
             15 810-EDI-CHRG-P1     PIC ----------.9(08).
             15 810-EDI-CONSM-RATE-C
                                    PIC X(02).
             15 810-EDI-CHRG-Q      PIC ----------.99.
             15 810-SERV-ORD-DESC-T PIC X(35).
             15 810-EDI-TAX-A       PIC ---------------.99.
             15 810-EDI-DELQ-INV-H  PIC X(22).
             15 810-SERV-CHRG-GRP-C PIC X(04).
             15 810-LAST-CHRG-REC-C PIC X(01).
             15 810-FILLER-92       PIC X(92).

