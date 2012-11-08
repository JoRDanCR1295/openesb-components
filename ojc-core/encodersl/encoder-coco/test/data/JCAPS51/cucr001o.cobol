      * MVS PORT-- CODE RUN THROUGH PROCESSOR
      *            on Sat May 30 01:06:08 1998

      *****************************************************************
      **                                                             **
      **           FOUNDATION FOR COOPERATIVE PROCESSING             **
      **                                                             **
      **                     COBOL  COPYBOOK                         **
      **                                                             **
      **           COPYRIGHT (C) 1996 ANDERSEN CONSULTING.           **
      **                    ALL RIGHTS RESERVED.                     **
      **                                                             **
      *****************************************************************
      **                                                             **
      **      COPYBOOK: CUCR001O                                     **
      **                                                             **
      **  COPYBOOK FOR: Copybook.CUCR001O RETRIEVAL SVC OUTPUT       **
      **                                                             **
      **  GENERATED ON: Thu May 14 17:08:59 1998                     **
      **                                                             **
      **  SHORT DESCRIPTION: CUCR001O RETRIEVAL SVC OUTPUT           **
      **                                                             **
      **            BY: GRWILLIS                                     **
      **                                                             **
      *****************************************************************
       03  MSG-O-CUCR001O.
         05  MSG-O-STANDARD-HEADER.
           07  MSG-O-CD-FUNC-ID                  PIC X(2).
               88  MSG-O-FUNC-01                     VALUE '01'.
               88  MSG-O-FUNC-02                     VALUE '02'.
               88  MSG-O-FUNC-03                     VALUE '03'.
               88  MSG-O-FUNC-04                     VALUE '04'.
               88  MSG-O-FUNC-05                     VALUE '05'.
               88  MSG-O-FUNC-06                     VALUE '06'.
               88  MSG-O-FUNC-07                     VALUE '07'.
               88  MSG-O-FUNC-08                     VALUE '08'.
               88  MSG-O-FUNC-09                     VALUE '09'.
               88  MSG-O-FUNC-10                     VALUE '10'.
               88  MSG-O-FUNC-11                     VALUE '11'.
               88  MSG-O-FUNC-12                     VALUE '12'.
               88  MSG-O-FUNC-13                     VALUE '13'.
               88  MSG-O-FUNC-14                     VALUE '14'.
               88  MSG-O-FUNC-15                     VALUE '15'.
               88  MSG-O-FUNC-16                     VALUE '16'.
               88  MSG-O-FUNC-17                     VALUE '17'.
               88  MSG-O-FUNC-18                     VALUE '18'.
               88  MSG-O-FUNC-19                     VALUE '19'.
               88  MSG-O-FUNC-20                     VALUE '20'.
               88  MSG-O-FUNC-21                     VALUE '21'.
               88  MSG-O-FUNC-22                     VALUE '22'.
               88  MSG-O-FUNC-23                     VALUE '23'.
               88  MSG-O-FUNC-24                     VALUE '24'.
               88  MSG-O-FUNC-25                     VALUE '25'.
               88  MSG-O-FUNC-26                     VALUE '26'.
               88  MSG-O-FUNC-27                     VALUE '27'.
               88  MSG-O-FUNC-28                     VALUE '28'.
               88  MSG-O-FUNC-29                     VALUE '29'.
               88  MSG-O-FUNC-30                     VALUE '30'.
               88  MSG-O-FUNC-31                     VALUE '31'.
               88  MSG-O-FUNC-32                     VALUE '32'.
               88  MSG-O-FUNC-33                     VALUE '33'.
               88  MSG-O-FUNC-34                     VALUE '34'.
               88  MSG-O-FUNC-35                     VALUE '35'.
               88  MSG-O-FUNC-36                     VALUE '36'.
               88  MSG-O-FUNC-37                     VALUE '37'.
               88  MSG-O-FUNC-38                     VALUE '38'.
               88  MSG-O-FUNC-39                     VALUE '39'.
               88  MSG-O-FUNC-40                     VALUE '40'.
               88  MSG-O-FUNC-41                     VALUE '41'.
               88  MSG-O-FUNC-42                     VALUE '42'.
               88  MSG-O-FUNC-43                     VALUE '43'.
               88  MSG-O-FUNC-44                     VALUE '44'.
               88  MSG-O-FUNC-45                     VALUE '45'.
               88  MSG-O-FUNC-46                     VALUE '46'.
               88  MSG-O-FUNC-47                     VALUE '47'.
               88  MSG-O-FUNC-48                     VALUE '48'.
               88  MSG-O-FUNC-49                     VALUE '49'.
               88  MSG-O-FUNC-50                     VALUE '50'.
           07  MSG-O-DT-CURR-DATE                PIC X(10).
           07  MSG-O-KY-CONV-ID                  PIC X(8).
           07  MSG-O-STNDRD-HEAD-SUBGRP.
             09  MSG-O-KY-BA                     PIC S9(10) COMP-3.
             09  MSG-O-KY-CUST-NO                PIC S9(9) COMP.
             09  MSG-O-KY-PREM-NO                PIC S9(9) COMP.
             09  MSG-O-KY-SPT                    PIC S9(8) COMP-3.
             09  MSG-O-KY-FORD-NO                PIC S9(9) COMP-3.
             09  MSG-O-ID-EQUIPMENT              PIC S9(8) COMP.
             09  MSG-O-NO-FUTURE-KEY-X           PIC S9(9) COMP.
             09  MSG-O-NO-FUTURE-KEY-Y           PIC S9(9) COMP.
             09  MSG-O-NO-FUTURE-KEY-Z           PIC S9(9) COMP.
             09  MSG-O-NO-LOCK-SEQ-CUST          PIC S9(2) COMP-3.
             09  MSG-O-NO-LOCK-SEQ-PREM          PIC S9(2) COMP-3.
             09  MSG-O-NO-LOCK-SEQ-BA            PIC S9(2) COMP-3.
             09  MSG-O-NO-LOCK-SEQ-BA-AIM        PIC S9(2) COMP-3.
             09  MSG-O-NO-LOCK-SEQ-SPT           PIC S9(2) COMP-3.
             09  MSG-O-NO-LOCK-SEQ-FO-HDR        PIC S9(2) COMP-3.
             09  MSG-O-NO-LOCK-SEQ-EQUIP         PIC S9(2) COMP.
             09  MSG-O-NO-LOCK-SEQ-X             PIC S9(2) COMP-3.
             09  MSG-O-NO-LOCK-SEQ-Y             PIC S9(2) COMP-3.
             09  MSG-O-NO-LOCK-SEQ-Z             PIC S9(2) COMP-3.
           07  MSG-O-KY-USERID-2                 PIC X(8).
           07  MSG-O-NO-SVC-VERSION              PIC S9(14) COMP.
           07  MSG-O-PERF-SUBGRP-2.
             09  MSG-O-ID-PREV-MSG-INST          PIC X(24).
             09  MSG-O-NO-PREV-RESP-TIME         PIC S9(4)V9(3) COMP-3.
           07  MSG-O-CD-STD-MSG-HDR-FLR          PIC X(20).
         05  MSG-O-RETR-KEYS-1.
           07  MSG-O-CD-MEGA-SCROLL-TYPE         PIC X(4).
           07  MSG-O-CD-ACS-CMD                  PIC X(2).
               88  MSG-O-FETCH                       VALUE 'F '.
               88  MSG-O-FETCH-NEXT                  VALUE 'FN'.
               88  MSG-O-FETCH-PREVIOUS              VALUE 'FP'.
               88  MSG-O-GET                         VALUE 'G '.
               88  MSG-O-GET-NEXT                    VALUE 'GN'.
               88  MSG-O-INSERTED                    VALUE 'I '.
               88  MSG-O-UPDATE                      VALUE 'U '.
               88  MSG-O-DELETED                     VALUE 'D '.
               88  MSG-O-RENAME                      VALUE 'R '.
               88  MSG-O-UPDATE-SET                  VALUE 'US'.
               88  MSG-O-DELETE-SET                  VALUE 'DS'.
               88  MSG-O-SUMM                        VALUE 'SU'.
               88  MSG-O-INSERT-SET                  VALUE 'IS'.
               88  MSG-O-COUNT                       VALUE 'C '.
               88  MSG-O-OPENED                      VALUE 'OP'.
               88  MSG-O-CLOSED                      VALUE 'CL'.
               88  MSG-O-CHECKPOINT                  VALUE 'CK'.
               88  MSG-O-NEXT-PREVIOUS               VALUE 'NP'.
               88  MSG-O-MAX                         VALUE 'MX'.
               88  MSG-O-MIN                         VALUE 'MN'.
               88  MSG-O-DACP-EXIST-CHK              VALUE 'EX'.
           07  MSG-O-FL-PAGE                     PIC S9(1) COMP-3.
           07  MSG-O-CD-RETRIEVAL-TYPE           PIC X(2).
               88  MSG-O-RETR-BA-AND-PRIM-CUST       VALUE 'BP'.
               88  MSG-O-RETR-PREM-NO                VALUE 'PR'.
               88  MSG-O-RETR-CUST-NO                VALUE 'CU'.
               88  MSG-O-RETR-SITE-NO                VALUE 'SN'.
               88  MSG-O-RETR-BLDG-NO                VALUE 'BU'.
               88  MSG-O-RETR-CUST-NO-AND-PREM-NO    VALUE 'CP'.
               88  MSG-O-RETR-RET-ONE-SITE           VALUE '1S'.
               88  MSG-O-RETR-RET-MULT-SITES         VALUE 'MS'.
               88  MSG-O-RETR-SAD-MULT-CRIT          VALUE 'SC'.
               88  MSG-O-RETR-RET-ONE-BLDG           VALUE '1B'.
               88  MSG-O-RETR-GAK                    VALUE 'GA'.
               88  MSG-O-RETR-GNK                    VALUE 'GN'.
               88  MSG-O-RETR-BA                     VALUE 'BA'.
               88  MSG-O-RETR-SSN                    VALUE 'SS'.
               88  MSG-O-RETR-OLD-ACCT               VALUE 'OA'.
               88  MSG-O-RETR-PHONE                  VALUE 'PH'.
               88  MSG-O-RETR-SITE-GNK               VALUE 'SK'.
           07  MSG-O-AD-COMPRESSED               PIC X(69).
           07  MSG-O-NM-COMPRESSED               PIC X(56).
           07  MSG-O-FL-CUST-NM-SOUNDEX          PIC X(1).
               88  MSG-O-CUST-NM-SOUNDEX             VALUE 'Y'.
               88  MSG-O-NOT-CUST-NM-SNDX            VALUE 'N'.
           07  MSG-O-KY-BA                       PIC S9(10) COMP-3.
           07  MSG-O-KY-SSN                      PIC S9(9) COMP.
           07  MSG-O-KY-OLD-ACCNT-NO             PIC X(16).
           07  MSG-O-TX-HOME-ACD                 PIC X(3).
           07  MSG-O-TX-HOME-PHN-NO              PIC X(8).
           07  MSG-O-KY-MTR-EQUIP-NO             PIC X(9).
           07  MSG-O-AD-SERV-CITY                PIC X(19).
           07  MSG-O-CD-COUNTY                   PIC X(3).
           07  MSG-O-AD-SERV-ZIP                 PIC S9(5) COMP-3.
           07  MSG-O-FL-ACTV-ACCTS-ONLY          PIC X(1).
               88  MSG-O-ACTV-ACCTS-ONLY             VALUE 'Y'.
               88  MSG-O-NO-ACTV-ACCTS-ONLY          VALUE 'N'.
           07  MSG-O-CD-CO                       PIC S9(4) COMP.
           07  MSG-O-CD-REGION-GROUP
                            OCCURS 4 TIMES.
             09  MSG-O-CD-REGION                 PIC X(3).
           07  MSG-O-CD-OPER-CNTR                PIC X(3).
               88  MSG-O-GEN-OFFICE                  VALUE 'GEN'.
           07  MSG-O-FL-SYSTEM-WIDE              PIC X(1).
               88  MSG-O-SYSTEM-WIDE                 VALUE 'Y'.
               88  MSG-O-NOT-SYSTEM-WIDE             VALUE 'N'.
           07  MSG-O-FL-SHOW-ACCT-MAIL           PIC X(1).
               88  MSG-O-SHOW-ACCT-MAIL              VALUE 'Y'.
               88  MSG-O-NOT-SHOW-ACCT-MAIL          VALUE 'N'.
           07  MSG-O-KY-CUST-NO                  PIC S9(9) COMP.
           07  MSG-O-KY-PREM-NO                  PIC S9(9) COMP.
           07  MSG-O-KY-BLDG-NO                  PIC S9(9) COMP.
           07  MSG-O-KY-SITE-NO                  PIC S9(9) COMP.
           07  MSG-O-KY-GEN-AD-NUM-KEYS
                            OCCURS 12 TIMES.
             09  MSG-O-KY-GEN-AD-NUMERIC         PIC S9(2) COMP.
           07  MSG-O-KY-GEN-NM.
             09  MSG-O-KY-GEN-NM-1               PIC X(8).
             09  MSG-O-KY-GEN-NM-2               PIC X(2).
           07  MSG-O-FL-USE-CRITERIA             PIC X(1).
               88  MSG-O-USE-CRITERIA                VALUE 'Y'.
               88  MSG-O-NOT-USE-CRITERIA            VALUE 'N'.
           07  MSG-O-RETR-WILD-CRITERIA.
             09  MSG-O-NM-CUST-1ST-1             PIC X(15).
             09  MSG-O-NM-CUST-MID-1             PIC X(15).
             09  MSG-O-NM-CUST-LST-1             PIC X(20).
             09  MSG-O-NM-CUST-2                 PIC X(28).
             09  MSG-O-AD-SERV-STR-NO            PIC X(15).
             09  MSG-O-AD-SERV-CDL-DIR           PIC X(2).
             09  MSG-O-AD-SERV-STR-NM            PIC X(23).
             09  MSG-O-AD-SERV-STR-SFIX          PIC X(4).
             09  MSG-O-AD-SERV-SFIX              PIC X(2).
               88  MSG-O-N                         VALUE 'N'.
               88  MSG-O-S                         VALUE 'S'.
               88  MSG-O-E                         VALUE 'E'.
               88  MSG-O-W                         VALUE 'W'.
               88  MSG-O-NE                        VALUE 'NE'.
               88  MSG-O-NW                        VALUE 'NW'.
               88  MSG-O-SE                        VALUE 'SE'.
               88  MSG-O-SW                        VALUE 'SW'.
               88  MSG-O-BLANKS                    VALUE '  '.
               88  MSG-O-VALID-VALUES VALUE 'N','S','E','W','NE','NW',
                   'SE','SW'.
             09  MSG-O-AD-SERV-STRUC             PIC X(15).
           07  MSG-O-RETR-WILD-KEYS.
             09  MSG-O-NM-CUST-2                 PIC X(28).
             09  MSG-O-AD-SERV-STR-NM            PIC X(23).
           07  MSG-O-RETR-NAME-KEYS.
             09  MSG-O-NM-CUST-1ST-1             PIC X(15).
             09  MSG-O-NM-CUST-MID-1             PIC X(15).
             09  MSG-O-NM-CUST-LST-1             PIC X(20).
             09  MSG-O-NM-CUST-TTL-1             PIC X(8).
             09  MSG-O-NM-CUST-SFIX-1            PIC X(4).
         05  MSG-O-K-CU04TB57.
           07  MSG-O-K457-PKEY-BUILDING.
             09  MSG-O-KY-BLDG-NO                PIC S9(9) COMP.
         05  MSG-O-CUR01K06.
           07  MSG-O-PKEY-SITE.
             09  MSG-O-KY-SITE-NO                PIC S9(9) COMP.
         05  MSG-O-SAD-TABLE-MAIN-GRP.
           07  MSG-O-KEY-SAD-TABLE-GROUP.
             09  MSG-O-KY-BA                     PIC S9(10) COMP-3.
             09  MSG-O-KY-CUST-NO                PIC S9(9) COMP.
             09  MSG-O-KY-PREM-NO                PIC S9(9) COMP.
             09  MSG-O-CD-ACCT-TYPE              PIC X(2).
               88  MSG-O-PEND-SERV                 VALUE '03'.
               88  MSG-O-DEFAULTED                 VALUE '  '.
               88  MSG-O-PEND-SERV-OTHER-ACT       VALUE '06'.
               88  MSG-O-PEND-FINAL-ELE            VALUE '12'.
               88  MSG-O-PEND-FINAL-ELE-ACT        VALUE '21'.
               88  MSG-O-FINAL-SERV-ACT            VALUE '42'.
               88  MSG-O-DMAG                      VALUE '48'.
               88  MSG-O-SUM-BILL                  VALUE '51'.
               88  MSG-O-PAL                       VALUE '54'.
               88  MSG-O-SL                        VALUE '57'.
               88  MSG-O-NONCONSUMER               VALUE '60'.
               88  MSG-O-CASH                      VALUE '63'.
               88  MSG-O-DEBITS                    VALUE '66'.
               88  MSG-O-UNCOLL-CASH               VALUE '69'.
               88  MSG-O-SERV-TYPE VALUE '12','21','18','33','42','09',
                   '15','24','27','30','36','39','54'.
               88  MSG-O-SERV-PEND                 VALUE '03', '06'.
               88  MSG-O-NON-SERV VALUE '45','48','60','72'.
               88  MSG-O-TAX-ONLY                  VALUE '72'.
           07  MSG-O-SAD-TABLE-SUB-GROUP.
             09  MSG-O-NM-CUST-1ST-1             PIC X(15).
             09  MSG-O-NM-CUST-MID-1             PIC X(15).
             09  MSG-O-NM-CUST-LST-1             PIC X(20).
             09  MSG-O-KY-SSN                    PIC S9(9) COMP.
             09  MSG-O-NM-CUST-TTL-1             PIC X(8).
             09  MSG-O-NM-CUST-SFIX-1            PIC X(4).
             09  MSG-O-KY-GRID-NO                PIC S9(14) COMP-3.
             09  MSG-O-AD-SERV-CITY              PIC X(19).
             09  MSG-O-AD-SERV-ST                PIC X(2).
             09  MSG-O-AD-SERV-ZIP               PIC S9(5) COMP-3.
             09  MSG-O-CD-GAS-MTR-SUM            PIC X(3).
             09  MSG-O-CD-ELE-MTR-SUM            PIC X(3).
             09  MSG-O-KY-OLD-ACCT-NO-1          PIC X(16).
             09  MSG-O-KY-GENER-AD-1             PIC X(7).
             09  MSG-O-KY-GENER-AD-2             PIC X(5).
             09  MSG-O-KY-GEN-NM-GRP.
               11  MSG-O-KY-GEN-NM-1             PIC X(8).
               11  MSG-O-KY-GEN-NM-2             PIC X(2).
             09  MSG-O-CD-SAD-STAT               PIC X(2).
               88  MSG-O-ACTIVE                    VALUE '03'.
               88  MSG-O-INACTIVE                  VALUE '06'.
               88  MSG-O-PEND-PREM                 VALUE '08'.
               88  MSG-O-PENDING                   VALUE '09'.
               88  MSG-O-VOID                      VALUE '12'.
               88  MSG-O-FINALED                   VALUE '15'.
               88  MSG-O-UNCOLL                    VALUE '18'.
               88  MSG-O-RO                        VALUE '21'.
               88  MSG-O-RECONC                    VALUE '24'.
               88  MSG-O-UNRECONC                  VALUE '27'.
               88  MSG-O-ESCHEAT                   VALUE '30'.
             09  MSG-O-NM-COMPRESSED             PIC X(56).
             09  MSG-O-AD-COMPRESSED             PIC X(69).
             09  MSG-O-CD-SUM-EXIST              PIC X(1).
               88  MSG-O-DEL                       VALUE 'D'.
               88  MSG-O-EXIST                     VALUE 'E'.
               88  MSG-O-CONV                      VALUE 'C'.
             09  MSG-O-CD-CIS-EXIST              PIC X(1).
               88  MSG-O-DOES-NOT-EXIST            VALUE 'D'.
               88  MSG-O-EXIST                     VALUE 'E'.
             09  MSG-O-CD-ACS-EXIST              PIC X(1).
               88  MSG-O-DOES-NOT-EXIST            VALUE 'D'.
               88  MSG-O-EXISTS                    VALUE 'E'.
             09  MSG-O-CD-NF-EXIST               PIC X(1).
             09  MSG-O-QY-LNGH-AD                PIC S9(2) COMP.
             09  MSG-O-KY-MTR-BILL-GRP           PIC S9(2) COMP-3.
             09  MSG-O-CD-CUST-INDIC             PIC S9(1) COMP-3.
               88  MSG-O-NO-CUST                   VALUE 0.
               88  MSG-O-PRIM                      VALUE 1.
               88  MSG-O-2ND                       VALUE 2.
             09  MSG-O-CD-ICS-EXIST              PIC X(1).
               88  MSG-O-EXIST                     VALUE 'E'.
               88  MSG-O-DOES-NOT-EXIST            VALUE 'D'.
             09  MSG-O-TX-HOME-ACD               PIC X(3).
             09  MSG-O-TX-HOME-PHN-NO            PIC X(8).
             09  MSG-O-TX-HOME-PHN-EXTN          PIC X(4).
             09  MSG-O-TX-BUS-ACD                PIC X(3).
             09  MSG-O-TX-BUS-PHN-NO             PIC X(8).
             09  MSG-O-TX-BUS-PHN-EXTN           PIC X(4).
             09  MSG-O-CD-COUNTY                 PIC X(3).
             09  MSG-O-CD-REGION                 PIC X(3).
             09  MSG-O-CD-OPER-CNTR              PIC X(3).
               88  MSG-O-GEN-OFFICE                VALUE 'GEN'.
             09  MSG-O-FL-CRITICAL-CONTCT        PIC X(1).
               88  MSG-O-CRITICAL                  VALUE 'Y'.
               88  MSG-O-NON-CRITICAL              VALUE 'N'.
             09  MSG-O-KY-AD                     PIC S9(9) COMP.
             09  MSG-O-CD-AD-TYPE                PIC X(1).
               88  MSG-O-PREM                      VALUE 'P'.
               88  MSG-O-BLDG                      VALUE 'B'.
               88  MSG-O-3RD-PARTY                 VALUE 'T'.
               88  MSG-O-MLNG-AD                   VALUE 'M'.
               88  MSG-O-GUAR                      VALUE 'G'.
               88  MSG-O-RFND                      VALUE 'R'.
               88  MSG-O-AGENCY                    VALUE 'A'.
               88  MSG-O-TEMPORARY                 VALUE 'Y'.
               88  MSG-O-CUSTOMER-88               VALUE 'C'.
               88  MSG-O-LANDLORD                  VALUE 'L'.
             09  MSG-O-KY-BLDG-NO                PIC S9(9) COMP.
             09  MSG-O-DT-STAT                   PIC X(10).
         05  MSG-O-CUSTOMER-GRP.
           07  MSG-O-NO-LOCK-SEQ-CUST            PIC S9(2) COMP-3.
           07  MSG-O-CD-CO                       PIC S9(4) COMP.
           07  MSG-O-KY-SSN                      PIC S9(9) COMP.
           07  MSG-O-TX-HOME-ACD                 PIC X(3).
           07  MSG-O-TX-HOME-PHN-NO              PIC X(8).
           07  MSG-O-TX-HOME-PHN-EXTN            PIC X(4).
           07  MSG-O-FL-UNLST-PHN-NO             PIC X(1).
           07  MSG-O-TX-BUS-ACD                  PIC X(3).
           07  MSG-O-TX-BUS-PHN-NO               PIC X(8).
           07  MSG-O-TX-BUS-PHN-EXTN             PIC X(4).
           07  MSG-O-FL-UNLST-BUS-PHN            PIC X(1).
           07  MSG-O-CD-CUST-STAT                PIC X(2).
               88  MSG-O-ACT                         VALUE '01'.
               88  MSG-O-PEND-ACT                    VALUE '02'.
               88  MSG-O-FINALED                     VALUE '03'.
           07  MSG-O-DT-CUST-STAT                PIC X(10).
           07  MSG-O-NM-CUST-1                   PIC X(28).
           07  MSG-O-NM-CUST-2                   PIC X(28).
           07  MSG-O-FL-QTE-DEP                  PIC X(1).
           07  MSG-O-KY-CNTL-UNIT                PIC S9(2) COMP-3.
           07  MSG-O-AT-HGST-CR-APPR             PIC S9(7)V9(2) COMP-3.
           07  MSG-O-CD-CUST-TYPE                PIC X(1).
               88  MSG-O-INDIVIDUAL                  VALUE 'I'.
               88  MSG-O-CORPORATION                 VALUE 'C'.
               88  MSG-O-NON-PROFIT                  VALUE 'N'.
               88  MSG-O-GOVT-MUNICIPAL              VALUE 'M'.
               88  MSG-O-GOVT-COUNTY                 VALUE 'Y'.
               88  MSG-O-GOVT-ST                     VALUE 'S'.
               88  MSG-O-GOVT-FED                    VALUE 'F'.
               88  MSG-O-FPC                         VALUE 'P'.
               88  MSG-O-OTHER-WHOLESALE             VALUE 'W'.
           07  MSG-O-CD-CITIZENSHIP              PIC X(1).
           07  MSG-O-CD-DO-NOT-QTE               PIC X(1).
               88  MSG-O-BUS-RSN                     VALUE '1'.
               88  MSG-O-HARDSHIP-CASE               VALUE '2'.
               88  MSG-O-HEALTH-RSN                  VALUE '3'.
               88  MSG-O-MED-FACIL                   VALUE '4'.
               88  MSG-O-CRT-ORD                     VALUE '5'.
               88  MSG-O-MGMT-DECISION               VALUE '6'.
               88  MSG-O-BUILD-REALTOR               VALUE '7'.
               88  MSG-O-ANY-VALID-CODE VALUE '1','2','3','4','5','6',
                   '7'.
           07  MSG-O-DT-CR-RPT-SECUR             PIC X(10).
           07  MSG-O-DT-MDSE-DECL                PIC X(10).
           07  MSG-O-DT-NOT-RESP-NTC             PIC X(10).
           07  MSG-O-CD-ORIG-ACT-AS              PIC X(1).
               88  MSG-O-1ST-TM-USER                 VALUE 'F'.
               88  MSG-O-USED-BEF                    VALUE 'U'.
               88  MSG-O-DEFAULT-VALU                VALUE ' '.
           07  MSG-O-DT-ORIG-MADE-ACT            PIC X(10).
           07  MSG-O-FL-SSN-NOT-PROV             PIC X(1).
           07  MSG-O-KY-AD                       PIC S9(9) COMP.
           07  MSG-O-CD-AD-TYPE                  PIC X(1).
               88  MSG-O-PREM                        VALUE 'P'.
               88  MSG-O-BLDG                        VALUE 'B'.
               88  MSG-O-3RD-PARTY                   VALUE 'T'.
               88  MSG-O-MLNG-AD                     VALUE 'M'.
               88  MSG-O-GUAR                        VALUE 'G'.
               88  MSG-O-RFND                        VALUE 'R'.
               88  MSG-O-AGENCY                      VALUE 'A'.
               88  MSG-O-TEMPORARY                   VALUE 'Y'.
               88  MSG-O-CUSTOMER-88                 VALUE 'C'.
               88  MSG-O-LANDLORD                    VALUE 'L'.
           07  MSG-O-CD-STAT                     PIC S9(2) COMP-3.
               88  MSG-O-INACT                       VALUE 01.
               88  MSG-O-ACT                         VALUE 02.
               88  MSG-O-PEND-ACT                    VALUE 03.
               88  MSG-O-GAS-ACT                     VALUE 04.
               88  MSG-O-ELECTRIC-ACT                VALUE 05.
               88  MSG-O-REMV                        VALUE 06.
               88  MSG-O-VOID                        VALUE 07.
               88  MSG-O-UNCOLL                      VALUE 08.
               88  MSG-O-FINALED                     VALUE 09.
               88  MSG-O-PEND-TRNSFR                 VALUE 10.
               88  MSG-O-TRNSFR                      VALUE 11.
               88  MSG-O-CNCLD                       VALUE 12.
               88  MSG-O-RECONCILE                   VALUE 20.
               88  MSG-O-RECONCILE-INSERT            VALUE 21.
               88  MSG-O-UNRECONCILE                 VALUE 22.
               88  MSG-O-DELETED                     VALUE 99.
           07  MSG-O-DT-STAT                     PIC X(10).
         05  MSG-O-BUILDING-GRP.
           07  MSG-O-NM-BUILDING                 PIC X(30).
           07  MSG-O-CD-BLDG-TYPE                PIC X(2).
               88  MSG-O-SINGLE-FAMILY               VALUE '01'.
               88  MSG-O-MULTI-UNIT-RESIDENTIAL      VALUE '02'.
               88  MSG-O-MOBILE-HOME                 VALUE '03'.
               88  MSG-O-SHOPPING-CENTER             VALUE '04'.
               88  MSG-O-WAREHOUSE                   VALUE '05'.
               88  MSG-O-FACTORY                     VALUE '06'.
               88  MSG-O-HOSPITAL                    VALUE '07'.
               88  MSG-O-SCHOOL                      VALUE '08'.
               88  MSG-O-OTHER-COMMERCIAL            VALUE '09'.
               88  MSG-O-NURSING-HOME                VALUE '10'.
               88  MSG-O-TEMPORARY-SERVICE           VALUE '11'.
               88  MSG-O-GOVERNMENTAL                VALUE '12'.
           07  MSG-O-FL-MULT-PREMISE             PIC X(1).
               88  MSG-O-FL-MULT-PREMISE-Y           VALUE 'Y'.
               88  MSG-O-FL-MULT-PREMISE-N           VALUE 'N'.
           07  MSG-O-CD-CITY-CNTY                PIC X(4).
           07  MSG-O-CD-OPER-CNTR                PIC X(3).
               88  MSG-O-GEN-OFFICE                  VALUE 'GEN'.
           07  MSG-O-KY-GWA                      PIC X(5).
           07  MSG-O-NO-LOT                      PIC X(20).
           07  MSG-O-NO-BLOCK                    PIC X(20).
           07  MSG-O-KY-STR-NM                   PIC S9(7) COMP-3.
           07  MSG-O-AD-SERV-STR-NO              PIC X(15).
           07  MSG-O-AD-SERV-CITY                PIC X(19).
           07  MSG-O-AD-SERV-ST                  PIC X(2).
           07  MSG-O-AD-NOTE                     PIC X(20).
           07  MSG-O-CD-STAR-RATING              PIC X(1).
           07  MSG-O-NM-CONTACT                  PIC X(20).
           07  MSG-O-TX-CNTC-ACD                 PIC X(3).
           07  MSG-O-TX-CNTC-PHN-NO              PIC X(8).
           07  MSG-O-TX-CNTC-EXTN                PIC X(4).
           07  MSG-O-KY-MRDG-ASGNMT              PIC S9(3) COMP-3.
           07  MSG-O-KY-MTR-BILL-GRP             PIC S9(2) COMP-3.
           07  MSG-O-KY-RMRK-BLDG-DIR            PIC S9(9) COMP.
           07  MSG-O-KY-CNTL-UNIT                PIC S9(2) COMP-3.
           07  MSG-O-CD-BLDG-STAT                PIC X(2).
               88  MSG-O-BLDG-INACT                  VALUE '01'.
               88  MSG-O-BLDG-ACT                    VALUE '02'.
               88  MSG-O-BLDG-PEND-ACT               VALUE '03'.
               88  MSG-O-BLDG-VOID                   VALUE '07'.
               88  MSG-O-BLDG-PENDING                VALUE '08'.
               88  MSG-O-BLDG-DELETE-CONV-ONLY       VALUE '09'.
           07  MSG-O-CD-REPORT-LEVEL             PIC X(5).
               88  MSG-O-RPT-LVL-APK                 VALUE 'APK'.
               88  MSG-O-GEN-OFFICE                  VALUE 'GEN'.
               88  MSG-O-RPT-LVL-APL                 VALUE 'APL'.
               88  MSG-O-RPT-LVL-AVP                 VALUE 'AVP'.
               88  MSG-O-RPT-LVL-BKS                 VALUE 'BKS'.
               88  MSG-O-RPT-LVL-BLG                 VALUE 'BLG'.
               88  MSG-O-RPT-LVL-BNV                 VALUE 'BNV'.
               88  MSG-O-RPT-LVL-CAR                 VALUE 'CAR'.
               88  MSG-O-RPT-LVL-CLR                 VALUE 'CLR'.
               88  MSG-O-RPT-LVL-CLW                 VALUE 'CLW'.
               88  MSG-O-RPT-LVL-CRC                 VALUE 'CRC'.
               88  MSG-O-RPT-LVL-CRV                 VALUE 'CRV'.
               88  MSG-O-RPT-LVL-CRW                 VALUE 'CRW'.
               88  MSG-O-RPT-LVL-DEL                 VALUE 'DEL'.
               88  MSG-O-RPT-LVL-DUN                 VALUE 'DUN'.
               88  MSG-O-RPT-LVL-EUS                 VALUE 'EUS'.
               88  MSG-O-RPT-LVL-FRP                 VALUE 'FRP'.
               88  MSG-O-RPT-LVL-HNC                 VALUE 'HNC'.
               88  MSG-O-RPT-LVL-HSP                 VALUE 'HSP'.
               88  MSG-O-RPT-LVL-INV                 VALUE 'INV'.
               88  MSG-O-RPT-LVL-JAM                 VALUE 'JAM'.
               88  MSG-O-RPT-LVL-JAS                 VALUE 'JAS'.
               88  MSG-O-RPT-LVL-LKP                 VALUE 'LKP'.
               88  MSG-O-RPT-LVL-LKW                 VALUE 'LKW'.
               88  MSG-O-RPT-LVL-MAD                 VALUE 'MAD'.
               88  MSG-O-RPT-LVL-MON                 VALUE 'MON'.
               88  MSG-O-RPT-LVL-OKL                 VALUE 'OKL'.
               88  MSG-O-RPT-LVL-PRY                 VALUE 'PRY'.
               88  MSG-O-RPT-LVL-PSJ                 VALUE 'PSJ'.
               88  MSG-O-RPT-LVL-RED                 VALUE 'RED'.
               88  MSG-O-RPT-LVL-STP                 VALUE 'STP'.
               88  MSG-O-RPT-LVL-TRN                 VALUE 'TRN'.
               88  MSG-O-RPT-LVL-TRP                 VALUE 'TRP'.
               88  MSG-O-RPT-LVL-WAL                 VALUE 'WAL'.
               88  MSG-O-RPT-LVL-WLD                 VALUE 'WLD'.
               88  MSG-O-RPT-LVL-ZEP                 VALUE 'ZEP'.
           07  MSG-O-CD-BTU-ZONE                 PIC X(2).
         05  MSG-O-PREMISE-GRP.
           07  MSG-O-NO-LOCK-SEQ-PREM            PIC S9(2) COMP-3.
           07  MSG-O-KY-BLDG-NO                  PIC S9(9) COMP.
           07  MSG-O-CD-CO                       PIC S9(4) COMP.
           07  MSG-O-KY-MRDG-ASGNMT              PIC S9(3) COMP-3.
           07  MSG-O-KY-MRDG-SEQ-NO              PIC S9(4) COMP.
           07  MSG-O-CD-SIC                      PIC X(6).
           07  MSG-O-CD-PREM-STAT                PIC X(2).
               88  MSG-O-PREM-INACT                  VALUE '01'.
               88  MSG-O-PREM-ACT                    VALUE '02'.
               88  MSG-O-PREM-PEND-ACT               VALUE '03'.
               88  MSG-O-PREM-VOID                   VALUE '07'.
               88  MSG-O-PREM-PENDING                VALUE '08'.
               88  MSG-O-PREM-DELETE-CONV-ONLY       VALUE '09'.
           07  MSG-O-KY-GRID-NO                  PIC S9(14) COMP-3.
           07  MSG-O-AD-SERV-STR-NO              PIC X(15).
           07  MSG-O-AD-SERV-STRUC               PIC X(15).
           07  MSG-O-AD-NOTE                     PIC X(20).
           07  MSG-O-DT-MTR-GRP-EFF              PIC X(10).
           07  MSG-O-KY-MTR-BILL-GRP             PIC S9(2) COMP-3.
           07  MSG-O-NO-KY                       PIC S9(5) COMP-3.
           07  MSG-O-KY-STR-NM                   PIC S9(7) COMP-3.
           07  MSG-O-KY-CNTL-UNIT                PIC S9(2) COMP-3.
           07  MSG-O-QY-CONSC-KY-NO-USE          PIC S9(2) COMP-3.
           07  MSG-O-CD-PREM-ENTR                PIC X(1).
           07  MSG-O-CD-PREM-TYPE                PIC X(2).
               88  MSG-O-SINGLE-FAMILY               VALUE '01'.
               88  MSG-O-APARTMENT                   VALUE '02'.
               88  MSG-O-CONDOMINIUM                 VALUE '03'.
               88  MSG-O-TOWNHOUSE                   VALUE '04'.
               88  MSG-O-MOBILE-HOME                 VALUE '05'.
               88  MSG-O-OFFICE                      VALUE '06'.
               88  MSG-O-HOSPITAL                    VALUE '07'.
               88  MSG-O-SCHOOL                      VALUE '08'.
               88  MSG-O-CHURCH                      VALUE '09'.
               88  MSG-O-COMMON-USE-FACILITIES       VALUE '10'.
               88  MSG-O-TEMPORARY-SERVICE           VALUE '11'.
               88  MSG-O-UNMETERED                   VALUE '12'.
               88  MSG-O-OTHER-TYPE                  VALUE '13'.
               88  MSG-O-NON-SERVICE                 VALUE '14'.
               88  MSG-O-SUBORDINATE                 VALUE '15'.
               88  MSG-O-RETAIL                      VALUE '16'.
               88  MSG-O-MANUFACTURING               VALUE '17'.
               88  MSG-O-REGULAR VALUE '01','02','03','04','05','06',
                   '07','08','09','10','11','13'.
               88  MSG-O-WAREHOUSE                   VALUE '18'.
           07  MSG-O-DT-PREM-STAT                PIC X(10).
           07  MSG-O-KY-RMRK-PREM-DIR            PIC S9(9) COMP.
           07  MSG-O-DT-FIRST-CONNECT            PIC X(10).
           07  MSG-O-CD-OPER-CNTR                PIC X(3).
               88  MSG-O-GEN-OFFICE                  VALUE 'GEN'.
           07  MSG-O-CD-REPORT-LEVEL             PIC X(5).
               88  MSG-O-RPT-LVL-APK                 VALUE 'APK'.
               88  MSG-O-GEN-OFFICE                  VALUE 'GEN'.
               88  MSG-O-RPT-LVL-APL                 VALUE 'APL'.
               88  MSG-O-RPT-LVL-AVP                 VALUE 'AVP'.
               88  MSG-O-RPT-LVL-BKS                 VALUE 'BKS'.
               88  MSG-O-RPT-LVL-BLG                 VALUE 'BLG'.
               88  MSG-O-RPT-LVL-BNV                 VALUE 'BNV'.
               88  MSG-O-RPT-LVL-CAR                 VALUE 'CAR'.
               88  MSG-O-RPT-LVL-CLR                 VALUE 'CLR'.
               88  MSG-O-RPT-LVL-CLW                 VALUE 'CLW'.
               88  MSG-O-RPT-LVL-CRC                 VALUE 'CRC'.
               88  MSG-O-RPT-LVL-CRV                 VALUE 'CRV'.
               88  MSG-O-RPT-LVL-CRW                 VALUE 'CRW'.
               88  MSG-O-RPT-LVL-DEL                 VALUE 'DEL'.
               88  MSG-O-RPT-LVL-DUN                 VALUE 'DUN'.
               88  MSG-O-RPT-LVL-EUS                 VALUE 'EUS'.
               88  MSG-O-RPT-LVL-FRP                 VALUE 'FRP'.
               88  MSG-O-RPT-LVL-HNC                 VALUE 'HNC'.
               88  MSG-O-RPT-LVL-HSP                 VALUE 'HSP'.
               88  MSG-O-RPT-LVL-INV                 VALUE 'INV'.
               88  MSG-O-RPT-LVL-JAM                 VALUE 'JAM'.
               88  MSG-O-RPT-LVL-JAS                 VALUE 'JAS'.
               88  MSG-O-RPT-LVL-LKP                 VALUE 'LKP'.
               88  MSG-O-RPT-LVL-LKW                 VALUE 'LKW'.
               88  MSG-O-RPT-LVL-MAD                 VALUE 'MAD'.
               88  MSG-O-RPT-LVL-MON                 VALUE 'MON'.
               88  MSG-O-RPT-LVL-OKL                 VALUE 'OKL'.
               88  MSG-O-RPT-LVL-PRY                 VALUE 'PRY'.
               88  MSG-O-RPT-LVL-PSJ                 VALUE 'PSJ'.
               88  MSG-O-RPT-LVL-RED                 VALUE 'RED'.
               88  MSG-O-RPT-LVL-STP                 VALUE 'STP'.
               88  MSG-O-RPT-LVL-TRN                 VALUE 'TRN'.
               88  MSG-O-RPT-LVL-TRP                 VALUE 'TRP'.
               88  MSG-O-RPT-LVL-WAL                 VALUE 'WAL'.
               88  MSG-O-RPT-LVL-WLD                 VALUE 'WLD'.
               88  MSG-O-RPT-LVL-ZEP                 VALUE 'ZEP'.
         05  MSG-O-BILL-ACCT-GRP.
           07  MSG-O-BILL-ACCT-GRP-1.
             09  MSG-O-NO-LOCK-SEQ-BA            PIC S9(2) COMP-3.
             09  MSG-O-NO-LOCK-SEQ-BA-AIM        PIC S9(2) COMP-3.
             09  MSG-O-KY-CUST-NO                PIC S9(9) COMP.
             09  MSG-O-KY-PREM-NO                PIC S9(9) COMP.
             09  MSG-O-CD-CO                     PIC S9(4) COMP.
             09  MSG-O-CD-COMPNY-FACILITY        PIC X(2).
               88  MSG-O-VS-COMP-STOREROOM         VALUE '01'.
             09  MSG-O-CD-EMP                    PIC X(2).
               88  MSG-O-ALABAMA-POWER             VALUE '01'.
             09  MSG-O-CD-BA-TRNSFR-TYPE         PIC X(2).
               88  MSG-O-INCONTIN                  VALUE '01'.
               88  MSG-O-ALSO-USES                 VALUE '03'.
               88  MSG-O-CHARGE-ON                 VALUE '05'.
               88  MSG-O-REBILL                    VALUE '06'.
             09  MSG-O-DT-NXT-SCH-RDG            PIC X(10).
             09  MSG-O-DT-LST-BLLD               PIC X(10).
             09  MSG-O-DT-NXT-BILL               PIC X(10).
             09  MSG-O-DT-BA-OPEN                PIC X(10).
             09  MSG-O-DT-BA-TERM                PIC X(10).
             09  MSG-O-DT-CNTL-UNIT-BAL          PIC X(10).
             09  MSG-O-DT-CUST-AWAY-BEG          PIC X(10).
             09  MSG-O-DT-CUST-AWAY-END          PIC X(10).
             09  MSG-O-DT-ON-DEM-BAL             PIC X(10).
             09  MSG-O-DT-BILL-CUTOFF            PIC X(10).
             09  MSG-O-DT-LST-REG                PIC X(10).
             09  MSG-O-DT-RT-FOLLOW-EFF          PIC X(10).
             09  MSG-O-CD-RT-TO-FOLLOW           PIC X(1).
               88  MSG-O-GAS-RTF                   VALUE 'G'.
               88  MSG-O-ELE-RTF                   VALUE 'E'.
               88  MSG-O-COMB-RTF                  VALUE 'C'.
               88  MSG-O-RTF-CD-SHOULD-REMV        VALUE 'X'.
             09  MSG-O-CD-RDG-INSTR              PIC X(2).
               88  MSG-O-DO-NOT-EST-TEMP           VALUE '01'.
               88  MSG-O-ACCT-CD-A-DEC             VALUE '02'.
               88  MSG-O-DO-NOT-EST-PERM           VALUE '03'.
               88  MSG-O-DO-NOT-EST-NO-POST-TEMP   VALUE '04'.
               88  MSG-O-ACCT-CD-D-DEC             VALUE '05'.
               88  MSG-O-DO-NOT-EST-NO-POST-PERM   VALUE '06'.
               88  MSG-O-DO-NOT-USE-ESTIMATE VALUE '01','02','03','04',
                   '05','06','07'.
               88  MSG-O-NO-POST-PERM              VALUE '07'.
             09  MSG-O-CD-BA-STAT                PIC X(2).
               88  MSG-O-ACT                       VALUE '02'.
               88  MSG-O-PEND-ACT                  VALUE '03'.
               88  MSG-O-PEND-TRNSFR               VALUE '04'.
               88  MSG-O-VOIDED                    VALUE '07'.
               88  MSG-O-FINALED                   VALUE '09'.
               88  MSG-O-UNCOLL                    VALUE '18'.
               88  MSG-O-NOT-VOID VALUE '02','03','04','09','18','30'.
               88  MSG-O-ESCHEAT                   VALUE '30'.
             09  MSG-O-CD-BILL-PRCS-INSTR        PIC X(4).
               88  MSG-O-NO-BILL-INSTR             VALUE '0000'.
               88  MSG-O-BILL                      VALUE '0100'.
               88  MSG-O-REBILL                    VALUE '0200'.
               88  MSG-O-BILL-FINAL                VALUE '0300'.
               88  MSG-O-REBILL-FINAL              VALUE '0400'.
               88  MSG-O-BILL-FINAL-COLL           VALUE '0500'.
               88  MSG-O-UPDATED                   VALUE '0600'.
               88  MSG-O-UPDATED-FINAL             VALUE '0700'.
             09  MSG-O-CD-SO-PRCS-INSTR          PIC X(3).
               88  MSG-O-NO-SO-PEND                VALUE '000'.
               88  MSG-O-BILL                      VALUE '100'.
               88  MSG-O-BILL-FINAL                VALUE '300'.
               88  MSG-O-PEND-TRNSFR               VALUE '700'.
             09  MSG-O-CD-TERM-TYPE-GAS          PIC X(2).
             09  MSG-O-CD-TERM-TYPE-ELE          PIC X(2).
             09  MSG-O-FL-PERM-BILL-RLSE         PIC X(1).
             09  MSG-O-KY-OLD-ACCTNO             PIC X(16).
             09  MSG-O-FL-SO-PEND                PIC X(1).
             09  MSG-O-KY-MTR-BILL-GRP           PIC S9(2) COMP-3.
             09  MSG-O-AT-PEND-SALE              PIC S9(7)V9(2) COMP-3.
             09  MSG-O-CD-BILL-LITR              PIC X(1).
             09  MSG-O-QY-BILL-COPY              PIC S9(1) COMP-3.
             09  MSG-O-CD-BILL-EXTN              PIC X(1).
               88  MSG-O-NOT-ON-EXTN               VALUE '0'.
               88  MSG-O-CUST-LESS-60-MO           VALUE '1'.
               88  MSG-O-CUST-LESS-60-1-YR         VALUE '2'.
               88  MSG-O-CUST-LESS-60-2-YR         VALUE '3'.
               88  MSG-O-CUST-LESS-60-3-YR         VALUE '4'.
               88  MSG-O-CUST-LESS-60-4-YR         VALUE '5'.
               88  MSG-O-CUST-OVER-60              VALUE '9'.
             09  MSG-O-FL-ELE-TRNSFR             PIC X(1).
             09  MSG-O-FL-DEFR-RDG               PIC X(1).
             09  MSG-O-FL-OUT-OF-TOWN            PIC X(1).
             09  MSG-O-CD-BILL-SAC               PIC X(1).
               88  MSG-O-ELE                       VALUE 'E'.
               88  MSG-O-GAS                       VALUE 'G'.
               88  MSG-O-ELE-GAS                   VALUE 'C'.
             09  MSG-O-CD-ACCT-TYPE              PIC X(2).
               88  MSG-O-PEND-SERV                 VALUE '03'.
               88  MSG-O-DEFAULTED                 VALUE '  '.
               88  MSG-O-PEND-SERV-OTHER-ACT       VALUE '06'.
               88  MSG-O-PEND-FINAL-ELE            VALUE '12'.
               88  MSG-O-PEND-FINAL-ELE-ACT        VALUE '21'.
               88  MSG-O-FINAL-SERV-ACT            VALUE '42'.
               88  MSG-O-DMAG                      VALUE '48'.
               88  MSG-O-SUM-BILL                  VALUE '51'.
               88  MSG-O-PAL                       VALUE '54'.
               88  MSG-O-SL                        VALUE '57'.
               88  MSG-O-NONCONSUMER               VALUE '60'.
               88  MSG-O-CASH                      VALUE '63'.
               88  MSG-O-DEBITS                    VALUE '66'.
               88  MSG-O-UNCOLL-CASH               VALUE '69'.
               88  MSG-O-SERV-TYPE VALUE '12','21','18','33','42','09',
                   '15','24','27','30','36','39','54'.
               88  MSG-O-SERV-PEND                 VALUE '03', '06'.
               88  MSG-O-NON-SERV VALUE '45','48','60','72'.
               88  MSG-O-TAX-ONLY                  VALUE '72'.
             09  MSG-O-CD-WAIVE-RSN              PIC X(1).
               88  MSG-O-UTIL-RF                   VALUE 'U'.
               88  MSG-O-DIST-WAIVE                VALUE 'W'.
               88  MSG-O-DEP-REFUND                VALUE 'R'.
               88  MSG-O-DEP-ALT                   VALUE 'A'.
               88  MSG-O-PEND-TRANSFER             VALUE 'P'.
               88  MSG-O-CREDIT-SCORING            VALUE 'C'.
             09  MSG-O-AT-OPEN-MO-BAL            PIC S9(9)V9(2) COMP-3.
             09  MSG-O-AT-MO-ACT                 PIC S9(9)V9(2) COMP-3.
             09  MSG-O-KY-CNTL-UNIT              PIC S9(2) COMP-3.
             09  MSG-O-KY-AD                     PIC S9(9) COMP.
             09  MSG-O-DT-CURR-RELAT-CYC         PIC S9(6) COMP-3.
             09  MSG-O-FL-OB                     PIC X(1).
             09  MSG-O-FL-RFND-UNCOLL            PIC X(1).
             09  MSG-O-CD-AD-TYPE                PIC X(1).
               88  MSG-O-PREM                      VALUE 'P'.
               88  MSG-O-BLDG                      VALUE 'B'.
               88  MSG-O-3RD-PARTY                 VALUE 'T'.
               88  MSG-O-MLNG-AD                   VALUE 'M'.
               88  MSG-O-GUAR                      VALUE 'G'.
               88  MSG-O-RFND                      VALUE 'R'.
               88  MSG-O-AGENCY                    VALUE 'A'.
               88  MSG-O-TEMPORARY                 VALUE 'Y'.
               88  MSG-O-CUSTOMER-88               VALUE 'C'.
               88  MSG-O-LANDLORD                  VALUE 'L'.
             09  MSG-O-KY-LST-USE-NO             PIC S9(5) COMP-3.
             09  MSG-O-CD-PRMTL-LITR             PIC S9(1) COMP-3.
             09  MSG-O-FL-TEMP-BILL-RLSE         PIC X(1).
             09  MSG-O-FL-MULT-CUST              PIC X(1).
             09  MSG-O-FL-SUM-BILL               PIC X(1).
           07  MSG-O-BILL-ACCT-GRP-2.
             09  MSG-O-AT-EXCR-BA                PIC S9(8)V9(2) COMP-3.
             09  MSG-O-AT-RFND-BA                PIC S9(8)V9(2) COMP-3.
             09  MSG-O-AT-BB-BLLD-BA             PIC S9(7)V9(2) COMP-3.
             09  MSG-O-AT-DEP-ON-HAND-BA         PIC S9(7)V9(2) COMP-3.
             09  MSG-O-AT-UNBIL-BAL-BA           PIC S9(8)V9(2) COMP-3.
             09  MSG-O-CD-MADE-ACT-AS            PIC X(1).
               88  MSG-O-INCONT                    VALUE 'C'.
               88  MSG-O-1ST-TM-USER               VALUE 'F'.
               88  MSG-O-USE-BEFORE                VALUE 'U'.
               88  MSG-O-ALSO-USES                 VALUE 'A'.
             09  MSG-O-CD-RTF-SO-TYPE            PIC S9(2) COMP-3.
               88  MSG-O-SETT                      VALUE 01.
               88  MSG-O-OTHR                      VALUE 02.
             09  MSG-O-CD-RES-COMM               PIC X(1).
               88  MSG-O-TAR-MIXED                 VALUE 'M'.
               88  MSG-O-TAR-RES                   VALUE 'R'.
               88  MSG-O-TAR-COMM                  VALUE 'C'.
             09  MSG-O-TM-CNTL-UNIT-BAL          PIC X(8).
             09  MSG-O-FL-PAD                    PIC X(1).
             09  MSG-O-CD-PAD-STAT               PIC S9(2) COMP.
             09  MSG-O-NO-PEND-BILL-GRP          PIC S9(2) COMP.
             09  MSG-O-DT-CYC-CHG                PIC X(10).
             09  MSG-O-CD-ACCT-HANDLING          PIC X(1).
               88  MSG-O-REGULAR                   VALUE 'R'.
               88  MSG-O-SPECIAL                   VALUE 'S'.
               88  MSG-O-PRE-CAL                   VALUE 'P'.
             09  MSG-O-NO-CC-TAX-EX-CERT         PIC X(13).
             09  MSG-O-DT-CC-TAX-EX-EXP          PIC X(10).
             09  MSG-O-CD-TAR-TYPE               PIC X(2).
               88  MSG-O-RES                       VALUE '01'.
               88  MSG-O-COMM                      VALUE '03'.
               88  MSG-O-INDUST                    VALUE '05'.
               88  MSG-O-STRT-AND-HWY-LT           VALUE '06'.
               88  MSG-O-SALES-TO-PUBL-AUTH        VALUE '07'.
               88  MSG-O-RURAL-ELE-COOP            VALUE '08'.
               88  MSG-O-MUNICIPAL                 VALUE '10'.
               88  MSG-O-SERVICE-CHARGE            VALUE '14'.
               88  MSG-O-EQUIPMENT-RENTALS         VALUE '15'.
             09  MSG-O-CD-TAX-AUTHORITY          PIC X(1).
             09  MSG-O-CD-CASH-ONLY-RES          PIC X(2).
             09  MSG-O-CD-OPER-CNTR              PIC X(3).
               88  MSG-O-GEN-OFFICE                VALUE 'GEN'.
             09  MSG-O-CD-REPORT-LEVEL           PIC X(5).
               88  MSG-O-RPT-LVL-APK               VALUE 'APK'.
               88  MSG-O-GEN-OFFICE                VALUE 'GEN'.
               88  MSG-O-RPT-LVL-APL               VALUE 'APL'.
               88  MSG-O-RPT-LVL-AVP               VALUE 'AVP'.
               88  MSG-O-RPT-LVL-BKS               VALUE 'BKS'.
               88  MSG-O-RPT-LVL-BLG               VALUE 'BLG'.
               88  MSG-O-RPT-LVL-BNV               VALUE 'BNV'.
               88  MSG-O-RPT-LVL-CAR               VALUE 'CAR'.
               88  MSG-O-RPT-LVL-CLR               VALUE 'CLR'.
               88  MSG-O-RPT-LVL-CLW               VALUE 'CLW'.
               88  MSG-O-RPT-LVL-CRC               VALUE 'CRC'.
               88  MSG-O-RPT-LVL-CRV               VALUE 'CRV'.
               88  MSG-O-RPT-LVL-CRW               VALUE 'CRW'.
               88  MSG-O-RPT-LVL-DEL               VALUE 'DEL'.
               88  MSG-O-RPT-LVL-DUN               VALUE 'DUN'.
               88  MSG-O-RPT-LVL-EUS               VALUE 'EUS'.
               88  MSG-O-RPT-LVL-FRP               VALUE 'FRP'.
               88  MSG-O-RPT-LVL-HNC               VALUE 'HNC'.
               88  MSG-O-RPT-LVL-HSP               VALUE 'HSP'.
               88  MSG-O-RPT-LVL-INV               VALUE 'INV'.
               88  MSG-O-RPT-LVL-JAM               VALUE 'JAM'.
               88  MSG-O-RPT-LVL-JAS               VALUE 'JAS'.
               88  MSG-O-RPT-LVL-LKP               VALUE 'LKP'.
               88  MSG-O-RPT-LVL-LKW               VALUE 'LKW'.
               88  MSG-O-RPT-LVL-MAD               VALUE 'MAD'.
               88  MSG-O-RPT-LVL-MON               VALUE 'MON'.
               88  MSG-O-RPT-LVL-OKL               VALUE 'OKL'.
               88  MSG-O-RPT-LVL-PRY               VALUE 'PRY'.
               88  MSG-O-RPT-LVL-PSJ               VALUE 'PSJ'.
               88  MSG-O-RPT-LVL-RED               VALUE 'RED'.
               88  MSG-O-RPT-LVL-STP               VALUE 'STP'.
               88  MSG-O-RPT-LVL-TRN               VALUE 'TRN'.
               88  MSG-O-RPT-LVL-TRP               VALUE 'TRP'.
               88  MSG-O-RPT-LVL-WAL               VALUE 'WAL'.
               88  MSG-O-RPT-LVL-WLD               VALUE 'WLD'.
               88  MSG-O-RPT-LVL-ZEP               VALUE 'ZEP'.
             09  MSG-O-CD-BP-TABLE-ACCESS        PIC X(2).
               88  MSG-O-USE-DEFAULT-ONLY          VALUE '01'.
               88  MSG-O-USE-BILL-FORMAT-ONLY      VALUE '02'.
               88  MSG-O-USE-DEF-PLUS-BILL-FORM    VALUE '03'.
             09  MSG-O-FL-LPC-ELIGIBLE           PIC X(1).
             09  MSG-O-CD-STAT                   PIC S9(2) COMP-3.
               88  MSG-O-INACT                     VALUE 01.
               88  MSG-O-ACT                       VALUE 02.
               88  MSG-O-PEND-ACT                  VALUE 03.
               88  MSG-O-GAS-ACT                   VALUE 04.
               88  MSG-O-ELECTRIC-ACT              VALUE 05.
               88  MSG-O-REMV                      VALUE 06.
               88  MSG-O-VOID                      VALUE 07.
               88  MSG-O-UNCOLL                    VALUE 08.
               88  MSG-O-FINALED                   VALUE 09.
               88  MSG-O-PEND-TRNSFR               VALUE 10.
               88  MSG-O-TRNSFR                    VALUE 11.
               88  MSG-O-CNCLD                     VALUE 12.
               88  MSG-O-RECONCILE                 VALUE 20.
               88  MSG-O-RECONCILE-INSERT          VALUE 21.
               88  MSG-O-UNRECONCILE               VALUE 22.
               88  MSG-O-DELETED                   VALUE 99.
             09  MSG-O-DT-STAT                   PIC X(10).
         05  MSG-O-BILL-ACCT-DATA.
           07  MSG-O-CD-COLL-STAT-DETL           PIC X(3).
               88  MSG-O-IMP-NTC-SENT                VALUE '01A'.
               88  MSG-O-30DAY-IMP-NTC-SENT          VALUE '01B'.
               88  MSG-O-FRNDLY-NTC-SENT             VALUE '01C'.
               88  MSG-O-FINAL-NTC-SENT              VALUE '01D'.
               88  MSG-O-SPECIAL-WFM-SENT            VALUE '01E'.
               88  MSG-O-COLL-ARNG-IMP-SENT          VALUE '01G'.
               88  MSG-O-COLL-ARNG-30-IMP-SENT       VALUE '01H'.
               88  MSG-O-COLL-ARNG-FRNDLY-SENT       VALUE '01I'.
               88  MSG-O-ELIGIBLE-FOR-CUT            VALUE '02A'.
               88  MSG-O-COLL-ARNG-ELG-FOR-CUT       VALUE '02B'.
               88  MSG-O-DOOR-HANGER-ISSUED          VALUE '03A'.
               88  MSG-O-NOTIFIED-IN-FIELD           VALUE '03B'.
               88  MSG-O-WC-RQST-OFFICE-RVW          VALUE '03C'.
               88  MSG-O-CUT-ORDER-ISSUED            VALUE '04A'.
               88  MSG-O-CUT-OUT-NON-PAY             VALUE '04B'.
               88  MSG-O-EXPD-FROM-CUT-LIST          VALUE '04C'.
               88  MSG-O-RMVD-FROM-CUT-LIST          VALUE '04D'.
               88  MSG-O-INVESTIGATE-ISSUED          VALUE '06A'.
               88  MSG-O-INVESTIGATE-CMPLTD          VALUE '06B'.
               88  MSG-O-FINAL-ISSUED                VALUE '07A'.
               88  MSG-O-FINAL-NO-NTC-SENT           VALUE '07B'.
               88  MSG-O-REINSTATED                  VALUE '07D'.
               88  MSG-O-PNTL-CHRG-OFF-LIST          VALUE '08A'.
               88  MSG-O-HOLD-PNTL-CHRG-OFF          VALUE '08B'.
               88  MSG-O-RDY-CHRG-OFF-NO-AC          VALUE '08C'.
               88  MSG-O-CHRG-OFF-TO-AGNCY           VALUE '09A'.
               88  MSG-O-CHRG-OFF-NO-AGNCY           VALUE '09B'.
               88  MSG-O-PEND-CHRG-OFF-AGENCY        VALUE '09C'.
               88  MSG-O-BAD-DEBT                    VALUE '10A'.
               88  MSG-O-COLLCTN-ARRNGMNT            VALUE '11A'.
               88  MSG-O-ACCT-TRNSFRD-ON             VALUE '12A'.
               88  MSG-O-RMVD-FROM-COLL              VALUE '13A'.
               88  MSG-O-AT-MIN-ACT-COLL             VALUE '25A'.
               88  MSG-O-AT-MIN-FIN-COLL             VALUE '25B'.
               88  MSG-O-VALID-NOTICE-TYPE VALUE '01A','01B','01C',
                   '01D','01G','01H','01I'.
               88  MSG-O-VLD-NTC-CRITERIA VALUE '13A','04C','04D','01C',
                   '07C','07A','01E','11A','01I','03C','02A','02B',
                   '06B','01F'.
               88  MSG-O-VLD-LIST-CRITERIA VALUE '01A','01B','02A',
                   '08B','01F','01D','01G','01H','02B','07B','07D',
                   '03B','03C'.
               88  MSG-O-NOT-OPENED-PT               VALUE '26A'.
               88  MSG-O-PAID-ON-TIME-PT             VALUE '26B'.
               88  MSG-O-PAID-OVERDUE-PT             VALUE '26C'.
               88  MSG-O-PAID-OUTSTANDING-PT         VALUE '26D'.
               88  MSG-O-CUT-FOR-NON-PAY-PT          VALUE '26E'.
               88  MSG-O-RTRN-CHECK-PT               VALUE '26F'.
               88  MSG-O-HI-A-RANGE                  VALUE '27A'.
               88  MSG-O-HI-B-RANGE                  VALUE '27B'.
               88  MSG-O-HI-C-RANGE                  VALUE '27C'.
               88  MSG-O-HI-D-RANGE                  VALUE '27D'.
               88  MSG-O-HI-E-RANGE                  VALUE '27E'.
               88  MSG-O-HI-F-RANGE                  VALUE '27F'.
               88  MSG-O-FIRST-MB-WEIGHT             VALUE '28A'.
               88  MSG-O-SEC-THRD-MB-WEIGHT          VALUE '28B'.
               88  MSG-O-LO-DET-CR-VALUE             VALUE '26A'.
               88  MSG-O-HI-DET-CR-VALUE             VALUE '28F'.
               88  MSG-O-RDY-FOR-CHRGE-OFF           VALUE '08D'.
               88  MSG-O-AT-MIN-AGENCY-COLL          VALUE '25C'.
               88  MSG-O-RTND-CHK-NTC-SENT           VALUE '01F'.
               88  MSG-O-FINAL-NON-SERV-ISSUED       VALUE '07C'.
               88  MSG-O-DEP-LOWER-THRESHOLD         VALUE '50A'.
               88  MSG-O-COMMERCIAL-THRESHOLD        VALUE '51A'.
               88  MSG-O-DEP-UPPER-THRESHOLD         VALUE '50B'.
               88  MSG-O-DEPOSIT-ALT-THRESHOLD       VALUE '52A'.
               88  MSG-O-FRIENDLY-MESSAG VALUE '01C','01E','01I','06A',
                   '06B','07A','07B','07C','11A','13A'.
               88  MSG-O-SERIOUS-MESSAG VALUE '01A','01B','01D','01F',
                   '01G','01H','02A','02B','03A','03B','03C','04A',
                   '04B','04C','04D','08A','08B','08C','08D','09A',
                   '09B','09C','10A'.
               88  MSG-O-FINAL-COLL-STAT VALUE '01D','07A','07B','07C',
                   '07D','08A','08B','08C','08D','09A','09B','09C'.
           07  MSG-O-AD-LN-1                     PIC X(28).
           07  MSG-O-AD-LN-2                     PIC X(28).
           07  MSG-O-AD-LN-3                     PIC X(28).
           07  MSG-O-AD-LN-4                     PIC X(28).
           07  MSG-O-TX-ACCT-HANDLING            PIC X(15).
         05  MSG-O-SITE-GROUP.
           07  MSG-O-NM-SITE                     PIC X(56).
           07  MSG-O-CD-SITE-TYPE                PIC X(2).
               88  MSG-O-APARTMENT-COMPLEX           VALUE '01'.
               88  MSG-O-SUBDIVISION                 VALUE '02'.
               88  MSG-O-OFFICE-COMPLEX              VALUE '03'.
               88  MSG-O-CONDO                       VALUE '04'.
               88  MSG-O-MOBILE-HOME-PARK            VALUE '05'.
               88  MSG-O-SHOPPING-CENTER-MALL        VALUE '06'.
               88  MSG-O-OTHER-SITE-TYPE             VALUE '07'.
               88  MSG-O-NURSING-HOME                VALUE '08'.
               88  MSG-O-AMUSEMENT-PARK              VALUE '09'.
               88  MSG-O-COUNTRY-CLUB                VALUE '10'.
               88  MSG-O-MED-COMPLEX                 VALUE '11'.
               88  MSG-O-COLL-UNIV                   VALUE '12'.
               88  MSG-O-INDUST-PARK                 VALUE '13'.
               88  MSG-O-SCHOOL                      VALUE '14'.
               88  MSG-O-AIRPORT                     VALUE '15'.
               88  MSG-O-GOV-FACILITY                VALUE '16'.
           07  MSG-O-CD-OPER-CNTR                PIC X(3).
               88  MSG-O-GEN-OFFICE                  VALUE 'GEN'.
           07  MSG-O-NM-CONTACT                  PIC X(20).
           07  MSG-O-TX-CNTC-ACD                 PIC X(3).
           07  MSG-O-TX-CNTC-PHN-NO              PIC X(8).
           07  MSG-O-TX-CNTC-EXTN                PIC X(4).
           07  MSG-O-KY-RMRK-SITE-DIR            PIC S9(9) COMP.
           07  MSG-O-AD-SERV-CITY                PIC X(19).
           07  MSG-O-AD-SERV-ST                  PIC X(2).
           07  MSG-O-AD-SERV-ZIP                 PIC S9(5) COMP-3.
           07  MSG-O-KY-GEN-NM-1                 PIC X(8).
           07  MSG-O-KY-GEN-NM-2                 PIC X(2).
           07  MSG-O-FL-LM-ALLOW                 PIC X(1).
               88  MSG-O-FL-LM-ALLOW-Y               VALUE 'Y'.
               88  MSG-O-FL-LM-ALLOW-N               VALUE 'N'.
           07  MSG-O-FL-EE-ALLOW                 PIC X(1).
               88  MSG-O-FL-EE-ALLOW-Y               VALUE 'Y'.
               88  MSG-O-FL-EE-ALLOW-N               VALUE 'N'.
           07  MSG-O-CD-ROW-STAT                 PIC X(2).
               88  MSG-O-INACTIVE                    VALUE '01'.
               88  MSG-O-ACTIVE                      VALUE '02'.
               88  MSG-O-DELETED                     VALUE '99'.
           07  MSG-O-DT-ROW-STAT                 PIC X(10).
         05  MSG-O-RETR-DATA-1.
           07  MSG-O-QY-ROWS-RETURNED            PIC S9(3) COMP-3.
           07  MSG-O-FL-ADDR-IGNORED             PIC X(1).
               88  MSG-O-ADDR-IGNORED                VALUE 'Y'.
               88  MSG-O-ADDR-NOT-IGNORED            VALUE 'N'.
           07  MSG-O-FL-NM-CUST-IGNORED          PIC X(1).
               88  MSG-O-NM-CUST-IGNORED             VALUE 'Y'.
               88  MSG-O-NM-CUST-NOT-IGNORE          VALUE 'N'.
           07  MSG-O-FL-BA-IGNORED               PIC X(1).
               88  MSG-O-BA-IGNORED                  VALUE 'Y'.
               88  MSG-O-BA-NOT-IGNORED              VALUE 'N'.
           07  MSG-O-FL-SSN-IGNORED              PIC X(1).
               88  MSG-O-SSN-IGNORED                 VALUE 'Y'.
               88  MSG-O-SSN-NOT-IGNORED             VALUE 'N'.
           07  MSG-O-FL-NM-SITE-IGNORED          PIC X(1).
               88  MSG-O-NM-SITE-IGNORED             VALUE 'Y'.
               88  MSG-O-NM-SITE-NOT-IGNORED         VALUE 'N'.
           07  MSG-O-FL-OL-ACCT-IGNORED          PIC X(1).
               88  MSG-O-OL-ACCT-IGNORED             VALUE 'Y'.
               88  MSG-O-OL-ACCT-NOT-IGNORE          VALUE 'N'.
           07  MSG-O-FL-PHN-IGNORED              PIC X(1).
               88  MSG-O-PHN-IGNORED                 VALUE 'Y'.
               88  MSG-O-PHN-NOT-IGNORED             VALUE 'N'.
           07  MSG-O-FL-MTR-NO-IGNORED           PIC X(1).
               88  MSG-O-MTR-NO-IGNORED              VALUE 'Y'.
               88  MSG-O-MTR-NO-NOT-IGNORED          VALUE 'N'.
           07  MSG-O-FL-CITY-IGNORED             PIC X(1).
               88  MSG-O-CITY-IGNORED                VALUE 'Y'.
               88  MSG-O-CITY-NOT-IGNORED            VALUE 'N'.
           07  MSG-O-FL-COUNTY-IGNORED           PIC X(1).
               88  MSG-O-COUNTY-IGNORED              VALUE 'Y'.
               88  MSG-O-COUNTY-NOT-IGNORED          VALUE 'N'.
           07  MSG-O-FL-ZIP-IGNORED              PIC X(1).
               88  MSG-O-ZIP-IGNORED                 VALUE 'Y'.
               88  MSG-O-ZIP-NOT-IGNORED             VALUE 'N'.
           07  MSG-O-FL-SOUNDEX-IGNORED          PIC X(1).
               88  MSG-O-SOUNDEX-IGNORED             VALUE 'Y'.
               88  MSG-O-SOUNDX-NOT-IGNORED          VALUE 'N'.
           07  MSG-O-FL-STREET-NM-SUBST          PIC X(1).
           07  MSG-O-AD-SERV-STR-NO              PIC X(15).
           07  MSG-O-AD-SERV-STR-NM              PIC X(23).
           07  MSG-O-KY-PWQ-WO-ID                PIC X(5).
           07  MSG-O-CD-CNTC-TYPE-GRP
                            OCCURS 10 TIMES.
             09  MSG-O-CD-CNTC-TYPE              PIC X(4).
           07  MSG-O-CD-CRITICAL-ACCT            PIC X(2).
           07  MSG-O-KY-BA-2                     PIC S9(10) COMP-3.
           07  MSG-O-FL-RULE-DATA                PIC X(1).
           07  MSG-O-FL-SO-PEND-AT-PREM          PIC X(1).
         05  MSG-O-RETR-ROW
                            OCCURS 50 TIMES.
           07  MSG-O-KEY-SAD.
             09  MSG-O-KY-BA                     PIC S9(10) COMP-3.
             09  MSG-O-KY-CUST-NO                PIC S9(9) COMP.
             09  MSG-O-KY-PREM-NO                PIC S9(9) COMP.
             09  MSG-O-CD-ACCT-TYPE              PIC X(2).
               88  MSG-O-PEND-SERV                 VALUE '03'.
               88  MSG-O-DEFAULTED                 VALUE '  '.
               88  MSG-O-PEND-SERV-OTHER-ACT       VALUE '06'.
               88  MSG-O-PEND-FINAL-ELE            VALUE '12'.
               88  MSG-O-PEND-FINAL-ELE-ACT        VALUE '21'.
               88  MSG-O-FINAL-SERV-ACT            VALUE '42'.
               88  MSG-O-DMAG                      VALUE '48'.
               88  MSG-O-SUM-BILL                  VALUE '51'.
               88  MSG-O-PAL                       VALUE '54'.
               88  MSG-O-SL                        VALUE '57'.
               88  MSG-O-NONCONSUMER               VALUE '60'.
               88  MSG-O-CASH                      VALUE '63'.
               88  MSG-O-DEBITS                    VALUE '66'.
               88  MSG-O-UNCOLL-CASH               VALUE '69'.
               88  MSG-O-SERV-TYPE VALUE '12','21','18','33','42','09',
                   '15','24','27','30','36','39','54'.
               88  MSG-O-SERV-PEND                 VALUE '03', '06'.
               88  MSG-O-NON-SERV VALUE '45','48','60','72'.
               88  MSG-O-TAX-ONLY                  VALUE '72'.
           07  MSG-O-SAD.
             09  MSG-O-NM-CUST-1ST-1             PIC X(15).
             09  MSG-O-NM-CUST-MID-1             PIC X(15).
             09  MSG-O-NM-CUST-LST-1             PIC X(20).
             09  MSG-O-KY-SSN                    PIC S9(9) COMP.
             09  MSG-O-NM-CUST-TTL-1             PIC X(8).
             09  MSG-O-NM-CUST-SFIX-1            PIC X(4).
             09  MSG-O-KY-GRID-NO                PIC S9(14) COMP-3.
             09  MSG-O-AD-SERV-CITY              PIC X(19).
             09  MSG-O-AD-SERV-ST                PIC X(2).
             09  MSG-O-AD-SERV-ZIP               PIC S9(5) COMP-3.
             09  MSG-O-CD-GAS-MTR-SUM            PIC X(3).
             09  MSG-O-CD-ELE-MTR-SUM            PIC X(3).
             09  MSG-O-KY-OLD-ACCT-NO-1          PIC X(16).
             09  MSG-O-KY-GENER-AD-1             PIC X(7).
             09  MSG-O-KY-GENER-AD-2             PIC X(5).
             09  MSG-O-KY-GEN-NM.
               11  MSG-O-KY-GEN-NM-1             PIC X(8).
               11  MSG-O-KY-GEN-NM-2             PIC X(2).
             09  MSG-O-CD-SAD-STAT               PIC X(2).
               88  MSG-O-ACTIVE                    VALUE '03'.
               88  MSG-O-INACTIVE                  VALUE '06'.
               88  MSG-O-PEND-PREM                 VALUE '08'.
               88  MSG-O-PENDING                   VALUE '09'.
               88  MSG-O-VOID                      VALUE '12'.
               88  MSG-O-FINALED                   VALUE '15'.
               88  MSG-O-UNCOLL                    VALUE '18'.
               88  MSG-O-RO                        VALUE '21'.
               88  MSG-O-RECONC                    VALUE '24'.
               88  MSG-O-UNRECONC                  VALUE '27'.
               88  MSG-O-ESCHEAT                   VALUE '30'.
             09  MSG-O-NM-COMPRESSED             PIC X(56).
             09  MSG-O-AD-COMPRESSED             PIC X(69).
             09  MSG-O-CD-SUM-EXIST              PIC X(1).
               88  MSG-O-DEL                       VALUE 'D'.
               88  MSG-O-EXIST                     VALUE 'E'.
               88  MSG-O-CONV                      VALUE 'C'.
             09  MSG-O-CD-CIS-EXIST              PIC X(1).
               88  MSG-O-DOES-NOT-EXIST            VALUE 'D'.
               88  MSG-O-EXIST                     VALUE 'E'.
             09  MSG-O-CD-ACS-EXIST              PIC X(1).
               88  MSG-O-DOES-NOT-EXIST            VALUE 'D'.
               88  MSG-O-EXISTS                    VALUE 'E'.
             09  MSG-O-CD-NF-EXIST               PIC X(1).
             09  MSG-O-QY-LNGH-AD                PIC S9(2) COMP.
             09  MSG-O-KY-MTR-BILL-GRP           PIC S9(2) COMP-3.
             09  MSG-O-CD-CUST-INDIC             PIC S9(1) COMP-3.
               88  MSG-O-NO-CUST                   VALUE 0.
               88  MSG-O-PRIM                      VALUE 1.
               88  MSG-O-2ND                       VALUE 2.
             09  MSG-O-CD-ICS-EXIST              PIC X(1).
               88  MSG-O-EXIST                     VALUE 'E'.
               88  MSG-O-DOES-NOT-EXIST            VALUE 'D'.
             09  MSG-O-TX-HOME-ACD               PIC X(3).
             09  MSG-O-TX-HOME-PHN-NO            PIC X(8).
             09  MSG-O-TX-HOME-PHN-EXTN          PIC X(4).
             09  MSG-O-TX-BUS-ACD                PIC X(3).
             09  MSG-O-TX-BUS-PHN-NO             PIC X(8).
             09  MSG-O-TX-BUS-PHN-EXTN           PIC X(4).
             09  MSG-O-CD-COUNTY                 PIC X(3).
             09  MSG-O-CD-REGION                 PIC X(3).
             09  MSG-O-CD-OPER-CNTR              PIC X(3).
               88  MSG-O-GEN-OFFICE                VALUE 'GEN'.
             09  MSG-O-FL-CRITICAL-CONTCT        PIC X(1).
               88  MSG-O-CRITICAL                  VALUE 'Y'.
               88  MSG-O-NON-CRITICAL              VALUE 'N'.
             09  MSG-O-KY-AD                     PIC S9(9) COMP.
             09  MSG-O-CD-AD-TYPE                PIC X(1).
               88  MSG-O-PREM                      VALUE 'P'.
               88  MSG-O-BLDG                      VALUE 'B'.
               88  MSG-O-3RD-PARTY                 VALUE 'T'.
               88  MSG-O-MLNG-AD                   VALUE 'M'.
               88  MSG-O-GUAR                      VALUE 'G'.
               88  MSG-O-RFND                      VALUE 'R'.
               88  MSG-O-AGENCY                    VALUE 'A'.
               88  MSG-O-TEMPORARY                 VALUE 'Y'.
               88  MSG-O-CUSTOMER-88               VALUE 'C'.
               88  MSG-O-LANDLORD                  VALUE 'L'.
             09  MSG-O-KY-BLDG-NO                PIC S9(9) COMP.
             09  MSG-O-CD-CO                     PIC S9(4) COMP.
             09  MSG-O-CD-CUST-RESPNSBL          PIC X(2).
               88  MSG-O-PRIMARY                   VALUE '01'.
               88  MSG-O-SPOUSE                    VALUE '02'.
               88  MSG-O-THIRD-PARTY               VALUE '03'.
               88  MSG-O-ROOMMATE                  VALUE '04'.
               88  MSG-O-BILL-CONTACT              VALUE '05'.
               88  MSG-O-COMP-CONTACT              VALUE '06'.
               88  MSG-O-MARK-CONTACT              VALUE '07'.
               88  MSG-O-OTHR                      VALUE '08'.
             09  MSG-O-DT-STAT                   PIC X(10).
           07  MSG-O-KY-SITE-NO                  PIC S9(9) COMP.
           07  MSG-O-AD-LN                       PIC X(28).
           07  MSG-O-KY-GEN-AD-NUM-KEYS
                            OCCURS 12 TIMES.
             09  MSG-O-KY-GEN-AD-NUMERIC         PIC S9(2) COMP.
           07  MSG-O-RETR-WILD-KEYS.
             09  MSG-O-NM-CUST-2                 PIC X(28).
             09  MSG-O-AD-SERV-STR-NM            PIC X(23).

