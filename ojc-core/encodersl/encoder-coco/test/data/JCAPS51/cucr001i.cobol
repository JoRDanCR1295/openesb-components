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
      **      COPYBOOK: CUCR001I                                     **
      **                                                             **
      **  COPYBOOK FOR: Copybook.CUCR001I RETRIEVAL SVC INPUT        **
      **                                                             **
      **  GENERATED ON: Thu May 14 17:08:40 1998                     **
      **                                                             **
      **  SHORT DESCRIPTION: CUCR001I RETRIEVAL SVC INPUT            **
      **                                                             **
      **            BY: GRWILLIS                                     **
      **                                                             **
      *****************************************************************
       03  MSG-I-CUCR001I.
         05  MSG-I-STANDARD-HEADER.
           07  MSG-I-CD-FUNC-ID                  PIC X(2).
               88  MSG-I-FUNC-01                     VALUE '01'.
               88  MSG-I-FUNC-02                     VALUE '02'.
               88  MSG-I-FUNC-03                     VALUE '03'.
               88  MSG-I-FUNC-04                     VALUE '04'.
               88  MSG-I-FUNC-05                     VALUE '05'.
               88  MSG-I-FUNC-06                     VALUE '06'.
               88  MSG-I-FUNC-07                     VALUE '07'.
               88  MSG-I-FUNC-08                     VALUE '08'.
               88  MSG-I-FUNC-09                     VALUE '09'.
               88  MSG-I-FUNC-10                     VALUE '10'.
               88  MSG-I-FUNC-11                     VALUE '11'.
               88  MSG-I-FUNC-12                     VALUE '12'.
               88  MSG-I-FUNC-13                     VALUE '13'.
               88  MSG-I-FUNC-14                     VALUE '14'.
               88  MSG-I-FUNC-15                     VALUE '15'.
               88  MSG-I-FUNC-16                     VALUE '16'.
               88  MSG-I-FUNC-17                     VALUE '17'.
               88  MSG-I-FUNC-18                     VALUE '18'.
               88  MSG-I-FUNC-19                     VALUE '19'.
               88  MSG-I-FUNC-20                     VALUE '20'.
               88  MSG-I-FUNC-21                     VALUE '21'.
               88  MSG-I-FUNC-22                     VALUE '22'.
               88  MSG-I-FUNC-23                     VALUE '23'.
               88  MSG-I-FUNC-24                     VALUE '24'.
               88  MSG-I-FUNC-25                     VALUE '25'.
               88  MSG-I-FUNC-26                     VALUE '26'.
               88  MSG-I-FUNC-27                     VALUE '27'.
               88  MSG-I-FUNC-28                     VALUE '28'.
               88  MSG-I-FUNC-29                     VALUE '29'.
               88  MSG-I-FUNC-30                     VALUE '30'.
               88  MSG-I-FUNC-31                     VALUE '31'.
               88  MSG-I-FUNC-32                     VALUE '32'.
               88  MSG-I-FUNC-33                     VALUE '33'.
               88  MSG-I-FUNC-34                     VALUE '34'.
               88  MSG-I-FUNC-35                     VALUE '35'.
               88  MSG-I-FUNC-36                     VALUE '36'.
               88  MSG-I-FUNC-37                     VALUE '37'.
               88  MSG-I-FUNC-38                     VALUE '38'.
               88  MSG-I-FUNC-39                     VALUE '39'.
               88  MSG-I-FUNC-40                     VALUE '40'.
               88  MSG-I-FUNC-41                     VALUE '41'.
               88  MSG-I-FUNC-42                     VALUE '42'.
               88  MSG-I-FUNC-43                     VALUE '43'.
               88  MSG-I-FUNC-44                     VALUE '44'.
               88  MSG-I-FUNC-45                     VALUE '45'.
               88  MSG-I-FUNC-46                     VALUE '46'.
               88  MSG-I-FUNC-47                     VALUE '47'.
               88  MSG-I-FUNC-48                     VALUE '48'.
               88  MSG-I-FUNC-49                     VALUE '49'.
               88  MSG-I-FUNC-50                     VALUE '50'.
           07  MSG-I-DT-CURR-DATE                PIC X(10).
           07  MSG-I-KY-CONV-ID                  PIC X(8).
           07  MSG-I-STNDRD-HEAD-SUBGRP.
             09  MSG-I-KY-BA                     PIC S9(10) COMP-3.
             09  MSG-I-KY-CUST-NO                PIC S9(9) COMP.
             09  MSG-I-KY-PREM-NO                PIC S9(9) COMP.
             09  MSG-I-KY-SPT                    PIC S9(8) COMP-3.
             09  MSG-I-KY-FORD-NO                PIC S9(9) COMP-3.
             09  MSG-I-ID-EQUIPMENT              PIC S9(8) COMP.
             09  MSG-I-NO-FUTURE-KEY-X           PIC S9(9) COMP.
             09  MSG-I-NO-FUTURE-KEY-Y           PIC S9(9) COMP.
             09  MSG-I-NO-FUTURE-KEY-Z           PIC S9(9) COMP.
             09  MSG-I-NO-LOCK-SEQ-CUST          PIC S9(2) COMP-3.
             09  MSG-I-NO-LOCK-SEQ-PREM          PIC S9(2) COMP-3.
             09  MSG-I-NO-LOCK-SEQ-BA            PIC S9(2) COMP-3.
             09  MSG-I-NO-LOCK-SEQ-BA-AIM        PIC S9(2) COMP-3.
             09  MSG-I-NO-LOCK-SEQ-SPT           PIC S9(2) COMP-3.
             09  MSG-I-NO-LOCK-SEQ-FO-HDR        PIC S9(2) COMP-3.
             09  MSG-I-NO-LOCK-SEQ-EQUIP         PIC S9(2) COMP.
             09  MSG-I-NO-LOCK-SEQ-X             PIC S9(2) COMP-3.
             09  MSG-I-NO-LOCK-SEQ-Y             PIC S9(2) COMP-3.
             09  MSG-I-NO-LOCK-SEQ-Z             PIC S9(2) COMP-3.
           07  MSG-I-KY-USERID-2                 PIC X(8).
           07  MSG-I-NO-SVC-VERSION              PIC S9(14) COMP.
           07  MSG-I-PERF-SUBGRP-2.
             09  MSG-I-ID-PREV-MSG-INST          PIC X(24).
             09  MSG-I-NO-PREV-RESP-TIME         PIC S9(4)V9(3) COMP-3.
           07  MSG-I-CD-STD-MSG-HDR-FLR          PIC X(20).
         05  MSG-I-RETR-KEYS-1.
           07  MSG-I-CD-MEGA-SCROLL-TYPE         PIC X(4).
           07  MSG-I-CD-ACS-CMD                  PIC X(2).
               88  MSG-I-FETCH                       VALUE 'F '.
               88  MSG-I-FETCH-NEXT                  VALUE 'FN'.
               88  MSG-I-FETCH-PREVIOUS              VALUE 'FP'.
               88  MSG-I-GET                         VALUE 'G '.
               88  MSG-I-GET-NEXT                    VALUE 'GN'.
               88  MSG-I-INSERTED                    VALUE 'I '.
               88  MSG-I-UPDATE                      VALUE 'U '.
               88  MSG-I-DELETED                     VALUE 'D '.
               88  MSG-I-RENAME                      VALUE 'R '.
               88  MSG-I-UPDATE-SET                  VALUE 'US'.
               88  MSG-I-DELETE-SET                  VALUE 'DS'.
               88  MSG-I-SUMM                        VALUE 'SU'.
               88  MSG-I-INSERT-SET                  VALUE 'IS'.
               88  MSG-I-COUNT                       VALUE 'C '.
               88  MSG-I-OPENED                      VALUE 'OP'.
               88  MSG-I-CLOSED                      VALUE 'CL'.
               88  MSG-I-CHECKPOINT                  VALUE 'CK'.
               88  MSG-I-NEXT-PREVIOUS               VALUE 'NP'.
               88  MSG-I-MAX                         VALUE 'MX'.
               88  MSG-I-MIN                         VALUE 'MN'.
               88  MSG-I-DACP-EXIST-CHK              VALUE 'EX'.
           07  MSG-I-FL-PAGE                     PIC S9(1) COMP-3.
           07  MSG-I-CD-RETRIEVAL-TYPE           PIC X(2).
               88  MSG-I-RETR-BA-AND-PRIM-CUST       VALUE 'BP'.
               88  MSG-I-RETR-PREM-NO                VALUE 'PR'.
               88  MSG-I-RETR-CUST-NO                VALUE 'CU'.
               88  MSG-I-RETR-SITE-NO                VALUE 'SN'.
               88  MSG-I-RETR-BLDG-NO                VALUE 'BU'.
               88  MSG-I-RETR-CUST-NO-AND-PREM-NO    VALUE 'CP'.
               88  MSG-I-RETR-RET-ONE-SITE           VALUE '1S'.
               88  MSG-I-RETR-RET-MULT-SITES         VALUE 'MS'.
               88  MSG-I-RETR-SAD-MULT-CRIT          VALUE 'SC'.
               88  MSG-I-RETR-RET-ONE-BLDG           VALUE '1B'.
               88  MSG-I-RETR-GAK                    VALUE 'GA'.
               88  MSG-I-RETR-GNK                    VALUE 'GN'.
               88  MSG-I-RETR-BA                     VALUE 'BA'.
               88  MSG-I-RETR-SSN                    VALUE 'SS'.
               88  MSG-I-RETR-OLD-ACCT               VALUE 'OA'.
               88  MSG-I-RETR-PHONE                  VALUE 'PH'.
               88  MSG-I-RETR-SITE-GNK               VALUE 'SK'.
           07  MSG-I-AD-COMPRESSED               PIC X(69).
           07  MSG-I-NM-COMPRESSED               PIC X(56).
           07  MSG-I-FL-CUST-NM-SOUNDEX          PIC X(1).
               88  MSG-I-CUST-NM-SOUNDEX             VALUE 'Y'.
               88  MSG-I-NOT-CUST-NM-SNDX            VALUE 'N'.
           07  MSG-I-KY-BA                       PIC S9(10) COMP-3.
           07  MSG-I-KY-SSN                      PIC S9(9) COMP.
           07  MSG-I-KY-OLD-ACCNT-NO             PIC X(16).
           07  MSG-I-TX-HOME-ACD                 PIC X(3).
           07  MSG-I-TX-HOME-PHN-NO              PIC X(8).
           07  MSG-I-KY-MTR-EQUIP-NO             PIC X(9).
           07  MSG-I-AD-SERV-CITY                PIC X(19).
           07  MSG-I-CD-COUNTY                   PIC X(3).
           07  MSG-I-AD-SERV-ZIP                 PIC S9(5) COMP-3.
           07  MSG-I-FL-ACTV-ACCTS-ONLY          PIC X(1).
               88  MSG-I-ACTV-ACCTS-ONLY             VALUE 'Y'.
               88  MSG-I-NO-ACTV-ACCTS-ONLY          VALUE 'N'.
           07  MSG-I-CD-CO                       PIC S9(4) COMP.
           07  MSG-I-CD-REGION-GROUP
                            OCCURS 4 TIMES.
             09  MSG-I-CD-REGION                 PIC X(3).
           07  MSG-I-CD-OPER-CNTR                PIC X(3).
               88  MSG-I-GEN-OFFICE                  VALUE 'GEN'.
           07  MSG-I-FL-SYSTEM-WIDE              PIC X(1).
               88  MSG-I-SYSTEM-WIDE                 VALUE 'Y'.
               88  MSG-I-NOT-SYSTEM-WIDE             VALUE 'N'.
           07  MSG-I-FL-SHOW-ACCT-MAIL           PIC X(1).
               88  MSG-I-SHOW-ACCT-MAIL              VALUE 'Y'.
               88  MSG-I-NOT-SHOW-ACCT-MAIL          VALUE 'N'.
           07  MSG-I-KY-CUST-NO                  PIC S9(9) COMP.
           07  MSG-I-KY-PREM-NO                  PIC S9(9) COMP.
           07  MSG-I-KY-BLDG-NO                  PIC S9(9) COMP.
           07  MSG-I-KY-SITE-NO                  PIC S9(9) COMP.
           07  MSG-I-KY-GEN-AD-NUM-KEYS
                            OCCURS 12 TIMES.
             09  MSG-I-KY-GEN-AD-NUMERIC         PIC S9(2) COMP.
           07  MSG-I-KY-GEN-NM.
             09  MSG-I-KY-GEN-NM-1               PIC X(8).
             09  MSG-I-KY-GEN-NM-2               PIC X(2).
           07  MSG-I-FL-USE-CRITERIA             PIC X(1).
               88  MSG-I-USE-CRITERIA                VALUE 'Y'.
               88  MSG-I-NOT-USE-CRITERIA            VALUE 'N'.
           07  MSG-I-RETR-WILD-CRITERIA.
             09  MSG-I-NM-CUST-1ST-1             PIC X(15).
             09  MSG-I-NM-CUST-MID-1             PIC X(15).
             09  MSG-I-NM-CUST-LST-1             PIC X(20).
             09  MSG-I-NM-CUST-2                 PIC X(28).
             09  MSG-I-AD-SERV-STR-NO            PIC X(15).
             09  MSG-I-AD-SERV-CDL-DIR           PIC X(2).
             09  MSG-I-AD-SERV-STR-NM            PIC X(23).
             09  MSG-I-AD-SERV-STR-SFIX          PIC X(4).
             09  MSG-I-AD-SERV-SFIX              PIC X(2).
               88  MSG-I-N                         VALUE 'N'.
               88  MSG-I-S                         VALUE 'S'.
               88  MSG-I-E                         VALUE 'E'.
               88  MSG-I-W                         VALUE 'W'.
               88  MSG-I-NE                        VALUE 'NE'.
               88  MSG-I-NW                        VALUE 'NW'.
               88  MSG-I-SE                        VALUE 'SE'.
               88  MSG-I-SW                        VALUE 'SW'.
               88  MSG-I-BLANKS                    VALUE '  '.
               88  MSG-I-VALID-VALUES VALUE 'N','S','E','W','NE','NW',
                   'SE','SW'.
             09  MSG-I-AD-SERV-STRUC             PIC X(15).
           07  MSG-I-RETR-WILD-KEYS.
             09  MSG-I-NM-CUST-2                 PIC X(28).
             09  MSG-I-AD-SERV-STR-NM            PIC X(23).
           07  MSG-I-RETR-NAME-KEYS.
             09  MSG-I-NM-CUST-1ST-1             PIC X(15).
             09  MSG-I-NM-CUST-MID-1             PIC X(15).
             09  MSG-I-NM-CUST-LST-1             PIC X(20).
             09  MSG-I-NM-CUST-TTL-1             PIC X(8).
             09  MSG-I-NM-CUST-SFIX-1            PIC X(4).
         05  MSG-I-K-CU04TB57.
           07  MSG-I-K457-PKEY-BUILDING.
             09  MSG-I-KY-BLDG-NO                PIC S9(9) COMP.
         05  MSG-I-CUR01K06.
           07  MSG-I-PKEY-SITE.
             09  MSG-I-KY-SITE-NO                PIC S9(9) COMP.
         05  MSG-I-SAD-TABLE-MAIN-GRP.
           07  MSG-I-KEY-SAD-TABLE-GROUP.
             09  MSG-I-KY-BA                     PIC S9(10) COMP-3.
             09  MSG-I-KY-CUST-NO                PIC S9(9) COMP.
             09  MSG-I-KY-PREM-NO                PIC S9(9) COMP.
             09  MSG-I-CD-ACCT-TYPE              PIC X(2).
               88  MSG-I-PEND-SERV                 VALUE '03'.
               88  MSG-I-DEFAULTED                 VALUE '  '.
               88  MSG-I-PEND-SERV-OTHER-ACT       VALUE '06'.
               88  MSG-I-PEND-FINAL-ELE            VALUE '12'.
               88  MSG-I-PEND-FINAL-ELE-ACT        VALUE '21'.
               88  MSG-I-FINAL-SERV-ACT            VALUE '42'.
               88  MSG-I-DMAG                      VALUE '48'.
               88  MSG-I-SUM-BILL                  VALUE '51'.
               88  MSG-I-PAL                       VALUE '54'.
               88  MSG-I-SL                        VALUE '57'.
               88  MSG-I-NONCONSUMER               VALUE '60'.
               88  MSG-I-CASH                      VALUE '63'.
               88  MSG-I-DEBITS                    VALUE '66'.
               88  MSG-I-UNCOLL-CASH               VALUE '69'.
               88  MSG-I-SERV-TYPE VALUE '12','21','18','33','42','09',
                   '15','24','27','30','36','39','54'.
               88  MSG-I-SERV-PEND                 VALUE '03', '06'.
               88  MSG-I-NON-SERV VALUE '45','48','60','72'.
               88  MSG-I-TAX-ONLY                  VALUE '72'.
           07  MSG-I-SAD-TABLE-SUB-GROUP.
             09  MSG-I-NM-CUST-1ST-1             PIC X(15).
             09  MSG-I-NM-CUST-MID-1             PIC X(15).
             09  MSG-I-NM-CUST-LST-1             PIC X(20).
             09  MSG-I-KY-SSN                    PIC S9(9) COMP.
             09  MSG-I-NM-CUST-TTL-1             PIC X(8).
             09  MSG-I-NM-CUST-SFIX-1            PIC X(4).
             09  MSG-I-KY-GRID-NO                PIC S9(14) COMP-3.
             09  MSG-I-AD-SERV-CITY              PIC X(19).
             09  MSG-I-AD-SERV-ST                PIC X(2).
             09  MSG-I-AD-SERV-ZIP               PIC S9(5) COMP-3.
             09  MSG-I-CD-GAS-MTR-SUM            PIC X(3).
             09  MSG-I-CD-ELE-MTR-SUM            PIC X(3).
             09  MSG-I-KY-OLD-ACCT-NO-1          PIC X(16).
             09  MSG-I-KY-GENER-AD-1             PIC X(7).
             09  MSG-I-KY-GENER-AD-2             PIC X(5).
             09  MSG-I-KY-GEN-NM-GRP.
               11  MSG-I-KY-GEN-NM-1             PIC X(8).
               11  MSG-I-KY-GEN-NM-2             PIC X(2).
             09  MSG-I-CD-SAD-STAT               PIC X(2).
               88  MSG-I-ACTIVE                    VALUE '03'.
               88  MSG-I-INACTIVE                  VALUE '06'.
               88  MSG-I-PEND-PREM                 VALUE '08'.
               88  MSG-I-PENDING                   VALUE '09'.
               88  MSG-I-VOID                      VALUE '12'.
               88  MSG-I-FINALED                   VALUE '15'.
               88  MSG-I-UNCOLL                    VALUE '18'.
               88  MSG-I-RO                        VALUE '21'.
               88  MSG-I-RECONC                    VALUE '24'.
               88  MSG-I-UNRECONC                  VALUE '27'.
               88  MSG-I-ESCHEAT                   VALUE '30'.
             09  MSG-I-NM-COMPRESSED             PIC X(56).
             09  MSG-I-AD-COMPRESSED             PIC X(69).
             09  MSG-I-CD-SUM-EXIST              PIC X(1).
               88  MSG-I-DEL                       VALUE 'D'.
               88  MSG-I-EXIST                     VALUE 'E'.
               88  MSG-I-CONV                      VALUE 'C'.
             09  MSG-I-CD-CIS-EXIST              PIC X(1).
               88  MSG-I-DOES-NOT-EXIST            VALUE 'D'.
               88  MSG-I-EXIST                     VALUE 'E'.
             09  MSG-I-CD-ACS-EXIST              PIC X(1).
               88  MSG-I-DOES-NOT-EXIST            VALUE 'D'.
               88  MSG-I-EXISTS                    VALUE 'E'.
             09  MSG-I-CD-NF-EXIST               PIC X(1).
             09  MSG-I-QY-LNGH-AD                PIC S9(2) COMP.
             09  MSG-I-KY-MTR-BILL-GRP           PIC S9(2) COMP-3.
             09  MSG-I-CD-CUST-INDIC             PIC S9(1) COMP-3.
               88  MSG-I-NO-CUST                   VALUE 0.
               88  MSG-I-PRIM                      VALUE 1.
               88  MSG-I-2ND                       VALUE 2.
             09  MSG-I-CD-ICS-EXIST              PIC X(1).
               88  MSG-I-EXIST                     VALUE 'E'.
               88  MSG-I-DOES-NOT-EXIST            VALUE 'D'.
             09  MSG-I-TX-HOME-ACD               PIC X(3).
             09  MSG-I-TX-HOME-PHN-NO            PIC X(8).
             09  MSG-I-TX-HOME-PHN-EXTN          PIC X(4).
             09  MSG-I-TX-BUS-ACD                PIC X(3).
             09  MSG-I-TX-BUS-PHN-NO             PIC X(8).
             09  MSG-I-TX-BUS-PHN-EXTN           PIC X(4).
             09  MSG-I-CD-COUNTY                 PIC X(3).
             09  MSG-I-CD-REGION                 PIC X(3).
             09  MSG-I-CD-OPER-CNTR              PIC X(3).
               88  MSG-I-GEN-OFFICE                VALUE 'GEN'.
             09  MSG-I-FL-CRITICAL-CONTCT        PIC X(1).
               88  MSG-I-CRITICAL                  VALUE 'Y'.
               88  MSG-I-NON-CRITICAL              VALUE 'N'.
             09  MSG-I-KY-AD                     PIC S9(9) COMP.
             09  MSG-I-CD-AD-TYPE                PIC X(1).
               88  MSG-I-PREM                      VALUE 'P'.
               88  MSG-I-BLDG                      VALUE 'B'.
               88  MSG-I-3RD-PARTY                 VALUE 'T'.
               88  MSG-I-MLNG-AD                   VALUE 'M'.
               88  MSG-I-GUAR                      VALUE 'G'.
               88  MSG-I-RFND                      VALUE 'R'.
               88  MSG-I-AGENCY                    VALUE 'A'.
               88  MSG-I-TEMPORARY                 VALUE 'Y'.
               88  MSG-I-CUSTOMER-88               VALUE 'C'.
               88  MSG-I-LANDLORD                  VALUE 'L'.
             09  MSG-I-KY-BLDG-NO                PIC S9(9) COMP.
             09  MSG-I-DT-STAT                   PIC X(10).
         05  MSG-I-CUSTOMER-GRP.
           07  MSG-I-NO-LOCK-SEQ-CUST            PIC S9(2) COMP-3.
           07  MSG-I-CD-CO                       PIC S9(4) COMP.
           07  MSG-I-KY-SSN                      PIC S9(9) COMP.
           07  MSG-I-TX-HOME-ACD                 PIC X(3).
           07  MSG-I-TX-HOME-PHN-NO              PIC X(8).
           07  MSG-I-TX-HOME-PHN-EXTN            PIC X(4).
           07  MSG-I-FL-UNLST-PHN-NO             PIC X(1).
           07  MSG-I-TX-BUS-ACD                  PIC X(3).
           07  MSG-I-TX-BUS-PHN-NO               PIC X(8).
           07  MSG-I-TX-BUS-PHN-EXTN             PIC X(4).
           07  MSG-I-FL-UNLST-BUS-PHN            PIC X(1).
           07  MSG-I-CD-CUST-STAT                PIC X(2).
               88  MSG-I-ACT                         VALUE '01'.
               88  MSG-I-PEND-ACT                    VALUE '02'.
               88  MSG-I-FINALED                     VALUE '03'.
           07  MSG-I-DT-CUST-STAT                PIC X(10).
           07  MSG-I-NM-CUST-1                   PIC X(28).
           07  MSG-I-NM-CUST-2                   PIC X(28).
           07  MSG-I-FL-QTE-DEP                  PIC X(1).
           07  MSG-I-KY-CNTL-UNIT                PIC S9(2) COMP-3.
           07  MSG-I-AT-HGST-CR-APPR             PIC S9(7)V9(2) COMP-3.
           07  MSG-I-CD-CUST-TYPE                PIC X(1).
               88  MSG-I-INDIVIDUAL                  VALUE 'I'.
               88  MSG-I-CORPORATION                 VALUE 'C'.
               88  MSG-I-NON-PROFIT                  VALUE 'N'.
               88  MSG-I-GOVT-MUNICIPAL              VALUE 'M'.
               88  MSG-I-GOVT-COUNTY                 VALUE 'Y'.
               88  MSG-I-GOVT-ST                     VALUE 'S'.
               88  MSG-I-GOVT-FED                    VALUE 'F'.
               88  MSG-I-FPC                         VALUE 'P'.
               88  MSG-I-OTHER-WHOLESALE             VALUE 'W'.
           07  MSG-I-CD-CITIZENSHIP              PIC X(1).
           07  MSG-I-CD-DO-NOT-QTE               PIC X(1).
               88  MSG-I-BUS-RSN                     VALUE '1'.
               88  MSG-I-HARDSHIP-CASE               VALUE '2'.
               88  MSG-I-HEALTH-RSN                  VALUE '3'.
               88  MSG-I-MED-FACIL                   VALUE '4'.
               88  MSG-I-CRT-ORD                     VALUE '5'.
               88  MSG-I-MGMT-DECISION               VALUE '6'.
               88  MSG-I-BUILD-REALTOR               VALUE '7'.
               88  MSG-I-ANY-VALID-CODE VALUE '1','2','3','4','5','6',
                   '7'.
           07  MSG-I-DT-CR-RPT-SECUR             PIC X(10).
           07  MSG-I-DT-MDSE-DECL                PIC X(10).
           07  MSG-I-DT-NOT-RESP-NTC             PIC X(10).
           07  MSG-I-CD-ORIG-ACT-AS              PIC X(1).
               88  MSG-I-1ST-TM-USER                 VALUE 'F'.
               88  MSG-I-USED-BEF                    VALUE 'U'.
               88  MSG-I-DEFAULT-VALU                VALUE ' '.
           07  MSG-I-DT-ORIG-MADE-ACT            PIC X(10).
           07  MSG-I-FL-SSN-NOT-PROV             PIC X(1).
           07  MSG-I-KY-AD                       PIC S9(9) COMP.
           07  MSG-I-CD-AD-TYPE                  PIC X(1).
               88  MSG-I-PREM                        VALUE 'P'.
               88  MSG-I-BLDG                        VALUE 'B'.
               88  MSG-I-3RD-PARTY                   VALUE 'T'.
               88  MSG-I-MLNG-AD                     VALUE 'M'.
               88  MSG-I-GUAR                        VALUE 'G'.
               88  MSG-I-RFND                        VALUE 'R'.
               88  MSG-I-AGENCY                      VALUE 'A'.
               88  MSG-I-TEMPORARY                   VALUE 'Y'.
               88  MSG-I-CUSTOMER-88                 VALUE 'C'.
               88  MSG-I-LANDLORD                    VALUE 'L'.
           07  MSG-I-CD-STAT                     PIC S9(2) COMP-3.
               88  MSG-I-INACT                       VALUE 01.
               88  MSG-I-ACT                         VALUE 02.
               88  MSG-I-PEND-ACT                    VALUE 03.
               88  MSG-I-GAS-ACT                     VALUE 04.
               88  MSG-I-ELECTRIC-ACT                VALUE 05.
               88  MSG-I-REMV                        VALUE 06.
               88  MSG-I-VOID                        VALUE 07.
               88  MSG-I-UNCOLL                      VALUE 08.
               88  MSG-I-FINALED                     VALUE 09.
               88  MSG-I-PEND-TRNSFR                 VALUE 10.
               88  MSG-I-TRNSFR                      VALUE 11.
               88  MSG-I-CNCLD                       VALUE 12.
               88  MSG-I-RECONCILE                   VALUE 20.
               88  MSG-I-RECONCILE-INSERT            VALUE 21.
               88  MSG-I-UNRECONCILE                 VALUE 22.
               88  MSG-I-DELETED                     VALUE 99.
           07  MSG-I-DT-STAT                     PIC X(10).
         05  MSG-I-BUILDING-GRP.
           07  MSG-I-NM-BUILDING                 PIC X(30).
           07  MSG-I-CD-BLDG-TYPE                PIC X(2).
               88  MSG-I-SINGLE-FAMILY               VALUE '01'.
               88  MSG-I-MULTI-UNIT-RESIDENTIAL      VALUE '02'.
               88  MSG-I-MOBILE-HOME                 VALUE '03'.
               88  MSG-I-SHOPPING-CENTER             VALUE '04'.
               88  MSG-I-WAREHOUSE                   VALUE '05'.
               88  MSG-I-FACTORY                     VALUE '06'.
               88  MSG-I-HOSPITAL                    VALUE '07'.
               88  MSG-I-SCHOOL                      VALUE '08'.
               88  MSG-I-OTHER-COMMERCIAL            VALUE '09'.
               88  MSG-I-NURSING-HOME                VALUE '10'.
               88  MSG-I-TEMPORARY-SERVICE           VALUE '11'.
               88  MSG-I-GOVERNMENTAL                VALUE '12'.
           07  MSG-I-FL-MULT-PREMISE             PIC X(1).
               88  MSG-I-FL-MULT-PREMISE-Y           VALUE 'Y'.
               88  MSG-I-FL-MULT-PREMISE-N           VALUE 'N'.
           07  MSG-I-CD-CITY-CNTY                PIC X(4).
           07  MSG-I-CD-OPER-CNTR                PIC X(3).
               88  MSG-I-GEN-OFFICE                  VALUE 'GEN'.
           07  MSG-I-KY-GWA                      PIC X(5).
           07  MSG-I-NO-LOT                      PIC X(20).
           07  MSG-I-NO-BLOCK                    PIC X(20).
           07  MSG-I-KY-STR-NM                   PIC S9(7) COMP-3.
           07  MSG-I-AD-SERV-STR-NO              PIC X(15).
           07  MSG-I-AD-SERV-CITY                PIC X(19).
           07  MSG-I-AD-SERV-ST                  PIC X(2).
           07  MSG-I-AD-NOTE                     PIC X(20).
           07  MSG-I-CD-STAR-RATING              PIC X(1).
           07  MSG-I-NM-CONTACT                  PIC X(20).
           07  MSG-I-TX-CNTC-ACD                 PIC X(3).
           07  MSG-I-TX-CNTC-PHN-NO              PIC X(8).
           07  MSG-I-TX-CNTC-EXTN                PIC X(4).
           07  MSG-I-KY-MRDG-ASGNMT              PIC S9(3) COMP-3.
           07  MSG-I-KY-MTR-BILL-GRP             PIC S9(2) COMP-3.
           07  MSG-I-KY-RMRK-BLDG-DIR            PIC S9(9) COMP.
           07  MSG-I-KY-CNTL-UNIT                PIC S9(2) COMP-3.
           07  MSG-I-CD-BLDG-STAT                PIC X(2).
               88  MSG-I-BLDG-INACT                  VALUE '01'.
               88  MSG-I-BLDG-ACT                    VALUE '02'.
               88  MSG-I-BLDG-PEND-ACT               VALUE '03'.
               88  MSG-I-BLDG-VOID                   VALUE '07'.
               88  MSG-I-BLDG-PENDING                VALUE '08'.
               88  MSG-I-BLDG-DELETE-CONV-ONLY       VALUE '09'.
           07  MSG-I-CD-REPORT-LEVEL             PIC X(5).
               88  MSG-I-RPT-LVL-APK                 VALUE 'APK'.
               88  MSG-I-GEN-OFFICE                  VALUE 'GEN'.
               88  MSG-I-RPT-LVL-APL                 VALUE 'APL'.
               88  MSG-I-RPT-LVL-AVP                 VALUE 'AVP'.
               88  MSG-I-RPT-LVL-BKS                 VALUE 'BKS'.
               88  MSG-I-RPT-LVL-BLG                 VALUE 'BLG'.
               88  MSG-I-RPT-LVL-BNV                 VALUE 'BNV'.
               88  MSG-I-RPT-LVL-CAR                 VALUE 'CAR'.
               88  MSG-I-RPT-LVL-CLR                 VALUE 'CLR'.
               88  MSG-I-RPT-LVL-CLW                 VALUE 'CLW'.
               88  MSG-I-RPT-LVL-CRC                 VALUE 'CRC'.
               88  MSG-I-RPT-LVL-CRV                 VALUE 'CRV'.
               88  MSG-I-RPT-LVL-CRW                 VALUE 'CRW'.
               88  MSG-I-RPT-LVL-DEL                 VALUE 'DEL'.
               88  MSG-I-RPT-LVL-DUN                 VALUE 'DUN'.
               88  MSG-I-RPT-LVL-EUS                 VALUE 'EUS'.
               88  MSG-I-RPT-LVL-FRP                 VALUE 'FRP'.
               88  MSG-I-RPT-LVL-HNC                 VALUE 'HNC'.
               88  MSG-I-RPT-LVL-HSP                 VALUE 'HSP'.
               88  MSG-I-RPT-LVL-INV                 VALUE 'INV'.
               88  MSG-I-RPT-LVL-JAM                 VALUE 'JAM'.
               88  MSG-I-RPT-LVL-JAS                 VALUE 'JAS'.
               88  MSG-I-RPT-LVL-LKP                 VALUE 'LKP'.
               88  MSG-I-RPT-LVL-LKW                 VALUE 'LKW'.
               88  MSG-I-RPT-LVL-MAD                 VALUE 'MAD'.
               88  MSG-I-RPT-LVL-MON                 VALUE 'MON'.
               88  MSG-I-RPT-LVL-OKL                 VALUE 'OKL'.
               88  MSG-I-RPT-LVL-PRY                 VALUE 'PRY'.
               88  MSG-I-RPT-LVL-PSJ                 VALUE 'PSJ'.
               88  MSG-I-RPT-LVL-RED                 VALUE 'RED'.
               88  MSG-I-RPT-LVL-STP                 VALUE 'STP'.
               88  MSG-I-RPT-LVL-TRN                 VALUE 'TRN'.
               88  MSG-I-RPT-LVL-TRP                 VALUE 'TRP'.
               88  MSG-I-RPT-LVL-WAL                 VALUE 'WAL'.
               88  MSG-I-RPT-LVL-WLD                 VALUE 'WLD'.
               88  MSG-I-RPT-LVL-ZEP                 VALUE 'ZEP'.
           07  MSG-I-CD-BTU-ZONE                 PIC X(2).
         05  MSG-I-PREMISE-GRP.
           07  MSG-I-NO-LOCK-SEQ-PREM            PIC S9(2) COMP-3.
           07  MSG-I-KY-BLDG-NO                  PIC S9(9) COMP.
           07  MSG-I-CD-CO                       PIC S9(4) COMP.
           07  MSG-I-KY-MRDG-ASGNMT              PIC S9(3) COMP-3.
           07  MSG-I-KY-MRDG-SEQ-NO              PIC S9(4) COMP.
           07  MSG-I-CD-SIC                      PIC X(6).
           07  MSG-I-CD-PREM-STAT                PIC X(2).
               88  MSG-I-PREM-INACT                  VALUE '01'.
               88  MSG-I-PREM-ACT                    VALUE '02'.
               88  MSG-I-PREM-PEND-ACT               VALUE '03'.
               88  MSG-I-PREM-VOID                   VALUE '07'.
               88  MSG-I-PREM-PENDING                VALUE '08'.
               88  MSG-I-PREM-DELETE-CONV-ONLY       VALUE '09'.
           07  MSG-I-KY-GRID-NO                  PIC S9(14) COMP-3.
           07  MSG-I-AD-SERV-STR-NO              PIC X(15).
           07  MSG-I-AD-SERV-STRUC               PIC X(15).
           07  MSG-I-AD-NOTE                     PIC X(20).
           07  MSG-I-DT-MTR-GRP-EFF              PIC X(10).
           07  MSG-I-KY-MTR-BILL-GRP             PIC S9(2) COMP-3.
           07  MSG-I-NO-KY                       PIC S9(5) COMP-3.
           07  MSG-I-KY-STR-NM                   PIC S9(7) COMP-3.
           07  MSG-I-KY-CNTL-UNIT                PIC S9(2) COMP-3.
           07  MSG-I-QY-CONSC-KY-NO-USE          PIC S9(2) COMP-3.
           07  MSG-I-CD-PREM-ENTR                PIC X(1).
           07  MSG-I-CD-PREM-TYPE                PIC X(2).
               88  MSG-I-SINGLE-FAMILY               VALUE '01'.
               88  MSG-I-APARTMENT                   VALUE '02'.
               88  MSG-I-CONDOMINIUM                 VALUE '03'.
               88  MSG-I-TOWNHOUSE                   VALUE '04'.
               88  MSG-I-MOBILE-HOME                 VALUE '05'.
               88  MSG-I-OFFICE                      VALUE '06'.
               88  MSG-I-HOSPITAL                    VALUE '07'.
               88  MSG-I-SCHOOL                      VALUE '08'.
               88  MSG-I-CHURCH                      VALUE '09'.
               88  MSG-I-COMMON-USE-FACILITIES       VALUE '10'.
               88  MSG-I-TEMPORARY-SERVICE           VALUE '11'.
               88  MSG-I-UNMETERED                   VALUE '12'.
               88  MSG-I-OTHER-TYPE                  VALUE '13'.
               88  MSG-I-NON-SERVICE                 VALUE '14'.
               88  MSG-I-SUBORDINATE                 VALUE '15'.
               88  MSG-I-RETAIL                      VALUE '16'.
               88  MSG-I-MANUFACTURING               VALUE '17'.
               88  MSG-I-REGULAR VALUE '01','02','03','04','05','06',
                   '07','08','09','10','11','13'.
               88  MSG-I-WAREHOUSE                   VALUE '18'.
           07  MSG-I-DT-PREM-STAT                PIC X(10).
           07  MSG-I-KY-RMRK-PREM-DIR            PIC S9(9) COMP.
           07  MSG-I-DT-FIRST-CONNECT            PIC X(10).
           07  MSG-I-CD-OPER-CNTR                PIC X(3).
               88  MSG-I-GEN-OFFICE                  VALUE 'GEN'.
           07  MSG-I-CD-REPORT-LEVEL             PIC X(5).
               88  MSG-I-RPT-LVL-APK                 VALUE 'APK'.
               88  MSG-I-GEN-OFFICE                  VALUE 'GEN'.
               88  MSG-I-RPT-LVL-APL                 VALUE 'APL'.
               88  MSG-I-RPT-LVL-AVP                 VALUE 'AVP'.
               88  MSG-I-RPT-LVL-BKS                 VALUE 'BKS'.
               88  MSG-I-RPT-LVL-BLG                 VALUE 'BLG'.
               88  MSG-I-RPT-LVL-BNV                 VALUE 'BNV'.
               88  MSG-I-RPT-LVL-CAR                 VALUE 'CAR'.
               88  MSG-I-RPT-LVL-CLR                 VALUE 'CLR'.
               88  MSG-I-RPT-LVL-CLW                 VALUE 'CLW'.
               88  MSG-I-RPT-LVL-CRC                 VALUE 'CRC'.
               88  MSG-I-RPT-LVL-CRV                 VALUE 'CRV'.
               88  MSG-I-RPT-LVL-CRW                 VALUE 'CRW'.
               88  MSG-I-RPT-LVL-DEL                 VALUE 'DEL'.
               88  MSG-I-RPT-LVL-DUN                 VALUE 'DUN'.
               88  MSG-I-RPT-LVL-EUS                 VALUE 'EUS'.
               88  MSG-I-RPT-LVL-FRP                 VALUE 'FRP'.
               88  MSG-I-RPT-LVL-HNC                 VALUE 'HNC'.
               88  MSG-I-RPT-LVL-HSP                 VALUE 'HSP'.
               88  MSG-I-RPT-LVL-INV                 VALUE 'INV'.
               88  MSG-I-RPT-LVL-JAM                 VALUE 'JAM'.
               88  MSG-I-RPT-LVL-JAS                 VALUE 'JAS'.
               88  MSG-I-RPT-LVL-LKP                 VALUE 'LKP'.
               88  MSG-I-RPT-LVL-LKW                 VALUE 'LKW'.
               88  MSG-I-RPT-LVL-MAD                 VALUE 'MAD'.
               88  MSG-I-RPT-LVL-MON                 VALUE 'MON'.
               88  MSG-I-RPT-LVL-OKL                 VALUE 'OKL'.
               88  MSG-I-RPT-LVL-PRY                 VALUE 'PRY'.
               88  MSG-I-RPT-LVL-PSJ                 VALUE 'PSJ'.
               88  MSG-I-RPT-LVL-RED                 VALUE 'RED'.
               88  MSG-I-RPT-LVL-STP                 VALUE 'STP'.
               88  MSG-I-RPT-LVL-TRN                 VALUE 'TRN'.
               88  MSG-I-RPT-LVL-TRP                 VALUE 'TRP'.
               88  MSG-I-RPT-LVL-WAL                 VALUE 'WAL'.
               88  MSG-I-RPT-LVL-WLD                 VALUE 'WLD'.
               88  MSG-I-RPT-LVL-ZEP                 VALUE 'ZEP'.
         05  MSG-I-BILL-ACCT-GRP.
           07  MSG-I-BILL-ACCT-GRP-1.
             09  MSG-I-NO-LOCK-SEQ-BA            PIC S9(2) COMP-3.
             09  MSG-I-NO-LOCK-SEQ-BA-AIM        PIC S9(2) COMP-3.
             09  MSG-I-KY-CUST-NO                PIC S9(9) COMP.
             09  MSG-I-KY-PREM-NO                PIC S9(9) COMP.
             09  MSG-I-CD-CO                     PIC S9(4) COMP.
             09  MSG-I-CD-COMPNY-FACILITY        PIC X(2).
               88  MSG-I-VS-COMP-STOREROOM         VALUE '01'.
             09  MSG-I-CD-EMP                    PIC X(2).
               88  MSG-I-ALABAMA-POWER             VALUE '01'.
             09  MSG-I-CD-BA-TRNSFR-TYPE         PIC X(2).
               88  MSG-I-INCONTIN                  VALUE '01'.
               88  MSG-I-ALSO-USES                 VALUE '03'.
               88  MSG-I-CHARGE-ON                 VALUE '05'.
               88  MSG-I-REBILL                    VALUE '06'.
             09  MSG-I-DT-NXT-SCH-RDG            PIC X(10).
             09  MSG-I-DT-LST-BLLD               PIC X(10).
             09  MSG-I-DT-NXT-BILL               PIC X(10).
             09  MSG-I-DT-BA-OPEN                PIC X(10).
             09  MSG-I-DT-BA-TERM                PIC X(10).
             09  MSG-I-DT-CNTL-UNIT-BAL          PIC X(10).
             09  MSG-I-DT-CUST-AWAY-BEG          PIC X(10).
             09  MSG-I-DT-CUST-AWAY-END          PIC X(10).
             09  MSG-I-DT-ON-DEM-BAL             PIC X(10).
             09  MSG-I-DT-BILL-CUTOFF            PIC X(10).
             09  MSG-I-DT-LST-REG                PIC X(10).
             09  MSG-I-DT-RT-FOLLOW-EFF          PIC X(10).
             09  MSG-I-CD-RT-TO-FOLLOW           PIC X(1).
               88  MSG-I-GAS-RTF                   VALUE 'G'.
               88  MSG-I-ELE-RTF                   VALUE 'E'.
               88  MSG-I-COMB-RTF                  VALUE 'C'.
               88  MSG-I-RTF-CD-SHOULD-REMV        VALUE 'X'.
             09  MSG-I-CD-RDG-INSTR              PIC X(2).
               88  MSG-I-DO-NOT-EST-TEMP           VALUE '01'.
               88  MSG-I-ACCT-CD-A-DEC             VALUE '02'.
               88  MSG-I-DO-NOT-EST-PERM           VALUE '03'.
               88  MSG-I-DO-NOT-EST-NO-POST-TEMP   VALUE '04'.
               88  MSG-I-ACCT-CD-D-DEC             VALUE '05'.
               88  MSG-I-DO-NOT-EST-NO-POST-PERM   VALUE '06'.
               88  MSG-I-DO-NOT-USE-ESTIMATE VALUE '01','02','03','04',
                   '05','06','07'.
               88  MSG-I-NO-POST-PERM              VALUE '07'.
             09  MSG-I-CD-BA-STAT                PIC X(2).
               88  MSG-I-ACT                       VALUE '02'.
               88  MSG-I-PEND-ACT                  VALUE '03'.
               88  MSG-I-PEND-TRNSFR               VALUE '04'.
               88  MSG-I-VOIDED                    VALUE '07'.
               88  MSG-I-FINALED                   VALUE '09'.
               88  MSG-I-UNCOLL                    VALUE '18'.
               88  MSG-I-NOT-VOID VALUE '02','03','04','09','18','30'.
               88  MSG-I-ESCHEAT                   VALUE '30'.
             09  MSG-I-CD-BILL-PRCS-INSTR        PIC X(4).
               88  MSG-I-NO-BILL-INSTR             VALUE '0000'.
               88  MSG-I-BILL                      VALUE '0100'.
               88  MSG-I-REBILL                    VALUE '0200'.
               88  MSG-I-BILL-FINAL                VALUE '0300'.
               88  MSG-I-REBILL-FINAL              VALUE '0400'.
               88  MSG-I-BILL-FINAL-COLL           VALUE '0500'.
               88  MSG-I-UPDATED                   VALUE '0600'.
               88  MSG-I-UPDATED-FINAL             VALUE '0700'.
             09  MSG-I-CD-SO-PRCS-INSTR          PIC X(3).
               88  MSG-I-NO-SO-PEND                VALUE '000'.
               88  MSG-I-BILL                      VALUE '100'.
               88  MSG-I-BILL-FINAL                VALUE '300'.
               88  MSG-I-PEND-TRNSFR               VALUE '700'.
             09  MSG-I-CD-TERM-TYPE-GAS          PIC X(2).
             09  MSG-I-CD-TERM-TYPE-ELE          PIC X(2).
             09  MSG-I-FL-PERM-BILL-RLSE         PIC X(1).
             09  MSG-I-KY-OLD-ACCTNO             PIC X(16).
             09  MSG-I-FL-SO-PEND                PIC X(1).
             09  MSG-I-KY-MTR-BILL-GRP           PIC S9(2) COMP-3.
             09  MSG-I-AT-PEND-SALE              PIC S9(7)V9(2) COMP-3.
             09  MSG-I-CD-BILL-LITR              PIC X(1).
             09  MSG-I-QY-BILL-COPY              PIC S9(1) COMP-3.
             09  MSG-I-CD-BILL-EXTN              PIC X(1).
               88  MSG-I-NOT-ON-EXTN               VALUE '0'.
               88  MSG-I-CUST-LESS-60-MO           VALUE '1'.
               88  MSG-I-CUST-LESS-60-1-YR         VALUE '2'.
               88  MSG-I-CUST-LESS-60-2-YR         VALUE '3'.
               88  MSG-I-CUST-LESS-60-3-YR         VALUE '4'.
               88  MSG-I-CUST-LESS-60-4-YR         VALUE '5'.
               88  MSG-I-CUST-OVER-60              VALUE '9'.
             09  MSG-I-FL-ELE-TRNSFR             PIC X(1).
             09  MSG-I-FL-DEFR-RDG               PIC X(1).
             09  MSG-I-FL-OUT-OF-TOWN            PIC X(1).
             09  MSG-I-CD-BILL-SAC               PIC X(1).
               88  MSG-I-ELE                       VALUE 'E'.
               88  MSG-I-GAS                       VALUE 'G'.
               88  MSG-I-ELE-GAS                   VALUE 'C'.
             09  MSG-I-CD-ACCT-TYPE              PIC X(2).
               88  MSG-I-PEND-SERV                 VALUE '03'.
               88  MSG-I-DEFAULTED                 VALUE '  '.
               88  MSG-I-PEND-SERV-OTHER-ACT       VALUE '06'.
               88  MSG-I-PEND-FINAL-ELE            VALUE '12'.
               88  MSG-I-PEND-FINAL-ELE-ACT        VALUE '21'.
               88  MSG-I-FINAL-SERV-ACT            VALUE '42'.
               88  MSG-I-DMAG                      VALUE '48'.
               88  MSG-I-SUM-BILL                  VALUE '51'.
               88  MSG-I-PAL                       VALUE '54'.
               88  MSG-I-SL                        VALUE '57'.
               88  MSG-I-NONCONSUMER               VALUE '60'.
               88  MSG-I-CASH                      VALUE '63'.
               88  MSG-I-DEBITS                    VALUE '66'.
               88  MSG-I-UNCOLL-CASH               VALUE '69'.
               88  MSG-I-SERV-TYPE VALUE '12','21','18','33','42','09',
                   '15','24','27','30','36','39','54'.
               88  MSG-I-SERV-PEND                 VALUE '03', '06'.
               88  MSG-I-NON-SERV VALUE '45','48','60','72'.
               88  MSG-I-TAX-ONLY                  VALUE '72'.
             09  MSG-I-CD-WAIVE-RSN              PIC X(1).
               88  MSG-I-UTIL-RF                   VALUE 'U'.
               88  MSG-I-DIST-WAIVE                VALUE 'W'.
               88  MSG-I-DEP-REFUND                VALUE 'R'.
               88  MSG-I-DEP-ALT                   VALUE 'A'.
               88  MSG-I-PEND-TRANSFER             VALUE 'P'.
               88  MSG-I-CREDIT-SCORING            VALUE 'C'.
             09  MSG-I-AT-OPEN-MO-BAL            PIC S9(9)V9(2) COMP-3.
             09  MSG-I-AT-MO-ACT                 PIC S9(9)V9(2) COMP-3.
             09  MSG-I-KY-CNTL-UNIT              PIC S9(2) COMP-3.
             09  MSG-I-KY-AD                     PIC S9(9) COMP.
             09  MSG-I-DT-CURR-RELAT-CYC         PIC S9(6) COMP-3.
             09  MSG-I-FL-OB                     PIC X(1).
             09  MSG-I-FL-RFND-UNCOLL            PIC X(1).
             09  MSG-I-CD-AD-TYPE                PIC X(1).
               88  MSG-I-PREM                      VALUE 'P'.
               88  MSG-I-BLDG                      VALUE 'B'.
               88  MSG-I-3RD-PARTY                 VALUE 'T'.
               88  MSG-I-MLNG-AD                   VALUE 'M'.
               88  MSG-I-GUAR                      VALUE 'G'.
               88  MSG-I-RFND                      VALUE 'R'.
               88  MSG-I-AGENCY                    VALUE 'A'.
               88  MSG-I-TEMPORARY                 VALUE 'Y'.
               88  MSG-I-CUSTOMER-88               VALUE 'C'.
               88  MSG-I-LANDLORD                  VALUE 'L'.
             09  MSG-I-KY-LST-USE-NO             PIC S9(5) COMP-3.
             09  MSG-I-CD-PRMTL-LITR             PIC S9(1) COMP-3.
             09  MSG-I-FL-TEMP-BILL-RLSE         PIC X(1).
             09  MSG-I-FL-MULT-CUST              PIC X(1).
             09  MSG-I-FL-SUM-BILL               PIC X(1).
           07  MSG-I-BILL-ACCT-GRP-2.
             09  MSG-I-AT-EXCR-BA                PIC S9(8)V9(2) COMP-3.
             09  MSG-I-AT-RFND-BA                PIC S9(8)V9(2) COMP-3.
             09  MSG-I-AT-BB-BLLD-BA             PIC S9(7)V9(2) COMP-3.
             09  MSG-I-AT-DEP-ON-HAND-BA         PIC S9(7)V9(2) COMP-3.
             09  MSG-I-AT-UNBIL-BAL-BA           PIC S9(8)V9(2) COMP-3.
             09  MSG-I-CD-MADE-ACT-AS            PIC X(1).
               88  MSG-I-INCONT                    VALUE 'C'.
               88  MSG-I-1ST-TM-USER               VALUE 'F'.
               88  MSG-I-USE-BEFORE                VALUE 'U'.
               88  MSG-I-ALSO-USES                 VALUE 'A'.
             09  MSG-I-CD-RTF-SO-TYPE            PIC S9(2) COMP-3.
               88  MSG-I-SETT                      VALUE 01.
               88  MSG-I-OTHR                      VALUE 02.
             09  MSG-I-CD-RES-COMM               PIC X(1).
               88  MSG-I-TAR-MIXED                 VALUE 'M'.
               88  MSG-I-TAR-RES                   VALUE 'R'.
               88  MSG-I-TAR-COMM                  VALUE 'C'.
             09  MSG-I-TM-CNTL-UNIT-BAL          PIC X(8).
             09  MSG-I-FL-PAD                    PIC X(1).
             09  MSG-I-CD-PAD-STAT               PIC S9(2) COMP.
             09  MSG-I-NO-PEND-BILL-GRP          PIC S9(2) COMP.
             09  MSG-I-DT-CYC-CHG                PIC X(10).
             09  MSG-I-CD-ACCT-HANDLING          PIC X(1).
               88  MSG-I-REGULAR                   VALUE 'R'.
               88  MSG-I-SPECIAL                   VALUE 'S'.
               88  MSG-I-PRE-CAL                   VALUE 'P'.
             09  MSG-I-NO-CC-TAX-EX-CERT         PIC X(13).
             09  MSG-I-DT-CC-TAX-EX-EXP          PIC X(10).
             09  MSG-I-CD-TAR-TYPE               PIC X(2).
               88  MSG-I-RES                       VALUE '01'.
               88  MSG-I-COMM                      VALUE '03'.
               88  MSG-I-INDUST                    VALUE '05'.
               88  MSG-I-STRT-AND-HWY-LT           VALUE '06'.
               88  MSG-I-SALES-TO-PUBL-AUTH        VALUE '07'.
               88  MSG-I-RURAL-ELE-COOP            VALUE '08'.
               88  MSG-I-MUNICIPAL                 VALUE '10'.
               88  MSG-I-SERVICE-CHARGE            VALUE '14'.
               88  MSG-I-EQUIPMENT-RENTALS         VALUE '15'.
             09  MSG-I-CD-TAX-AUTHORITY          PIC X(1).
             09  MSG-I-CD-CASH-ONLY-RES          PIC X(2).
             09  MSG-I-CD-OPER-CNTR              PIC X(3).
               88  MSG-I-GEN-OFFICE                VALUE 'GEN'.
             09  MSG-I-CD-REPORT-LEVEL           PIC X(5).
               88  MSG-I-RPT-LVL-APK               VALUE 'APK'.
               88  MSG-I-GEN-OFFICE                VALUE 'GEN'.
               88  MSG-I-RPT-LVL-APL               VALUE 'APL'.
               88  MSG-I-RPT-LVL-AVP               VALUE 'AVP'.
               88  MSG-I-RPT-LVL-BKS               VALUE 'BKS'.
               88  MSG-I-RPT-LVL-BLG               VALUE 'BLG'.
               88  MSG-I-RPT-LVL-BNV               VALUE 'BNV'.
               88  MSG-I-RPT-LVL-CAR               VALUE 'CAR'.
               88  MSG-I-RPT-LVL-CLR               VALUE 'CLR'.
               88  MSG-I-RPT-LVL-CLW               VALUE 'CLW'.
               88  MSG-I-RPT-LVL-CRC               VALUE 'CRC'.
               88  MSG-I-RPT-LVL-CRV               VALUE 'CRV'.
               88  MSG-I-RPT-LVL-CRW               VALUE 'CRW'.
               88  MSG-I-RPT-LVL-DEL               VALUE 'DEL'.
               88  MSG-I-RPT-LVL-DUN               VALUE 'DUN'.
               88  MSG-I-RPT-LVL-EUS               VALUE 'EUS'.
               88  MSG-I-RPT-LVL-FRP               VALUE 'FRP'.
               88  MSG-I-RPT-LVL-HNC               VALUE 'HNC'.
               88  MSG-I-RPT-LVL-HSP               VALUE 'HSP'.
               88  MSG-I-RPT-LVL-INV               VALUE 'INV'.
               88  MSG-I-RPT-LVL-JAM               VALUE 'JAM'.
               88  MSG-I-RPT-LVL-JAS               VALUE 'JAS'.
               88  MSG-I-RPT-LVL-LKP               VALUE 'LKP'.
               88  MSG-I-RPT-LVL-LKW               VALUE 'LKW'.
               88  MSG-I-RPT-LVL-MAD               VALUE 'MAD'.
               88  MSG-I-RPT-LVL-MON               VALUE 'MON'.
               88  MSG-I-RPT-LVL-OKL               VALUE 'OKL'.
               88  MSG-I-RPT-LVL-PRY               VALUE 'PRY'.
               88  MSG-I-RPT-LVL-PSJ               VALUE 'PSJ'.
               88  MSG-I-RPT-LVL-RED               VALUE 'RED'.
               88  MSG-I-RPT-LVL-STP               VALUE 'STP'.
               88  MSG-I-RPT-LVL-TRN               VALUE 'TRN'.
               88  MSG-I-RPT-LVL-TRP               VALUE 'TRP'.
               88  MSG-I-RPT-LVL-WAL               VALUE 'WAL'.
               88  MSG-I-RPT-LVL-WLD               VALUE 'WLD'.
               88  MSG-I-RPT-LVL-ZEP               VALUE 'ZEP'.
             09  MSG-I-CD-BP-TABLE-ACCESS        PIC X(2).
               88  MSG-I-USE-DEFAULT-ONLY          VALUE '01'.
               88  MSG-I-USE-BILL-FORMAT-ONLY      VALUE '02'.
               88  MSG-I-USE-DEF-PLUS-BILL-FORM    VALUE '03'.
             09  MSG-I-FL-LPC-ELIGIBLE           PIC X(1).
             09  MSG-I-CD-STAT                   PIC S9(2) COMP-3.
               88  MSG-I-INACT                     VALUE 01.
               88  MSG-I-ACT                       VALUE 02.
               88  MSG-I-PEND-ACT                  VALUE 03.
               88  MSG-I-GAS-ACT                   VALUE 04.
               88  MSG-I-ELECTRIC-ACT              VALUE 05.
               88  MSG-I-REMV                      VALUE 06.
               88  MSG-I-VOID                      VALUE 07.
               88  MSG-I-UNCOLL                    VALUE 08.
               88  MSG-I-FINALED                   VALUE 09.
               88  MSG-I-PEND-TRNSFR               VALUE 10.
               88  MSG-I-TRNSFR                    VALUE 11.
               88  MSG-I-CNCLD                     VALUE 12.
               88  MSG-I-RECONCILE                 VALUE 20.
               88  MSG-I-RECONCILE-INSERT          VALUE 21.
               88  MSG-I-UNRECONCILE               VALUE 22.
               88  MSG-I-DELETED                   VALUE 99.
             09  MSG-I-DT-STAT                   PIC X(10).
         05  MSG-I-BILL-ACCT-DATA.
           07  MSG-I-CD-COLL-STAT-DETL           PIC X(3).
               88  MSG-I-IMP-NTC-SENT                VALUE '01A'.
               88  MSG-I-30DAY-IMP-NTC-SENT          VALUE '01B'.
               88  MSG-I-FRNDLY-NTC-SENT             VALUE '01C'.
               88  MSG-I-FINAL-NTC-SENT              VALUE '01D'.
               88  MSG-I-SPECIAL-WFM-SENT            VALUE '01E'.
               88  MSG-I-COLL-ARNG-IMP-SENT          VALUE '01G'.
               88  MSG-I-COLL-ARNG-30-IMP-SENT       VALUE '01H'.
               88  MSG-I-COLL-ARNG-FRNDLY-SENT       VALUE '01I'.
               88  MSG-I-ELIGIBLE-FOR-CUT            VALUE '02A'.
               88  MSG-I-COLL-ARNG-ELG-FOR-CUT       VALUE '02B'.
               88  MSG-I-DOOR-HANGER-ISSUED          VALUE '03A'.
               88  MSG-I-NOTIFIED-IN-FIELD           VALUE '03B'.
               88  MSG-I-WC-RQST-OFFICE-RVW          VALUE '03C'.
               88  MSG-I-CUT-ORDER-ISSUED            VALUE '04A'.
               88  MSG-I-CUT-OUT-NON-PAY             VALUE '04B'.
               88  MSG-I-EXPD-FROM-CUT-LIST          VALUE '04C'.
               88  MSG-I-RMVD-FROM-CUT-LIST          VALUE '04D'.
               88  MSG-I-INVESTIGATE-ISSUED          VALUE '06A'.
               88  MSG-I-INVESTIGATE-CMPLTD          VALUE '06B'.
               88  MSG-I-FINAL-ISSUED                VALUE '07A'.
               88  MSG-I-FINAL-NO-NTC-SENT           VALUE '07B'.
               88  MSG-I-REINSTATED                  VALUE '07D'.
               88  MSG-I-PNTL-CHRG-OFF-LIST          VALUE '08A'.
               88  MSG-I-HOLD-PNTL-CHRG-OFF          VALUE '08B'.
               88  MSG-I-RDY-CHRG-OFF-NO-AC          VALUE '08C'.
               88  MSG-I-CHRG-OFF-TO-AGNCY           VALUE '09A'.
               88  MSG-I-CHRG-OFF-NO-AGNCY           VALUE '09B'.
               88  MSG-I-PEND-CHRG-OFF-AGENCY        VALUE '09C'.
               88  MSG-I-BAD-DEBT                    VALUE '10A'.
               88  MSG-I-COLLCTN-ARRNGMNT            VALUE '11A'.
               88  MSG-I-ACCT-TRNSFRD-ON             VALUE '12A'.
               88  MSG-I-RMVD-FROM-COLL              VALUE '13A'.
               88  MSG-I-AT-MIN-ACT-COLL             VALUE '25A'.
               88  MSG-I-AT-MIN-FIN-COLL             VALUE '25B'.
               88  MSG-I-VALID-NOTICE-TYPE VALUE '01A','01B','01C',
                   '01D','01G','01H','01I'.
               88  MSG-I-VLD-NTC-CRITERIA VALUE '13A','04C','04D','01C',
                   '07C','07A','01E','11A','01I','03C','02A','02B',
                   '06B','01F'.
               88  MSG-I-VLD-LIST-CRITERIA VALUE '01A','01B','02A',
                   '08B','01F','01D','01G','01H','02B','07B','07D',
                   '03B','03C'.
               88  MSG-I-NOT-OPENED-PT               VALUE '26A'.
               88  MSG-I-PAID-ON-TIME-PT             VALUE '26B'.
               88  MSG-I-PAID-OVERDUE-PT             VALUE '26C'.
               88  MSG-I-PAID-OUTSTANDING-PT         VALUE '26D'.
               88  MSG-I-CUT-FOR-NON-PAY-PT          VALUE '26E'.
               88  MSG-I-RTRN-CHECK-PT               VALUE '26F'.
               88  MSG-I-HI-A-RANGE                  VALUE '27A'.
               88  MSG-I-HI-B-RANGE                  VALUE '27B'.
               88  MSG-I-HI-C-RANGE                  VALUE '27C'.
               88  MSG-I-HI-D-RANGE                  VALUE '27D'.
               88  MSG-I-HI-E-RANGE                  VALUE '27E'.
               88  MSG-I-HI-F-RANGE                  VALUE '27F'.
               88  MSG-I-FIRST-MB-WEIGHT             VALUE '28A'.
               88  MSG-I-SEC-THRD-MB-WEIGHT          VALUE '28B'.
               88  MSG-I-LO-DET-CR-VALUE             VALUE '26A'.
               88  MSG-I-HI-DET-CR-VALUE             VALUE '28F'.
               88  MSG-I-RDY-FOR-CHRGE-OFF           VALUE '08D'.
               88  MSG-I-AT-MIN-AGENCY-COLL          VALUE '25C'.
               88  MSG-I-RTND-CHK-NTC-SENT           VALUE '01F'.
               88  MSG-I-FINAL-NON-SERV-ISSUED       VALUE '07C'.
               88  MSG-I-DEP-LOWER-THRESHOLD         VALUE '50A'.
               88  MSG-I-COMMERCIAL-THRESHOLD        VALUE '51A'.
               88  MSG-I-DEP-UPPER-THRESHOLD         VALUE '50B'.
               88  MSG-I-DEPOSIT-ALT-THRESHOLD       VALUE '52A'.
               88  MSG-I-FRIENDLY-MESSAG VALUE '01C','01E','01I','06A',
                   '06B','07A','07B','07C','11A','13A'.
               88  MSG-I-SERIOUS-MESSAG VALUE '01A','01B','01D','01F',
                   '01G','01H','02A','02B','03A','03B','03C','04A',
                   '04B','04C','04D','08A','08B','08C','08D','09A',
                   '09B','09C','10A'.
               88  MSG-I-FINAL-COLL-STAT VALUE '01D','07A','07B','07C',
                   '07D','08A','08B','08C','08D','09A','09B','09C'.
           07  MSG-I-AD-LN-1                     PIC X(28).
           07  MSG-I-AD-LN-2                     PIC X(28).
           07  MSG-I-AD-LN-3                     PIC X(28).
           07  MSG-I-AD-LN-4                     PIC X(28).
           07  MSG-I-TX-ACCT-HANDLING            PIC X(15).
         05  MSG-I-SITE-GROUP.
           07  MSG-I-NM-SITE                     PIC X(56).
           07  MSG-I-CD-SITE-TYPE                PIC X(2).
               88  MSG-I-APARTMENT-COMPLEX           VALUE '01'.
               88  MSG-I-SUBDIVISION                 VALUE '02'.
               88  MSG-I-OFFICE-COMPLEX              VALUE '03'.
               88  MSG-I-CONDO                       VALUE '04'.
               88  MSG-I-MOBILE-HOME-PARK            VALUE '05'.
               88  MSG-I-SHOPPING-CENTER-MALL        VALUE '06'.
               88  MSG-I-OTHER-SITE-TYPE             VALUE '07'.
               88  MSG-I-NURSING-HOME                VALUE '08'.
               88  MSG-I-AMUSEMENT-PARK              VALUE '09'.
               88  MSG-I-COUNTRY-CLUB                VALUE '10'.
               88  MSG-I-MED-COMPLEX                 VALUE '11'.
               88  MSG-I-COLL-UNIV                   VALUE '12'.
               88  MSG-I-INDUST-PARK                 VALUE '13'.
               88  MSG-I-SCHOOL                      VALUE '14'.
               88  MSG-I-AIRPORT                     VALUE '15'.
               88  MSG-I-GOV-FACILITY                VALUE '16'.
           07  MSG-I-CD-OPER-CNTR                PIC X(3).
               88  MSG-I-GEN-OFFICE                  VALUE 'GEN'.
           07  MSG-I-NM-CONTACT                  PIC X(20).
           07  MSG-I-TX-CNTC-ACD                 PIC X(3).
           07  MSG-I-TX-CNTC-PHN-NO              PIC X(8).
           07  MSG-I-TX-CNTC-EXTN                PIC X(4).
           07  MSG-I-KY-RMRK-SITE-DIR            PIC S9(9) COMP.
           07  MSG-I-AD-SERV-CITY                PIC X(19).
           07  MSG-I-AD-SERV-ST                  PIC X(2).
           07  MSG-I-AD-SERV-ZIP                 PIC S9(5) COMP-3.
           07  MSG-I-KY-GEN-NM-1                 PIC X(8).
           07  MSG-I-KY-GEN-NM-2                 PIC X(2).
           07  MSG-I-FL-LM-ALLOW                 PIC X(1).
               88  MSG-I-FL-LM-ALLOW-Y               VALUE 'Y'.
               88  MSG-I-FL-LM-ALLOW-N               VALUE 'N'.
           07  MSG-I-FL-EE-ALLOW                 PIC X(1).
               88  MSG-I-FL-EE-ALLOW-Y               VALUE 'Y'.
               88  MSG-I-FL-EE-ALLOW-N               VALUE 'N'.
           07  MSG-I-CD-ROW-STAT                 PIC X(2).
               88  MSG-I-INACTIVE                    VALUE '01'.
               88  MSG-I-ACTIVE                      VALUE '02'.
               88  MSG-I-DELETED                     VALUE '99'.
           07  MSG-I-DT-ROW-STAT                 PIC X(10).
         05  MSG-I-RETR-DATA-1.
           07  MSG-I-QY-ROWS-RETURNED            PIC S9(3) COMP-3.
           07  MSG-I-FL-ADDR-IGNORED             PIC X(1).
               88  MSG-I-ADDR-IGNORED                VALUE 'Y'.
               88  MSG-I-ADDR-NOT-IGNORED            VALUE 'N'.
           07  MSG-I-FL-NM-CUST-IGNORED          PIC X(1).
               88  MSG-I-NM-CUST-IGNORED             VALUE 'Y'.
               88  MSG-I-NM-CUST-NOT-IGNORE          VALUE 'N'.
           07  MSG-I-FL-BA-IGNORED               PIC X(1).
               88  MSG-I-BA-IGNORED                  VALUE 'Y'.
               88  MSG-I-BA-NOT-IGNORED              VALUE 'N'.
           07  MSG-I-FL-SSN-IGNORED              PIC X(1).
               88  MSG-I-SSN-IGNORED                 VALUE 'Y'.
               88  MSG-I-SSN-NOT-IGNORED             VALUE 'N'.
           07  MSG-I-FL-NM-SITE-IGNORED          PIC X(1).
               88  MSG-I-NM-SITE-IGNORED             VALUE 'Y'.
               88  MSG-I-NM-SITE-NOT-IGNORED         VALUE 'N'.
           07  MSG-I-FL-OL-ACCT-IGNORED          PIC X(1).
               88  MSG-I-OL-ACCT-IGNORED             VALUE 'Y'.
               88  MSG-I-OL-ACCT-NOT-IGNORE          VALUE 'N'.
           07  MSG-I-FL-PHN-IGNORED              PIC X(1).
               88  MSG-I-PHN-IGNORED                 VALUE 'Y'.
               88  MSG-I-PHN-NOT-IGNORED             VALUE 'N'.
           07  MSG-I-FL-MTR-NO-IGNORED           PIC X(1).
               88  MSG-I-MTR-NO-IGNORED              VALUE 'Y'.
               88  MSG-I-MTR-NO-NOT-IGNORED          VALUE 'N'.
           07  MSG-I-FL-CITY-IGNORED             PIC X(1).
               88  MSG-I-CITY-IGNORED                VALUE 'Y'.
               88  MSG-I-CITY-NOT-IGNORED            VALUE 'N'.
           07  MSG-I-FL-COUNTY-IGNORED           PIC X(1).
               88  MSG-I-COUNTY-IGNORED              VALUE 'Y'.
               88  MSG-I-COUNTY-NOT-IGNORED          VALUE 'N'.
           07  MSG-I-FL-ZIP-IGNORED              PIC X(1).
               88  MSG-I-ZIP-IGNORED                 VALUE 'Y'.
               88  MSG-I-ZIP-NOT-IGNORED             VALUE 'N'.
           07  MSG-I-FL-SOUNDEX-IGNORED          PIC X(1).
               88  MSG-I-SOUNDEX-IGNORED             VALUE 'Y'.
               88  MSG-I-SOUNDX-NOT-IGNORED          VALUE 'N'.
           07  MSG-I-FL-STREET-NM-SUBST          PIC X(1).
           07  MSG-I-AD-SERV-STR-NO              PIC X(15).
           07  MSG-I-AD-SERV-STR-NM              PIC X(23).
           07  MSG-I-KY-PWQ-WO-ID                PIC X(5).
           07  MSG-I-CD-CNTC-TYPE-GRP
                            OCCURS 10 TIMES.
             09  MSG-I-CD-CNTC-TYPE              PIC X(4).
           07  MSG-I-CD-CRITICAL-ACCT            PIC X(2).
           07  MSG-I-KY-BA-2                     PIC S9(10) COMP-3.
           07  MSG-I-FL-RULE-DATA                PIC X(1).
           07  MSG-I-FL-SO-PEND-AT-PREM          PIC X(1).
         05  MSG-I-RETR-ROW
                            OCCURS 50 TIMES.
           07  MSG-I-KEY-SAD.
             09  MSG-I-KY-BA                     PIC S9(10) COMP-3.
             09  MSG-I-KY-CUST-NO                PIC S9(9) COMP.
             09  MSG-I-KY-PREM-NO                PIC S9(9) COMP.
             09  MSG-I-CD-ACCT-TYPE              PIC X(2).
               88  MSG-I-PEND-SERV                 VALUE '03'.
               88  MSG-I-DEFAULTED                 VALUE '  '.
               88  MSG-I-PEND-SERV-OTHER-ACT       VALUE '06'.
               88  MSG-I-PEND-FINAL-ELE            VALUE '12'.
               88  MSG-I-PEND-FINAL-ELE-ACT        VALUE '21'.
               88  MSG-I-FINAL-SERV-ACT            VALUE '42'.
               88  MSG-I-DMAG                      VALUE '48'.
               88  MSG-I-SUM-BILL                  VALUE '51'.
               88  MSG-I-PAL                       VALUE '54'.
               88  MSG-I-SL                        VALUE '57'.
               88  MSG-I-NONCONSUMER               VALUE '60'.
               88  MSG-I-CASH                      VALUE '63'.
               88  MSG-I-DEBITS                    VALUE '66'.
               88  MSG-I-UNCOLL-CASH               VALUE '69'.
               88  MSG-I-SERV-TYPE VALUE '12','21','18','33','42','09',
                   '15','24','27','30','36','39','54'.
               88  MSG-I-SERV-PEND                 VALUE '03', '06'.
               88  MSG-I-NON-SERV VALUE '45','48','60','72'.
               88  MSG-I-TAX-ONLY                  VALUE '72'.
           07  MSG-I-SAD.
             09  MSG-I-NM-CUST-1ST-1             PIC X(15).
             09  MSG-I-NM-CUST-MID-1             PIC X(15).
             09  MSG-I-NM-CUST-LST-1             PIC X(20).
             09  MSG-I-KY-SSN                    PIC S9(9) COMP.
             09  MSG-I-NM-CUST-TTL-1             PIC X(8).
             09  MSG-I-NM-CUST-SFIX-1            PIC X(4).
             09  MSG-I-KY-GRID-NO                PIC S9(14) COMP-3.
             09  MSG-I-AD-SERV-CITY              PIC X(19).
             09  MSG-I-AD-SERV-ST                PIC X(2).
             09  MSG-I-AD-SERV-ZIP               PIC S9(5) COMP-3.
             09  MSG-I-CD-GAS-MTR-SUM            PIC X(3).
             09  MSG-I-CD-ELE-MTR-SUM            PIC X(3).
             09  MSG-I-KY-OLD-ACCT-NO-1          PIC X(16).
             09  MSG-I-KY-GENER-AD-1             PIC X(7).
             09  MSG-I-KY-GENER-AD-2             PIC X(5).
             09  MSG-I-KY-GEN-NM.
               11  MSG-I-KY-GEN-NM-1             PIC X(8).
               11  MSG-I-KY-GEN-NM-2             PIC X(2).
             09  MSG-I-CD-SAD-STAT               PIC X(2).
               88  MSG-I-ACTIVE                    VALUE '03'.
               88  MSG-I-INACTIVE                  VALUE '06'.
               88  MSG-I-PEND-PREM                 VALUE '08'.
               88  MSG-I-PENDING                   VALUE '09'.
               88  MSG-I-VOID                      VALUE '12'.
               88  MSG-I-FINALED                   VALUE '15'.
               88  MSG-I-UNCOLL                    VALUE '18'.
               88  MSG-I-RO                        VALUE '21'.
               88  MSG-I-RECONC                    VALUE '24'.
               88  MSG-I-UNRECONC                  VALUE '27'.
               88  MSG-I-ESCHEAT                   VALUE '30'.
             09  MSG-I-NM-COMPRESSED             PIC X(56).
             09  MSG-I-AD-COMPRESSED             PIC X(69).
             09  MSG-I-CD-SUM-EXIST              PIC X(1).
               88  MSG-I-DEL                       VALUE 'D'.
               88  MSG-I-EXIST                     VALUE 'E'.
               88  MSG-I-CONV                      VALUE 'C'.
             09  MSG-I-CD-CIS-EXIST              PIC X(1).
               88  MSG-I-DOES-NOT-EXIST            VALUE 'D'.
               88  MSG-I-EXIST                     VALUE 'E'.
             09  MSG-I-CD-ACS-EXIST              PIC X(1).
               88  MSG-I-DOES-NOT-EXIST            VALUE 'D'.
               88  MSG-I-EXISTS                    VALUE 'E'.
             09  MSG-I-CD-NF-EXIST               PIC X(1).
             09  MSG-I-QY-LNGH-AD                PIC S9(2) COMP.
             09  MSG-I-KY-MTR-BILL-GRP           PIC S9(2) COMP-3.
             09  MSG-I-CD-CUST-INDIC             PIC S9(1) COMP-3.
               88  MSG-I-NO-CUST                   VALUE 0.
               88  MSG-I-PRIM                      VALUE 1.
               88  MSG-I-2ND                       VALUE 2.
             09  MSG-I-CD-ICS-EXIST              PIC X(1).
               88  MSG-I-EXIST                     VALUE 'E'.
               88  MSG-I-DOES-NOT-EXIST            VALUE 'D'.
             09  MSG-I-TX-HOME-ACD               PIC X(3).
             09  MSG-I-TX-HOME-PHN-NO            PIC X(8).
             09  MSG-I-TX-HOME-PHN-EXTN          PIC X(4).
             09  MSG-I-TX-BUS-ACD                PIC X(3).
             09  MSG-I-TX-BUS-PHN-NO             PIC X(8).
             09  MSG-I-TX-BUS-PHN-EXTN           PIC X(4).
             09  MSG-I-CD-COUNTY                 PIC X(3).
             09  MSG-I-CD-REGION                 PIC X(3).
             09  MSG-I-CD-OPER-CNTR              PIC X(3).
               88  MSG-I-GEN-OFFICE                VALUE 'GEN'.
             09  MSG-I-FL-CRITICAL-CONTCT        PIC X(1).
               88  MSG-I-CRITICAL                  VALUE 'Y'.
               88  MSG-I-NON-CRITICAL              VALUE 'N'.
             09  MSG-I-KY-AD                     PIC S9(9) COMP.
             09  MSG-I-CD-AD-TYPE                PIC X(1).
               88  MSG-I-PREM                      VALUE 'P'.
               88  MSG-I-BLDG                      VALUE 'B'.
               88  MSG-I-3RD-PARTY                 VALUE 'T'.
               88  MSG-I-MLNG-AD                   VALUE 'M'.
               88  MSG-I-GUAR                      VALUE 'G'.
               88  MSG-I-RFND                      VALUE 'R'.
               88  MSG-I-AGENCY                    VALUE 'A'.
               88  MSG-I-TEMPORARY                 VALUE 'Y'.
               88  MSG-I-CUSTOMER-88               VALUE 'C'.
               88  MSG-I-LANDLORD                  VALUE 'L'.
             09  MSG-I-KY-BLDG-NO                PIC S9(9) COMP.
             09  MSG-I-CD-CO                     PIC S9(4) COMP.
             09  MSG-I-CD-CUST-RESPNSBL          PIC X(2).
               88  MSG-I-PRIMARY                   VALUE '01'.
               88  MSG-I-SPOUSE                    VALUE '02'.
               88  MSG-I-THIRD-PARTY               VALUE '03'.
               88  MSG-I-ROOMMATE                  VALUE '04'.
               88  MSG-I-BILL-CONTACT              VALUE '05'.
               88  MSG-I-COMP-CONTACT              VALUE '06'.
               88  MSG-I-MARK-CONTACT              VALUE '07'.
               88  MSG-I-OTHR                      VALUE '08'.
             09  MSG-I-DT-STAT                   PIC X(10).
           07  MSG-I-KY-SITE-NO                  PIC S9(9) COMP.
           07  MSG-I-AD-LN                       PIC X(28).
           07  MSG-I-KY-GEN-AD-NUM-KEYS
                            OCCURS 12 TIMES.
             09  MSG-I-KY-GEN-AD-NUMERIC         PIC S9(2) COMP.
           07  MSG-I-RETR-WILD-KEYS.
             09  MSG-I-NM-CUST-2                 PIC X(28).
             09  MSG-I-AD-SERV-STR-NM            PIC X(23).

