      * MVS PORT-- CODE RUN THROUGH PROCESSOR
      *            on Fri Feb  7 10:16:43 2003

      *****************************************************************
      **                                                             **
      **           FOUNDATION FOR COOPERATIVE PROCESSING             **
      **                                                             **
      **                     COBOL  COPYBOOK                         **
      **                                                             **
      *****************************************************************
      **                                                             **
      **      COPYBOOK: CUCL107I                                     **
      **                                                             **
      **  COPYBOOK FOR: Copybook.CUCL107I MAINTAIN BILL ACCT         **
      **                                                             **
      **  GENERATED ON: Fri Jan 31 13:20:23 2003                     **
      **                                                             **
      **  SHORT DESCRIPTION: CUCL107I MAINTAIN BILL ACCT             **
      **                                                             **
      **            BY: NAZIRA__                                     **
      **                                                             **
      *****************************************************************
       03  MSG-I-MNT-BILL-ACCT.
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
         05  MSG-I-HDR-CUMCC001.
           07  MSG-I-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-I-TX-RQST-BY                  PIC X(28).
           07  MSG-I-TX-RQST-BY-PHN              PIC X(8).
         05  MSG-I-HDR-CUMCC002.
           07  MSG-I-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-I-CD-FUNCTION                 PIC X(2).
               88  MSG-I-FUNC-CD-SELECT              VALUE 'S'.
               88  MSG-I-FUNC-CD-INSERT              VALUE 'I'.
               88  MSG-I-FUNC-CD-UPDATE              VALUE 'U'.
               88  MSG-I-FUNC-CD-DELETE              VALUE 'D'.
               88  MSG-I-FUNC-CD-SELECT-LESS-THAN    VALUE 'SL'.
               88  MSG-I-FUNC-CD-SELECT-GTR-THAN     VALUE 'SG'.
           07  MSG-I-KY-REMARKS                  PIC S9(9) COMP.
           07  MSG-I-TX-REMARKS-1                PIC X(360).
           07  MSG-I-DT-GREGORIAN                PIC X(10).
         05  MSG-I-HDR-CUMSO005.
           07  MSG-I-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-I-CD-ADDR-FUNC                PIC X(1).
               88  MSG-I-ADD                         VALUE 'A'.
               88  MSG-I-UPDATE                      VALUE 'U'.
               88  MSG-I-DELETE                      VALUE 'D'.
               88  MSG-I-ADD-ONLY                    VALUE 'O'.
               88  MSG-I-REPLACE-BILLING             VALUE 'R'.
           07  MSG-I-KY-BA                       PIC S9(10) COMP-3.
           07  MSG-I-KY-CUST-NO                  PIC S9(9) COMP.
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
               88  MSG-I-OWNER                       VALUE 'O'.
           07  MSG-I-CD-RTRN-MAIL-PRCS           PIC X(4).
               88  MSG-I-SUCC-CMPLT                  VALUE '0000'.
               88  MSG-I-MSNG-REQD-FLD               VALUE '1111'.
               88  MSG-I-INVAL-FUNC                  VALUE '2222'.
               88  MSG-I-CUST-ADDR-BEING-USED        VALUE '3333'.
               88  MSG-I-PGM-ABEND                   VALUE '9999'.
           07  MSG-I-DT-GREGORIAN                PIC X(10).
         05  MSG-I-HDR-CUMGM003.
           07  MSG-I-QY-ALR-ACL-CNT              PIC S9(3) COMP-3.
           07  MSG-I-KY-BA                       PIC S9(10) COMP-3.
           07  MSG-I-KY-CUST-NO                  PIC S9(9) COMP.
           07  MSG-I-CD-BP-TABLE-ACCESS          PIC X(2).
               88  MSG-I-USE-DEFAULT-ONLY            VALUE '01'.
               88  MSG-I-USE-BILL-FORMAT-ONLY        VALUE '02'.
               88  MSG-I-USE-DEF-PLUS-BILL-FORM      VALUE '03'.
         05  MSG-I-HDR-CU0234I.
           07  MSG-I-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-I-QY-ALR-ACL-CNT              PIC S9(3) COMP-3.
           07  MSG-I-KY-BA                       PIC S9(10) COMP-3.
           07  MSG-I-KY-CUST-NO                  PIC S9(9) COMP.
           07  MSG-I-FL-MEDICAL-ACCOUNT          PIC X(1).
         05  MSG-I-HDR-CU0303A.
           07  MSG-I-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-I-QY-ALR-ACL-CNT              PIC S9(3) COMP-3.
           07  MSG-I-KY-CUST-NO                  PIC S9(9) COMP.
           07  MSG-I-KY-PREM-NO                  PIC S9(9) COMP.
           07  MSG-I-CD-CRITICAL-ACCT            PIC X(2).
           07  MSG-I-TX-CRIT-ACCT-RSN            PIC X(20).
           07  MSG-I-FL-CAP-EMERGENCY            PIC X(1).
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
           07  MSG-I-DT-MED-CERT-START           PIC X(10).
           07  MSG-I-DT-MED-CERT-EXP             PIC X(10).
           07  MSG-I-CD-MED-CERT-STAT            PIC X(2).
           07  MSG-I-NM-MED-AUTH                 PIC X(28).
           07  MSG-I-AD-STR-1                    PIC X(28).
           07  MSG-I-AD-STR-2                    PIC X(28).
           07  MSG-I-AD-CITY                     PIC X(18).
           07  MSG-I-AD-ZIP                      PIC S9(6) COMP-3.
           07  MSG-I-AD-ST                       PIC X(2).
               88  MSG-I-ALBERTA                     VALUE 'AB'.
               88  MSG-I-AMERICAN-SAMOA              VALUE 'AS'.
               88  MSG-I-BRITISH-COLUMBIA            VALUE 'BC'.
               88  MSG-I-FED-STATES-MICRO            VALUE 'FM'.
               88  MSG-I-GUAM                        VALUE 'GU'.
               88  MSG-I-MANITOBA                    VALUE 'MB'.
               88  MSG-I-MARSHALL-ISLANDS            VALUE 'MH'.
               88  MSG-I-NRTHRN-MARIANA-ISLNDS       VALUE 'MP'.
               88  MSG-I-NEW-BRUNSWICK               VALUE 'NB'.
               88  MSG-I-NEWFOUNDLAND                VALUE 'NF'.
               88  MSG-I-NORTHWEST-TERRITORIES       VALUE 'NT'.
               88  MSG-I-NOVA-SCOTIA                 VALUE 'NS'.
               88  MSG-I-NORTHWEST-TERRITORY         VALUE 'NT'.
               88  MSG-I-ONTARIO                     VALUE 'ON'.
               88  MSG-I-PRINCE-EDWARD-ISLAND        VALUE 'PE'.
               88  MSG-I-PUERTO-RICO                 VALUE 'PR'.
               88  MSG-I-QUEBEC                      VALUE 'PQ'.
               88  MSG-I-PALAU                       VALUE 'PW'.
               88  MSG-I-SASKATCHEWAN                VALUE 'SK'.
               88  MSG-I-VIRGIN-ISLANDS              VALUE 'VI'.
               88  MSG-I-YUKON-TERRITORY             VALUE 'YT'.
           07  MSG-I-TX-TELEPHONE                PIC X(10).
           07  MSG-I-CD-MED-EQUIP                PIC X(2).
           07  MSG-I-TX-FREQ-USE                 PIC X(24).
           07  MSG-I-NM-PATIENT                  PIC X(30).
           07  MSG-I-TX-PATIENT-RLSHIP           PIC X(20).
           07  MSG-I-KY-REMARKS                  PIC S9(9) COMP.
           07  MSG-I-TX-REMARKS-1                PIC X(360).
         05  MSG-I-HDR-CU0307P.
           07  MSG-I-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-I-QY-ALR-ACL-CNT              PIC S9(3) COMP-3.
           07  MSG-I-KY-BA                       PIC S9(10) COMP-3.
           07  MSG-I-KY-CUST-NO                  PIC S9(9) COMP.
           07  MSG-I-DT-TEMP-MAIL-START          PIC X(10).
           07  MSG-I-TX-TEMP-ACD                 PIC X(3).
           07  MSG-I-TX-TEMP-PHN-NO              PIC X(8).
           07  MSG-I-TX-TEMP-PHN-EXTN            PIC X(4).
           07  MSG-I-TX-HOME-ACD                 PIC X(3).
           07  MSG-I-TX-HOME-PHN-NO              PIC X(8).
           07  MSG-I-TX-HOME-PHN-EXTN            PIC X(4).
           07  MSG-I-FL-UNLST-PHN-NO             PIC X(1).
           07  MSG-I-TX-BUS-ACD                  PIC X(3).
           07  MSG-I-TX-BUS-PHN-NO               PIC X(8).
           07  MSG-I-TX-BUS-PHN-EXTN             PIC X(4).
           07  MSG-I-FL-UNLST-BUS-PHN            PIC X(1).
           07  MSG-I-DT-TEMP-MAIL-END            PIC X(10).
           07  MSG-I-FL-UNLST-TEMP-PHN           PIC X(1).
           07  MSG-I-KY-ASSIGND-REP-UID          PIC X(8).
           07  MSG-I-FL-UPDT-PHONE               PIC X(1).
               88  MSG-I-FL-UPDT-PHONE-Y             VALUE 'Y'.
               88  MSG-I-FL-UPDT-PHONE-N             VALUE 'N'.
           07  MSG-I-KY-CUST-ID                  PIC X(15).
           07  MSG-I-NM-DO-BUSINESS-AS           PIC X(28).
         05  MSG-I-HDR-CU0320I.
           07  MSG-I-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-I-QY-ALR-ACL-CNT              PIC S9(3) COMP-3.
           07  MSG-I-CD-CUST-RESPNSBL            PIC X(2).
               88  MSG-I-PRIMARY                     VALUE '01'.
               88  MSG-I-SPOUSE                      VALUE '02'.
               88  MSG-I-THIRD-PARTY                 VALUE '03'.
               88  MSG-I-ROOMMATE                    VALUE '04'.
               88  MSG-I-BILL-CONTACT                VALUE '05'.
               88  MSG-I-COMP-CONTACT                VALUE '06'.
               88  MSG-I-MARK-CONTACT                VALUE '07'.
               88  MSG-I-OTHR                        VALUE '08'.
           07  MSG-I-KY-REMARKS                  PIC S9(9) COMP.
           07  MSG-I-HDR-CU02013.
             09  MSG-I-CD-RETURN                 PIC S9(9) COMP-3.
             09  MSG-I-QY-ALR-ACL-CNT            PIC S9(3) COMP-3.
             09  MSG-I-KY-BA                     PIC S9(10) COMP-3.
             09  MSG-I-NO-CC-TAX-EX-CERT         PIC X(13).
             09  MSG-I-DT-CC-TAX-EX-EXP          PIC X(10).
             09  MSG-I-CD-TAX-AUTHORITY          PIC X(1).
             09  MSG-I-CD-CASH-ONLY-RES          PIC X(2).
         05  MSG-I-HDR-CU02012.
           07  MSG-I-CD-ACCT-TYPE                PIC X(2).
               88  MSG-I-PEND-SERV                   VALUE '03'.
               88  MSG-I-DEFAULTED                   VALUE '  '.
               88  MSG-I-PEND-SERV-OTHER-ACT         VALUE '06'.
               88  MSG-I-PEND-FINAL-ELE              VALUE '12'.
               88  MSG-I-PEND-FINAL-ELE-ACT          VALUE '21'.
               88  MSG-I-FINAL-SERV-ACT              VALUE '42'.
               88  MSG-I-DMAG                        VALUE '48'.
               88  MSG-I-SUM-BILL                    VALUE '51'.
               88  MSG-I-PAL                         VALUE '54'.
               88  MSG-I-SL                          VALUE '57'.
               88  MSG-I-NONCONSUMER                 VALUE '60'.
               88  MSG-I-CASH                        VALUE '63'.
               88  MSG-I-DEBITS                      VALUE '66'.
               88  MSG-I-UNCOLL-CASH                 VALUE '69'.
               88  MSG-I-SERV-TYPE VALUE '12','21','18','33','42','09',
                   '15','24','27','30','36','39','54'.
               88  MSG-I-SERV-PEND                   VALUE '03', '06'.
               88  MSG-I-NON-SERV VALUE '45','48','60','72'.
               88  MSG-I-TAX-ONLY                    VALUE '72'.
           07  MSG-I-CD-BA-STAT                  PIC X(2).
               88  MSG-I-ACT                         VALUE '02'.
               88  MSG-I-PEND-ACT                    VALUE '03'.
               88  MSG-I-PEND-TRNSFR                 VALUE '04'.
               88  MSG-I-VOIDED                      VALUE '07'.
               88  MSG-I-FINALED                     VALUE '09'.
               88  MSG-I-UNCOLL                      VALUE '18'.
               88  MSG-I-NOT-VOID VALUE '02','03','04','09','18','30'.
               88  MSG-I-ESCHEAT                     VALUE '30'.
         05  MSG-I-HDR-CU0401P.
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
           07  MSG-I-CD-SIC                      PIC X(6).
         05  MSG-I-DAT-CUMGM003.
           07  MSG-I-KY-BILL-FORMAT              PIC S9(6) COMP-3.
           07  MSG-I-CD-FORMAT-ID                PIC X(3).
           07  MSG-I-CD-DELIV-METHOD             PIC X(2).
               88  MSG-I-MAIL                        VALUE '01'.
               88  MSG-I-FAX                         VALUE '02'.
               88  MSG-I-EDI                         VALUE '03'.
           07  MSG-I-TX-FAX-PHN-NO               PIC 9(10).
           07  MSG-I-TX-ELEC-ADDRESS             PIC X(80).
           07  MSG-I-QY-BILL-COPY                PIC S9(1) COMP-3.
           07  MSG-I-CD-ROW-ACTION               PIC X(1).
               88  MSG-I-NO-ACTION                   VALUE '0'.
               88  MSG-I-INSERT                      VALUE '1'.
               88  MSG-I-UPDATE                      VALUE '2'.
               88  MSG-I-INACTIVATE                  VALUE '3'.
               88  MSG-I-ACTIVATE                    VALUE '4'.
           07  MSG-I-CD-BILL-LITR                PIC X(1).
           07  MSG-I-CD-DTL-LVL                  PIC X(1).
           07  MSG-I-CD-BILL-FORM                PIC X(2).
         05  MSG-I-CUR-CUMGM003.
           07  MSG-I-CD-FORMAT-ID                PIC X(3).
         05  MSG-I-DAT-CUMSO005
                            OCCURS 2 TIMES.
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
               88  MSG-I-OWNER                       VALUE 'O'.
           07  MSG-I-AD-MAIL-TO-1                PIC X(28).
           07  MSG-I-AD-MAIL-TO-2                PIC X(28).
           07  MSG-I-AD-MAIL-TO-3                PIC X(28).
           07  MSG-I-AD-MAIL-TO-4                PIC X(28).
           07  MSG-I-AD-COUNTRY                  PIC X(28).
           07  MSG-I-DT-TEMP-MAIL-START          PIC X(10).
           07  MSG-I-DT-TEMP-MAIL-END            PIC X(10).
           07  MSG-I-KY-AD                       PIC S9(9) COMP.
         05  MSG-I-HDR-CI0201W.
           07  MSG-I-CD-BILL-EXTN                PIC X(1).
               88  MSG-I-NOT-ON-EXTN                 VALUE '0'.
               88  MSG-I-CUST-LESS-60-MO             VALUE '1'.
               88  MSG-I-CUST-LESS-60-1-YR           VALUE '2'.
               88  MSG-I-CUST-LESS-60-2-YR           VALUE '3'.
               88  MSG-I-CUST-LESS-60-3-YR           VALUE '4'.
               88  MSG-I-CUST-LESS-60-4-YR           VALUE '5'.
               88  MSG-I-CUST-OVER-60                VALUE '9'.
           07  MSG-I-CD-ACCT-HANDLING            PIC X(1).
               88  MSG-I-REGULAR                     VALUE 'R'.
               88  MSG-I-SPECIAL                     VALUE 'S'.
               88  MSG-I-PRE-CAL                     VALUE 'P'.
           07  MSG-I-CD-COMPNY-FACILITY          PIC X(2).
               88  MSG-I-VS-COMP-STOREROOM           VALUE '01'.
           07  MSG-I-CD-RDG-INSTR                PIC X(2).
               88  MSG-I-DO-NOT-EST-TEMP             VALUE '01'.
               88  MSG-I-ACCT-CD-A-DEC               VALUE '02'.
               88  MSG-I-DO-NOT-EST-PERM             VALUE '03'.
               88  MSG-I-DO-NOT-EST-NO-POST-TEMP     VALUE '04'.
               88  MSG-I-ACCT-CD-D-DEC               VALUE '05'.
               88  MSG-I-DO-NOT-EST-NO-POST-PERM     VALUE '06'.
               88  MSG-I-DO-NOT-USE-ESTIMATE VALUE '01','02','03','04',
                   '05','06','07'.
               88  MSG-I-NO-POST-PERM                VALUE '07'.
           07  MSG-I-FL-LPC-ELIGIBLE             PIC X(1).
           07  MSG-I-SUMMARY-BILL.
             09  MSG-I-FL-SHOW-RTP-MASTER        PIC X(1).
             09  MSG-I-FL-SUM-BILL               PIC X(1).
           07  MSG-I-CD-RESIDENCE                PIC X(1).
         05  MSG-I-GRP-CUMGM003.
           07  MSG-I-CD-FORMAT-ID                PIC X(3).

