      * MVS PORT-- CODE RUN THROUGH PROCESSOR
      *            on Fri Feb  7 10:16:49 2003

      *****************************************************************
      **                                                             **
      **           FOUNDATION FOR COOPERATIVE PROCESSING             **
      **                                                             **
      **                     COBOL  COPYBOOK                         **
      **                                                             **
      *****************************************************************
      **                                                             **
      **      COPYBOOK: CUCL107O                                     **
      **                                                             **
      **  COPYBOOK FOR: Copybook.CUCL107O MAINTAIN BILL ACCT         **
      **                                                             **
      **  GENERATED ON: Fri Jan 31 13:20:28 2003                     **
      **                                                             **
      **  SHORT DESCRIPTION: CUCL107O MAINTAIN BILL ACCT             **
      **                                                             **
      **            BY: NAZIRA__                                     **
      **                                                             **
      *****************************************************************
       03  MSG-O-MNT-BILL-ACCT.
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
         05  MSG-O-HDR-CUMCC001.
           07  MSG-O-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-O-TX-RQST-BY                  PIC X(28).
           07  MSG-O-TX-RQST-BY-PHN              PIC X(8).
         05  MSG-O-HDR-CUMCC002.
           07  MSG-O-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-O-CD-FUNCTION                 PIC X(2).
               88  MSG-O-FUNC-CD-SELECT              VALUE 'S'.
               88  MSG-O-FUNC-CD-INSERT              VALUE 'I'.
               88  MSG-O-FUNC-CD-UPDATE              VALUE 'U'.
               88  MSG-O-FUNC-CD-DELETE              VALUE 'D'.
               88  MSG-O-FUNC-CD-SELECT-LESS-THAN    VALUE 'SL'.
               88  MSG-O-FUNC-CD-SELECT-GTR-THAN     VALUE 'SG'.
           07  MSG-O-KY-REMARKS                  PIC S9(9) COMP.
           07  MSG-O-TX-REMARKS-1                PIC X(360).
           07  MSG-O-DT-GREGORIAN                PIC X(10).
         05  MSG-O-HDR-CUMSO005.
           07  MSG-O-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-O-CD-ADDR-FUNC                PIC X(1).
               88  MSG-O-ADD                         VALUE 'A'.
               88  MSG-O-UPDATE                      VALUE 'U'.
               88  MSG-O-DELETE                      VALUE 'D'.
               88  MSG-O-ADD-ONLY                    VALUE 'O'.
               88  MSG-O-REPLACE-BILLING             VALUE 'R'.
           07  MSG-O-KY-BA                       PIC S9(10) COMP-3.
           07  MSG-O-KY-CUST-NO                  PIC S9(9) COMP.
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
               88  MSG-O-OWNER                       VALUE 'O'.
           07  MSG-O-CD-RTRN-MAIL-PRCS           PIC X(4).
               88  MSG-O-SUCC-CMPLT                  VALUE '0000'.
               88  MSG-O-MSNG-REQD-FLD               VALUE '1111'.
               88  MSG-O-INVAL-FUNC                  VALUE '2222'.
               88  MSG-O-CUST-ADDR-BEING-USED        VALUE '3333'.
               88  MSG-O-PGM-ABEND                   VALUE '9999'.
           07  MSG-O-DT-GREGORIAN                PIC X(10).
         05  MSG-O-HDR-CUMGM003.
           07  MSG-O-QY-ALR-ACL-CNT              PIC S9(3) COMP-3.
           07  MSG-O-KY-BA                       PIC S9(10) COMP-3.
           07  MSG-O-KY-CUST-NO                  PIC S9(9) COMP.
           07  MSG-O-CD-BP-TABLE-ACCESS          PIC X(2).
               88  MSG-O-USE-DEFAULT-ONLY            VALUE '01'.
               88  MSG-O-USE-BILL-FORMAT-ONLY        VALUE '02'.
               88  MSG-O-USE-DEF-PLUS-BILL-FORM      VALUE '03'.
         05  MSG-O-HDR-CU0234I.
           07  MSG-O-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-O-QY-ALR-ACL-CNT              PIC S9(3) COMP-3.
           07  MSG-O-KY-BA                       PIC S9(10) COMP-3.
           07  MSG-O-KY-CUST-NO                  PIC S9(9) COMP.
           07  MSG-O-FL-MEDICAL-ACCOUNT          PIC X(1).
         05  MSG-O-HDR-CU0303A.
           07  MSG-O-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-O-QY-ALR-ACL-CNT              PIC S9(3) COMP-3.
           07  MSG-O-KY-CUST-NO                  PIC S9(9) COMP.
           07  MSG-O-KY-PREM-NO                  PIC S9(9) COMP.
           07  MSG-O-CD-CRITICAL-ACCT            PIC X(2).
           07  MSG-O-TX-CRIT-ACCT-RSN            PIC X(20).
           07  MSG-O-FL-CAP-EMERGENCY            PIC X(1).
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
           07  MSG-O-DT-MED-CERT-START           PIC X(10).
           07  MSG-O-DT-MED-CERT-EXP             PIC X(10).
           07  MSG-O-CD-MED-CERT-STAT            PIC X(2).
           07  MSG-O-NM-MED-AUTH                 PIC X(28).
           07  MSG-O-AD-STR-1                    PIC X(28).
           07  MSG-O-AD-STR-2                    PIC X(28).
           07  MSG-O-AD-CITY                     PIC X(18).
           07  MSG-O-AD-ZIP                      PIC S9(6) COMP-3.
           07  MSG-O-AD-ST                       PIC X(2).
               88  MSG-O-ALBERTA                     VALUE 'AB'.
               88  MSG-O-AMERICAN-SAMOA              VALUE 'AS'.
               88  MSG-O-BRITISH-COLUMBIA            VALUE 'BC'.
               88  MSG-O-FED-STATES-MICRO            VALUE 'FM'.
               88  MSG-O-GUAM                        VALUE 'GU'.
               88  MSG-O-MANITOBA                    VALUE 'MB'.
               88  MSG-O-MARSHALL-ISLANDS            VALUE 'MH'.
               88  MSG-O-NRTHRN-MARIANA-ISLNDS       VALUE 'MP'.
               88  MSG-O-NEW-BRUNSWICK               VALUE 'NB'.
               88  MSG-O-NEWFOUNDLAND                VALUE 'NF'.
               88  MSG-O-NORTHWEST-TERRITORIES       VALUE 'NT'.
               88  MSG-O-NOVA-SCOTIA                 VALUE 'NS'.
               88  MSG-O-NORTHWEST-TERRITORY         VALUE 'NT'.
               88  MSG-O-ONTARIO                     VALUE 'ON'.
               88  MSG-O-PRINCE-EDWARD-ISLAND        VALUE 'PE'.
               88  MSG-O-PUERTO-RICO                 VALUE 'PR'.
               88  MSG-O-QUEBEC                      VALUE 'PQ'.
               88  MSG-O-PALAU                       VALUE 'PW'.
               88  MSG-O-SASKATCHEWAN                VALUE 'SK'.
               88  MSG-O-VIRGIN-ISLANDS              VALUE 'VI'.
               88  MSG-O-YUKON-TERRITORY             VALUE 'YT'.
           07  MSG-O-TX-TELEPHONE                PIC X(10).
           07  MSG-O-CD-MED-EQUIP                PIC X(2).
           07  MSG-O-TX-FREQ-USE                 PIC X(24).
           07  MSG-O-NM-PATIENT                  PIC X(30).
           07  MSG-O-TX-PATIENT-RLSHIP           PIC X(20).
           07  MSG-O-KY-REMARKS                  PIC S9(9) COMP.
           07  MSG-O-TX-REMARKS-1                PIC X(360).
         05  MSG-O-HDR-CU0307P.
           07  MSG-O-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-O-QY-ALR-ACL-CNT              PIC S9(3) COMP-3.
           07  MSG-O-KY-BA                       PIC S9(10) COMP-3.
           07  MSG-O-KY-CUST-NO                  PIC S9(9) COMP.
           07  MSG-O-DT-TEMP-MAIL-START          PIC X(10).
           07  MSG-O-TX-TEMP-ACD                 PIC X(3).
           07  MSG-O-TX-TEMP-PHN-NO              PIC X(8).
           07  MSG-O-TX-TEMP-PHN-EXTN            PIC X(4).
           07  MSG-O-TX-HOME-ACD                 PIC X(3).
           07  MSG-O-TX-HOME-PHN-NO              PIC X(8).
           07  MSG-O-TX-HOME-PHN-EXTN            PIC X(4).
           07  MSG-O-FL-UNLST-PHN-NO             PIC X(1).
           07  MSG-O-TX-BUS-ACD                  PIC X(3).
           07  MSG-O-TX-BUS-PHN-NO               PIC X(8).
           07  MSG-O-TX-BUS-PHN-EXTN             PIC X(4).
           07  MSG-O-FL-UNLST-BUS-PHN            PIC X(1).
           07  MSG-O-DT-TEMP-MAIL-END            PIC X(10).
           07  MSG-O-FL-UNLST-TEMP-PHN           PIC X(1).
           07  MSG-O-KY-ASSIGND-REP-UID          PIC X(8).
           07  MSG-O-FL-UPDT-PHONE               PIC X(1).
               88  MSG-O-FL-UPDT-PHONE-Y             VALUE 'Y'.
               88  MSG-O-FL-UPDT-PHONE-N             VALUE 'N'.
           07  MSG-O-KY-CUST-ID                  PIC X(15).
           07  MSG-O-NM-DO-BUSINESS-AS           PIC X(28).
         05  MSG-O-HDR-CU0320I.
           07  MSG-O-CD-RETURN                   PIC S9(9) COMP-3.
           07  MSG-O-QY-ALR-ACL-CNT              PIC S9(3) COMP-3.
           07  MSG-O-CD-CUST-RESPNSBL            PIC X(2).
               88  MSG-O-PRIMARY                     VALUE '01'.
               88  MSG-O-SPOUSE                      VALUE '02'.
               88  MSG-O-THIRD-PARTY                 VALUE '03'.
               88  MSG-O-ROOMMATE                    VALUE '04'.
               88  MSG-O-BILL-CONTACT                VALUE '05'.
               88  MSG-O-COMP-CONTACT                VALUE '06'.
               88  MSG-O-MARK-CONTACT                VALUE '07'.
               88  MSG-O-OTHR                        VALUE '08'.
           07  MSG-O-KY-REMARKS                  PIC S9(9) COMP.
           07  MSG-O-HDR-CU02013.
             09  MSG-O-CD-RETURN                 PIC S9(9) COMP-3.
             09  MSG-O-QY-ALR-ACL-CNT            PIC S9(3) COMP-3.
             09  MSG-O-KY-BA                     PIC S9(10) COMP-3.
             09  MSG-O-NO-CC-TAX-EX-CERT         PIC X(13).
             09  MSG-O-DT-CC-TAX-EX-EXP          PIC X(10).
             09  MSG-O-CD-TAX-AUTHORITY          PIC X(1).
             09  MSG-O-CD-CASH-ONLY-RES          PIC X(2).
         05  MSG-O-HDR-CU02012.
           07  MSG-O-CD-ACCT-TYPE                PIC X(2).
               88  MSG-O-PEND-SERV                   VALUE '03'.
               88  MSG-O-DEFAULTED                   VALUE '  '.
               88  MSG-O-PEND-SERV-OTHER-ACT         VALUE '06'.
               88  MSG-O-PEND-FINAL-ELE              VALUE '12'.
               88  MSG-O-PEND-FINAL-ELE-ACT          VALUE '21'.
               88  MSG-O-FINAL-SERV-ACT              VALUE '42'.
               88  MSG-O-DMAG                        VALUE '48'.
               88  MSG-O-SUM-BILL                    VALUE '51'.
               88  MSG-O-PAL                         VALUE '54'.
               88  MSG-O-SL                          VALUE '57'.
               88  MSG-O-NONCONSUMER                 VALUE '60'.
               88  MSG-O-CASH                        VALUE '63'.
               88  MSG-O-DEBITS                      VALUE '66'.
               88  MSG-O-UNCOLL-CASH                 VALUE '69'.
               88  MSG-O-SERV-TYPE VALUE '12','21','18','33','42','09',
                   '15','24','27','30','36','39','54'.
               88  MSG-O-SERV-PEND                   VALUE '03', '06'.
               88  MSG-O-NON-SERV VALUE '45','48','60','72'.
               88  MSG-O-TAX-ONLY                    VALUE '72'.
           07  MSG-O-CD-BA-STAT                  PIC X(2).
               88  MSG-O-ACT                         VALUE '02'.
               88  MSG-O-PEND-ACT                    VALUE '03'.
               88  MSG-O-PEND-TRNSFR                 VALUE '04'.
               88  MSG-O-VOIDED                      VALUE '07'.
               88  MSG-O-FINALED                     VALUE '09'.
               88  MSG-O-UNCOLL                      VALUE '18'.
               88  MSG-O-NOT-VOID VALUE '02','03','04','09','18','30'.
               88  MSG-O-ESCHEAT                     VALUE '30'.
         05  MSG-O-HDR-CU0401P.
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
           07  MSG-O-CD-SIC                      PIC X(6).
         05  MSG-O-DAT-CUMGM003.
           07  MSG-O-KY-BILL-FORMAT              PIC S9(6) COMP-3.
           07  MSG-O-CD-FORMAT-ID                PIC X(3).
           07  MSG-O-CD-DELIV-METHOD             PIC X(2).
               88  MSG-O-MAIL                        VALUE '01'.
               88  MSG-O-FAX                         VALUE '02'.
               88  MSG-O-EDI                         VALUE '03'.
           07  MSG-O-TX-FAX-PHN-NO               PIC 9(10).
           07  MSG-O-TX-ELEC-ADDRESS             PIC X(80).
           07  MSG-O-QY-BILL-COPY                PIC S9(1) COMP-3.
           07  MSG-O-CD-ROW-ACTION               PIC X(1).
               88  MSG-O-NO-ACTION                   VALUE '0'.
               88  MSG-O-INSERT                      VALUE '1'.
               88  MSG-O-UPDATE                      VALUE '2'.
               88  MSG-O-INACTIVATE                  VALUE '3'.
               88  MSG-O-ACTIVATE                    VALUE '4'.
           07  MSG-O-CD-BILL-LITR                PIC X(1).
           07  MSG-O-CD-DTL-LVL                  PIC X(1).
           07  MSG-O-CD-BILL-FORM                PIC X(2).
         05  MSG-O-CUR-CUMGM003.
           07  MSG-O-CD-FORMAT-ID                PIC X(3).
         05  MSG-O-DAT-CUMSO005
                            OCCURS 2 TIMES.
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
               88  MSG-O-OWNER                       VALUE 'O'.
           07  MSG-O-AD-MAIL-TO-1                PIC X(28).
           07  MSG-O-AD-MAIL-TO-2                PIC X(28).
           07  MSG-O-AD-MAIL-TO-3                PIC X(28).
           07  MSG-O-AD-MAIL-TO-4                PIC X(28).
           07  MSG-O-AD-COUNTRY                  PIC X(28).
           07  MSG-O-DT-TEMP-MAIL-START          PIC X(10).
           07  MSG-O-DT-TEMP-MAIL-END            PIC X(10).
           07  MSG-O-KY-AD                       PIC S9(9) COMP.
         05  MSG-O-HDR-CI0201W.
           07  MSG-O-CD-BILL-EXTN                PIC X(1).
               88  MSG-O-NOT-ON-EXTN                 VALUE '0'.
               88  MSG-O-CUST-LESS-60-MO             VALUE '1'.
               88  MSG-O-CUST-LESS-60-1-YR           VALUE '2'.
               88  MSG-O-CUST-LESS-60-2-YR           VALUE '3'.
               88  MSG-O-CUST-LESS-60-3-YR           VALUE '4'.
               88  MSG-O-CUST-LESS-60-4-YR           VALUE '5'.
               88  MSG-O-CUST-OVER-60                VALUE '9'.
           07  MSG-O-CD-ACCT-HANDLING            PIC X(1).
               88  MSG-O-REGULAR                     VALUE 'R'.
               88  MSG-O-SPECIAL                     VALUE 'S'.
               88  MSG-O-PRE-CAL                     VALUE 'P'.
           07  MSG-O-CD-COMPNY-FACILITY          PIC X(2).
               88  MSG-O-VS-COMP-STOREROOM           VALUE '01'.
           07  MSG-O-CD-RDG-INSTR                PIC X(2).
               88  MSG-O-DO-NOT-EST-TEMP             VALUE '01'.
               88  MSG-O-ACCT-CD-A-DEC               VALUE '02'.
               88  MSG-O-DO-NOT-EST-PERM             VALUE '03'.
               88  MSG-O-DO-NOT-EST-NO-POST-TEMP     VALUE '04'.
               88  MSG-O-ACCT-CD-D-DEC               VALUE '05'.
               88  MSG-O-DO-NOT-EST-NO-POST-PERM     VALUE '06'.
               88  MSG-O-DO-NOT-USE-ESTIMATE VALUE '01','02','03','04',
                   '05','06','07'.
               88  MSG-O-NO-POST-PERM                VALUE '07'.
           07  MSG-O-FL-LPC-ELIGIBLE             PIC X(1).
           07  MSG-O-SUMMARY-BILL.
             09  MSG-O-FL-SHOW-RTP-MASTER        PIC X(1).
             09  MSG-O-FL-SUM-BILL               PIC X(1).
           07  MSG-O-CD-RESIDENCE                PIC X(1).
         05  MSG-O-GRP-CUMGM003.
           07  MSG-O-CD-FORMAT-ID                PIC X(3).

