      **** CSPAIUC ****************************************************
      *****************************************************************
      **                                                              *
      **  VERSION: 97314   BY: T. FLORES      DATE WRITTEN: 11/10/97  *
      **                                                              *
      **  PASSED DATA AREA FOR COMMON INSERT/UPDATE ROUTINE FOR       *
      **  CLINK TABLE PROCESSING. SHOULD ONLY CHANGED IN COORDINATION *
      **  WITH DATABASE TABLE CHANGES.                                *
      **                                                              *
      *****************************************************************
      **                                                              *
      ** LAST UPDATE:                                                 *
      **   02/15/97   TIF    96-CS058   ORIGINAL                      *
      **   09/10/97   TIF    97-CS142   CUSTOMER CHOICE               *
      **                                ADDED FIELDS FOR PREMISE AND  *
      **                                AND USAGE POINT TABLES        *
      **   11/10/97   TIF    97-CS142   CUSTOMER CHOICE               *
      **                                ADDED SA40 FIELDS FOR TR77    *
      **                                                              *
      *****************************************************************
       01  D40013-LK.
           05  D40013-TABLE-SWITCHES.
               10 D40013-ADD-SA03-SW            PIC X.
                  88 D40013-ADD-SA03            VALUE 'A'.
               10 D40013-ADD-SA09-SW            PIC X.
                  88 D40013-ADD-SA09            VALUE 'A'.
               10 D40013-ADD-SA10-SW            PIC X.
                  88 D40013-ADD-SA10            VALUE 'A'.
               10 D40013-ADD-SA34-SW            PIC X.
                  88 D40013-ADD-SA34            VALUE 'A'.
               10 D40013-ADD-CL10-SW            PIC X.
                  88 D40013-ADD-CL10            VALUE 'A'.
               10 D40013-ADD-CS01-SW            PIC X.
                  88 D40013-ADD-CS01            VALUE 'A'.
               10 D40013-ADD-PD20-SW            PIC X.
                  88 D40013-ADD-PD20            VALUE 'A'.
                  88 D40013-UPD-PD20            VALUE 'C'.
               10 D40013-ADD-SA99-SW            PIC X.
                  88 D40013-ADD-SA99            VALUE 'A'.
               10 D40013-ADD-SA40-SW            PIC X.
                  88 D40013-ADD-SA40            VALUE 'A'.
                  88 D40013-UPD-SA40            VALUE 'C'.
               10 D40013-ADD-SA42-SW            PIC X.
                  88 D40013-ADD-SA42            VALUE 'A'.
                  88 D40013-UPD-SA42            VALUE 'C'.
           05  D40013-SA03.
               10 D40013-SA03-CLIENT-KEY1       PIC S9(9) COMP.
               10 D40013-SA03-CLIENT-KEY2       PIC S9(9) COMP.
               10 D40013-SA03-CLIENT-KEY3       PIC S9(9) COMP.
               10 D40013-SA03-CLIENT-KEY4       PIC S9(9) COMP.
               10 D40013-SA03-STMT-CLNT-KEY     PIC S9(9) COMP.
               10 D40013-SA03-SEQUENCE-NO       PIC S9(4) COMP.
               10 D40013-SA03-START-DT          PIC X(10).
               10 D40013-SA03-PRINT-IN          PIC X.
               10 D40013-SA03-STMT-COPY-QT      PIC S9(4) COMP.
               10 D40013-SA03-END-DT            PIC X(10).
               10 D40013-SA03-CREATE-DT         PIC X(10).
               10 D40013-SA03-CREATE-USR-ID     PIC X(6).
               10 D40013-SA03-UPDATE-TS         PIC X(26).
               10 D40013-SA03-UPDT-USR-ID       PIC X(6).
           05  D40013-SA09.
               10 D40013-SA09-CLIENT-KEY        PIC S9(9) COMP.
               10 D40013-SA09-SEQUENCE-NO       PIC S9(4) COMP.
               10 D40013-SA09-STATEMTBL-IN      PIC X.
               10 D40013-SA09-STMT-COPY-QT      PIC S9(4) COMP.
               10 D40013-SA09-ENVL-PREF-IN      PIC X.
               10 D40013-SA09-INACTIVE-DT       PIC X(10).
               10 D40013-SA09-UNPAID-CHG-AM     PIC S9(9)V9(2) COMP-3.
               10 D40013-SA09-UNPD-STMTD-AM     PIC S9(9)V9(2) COMP-3.
               10 D40013-SA09-ODUE-BAL-AM       PIC S9(9)V9(2) COMP-3.
               10 D40013-SA09-ARGM-BAL-AM       PIC S9(9)V9(2) COMP-3.
               10 D40013-SA09-PEND-IN           PIC X.
               10 D40013-SA09-PEND-RSN-TX       PIC X(254).
               10 D40013-SA09-PEND-START-DT     PIC X(10).
               10 D40013-SA09-PEND-END-DT       PIC X(10).
               10 D40013-SA09-CREATE-DT         PIC X(10).
               10 D40013-SA09-CREATE-USR-ID     PIC X(6).
               10 D40013-SA09-UPDATE-TS         PIC X(26).
               10 D40013-SA09-UPDT-USR-ID       PIC X(6).
           05  D40013-SA10.
               10 D40013-SA10-CLIENT-KEY        PIC S9(9) COMP.
               10 D40013-SA10-SEQUENCE-NO       PIC S9(4) COMP.
               10 D40013-SA10-ACCOUNT-KEY       PIC S9(9) COMP.
               10 D40013-SA10-SOURCE-CD         PIC X(4).
               10 D40013-SA10-ACCT-STAT-CD      PIC X(4).
               10 D40013-SA10-NM                PIC X(50).
               10 D40013-SA10-BALANCE-AM        PIC S9(9)V9(2) COMP-3.
               10 D40013-SA10-UNPD-STMTD-AM     PIC S9(9)V9(2) COMP-3.
               10 D40013-SA10-BAD-CHKRSK-IN     PIC X.
               10 D40013-SA10-CR-RTNG-RV-DT     PIC X(10).
               10 D40013-SA10-STATUS-CHG-DT     PIC X(10).
               10 D40013-SA10-EXT-ACCT-EI       PIC X(25).
               10 D40013-SA10-CREATE-DT         PIC X(10).
               10 D40013-SA10-CREATE-USR-ID     PIC X(6).
               10 D40013-SA10-UPDATE-TS         PIC X(26).
               10 D40013-SA10-UPDT-USR-ID       PIC X(6).
           05  D40013-SA34.
               10 D40013-SA34-CLIENT-KEY        PIC S9(9) COMP.
               10 D40013-SA34-SEQUENCE-NO       PIC S9(4) COMP.
               10 D40013-SA34-ST-AC-ADD-KEY     PIC S9(9) COMP.
               10 D40013-SA34-ADDRESS-KEY       PIC S9(9) COMP.
               10 D40013-SA34-ADDTL-ADDR-TX     PIC X(50).
               10 D40013-SA34-START-DT          PIC X(10).
               10 D40013-SA34-END-DT            PIC X(10).
               10 D40013-SA34-CREATE-DT         PIC X(10).
               10 D40013-SA34-CREATE-USR-ID     PIC X(6).
               10 D40013-SA34-UPDATE-TS         PIC X(26).
               10 D40013-SA34-UPDT-USR-ID       PIC X(6).
           05  D40013-CL10.
               10 D40013-CL10-ADDRESS-KEY       PIC S9(9) COMP.
               10 D40013-CL10-STATE-CD          PIC X(2).
               10 D40013-CL10-DIRECTION-CD      PIC X(2).
               10 D40013-CL10-HOUSE-NO          PIC X(10).
               10 D40013-CL10-STREET-NM         PIC X(30).
               10 D40013-CL10-CITY-NM           PIC X(25).
               10 D40013-CL10-ZIP-CD            PIC X(5).
               10 D40013-CL10-ADDR-LINE1-TX     PIC X(50).
               10 D40013-CL10-ADDR-LINE2-TX     PIC X(50).
               10 D40013-CL10-ADDR-LINE3-TX     PIC X(50).
               10 D40013-CL10-FRGN-ADDR-IN      PIC X.
           05  D40013-CS01.
               10 D40013-CS01-PREMISE-KEY       PIC S9(9) COMP.
               10 D40013-CS01-ACTIVE-DT         PIC X(10).
               10 D40013-CS01-INACTIVE-DT       PIC X(10).
               10 D40013-CS01-UPDATE-TS         PIC X(26).
               10 D40013-CS01-UPDT-USR-ID       PIC X(6).
               10 D40013-CS01-CREATE-DT         PIC X(10).
               10 D40013-CS01-CREATE-USR-ID     PIC X(6).
           05  D40013-PD20.
               10 D40013-PD20-USAGE-PT-KEY      PIC S9(9) COMP.
               10 D40013-PD20-PREMISE-KEY       PIC S9(9) COMP.
               10 D40013-PD20-USAGE-PT-EI       PIC S9(9) USAGE COMP.
               10 D40013-PD20-UP-STATUS-CD      PIC X(4).
               10 D40013-PD20-STATUS-CHG-DT     PIC X(10).
               10 D40013-PD20-UPDATE-TS         PIC X(26).
               10 D40013-PD20-UPDT-USR-ID       PIC X(6).
               10 D40013-PD20-CREATE-DT         PIC X(10).
               10 D40013-PD20-CREATE-USR-ID     PIC X(6).
           05  D40013-SA99.
               10 D40013-SA99-CLIENT-KEY        PIC S9(9) COMP.
               10 D40013-SA99-SEQUENCE-NO       PIC S9(4) COMP.
               10 D40013-SA99-ACCOUNT-KEY       PIC S9(9) COMP.
               10 D40013-SA99-PREMISE-KEY       PIC S9(9) COMP.
           05  D40013-SA40.
               10 D40013-SA40-ESP-CLNT-KEY      PIC S9(9) COMP.
               10 D40013-SA40-ENROLL-KEY        PIC S9(9) COMP.
               10 D40013-SA40-TIMESTAMP-TS      PIC X(20).
               10 D40013-SA40-ESP-DUNS-EI       PIC X(13).
               10 D40013-SA40-PODID-EI          PIC X(9).
               10 D40013-SA40-E-CLNT-KEY        PIC S9(9) COMP.
               10 D40013-SA40-E-SA-SEQ-NO       PIC S9(4) COMP.
               10 D40013-SA40-E-ACCT-KEY        PIC S9(9) COMP.
               10 D40013-SA40-E-ACCT-EI         PIC X(15).
               10 D40013-SA40-P-CLNT-KEY        PIC S9(9) COMP.
               10 D40013-SA40-P-SA-SEQ-NO       PIC S9(4) COMP.
               10 D40013-SA40-P-ACCT-KEY        PIC S9(9) COMP.
               10 D40013-SA40-P-ACCT-EI         PIC X(15).
               10 D40013-SA40-ESP-ACCT-EI       PIC X(15).
           05  D40013-SA42.
               10 D40013-SA42-ESP-CLNT-KEY      PIC S9(9) COMP.
               10 D40013-SA42-ENROLL-KEY        PIC S9(9) COMP.
               10 D40013-SA42-STAT-KEY          PIC S9(9) COMP.
               10 D40013-SA42-ENRL-STAT-CD      PIC X(4).
           05  D40013-RETURNED-VALUES.
               10 D40013-RETURN-CD              PIC X(4).
               10 D40013-RETURN-MSG             PIC X(78).
               10 D40013-RETURN-SQLCA           PIC X(136).
