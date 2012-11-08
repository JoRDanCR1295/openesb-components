      *
      * Simple handcoded input copybook
      *
        01 DI-REQUEST.
               02 DI-CICS-TRN                      PIC X(8).
               02 DI-EXT-TRANSACTION-SUBCODE       PIC X(4).
               02 DI-CHCK-SUM                     PIC 9(16).
               02 DI-MESSAGE-LENGTH                PIC 9(4).
               02 DI-MESSAGE-LAYOUT-IDENTIFICATION PIC X(4).
               02 DI-TELLER-NUMBER                 PIC 9(6).
               02 DI-TRN-SEQUENCE-NO               PIC 9(6).
               02 FILLER                           PIC 9(1).
               02 DI-TIME-TRANSACTION-ENTERED      PIC 9(4).
               02 DI-DATE-TRANSACTION-ENTERED      PIC 9(8).
               02 DI-TERMINAL-ID                   PIC X(4).
               02 DI-IB-BRANCH-NUMBER              PIC 9(4).
               02 DI-APPLICATION-CODE              PIC X(2).
               02 DI-ACCOUNT-NUMBER                PIC 9(16).
               02 DI-USER-ID                       PIC X(9).
