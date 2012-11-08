      *
      * Simple Copybook drawn from the KB specs
      *

        01 DI-RESPONSE.
               02 DI-REPLY-LAYOUT-ID       PIC X(5).
               02 DI-EXT-TRANSACTION-CODE  PIC X(4).
               02 DI-RETURN-CODE           PIC 9(1).
               02 DI-ERROR-CODE            PIC 9(4).
               02 DI-TELLER-NUMBER         PIC 9(6).
               02 DI-TRN-SEQUENCE-NO       PIC X(6).
               02 DI-AM-PM-FLAG            PIC X(1).
               02 DI-APPLICATION-CODE      PIC 9(2).
               02 DI-ACCOUNT-NUMBER        PIC 9(16).
               02 DI-CURRENCY-CODE         PIC X(3).
               02 DI-STOP-PAYMENT-INDICATOR      PIC X(1).
               02 DI-ACCOUNT-STATUS-INDICATOR    PIC X(1).
      *        02 DI-CURRENT-BALANCE     PIC S 9(13)V9(2) / S9(15).
      *        02 DI-AVAILABLE-BALANCE   PIC S 9(13)V9(2) / S9(15).
      *        02 DI-TODAY-ACTIVITY      PIC S 9(13)V9(2) / S9(15).
               02 DI-CURRENT-BALANCE     PIC S9(13)V9(2).
               02 DI-AVAILABLE-BALANCE   PIC S9(13)V9(2).
               02 DI-TODAY-ACTIVITY      PIC S9(13)V9(2).
               02 DI-DI-BUSINESS-DAY     PIC 9(8).
      *        02 DI-HOLDS-TOTAL	        PIC S9(13)V9(2) / S9(15).
               02 DI-HOLDS-TOTAL              PIC S9(11)V9(2).
               02 DI-ACCOUNT-TYPE        PIC 9(4).
               02 DI-BRANCH-NO           PIC 9(4).
               02 DI-INTEREST-RATE       PIC S9(3)V9(8).
      *        02 DI-INTEREST-RATE       PIC S9(11).
               02 DI-END-OF-REPLY-RECORD PIC X(3).
