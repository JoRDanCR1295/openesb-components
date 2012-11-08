      ****************************************************************  00000010
      * COPYBOOK  DMPLUS12                                              00000020
      * LRECL=638                                                       00000030
      * SHIPPING EXTERNAL ORDER SHIP-TO                                 00000040
      *              (SORD_ADDR)                                        00000050
      *                                                                 00000060
      *  MODIFICATIONS:                                                 00000070
      *  8/12/97 - ZMPMTS - ADDED DMPLUS12-ADDRESS-LINE-4               00000080
      *                                                                 00000090
      *                                                                 00000100
      ****************************************************************  00000110
       01  DMPLUS12-RECORD.                                             00000120
           05  DMPLUS12-ORIGIN-HOST-NAME            PIC X(20).          00000130
           05  DMPLUS12-TRANSACTION-DATE            PIC X(14).          00000140
           05  DMPLUS12-TRANSACTION-SEQ             PIC X(08).          00000150
           05  DMPLUS12-TRANSACTION-CODE            PIC X(10).          00000160
           05  DMPLUS12-TRANSACTION-VERSION         PIC X(04).          00000170
           05  DMPLUS12-TRANSACTION-SEGMENT         PIC X(04).          00000180
           05  DMPLUS12-TRANSACTION-ACTION          PIC X(02).          00000190
           05  DMPLUS12-ORDER-NUMBER                PIC X(30).          00000200
           05  DMPLUS12-ORDER-TYPE                  PIC X(04).          00000210
           05  DMPLUS12-ADDRESS-TYPE                PIC X(01).          00000220
           05  DMPLUS12-ADDRESS-ID                  PIC X(30).          00000230
           05  DMPLUS12-NAME                        PIC X(40).          00000240
           05  DMPLUS12-ADDRESS-LINE-1              PIC X(40).          00000250
           05  DMPLUS12-ADDRESS-LINE-2              PIC X(40).          00000260
           05  DMPLUS12-ADDRESS-LINE-3              PIC X(40).          00000270
           05  DMPLUS12-CITY                        PIC X(25).          00000280
           05  DMPLUS12-STATE-PROVINCE              PIC X(02).          00000290
           05  DMPLUS12-COUNTY-CODE                 PIC X(03).          00000300
           05  DMPLUS12-COUNTRY-CODE                PIC X(10).          00000310
           05  DMPLUS12-POSTAL-CODE                 PIC X(10).          00000320
           05  DMPLUS12-ATTENTION-NAME              PIC X(40).          00000330
           05  DMPLUS12-ATTENTION-TELEPHONE         PIC X(20).          00000340
           05  DMPLUS12-CONTACT-NAME                PIC X(40).          00000350
           05  DMPLUS12-CONTACT-TELEPHONE           PIC X(20).          00000360
           05  DMPLUS12-FAX-NUMBER                  PIC X(20).          00000370
           05  DMPLUS12-SECOND-CONTACT-NAME         PIC X(40).          00000380
           05  DMPLUS12-SECOND-CONTACT-PHONE        PIC X(20).          00000390
           05  DMPLUS12-BUYER-CONTACT-NAME          PIC X(40).          00000400
           05  DMPLUS12-BUYER-CONTACT-PHONE         PIC X(20).          00000410
           05  DMPLUS12-TEMPORARY-FLAG              PIC X(01).          00000420
           05  DMPLUS12-ADDRESS-LINE-4              PIC X(40).          00000430
