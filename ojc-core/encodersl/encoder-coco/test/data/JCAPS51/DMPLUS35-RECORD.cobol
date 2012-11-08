      ****************************************************************  00000010
      * COPYBOOK  DMPLUS35                                              00000020
      * LRECL=156                                                       00000030
      * MATTHEW R. REPOLI   9/15/97                                     00000040
      * SHIPPING EXTERNAL ORDER PACKAGE                                 00000050
      *             (SORD_PACKAGE)NEW                                   00000060
      *                                                                 00000070
      * MODIFICATIONS:                                                  00000080
      *                                                                 00000090
      * 08/14/98 - ZMPMTS - ADDED DMPLUS35-PKG-CUSTOMER-SKU-30 BYTES.   00000100
      *                                                                 00000110
      ****************************************************************  00000120
       01  DMPLUS35-RECORD.                                             00000130
           05  DMPLUS35-ORIGIN-HOST-NAME            PIC X(20).          00000140
           05  DMPLUS35-TRANSACTION-DATE            PIC X(14).          00000150
           05  DMPLUS35-TRANSACTION-SEQ             PIC X(08).          00000160
           05  DMPLUS35-TRANSACTION-CODE            PIC X(10).          00000170
           05  DMPLUS35-TRANSACTION-VERSION         PIC X(04).          00000180
           05  DMPLUS35-TRANSACTION-SEGMENT         PIC X(04).          00000190
           05  DMPLUS35-TRANSACTION-ACTION          PIC X(02).          00000200
           05  DMPLUS35-ORDER-NUMBER                PIC X(30).          00000210
           05  DMPLUS35-ORDER-TYPE                  PIC X(04).          00000220
           05  DMPLUS35-DTL-LINE-NUM                PIC X(04).          00000230
           05  DMPLUS35-DTL-NUMERIC  REDEFINES                          00000240
                   DMPLUS35-DTL-LINE-NUM            PIC 9(04).          00000250
           05  DMPLUS35-SEQUENCE-NUM                PIC X(04).          00000260
           05  DMPLUS35-SEQUENCE-NUMERIC REDEFINES                      00000270
                       DMPLUS35-SEQUENCE-NUM        PIC 9(04).          00000280
           05  DMPLUS35-ORDER-DTL-QTY               PIC 9(08)V999.      00000290
           05  DMPLUS35-PACKAGE-ITM-NUM             PIC X(30).          00000300
           05  DMPLUS35-PACKAGE-QTY                 PIC 9(08)V999.      00000310
           05  DMPLUS35-PACKAGE-QTY-ALF REDEFINES                       00000320
                       DMPLUS35-PACKAGE-QTY         PIC X(11).          00000330
           05  DMPLUS35-PKG-CUSTOMER-SKU            PIC X(30).          00000340
           05  DMPLUS35-PICKING-LINE-DTL-ID         PIC 9(10).          00000350
           05  DMPLUS35-PICKING-LINE-DTL-ID-A REDEFINES                 00000360
                DMPLUS35-PICKING-LINE-DTL-ID        PIC X(10).          00000370
           05  DMPLUS35-DELIVERY-ID                 PIC 9(10).          00000380
           05  DMPLUS35-DELIVERY-ID-A REDEFINES                         00000390
               DMPLUS35-DELIVERY-ID                 PIC X(10).          00000400
