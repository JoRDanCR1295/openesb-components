      ****************************************************************  00000010
      * COPYBOOK  DMPLUS13                                              00000020
      * LRECL=402                                                       00000030
      * SHIPPING EXTERNAL ORDER DETAIL                                  00000040
      *                 (SORD_DTL)                                      00000050
      *                                                                 00000060
      * MODIFICATIONS:                                                  00000070
      *                                                                 00000080
      *  8/12/97 - ZMPMTS - ADD 3 BYTES TO DMPLUS13-UNIT-DOLLAR-AMOUNT  00000090
      *                                    DMPLUS13-CUSTOMER-ORD-QTY    00000100
      *                                                                 00000110
      * 10/24/01 - ZMPJPS - ADD 3 NEW FIELDS TO COPYBOOK                00000120
      *                  1)  DMPLUS13-HOST-ROUTING          PIC X(30)   00000130
      *                  2)  DMPLUS13-ASSET-OWNER           PIC X(30)   00000140
      *                  3)  DMPLUS13-LOGICAL-WAREHOUSE     PIC X(30)   00000150
      ****************************************************************  00000160
       01  DMPLUS13-RECORD.                                             00000170
           05  DMPLUS13-ORIGIN-HOST-NAME            PIC X(20).          00000180
           05  DMPLUS13-TRANSACTION-DATE            PIC X(14).          00000190
           05  DMPLUS13-TRANSACTION-SEQ             PIC X(08).          00000200
           05  DMPLUS13-TRANSACTION-CODE            PIC X(10).          00000210
           05  DMPLUS13-TRANSACTION-VERSION         PIC X(04).          00000220
           05  DMPLUS13-TRANSACTION-SEGMENT         PIC X(04).          00000230
           05  DMPLUS13-TRANSACTION-ACTION          PIC X(02).          00000240
           05  DMPLUS13-ORDER-NUMBER                PIC X(30).          00000250
           05  DMPLUS13-ORDER-TYPE                  PIC X(04).          00000260
           05  DMPLUS13-LINE-NUMBER                 PIC X(04).          00000270
           05  DMPLUS13-LINE-NUMBER-NUMERIC REDEFINES                   00000280
               DMPLUS13-LINE-NUMBER                 PIC 9(04).          00000290
           05  DMPLUS13-ITEM-NUMBER                 PIC X(30).          00000300
           05  DMPLUS13-INVENTORY-STATUS            PIC X(04).          00000310
           05  DMPLUS13-LOT-NUMBER                  PIC X(12).          00000320
           05  DMPLUS13-PLATFORM                    PIC X(04).          00000330
           05  DMPLUS13-PRODUCT-SOURCE              PIC X(01).          00000340
           05  DMPLUS13-ORDER-QUANTITY              PIC 9(06)V999.      00000350
           05  DMPLUS13-CUSTOMER-LINE-NUMBER        PIC X(04).          00000360
           05  DMPLUS13-MERCH-COD-VALIDATION        PIC X(09).          00000370
           05  DMPLUS13-MERCH-INS-VALIDATION        PIC 9(06)V999.      00000380
           05  DMPLUS13-MERCH-INS-VLDTN REDEFINES                       00000390
               DMPLUS13-MERCH-INS-VALIDATION        PIC X(09).          00000400
           05  DMPLUS13-ORDER-CATCH-QUANTITY        PIC X(09).          00000410
           05  DMPLUS13-MINIMUM-CATCH-QTY           PIC X(09).          00000420
           05  DMPLUS13-MAXIMUM-CATCH-QTY           PIC X(09).          00000430
           05  DMPLUS13-UNIT-DOLLAR-AMOUNT          PIC 9(08)V999.      00000440
           05  DMPLUS13-SERIAL-NO-CAPT-FLAG         PIC X(01).          00000450
           05  DMPLUS13-CUSTOMER-SKU                PIC X(30).          00000460
           05  DMPLUS13-CUSTOMER-ORD-QTY            PIC 9(08)V999.      00000470
           05  DMPLUS13-PICKING-LINE-DTL-ID         PIC 9(10).          00000480
           05  DMPLUS13-PICKING-LINE-DTL-ID-A REDEFINES                 00000490
                DMPLUS13-PICKING-LINE-DTL-ID        PIC X(10).          00000500
           05  DMPLUS13-DELIVERY-ID                 PIC 9(10).          00000510
           05  DMPLUS13-DELIVERY-ID-A REDEFINES                         00000520
               DMPLUS13-DELIVERY-ID                 PIC X(10).          00000530
           05  DMPLUS13-ORACLE1-ITEM-NUM            PIC X(30).          00000540
           05  DMPLUS13-HOST-ROUTING                PIC X(30).          00000550
           05  DMPLUS13-ASSET-OWNER                 PIC X(30).          00000560
           05  DMPLUS13-LOGICAL-WAREHOUSE           PIC X(30).          00000570
