      ****************************************************************  00000010
      * COPYBOOK  DMPLUS10                                              00000020
      * LRECL=922                                                       00000030
      * SHIPPING EXTERNAL ORDER HEADER   (SORD_ORD)                     00000040
      *                                                                 00000050
      * MODIFICATIONS:                                                  00000060
      *   8/12/97 - ZMPMTS  ADDED NEW FIELDS AT BOTTOM OF LAYOUT        00000070
      *                                                                 00000080
      ****************************************************************  00000090
       01  DMPLUS10-RECORD.                                             00000100
           05  DMPLUS10-ORIGIN-HOST-NAME            PIC X(20).          00000110
           05  DMPLUS10-TRANSACTION-DATE            PIC X(14).          00000120
           05  DMPLUS10-TRANSACTION-SEQ             PIC X(08).          00000130
           05  DMPLUS10-TRANSACTION-CODE            PIC X(10).          00000140
           05  DMPLUS10-TRANSACTION-VERSION         PIC X(04).          00000150
           05  DMPLUS10-TRANSACTION-SEGMENT         PIC X(04).          00000160
           05  DMPLUS10-TRANSACTION-ACTION          PIC X(02).          00000170
           05  DMPLUS10-ORDER-NUMBER                PIC X(30).          00000180
           05  DMPLUS10-ORDER-TYPE                  PIC X(04).          00000190
           05  DMPLUS10-CARRIER-ID                  PIC X(17).          00000200
           05  DMPLUS10-REQUESTED-CARRIER-ID        PIC X(17).          00000210
           05  DMPLUS10-ACCOUNT-NUMBER              PIC X(12).          00000220
           05  DMPLUS10-DESTINATION-LOCATION        PIC X(10).          00000230
           05  DMPLUS10-SHIPMENT-NUMBER             PIC X(30).          00000240
           05  DMPLUS10-ALLOW-OVER-SHIPMENT         PIC X(01).          00000250
           05  DMPLUS10-ALLOW-PART-SHIPMENT         PIC X(01).          00000260
           05  DMPLUS10-FIFO-FLAG                   PIC X(01).          00000270
           05  DMPLUS10-CUSTOMER-ORDER-NUM          PIC X(30).          00000280
           05  DMPLUS10-HOST-ORDER-NUM              PIC X(30).          00000290
           05  DMPLUS10-ORDER-DATE                  PIC X(14).          00000300
           05  DMPLUS10-RELEASE-DATE                PIC X(14).          00000310
           05  DMPLUS10-MAX-PRODUCT-AGE             PIC X(03).          00000320
           05  DMPLUS10-ITEM-NUMBER                 PIC X(30).          00000330
           05  DMPLUS10-QUANTITY                    PIC X(09).          00000340
           05  DMPLUS10-PRODUCT-SOURCE              PIC X(01).          00000350
           05  DMPLUS10-ESTIMATED-WEIGHT            PIC 9(08).          00000360
           05  DMPLUS10-ESTIMATED-WGHT-SMART REDEFINES                  00000370
               DMPLUS10-ESTIMATED-WEIGHT            PIC X(08).          00000380
           05  DMPLUS10-ESTIMATED-CUBE              PIC 9(08).          00000390
           05  DMPLUS10-ESTIMATED-CUBE-SMART REDEFINES                  00000400
               DMPLUS10-ESTIMATED-CUBE              PIC X(08).          00000410
           05  DMPLUS10-ESTIMATED-PACKAGE-CNT       PIC X(08).          00000420
           05  DMPLUS10-ORDER-PRIORITY              PIC 9(02).          00000430
           05  DMPLUS10-WAREHOUSE-ID                PIC X(04).          00000440
           05  DMPLUS10-WAVE-FLAG                   PIC X(01).          00000450
           05  DMPLUS10-RULE-NAME                   PIC X(04).          00000460
           05  DMPLUS10-EARLY-SHIP-BY-DATE          PIC X(14).          00000470
           05  DMPLUS10-LATEST-SHIP-BY-DATE         PIC X(14).          00000480
           05  DMPLUS10-EARLY-DELIVER-BY-DATE       PIC X(14).          00000490
           05  DMPLUS10-LATE-DELIVER-BY-DATE        PIC X(14).          00000500
           05  DMPLUS10-REQUIRE-TMS-FLAG            PIC X(01).          00000510
           05  DMPLUS10-ORDER-STUS-TRANSPORT        PIC X(06).          00000520
           05  DMPLUS10-CUSTOMER-ID                 PIC X(17).          00000530
           05  DMPLUS10-BILL-TO-ADDRESS-ID          PIC X(30).          00000540
           05  DMPLUS10-MARK-FOR-ADDRESS-ID         PIC X(30).          00000550
           05  DMPLUS10-SHIP-TO-ADDRESS-ID          PIC X(30).          00000560
           05  DMPLUS10-PREPAID-COLLECT-TEXT        PIC X(01).          00000570
           05  DMPLUS10-FREIGHT-TERMS-TEXT          PIC X(40).          00000580
           05  DMPLUS10-REQUESTED-VESSEL-TYPE       PIC X(04).          00000590
           05  DMPLUS10-REQUESTED-VESSEL-LEN        PIC X(03).          00000600
           05  DMPLUS10-TRANSPORTATION-TYPE         PIC X(06).          00000610
           05  DMPLUS10-RQSTD-TRANSPORT-TYPE        PIC X(06).          00000620
           05  DMPLUS10-TRANSPORTATION-SVC          PIC X(06).          00000630
           05  DMPLUS10-RQSTD-TRANSPORT-SVC         PIC X(06).          00000640
           05  DMPLUS10-SATURDAY-PICKUP-FLAG        PIC X(01).          00000650
           05  DMPLUS10-SATURDAY-DELIVER-FLAG       PIC X(01).          00000660
           05  DMPLUS10-SHIPMENT-INSURE-FLAG        PIC X(01).          00000670
           05  DMPLUS10-HOLD-FOR-PICKUP-FLAG        PIC X(01).          00000680
           05  DMPLUS10-INSIDE-DELIVERY             PIC X(01).          00000690
           05  DMPLUS10-RESIDENTIAL-FLAG            PIC X(01).          00000700
           05  DMPLUS10-ACKNOWLEDGE-DELIVERY        PIC X(01).          00000710
           05  DMPLUS10-BILL-TO-NUM-SHIP-ID         PIC X(20).          00000720
           05  DMPLUS10-MERCHANDISE-COD-FLAG        PIC X(01).          00000730
           05  DMPLUS10-FREIGHT-COD-FLAG            PIC X(01).          00000740
           05  DMPLUS10-COD-TAX-RATE                PIC X(08).          00000750
           05  DMPLUS10-BUILDING-NUMBER             PIC X(08).          00000760
           05  DMPLUS10-RUN-DATE                    PIC X(14).          00000770
           05  DMPLUS10-RELEASE-PERCENTAGE          PIC X(03).          00000780
           05  DMPLUS10-STAGE-COMPLETE-FLAG         PIC X(01).          00000790
           05  DMPLUS10-SHIPPING-LABELS-FLAG        PIC X(01).          00000800
           05  DMPLUS10-LABEL-COUNT                 PIC X(03).          00000810
           05  DMPLUS10-APPLY-LABEL-INSTRUCT        PIC X(40).          00000820
           05  DMPLUS10-ORDER-LOAD-SEQUENCE         PIC X(03).          00000830
           05  DMPLUS10-COD-ADDRESS-ID              PIC X(30).          00000840
      *** ZMPMTS  START 08/12/97                                        00000850
           05  DMPLUS10-HOST-SYS-NAME               PIC X(20).          00000860
           05  DMPLUS10-HOST-WHSE-CODE              PIC X(03).          00000870
           05  DMPLUS10-HOST-PRIORITY-FLAG          PIC X(01).          00000880
           05  DMPLUS10-EXP-ORD-FLAG                PIC X(01).          00000890
           05  DMPLUS10-INVOICE-NUMBER              PIC X(15).          00000900
           05  DMPLUS10-SHIP-TYPE-CODE              PIC X(01).          00000910
           05  DMPLUS10-CALL-BFR-DEL-NUMB           PIC X(20).          00000920
           05  DMPLUS10-HOST-ORD-CON-NUMB           PIC X(13).          00000930
           05  DMPLUS10-CUST-SVC-NAME               PIC X(30).          00000940
           05  DMPLUS10-CUST-SVC-PNE-NUMB           PIC X(20).          00000950
           05  DMPLUS10-SKU-LBL-REQ-FLAG            PIC X(01).          00000960
           05  DMPLUS10-UCC128-LVL-FLAG             PIC X(01).          00000970
           05  DMPLUS10-FREIGHT-CHG-CD              PIC X(03).          00000980
      *** ZMPMTS  END   08/12/97                                        00000990
           05  DMPLUS10-DELIVERY-ID                 PIC 9(10).          00001000
           05  DMPLUS10-DELIVERY-ID-A  REDEFINES                        00001010
               DMPLUS10-DELIVERY-ID                 PIC X(10).          00001020
           05  DMPLUS10-FNTEND-SYS-REF-NUM          PIC X(50).          00001030
