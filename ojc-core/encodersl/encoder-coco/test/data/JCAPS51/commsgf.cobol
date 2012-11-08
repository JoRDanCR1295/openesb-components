000010*---------------------------------------------------------------*
000020*  -INC COMMSGF     MESSAGING STANDARD LAYOUT       06/08/99    *
000030*                                                               *
000040*---------------------------------------------------------------*
000050*---------------  PROGRAM MODIFICATION -------------------------*
000060*---------------------------------------------------------------*
000070*        MAINTENANCE LOG - ADD LATEST CHANGE TO THE TOP         *
000080* MOD-DATE PROGRAMMER  MOD DESCRIPTION                          *
000090* -------- ----------  -----------------------------------------*
000100* 09/11/03 B.GRAVES   ADDED FIELDS TO RGS1 AND RGS2 SEGMENTS    * BG030911
000100* 08/22/03 C.BRANDEL  ADDED BPR1/BPR2 SEGMENTS FOR REPLENISHMENT* CB030822
000100* 06/11/03 K.REEVES   ADDED PRDB- SEGMENT FOR IBS CONVERSION.   * KR030731
000110* 07/08/03 A.FEN      ADDED LOC8 FIELDS                           AF030708
000120* 05/10/03 R.STEWART  ADDED LOC8 CPN FIELDS, LOC2 OPEN, CLOSE   * RS030510
000130*                     DATES.                                    * RS030510
000140* 01/17/03 A.FEN      ADDED PRD1-STYMVE-FLAG AND REMOVED        * AF030117
000150*                     PRD5-STYMVE-FLAG                          * AF030117
000160* 01/15/03 A.FEN      ADDED PRD5-STYMVE-FLAG                    * AF030115
000170* 01/14/03 D.JAIPERSA ADDED PRD8-PLU-EXPL-F TO PRD8 SEGMENT     * DJ030114
000180*                     CHANGED PRD8-REF-V-STYLE TO PIC X(15)     * DJ030114
000190* 12/17/02 D.FURTER   ADDED TWO 88 LEVELS FOR PRD1-PRODUCT-LEVEL* DF021217
000200* 12/16/02 D.FURTER   ADDED PDF1 SEGMENT                        * DF021216
000210* 10/05/02 V.SMITH    ADD ORD4-PACKS TO ORD4                    * VS021007
000220* 09/05/02 R.STEWART  ADD DISTRO-TYPE TO RCV2                   * RS020905
000230* 09/04/02 R.STEWART  COMMENT RCV2-EDI-MARK-CD                  * RS020904
000240* 08/28/02 R.STEWART  CHNGD RCV1-PO-SHPG-NBR FROM 9(18) TO 9(10)* RS020828
000250*                     RESERVED A FILLER OF X(08) IF NEEDED LATER* RS020828
000260* 08/08/02 R.STEWART  ADDED NEW FIELDS TO RCV6 AND RCVA         * RS020808
000270* 07/16/02 K.MILLS    ADD TRANS TYPE TO ORD4                    * KM020716
000280* 06/06/02 A.FEN      ADD DCL1-ACTIVE-IND,DPT1-ACTIVE-IND,      * AF020606
000290*                     DPT-PLN-EXCLUDE-FLAG                      * AF020607
000300* 06/03/02 C.WISE-COOPER  ADD RCV1, RCV8, RCV9 AND RCVA SEGMENTS* CC020603
000310*                     FOR THE RECEIPTS PROJECT (ONLINE PIECE)   * CC020603
000320*                     AND SEGMENTS RCV2, RCV3, RCV4, RCV5, RCV6 * CC020603
000330*                     AND RCV7 FOR THE BATCH PIECES.            * CC020603
000340* 05/30/02 K.MILLS    ADD PACKS TO ORD4                         * KM020530
000350* 05/07/02 D.JAIPERSA ADDED PCS1 SEGMENT FOR REGIONAL PRICING   * DJ020520
000360* 05/07/02 M.YATES    ADDED LOC1-APP-TYP-CODE TO LOC1 SEGMENT   * MY020507
000370* 03/14/02 C.BRANDEL  ADDED PURGE-FLAG TO DOC1 SEGMENT.         * CB020314
000380* 03/14/02 K.MILLS    ADDED ROLE TO AUD1 SEGMENT                * KM020314
000390* 02/21/02 S.PHILLIPS ADDED DPT1 AND HRC1 STATUS                * SD020221
000400* 12/06/01 A.FEN      ADDED ERROR-FLG TO VSP1,VMF1,VCR1 SEGMENTS* AF011206
000410* 11/12/01 C.BRANDEL  MODIFY LOC1 SEGMENT, ADD LOC2-7 SEGMENTS. * CB011112
000420* 10/24/01 S.PHILLIPS MODIFIED VSP1 SEGMENT DEFINITION          * LH001104
000430* 10/24/01 S.PHILLIPS ADDED VCR1 AND VMF1 SEGMENT DEFINITION    * LH001204
000440* 10/24/01 L.HARVEY   ADDED PRODUCT-LEVEL TO PRD1 SEGMENT       * LH001304
000450* 09/17/01 J.HOWE     ADDED PCD1 SEGMENT FOR PCD CREATION       * JH001407
000460* 09/11/01 K.MILLS    ADDED INFO TO ORD4 AND NEW SEGMENT ORD5   * KM001501
000470* 07/10/01 D.WYRICK   ADDED NRS1 THRU NRS7 SEGMENTS.            * SO001602
000480* 06/12/01 SUDA O.    ADDED PRDA  SEGMENT FOR MFCTR-VND-NBR     * SO001702
000490* 06/04/01 E.RIVARD   ADDED GIFT REGISTRY SEGMENTS RGS1 - RGS6  * ER001804
000500*                     AND ADDED DAT4 AND NAM1 SEGMENTS.         * ER001904
000510* 05/21/01 R.STEWART  ADDED THE SELLING_LOC_FLG TO THE HRC1     * RS002001
000520*                     SEGMENT.                                  * RS002101
000530* 05/14/01 C.WILFRET  ADDED THE ITK1 AND DGS1 SEGMENTS FOR      * CW002204
000540*                     MASTERSTYLE AND SUBCLASS MAINTENANCE.     * CW002304
000550* 05/02/01 S.PHILLIPS ADDED ERROR-FLAGS TO FOLLOWING SEGMENTS   * SD002402
000560*                     DVN1, HRC1, DPT1, CVN1, DCL1, LOC1, PRD1  * SD002502
000570* 03/23/01 M.YATES    CHANGED SAL2-ORIG-DATE TO 9(08)           * MY002603
000580* 03/23/01 S.PHILLIPS CHANGED DCL1-DEPT-CLASS-DIM-ID TO S9(18)  * MY002706
000590* 03/16/01 M.YATES    ADDED SAL2-RET-ADJ-REASON                 * MY002806
000600* 03/05/01 S.PHILLIPS MADE ALL DIS1 AND TAX1 FIELDS UNSIGNED    * SD002905
000610* 03/05/01 S.PHILLIPS DELETED DVN1, DPT1, CVN1, HRC1 DIM-ID     * SD003005
000620* 03/05/01 S.PHILLIPS REMOVED QTY1 DIM-ID                       * SD003105
000630* 02/26/01 R.STEWART   MODIFIED HRC1-STRUCTURE-ID FROM 9(03)    * RS003206
000640*                      TO X(03)                                 * RS003306
000650* 02/20/01 S.PHILLIPS  MODIFIED DPT1, DCL1, HRC1 DATE TO 9(8)   * SD003400
000660* 02/20/01 S.PHILLIPS  ADDED HDR1-DTS-FLAG                      * SD003500
000670* 02/20/01 S.PHILLIPS  ADDED HDR1-SRC-NBR, QTY1-VSN-ADJ-NBR     * SD003600
000680* 01/22/01 K.MILUM     ADDED LOC1-TYPE-CODE                     * KM003702
000690* 12/12/00 RW STEWART  ADDED TP-NICKNAME TO DVN3 SEGMENT        * RS003802
000700* 12/05/00 JC SMITH    ADDED FLAG ON ORD1 SEGMENT               * CS003904
000710* 11/30/00 M YATES     ADDED AUD2, DIS1, FEE1, GFT1, SAL1, SAL2,* MY004000
000720*                      TAX1, AND TND1 SEGMENTS.  ALSO ADDED     * MY004100
000730*                      QTY1-SEQ-NBR, QTY1-ORIG-SEQ-NBR          * MY004200
000740* 11/06/00 R.STEWART   ADDED PDS-DIV, PDS-GRP TO DPT1           * RS004306
000750* 11/02/00 JC SMITH    ADDED PO DESC TO ORD3                    * CS004402
000760* 10/04/00 R.STEWART   ADDED CVN1, DCL1, DPT1, DVN1, DVN2,      * RS004504
000770*                            DVN3, HRC1 SEGMENTS                * RS004604
000780* 10/03/00 K. MILLS    ADDED RACF ID IN THE AUD1 SEGMENT        * KM004703
000790* 08/03/00 D.JAIPERSAD CHANGE PRD4 AND ADDED PRD5-9             * DJ004803
000800* 08/03/00 K.MILLS     REMOVED VND1 SEGMENT AND ADDED THESE:    * KM004903
000810*                      ADR1-2, HDR4, ORD1-4 AND VSP1            * KM005003
000820* 07/05/00 B06AXF      ADDED PRD5-PROD-DIM-ID                   * AF005105
000830* 06/15/00 L.HARVEY    ADDED TO THE PRD3 SEGMENT                * LH005205
000840* 01/26/00 B06DLW      ADD AUD1-TRACE-FLG AND AUD1-TRACE-ID     * AF005300
000850* 12/20/99 B06AXF      CHANGED PRD5 'MVE' FIELDS ON 'NEW' FIELDS* AF005400
000860* 11/10/99 B06SLO      ADDED DCL1 FOR DEPARTMENT CLASS MAINT.   * DF005508
000870* 11/10/99 B06SLO      MODIFIED PRD4 FOR LABEL AND UOM          * DF005608
000880* 11/08/99 B06DDF      ADDED PRD5 SEGMENT                       * DF005708
000890*---------------------------------------------------------------*
000900*---------------------------------------------------------------*
000910*  THE VERSION-NBR VALUE WILL BE INCREMENTED ONLY IF AN EXISTING*
000920*  FIELD DEFINITION OR FIELD NAME IS MODIFIED OR EXISTING DATA  *
000930*  LAYOUTS ARE MODIFIED.                                        *
000940*  THIS SHOULD BE EXTREMELY RARE.  CONSIDER IT THE EQUIVALENT   *
000950*  OF A Y2K OR A 99X PROJECT.  IF THE VERSION-NBR CHANGES THEN  *
000960*  ALL SYSTEMS/PROGRAMS THAT USE THIS INCLUDE MUST BE MODIFIED  *
000970*  AND IMPLEMENTED SIMULTANEOUSLY.  IT ALSO HAS AN IMPACT ON THE*
000980*  PREVIOUSLY ARCHIVED DATA AND WOULD REQUIRE A CONVERSION OF   *
000990*  THE ARCHIVE.                                                 *
001000*                                                               *
001010*  THE SEGMENT-VERSION-NBR HAS MORE FLEXIBILITY FOR CHANGE.  IT *
001020*  WILL BE INCREMENTED UNDER 2 CONDITIONS.  1)  IT WAS THE      *
001030*  SEGMENT THAT WAS CHANGED RESULTING IN A VERSION NBR CHANGE   *
001040*  AS DESCRIBED ABOVE.  THEREFORE REQUIRING A CONVERSION OF THE *
001050*  ARCHIVED DATA, OR  2)  IT WAS MODIFED TO UTILIZE THE FILLER  *
001060*  WITH ADDITIONAL FIELD DEFINITIONS.  THE FORMATTING PROGRAMS  *
001070*  WILL NEED TO CODE "SET XXX-VERSION-NBR TO TRUE"  WHEN        *
001080*  FORMATTING A COMMSGF RECORD.  THIS WILL IDENTIFY THE DEFINI- *
001090*  TION USED FOR THE DATA WHEN IT WAS CREATED.  USERS OF THESE  *
001100*  SEGMENTS WILL HAVE TO PROVIDE CODE COMPATIBLE AND EXECUTEABLE*
001110*  WITH THE CURRENT AND ALL PREVIOUS VERSIONS.  THEREFORE, NO   *
001120*  CONVERSION OF THE ARCHIVE WILL BE REQUIRED WITH THIS TYPE OF *
001130*  SEGMENT-VERSION-NBR CHANGE AND SYSTEMS WILL BE ALLOWED TO    *
001140*  BECOME CURRENT ON THEIR OWN TIMELINES.                       *
001150*---------------------------------------------------------------*
001160*
001170 05  FSG-MESSAGE.
001180      10 VERSION-NBR                     PIC  9(02) VALUE 01.
001190      10 SEGMENT-CNT                     PIC  9(04).
001200            88 SEGMENT-MAX                          VALUE 0270.
001210
001220*---------------------------------------------------------------*
001230*  SEGMENT LINE OCCURS BEGINS HERE                              *
001240*---------------------------------------------------------------*
001250      10 SEGMENT-LINE OCCURS 1 TO 270 DEPENDING ON SEGMENT-CNT.
001260         15 SEGMENT-HIERARCHY            PIC  9(18).
001270         15 SEGMENT-HIER-NODES REDEFINES SEGMENT-HIERARCHY.
001280            20 SEGMENT-HIER-NODE OCCURS 9 PIC  9(02).
001290         15 SEGMENT-TYPE                 PIC  X(04).
001300            88 HDR1-SEGMENT-TYPE                    VALUE 'HDR1'.
001310            88 HDR2-SEGMENT-TYPE                    VALUE 'HDR2'.
001320            88 HDR3-SEGMENT-TYPE                    VALUE 'HDR3'.
001330            88 HDR4-SEGMENT-TYPE                    VALUE 'HDR4'.
001340            88 AUD1-SEGMENT-TYPE                    VALUE 'AUD1'.
001350            88 AUD2-SEGMENT-TYPE                    VALUE 'AUD2'. MY010400
001360            88 ADR1-SEGMENT-TYPE                    VALUE 'ADR1'. KM010503
001370            88 ADR2-SEGMENT-TYPE                    VALUE 'ADR2'. KM010603
001370            88 BPR1-SEGMENT-TYPE                    VALUE 'BPR1'. CB030822
001370            88 BPR2-SEGMENT-TYPE                    VALUE 'BPR2'. CB030822
001380            88 CVN1-SEGMENT-TYPE                    VALUE 'CVN1'. KM010703
001390            88 DAT1-SEGMENT-TYPE                    VALUE 'DAT1'.
001400            88 DAT2-SEGMENT-TYPE                    VALUE 'DAT2'. KM010903
001410            88 DAT3-SEGMENT-TYPE                    VALUE 'DAT3'. KM011003
001420            88 DAT4-SEGMENT-TYPE                    VALUE 'DAT4'. ER011104
001430            88 DCL1-SEGMENT-TYPE                    VALUE 'DCL1'. RS011204
001440            88 DGS1-SEGMENT-TYPE                    VALUE 'DGS1'. CW011304
001450            88 DIS1-SEGMENT-TYPE                    VALUE 'DIS1'. MY011400
001460            88 DOC1-SEGMENT-TYPE                    VALUE 'DOC1'.
001470            88 DPT1-SEGMENT-TYPE                    VALUE 'DPT1'. RS011604
001480            88 DVN1-SEGMENT-TYPE                    VALUE 'DVN1'. RS011704
001490            88 DVN2-SEGMENT-TYPE                    VALUE 'DVN2'. RS011804
001500            88 DVN3-SEGMENT-TYPE                    VALUE 'DVN3'. RS011904
001510            88 FEE1-SEGMENT-TYPE                    VALUE 'FEE1'. MY012000
001520            88 GFT1-SEGMENT-TYPE                    VALUE 'GFT1'. MY012100
001530            88 HRC1-SEGMENT-TYPE                    VALUE 'HRC1'. RS012204
001540            88 ITK1-SEGMENT-TYPE                    VALUE 'ITK1'. CW012304
001550            88 LOC1-SEGMENT-TYPE                    VALUE 'LOC1'.
001560            88 LOC2-SEGMENT-TYPE                    VALUE 'LOC2'. CB012507
001570            88 LOC3-SEGMENT-TYPE                    VALUE 'LOC3'. CB012607
001580            88 LOC4-SEGMENT-TYPE                    VALUE 'LOC4'. CB012707
001590            88 LOC5-SEGMENT-TYPE                    VALUE 'LOC5'. CB012807
001600            88 LOC6-SEGMENT-TYPE                    VALUE 'LOC6'. CB012907
001610            88 LOC7-SEGMENT-TYPE                    VALUE 'LOC7'. CB013007
001620            88 LOC8-SEGMENT-TYPE                    VALUE 'LOC8'. AF030708
001630            88 NAM1-SEGMENT-TYPE                    VALUE 'NAM1'. ER013104
001640            88 NRS1-SEGMENT-TYPE                    VALUE 'NRS1'. ER013204
001650            88 NRS2-SEGMENT-TYPE                    VALUE 'NRS2'. ER013304
001660            88 NRS3-SEGMENT-TYPE                    VALUE 'NRS3'. ER013404
001670            88 NRS4-SEGMENT-TYPE                    VALUE 'NRS4'. ER013504
001680            88 NRS5-SEGMENT-TYPE                    VALUE 'NRS5'. ER013604
001690            88 NRS6-SEGMENT-TYPE                    VALUE 'NRS6'. ER013704
001700            88 NRS7-SEGMENT-TYPE                    VALUE 'NRS7'. ER013804
001710            88 ORD1-SEGMENT-TYPE                    VALUE 'ORD1'. KM013903
001720            88 ORD2-SEGMENT-TYPE                    VALUE 'ORD2'. KM014003
001730            88 ORD3-SEGMENT-TYPE                    VALUE 'ORD3'. KM014103
001740            88 ORD4-SEGMENT-TYPE                    VALUE 'ORD4'. KM014203
001750            88 ORD5-SEGMENT-TYPE                    VALUE 'ORD5'. KM014301
001760            88 PCD1-SEGMENT-TYPE                    VALUE 'PCD1'.
001770            88 PCS1-SEGMENT-TYPE                    VALUE 'PCS1'. DJ020520
001780            88 PDF1-SEGMENT-TYPE                    VALUE 'PDF1'. DF021216
001790            88 PRD1-SEGMENT-TYPE                    VALUE 'PRD1'. JH014507
001800            88 PRD2-SEGMENT-TYPE                    VALUE 'PRD2'.
001810            88 PRD3-SEGMENT-TYPE                    VALUE 'PRD3'.
001820            88 PRD4-SEGMENT-TYPE                    VALUE 'PRD4'.
001830            88 PRD5-SEGMENT-TYPE                    VALUE 'PRD5'.
001840            88 PRD6-SEGMENT-TYPE                    VALUE 'PRD6'. DJ015003
001850            88 PRD7-SEGMENT-TYPE                    VALUE 'PRD7'. DJ015103
001860            88 PRD8-SEGMENT-TYPE                    VALUE 'PRD8'. DJ015203
001870            88 PRD9-SEGMENT-TYPE                    VALUE 'PRD9'. DJ015303
001880            88 PRDA-SEGMENT-TYPE                    VALUE 'PRDA'. SO015401
001890            88 PRDB-SEGMENT-TYPE                    VALUE 'PRDB'. KR030731
001900            88 QTY1-SEGMENT-TYPE                    VALUE 'QTY1'.
001910            88 RCV1-SEGMENT-TYPE                    VALUE 'RCV1'. CC020603
001920            88 RCV2-SEGMENT-TYPE                    VALUE 'RCV2'. CC020607
001930            88 RCV3-SEGMENT-TYPE                    VALUE 'RCV3'. CC020617
001940            88 RCV4-SEGMENT-TYPE                    VALUE 'RCV4'. CC020617
001950            88 RCV5-SEGMENT-TYPE                    VALUE 'RCV5'. CC020617
001960            88 RCV6-SEGMENT-TYPE                    VALUE 'RCV6'. CC020617
001970            88 RCV7-SEGMENT-TYPE                    VALUE 'RCV7'. CC020617
001980            88 RCV8-SEGMENT-TYPE                    VALUE 'RCV8'. CC020603
001990            88 RCV9-SEGMENT-TYPE                    VALUE 'RCV9'. CC020603
002000            88 RCVA-SEGMENT-TYPE                    VALUE 'RCVA'. CC020603
002010            88 RGS1-SEGMENT-TYPE                    VALUE 'RGS1'. ER015604
002020            88 RGS2-SEGMENT-TYPE                    VALUE 'RGS2'. ER015704
002030            88 RGS3-SEGMENT-TYPE                    VALUE 'RGS3'. ER015804
002040            88 RGS4-SEGMENT-TYPE                    VALUE 'RGS4'. ER015904
002050            88 RGS5-SEGMENT-TYPE                    VALUE 'RGS5'. ER016004
002060            88 RGS6-SEGMENT-TYPE                    VALUE 'RGS6'. ER016104
002070            88 SAL1-SEGMENT-TYPE                    VALUE 'SAL1'. MY016200
002080            88 SAL2-SEGMENT-TYPE                    VALUE 'SAL2'. MY016300
002090            88 TAX1-SEGMENT-TYPE                    VALUE 'TAX1'. MY016400
002100            88 TND1-SEGMENT-TYPE                    VALUE 'TND1'. MY016500
002110            88 VCR1-SEGMENT-TYPE                    VALUE 'VCR1'. SD016602
002120            88 VMF1-SEGMENT-TYPE                    VALUE 'VMF1'. SD016702
002130            88 VSP1-SEGMENT-TYPE                    VALUE 'VSP1'. KM016803
002140         15 SEGMENT-VERSION-NBR          PIC  9(02).
002150            88 HDR1-VERSION-NBR                     VALUE 01.
002160            88 HDR2-VERSION-NBR                     VALUE 01.
002170            88 HDR3-VERSION-NBR                     VALUE 01.
002180            88 HDR4-VERSION-NBR                     VALUE 01.
002190            88 AUD1-VERSION-NBR                     VALUE 01.
002200            88 AUD2-VERSION-NBR                     VALUE 01.     MY017500
002210            88 ADR1-VERSION-NBR                     VALUE 01.     KM017603
002220            88 ADR2-VERSION-NBR                     VALUE 01.     KM017703
002220            88 BPR1-VERSION-NBR                     VALUE 01.     CB030822
002220            88 BPR2-VERSION-NBR                     VALUE 01.     CB030822
002230            88 CVN1-VERSION-NBR                     VALUE 01.     KM017803
002240            88 DAT1-VERSION-NBR                     VALUE 01.
002250            88 DAT2-VERSION-NBR                     VALUE 01.     KM018003
002260            88 DAT3-VERSION-NBR                     VALUE 01.     KM018103
002270            88 DAT4-VERSION-NBR                     VALUE 01.     ER018204
002280            88 DCL1-VERSION-NBR                     VALUE 01.     DF018308
002290            88 DGS1-VERSION-NBR                     VALUE 01.     CW018404
002300            88 DIS1-VERSION-NBR                     VALUE 01.     MY018500
002310            88 DOC1-VERSION-NBR                     VALUE 01.
002320            88 DPT1-VERSION-NBR                     VALUE 01.     RS018704
002330            88 DVN1-VERSION-NBR                     VALUE 01.     RS018804
002340            88 DVN2-VERSION-NBR                     VALUE 01.     RS018904
002350            88 DVN3-VERSION-NBR                     VALUE 01.     RS019004
002360            88 FEE1-VERSION-NBR                     VALUE 01.     MY019100
002370            88 GFT1-VERSION-NBR                     VALUE 01.     MY019200
002380            88 HRC1-VERSION-NBR                     VALUE 01.     RS019304
002390            88 ITK1-VERSION-NBR                     VALUE 01.     CW019404
002400            88 LOC1-VERSION-NBR                     VALUE 01.
002410            88 LOC2-VERSION-NBR                     VALUE 01.     CB019607
002420            88 LOC3-VERSION-NBR                     VALUE 01.     CB019707
002430            88 LOC4-VERSION-NBR                     VALUE 01.     CB019807
002440            88 LOC5-VERSION-NBR                     VALUE 01.     CB019907
002450            88 LOC6-VERSION-NBR                     VALUE 01.     CB020007
002460            88 LOC7-VERSION-NBR                     VALUE 01.     CB020107
002470            88 LOC8-VERSION-NBR                     VALUE 01.     AF030708
002480            88 NAM1-VERSION-NBR                     VALUE 01.     ER020204
002490            88 NRS1-VERSION-NBR                     VALUE 01.     ER020304
002500            88 NRS2-VERSION-NBR                     VALUE 01.     ER020404
002510            88 NRS3-VERSION-NBR                     VALUE 01.     ER020504
002520            88 NRS4-VERSION-NBR                     VALUE 01.     ER020604
002530            88 NRS5-VERSION-NBR                     VALUE 01.     ER020704
002540            88 NRS6-VERSION-NBR                     VALUE 01.     ER020804
002550            88 NRS7-VERSION-NBR                     VALUE 01.     ER020904
002560            88 ORD1-VERSION-NBR                     VALUE 01.     KM021003
002570            88 ORD2-VERSION-NBR                     VALUE 01.     KM021103
002580            88 ORD3-VERSION-NBR                     VALUE 01.     KM021203
002590            88 ORD4-VERSION-NBR                     VALUE 01.     KM021303
002600            88 ORD5-VERSION-NBR                     VALUE 01.     KM021401
002610            88 PCD1-VERSION-NBR                     VALUE 01.     JH021507
002620            88 PCS1-VERSION-NBR                     VALUE 01.     DJ020520
002630            88 PDF1-VERSION-NBR                     VALUE 01.     DF021216
002640            88 PRD1-VERSION-NBR                     VALUE 01.
002650            88 PRD2-VERSION-NBR                     VALUE 01.
002660            88 PRD3-VERSION-NBR                     VALUE 01.
002670            88 PRD4-VERSION-NBR                     VALUE 01.
002680            88 PRD5-VERSION-NBR                     VALUE 01.     DF022008
002690            88 PRD6-VERSION-NBR                     VALUE 01.     DJ022103
002700            88 PRD7-VERSION-NBR                     VALUE 01.     DJ022203
002710            88 PRD8-VERSION-NBR                     VALUE 01.     DJ022303
002720            88 PRD9-VERSION-NBR                     VALUE 01.     DJ022403
002730            88 PRDA-VERSION-NBR                     VALUE 01.     SO022501
002740            88 PRDB-VERSION-NBR                     VALUE 01.     KR030731
002750            88 QTY1-VERSION-NBR                     VALUE 01.
002760            88 RCV1-VERSION-NBR                     VALUE 01.     CC020603
002770            88 RCV2-VERSION-NBR                     VALUE 01.     CC020607
002780            88 RCV3-VERSION-NBR                     VALUE 01.     CC020617
002790            88 RCV4-VERSION-NBR                     VALUE 01.     CC020617
002800            88 RCV5-VERSION-NBR                     VALUE 01.     CC020617
002810            88 RCV6-VERSION-NBR                     VALUE 01.     CC020617
002820            88 RCV7-VERSION-NBR                     VALUE 01.     CC020617
002830            88 RCV8-VERSION-NBR                     VALUE 01.     CC020603
002840            88 RCV9-VERSION-NBR                     VALUE 01.     CC020603
002850            88 RCVA-VERSION-NBR                     VALUE 01.     CC020603
002860            88 RGS1-VERSION-NBR                     VALUE 01.     ER022704
002870            88 RGS2-VERSION-NBR                     VALUE 01.     ER022804
002880            88 RGS3-VERSION-NBR                     VALUE 01.     ER022904
002890            88 RGS4-VERSION-NBR                     VALUE 01.     ER023004
002900            88 RGS5-VERSION-NBR                     VALUE 01.     ER023104
002910            88 RGS6-VERSION-NBR                     VALUE 01.     ER023204
002920            88 SAL1-VERSION-NBR                     VALUE 01.     MY023300
002930            88 SAL2-VERSION-NBR                     VALUE 01.     MY023400
002940            88 TAX1-VERSION-NBR                     VALUE 01.     MY023500
002950            88 TND1-VERSION-NBR                     VALUE 01.     MY023600
002960            88 VCR1-VERSION-NBR                     VALUE 01.     SD023702
002970            88 VMF1-VERSION-NBR                     VALUE 01.     SD023802
002980            88 VSP1-VERSION-NBR                     VALUE 01.     KM023903
002990         15 SEGMENT-HASHING-NBR          PIC  9(02).
003000
003010         15 SEGMENT-BODY                 PIC  X(80).
003020*---------------------------------------------------------------*
003030*  REDEFINED SEGMENT BODY DEFINITIONS BEGIN HERE                *
003040*---------------------------------------------------------------*
003050         15 HDR1-SEGMENT REDEFINES SEGMENT-BODY.
003060*               SEGMENT-HIER-NODE WILL ALWAYS BE 00.
003070            20 HDR1-SENDER-CODE          PIC  X(06).
003080            20 HDR1-BUSINESS-EVENT       PIC  X(06).
003090            20 HDR1-ROUTING-ZL-DIV-NBR   PIC  9(02).
003100            20 HDR1-ROUTING-LOCN-NBR     PIC  9(06).
003110            20 HDR1-SENT-DATE            PIC  9(08).
003120            20 HDR1-SENT-TIME            PIC  9(08).
003130            20 HDR1-MSG-HASH-TOTAL       PIC  9(09).
003140            20 HDR1-HASH-TOTAL-DIFFERENCE PIC S9(09).
003150            20 HDR1-PROCESS-ID           PIC  X(01).
003160               88 HDR1-TEST-MSG                     VALUE 'T'.
003170               88 HDR1-PROD-MSG                     VALUE 'P'.
003180               88 HDR1-DEBUG-MSG                    VALUE 'D'.
003190            20 HDR1-SRC-NBR              PIC  9(03).              SD026000
003200            20 HDR1-DTS-FLAG             PIC  X(01).              SD026100
003210            20 FILLER                    PIC  X(21).              SD026200
003220
003230         15 HDR2-SEGMENT REDEFINES SEGMENT-BODY.
003240*               SEGMENT-HIER-NODE WILL ALWAYS BE 00
003250            20 HDR2-MSG-ERROR-CODE       PIC  X(15).
003260            20 HDR2-MSG-ERROR-PARM-DATA  PIC  X(65).
003270
003280         15 HDR3-SEGMENT REDEFINES SEGMENT-BODY.
003290*               SEGMENT-HIER-NODE WILL ALWAYS BE 00
003300            20 HDR3-FILE-OR-RUN-NBR      PIC  X(70).
003310            20 FILLER                    PIC  X(10).
003320
003330         15 HDR4-SEGMENT REDEFINES SEGMENT-BODY.
003340             20 HDR4-MSG-GROUP-ID        PIC  X(24).
003350             20 HDR4-MSG-SEQUENCE        PIC  9(09).
003360             20 HDR4-MSG-GROUP-FLAG      PIC  X(01).
003370                 88 HDR4-FIRST-IN-GRP               VALUE 'F'.
003380                 88 HDR4-NEXT-IN-GRP                VALUE 'N'.
003390                 88 HDR4-LAST-IN-GRP                VALUE 'L'.
003400             20 FILLER                   PIC  X(46).
003410
003420         15 AUD1-SEGMENT REDEFINES SEGMENT-BODY.
003430*               SEGMENT-HIER-NODE WILL ALWAYS BE 00
003440            20 AUD1-USER-INITIALS        PIC  X(02).
003450            20 AUD1-USER-EMP-NO          PIC  9(06).
003460            20 AUD1-PGM-NAME             PIC  X(08).
003470            20 AUD1-TRACE-FLG            PIC  X(01).
003480            20 AUD1-TRACE-ID             PIC S9(18).
003490            20 AUD1-RACF-ID              PIC  X(10).              KM029003
003500            20 AUD1-USER-ROLE-NBR        PIC  9(03).              KM020314
003510            20 FILLER                    PIC  X(32).              KM020314
003520
003530         15 AUD2-SEGMENT REDEFINES SEGMENT-BODY.                  MY029300
003540*               SEGMENT-HIER-NODE WILL ALWAYS BE 00               MY029400
003550            20 AUD2-TOTAL-MSG-TRANS      PIC  9(07).              MY029500
003560            20 AUD2-TOTAL-MSG-UNITS      PIC S9(18)               MY029600
003570               SIGN IS LEADING SEPARATE CHARACTER.                MY029700
003580            20 AUD2-TOTAL-MSG-DOLLARS    PIC S9(16)V99            MY029800
003590               SIGN IS LEADING SEPARATE CHARACTER.                MY029900
003600            20 FILLER                    PIC  X(35).              MY030000
003610*---------------------------------------------------------------*
003620*                                                               *
003630*---------------------------------------------------------------*
003640         15 ADR1-SEGMENT REDEFINES SEGMENT-BODY.                  KM030403
003650            20 ADR1-ADDRESS-LINE-1       PIC  X(40).              KM030503
003660            20 ADR1-ADDRESS-LINE-2       PIC  X(40).              KM030603
003670                                                                  KM030703
003680         15 ADR2-SEGMENT REDEFINES SEGMENT-BODY.                  KM030803
003690            20 ADR2-CITY                 PIC  X(30).              KM030903
003700            20 ADR2-STATE                PIC  X(02).              KM031003
003710            20 ADR2-NTN-CODE             PIC  X(02).              KM031103
003720            20 ADR2-ZIP-CODE-CMPLT       PIC  X(12).              KM031203
003730            20 FILLER                    PIC  X(34).              KM031303
003740
003800         15 BPR1-SEGMENT REDEFINES SEGMENT-BODY.                  CB030822
003810            20 BPR1-FT                   PIC  X(01).              CB030822
003820            20 BPR1-DD                   PIC  S9(6)V9.            CB030822
003830            20 BPR1-PTAG                 PIC  X(07).              CB030822
003840            20 BPR1-CREATE-DATE          PIC  9(08).              CB030822
003850*               FORMAT CCYYMMDD                                   CB030822
003860            20 BPR1-LAST-REVIEW-DATE     PIC  9(08).              CB030822
003870*               FORMAT CCYYMMDD                                   CB030822
003880            20 BPR1-MINOUTL              PIC  S9(07).             CB030822
003890            20 BPR1-CSTOCK               PIC  S9(05).             CB030822
003900            20 BPR1-USERMIN              PIC  S9(07).             CB030822
003910            20 BPR1-USERMAX              PIC  S9(07).             CB030822
003920            20 BPR1-DREG                 PIC  X(01).              CB030822
003930            20 BPR1-CONTROL-NUM          PIC  S9(07).             CB030822
003940            20 BPR1-FOQ                  PIC  S9(07).             CB030822
003950            20 BPR1-PURGE-FLAG           PIC  X(01).              CB030822
003960            20 BPR1-ERROR-FLAG           PIC  X(01).              CB030822
003970            20 FILLER                    PIC  X(06).              CB030822

003990         15 BPR2-SEGMENT REDEFINES SEGMENT-BODY.                  CB030822
004000            20 BPR2-RT                   PIC  S999.               CB030822
004010            20 BPR2-LT                   PIC  S999.               CB030822
004020            20 BPR2-OSTRAT               PIC  S999V9.             CB030822
004030            20 BPR2-OP                   PIC  S9(07).             CB030822
004040            20 BPR2-OUTL                 PIC  S9(07).             CB030822
004050            20 BPR2-SSTOCK               PIC  S9(06)V9.           CB030822
004060            20 BPR2-TSUPNV               PIC  S999.               CB030822
004070            20 BPR2-TSUP                 PIC  S999.               CB030822
004080            20 BPR2-LDD                  PIC  S9(6)V9.            CB030822
004090            20 BPR2-BREG                 PIC  X(01).              CB030822
004100            20 BPR2-COMSTK               PIC  S9(07).             CB030822
004110            20 BPR2-OSTRAT-OPTION        PIC  X(01).              CB030822
004120            20 FILLER                    PIC  X(27).              CB030822

003750         15 CVN1-SEGMENT REDEFINES SEGMENT-BODY.                  RS031504
003760            20 CVN1-DIV-NBR              PIC  9(02).              RS031604
003770            20 CVN1-DIV-DEPT             PIC  9(04).              RS031704
003780            20 CVN1-DIV-VENDOR           PIC  9(03).              RS031804
003790            20 CVN1-DEPT                 PIC  9(04).              DF031902
003800            20 CVN1-VENDOR               PIC  9(03).              RS032004
003810            20 CVN1-NAME                 PIC  X(35).              RS032104
003820            20 CVN1-PARENT-DUNS          PIC  9(11).              RS032204
003830            20 CVN1-ERROR-FLAG           PIC  X(1).               SD032302
003840            20 FILLER                    PIC  X(17).              RS032404
003850                                                                  KM032503
003860         15 DAT1-SEGMENT REDEFINES SEGMENT-BODY.
003870            20 DAT1-DATE-QUALIFIER       PIC  X(01).
003880               88 DAT1-GREGORIAN                    VALUE 'G'.
003890               88 DAT1-JULIAN                       VALUE 'J'.
003900               88 DAT1-AMC                          VALUE 'A'.
003910            20 DAT1-MEDIA-DATE           PIC  9(08).
003920            20 DAT1-MEDIA-TIME           PIC  9(08).
003930            20 DAT1-DATE-DIM-ID          PIC S9(09).              DF033302
003940            20 DAT1-ERROR-FLAG           PIC  X(1).               SD033402
003950            20 FILLER                    PIC  X(53).
003960
003970         15 DAT2-SEGMENT REDEFINES SEGMENT-BODY.                  KM033703
003980            20 DAT2-ENTRY OCCURS 4.                               KM033803
003990               25 DAT2-ENTERPRISE-TRANS-TYPE-1 PIC X(02).         KM033903
004000               25 DAT2-DATE-QUALIFIER    PIC  X(01).              KM034003
004010                  88 DAT2-GREGORIAN                 VALUE 'G'.    KM034103
004020                  88 DAT2-JULIAN                    VALUE 'J'.    KM034203
004030                  88 DAT2-AMC                       VALUE 'A'.    KM034303
004040               25 DAT2-DATE              PIC  9(08).              KM034403
004050               25 DAT2-DATE-DIM-ID       PIC S9(08)               KM034503
004060                  SIGN IS LEADING SEPARATE CHARACTER.             KM034603
004070                                                                  KM034703
004080         15 DAT3-SEGMENT REDEFINES SEGMENT-BODY.                  KM034803
004090            20 DAT3-ENTRY OCCURS 2.                               KM034903
004100               25 DAT3-ENTERPRISE-TRANS-TYPE-1 PIC X(02).         KM035003
004110               25 DAT3-DATE-QUALIFIER    PIC  X(01).              KM035103
004120                  88 DAT3-GREGORIAN                 VALUE 'G'.    KM035203
004130                  88 DAT3-JULIAN                    VALUE 'J'.    KM035303
004140                  88 DAT3-AMC                       VALUE 'A'.    KM035403
004150               25 DAT3-DATE              PIC  9(08).              KM035503
004160               25 DAT3-TIME              PIC  9(08).              KM035603
004170               25 DAT3-DATE-DIM-ID       PIC S9(08)               KM035703
004180                  SIGN IS LEADING SEPARATE CHARACTER.             KM035803
004190            20 FILLER                    PIC  X(24).              KM035903
004200
004210         15 DAT4-SEGMENT REDEFINES SEGMENT-BODY.                  ER036104
004220            20 DAT4-ENTRY OCCURS 2.                               ER036204
004230               25 DAT4-ENTERPRISE-TRANS-TYPE-1 PIC X(02).         ER036304
004240               25 DAT4-DATE-QUALIFIER    PIC  X(01).              ER036404
004250                  88 DAT4-GREGORIAN                 VALUE 'G'.    ER036504
004260                  88 DAT4-JULIAN                    VALUE 'J'.    ER036604
004270                  88 DAT4-AMC                       VALUE 'A'.    ER036704
004280               25 DAT4-DATE              PIC  9(08).              ER036804
004290               25 DAT4-TIME              PIC  9(12).              ER036904
004300               25 DAT4-DATE-DIM-ID       PIC S9(08)               ER037004
004310                  SIGN IS LEADING SEPARATE CHARACTER.             ER037104
004320            20 FILLER                    PIC  X(16).              ER037204
004330
004340         15 DCL1-SEGMENT REDEFINES SEGMENT-BODY.                  RS037404
004350            20 DCL1-DIV-NBR              PIC  9(02).              RS037504
004360            20 DCL1-DEPT-NBR             PIC  9(04).              RS037604
004370            20 DCL1-CLASS                PIC  9(02).              RS037704
004380            20 DCL1-DEPT-CLASS-DESC      PIC  X(30).              RS037804
004390            20 DCL1-NEW-DEPT-NBR         PIC  9(04).              RS037904
004400            20 DCL1-NEW-CLASS            PIC  9(02).              RS038004
004410            20 DCL1-EFF-DATE             PIC  9(08).              SD038100
004420            20 DCL1-DEPT-CLASS-DIM-ID    PIC  S9(18).             RS038204
004430            20 DCL1-ERROR-FLAG           PIC  X(1).               SD038302
004440            20 DCL1-ACTIVE-IND           PIC  9(01).              AF020606
004450            20 FILLER                    PIC  X(08).              SD038400
004460
004470         15 DGS1-SEGMENT REDEFINES SEGMENT-BODY.                  CW038604
004480            20 DGS1-DIV-NBR              PIC  9(06).              CW038704
004490            20 DGS1-PARENT-DIV-ID        PIC  9(02).              CW038804
004500            20 DGS1-GROUP                PIC  9(02).              CW038904
004510            20 DGS1-GROUP-NAME           PIC  X(15).              CW039004
004520            20 DGS1-SUBCLASS-NBR         PIC  9(02).              CW039104
004530            20 DGS1-SUBCLASS-NAME        PIC  X(15).              CW039204
004540            20 DGS1-ERROR-FLAG           PIC  X(1).               CW039304
004550            20 FILLER                    PIC  X(37).              CW039404
004560
004570         15 DIS1-SEGMENT REDEFINES SEGMENT-BODY.                  MY039600
004580            20 DIS1-REASON-CODE          PIC  9(02).              MY039700
004590            20 DIS1-DISCOUNT-PCT         PIC 9(09)V99.            SD039805
004600            20 DIS1-DOCUMENT-NBR         PIC 9(18).               SD039905
004610            20 DIS1-DISCOUNT-ID2         PIC 9(18).               SD040005
004620            20 DIS1-DISC-FLAG            PIC  X(01).              MY040100
004630            20 DIS1-NON-TAXABLE-FLAG     PIC  X(01).              MY040200
004640            20 DIS1-COUPON-KEYED         PIC  X(01).              MY040300
004650            20 FILLER                    PIC  X(28).              MY040400
004660
004670         15 DOC1-SEGMENT REDEFINES SEGMENT-BODY.
004680            20 DOC1-ZL-DIV-NBR           PIC  9(02).
004690            20 DOC1-OWNER-LOC-NBR        PIC  9(06).
004700            20 DOC1-TYPE                 PIC  X(04).
004710               88 VENDOR-RECEIPT                    VALUE 'RCPT'.
004720               88 TRANSFER-DAMAGE                   VALUE 'XFRD'.
004730               88 TRANSFER-ACCOM                    VALUE 'XFRA'.
004740               88 RTV-DAMAGE                        VALUE 'RTVD'.
004750               88 RTV-ACCOM                         VALUE 'RTVA'.
004760               88 RTV-SPECIAL-SHIP                  VALUE 'RTVS'.
004770               88 SALES-RECEIPT                     VALUE 'SALE'.
004780               88 ORDER-HEADER                      VALUE 'ORDR'. KM041703
004790            20 DOC1-ID                   PIC  X(68).
004800            20 DOC1-PO-ID REDEFINES DOC1-ID.
004810               25 DOC1-PO-NBR            PIC  9(18).
004820               25 DOC1-SHPG-NBR          PIC  9(05).              KM042103
004830               25 DOC1-PURGE-FLAG        PIC  X.                  CB020314
004840               25 FILLER                 PIC  X(44).              CB020314
004850            20 DOC1-VND-RECEIPT-ID REDEFINES DOC1-ID.
004860               25 DOC1-KEYREC-NBR        PIC  9(07).
004870               25 DOC1-KEYREC-SUFFIX     PIC  9(03).
004880               25 FILLER                 PIC  X(58).
004890            20 DOC1-TRANSFER-ID    REDEFINES DOC1-ID.
004900               25 DOC1-TRANSFER-NBR      PIC  9(18).
004910               25 FILLER                 PIC  X(50).
004920            20 DOC1-VND-RETURN-ID  REDEFINES DOC1-ID.
004930               25 DOC1-RTV-NBR           PIC  9(18).
004940               25 FILLER                 PIC  X(50).
004950
004960         15 DPT1-SEGMENT REDEFINES SEGMENT-BODY.                  RS043404
004970            20 DPT1-DIV-NBR              PIC  9(02).              RS043504
004980            20 DPT1-DEPT-NBR             PIC  9(04).              RS043604
004990            20 DPT1-AR-DESC              PIC  X(22).              RS043704
005000            20 DPT1-DESC-DEPT            PIC  9(04).              RS043804
005010            20 DPT1-PDS-DEPT             PIC  X(01).              RS043904
005020            20 DPT1-PDS-DIV              PIC  9(02).              RS044006
005030            20 DPT1-PDS-GRP              PIC  9(02).              RS044106
005040            20 DPT1-RDS-FLAG             PIC  X(01).              RS044204
005050            20 DPT1-LOW-OWNED            PIC  X(01).              RS044304
005060            20 DPT1-ITM-DEPT             PIC  X(01).              RS044404
005070            20 DPT1-ACCT-CODE            PIC  X(01).              RS044504
005080            20 DPT1-NEW-DEPT             PIC  9(04).              RS044604
005090            20 DPT1-EFF-DATE             PIC  9(08).              SD044700
005100            20 DPT1-ERROR-FLAG           PIC  X(1).               SD044802
005110            20 DPT1-STATUS               PIC  9(03).              SD044802
005120            20 DPT1-ACTIVE-IND           PIC  9(01).              AF020607
005130            20 DPT1-PLN-EXCLUDE-FLAG     PIC  X(01).              AF020606
005140            20 FILLER                    PIC  X(21).              SD044900
005150
005160         15 DVN1-SEGMENT REDEFINES SEGMENT-BODY.                  RS045104
005170            20 DVN1-SOURCE               PIC  X(03).              RS045204
005180            20 DVN1-DIV-NBR              PIC  9(03).              RS045304
005190            20 DVN1-DEPT-NBR             PIC  9(04).              RS045404
005200            20 DVN1-VND-NUMERIC-DESC     PIC  9(04).              RS045504
005210            20 DVN1-DUNS-NO              PIC  9(11).              RS045604
005220            20 DVN1-DUNS-NAME            PIC  X(35).              RS045704
005230            20 DVN1-UPC-FLAG             PIC  X(01).              RS045804
005240            20 DVN1-INACTIVE-DATE        PIC  9(08).              SD045900
005250            20 DVN1-ERROR-FLAG           PIC  X(1).               SD046002
005260            20 FILLER                    PIC  X(10).              SD046100
005270
005280         15 DVN2-SEGMENT REDEFINES SEGMENT-BODY.                  RS046304
005290            20 DVN2-PARENT-VNDR          PIC  9(11).              RS046404
005300            20 DVN2-PARENT-VNDR-NAME     PIC  X(35).              RS046504
005310            20 FILLER                    PIC  X(34).              RS046604
005320
005330         15 DVN3-SEGMENT REDEFINES SEGMENT-BODY.                  RS046804
005340            20 DVN3-PID-ASSIGN-FLAG       PIC  X(01).             RS046904
005350            20 DVN3-CORP-PARENT-VNDR      PIC  9(11).             RS047004
005360            20 DVN3-CORP-PARENT-VNDR-NAME PIC  X(35).             RS047104
005370            20 DVN3-TP-NICKNAME           PIC  X(11).             RS047202
005380            20 FILLER                     PIC  X(22).             RS047302
005390
005400         15 FEE1-SEGMENT REDEFINES SEGMENT-BODY.                  MY047500
005410            20 FEE1-FEE-TYPE              PIC  9(02).             MY047600
005420            20 FEE1-NON-TAXABLE-FLAG      PIC  X(01).             MY047700
005430            20 FILLER                     PIC  X(77).             MY047800
005440
005450         15 GFT1-SEGMENT REDEFINES SEGMENT-BODY.                  MY048000
005460            20 GFT1-CERT-CARD-CODE        PIC  9(02).             MY048100
005470            20 GFT1-APPROVAL-CODE         PIC  X(08).             MY048200
005480            20 GFT1-BEGIN-NBR             PIC  9(18).             MY048300
005490            20 GFT1-END-NBR               PIC  9(18).             MY048400
005500            20 GFT1-NON-TAXABLE-FLAG      PIC  X(01).             MY048500
005510            20 GFT1-EXPIRATION-DATE       PIC  9(08).             MY048600
005520            20 GFT1-HARD-DECLINE          PIC  X(01).             MY048700
005530            20 FILLER                     PIC  X(24).             MY048800
005540
005550         15 HRC1-SEGMENT REDEFINES SEGMENT-BODY.                  RS049004
005560            20 HRC1-DIV-NBR              PIC  9(02).              RS049104
005570            20 HRC1-STRUCTURE-ID         PIC  X(03).              RS049204
005580            20 HRC1-COMPONENT-LEVEL      PIC  9(02).              RS049304
005590            20 HRC1-COMPONENT-ID         PIC  9(09).              RS049404
005600            20 HRC1-PARENT-LEVEL         PIC  9(02).              RS049504
005610            20 HRC1-PARENT-ID            PIC  9(09).              RS049604
005620            20 HRC1-EFF-DATE             PIC  9(08).              SD049700
005630            20 HRC1-NAME                 PIC  X(32).              RS049804
005640            20 HRC1-ERROR-FLAG           PIC  X(1).               SD049902
005650            20 HRC1-SELLING-LOC-FLG      PIC  X(1).               RS050001
005660            20 HRC1-STATUS               PIC  9(3).               SD020221
005670            20 FILLER                    PIC  X(8).               RS050101
005680
005690         15 ITK1-SEGMENT REDEFINES SEGMENT-BODY.                  CW050304
005700            20 ITK1-DIV-NBR              PIC  9(02).              CW050404
005710            20 ITK1-DIV-MASTER-STYLE     PIC  9(06).              CW050504
005720            20 ITK1-MASTER-STYLE         PIC  9(05).              CW050604
005730            20 ITK1-DESCRIPTION          PIC  X(20).              CW050704
005740            20 ITK1-ERROR-FLAG           PIC  X(1).               CW050804
005750            20 FILLER                    PIC  X(46).              CW050904
005760
005770         15 LOC1-SEGMENT REDEFINES SEGMENT-BODY.
005780            20 LOC1-ZL-OWNER-DIV-NBR     PIC  9(02).
005790            20 LOC1-ZL-STORE-NBR         PIC  9(04).
005800            20 LOC1-INVENTORY-LOCN-NBR   PIC  9(06).
005810            20 LOC1-LOC-DIM-ID           PIC S9(09).              DF051502
005820            20 LOC1-OWNER-LOCN-NBR       PIC  9(06).
005830            20 LOC1-TYPE-CODE            PIC  X(03).              KM051702
005840            20 LOC1-ERROR-FLAG           PIC  X(1).               SD051802
005850            20 LOC1-STAT-CODE            PIC  X(1).               CB011112
005860            20 LOC1-ABBR                 PIC  X(03).              CB011112
005870            20 LOC1-SQ-FT                PIC  9(10).              CB011112
005880            20 LOC1-SHORT-NAME           PIC  X(10).              CB011112
005890            20 LOC1-RETURN-TYPE          PIC  9(05).              CB011112
005900            20 LOC1-OLD-ZL-STORE-NBR     PIC  9(04).              CB011112
005910            20 LOC1-OLD-ZL-DIVN-NBR      PIC  9(04).              CB011112
005920            20 LOC1-ADD-CHG-DEL-CODE     PIC  X(1).               CB011112
005930            20 LOC1-APP-TYP-CODE         PIC  X(3).               MY020507
005940            20 FILLER                    PIC  X(08).              MY020507
005950
005960         15 LOC2-SEGMENT REDEFINES SEGMENT-BODY.                  CB011112
005970            20 LOC2-LOC-NAME             PIC  X(50).              CB011112
005980            20 LOC2-OPEN-DATE            PIC  9(08).              RS030510
005990            20 LOC2-CLOSED-DATE          PIC  9(08).              RS030510
006000            20 FILLER                    PIC  X(14).              CB011112
006010
006020         15 LOC3-SEGMENT REDEFINES SEGMENT-BODY.                  CB011112
006030            20 LOC3-DIVN-NBR             PIC  9(06).              CB011112
006040            20 LOC3-DIVN-NAME            PIC  X(50).              CB011112
006050            20 FILLER                    PIC  X(24).              CB011112
006060
006070         15 LOC4-SEGMENT REDEFINES SEGMENT-BODY.                  CB011112
006080            20 LOC4-FACLTY-NBR           PIC  9(06).              CB011112
006090            20 LOC4-FACLTY-NAME          PIC  X(50).              CB011112
006100            20 FILLER                    PIC  X(24).              CB011112
006110
006120         15 LOC5-SEGMENT REDEFINES SEGMENT-BODY.                  CB011112
006130            20 LOC5-NPL-NBR              PIC  9(06).              CB011112
006140            20 LOC5-NPL-NAME             PIC  X(50).              CB011112
006150            20 FILLER                    PIC  X(24).              CB011112
006160
006170         15 LOC6-SEGMENT REDEFINES SEGMENT-BODY.                  CB011112
006180            20 LOC6-CORP-NBR             PIC  9(06).              CB011112
006190            20 LOC6-CORP-NAME            PIC  X(50).              CB011112
006200            20 FILLER                    PIC  X(24).              CB011112
006210
006220         15 LOC7-SEGMENT REDEFINES SEGMENT-BODY.                  CB011112
006230            20 LOC7-PRN-NBR              PIC  9(06).              CB011112
006240            20 LOC7-PRN-NAME             PIC  X(50).              CB011112
006250            20 FILLER                    PIC  X(24).              CB011112
006260
006270         15 LOC8-SEGMENT REDEFINES SEGMENT-BODY.                  AF030708
006280            20 LOC8-COUPON-REGION-NBR    PIC  9(06).              AF030708
006290            20 LOC8-COUPON-REGION-DESC   PIC  X(50).              AF030708
006300            20 FILLER                    PIC  X(24).              AF030708
006310
006320         15 NAM1-SEGMENT REDEFINES SEGMENT-BODY.                  ER055704
006330            20 NAM1-FIRST                PIC  X(20).              ER055804
006340            20 NAM1-MIDDLE               PIC  X(20).              ER055904
006350            20 NAM1-LAST                 PIC  X(30).              ER056004
006360            20 NAM1-TITLE                PIC  X(05).              ER056104
006370            20 NAM1-SUFFIX               PIC  X(05).              ER056204
006380
006390         15 NRS1-SEGMENT REDEFINES SEGMENT-BODY.
006400            20 NRS1-NRF-SIZE-NBR         PIC  9(05).
006410            20 NRS1-MED-DESC             PIC  X(10).
006420            20 NRS1-SHORT-DESC           PIC  X(10).
006430            20 NRS1-LONG-DESC            PIC  X(30).
006440            20 NRS1-METHOD-NBR           PIC  9(05).
006450            20 NRS1-SHORT-SORT-ORDER     PIC  9(05).
006460            20 NRS1-SHORT-SORT-GRP       PIC  9(05).
006470            20 NRS1-NRF-LAST-UPDATE      PIC  9(08).
006480            20 NRS1-ERROR-FLAG           PIC  X(01).
006490            20 NRS1-CALL-ID              PIC  X(01).
006500               88  NRS1-NRS              VALUE 'N'.
006510               88  NRS1-WEB              VALUE 'W'.
006520               88  NRS1-USR              VALUE 'U'.
006530
006540         15 NRS2-SEGMENT REDEFINES SEGMENT-BODY.
006550            20 NRS2-CATEGORY-NAME        PIC  X(50).
006560            20 FILLER                    PIC  X(30).
006570
006580         15 NRS3-SEGMENT REDEFINES SEGMENT-BODY.
006590            20 NRS3-GRP-SELECT-NAME      PIC  X(50).
006600            20 FILLER                    PIC  X(30).
006610
006620         15 NRS4-SEGMENT REDEFINES SEGMENT-BODY.
006630            20 NRS4-PRIM-DESC            PIC  X(50).
006640            20 NRS4-PRIM-UNIT-TYP-NM     PIC  X(25).
006650            20 FILLER                    PIC  X(05).
006660
006670         15 NRS5-SEGMENT REDEFINES SEGMENT-BODY.
006680            20 NRS5-SEC-DESC             PIC  X(50).
006690            20 NRS5-SEC-UNIT-TYP-NM      PIC  X(25).
006700            20 FILLER                    PIC  X(05).
006710
006720         15 NRS6-SEGMENT REDEFINES SEGMENT-BODY.
006730            20 NRS6-HEADING1             PIC  X(50).
006740            20 FILLER                    PIC  X(30).
006750
006760         15 NRS7-SEGMENT REDEFINES SEGMENT-BODY.
006770            20 NRS7-HEADING2             PIC  X(50).
006780            20 FILLER                    PIC  X(30).
006790
006800         15 ORD1-SEGMENT REDEFINES SEGMENT-BODY.                  KM060503
006810            20 ORD1-REF-NBR              PIC  9(11).              KM060603
006820            20 ORD1-DECONSLDTR-ID        PIC  X(02).              KM060703
006830            20 ORD1-TERMS-TYPE           PIC  X(03).              KM060803
006840            20 ORD1-TERMS-DAYS           PIC  9(05).              KM060903
006850            20 ORD1-TERMS-PCT            PIC  9(03)V99.           KM061003
006860            20 ORD1-EVT-NBR              PIC  9(15).              KM061103
006870            20 ORD1-EVT-CODE             PIC  X(09).              KM061203
006880            20 ORD1-ADDR-SEQ-NBR         PIC  9(05).              KM061303
006890            20 ORD1-STAT-NBR             PIC  9(03).              KM061403
006900            20 ORD1-PO-TYP-NBR           PIC  9(03).              KM061503
006910            20 ORD1-ORIGIN-NBR           PIC  9(03).              KM061603
006920            20 ORD1-GEN-NBR              PIC  9(03).              KM061703
006930            20 ORD1-DIST-NBR             PIC  9(03).              KM061803
006940            20 ORD1-AUTO-REPLN-F         PIC  X(01).              KM061903
006950            20 ORD1-REINSTATED-F         PIC  X(01).              KM062003
006960            20 ORD1-HANGER-REQ-F         PIC  X(01).              KM062103
006970            20 ORD1-RTL-TCKT-REQ-F       PIC  X(01).              KM062203
006980            20 ORD1-OTR-F                PIC  X(01).              KM062303
006990            20 ORD1-EDI-F                PIC  X(01).              KM062403
007000            20 ORD1-ALLOC-EXISTS-F       PIC  X(01).              CS062505
007010            20 FILLER                    PIC  X(03).              CS062605
007020
007030         15 ORD2-SEGMENT REDEFINES SEGMENT-BODY.                  KM062803
007040*               SEGMENT-HIER-NODE BE A SEQUENCE NUMBER 1 - 6
007050            20 ORD2-AMC-PRD              PIC  9(03).              KM063003
007060            20 ORD2-AMC-PRD-ABBR         PIC  X(03).              KM063103
007070            20 ORD2-RCPT-FLOW-DLR        PIC  9(09)V99.           KM063203
007080            20 ORD2-RCPT-FLOW-PCT        PIC  9(07)V99.           KM063303
007090            20 ORD2-RCPT-FLOW-YR         PIC  X(04).              KM063403
007100            20 ORD2-RCPT-FLOW-SEQ        PIC  X(04).              KM063503
007110            20 FILLER                    PIC  X(46).              KM063603
007120
007130         15 ORD3-SEGMENT REDEFINES SEGMENT-BODY.                  KM063803
007140*               SEGMENT-HIER-NODE BE A SEQUENCE NUMBER 1 - 99
007150            20 ORD3-SHPG-NBR             PIC  9(05).              KM064003
007160            20 ORD3-SHPG-REL-TYP         PIC  X(02).              KM064103
007170            20 ORD3-DESC                 PIC  X(30).              CS064202
007180            20 FILLER                    PIC  X(43).              CS064302
007190
007200         15 ORD4-SEGMENT REDEFINES SEGMENT-BODY.                  KM064503
007210            20 ORD4-PID-PO-LINE-ITM      PIC  9(05).              KM064603
007220            20 ORD4-CLR-PO-LINE-ITM      PIC  9(05).              KM064703
007230            20 ORD4-PO-LINE-ITM          PIC  9(05).              KM064803
007240            20 ORD4-SZ-GRP-MDL-NAME      PIC  X(30).              KM064903
007250            20 ORD4-PRE-PACK-F           PIC  X(01).              KM065003
007260            20 ORD4-PACK-MULTIPLE        PIC  9(05).              KM065101
007270            20 ORD4-SZ-SCALE-CD          PIC  X(02).              KM065201
007280            20 ORD4-UNIT-CD              PIC  X(01).              KM065301
007290            20 ORD4-QTY-OR-PCT           PIC  9(05)V9999.         KM065401
007300            20 ORD4-UNITS                PIC S9(07)               KM065601
007310                  SIGN IS LEADING SEPARATE CHARACTER.             KM065601
007320            20 ORD4-PACKS                PIC  9(05).              DF021004
007330            20 ORD4-ENTERPRISE-TRANS-TYPE PIC  X(01).             KM020716
007340               88 ORD4-PRODUCT-LEVEL     VALUE 'P'.               KM020716
007350               88 ORD4-LOCATION-LEVEL    VALUE 'L'.               KM020716
007360            20 ORD4-PACKS-PER-CARTON      PIC  X(03).             VS021004
007370
007380         15 ORD5-SEGMENT REDEFINES SEGMENT-BODY.                  KM065901
007390            20 ORD5-LOC-GRP-NAME         PIC  X(30).              KM066001
007400            20 ORD5-SUB-LOC-GRP-NAME     PIC  X(30).              KM066101
007410            20 ORD5-UNIT-CD              PIC  X(01).              KM066201
007420            20 ORD5-QTY-OR-PCT           PIC  9(05)V9999.         KM066301
007430            20 ORD5-UNITS                PIC S9(07)               KM066401
007440                  SIGN IS LEADING SEPARATE CHARACTER.             KM066501
007450            20 FILLER                    PIC  X(02).              KM066601
007460
007470         15 PCD1-SEGMENT REDEFINES SEGMENT-BODY.                  JH066807
007480            20 PCD1-SOURCE-CODE          PIC  9(02).              JH066907
007490            20 PCD1-REF-NBR              PIC  X(10).              JH067007
007500            20 PCD1-PC-TYPE              PIC  9(02).              JH067107
007510            20 PCD1-ORIG-PC-TYPE         PIC  9(05).              JH067207
007520            20 PCD1-ORIG-SOURCE-CODE     PIC  9(02).              JH067307
007530            20 FILLER                    PIC  X(59).              JH067407
007540
007550         15 PCS1-SEGMENT REDEFINES SEGMENT-BODY.                  DJ020520
007560            20 PCS1-RGNSTAT    OCCURS 10 PIC  9(02).              DJ020520
007570            20 FILLER                    PIC  X(60).              DJ020520
007580
007590         15 PDF1-SEGMENT REDEFINES SEGMENT-BODY.                  DF021216
007600            20 PDF1-CORP-SKU-ID          PIC  9(10).              DF021216
007610            20 PDF1-CORP-PROD-NBR        PIC  9(10).              DF021216
007620            20 PDF1-DIV-PROD-NBR         PIC  9(10).              DF021216
007630            20 PDF1-PCS-SEQ-NBR          PIC  9(05).              DF021216
007640            20 PDF1-ERROR-FLAG           PIC  X(01).              DF021216
007650            20 FILLER                    PIC  X(44).              DF021216
007660
007670         15 PRD1-SEGMENT REDEFINES SEGMENT-BODY.
007680            20 PRD1-DEPT-NBR             PIC  9(04).
007690            20 PRD1-VND-NUMERIC-DESC     PIC  9(03).
007700            20 PRD1-MARKSTYLE-NBR        PIC  9(05).
007710            20 PRD1-COLOR-NBR            PIC  9(02).
007720            20 PRD1-SIZE-NBR             PIC  9(03).
007730            20 PRD1-UPC-NBR              PIC  9(18).
007740            20 PRD1-PROD-DIM-ID          PIC S9(18).              DF068302
007750            20 PRD1-CLASS                PIC  9(02).
007760            20 FILLER                    PIC  X(09).              DF068502
007770            20 PRD1-ZL-OWNER-DIV-NBR     PIC  9(02).
007780            20 PRD1-OWNER-LOCN-NBR       PIC  9(06).
007790            20 PRD1-REPL-FLAG            PIC  X(01).
007800            20 PRD1-ERROR-FLAG           PIC  X(01).              SD068902
007810            20 PRD1-PRODUCT-LEVEL        PIC  X(01).              LH069004
007820               88 PRD1-STYLE-LEVEL    VALUE 'S'.                  LH069104
007830               88 PRD1-CLR-SZ-LEVEL   VALUE 'C'.                  LH069204
007840               88 PRD1-UPC-LEVEL      VALUE 'U'.                  DF021217
007850               88 PRD1-PROD-LEVEL     VALUE 'P'.                  DF021217
007860            20 PRD1-STYMVE-FLAG          PIC  X.                  AF030117
007870            20 FILLER                    PIC  X(04).              AF030117
007880
007890         15 PRD2-SEGMENT REDEFINES SEGMENT-BODY.
007900            20 PRD2-PID-NAME             PIC  X(20).
007910            20 PRD2-PID-DESC             PIC  X(60).
007920
007930         15 PRD3-SEGMENT REDEFINES SEGMENT-BODY.
007940            20 PRD3-NRF-COLOR-NBR        PIC  9(03).
007950            20 PRD3-NRF-COLOR-NAME       PIC  X(10).
007960            20 PRD3-NRF-SIZE-NBR         PIC  9(05).
007970            20 PRD3-NRF-SIZE-NAME        PIC  X(10).
007980            20 PRD3-REPORT-COLOR-NAME    PIC  X(10).
007990            20 PRD3-REPORT-SIZE-NAME     PIC  X(10).
008000            20 PRD3-UCC-ID               PIC  9(15).              LH070605
008010            20 PRD3-LBL-NBR              PIC  9(09).              LH070705
008020            20 PRD3-PRODUCT-GRP-CD       PIC  X(06).              LH070805
008030            20 FILLER                    PIC  X(02).              LH070905
008040
008050         15 PRD4-SEGMENT REDEFINES SEGMENT-BODY.                  DJ071103
008060            20 PRD4-V-STYLE              PIC  X(07).              DJ071203
008070            20 PRD4-DESCRIPTION          PIC  X(30).              DJ071303
008080            20 PRD4-SUBCLASS             PIC  9(02).              DJ071403
008090            20 PRD4-STATUS               PIC  9(02).              DJ071503
008100            20 PRD4-ITR-TYPE             PIC  9(02).              DJ071603
008110            20 PRD4-SKU-TYPE REDEFINES PRD4-ITR-TYPE  PIC 9(02).  DJ071703
008120            20 PRD4-CATEGORY             PIC  9(02).              DJ071803
008130            20 PRD4-BASIC-STOCK-FLAG     PIC  X(01).              DJ071903
008140            20 PRD4-RTL-UNT-OF-MEASURE   PIC  9(02).              DJ072003
008150            20 PRD4-RTL-UNT-OF-MEASURE-DESC  PIC X(04).           DJ072103
008160            20 PRD4-COST-UNT-OF-MEASURE  PIC  9(02).              DJ072203
008170            20 PRD4-COST-UNT-OF-MEASURE-DESC PIC X(04).           DJ072303
008180            20 PRD4-VEND-PACK-SIZE       PIC S9(03).              DJ072403
008190            20 PRD4-MARKSTYLE-BY-COL     PIC  X(01).              DJ072503
008200            20 PRD4-MASTERSTYLE-DIV      PIC  9(02).              DJ072603
008210            20 PRD4-MASTERSTYLE-NUMBER   PIC  9(05).              DJ072703
008220            20 PRD4-DISCONTINUE-DATE     PIC  9(08).              DJ072803
008230            20 PRD4-REORDER-FLAG         PIC  X(01).              DJ072903
008240            20 FILLER                    PIC  X(02).              DJ073003
008250
008260         15 PRD5-SEGMENT REDEFINES SEGMENT-BODY.                  DF073208
008270            20 PRD5-NEW-DEPT-NBR         PIC  9(04).              DF073308
008280            20 PRD5-NEW-VENDOR-NBR       PIC  9(03).              DF073408
008290            20 PRD5-NEW-MKST-NBR         PIC  9(05).              DF073508
008300            20 PRD5-NEW-COLOR-NBR        PIC  9(02).              DF073608
008310            20 PRD5-NEW-SIZE-NBR         PIC  9(03).              DF073708
008320            20 PRD5-NEW-CLASS            PIC  9(02).              DF073808
008330            20 PRD5-PROD-DIM-ID          PIC S9(18).              AF073905
008340*           20 PRD5-STYMVE-FLAG          PIC  X(01).              AF030117
008350            20 FILLER                    PIC  X(43).              AF030117
008360
008370         15 PRD6-SEGMENT REDEFINES SEGMENT-BODY.                  DJ074203
008380            20 PRD6-COST                 PIC S9(05)V99.           DJ074303
008390            20 PRD6-1ST-TICKETED-RTL OCCURS 10 PIC S9(05)V99.     DJ074403
008400            20 FILLER                    PIC  X(03).              DJ074503
008410
008420         15 PRD7-SEGMENT REDEFINES SEGMENT-BODY.                  DJ074703
008430            20 PRD7-OWNED-RETAIL         PIC S9(05)V99.           DJ074803
008440            20 PRD7-TICKETED-RTL OCCURS 10 PIC S9(05)V99.         DJ074903
008450            20 FILLER                    PIC  X(03).              DJ075003
008460
008470         15 PRD8-SEGMENT REDEFINES SEGMENT-BODY.                  DJ075203
008480            20 PRD8-FIRST-RECEIPT-DATE   PIC  9(08).              DJ075303
008490            20 PRD8-LAST-RECEIPT-DATE    PIC  9(08).              DJ075403
008500            20 PRD8-FIRST-SOLD-DATE      PIC  9(08).              DJ075503
008510            20 PRD8-LAST-SOLD-DATE       PIC  9(08).              DJ075603
008520            20 PRD8-FIRST-MARKDOWN-DATE  PIC  9(08).              DJ075703
008530            20 PRD8-LAST-MARKDOWN-DATE   PIC  9(08).              DJ075803
008540            20 PRD8-ISP-SUPER-SHORT-SKU  PIC  X(06).              DJ075903
008550            20 PRD8-EXCLUDE-FROM-PURGE   PIC  X(01).              DJ076003
008560            20 PRD8-OH-LEVEL             PIC  9(02).              DJ076103
008570            20 PRD8-REF-V-STYLE          PIC  X(15).              DJ030114
008580            20 PRD8-1ST-OWNED-RETAIL     PIC S9(05)V99.           DJ076303
008590            20 PRD8-PLU-EXPL-F           PIC  X(01).              DJ030114
008600
008610         15 PRD9-SEGMENT REDEFINES SEGMENT-BODY.                  DJ076603
008620            20 PRD9-CRC-RETURN-DISP      PIC  9(05).              DJ076703
008630            20 PRD9-CRC-DISPLACEMENT     PIC  X(01).              DJ076803
008640            20 PRD9-CRC-VEND-RTRN-PROF   PIC  9(15).              DJ076903
008650            20 FILLER                    PIC  X(59).              DJ077003
008660
008670         15 PRDA-SEGMENT REDEFINES SEGMENT-BODY.                  SO077202
008680            20 PRDA-MFCTR-VND-NBR        PIC 9(15).               SO077302
008690            20 FILLER                    PIC X(65).               SO077402
008700
008710         15 PRDB-SEGMENT REDEFINES SEGMENT-BODY.                  KR030731
008720            20 PRDB-SOURCE-CODE            PIC X(08).             KR030731
008730            20 PRDB-PRICE-CHG-NO           PIC 9(07).             KR030731
008740            20 PRDB-TOTAL-MD-COUNT         PIC 9(02).             KR030731
008750            20 PRDB-BAM-PDF.                                      KR030731
008760               25 PRDB-MARKLEVEL           PIC X(01).             KR030731
008770               25 PRDB-BOG-GROUP           PIC 9(05).             KR030731
008780               25 FILLER                   PIC X(57).             KR030731
008790            20 PRDB-ITR-PDF REDEFINES PRDB-BAM-PDF.               KR030731
008800               25 PRDB-LAST-ONORDER-DATE   PIC 9(08).             KR030731
008810               25 PRDB-MARK-OUT-OF-STOCK   PIC X(01).             KR030731
008820               25 PRDB-NULL-ITM-KEY-2-3    PIC X(02).             KR030731
008830               25 PRDB-OUTDATE             PIC 9(04).             KR030731
008840               25 PRDB-FED-AGE             PIC 9(02).             KR030731
008850               25 PRDB-LAST-PO-NO          PIC 9(07).             KR030731
008860               25 PRDB-TYPE                PIC 99.                KR030731
008870               25 PRDB-LAST-SHIP-DATE      PIC 9(08).             KR030731
008880               25 FILLER                   PIC X(29).             KR030731
008890            20 PRDB-SKU-PDF REDEFINES PRDB-BAM-PDF.               KR030731
008900               25 PRDB-ISP-ITM-EXT-DATE-YMD PIC 9(08).            KR030731
008910               25 PRDB-ISP-SKU-EXT-DATE-YMD PIC 9(08).            KR030731
008920               25 PRDB-NULL-ACTIVITY       PIC X(01).             KR030731
008930               25 PRDB-ISP-ITM-FLAG        PIC X(01).             KR030731
008940               25 PRDB-ISP-SKU-FLAG        PIC X(01).             KR030731
008950               25 PRDB-OLD-DEPT            PIC 9(04).             KR030731
008960               25 PRDB-OLD-VENDOR          PIC 9(03).             KR030731
008970               25 PRDB-OLD-MKST            PIC 9(05).             KR030731
008980               25 PRDB-OLD-CLASS           PIC 9(02).             KR030731
008990               25 FILLER                   PIC X(30).             KR030731
009000
009010         15 QTY1-SEGMENT REDEFINES SEGMENT-BODY.
009020            20 QTY1-ENTERPRISE-TRANS-TYPE-1 PIC  X(06).
009030            20 QTY1-UNITS-1              PIC S9(07).
009040            20 QTY1-DOLLARS-1            PIC S9(09)V99.
009050            20 FILLER                    PIC  X(09).              SD078005
009060            20 QTY1-ENTERPRISE-TRANS-TYPE-2 PIC  X(06).
009070            20 QTY1-UNITS-2              PIC S9(07).
009080            20 QTY1-DOLLARS-2            PIC S9(09)V99.
009090            20 FILLER                    PIC  X(09).              SD078405
009100            20 QTY1-SEQ-NBR              PIC  9(03).              MY078500
009110            20 QTY1-ORIG-SEQ-NBR         PIC  9(03).              MY078600
009120            20 QTY1-VSN-ADJ-NBR          PIC  9(03).              SD078700
009130            20 FILLER                    PIC  X(05).              MY078800
009140
009150         15 RCV1-SEGMENT REDEFINES SEGMENT-BODY.                  CC020603
009160            20 RCV1-DEPT-NBR             PIC  9(04).              CC020603
009170            20 RCV1-KR-KEY.                                       RS020626
009180               25 RCV1-KR-NBR            PIC S9(15).              CC020603
009190               25 RCV1-KR-SUFFIX-NBR     PIC  X(03).              CC020603
009200               25 RCV1-KR-ENTRY-DT       PIC  9(08).              CC020603
009210*           FORMAT YYYYMMDD                                       CC020603
009220            20 RCV1-PO-NBR               PIC S9(11).              CC020603
009230            20 RCV1-PO-ENTRY-DT          PIC  9(08).              CC020603
009240*           FORMAT YYYYMMDD                                       CC020603
009250*****       FILLER RESERVED FOR PO-SHPG-NBR IF NEEDED             RS020828
009260            20 FILLER                    PIC  X(08).              RS020828
009270            20 RCV1-PO-SHPG-NBR          PIC  9(10).              RS020828
009280            20 RCV1-KR-STAT              PIC  X(01).              CC020603
009290            20 RCV1-RCV-STAT             PIC S9(04).              CC020603
009300            20 RCV1-DTL-TRANS-CD         PIC  9(03).              CC020617
009310            20 FILLER                    PIC  X(05).              CC020603
009320         15 RCV2-SEGMENT REDEFINES SEGMENT-BODY.                  CC020603
009330            20 RCV2-AUDIT-PCT            PIC  9V9(4).             CC020607
009340            20 RCV2-CLOSE-OUT-DT         PIC  9(8).               CC020607
009350            20 RCV2-EDI856-SW            PIC  X(01).              CC020607
009360            20 RCV2-ENTRY-SOURCE         PIC  X(01).              CC020607
009370            20 RCV2-FEDFLO-FLAG          PIC  X(01).              CC020607
009380            20 RCV2-FMG-PO-REF-NBR       PIC  9(11).              CC020607
009390            20 RCV2-IVC-MTCHD-FLAG       PIC  X(01).              CC020607
009400            20 RCV2-ORIG-KR-STATUS-CODE  PIC  X(01).              CC020607
009410            20 RCV2-POD-FLAG             PIC  X(01).              CC020607
009420            20 RCV2-RECEIVE-DT           PIC  9(08).              CC020607
009430**          FORMAT YYYYMMDD
009440            20 RCV2-TKT-REQ-CNT          PIC S9(15).              CC020607
009450**          20 RCV2-EDI-MARK-CD          PIC  9(10).              RS020904
009460            20 RCV2-DISTRO-TYPE          PIC  X(01).              RS020905
009470            20 FILLER                    PIC  X(26).              RS020905
009480         15 RCV3-SEGMENT REDEFINES SEGMENT-BODY.                  CC020603
009490            20 RCV3-ENTERED-TS           PIC  9(14).              CC020617
009500            20 RCV3-CREATE-USERID        PIC  X(12).              CC020617
009510            20 RCV3-CREATE-INITIALS      PIC  X(03).              CC020617
009520            20 RCV3-BOOK-INTRANSIT-DT    PIC  9(08).              CC020617
009530            20 RCV3-EXCISE-TAX-DT        PIC  9(08).              CC020617
009540            20 RCV3-KR-STATUS-CHG-DT     PIC  9(08).              CC020617
009550            20 RCV3-SVC-START-DT         PIC  9(08).              CC020617
009560            20 RCV3-FREIGHT-DT           PIC  9(08).              CC020617
009570            20 RCV3-INVOICE-DT           PIC  9(08).              CC020617
009580            20 FILLER                    PIC  X(03).              CC020617
009590         15 RCV4-SEGMENT REDEFINES SEGMENT-BODY.                  CC020603
009600            20 RCV4-CARTON-CNT           PIC  9(05).              CC020617
009610            20 RCV4-FB-CHRG              PIC  9(13)V99.           CC020617
009620            20 RCV4-FB-WEIGHT            PIC  9(07)V99.           CC020617
009630            20 RCV4-FRT-BILL-NBR         PIC  X(22).              CC020617
009640            20 RCV4-FRT-CD               PIC  X(01).              CC020617
009650            20 RCV4-FRT-CHGBCK           PIC  X(05).              CC020617
009660            20 RCV4-FRT-DISTD-FLAG       PIC  X(01).              CC020617
009670            20 RCV4-LAST-CARTON-SEQ      PIC  9(10).              CC020617
009680            20 RCV4-RCPT-TRK-FLAG        PIC  X(01).              CC020617
009690            20 FILLER                    PIC  X(11).              CC020617
009700         15 RCV5-SEGMENT REDEFINES SEGMENT-BODY.                  CC020603
009710            20 RCV5-SHIP-CITY            PIC  X(13).              CC020617
009720            20 RCV5-SHIP-STATE           PIC  X(02).              CC020617
009730            20 RCV5-GOH-CODE             PIC  X(01).              CC020617
009740            20 RCV5-ORIG-APPMNT          PIC  9(12).              CC020617
009750            20 RCV5-PROC-AREA            PIC  X(02).              CC020617
009760            20 RCV5-RCV-BY-LOC           PIC  X(01).              CC020617
009770            20 RCV5-WORK-CENTER          PIC  9(04).              CC020617
009780            20 RCV5-BILL-OF-LADING       PIC  X(30).              CC020617
009790            20 FILLER                    PIC  X(15).              CC020617
009800         15 RCV6-SEGMENT REDEFINES SEGMENT-BODY.                  CC020603
009810            20 RCV6-TRBL-CNT-NBR         PIC  9(03).              CC020617
009820            20 RCV6-TRBL-DTL-CD          PIC  X(02).              CC020617
009830            20 RCV6-TRBL-HIST            PIC  X(01).              CC020617
009840            20 RCV6-TRBL-RSN             PIC  X(01).              CC020617
009850            20 RCV6-TRF-CLAIM-NBR        PIC  X(05).              CC020617
009860            20 RCV6-RECV-LOC             PIC  X(04).              RS020808
009870            20 RCV6-ERM-DC-LOC           PIC  X(04).              RS020808
009880            20 RCV6-ORIG-RECV-LOC        PIC  X(04).              RS020808
009890            20 RCV6-XTRCT-XCPTN-CD       PIC  9(05).              RS020808
009900            20 FILLER                    PIC  X(51).              RS020808
009910         15 RCV7-SEGMENT REDEFINES SEGMENT-BODY.                  CC020603
009920            20 RCV7-REMARKS              PIC  X(31).              CC020617
009930            20 RCV7-REMARKS-MORE         PIC  X(15).              CC020617
009940            20 FILLER                    PIC  X(34).              CC020617
009950         15 RCV8-SEGMENT REDEFINES SEGMENT-BODY.                  CC020603
009960            20 RCV8-DTL-STAT             PIC S9(05).              CC020603
009970            20 RCV8-DTL-TYPE             PIC  X(01).              CC020603
009980            20 RCV8-LAST-UPD-CRT-ID      PIC  X(03).              CC020603
009990            20 RCV8-LAST-UPD-USERID      PIC  X(12).              CC020603
010000            20 RCV8-LAST-UPD-INITIALS    PIC  X(03).              CC020603
010010            20 RCV8-LAST-UPD-TS          PIC  9(14).              CC020603
010020*           FORMAT YYYYMMDDHHMMSS                                 CC020603
010030            20 RCV8-SEQ-NBR              PIC  9(05).              CC020819
010040            20 FILLER                    PIC  X(37).              CC020819
010050         15 RCV9-SEGMENT REDEFINES SEGMENT-BODY.                  CC020603
010060            20 RCV9-ZL-VSTYLE-SUFFIX.                             CC020603
010070               25 RCV9-ZL-VSTYLE         PIC X(07).               CC020603
010080               25 RCV9-ZL-VSTYLE-SUFF    PIC X(03).               CC020603
010090            20 RCV9-VSTYLE-SUBST-SUFFIX.                          CC020603
010100               25 RCV9-VSTYLE-SUBST      PIC X(07).               CC020603
010110               25 RCV9-VSTYLE-SUBST-SUFF PIC X(03).               CC020603
010120            20 RCV9-FPR-ADJ-FLG          PIC X(01).               CC020603
010130            20 RCV9-MARKING-LEVEL        PIC X(01).               CC020603
010140            20 RCV9-POMRCV-ADDED-FLG     PIC X(01).               CC020603
010150            20 RCV9-REDISTRO-CD          PIC X(01).               CC020603
010160            20 RCV9-ENT-STAT             PIC 9(05).               CC020819
010170            20 RCV9-STYLE-NOT-CORP       PIC X(01).               CC020603
010180            20 RCV9-TKT-TYPE             PIC X(04).               CC020603
010190            20 FILLER                    PIC X(46).               CC020603
010200         15 RCVA-SEGMENT REDEFINES SEGMENT-BODY.                  CC020603
010210            20 RCVA-ENTRY-DT             PIC 9(8).                CC020603
010220            20 RCVA-RESOLVE-DT           PIC 9(8).                CC020603
010230            20 RCVA-MIO-DT               PIC 9(8).                CC020603
010240            20 RCVA-KS-EXTRACT-DT        PIC 9(8).                CC020603
010250            20 RCVA-CHG-PO-NBR           PIC 9(11).               CC020603
010260            20 RCVA-MKST                 PIC 9(5).                CC020603
010270            20 RCVA-DC-APPROVE           PIC X(2).                CC020603
010280            20 RCVA-ADJ-TYPE             PIC X(2).                CC020603
010290            20 RCVA-ADJ-STAT             PIC S9(4).               CC020603
010300            20 RCVA-ADJ-ACTION           PIC X(2).                CC020603
010310            20 RCVA-XTRCT-XCPTN-CD       PIC 9(5).                RS020808
010320            20 FILLER                    PIC X(17).               RS020808
010330         15 RGS1-SEGMENT REDEFINES SEGMENT-BODY.                  ER079004
010340            20 RGS1-RGS-ID               PIC  9(09).              ER079104
010350            20 RGS1-OCCN-ID              PIC  X(01).              ER079204
010360            20 RGS1-APP-ID               PIC  X(01).              ER079304
010370            20 RGS1-RFRL-ID              PIC  X(01).              ER079404
010380            20 RGS1-MTHD-ID              PIC  X(01).              ER079504
010390            20 RGS1-RGS-STAT             PIC  X(01).              ER079604
010400            20 RGS1-NBR-OF-GUESTS        PIC  9(04).              ER079704
010410            20 RGS1-RGS-INTERNET-F       PIC  X(01).              ER079804
010420            20 RGS1-SUPPR-ADDR-F         PIC  X(01).              ER079904
010430            20 RGS1-RGS-OCCN-DTE-F       PIC  X(01).              ER080004
010430            20 RGS1-BRLP-ENROL-STAT      PIC  X(01).              BG030911
010440            20 FILLER                    PIC  X(58).              BG030911
010450
010460         15 RGS2-SEGMENT REDEFINES SEGMENT-BODY.                  ER080304
010470            20 RGS2-ITM-ID               PIC  9(09).              ER080404
010480            20 RGS2-RGS-LI-NBR           PIC  9(09).              ER080504
010490            20 RGS2-CATEGORY-ID          PIC  9(04).              ER080604
010500            20 RGS2-ITEM-SET-F           PIC  X(01).              ER080704
010510            20 RGS2-ORIG-RITM-CAT-ID     PIC  9(04).              ER080804
010510            20 RGS2-PRICE                PIC  S9(05)V99.          BG030911
010520            20 FILLER                    PIC  X(46).              BG030911
010530
010540         15 RGS3-SEGMENT REDEFINES SEGMENT-BODY.                  ER081104
010550            20 RGS3-ROLE-ID              PIC  9(04).              ER081204
010560            20 RGS3-CUST-ID              PIC  9(09).              ER081304
010570            20 FILLER                    PIC  X(67).              ER081404
010580
010590         15 RGS4-SEGMENT REDEFINES SEGMENT-BODY.                  ER081604
010600            20 RGS4-ASOC-ID              PIC  9(10).              ER081704
010610            20 RGS4-ASOC-TYP-CD          PIC  X(01).              ER081804
010620            20 FILLER                    PIC  X(69).              ER081904
010630
010640         15 RGS5-SEGMENT REDEFINES SEGMENT-BODY.                  ER082104
010650            20 RGS5-TRN-ID               PIC  9(09).              ER082204
010660            20 RGS5-CMPLTN-STAT-F        PIC  X(01).              ER082304
010670            20 FILLER                    PIC  X(70).              ER082404
010680                                                                  ER082504
010690         15 RGS6-SEGMENT REDEFINES SEGMENT-BODY.                  ER082604
010700            20 RGS6-RGS-LI-NBR           PIC  9(09).              ER082704
010710            20 RGS6-ITM-ID               PIC  9(09).              ER082804
010720            20 RGS6-TITM-SEQ-NBR         PIC  9(04).              ER082904
010730            20 RGS6-TITM-TYP             PIC  X(01).              ER083004
010740            20 RGS6-TITM-STAT            PIC  X(01).              ER083104
010750            20 FILLER                    PIC  X(56).              ER083204
010760
010770         15 SAL1-SEGMENT REDEFINES SEGMENT-BODY.                  MY083400
010780            20 SAL1-REG                  PIC  9(04).              MY083500
010790            20 SAL1-ROLL                 PIC  9(01).              MY083600
010800            20 SAL1-TRANS-NBR            PIC  9(04).              MY083700
010810            20 SAL1-VERSION              PIC  9(02).              MY083800
010820            20 SAL1-SD-ACTION-FLAG       PIC  X(01).              MY083900
010830            20 SAL1-TRAN-TYPE            PIC  9(02).              MY084000
010840            20 SAL1-SUB-TYPE             PIC  X(01).              MY084100
010850            20 SAL1-TRAN-HHMM            PIC  9(05).              MY084200
010860            20 SAL1-RING-SPSN-NBR        PIC  9(09).              MY084300
010870            20 SAL1-COMMISSION-SPSN      PIC  9(09).              MY084400
010880            20 SAL1-REG-QUAL-CODE        PIC  X(02).              MY084500
010890            20 SAL1-REGISTER-ID          PIC  X(02).              MY084600
010900            20 SAL1-BEGIN-TIME           PIC  9(07).              MY084700
010910            20 SAL1-DURATION-TIME        PIC  9(07).              MY084800
010920            20 SAL1-AUTHORIZATION-LEVEL  PIC  9(02).              MY084900
010930            20 SAL1-VOID-FLAG            PIC  X(01).              MY085000
010940            20 SAL1-ORIG-LOC             PIC  9(04).              MY085100
010950            20 SAL1-ERROR-FLAG           PIC  X(01).              MY085200
010960            20 SAL1-NBR-LINE-ITEMS       PIC  9(03).              MY085300
010970            20 SAL1-TAX-EXEMPT-FLAG      PIC  X(01).              MY085400
010980            20 FILLER                    PIC  X(12).              MY085500
010990
011000         15 SAL2-SEGMENT REDEFINES SEGMENT-BODY.                  MY085700
011010            20 SAL2-ORIG-REG             PIC  9(04).              MY085800
011020            20 SAL2-ORIG-SPSN            PIC  9(09).              MY085900
011030            20 SAL2-ORIG-TRANS           PIC  9(05).              MY086000
011040            20 SAL2-ORIG-DATE            PIC  9(08).              MY086103
011050            20 FILLER                    PIC  X(01).              MY086203
011060            20 SAL2-ORIG-TIME            PIC  9(05).              MY086300
011070            20 SAL2-ORIG-LOC-FLAG        PIC  X(01).              MY086400
011080            20 SAL2-ORIG-RINGING-LOC     PIC  9(04).              MY086500
011090            20 SAL2-EXCHG-FLAG           PIC  X(01).              MY086600
011100            20 SAL2-RETURN-MOD-FLAG      PIC  X(01).              MY086700
011110            20 SAL2-RET-EXCH-ADJ-FLAG    PIC  X(01).              MY086800
011120            20 SAL2-RET-ADJ-REASON       PIC  9(02).              MY086906
011130            20 FILLER                    PIC  X(38).              MY087006
011140
011150         15 TAX1-SEGMENT REDEFINES SEGMENT-BODY.                  MY087200
011160            20 TAX1-TAX-CODE             PIC  9(02).              MY087300
011170            20 TAX1-OTHER-TAX-CODE       PIC  9(04).              MY087400
011180            20 TAX1-USER-TAX-REASON      PIC  9(02).              MY087500
011190            20 TAX1-COUNTY-CODE          PIC  9(08).              MY087600
011200            20 TAX1-TAX-CODE2            PIC  9(08).              MY087700
011210            20 TAX1-TAX-RATE             PIC 9(04)V9(05).         SD087805
011220            20 TAX1-ORIG-TAX-RATE        PIC 9(09).               SD087905
011230            20 TAX1-TAX-STATE            PIC  X(02).              MY088000
011240            20 FILLER                    PIC  X(36).              MY088100
011250
011260         15 TND1-SEGMENT REDEFINES SEGMENT-BODY.                  MY088300
011270            20 TND1-TENDER-TYPE          PIC  9(02).              MY088400
011280            20 TND1-COMMON-TENDER-TYPE   PIC  9(02).              MY088500
011290            20 TND1-CHARGE-CARD-ID       PIC  9(04).              MY088600
011300            20 TND1-APPROVAL-CODE        PIC  X(08).              MY088700
011310            20 TND1-APPROVED-FLAG        PIC  X(01).              MY088800
011320            20 TND1-DISAPPROVED-FLAG     PIC  X(01).              MY088900
011330            20 TND1-SUSPENDED-FLAG       PIC  X(01).              MY089000
011340            20 TND1-CUSTOMER-NBR         PIC  9(18).              MY089100
011350            20 TND1-PURE-CUSTOMER-NBR    PIC  9(18).              MY089200
011360            20 FILLER                    PIC  X(25).              MY089300
011370
011380
011390         15 VCR1-SEGMENT REDEFINES SEGMENT-BODY.                  SD089602
011400            20 VCR1-CARRIER-NBR          PIC  9(10).              SD089702
011410            20 VCR1-CARRIER-NAME         PIC  X(35).              SD089802
011420            20 VCR1-CARRIER-SCAC         PIC  X(04).              SD089902
011430            20 VCR1-ERROR-FLAG           PIC  X(01).              AF011206
011440            20 VCR1-TRANS-VND-SCAC       PIC  X(07).              CC020617
011450            20 FILLER                    PIC  X(23).              SD090002
011460                                                                  SD090102
011470        15 VMF1-SEGMENT REDEFINES SEGMENT-BODY.                   SD090202
011480           20 VMF1-MFCTR-NBR            PIC  9(10).               SD090302
011490           20 VMF1-MFCTR-NAME           PIC  X(35).               SD090402
011500           20 VMF1-MFCTR-UCCID          PIC  9(15).               SD090502
011510           20 VMF1-ERROR-FLAG           PIC  X(01).               AF011206
011520           20 FILLER                    PIC  X(19).               SD090602
011530                                                                  SD090702
011540        15 VSP1-SEGMENT REDEFINES SEGMENT-BODY.                   SD090802
011550           20 VSP1-SUPPLIER-NBR         PIC  9(10).               SD090902
011560           20 VSP1-SUPPLIER-NAME        PIC  X(35).               SD091002
011570           20 VSP1-DUNS-NO              PIC  9(11).               SD091102
011580           20 VSP1-DEPT-NBR             PIC  9(04).               SD091202
011590           20 VSP1-VND-NUMERIC-DESC     PIC  9(04).               SD091302
011600           20 VSP1-ERROR-FLAG           PIC  X(01).               AF011206
011610           20 FILLER                    PIC  X(15).               SD091402
011620*---------------------------------------------------------------*
011630*           E N D   O F   C O M M S G F   I N C L U D E         *
011640*---------------------------------------------------------------*
