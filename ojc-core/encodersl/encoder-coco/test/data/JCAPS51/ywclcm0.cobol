           05  CM-LENGTH                    PIC X(4).                           
           05  CM-IBMSNAP-COMMITSEQ         PIC X(10).                          
           05  CM-IBMSNAP-INTENTSEQ         PIC X(10).                          
           05  CM-IBMSNAP-LOGMARKER         PIC X(26).                          
           05  CM-IBMSNAP-OPERATION         PIC X.                              
003140     05  CM-KEY.                                                         1
003150         10  CM-PZ-ID                        PIC XX.                     1
003170         10  CM-CI-ID                        PIC XX.                     3
003180         10  CM-REC-ID                       PIC XX.                     5
003200         10  CM-KEY1.                                                    7
003210             15  CM-PA-KEY1.                                             7
003220                 20  CM-ME-KEY1.                                         7
003230                     25  CM-PA-GROUP         PIC X(08).                  7
003240                     25  CM-PA-ID            PIC X(09).                 15
003250                 20  CM-PA-REL-NAME.                                    24
003260                     25  CM-PA-REL           PIC X.                     24
003270                     25  CM-PA-ID-NAME       PIC X(06).                 25
003280             15  CM-SYS-ID                   PIC X.                     31
003340             15  CM-CM-KEY1.                                            32
003350                 20  CM-CM-ID                PIC X(09).                 32
003360                 20  CM-CM-GEN-X.                                       41
003370                     25  CM-CM-GEN           PIC 99.                    41
003380         10  CM-KEY-FIL                      PIC XX.                    43
006510**************************************************                        
006520*    C H E C K   F I E L D S                     *                        
006530**************************************************                        
006540     05  CM-C-INFO-G.                                                  601
006560         10  CM-C-INFO                    OCCURS  3  TIMES             601
006570                                          INDEXED BY CM-C-IDX.            
                 15 CM-CHECK-LOG.                                               
                   20 CM-CHECK-LINE-NO           PIC S9(4) COMP.                
                   20 CM-LG-TYPE                 PIC X.                         
                   20 CM-LG-SUB-FNCTN            PIC X.                         
                 15 CM-CHECK-ITEM.                                              
006580             20  CM-C-STATUS                 PIC XX.                   601
006950             20  CM-C-ACTION-DT              PIC 9(8).                 603
007010             20  CM-C-PAYEE                  PIC X.                    609
007070             20  CM-C-PAYEE-ID.                                        610
007080                 25  CM-C-PAYEE-SSN          PIC X(9).                 610
007090                 25  CM-C-PAYEE-ID-SUF       PIC XX.                   619
007100             20  CM-C-COMPUTED               PIC S9(5)V99 COMP-3.      621
007110             20  CM-C-ACCUMULATED            PIC S9(5)V99 COMP-3.      625
007120             20  CM-C-COB-ADJ                PIC S9(5)V99 COMP-3.      629
007130             20  CM-C-ADJ-AMT                PIC S9(5)V99 COMP-3.      633
007140             20  CM-C-ADJ-EXPL               PIC X(03).                637
007150             20  CM-C-SYS-ADJ-AMT            PIC S9(5)V99 COMP-3.      640
007160             20  CM-C-SYS-ADJ-EXPL.                                    644
007170                 25  CM-C-SYS-ADJ-PFX        PIC X.                    644
007210                 25  CM-C-SYS-ADJ-SFX        PIC X.                    645
007220                 25  FILLER                  PIC X.                    646
007230             20  CM-C-SEG-AMT                PIC S9(5)V99 COMP-3.      647
007240             20  CM-C-COMB-CM-ID             PIC X(11).                651
007280             20  CM-C-CHK-AMT                PIC S9(7)V99 COMP-3.      662
007290             20  CM-C-OVERPYMT               PIC S9(5)V99 COMP-3.      667
007300             20  CM-C-REC-AMT                PIC S9(5)V99 COMP-3.      671
007310             20  CM-C-WITHHOLD-AMT           PIC S9(5)V99 COMP-3.      675
007320             20  CM-C-WITHHOLD-EXPL.                                   679
007330                 25  CM-C-WITHHOLD-PFX       PIC X.                    679
007350                 25  CM-C-WITHHOLD-SFX       PIC X.                    680
007360                 25  FILLER                  PIC X.                    681
007370             20  CM-C-TYPE                   PIC X(1).                 682
007400             20  CM-C-SUBF-AND-EXPL.                                   683
007410                 25  CM-C-SUBF               PIC X.                    683
007530                 25  CM-C-EXPL               PIC X(3).                 684
007540             20  FILLER                      PIC X(14).                687
           05 CM-TRAN-SEQ-NR                       PIC S9(9) COMP.              
