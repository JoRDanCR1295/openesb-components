000021*    ============================================================         
000022*    GROUP FACTS GR  RECORD FOR OLYMPUS SYSTEM                            
000023*        GF GROUP RECORD    :    G5XRGRB0                      *          
000025*    ------------------------------------------------------------         
000026*    THIS IS A MODIFIED VERSION OF THE GROUP FACTS RECORD.                
000027*    IT CONTAINMS THE GF DATA, AND FIELDS ADDED BY THE                    
000028*    DATA DIFFERENCE UTILITY.                                             
000029*    ------------------------------------------------------------         
000030*    USE COPY REPLACING HIGH LEVEL QUAL :GR:                              
000031*    I.E.  COPY  YWPLGR1  REPLACING ==:GR:== BY ==GR==.                   
000032*    ------------------------------------------------------------         
000033*    ALL OF THE REDEFINES FIELDS HAVE BEEN ELIMINATED.                    
000034*    88 LEVELS REMOVED.                                                   
000035*    ALL GROUP DATE DEFINITIONS CHANGED TO PIC X(08).                     
000036*    ============================================================         
000038 01  :GR:-DDU-GR-REC.                                                     
000040     03  :GR:-DDU-DATA.                                                   
000042         10  :GR:-IBMSNAP-OPERATI               PIC X(1).                 
000043         10  :GR:-IBMSNAP-COMMITS               PIC X(10).                
000044         10  :GR:-IBMSNAP-INTENTS               PIC X(10).                
000045         10  :GR:-IBMSNAP-LOGMARK               PIC X(26).                
000047     03  :GR:-DDU-GR-DATA.                                                
000570     05  :GR:-KEY.                                                        
000580         10  :GR:-PZ-ID                  PIC X(02).                       
000600         10  :GR:-CI-ID                  PIC X(02).                       
000610         10  :GR:-REC-ID                 PIC X(02).                       
000630         10  :GR:-KEY1.                                                   
000640             15  :GR:-GR-ID.                                              
000650                 20  :GR:-GR-ID-PFX      PIC X(02).                       
000660                 20  :GR:-GR-ID-SFX      PIC X(06).                       
000670             15  :GR:-DV-ID-FIL          PIC X(04).                       
000690             15  :GR:-SUB-REC-ID         PIC X(02).                       
000710             15  :GR:-ID-GEN-X.                                           
000720                 20  :GR:-ID-GEN         PIC 9(02).                       
000730         10  :GR:-KEY-FIL                PIC X(02).                       
000740     05  :GR:-ODO-CTRS           COMP.                                    
000750         10  :GR:-FILL-ODO-CTR           PIC S9(04).                      
000760         10  :GR:-CUST-ODO-CTR           PIC S9(04).                      
000770     05  :GR:-SYSTEM-DATA.                                                
000780         10  :GR:-LAST-UPD-DATA.                                          
000790             15  :GR:-LAST-UPD-ONLINE-DATA.                               
000800                  20  :GR:-LAST-UPD-DATE             PIC 9(08).           
001010                 20  :GR:-LAST-UPD-TIME             PIC 9(06).            
001070                 20  :GR:-LAST-UPD-US-ID            PIC X(10).            
001080                 20  :GR:-LOCK-SW                   PIC X(01).            
001090                 20  :GR:-LAST-UPD-PC-ID            PIC X(02).            
001100                 20  :GR:-LAST-UPD-REL              PIC X(03).            
001110             15  :GR:-LAST-UPD-BATCH-DATA.                                
001120                 20  :GR:-LAST-UPD-BATCH-DT         PIC 9(08).            
001220                 20  :GR:-LAST-UPD-BATCH-PROG-ID    PIC X(08).            
001230                 20  :GR:-LAST-UPD-BATCH-FIL        PIC X(04).            
001240         10  :GR:-USER-AREA-TBL.                                          
001250             15  :GR:-USER-AREA-ITEM     PIC X(10)  OCCURS 3.             
001260     05  :GR:-INDIC-DATA.                                                 
001262         10  :GR:-EFF-DT                 PIC X(08).                       
001263         10  :GR:-LIMIT-DT               PIC X(08).                       
001440         10  :GR:-GEN-ID                 PIC X(12).                       
001450         10  :GR:-MEMO                   PIC X(70).                       
001460         10  :GR:-OPTIONS.                                                
001470             15  :GR:-OPT1               PIC X(01).                       
001510             15  :GR:-OPT2               PIC X(01).                       
001560             15  :GR:-OPT3               PIC X(01).                       
001620             15  :GR:-OPT4               PIC X(01).                       
001690             15  :GR:-OPT5               PIC X(01).                       
001740             15  :GR:-OPT6               PIC X(01).                       
001780             15  :GR:-OPT7               PIC X(01).                       
001850             15  :GR:-OPT8               PIC X(01).                       
001900             15  :GR:-OPT9               PIC X(01).                       
001950             15  :GR:-OPT10              PIC X(01).                       
002000             15  :GR:-OPT11              PIC X(01).                       
002050             15  :GR:-OPT12              PIC X(01).                       
002070             15  :GR:-OPT13              PIC X(01).                       
002090             15  :GR:-OPT14              PIC X(01).                       
002110             15  :GR:-OPT15              PIC X(01).                       
002130             15  :GR:-OPT16              PIC X(01).                       
002150             15  :GR:-OPT-UNUSED-FIL     PIC X(04).                       
002160         10  :GR:-ELIG-OPTIONS.                                           
002170             15  :GR:-ELIG-OPT1          PIC X(01).                       
002230             15  :GR:-ELIG-OPT2          PIC X(01).                       
002270             15  :GR:-ELIG-OPT3          PIC X(01).                       
002310             15  :GR:-ELIG-OPT4          PIC X(01).                       
002340             15  :GR:-ELIG-OPT5          PIC X(01).                       
002390             15  :GR:-ELIG-OPT6          PIC X(01).                       
002430             15  :GR:-ELIG-OPT7          PIC X(01).                       
002460             15  :GR:-ELIG-OPT8          PIC X(01).                       
002490             15  :GR:-ELIG-OPT9          PIC X(01).                       
002510             15  :GR:-ELIG-OPT10         PIC X(01).                       
002530             15  :GR:-ELIG-OPT11         PIC X(01).                       
002550             15  :GR:-ELIG-OPT12         PIC X(01).                       
002570             15  :GR:-ELIG-OPT13         PIC X(01).                       
002590             15  :GR:-ELIG-OPT14         PIC X(01).                       
002610             15  :GR:-ELIG-OPT15         PIC X(01).                       
002630             15  :GR:-ELIG-OPT16         PIC X(01).                       
002650         10  :GR:-NAME-N-ADDR.                                            
002660             15  :GR:-AD-OPTIONS.                                         
002670                 20  :GR:-AD-OPT1        PIC X(01).                       
002730                 20  :GR:-AD-OPT2        PIC X(01).                       
002750                 20  :GR:-AD-OPT3        PIC X(01).                       
002770                 20  :GR:-AD-OPT4        PIC X(01).                       
002790                 20  :GR:-AD-OPT-FIL     PIC X(04).                       
002800             15  :GR:-NAME.                                               
002810                 20  :GR:-NAME1          PIC X(30).                       
002820                 20  :GR:-NAME2          PIC X(30).                       
002830             15  :GR:-CONTACT            PIC X(30).                       
002840             15  :GR:-ADDR.                                               
002850                 20  :GR:-ADDR1          PIC X(30).                       
002860                 20  :GR:-ADDR2          PIC X(30).                       
002870                 20  :GR:-ADDR3          PIC X(30).                       
002880                 20  :GR:-ADDR4.                                          
002890                     25  :GR:-CITY       PIC X(19).                       
002900                     25  :GR:-STATE      PIC X(02).                       
002910                     25  :GR:-ZIP        PIC X(11).                       
002920                 20  :GR:-COUNTRY        PIC X(03).                       
002930         10  :GR:-PHONE.                                                  
002940             15  FILLER                  PIC X(01).                       
002950             15  :GR:-PHONE-AREA         PIC X(03).                       
002960             15  :GR:-PHONE-EXCHANGE     PIC X(03).                       
002970             15  :GR:-PHONE-NUMBER       PIC X(04).                       
002980             15  :GR:-PHONE-FIL          PIC X(04).                       
002990         10  :GR:-GP-TBL.                                                 
003000             15  :GR:-GP-ITEM OCCURS 3 TIMES                              
003010                             INDEXED BY :GR:-GP-IDX.                      
003020                 20  :GR:-GP-CHK-NO        PIC X(08).                     
003030                 20  :GR:-GP-CHK-AMT       PIC S9(07)V99  COMP-3.         
003040                 20  :GR:-GP-CHK-APP-ADJ-AMT PIC S9(07)V99 COMP-3.        
003050                 20  :GR:-GP-CHK-DT        PIC X(08).                     
003110                 20  :GR:-GP-ACT-CD        PIC X(01).                     
003120         10  :GR:-UNUSED-FIL-3           PIC X(103).                      
003130         10  :GR:-KEY-BUILDING-CODES.                                     
003140             15  :GR:-BIL-RP-VERSION     PIC X(02).                       
003150             15  :GR:-RN-PTR             PIC X(08).                       
003160             15  :GR:-TD-PTR             PIC X(13).                       
003170             15  :GR:-ALT-ADDR-PTRS.                                      
003180                 20  :GR:-ALT-ADDR1      PIC X(02).                       
003190                 20  :GR:-ALT-ADDR2      PIC X(02).                       
003200                 20  :GR:-ALT-ADDR3      PIC X(02).                       
003210             15  :GR:-KEY-BLD-UNUSED-FIL PIC X(30).                       
003220         10  :GR:-CODES.                                                  
003230             15  :GR:-TYPE-CD            PIC X(01).                       
003340             15  :GR:-ADMIN-CD           PIC X(01).                       
003410             15  :GR:-CS-CD              PIC X(01).                       
003450             15  :GR:-STS-CD             PIC X(01).                       
003500             15  :GR:-DVDND-ELIG-CD      PIC X(01).                       
003540             15  :GR:-AUTO-CERT-ASSIGN-CD PIC X(01).                      
003580             15  :GR:-POOL-CD            PIC X(02).                       
003590             15  :GR:-STATE-CD           PIC X(02).                       
003600             15  :GR:-UND-ID             PIC X(02).                       
003610             15  :GR:-AGE-CHANGE-CD      PIC X(01).                       
003760             15  :GR:-RN-CESSION-NO      PIC X(08).                       
003770             15  :GR:-POL-PER-MONTHS     PIC S9(03) COMP-3.               
003780             15  :GR:-TERMN8-RSN         PIC X(02).                       
003790             15  :GR:-MARKET-DATA.                                        
003800                 20  :GR:-MD-FIELD-OFF   PIC X(03).                       
003810                 20  :GR:-MD-REP         PIC X(04).                       
003820                 20  :GR:-MD-PROD        PIC X(09).                       
003830             15  :GR:-DISPUTE-IND        PIC X(01).                       
003900             15  :GR:-NG-CHECK           PIC X(01).                       
003930         10  :GR:-MIN-PTC-IND            PIC X(01).                       
003970         10  :GR:-CLERK-CD               PIC X(03).                       
003980         10  :GR:-REG-OFF-CD             PIC X(04).                       
003990         10  :GR:-SVC-OFF-CD             PIC X(04).                       
004000         10  :GR:-PLAN-TYPE              PIC X(01).                       
004010         10  :GR:-MULT-DV-IND            PIC X(01).                       
004040         10  :GR:-SMRY                   PIC X(01).                       
004080         10  :GR:-UNUSED-FIL-2           PIC X(23).                       
004090         10  :GR:-POLICIES   OCCURS 10 TIMES                              
004100                             INDEXED BY :GR:-POL-IDX.                     
004110             15  :GR:-POL                PIC X(09).                       
004120         10  :GR:-DATES.                                                  
004130             15  :GR:-ORIG-EFF-DT                PIC X(08).               
004190             15  :GR:-LAST-ANNIV-DT              PIC X(08).               
004250             15  :GR:-CURR-ANNIV-DT              PIC X(08).               
004320             15  :GR:-NEXT-ANNIV-DT              PIC X(08).               
004380             15  :GR:-RENEW-RPT-DT               PIC X(08).               
004440             15  :GR:-TERMN8-DT                  PIC X(08).               
004500             15  :GR:-REINSTATE-DT               PIC X(08).               
004560             15  :GR:-PURGE-DT                   PIC X(08).               
004620             15  :GR:-INFRC-EFF-DT               PIC X(08).               
004680             15  :GR:-INFRC-TERMN8-DT            PIC X(08).               
004740             15  :GR:-INFRC-REINSTATE-DT         PIC X(08).               
004800             15  :GR:-RENEW-DT.                                           
004810                 20  :GR:-RENEW-MM               PIC X(02).               
004820                 20  :GR:-RENEW-DD               PIC X(02).               
004830             15  :GR:-LAST-PD-DT                 PIC X(08).               
004890             15  :GR:-UNUSED-FIL-1               PIC X(32).               
004900****************************************************************          
004910*            REPORT SELECTION SECTION                          *          
004920****************************************************************          
004930     05  :GR:-RS-TBL.                                                     
004940         10  :GR:-RS-ITEM    OCCURS 6 TIMES                               
004950                             INDEXED BY :GR:-RS-IDX.                      
004960             15  :GR:-RS-RPT-TYPE        PIC X(03).                       
004970             15  :GR:-RS-VERSION         PIC X(02).                       
004980             15  :GR:-RS-OPTIONS.                                         
004990                 20  :GR:-RS-OPT1        PIC X(01).                       
005010                 20  :GR:-RS-OPT2        PIC X(01).                       
005030                 20  :GR:-RS-OPT3        PIC X(01).                       
005050                 20  :GR:-RS-OPT4        PIC X(01).                       
005070                 20  :GR:-RS-OPT5        PIC X(01).                       
005090                 20  :GR:-RS-OPT6        PIC X(01).                       
005110                 20  :GR:-RS-OPT7        PIC X(01).                       
005130                 20  :GR:-RS-OPT8        PIC X(01).                       
005150             15  :GR:-RS-REQ-DT          PIC X(08).                       
005210             15  :GR:-RS-FIELD-A         PIC X(02).                       
005220             15  :GR:-RS-FIELD-B         PIC X(02).                       
005230             15  :GR:-RS-FIELD-C         PIC X(03).                       
005240             15  :GR:-RS-FIELD-D         PIC X(08).                       
005250             15  :GR:-RS-FIELD-E         PIC X(08).                       
005260             15  :GR:-RS-FIELD-F         PIC X(08).                       
005270             15  :GR:-RS-FIELD-G         PIC X(08).                       
005280             15  :GR:-RS-UNUSED-FIL      PIC X(09).                       
005290     05  :GR:-MPP-TBL.                                                    
005300         10  :GR:-MPP-ITEM    OCCURS 10 TIMES                             
005310                              INDEXED BY :GR:-MPP-IDX.                    
005320             15  :GR:-MPP-BN-PL.                                          
005330                 20  :GR:-MPP-BN-ID         PIC X(02).                    
005340                 20  :GR:-MPP-PLAN          PIC X(06).                    
005350             15  :GR:-MPP-OPTIONS.                                        
005360                 20  :GR:-MPP-OPT1          PIC X(01).                    
005450                 20  :GR:-MPP-OPT2          PIC X(01).                    
005500                 20  :GR:-MPP-OPT3          PIC X(01).                    
005580                 20  :GR:-MPP-OPT4          PIC X(01).                    
005630             15  :GR:-MPP-UNUSED-FIL1       PIC X(8).                     
005640             15  :GR:-MPP-STOP-LOSS-AMT     PIC S9(07)V99 COMP-3.         
005650             15  :GR:-MPP-UNUSED-AMT1       PIC S9(09)    COMP-3.         
005660             15  :GR:-MPP-FIL               PIC X(26).                    
