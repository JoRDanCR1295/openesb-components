000050*****************************************************************         
000100* YEAR 2000 COMPLIANT  ?  MM/DD/CCYY   REVIEWED BY: ______      *         
000150* YEAR 2000 UPDATED    _  MM/DD/CCYY    UPDATED BY: ______      *         
000200*****************************************************************         
000250* FTL - FORMATED TRANSACTION LOG.  CREATED BY STL EXTRACT.       *        
000300*                                  AND BY DEBIT DTF EXTRACT      *        
000350*----------------------------------------------------------------*        
000400*                         CHANGE HISTORY                         *        
000450*----------------------------------------------------------------*        
000500* MOD DATE  BY WHOM  REASON FOR CHANGE - CHANGE MADE             *        
000550*                                                                         
EPAY1A*  1/09/95  J.COVINGTON (EPAY)   ADDED CONTINUATION RECORD TYPE           
R81768*  6/14/95  F. BRADEN   R0081768 ADDED CONTINUATION RECORD 4.2            
R70235*  7/14/95  F. BRADEN   R0070235 READYPOST MISC FIELDS & FLAGS            
EPAY1C*  7/24/95  F. BRADEN   EPAY1C   A1XPVID FLAG                             
EPAY2 *  2/05/96  F. BRADEN   EPAY2    ISO 61.3, 73, 103, 126.5                 
EPAY2 *                                ADDED TO EBANK EXTENSION RECORD          
NNSS  *  2/07/96  F. BRADEN   NNSS     NATIONAL NET SETTLEMENT SYSTEMS          
NNSS  *                                BIT249 (NO ISO) SWITCHES                 
NNSS  *                                ISO119 STTLMNT SERVICE DATA              
NNSS  *                                - ALL FIELDS ADDED IN TYPE4.2            
NNSS  *                                - EXTENSION RECORD                       
CRIS  *  2/14/96  F. BRADEN   CRIS     CRIS ADVICES FIELD 48 DATA               
ONEATM*  3/01/96  F. BRADEN   ONEATM   FIELD 28                                 
SVCDEV*  3/03/96  F. BRADEN   SVCDEV   STT SERVICE DEVELOPMENT FIELD            
CHIP1A*  4/08/96  F. BRADEN   CHIP1A   CCPS - CHIP CARD PROCESSING SYS          
IDEF63*  6/12/96  F. BRADEN   IDEF63   ADD ISO 63.2                             
CCFS  *  7/09/96  F. BRADEN   CCFS     CURRENCY CONVERSION FEE SERVICE          
VCASH *           F. BRADEN   VCASH    VISA CASH LOAD RESPONSE SIGNATUR         
SUSEG *  7/26/96  C. JOHNSON  SUSEG    SU SEGMENT NEWLY EXTRACTED TRANS         
CHIP1B* 12/13/96  C. JOHNSON  CHIP1B   CCPS - PHASE 1B                          
CHIP1B*                                C0158654                                 
CHIP1B* 12/30/96  F. BRADEN   CHIP1B   SUPPORTING BASEII USE OF BXCFTLN         
CHIP1B* 05/09/97  F. BRADEN   C0XXXXXX CORRECTIONS TO NEW CHIP FIELDS           
BER574* 07/30/97  C. JOHNSON  C0173405 BANKRUPTCY RISK PREDICTION SRV           
VDAS  * 08/25/97  C. JOHNSON  C0173416 VDAS Workstation BIN Address             
BER182* 11/24/97  Y. MOLDAVS  C0185260 COPAC CHIP OFF-LINE                      
BER182*                                PRE-AUTHORIZED CARD                      
BER926* 11/25/97  Y. MOLDAVS  C0185260 VISANET DELIVERY ENHANCEMENT FOR         
BER926*                                CRIS ALERT                               
BER159* 01/07/98  Y. MOLDAVS  BER1599  ADDITIONAL DATA INDICATOR                
BER881* 01/07/98  Y. MOLDAVS  MERCHANT MULTI VOLUME INDICATOR                   
RT1401* 04/23/98  Y. MOLDAVS  RTN1401  VSEC                                     
RT1268* 04/23/98  Y. MOLDAVS  RTN1268  CVV2                                     
RT1682* 04/23/98  Y. MOLDAVS  RTN1682  F44.9                                    
SEPT98* 05/04/98  F. BRADEN   SEPT98   SEPTEMBER 1998 MISC CLEANUP              
B53924* 05/12/98  F. BRADEN   P0253924 MULTIPLE CLEARING SEQ NUMBER FOR         
B53924*                                BOAS/VTRS                                
RT0712* 05/22/98  F. BRADEN   RTN712   VISA ELECTRON PROD CODE EXPANSION        
RT0712*                                & USAGE CODE                             
RT1794* 05/28/98  F. BRADEN   RTN1794  VCCII LOG AREA                           
RT1291* 06/01/98  Y. MOLDAVS  RTN1291  FLEET PHASE III                          
RT1794* 06/16/98  F. BRADEN   RTN1794  F62.15                                   
RT1925* 12/07/98  S. LEWIS    RTN1925  LARGE TICKET & IGOTS                     
RT2013* 12/11/98  F. BRADEN   RTN2013  ELECTRON STIP REPORTING                  
RT1864* 12/17/98  F. BRADEN   RTN1864  U.S. COLLECTION ONLY TRANSACTIONS        
APRL99* 12/18/98  F. BRADEN   APRIL99  APRIL MISC COSMETIC CLEANUPS             
RT2043* 03/09/99  S. ROY      RTN2043  88 LEVELS FOR EXISITNG FIELDS            
RT1852* 03/17/99  F. BRADEN   RTN1852  MASTERCARD FIELD 43 - NAME/LOC           
RT1628* 04/21/99  F. BRADEN   RTN1628  ATM FORMAT STATUS FLAGS                  
RT2395* 03/30/00  F. BRADEN            POS Check Service                        
RT2433* 03/30/00  F. BRADEN            FTL 2-hr feed to VTRS & the world        
003100*----------------------------------------------------------------*        
003150*                                                                         
003150*  ===                                                                    
003150*  ======> The default values are supplied by the "INITIALIZE"            
003150*  ======> COBOL verb except where indicated by comments                  
003150*  ===                                                                    
003150*                                                                         
003200*----------------------------------------------------------------*        
003250*    RECORD HEADER - AUTHORIZATION RECORD COMMON AREA            *        
003300*----------------------------------------------------------------*    
003325     01  BXCFTLAU.    
003350     03  :FTL:-AU-COMMON-DATA.                                            
003400         05  :FTL:-AU-REC-TYPE               PIC  X(001).                 
003450             88  :FTL:-AU-HEADER             VALUE 'H'.                   
003500             88  :FTL:-AU-AUTHORIZATION      VALUE '0'.                   
003550             88  :FTL:-AU-FILE-MAINTENANCE   VALUE '1'.                   
003600             88  :FTL:-AU-CP-NONPACM         VALUE '3'.                   
003700             88  :FTL:-AU-MIP-BILLING-STATS  VALUE '5'.                   
003750             88  :FTL:-AU-MIP-ONUS-STATS     VALUE '6'.                   
003800             88  :FTL:-AU-ARS-ADVICE         VALUE '7'.                   
003850             88  :FTL:-AU-INTER-VIC          VALUE '8'.                   
003900             88  :FTL:-AU-REJECTED-MSG       VALUE '9'.                   
SUSEG              88  :FTL:-AU-SUSEGMENT-MSG      VALUE 'S'.                   
004000             88  :FTL:-AU-TRAILER            VALUE 'T'.                   
004500*----------------------------------------------------------------*        
004550*   AUTHORIZATION RECORD   TYPE '0'.                             *        
004600*   AUTH. ADVICE RECORD    TYPE '7'.                             *        
004650*   OCEMA INTER-VIC RECORD TYPE '8'.                             *        
004700*   LATE RESPONSES         TYPE '9'.                             *        
004750*----------------------------------------------------------------*        
004050         05  :FTL:-AU-VIC                    PIC  X(001).                 
004100             88  :FTL:-AU-VIC-OCW            VALUE 'A'.                   
004150             88  :FTL:-AU-VIC-OCE            VALUE 'B'.                   
004200             88  :FTL:-AU-VIC-OCEMA          VALUE 'C'.                   
004250             88  :FTL:-AU-VIC-OCB            VALUE 'D'.                   
004300             88  :FTL:-AU-VIC-OCAP           VALUE 'E'.                   
004350         05  :FTL:-AU-DATE-YYDDD             PIC S9(005) COMP-3.          
004400         05  :FTL:-AU-TIME-HHMMSS            PIC S9(006) COMP-3.          
004450         05  :FTL:-AU-SRCE-STATION           PIC S9(006) COMP-3.          
004750*----------------------------------------------------------------*        
004460         05  :FTL:-AU-DEST-STATION           PIC S9(006) COMP-3.          
004470         05  :FTL:-AU-PETERLOWRY-SWITCHES.                                
RT2433             10  :FTL:-AU-SW01               PIC X(001).                  
RT2433             10  :FTL:-AU-SW02               PIC X(001).                  
RT2433             10  :FTL:-AU-SW03               PIC X(001).                  
RT2433             10  :FTL:-AU-SW04               PIC X(001).                  
RT2433             10  :FTL:-AU-SW05               PIC X(001).                  
RT2433             10  :FTL:-AU-SW06               PIC X(001).                  
RT2433             10  :FTL:-AU-SW07               PIC X(001).                  
RT2433             10  :FTL:-AU-SW08               PIC X(001).                  
RT2433             10  :FTL:-AU-SW09               PIC X(001).                  
RT2433             10  :FTL:-AU-SW10               PIC X(001).                  
004470         05  FILLER                          PIC X(025).                  
004800     03  :FTL:-AU-RECORD.                                                 
004850         05  :FTL:-AU-STIP                   PIC S9(004) COMP.            
004900            88  :FTL:-AU-STIP-NOT-PROC-BY    VALUE +0.                    
004950            88  :FTL:-AU-STIP-UNPLANED       VALUE +1  +2  +3  +4         
005000                                                   +6  +8  +9 +13         
005050                                                  +16 +17 +18 +19.        
005100            88  :FTL:-AU-STIP-LINE-INVALID       VALUE +1.                
005150            88  :FTL:-AU-STIP-LINE-STOPPED       VALUE +2.                
005200            88  :FTL:-AU-STIP-TERM-STOPPED       VALUE +3.                
005250            88  :FTL:-AU-STIP-TERM-SLOW-POLL     VALUE +4.                
005300            88  :FTL:-AU-STIP-SIGNED-ON          VALUE +5.                
005350            88  :FTL:-AU-STIP-EDIT-ERROR         VALUE +6.                
005400            88  :FTL:-AU-STIP-TERM-SI-MODE       VALUE +7.                
005450            88  :FTL:-AU-STIP-LINE-QUEUE-LONG    VALUE +8.                
005500            88  :FTL:-AU-STIP-RETURNED-MESSAGE   VALUE +9.                
005550            88  :FTL:-AU-STIP-FAST-RESP-REQD     VALUE +10.               
005600            88  :FTL:-AU-STIP-FAST-RESP-ALT-RTG  VALUE +11.               
005650            88  :FTL:-AU-STIP-TERM-SIGNED-OFF    VALUE +12.               
005700            88  :FTL:-AU-STIP-OTHER              VALUE +13.               
005750            88  :FTL:-AU-STIP-VERIFICATION       VALUE +14.               
005800            88  :FTL:-AU-STIP-LCS-CATEGORY-AB    VALUE +15.               
005850            88  :FTL:-AU-STIP-SERIES1-DEST       VALUE +16.               
005900            88  :FTL:-AU-STIP-IN-DATA-FLOW-RESET VALUE +17.               
005950            88  :FTL:-AU-STIP-BIND-NOT-RECEIVED  VALUE +18.               
006000            88  :FTL:-AU-STIP-NODE-NOT-ACTIVE    VALUE +19.               
006050            88  :FTL:-AU-STIP-ATR-TIMEOUT        VALUE +20.               
006100            88  :FTL:-AU-STIP-PVV-ATTEMPTED      VALUE +21.               
006150            88  :FTL:-AU-STIP-FREE-ACTIVITY      VALUE +22.               
006200            88  :FTL:-AU-STIP-FORCE-ON-REQUEST   VALUE +23.               
006250         05  :FTL:-AU-STIP-ADV-CODE          PIC  9(001).                 
006300            88  :FTL:-AU-STIP-ATR-TIMEOUT        VALUE 1.                 
006350            88  :FTL:-AU-STIP-LCS-CATEGORY-AB    VALUE 2.                 
006400            88  :FTL:-AU-STIP-ISS-SI-MODE        VALUE 3.                 
006450            88  :FTL:-AU-STIP-ISS-UNAVAILABLE    VALUE 4.                 
006500            88  :FTL:-AU-MCAS-ISS-RESPONDED      VALUE 5.                 
006550            88  :FTL:-AU-STIP-FORCE-ATR          VALUE 6.                 
006600            88  :FTL:-AU-STIP-POTENTIAL-DUP      VALUE 7.                 
006650            88  :FTL:-AU-STIP-PROBABLE-DUP       VALUE 8.                 
006700            88  :FTL:-AU-STIP-BKUP-CENTER-ADVICE VALUE 9.                 
006750         05  :FTL:-AU-AVS-RESULT             PIC  X(001).                 
006800            88  :FTL:-AU-AVS-NOT-PERFORMED     VALUE    SPACE.            
006850            88  :FTL:-AU-AVS-YADR-Y5ZIP        VALUE    'Y'.              
006900            88  :FTL:-AU-AVS-YADR-Y9ZIP        VALUE    'X'.              
006950            88  :FTL:-AU-AVS-YADR-NZIP         VALUE    'A'.              
007000            88  :FTL:-AU-AVS-NADR-Y5ZIP        VALUE    'Z'.              
007050            88  :FTL:-AU-AVS-NADR-Y9ZIP        VALUE    'W'.              
007100            88  :FTL:-AU-AVS-NADR-NZIP         VALUE    'N'.              
007150            88  :FTL:-AU-AVS-INFO-UNAVAILABLE  VALUE    'U'.              
007200            88  :FTL:-AU-AVS-SYS-UNAVAIL-RETRY VALUE    'R'.              
007250            88  :FTL:-AU-AVS-NOT-SUPPORTED     VALUE    'S'.              
007300         05  :FTL:-AU-TELECD-RESULT-REDEF    REDEFINES                    
007350             :FTL:-AU-AVS-RESULT.                                         
007400             10  :FTL:-AU-TELECODE-RESULT    PIC  X(001).                 
007450                 88 :FTL:-AU-TELECODE-NOT-PERFORMED VALUE SPACE.          
007500                 88 :FTL:-AU-TELECODE-MATCHED       VALUE '0'.            
007550                 88 :FTL:-AU-NO-TELECODE-SEGMENT    VALUE '1'.            
007600                 88 :FTL:-AU-TELECODE-UNMATCHED     VALUE '2'.            
007650                 88 :FTL:-AU-TEL-MANY-VERIF-ATTEMPT VALUE '3'.            
007700                 88 :FTL:-AU-TEL-DAILY-USE-EXCEEDED VALUE '4'.            
007750         05  :FTL:-AU-RESP-SECONDS           PIC S9(004) COMP.            
007800         05  :FTL:-AU-CARDTYPE               PIC  X(001).                 
RT2043             88  :FTL:-AU-VISA VALUE '4' 'P' 'E' 'B' 'H'                  
RT2043                                     'J' 'K' 'R' 'S'.                     
007950             88  :FTL:-AU-VISA-CLASIC       VALUE '4'.                    
008000             88  :FTL:-AU-VISA-PREMIER      VALUE 'P'.                    
008050             88  :FTL:-AU-VISA-ELECTRON     VALUE 'E'.                    
008100             88  :FTL:-AU-VISA-BUSINESS     VALUE 'B'.                    
RT2043             88  :FTL:-AU-VISA-INFINITE     VALUE 'H'.                    
RT2043             88  :FTL:-AU-VISA-PLATINUM     VALUE 'J'.                    
RT2043             88  :FTL:-AU-VISA-SIGNATURE    VALUE 'K'.                    
008300             88  :FTL:-AU-VISA-CORPORATE    VALUE 'R'.                    
008350             88  :FTL:-AU-VISA-PROCUREMENT  VALUE 'S'.                    
008400             88  :FTL:-AU-AMEX              VALUE '3'.                    
008450             88  :FTL:-AU-MASTERCARD        VALUE '5'.                    
008500             88  :FTL:-AU-DINERS            VALUE '6'.                    
008550             88  :FTL:-AU-JCB               VALUE '7'.                    
008600             88  :FTL:-AU-DISCOVER          VALUE '8'.                    
008650             88  :FTL:-AU-PRIVATE           VALUE '9'.                    
008700             88  :FTL:-AU-UNKNOWN-CARD      VALUE 'U'.                    
008750***F102 ACCOUNT ID 1 OR F103  ACCOUNT ID 2                                
008775         05  :FTL:-AU-CARD-NUMBER           PIC  X(028).                  
RT2395         05  :FTL:-AU-F125-CHK-ID-NUMBER    REDEFINES                     
RT2395             :FTL:-AU-CARD-NUMBER.                                        
RT2395             10 :FTL:-AU-F125-CHK-ABA-NUMBER PIC  9(009).                 
RT2395             10 :FTL:-AU-F125-CHK-ACCT-NBR   PIC  X(019).                 
008850         05  :FTL:-AU-CARD-EXP-DATE-MMYY     PIC S9(004)  COMP.           
008900         05  :FTL:-AU-TRAN-AMT               PIC S9(10)V99                
008901                                             COMP-3.                      
008950         05  :FTL:-AU-ACQ-FORMAT-FLAG        PIC  X(001).                 
009000             88  :FTL:-AU-ACQ-FIX-MSG        VALUE '0'.                   
009050             88  :FTL:-AU-ACQ-ISO-MSG        VALUE '1'.                   
009100             88  :FTL:-AU-ACQ-AMEX-MSG       VALUE '2'.                   
009150             88  :FTL:-AU-ACQ-MC-MSG         VALUE '3'.                   
009200         05  :FTL:-AU-ACQ-MSG-TYPE           PIC  9(004).                 
009250         05  :FTL:-AU-ACQ-MSG-REDEF          REDEFINES                    
009300             :FTL:-AU-ACQ-MSG-TYPE.                                       
009350             10 :FTL:-AU-ACQ-MSG-TYPE1       PIC  9(002).                 
009400                88 :FTL:-AU-ACQ-AUTHORIZATION   VALUE 01.                 
009450                88 :FTL:-AU-ACQ-FILE-UPDATE     VALUE 03.                 
009500                88 :FTL:-AU-ACQ-REVERSAL        VALUE 04.                 
009550                88 :FTL:-AU-ACQ-ADMINISTRATIVE  VALUE 06.                 
009600                88 :FTL:-AU-ACQ-NETWORK         VALUE 08.                 
009650                88 :FTL:-AU-ACQ-PRVT-LBL-MSG    VALUE 90.                 
009700                88 :FTL:-AU-ACQ-SECURITY-MODULE VALUE 99.                 
009750             10 :FTL:-AU-ACQ-MSG-TYPE2       PIC  9(002).                 
009800                88 :FTL:-AU-ACQ-REQUEST             VALUE 00.             
009850                88 :FTL:-AU-ACQ-REPEAT              VALUE 01.             
009900                88 :FTL:-AU-ACQ-CONFIRMATION        VALUE 02.             
009950                88 :FTL:-AU-ACQ-RESPONSE            VALUE 10.             
010000                88 :FTL:-AU-ACQ-ONLINE-FM-UPDT-RESP VALUE 12.             
010050                88 :FTL:-AU-ACQ-ADVICE              VALUE 20.             
010100                88 :FTL:-AU-ACQ-ONLINE-FM-UPDT-ADV  VALUE 22.             
010150                88 :FTL:-AU-ACQ-ADVICE-RESPONSE     VALUE 30.             
010200                88 :FTL:-AU-ACQ-ADVICE-RESP-FM-UPDT VALUE 32.             
010250         05  :FTL:-AU-PROC-CODE.                                          
010300             10  :FTL:-AU-PROC-TRAN          PIC  9(002).                 
010350             10  :FTL:-AU-PROC-ACCT          PIC  9(002).                 
BER182                 88 :FTL:-AU-COPAC-ACCT      VALUE  62.                   
010450         05  :FTL:-AU-TRAN-CODE.                                          
010500             88  :FTL:-AU-CHECK-ACCEPTANCE VALUE 'C1' 'C2' 'C3'           
010550                                                 'C4' 'C5'.               
010600             10  :FTL:-AU-TRAN-CD-CHAR1      PIC  X(001).                 
010650             10  :FTL:-AU-TRAN-CD-CHAR2      PIC  X(001).                 
010700         05  :FTL:-AU-SRCE-DEV               PIC  X(001).                 
010750***2.7000 - 7999 OR 1001 - 1699 OR 3001 - 3699 FTL-AU-SYSTEM-MIP=3        
010800         05  :FTL:-AU-ACQ-SYSTEM-TYPE        PIC  X(001).                 
010850             88  :FTL:-AU-SYSTEM-REGULAR     VALUE '0'.                   
010900             88  :FTL:-AU-SYSTEM-MIP         VALUE '3'.                   
010950             88  :FTL:-AU-SYSTEM-POS         VALUE '4'.                   
011000         05  :FTL:-AU-ACQ-BIN                PIC  9(006) COMP-3.          
011050         05  :FTL:-AU-ACQ-PCR                PIC  X(004).                 
011100         05  :FTL:-AU-ACQ-PCR-COUNTRY        PIC  9(003).                 
011150         05  :FTL:-AU-ACQ-PCR-REGION         PIC  X(001).                 
011200         05  :FTL:-AU-ACQ-MERCH-TYPE         PIC  X(004).                 
011250         05  :FTL:-AU-ACQ-MERCH-NUM          PIC S9(006) COMP-3.          
011300         05  :FTL:-AU-ACQ-TERM-ID            PIC  X(015).                 
011350         05  :FTL:-AU-ACQ-TERM-ID-CODE       PIC  X(008).                 
011400         05  :FTL:-AU-ACQ-POSTAL-CODE        PIC  X(009).                 
011450         05  :FTL:-AU-ACQ-POST-REDEF         REDEFINES                    
011500             :FTL:-AU-ACQ-POSTAL-CODE.                                    
011550             10  :FTL:-AU-ACQ-POSTAL-ONE     PIC  X(005).                 
011600             10  :FTL:-AU-ACQ-POSTAL-TWO     PIC  X(004).                 
011650         05  :FTL:-AU-POS-ENT-MODE           PIC  X(003).                 
011700         05  FILLER                          REDEFINES                    
011750             :FTL:-AU-POS-ENT-MODE.                                       
011800             10 :FTL:-AU-POS-ENT-MODE-USED PIC X(002).                    
CHIP1B                88 :FTL:-AU-CHIP-CVV-RELIABLE   VALUE '05'.               
CHIP1B                88 :FTL:-AU-CHIP-CVV-UNRELIABLE VALUE '95'.               
011950             10 :FTL:-AU-POS-ENT-CAPABILITY PIC X(001).                   
012000         05  :FTL:-AU-POS-ENT-CAP-CODE       PIC X(002).                  
012050         05  FILLER                          REDEFINES                    
012100             :FTL:-AU-POS-ENT-CAP-CODE.                                   
012150             10 :FTL:-AU-POS-TERM-TYPE       PIC X(001).                  
012200             10 :FTL:-AU-POS-TERM-CAPABILITY PIC X(001).                  
012250         05  :FTL:-AU-POS-COND-CODE          PIC X(002).                  
012300         05  :FTL:-AU-ACQ-OPER-ID            PIC S9(003) COMP-3.          
012350         05  :FTL:-AU-TRACK1-OR-TRACK2       REDEFINES                    
012400             :FTL:-AU-ACQ-OPER-ID.                                        
012450             10 :FTL:-AU-TRACK-SELECTED      PIC S9(003) COMP-3.          
012500                88 :FTL:-AU-NO-DATA-ON-TRACKS VALUE +0.                   
012550                88 :FTL:-AU-TRACK-ONE         VALUE +1.                   
012600                88 :FTL:-AU-TRACK-TWO         VALUE +2.                   
012650         05  :FTL:-AU-INTER-VIC-FLAG         PIC  X(001).                 
012700             88 :FTL:-AU-INTER-VIC-REC        VALUE '1'.                  
012750         05  :FTL:-AU-ISS-FORMAT-FLAG        PIC  X(001).                 
012800             88 :FTL:-AU-ISS-FIX-MSG          VALUE '0'.                  
012850             88 :FTL:-AU-ISS-ISO-MSG          VALUE '1'.                  
012900             88 :FTL:-AU-ISS-AMEX-MSG         VALUE '2'.                  
012950             88 :FTL:-AU-ISS-MC-MSG           VALUE '3'.                  
013000         05  :FTL:-AU-ISS-MSG-TYPE           PIC  9(004).                 
013050         05  :FTL:-AU-ISS-MSG-REDEF          REDEFINES                    
013100             :FTL:-AU-ISS-MSG-TYPE.                                       
013150             10 :FTL:-AU-ISS-MSG-TYPE1       PIC  9(002).                 
013200                88 :FTL:-AU-ISS-AUTHORIZATION   VALUE 01.                 
013250                88 :FTL:-AU-ISS-FILE-UPDATE     VALUE 03.                 
013300                88 :FTL:-AU-ISS-REVERSAL        VALUE 04.                 
013350                88 :FTL:-AU-ISS-ADMINISTRATIVE  VALUE 06.                 
013400                88 :FTL:-AU-ISS-NETWORK         VALUE 08.                 
013450                88 :FTL:-AU-ISS-PRVT-LBL-MSG    VALUE 90.                 
013500                88 :FTL:-AU-ISS-SECURITY-MODULE VALUE 99.                 
013550             10 :FTL:-AU-ISS-MSG-TYPE2       PIC  9(002).                 
013600                88 :FTL:-AU-ISS-REQUEST             VALUE 00.             
013650                88 :FTL:-AU-ISS-REPEAT              VALUE 01.             
013700                88 :FTL:-AU-ISS-CONFIRMATION        VALUE 02.             
013750                88 :FTL:-AU-ISS-RESPONSE            VALUE 10.             
013800                88 :FTL:-AU-ISS-ONLINE-FM-UPDT-RESP VALUE 12.             
013850                88 :FTL:-AU-ISS-ADVICE              VALUE 20.             
013900                88 :FTL:-AU-ISS-ONLINE-FM-UPDT-ADV  VALUE 22.             
013950                88 :FTL:-AU-ISS-ADVICE-RESPONSE     VALUE 30.             
014000                88 :FTL:-AU-ISS-ADVICE-RESP-FM-UPDT VALUE 32.             
014050* THE FOLLOWING FIELD FTL-AU-ISS-BIN SHOULD NOT BE USED ANYMORE           
014100* SINCE BIN NUMBER CAN BE UPTO 11 BYTES.                                  
014150* INSTEAD FIELD FTL-AU-ISS-INST-ID SHOULD BE USED.                        
014200* THIS FIELD WILL CONTINUE TO BE EXTRACTED.                               
014250* 12/01/94  SROY                                                          
014300***F100 RECEIVING INSTITUTION ID CODE                                     
014350         05  :FTL:-AU-ISS-BIN                PIC S9(006) COMP-3.          
014400*            REMAINDER OF POS-ENT-CAP-CODE                       *        
014450         05  :FTL:-AU-MCAS-REASON-CODE       PIC  9(002).                 
014500         05  :FTL:-AU-ACQ-CVV-FLAGS-REDEF    REDEFINES                    
014550             :FTL:-AU-MCAS-REASON-CODE.                                   
014600             10  :FTL:-AU-ACQ-CVV-FLAGS      PIC S9(004) COMP.            
014650             10  :FTL:-AU-ACQ-CVV-FLAGS-GROUP    REDEFINES                
014700                 :FTL:-AU-ACQ-CVV-FLAGS.                                  
014750                 15 :FTL:-AU-CVV-FLAG1       PIC  X(001).                 
014800                 15 :FTL:-AU-CVV-FLAG2       PIC  X(001).                 
014850         05  :FTL:-AU-MRCH-GROUP             PIC  9(002).                 
014900         05  :FTL:-AU-MRCH-GROUP-REDEF       REDEFINES                    
014950             :FTL:-AU-MRCH-GROUP.                                         
015000             10  :FTL:-AU-MERCH-GROUP        PIC  9(004) COMP.            
015050                 88  :FTL:-AU-MERCH-NOT-PRESENT    VALUE 00.              
015100                 88  :FTL:-AU-MERCH-TRAVEL         VALUE 01.              
015150                 88  :FTL:-AU-MERCH-LODGING        VALUE 02.              
015200                 88  :FTL:-AU-MERCH-AUTO-RENTAL    VALUE 03.              
015250                 88  :FTL:-AU-MERCH-RESTAURANT     VALUE 04.              
015300                 88  :FTL:-AU-MERCH-MAIL-PHONE     VALUE 05.              
015350                 88  :FTL:-AU-MERCH-RISKY-PURCHASE VALUE 06.              
015400                 88  :FTL:-AU-MERCH-OTHER-PURCHASE VALUE 07.              
015450                 88  :FTL:-AU-MERCH-OTHER-CASH     VALUE 08.              
015500                 88  :FTL:-AU-MERCH-ATM-CASH       VALUE 09.              
015550                 88  :FTL:-AU-MERCH-QUASI-CASH     VALUE 10.              
015600                 88  :FTL:-AU-MERCH-MEDICAL        VALUE 11.              
015650             10  :FTL:-AU-MERCH-BYTES        REDEFINES                    
015700                 :FTL:-AU-MERCH-GROUP.                                    
015750                15  :FTL:-AU-MERCH-BYTE01    PIC  X(001).                 
015800                15  :FTL:-AU-MERCH-BYTE02    PIC  X(001).                 
015850         05  :FTL:-AU-RISK-LEVEL             PIC  X(001).                 
015900             88  :FTL:-AU-RISK-LEVEL-A       VALUE 'A'.                   
015950             88  :FTL:-AU-RISK-LEVEL-B       VALUE 'B'.                   
016000             88  :FTL:-AU-RISK-LEVEL-C       VALUE 'C'.                   
016050             88  :FTL:-AU-RISK-LEVEL-D       VALUE 'D'.                   
016100         05  :FTL:-AU-RISK-FLAG              PIC  X(001).                 
016150             88  :FTL:-AU-CARD-REC-NOT-ACCESSED VALUE '0'.                
016200             88  :FTL:-AU-CARD-REC-ACCESSED    VALUE '1'.                 
016250         05  :FTL:-AU-ISS-PCR                PIC  X(004).                 
016300         05  :FTL:-AU-ISS-REGION             PIC  X(001).                 
016350         05  :FTL:-AU-ISS-COUNTRY            PIC  9(003).                 
016400*  OR F184 MISC FIELDS ISSUER (DEST) COUNTRY CODE                         
016450         05  :FTL:-AU-ISS-PCR-COUNTRY        PIC  9(003).                 
016500         05  :FTL:-AU-ISS-SI-PEN-FLAG        PIC  X(001).                 
016550             88  :FTL:-AU-SI-MODE-PENETRATED VALUE '1'.                   
016600         05  :FTL:-AU-LCS-RAND-FLAG          PIC  X(001).                 
016650         05  :FTL:-AU-LCS-CAT                PIC  X(001).                 
016700         05  :FTL:-AU-LCS-CRB-REGION         PIC  X(001).                 
016750         05  :FTL:-AU-LCS-FLAGS              PIC  X(007).                 
016800         05  :FTL:-AU-LCS-FLAGS-REDEF        REDEFINES                    
016850             :FTL:-AU-LCS-FLAGS.                                          
016900             10  :FTL:-AU-LCS-FLAGS-ON.                                   
016950               15  :FTL:-AU-LCS-ACT-TEST-FLAG PIC X(001).                 
017000               15  :FTL:-AU-LCS-ACT-DOL-EX-FLAG PIC X(001).               
017050               15  :FTL:-AU-LCS-ACT-CNT-EX-FLAG PIC X(001).               
017100               15  :FTL:-AU-LCS-ADVICE-FLAG PIC    X(001).                
017150               15  :FTL:-AU-LCS-FORWARD-FLAG PIC X(001).                  
017200               15  :FTL:-AU-LCS-FORCE-CAT-C-FLAG PIC X(001).              
017250               15  :FTL:-AU-DIAL-SOURCE-FLAG PIC X(001).                  
017300                   88  :FTL:-AU-DIAL-SERIES1 VALUE '1'.                   
017350         05  :FTL:-AU-RESP-DATE-YYDDD        PIC S9(005) COMP-3.          
017400         05  :FTL:-AU-RESP-TIME-HHMMSS       PIC S9(006) COMP-3.          
017450*  OLD FMT DESTINATION /*BM*/ (ISO:A0IDSI)  OR                            
017500* MESSAGE HEADER SOURCE ID                                                
017550         05  :FTL:-AU-ISS-STATION            PIC S9(006) COMP-3.          
017600*  OR REPLACEMENT AMT, US$ EQUIV. (W/ FEES)                               
017650         05  :FTL:-AU-AUTH-AMT               PIC S9(10)V99 COMP-3.        
017700         05  :FTL:-AU-ISO-RESP-CODE          PIC  X(002).                 
017750         05  :FTL:-AU-RESP-CODE              PIC  X(002).                 
017800         05  :FTL:-AU-AUTH-CODE              PIC  X(006).                 
017850         05  :FTL:-AU-PIN-TRANSLATION-FLAG PIC    X(001).                 
017900             88  :FTL:-AU-PIN-NOT-TRANSLATED VALUE '0'.                   
017950             88  :FTL:-AU-PIN-TRANSLATED     VALUE '1'.                   
018000         05  :FTL:-AU-PIN-VERIFICATION-FLAG PIC X(001).                   
018050             88  :FTL:-AU-PIN-NOT-VERIFIED VALUE '0'.                     
018100             88  :FTL:-AU-PIN-VERIFIED       VALUE '1' '2'.               
018150             88  :FTL:-AU-PIN-VERIFY-CORRECT VALUE '1'.                   
018200             88  :FTL:-AU-PIN-VERIFY-INCORRECT VALUE '2'.                 
018250         05  :FTL:-AU-PIN-ERROR-FLAG         PIC  X(001).                 
018300             88  :FTL:-AU-PIN-ERRORS-EXCEEDED VALUE '1'.                  
018350*                PIN INVALID ACTIVITY COUNT  EXCEEDED            *        
018400         05  :FTL:-AU-GENERAL-USE-DATA       PIC X(002).                  
018450         05  :FTL:-AU-AVS-REDEF              REDEFINES                    
018500             :FTL:-AU-GENERAL-USE-DATA.                                   
018550             10  :FTL:-AU-ATM-NETWORK-ID     PIC  X(001).                 
018600*            POSSIBLE NUMERIC VALUES AND THEIR EXPLANATIONS:              
018650*            HEX'00 ' AND HEX'02' IS USED FOR VISA NETWORK ID.            
018700                 88  :FTL:-AU-ATM-ID-VISA-UNKNOWN VALUE X'00'.            
018800                 88  :FTL:-AU-ATM-ID-VISA          VALUE  X'02'.          
018900                 88  :FTL:-AU-ATM-ID-PLUS          VALUE  X'04'.          
019000                 88  :FTL:-AU-ATM-ID-ENTREE        VALUE  X'05'.          
019100             10  :FTL:-AU-AVS-DATA-PRESENT PIC    X(001).                 
019150                 88 :FTL:-AU-AVS-ADR-PRSNT         VALUES 'P' 'O'.        
019200                 88 :FTL:-AU-AVS-ZIP-ONLY          VALUE 'O'.             
019250                 88 :FTL:-AU-AVS-ADR-NO-ADR        VALUE 'N'.             
019300         05  :FTL:-AU-REJECT-REDEF           REDEFINES                    
019350             :FTL:-AU-GENERAL-USE-DATA.                                   
019400             10  :FTL:-AU-REJECT-REASON      PIC 9(004) COMP.             
019450                 88  :FTL:-AU-REJ-DUP-TRAN VALUE 2 14.                    
019500*            POSSIBLE NUMERIC VALUES AND THEIR EXPLANATIONS:              
019550*               1 = UNSOLICITED AUTHORIZATION RESPONSE                    
019600*               2 = AUTHORIZATION REQUEST IN PROGRESS                     
019650*               3 = LATE AUTHORIZATION RESPONSE                           
019700*              12 = LATE RESPONSE BUT INQUIRY NOT IN TABLE                
019750*              13 = LATE REVERSAL RESPONSE                                
019800*              14 = REVERSAL IN PROGRESS                                  
019850*              15 = LATE REVERSAL RESPONSE                                
019900*              16 = UNSOLICITED REVERSAL RESPONSE                         
019950*              30 = UNSLCTD RESP FROM OTHER VIC/UNSLCTD THERE TOO         
020000         05  :FTL:-AU-ACQ-INST-ID            PIC  X(011).                 
020050*  F121  ISSUING INSTITUTION ID   OR                                      
020100*  F184 MISC FIELDS BILLING BIN ID                                        
020150         05  :FTL:-AU-ISS-INST-ID            PIC  X(011).                 
020200         05  :FTL:-AU-TERMINAL-TYPE          PIC  X(001).                 
020250*  F184 MISC FIELDS ACQUIRER (SRCE) COUNTRY CODE                          
020300         05  :FTL:-AU-ACQ-COUNTRY            PIC  9(003).                 
020350         05  :FTL:-AU-BILL-INDIC             PIC  X(001).                 
020400             88  :FTL:-AU-BILL-IND-POS-LEASED     VALUE 'A'.              
020450             88  :FTL:-AU-BILL-IND-WATTS          VALUE 'B'.              
020500             88  :FTL:-AU-BILL-IND-LOCAL          VALUE 'C'.              
020550             88  :FTL:-AU-BILL-IND-UNUSED         VALUE 'D'.              
020600             88  :FTL:-AU-BILL-IND-ASYNCH         VALUE 'D'.              
020650             88  :FTL:-AU-BILL-IND-DRN-LATA       VALUE 'E'.              
020700             88  :FTL:-AU-BILL-IND-X25-GATEWAY    VALUE 'E'.              
020750             88  :FTL:-AU-BILL-IND-NON-US-DIAL    VALUE 'F'.              
020800             88  :FTL:-AU-BILL-IND-JAPAN-ACQ-SERV VALUE 'J'.              
020850             88  :FTL:-AU-BILL-IND-LOCAL-CANADA   VALUE 'I'.              
020900             88  :FTL:-AU-BILL-IND-FGB-SYNCH      VALUE 'N'.              
020950             88  :FTL:-AU-BILL-IND-SYNCH800       VALUE 'O'.              
021000             88  :FTL:-AU-BILL-IND-DATAOVR-VOICE  VALUE 'P'.              
021050             88  :FTL:-AU-BILL-IND-DIAL-ISDN      VALUE 'R'.              
021100             88  :FTL:-AU-BILL-IND-DPAC-CANADA    VALUE 'S'.              
021150             88  :FTL:-AU-BILL-IND-DIAL-SATLITE   VALUE 'T'.              
021200*SPECIAL CITIBANK STATIONS ARE FROM 1 TO 15 (X'01'..X'0F')                
021250*X'00' INDICATES NO SPECIAL ROUTING USED.                                 
021300         05  :FTL:-AU-SPECIAL-ROUTING-IND    PIC  X(001).                 
021350             88 :FTL:-AU-NO-SPECIAL-ROUTING-X00 VALUE X'00'.              
021450*  THIS FIELD IS DEFINED THE SAME WAY AS FTL-AU-PROC-CODE FROM            
021500*  ACQUIRER.  THIS NEW FIELD IS THE PROCESSING CODE FROM ISSUER.          
021550         05  :FTL:-AU-ISS-PROC-CODE.                                      
021600             10  :FTL:-AU-ISS-PROC-TYPE      PIC  9(002).                 
021650             10  :FTL:-AU-ISS-PROC-ACCT      PIC  9(002).                 
021700         05  :FTL:-AU-ADV-CARD-PRODUCT       PIC  X(001).                 
021750             88 :FTL:-AU-ADV-VISA-BUSINESS   VALUE 'B'.                   
021800             88 :FTL:-AU--ADV-VISA-CLASSIC   VALUE 'C'.                   
021850             88 :FTL:-AU--ADV-VISA-ELECTRON  VALUE 'E'.                   
021900             88 :FTL:-AU-ADV-VISA-PREMR-GOLD VALUE 'P'.                   
021950             88 :FTL:-AU-ADV-VISA-CORPORATE  VALUE 'R'.                   
022000             88 :FTL:-AU-ADV-VISA-PROCUREMNT VALUE 'S'.                   
022050             88 :FTL:-AU-ADV-NON-US-ISSUER   VALUE 'X'.                   
022100             88 :FTL:-AU-ADV-ISS-DEBIT-ATCHD VALUE 'Y'.                   
022150             88 :FTL:-AU-ADV-NON-VISA-CARD   VALUE 'X'.                   
022200         05  :FTL:-AU-RETRIEVAL-REF-NO       PIC S9(012) COMP-3.          
022250         05  :FTL:-AU-EAS-AUTO-CDB           PIC  X(001).                 
022300         05  :FTL:-AU-EAR-CERT               PIC  X(001).                 
022350         05  :FTL:-AU-PACM                   PIC  X(001).                 
022400         05  :FTL:-AU-EAS1-RESV              PIC  X(001).                 
022450         05  :FTL:-AU-ACDB-BILLING           PIC  X(001).                 
022500         05  :FTL:-AU-EAR-BILLING            PIC  X(001).                 
022550         05  :FTL:-AU-PACM-BILLING           PIC  X(001).                 
022600         05  :FTL:-AU-EAS2-RESV              PIC  X(001).                 
022650         05  :FTL:-AU-EAS-MSG-ID             PIC  X(001).                 
022700             88  :FTL:-AU-EAS-NO-ID          VALUE '0'.                   
022750             88  :FTL:-AU-EAS-AUTOCDB-ID1    VALUE '1'.                   
022800             88  :FTL:-AU-EAS-EAR-ID2        VALUE '2'.                   
022850             88  :FTL:-AU-EAS-EAR-VIOLATOR VALUE '3'.                     
022900         05  :FTL:-AU-STIP-ELIGIBLE          PIC  X(001).                 
022950         05  :FTL:-AU-PACM-DIVERTED          PIC  X(001).                 
023000         05  :FTL:-AU-PACM-DIVERT-LEVEL      PIC  X(002).                 
023050         05  :FTL:-AU-PACM-DIVERT-REASON     PIC  X(001).                 
023100         05  :FTL:-AU-PARTCIP-FLAG-HEX       PIC  X(001).                 
023150         05  :FTL:-AU-DOWNGRD-RSN-CODE       PIC  X(002).                 
023200         05  :FTL:-AU-PYMT-SVC-IND           PIC  X(001).                 
023250             88  :FTL:-AU-PYMT-SVC-ID-A      VALUE 'A'.                   
023300             88  :FTL:-AU-PYMT-SVC-ID-N      VALUE 'N'.                   
023350             88  :FTL:-AU-PYMT-SVC-ID-Y      VALUE 'Y'.                   
023400         05  :FTL:-AU-TRAN-ID-VALUE          PIC  S9(15) COMP-3.          
023450         05  :FTL:-AU-VALDTN-CODE-VALUE      PIC  X(004).                 
023500         05  :FTL:-AU-TRANS-CURR-CODE        PIC  X(003).                 
023550         05  :FTL:-AU-SYS-TRACE-AUDIT-NBR    PIC  X(006).                 
023600         05  :FTL:-AU-F44-FIFTH-BYTE         PIC  X(001).                 
023650             88  :FTL:-AU-F44-FIFTH-F1       VALUE '1'.                   
023700             88  :FTL:-AU-F44-FIFTH-F2       VALUE '2'.                   
023750             88  :FTL:-AU-F44-FIFTH-F3       VALUE '3'.                   
023800         05  :FTL:-AU-A1CVVFA                PIC  X(001).                 
023850         05  :FTL:-AU-A1CVVFB                PIC  X(001).                 
023900         05  :FTL:-AU-AMT-CRDHLDR-BILL       PIC S9(011) COMP-3.          
023950         05  :FTL:-AU-CONV-RATE-DEC-PT       PIC  X(001).                 
024000         05  :FTL:-AU-CONV-RATE              PIC S9(007) COMP-3.          
024050         05  :FTL:-AU-CRDHLDR-BILL-CURR-CODE PIC S9(003) COMP-3.          
024100         05  :FTL:-AU-A0IAMT-TRAN-FLD4       PIC S9(012) COMP-3.          
024150         05  :FTL:-AU-NETWORK-ID             PIC  X(004).                 
024200         05  :FTL:-AU-TRACK-LEN              PIC  9(002).                 
024250         05  :FTL:-AU-TRACK-DATA.                                         
024300            10 :FTL:-AU-TRACK1-DATA          PIC  X(076).                 
024350            10 :FTL:-AU-TRACK2-ONLY                                       
024400                   REDEFINES :FTL:-AU-TRACK1-DATA.                        
024450                15 :FTL:-AU-TRACK2-DATA      PIC  X(038).                 
024500                15 FILLER                    PIC  X(038).                 
024550         05 :FTL:-AU-ODE-DATA.                                            
024600            10 :FTL:-AU-ODE-MSG-TYPE         PIC  X(04).                  
024650            10 :FTL:-AU-ODE-TRACE-NBR        PIC  X(06).                  
024700            10 :FTL:-AU-ODE-TRANSMIT-DATA.                                
024750               15 :FTL:-AU-ODE-TRANSMIT-DATE PIC X(04).                   
024800               15 :FTL:-AU-ODE-TRANSMIT-TIME PIC  X(06).                  
024850            10 :FTL:-AU-ODE-ACQ-ID           PIC  X(11).                  
024900            10 :FTL:-AU-ODE-FWD-INST-ID      PIC  X(11).                  
024950         05 :FTL:-AU-LCS-ORG-CAT             PIC  X(001).                 
025000         05 :FTL:-AU-MERCH-RGN-CODE          PIC  X(001).                 
025050         05 :FTL:-AU-ACQ-RGN-CODE            PIC  X(001).                 
025100         05 :FTL:-AU-AVS-ISS-RESULT          PIC  X(001).                 
025150*  F39 RESPONSE CODE OR IF REC TYP 7 F184                                 
025200         05 :FTL:-AU-ISS-RESP-CODE           PIC  X(002).                 
025250         05 :FTL:-AU-CASHBACK-AMT            PIC S9(12) COMP-3.           
025300*  PS94 FIELDS  -------------------------------------------------*        
025350         05 :FTL:-AU-MARKET-DATA             PIC  X(001).                 
025400            88 :FTL:-AU-MARKET-AUTO-RENTAL              VALUE 'A'.        
025450            88 :FTL:-AU-MARKET-HOTEL                    VALUE 'H'.        
025500            88 :FTL:-AU-MARKET-FAILED-EDITS             VALUE 'N'.        
025550         05 :FTL:-AU-DURATION                PIC  9(002).                 
025600         05 :FTL:-AU-PRST-PROP-IND           PIC  X(001).                 
025650            88 :FTL:-AU-PRST-PROP-MULTI-LIMIT           VALUE 'D'.        
025700            88 :FTL:-AU-PRST-PROP-LIMIT-1000            VALUE 'B'.        
025750            88 :FTL:-AU-PRST-PROP-LIMIT-1500            VALUE 'S'.        
025800         05 :FTL:-AU-STAT-MC                 PIC  X(001).                 
025850         05 :FTL:-AU-BIT-FIELDS              PIC  X(001).                 
025900*           X'80'  ACQUIRER PARTICIPATES IN PS/2000                       
025950*           X'40'  ACQUIRER CERTIFYING IN PS/2000                         
026000*           X'C0'  INDICATOR TO CHK ACQUIRER OPTIONS: PS/2000             
026050*           X'20'  ACQUIRER PARTICIPATES IN CPS/ATM                       
026100*           X'10'  ACQUIRER CERTIFYING IN CPS/ATM                         
026150*           X'30'  INDICATOR TO CHK ACQUIRER OPTIONS: CPS/ATM             
026200*           X'08'  ISSUER PARTICIPATES IN PS/2000                         
026250*           X'04'  ISSUER CERTIFYING IN PS/2000                           
026300*           X'0C'  INDICATOR TO CHK ISSUER OPTIONS: PS/2000               
026350*           X'02'  ISSUER PARTICIPATES IN CPS/ATM                         
026400*           X'01'  ISSUER CERTIFYING IN CPS/ATM                           
026450*           X'03'  INDICATOR TO CHK ISSUER OPTIONS: CPS/ATM               
026500         05 :FTL:-AU-REPL-AMT           PIC S9(12)    COMP-3.             
026550         05 :FTL:-AU-REPL-AMT-US        PIC S9(10)V99 COMP-3.             
026600         05 :FTL:-AU-SUBMIT-ACI         PIC  X(001).                      
026650            88 :FTL:-AU-MEETS-CPS-RETAIL           VALUE 'A'.             
026700            88 :FTL:-AU-CUST-ACTIV-TERM-QUALFD     VALUE 'C'.             
026750            88 :FTL:-AU-MERCH-NAME-LOC-QUALFD      VALUE 'E'.             
026800            88 :FTL:-AU-INCREMENTAL-QUALFD         VALUE 'I'.             
026850            88 :FTL:-AU-DIRECT-MARKETING           VALUE 'M'.             
026900            88 :FTL:-AU-DISQUALIFIED-FOR-PS2K      VALUE 'N'.             
026950            88 :FTL:-AU-PREFERRED-CUST-QUALFD      VALUE 'P'.             
027000            88 :FTL:-AU-AVS-QUALFD                 VALUE 'V'.             
027050            88 :FTL:-AU-REQUESTING-CPS-RETAIL      VALUE 'Y'.             
027100         05 :FTL:-AU-MASTER-CARD-ID.                                      
027150            10 :FTL:-AU-MCID-BANKNET-DATE    PIC  X(004).                 
027200            10 :FTL:-AU-MCID-BANKNET-REF-NO PIC X(009).                   
027250            10 FILLER                        PIC  X(002).                 
027300         05 :FTL:-AU-DATA-AUG-IND            PIC  X(002).                 
027350         05 :FTL:-AU-DEBIT-CREDIT-FLAG       PIC  X(001).                 
027400            88  :FTL:-AU-CREDIT                   VALUE 'C'.              
027450            88  :FTL:-AU-DEBIT                    VALUE 'D'.              
027500         05 :FTL:-AU-CARDHOLDER-NAME-LOC.                                 
027550            10  :FTL:-AU-CARDHOLDER-NAME     PIC X(025).                  
027600            10  :FTL:-AU-CARDHOLDER-CITY     PIC X(013).                  
027650            10  :FTL:-AU-CARDHOLDER-CNTRY    PIC X(002).                  
027700         05 :FTL:-AU-CARDACCPTR-NAME-LOC REDEFINES                        
027750            :FTL:-AU-CARDHOLDER-NAME-LOC.                                 
027800            10  :FTL:-AU-CARDACCPTR-NAME     PIC X(025).                  
027850            10  :FTL:-AU-CARDACCPTR-CITY     PIC X(013).                  
027900            10  :FTL:-AU-CARDACCPTR-CNTRY    PIC X(002).                  
027950         05 :FTL:-AU-ACQ-STATE-CODE          PIC X(002).                  
028000         05 :FTL:-AU-ACQ-COUNTY-CODE         PIC X(003).                  
028050         05 :FTL:-AU-COBRAND-BIN-ID          PIC X(011).                  
028100         05 :FTL:-AU-COBRAND-FLAGS.                                       
028150            10 :FTL:-AU-COBRAND-FLAG-1       PIC X(001).                  
028200            10 :FTL:-AU-COBRAND-FLAG-2       PIC X(001).                  
028250         05 :FTL:-AU-TRANSMIT-DATE           PIC 9(004).                  
028300         05 :FTL:-AU-TRANSMIT-TIME           PIC 9(006).                  
028350         05 :FTL:-AU-RETURN-MSG-FLAG-3       PIC X(001).                  
028400* SINCE MULTIPLE FOLLOWING FIELDS MAY BE PRESENT AT THE SAME              
028450* TIME, THE 1 BYTE FIELDS-PRESENT-INDICATOR BELOW IS ARRIVED              
028500* AT BY OR'ING FOLLOWING APPROPRIATE HEX VALUES:                          
028550* WS-AU-POS-ENT-MODE      FIELD-PRESENT INDICATED BY FLAG X'80'           
028600* WS-AU-POS-ENT-CAP-CODE  FIELD-PRESENT INDICATED BY FLAG X'40'           
028650* WS-AU-POS-COND-CODE     FIELD-PRESENT INDICATED BY FLAG X'20'           
028700* WS-AU-ACQ-TERM-ID       FIELD-PRESENT INDICATED BY FLAG X'10'           
028750* WS-AU-CARD-ACPTR-ID     FIELD-PRESENT INDICATED BY FLAG X'10'           
028800* WS-AU-ACQ-TERM-ID-CODE  FIELD-PRESENT INDICATED BY FLAG X'08'           
028850* WS-AU-CARD-ACPTR-TERMID FIELD-PRESENT INDICATED BY FLAG X'08'           
028900* WS-AU-CARDHOLDER-NAME   FIELD-PRESENT INDICATED BY FLAG X'04'           
028950* WS-AU-CARD-ACPTR-NAME   FIELD-PRESENT INDICATED BY FLAG X'04'           
029000* WS-AU-ACQ-POSTAL-CODE   FIELD-PRESENT INDICATED BY FLAG X'02'           
029050* WS-AU-TRAN-DATE-TIME    FIELD-PRESENT INDICATED BY FLAG X'01'           
029100         05 :FTL:-AU-FLDS-PRESENT-IND-BITS PIC X(001).                    
029150         05 :FTL:-AU-READYPOST-FLAGS         PIC X(001).                  
029200         05 :FTL:-AU-ISSUER-STATION-VAP      PIC X(006).                  
029250         05 :FTL:-AU-ISSUER-STATION-PCR      PIC X(004).                  
029300         05 :FTL:-AU-ACQUIRER-STATION-VAP    PIC X(006).                  
029350         05 :FTL:-AU-ACQUIRER-STATION-PCR    PIC X(004).                  
EPAY1C*        * --------------------------------------------------             
EPAY1C*        * - THIS LOGICALLY BELONGS IN THE EPAY EXTENSION REC             
EPAY1C*        * - BUT IT IS USED BY CREDIT, TOO, AND THEY DON'T                
EPAY1C*        * - HAVE AN EXTENSION REC.                                       
EPAY1C*        * --------------------------------------------------             
EPAY1C         05 :FTL:-AU-A1IEPI1-EPAY-FLAGS.                                  
EPAY1C            10  :FTL:-AU-A1XPVID-RCVD-IN-PMT-REQ PIC X.                   
EPAY2             10  :FTL:-AU-A0VEES1-EPAY-VEES-FLAGS PIC X.                   
EPAY1C*           * ---------------------------------------                     
EPAY1C*           * - FUTURE EPAY FLAGS                                         
EPAY1C*           * ---------------------------------------                     
EPAY1C            10  FILLER                         PIC X.                     
CRIS           05 :FTL:-AU-CRIS.                                                
CRIS              10  :FTL:-AU-CRIS-IDENTIFIER       PIC X.                     
CRIS              10  :FTL:-AU-CRIS-ALERT-TYPE       PIC X(3).                  
CRIS              10  :FTL:-AU-CRIS-ALERT-SCORE      PIC X(5).                  
CRIS           05 :FTL:-AU-NETWK-MGMT-ID-CODE        PIC X(3).                  
ONEATM         05 :FTL:-AU-TRANSAMT-GROUP.                                      
ONEATM            10  :FTL:-AU-TRANSAMT-PRESENT-FLAG PIC X(01).                 
ONEATM                88 :FTL:-AU-TRANSAMT-IS-PRESENT   VALUE '1'.              
ONEATM            10  :FTL:-AU-TRANSAMT-ID           PIC X(01).                 
ONEATM                88 :FTL:-AU-TRANSAMT-IS-DEBIT     VALUE 'D'.              
ONEATM                88 :FTL:-AU-TRANSAMT-IS-CREDIT    VALUE 'C'.              
ONEATM            10  :FTL:-AU-TRANSAMT              PIC 9(08).                 
SVCDEV         05 :FTL:-AU-SERVICE-DEV               PIC 9(01).                 
RT0712         05 :FTL:-AU-CARD-PROD-CODE            PIC X(01).                 
RT0712         05 :FTL:-AU-CARD-PR-EXPANSION         PIC X(01).                 
RT0712         05 :FTL:-AU-CARD-USAGE-CODE           PIC X(01).                 
RT0712            88 :FTL:-AU-USAGE-CODE-DEBIT          VALUE 'D'.              
RT0712            88 :FTL:-AU-USAGE-CODE-CREDIT         VALUE 'C'.              
RT0712            88 :FTL:-AU-USAGE-CODE-VCCII-ELECT    VALUE 'F'.              
RT2433         05 FILLER                             PIC X(200).                
*******-----------------------------------------------------------              
002800*        *-------------------------------------------------------*        
002850*        *   DEBIT CONTINUATION FIELDS                           *        
002900*        *-------------------------------------------------------*        
RT2433     03  :FTL:-ST-GROUP.                                                  
R81768         05  :FTL:-ST-RECORD.                                             
R81768             10 :FTL:-ST-PLUS-MIS-PMC        PIC  9(06).                  
R81768             10 :FTL:-ST-ACQ-BUSN-ID         PIC  9(08).                  
R81768             10 :FTL:-ST-NAF-PROC-CODE.                                   
R81768                15 :FTL:-ST-NAF-PROC-CODE-TRAN-TYPE                       
R81768                                             PIC  9(02).                  
R81768                15 :FTL:-ST-NAF-PROC-CODE-FROM-ACCT                       
R81768                                             PIC  9(02).                  
R81768                15 :FTL:-ST-NAF-PROC-CODE-TO-ACCT                         
R81768                                             PIC  9(02).                  
R81768             10 :FTL:-ST-TRAN-OTHER-AMT                                   
R81768                               PIC S9(07)V99 COMP-3.                      
R81768             10 :FTL:-ST-MSG-RSN-CODE        PIC  9(04).                  
R81768             10 :FTL:-ST-ADDL-RESP-DATA      PIC  X(25).                  
R81768             10 :FTL:-ST-REIMB-ATTR          PIC  X(01).                  
R81768             10 :FTL:-ST-CHGBK-CODES.                                     
R81768               15 :FTL:-ST-CHGBK-FLR-LIMIT-IND PIC X(01).                 
R81768               15 :FTL:-ST-CHGBK-CWB-CRB-IND PIC  X(01).                  
R81768             10 :FTL:-ST-CHGBK-LCS-IND       PIC  X(01).                  
R81768             10 :FTL:-ST-CHGBK-MAIL-PHONE-IND PIC X(01).                  
R81768             10 :FTL:-ST-CHGBK-SPCL-IND      PIC  X(01).                  
R81768             10 :FTL:-ST-CHGBK-SPCL-COND-RIS PIC  X(01).                  
R81768             10 :FTL:-ST-CHGBK-SPCL-COND-MERCH PIC X(01).                 
R81768             10 :FTL:-ST-BASE1-BILL-FLAG     PIC  X(01).                  
R81768             10 :FTL:-ST-LOCAL-TRAN-DATE     PIC  9(04).                  
R81768             10 :FTL:-ST-LOCAL-TRAN-TIME     PIC  9(06).                  
R81768             10 :FTL:-ST-DIAL-DEBIT-FLAG     PIC  X(01).                  
R81768             10 :FTL:-ST-PURCHASE-INDICATOR  PIC  X(17).                  
R81768             10 :FTL:-ST-MERCH-IDENT-FIELD.                               
R81768                15 :FTL:-ST-FINANCIAL-INST-ID PIC X(04).                  
R81768                15 :FTL:-ST-MERCH-ABA-NBR    PIC  X(09).                  
R81768             10 :FTL:-ST-GIV-FLAG            PIC  X(01).                  
R81768             10 :FTL:-ST-GIV-PREV-FLAG       PIC  X(01).                  
R81768             10 :FTL:-ST-ACQ-AMT             PIC S9(13) COMP-3.           
R81768             10 :FTL:-ST-ACQ-CURR-CODE       PIC S9(03) COMP-3.           
R81768             10 :FTL:-ST-ACQ-CONV-RATE-DEC-PT PIC X(01).                  
R81768             10 :FTL:-ST-ACQ-CONV-RATE       PIC S9(07) COMP-3.           
R81768             10 :FTL:-ST-ISS-AMT             PIC S9(13) COMP-3.           
R81768             10 :FTL:-ST-ISS-CURR-CODE       PIC S9(03) COMP-3.           
R81768             10 :FTL:-ST-ISS-CONV-RATE-DEC-PT PIC X(01).                  
R81768             10 :FTL:-ST-ISS-CONV-RATE       PIC S9(07) COMP-3.           
R81768             10 :FTL:-ST-ACQ-INST-ID         PIC  X(11).                  
R81768             10 :FTL:-ST-ISS-INST-ID         PIC  X(11).                  
R81768             10 :FTL:-ST-RAW-DATA.                                        
R81768                15 :FTL:-ST-RAW-REQ-MSG-TYPE      PIC  9(04).             
R81768                15 :FTL:-ST-RAW-PROC-CODE-FIRST2  PIC  9(02).             
R81768                15 :FTL:-ST-RAW-POS-COND-CODE     PIC  9(02).             
R81768                15 :FTL:-ST-RAW-ODE-MSG-TYPE      PIC  X(04).             
R81768                15 :FTL:-ST-RAW-POS-ENTRY-MODE.                           
R81768                 20 :FTL:-ST-RAW-ACTUAL-PAN-ENTRY PIC 9(02).              
R81768                 20 :FTL:-ST-RAW-TERM-PIN-ABILITY PIC 9(01).              
R81768                15 :FTL:-ST-RAW-ACQ-ID            PIC  9(11).             
R81768             10 :FTL:-ST-CRDHLDR-BILL-AMT    PIC 9(11)V99 COMP-3.         
R81768             10 :FTL:-ST-CRDHLDR-BILL-CURR-CODE   PIC 9(03).              
R81768             10 :FTL:-ST-NTWK-ID                  PIC  9(04).             
R81768             10 :FTL:-ST-STIP-RSN-CODE            PIC  9(04).             
R81768             10 :FTL:-ST-ODE-DATA.                                        
R81768                15 :FTL:-ST-ODE-MSG-TYPE          PIC  X(04).             
R81768                15 :FTL:-ST-ODE-TRACE-NBR         PIC  9(06).             
R81768                15 :FTL:-ST-ODE-TRANSMIT-DATA.                            
R81768                 20 :FTL:-ST-ODE-TRANSMIT-DATE    PIC 9(04).              
R81768                 20 :FTL:-ST-ODE-TRANSMIT-TIME    PIC 9(06).              
R81768                15 :FTL:-ST-ODE-ACQ-ID       PIC  X(11).                  
R81768                15 :FTL:-ST-ODE-FWD-INST-ID  PIC  X(11).                  
R81768                15 :FTL:-ST-GEO-CNTY-CODE    PIC  9(03).                  
R81768                15 :FTL:-ST-GEO-STATE-CODE   PIC  9(02).                  
R81768                15 :FTL:-ST-CRDHLDR-CASH-BACK-AMT                         
R81768                                   PIC S9(11)V99 COMP-3.                  
R81768                15 :FTL:-ST-IL-PLAYER-FLAG   PIC  X(01).                  
R81768                15 :FTL:-ST-VIP-STLMT-FLAG   PIC  X(01).                  
NNSS               10 :FTL:-ST-NNSS.                                            
NNSS                  15 :FTL:-ST-NNSS-SWITCHING-FLDS.                          
NNSS                   20 :FTL:-ST-NNSS-SETTLEMENT-GRP-ID PIC X(04).            
NNSS                   20 :FTL:-ST-NNSS-ZONE         PIC X(01).                 
NNSS                   20 :FTL:-ST-NNSS-FLAGS        PIC X(01).                 
NNSS                   20 :FTL:-ST-NNSS-NATL-BI-INDICATOR PIC X(01).            
NNSS                   20 :FTL:-ST-NNSS-ORIG-REQST-FLAGS PIC X(01).             
NNSS                  15 :FTL:-ST-NNSS-STLMN-SERVICE.                           
NNSS                   20 :FTL:-ST-NNSS-CNTRY-CODE   PIC X(03).                 
NNSS                   20 :FTL:-ST-NNSS-MEMBER-SUPPL-IRF PIC 9(13)              
NNSS                                                    COMP-3.                 
NMIC               10 :FTL:-ST-NTWK-MGMT-CODE        PIC 9(03).                 
IDEF63             10 :FTL:-ST-IDE-PREAUTH-LIMIT     PIC 9(04).                 
CCFS  *            *---------------------------------------------------*        
CCFS  *            * ISSUER CURRENCY CONVERSION DATA                   *        
CCFS  *            *---------------------------------------------------*        
CCFS               10 :FTL:-ST-ISS-CONV-FEE-INFO     PIC S9(015)                
CCFS                                                     COMP-3.                
CCFS               10 :FTL:-ST-ACQ-CONV-FEE-INFO     PIC S9(015)                
CCFS                                                     COMP-3.                
CCFS               10 :FTL:-ST-ACQ-AMT-CFEE-SIGN     PIC X(01).                 
CCFS               10 :FTL:-ST-ISS-AMT-CFEE-OPT-SIGN PIC X(01).                 
SUSEG              10 :FTL:-ST-SUSEG-SU1FLG1         PIC 9(01).                 
VCASH              10 :FTL:-ST-VCASH-LOAD-RESP-SIG   PIC X(019).                
VCASH              10 :FTL:-ST-VCASH-LOAD-IND        PIC X(001).                
BER282             10 :FTL:-ST-USIRF-F48-U10.                                   
BER282                15 :FTL:-ST-ADDTNL-DATA-IND    PIC X(001).                
BER282                15 :FTL:-ST-ADDTNL-DATA-ID     PIC X(002).                
BER282                15 :FTL:-ST-ADDTNL-DATA-TYPE   PIC X(001).                
BER282                   88 :FTL:-ST-USIRF-NON-COMMERCIAL VALUE '0'.            
BER282                   88 :FTL:-ST-USIRF-COMMERCIAL-BUS VALUE 'B'.            
BER282                   88 :FTL:-ST-USIRF-CORP-TRAV-ENT  VALUE 'R'.            
BER282                   88 :FTL:-ST-USIRF-PURCH-PROCURE  VALUE 'S'.            
BER427             10 :FTL:-ST-ELEC-COMMERCE-IND     PIC X(002).                
BER427                   88 :FTL:-ST-NOT-APPLIABLE        VALUE '00'.           
BER427                   88 :FTL:-ST-SEC-WITH-CERTIFICATE VALUE '05'.           
BER427                   88 :FTL:-ST-SEC-NO-CERTIFICATE   VALUE '06'.           
BER427                   88 :FTL:-ST-CHANEL-ENCRYPT-TXN   VALUE '07'.           
BER427                   88 :FTL:-ST-NON-SECURE-SEC-TXN   VALUE '08'.           
BER574             10 :FTL:-ST-BNKRUPT-ALERT.                                   
BER574                15 :FTL:-ST-BNK-DATA-ID        PIC  X(001).               
BER574                   88 :FTL:-ST-BNKRUPT-IND          VALUE 'B'.            
BER574                15 :FTL:-ST-BNK-SUB-ID         PIC  X(002).               
BER574                15 :FTL:-ST-BNK-ACTIV-FROM     PIC  X(008).               
BER574                15 :FTL:-ST-BNK-ACTIV-TO       PIC  X(008).               
BER574                15 :FTL:-ST-BNK-ISS-ACCT-THR   PIC  X(004).               
BER574                15 :FTL:-ST-BNK-ISS-CONS-THR   PIC  X(004).               
BER574                15 :FTL:-ST-BNK-ACCT-SCORE     PIC  X(004).               
BER574                15 :FTL:-ST-BNK-CONS-SCORE     PIC  X(004).               
BER574                15 :FTL:-ST-BNK-ACCT-ALRT-SCO  PIC  X(008).               
BER574                15 :FTL:-ST-BNK-CONS-ALRT-SCO  PIC  X(008).               
BER574                15 :FTL:-ST-BNK-LAST-ALRT-DTE  PIC  X(008).               
BER574                15 :FTL:-ST-BNK-LAST-ALRT-SCO  PIC  X(004).               
BER574                15 :FTL:-ST-BNK-LAST-CON-DATE  PIC  X(008).               
BER574                15 :FTL:-ST-BNK-LAST-CON-SCOR  PIC  X(004).               
BER574                15 :FTL:-ST-BNK-CRIS-ALT-IND   PIC  X(001).               
BER574                15 :FTL:-ST-BNK-ACCT-ALT-TYP   PIC  X(003).               
BER574                15 :FTL:-ST-BNK-ACCT-RSN-COD1  PIC  X(003).               
BER574                15 :FTL:-ST-BNK-ACCT-RSN-COD2  PIC  X(003).               
BER574                15 :FTL:-ST-BNK-ACCT-RSN-COD3  PIC  X(003).               
BER574                15 :FTL:-ST-BNK-ACCT-RSN-COD4  PIC  X(003).               
BER574                15 :FTL:-ST-BNK-CONS-ALT-TYP   PIC  X(003).               
BER574                15 :FTL:-ST-BNK-CONS-RSN-COD1  PIC  X(003).               
BER574                15 :FTL:-ST-BNK-CONS-RSN-COD2  PIC  X(003).               
BER574                15 :FTL:-ST-BNK-CONS-RSN-COD3  PIC  X(003).               
BER574                15 :FTL:-ST-BNK-CONS-RSN-COD4  PIC  X(003).               
BER574                15 :FTL:-ST-BNK-RISK-AUDIT-ID.                            
BER574                   20 :FTL:-ST-BNKRUPT-SERV-ID    PIC  X(002).            
BER574                   20 :FTL:-ST-BNKRUPT-GROUP-NBR  PIC  X(004).            
BER574                   20 :FTL:-ST-BNKRUPT-CYCLE-DATE PIC  X(008).            
BER574                   20 :FTL:-ST-BNKRUPT-CYCLE-NBR  PIC  X(002).            
BER574                   20 :FTL:-ST-BNKRUPT-AUDIT-NBR  PIC  X(006).            
BER574                15 :FTL:-ST-BNK-SEQUENCE-NBR      PIC  X(010).            
VDAS               10 :FTL:-ST-VDAS-BIN-ADDRESS         PIC  9(006).            
BER159* ADDITIONAL DATA ISO F63.17                                              
BER159             10 :FTL:-ST-VOLTIER-ADDTL-DATA-IND.                          
BER159                15 FILLER                      PIC X(01).                 
BER159                15 :FTL:-ST-ADDTL-DATA-IND     PIC X(01).                 
BER159                   88 :FTL:-ST-NO-ADDTL-IND    VALUE '0'.                 
BER159                   88 :FTL:-ST-ADDTL-IND       VALUE '1'.                 
BER881* MERCHANT VOLUME ISO F63.18                                              
BER881             10 :FTL:-ST-VOLTIER-REQST-IND.                               
BER881                15 FILLER                      PIC X(01).                 
BER881                15 :FTL:-ST-VOLUME-REQST-IND   PIC X(01).                 
BER881                88 :FTL:-ST-VOLTIER-NON-PARTIC VALUE SPACE.               
BER881                88 :FTL:-ST-VOLTIER-TIER1-DISC VALUE '1'.                 
BER881                88 :FTL:-ST-VOLTIER-TIER2-DISC VALUE '2'.                 
B53924             10 :FTL:-ST-CLEARING-SEQ-NBR      PIC  9(02).                
RT1794             10 :FTL:-ST-CRDHLDR-ID-METHOD     PIC  X(001).               
RT1794             10 :FTL:-ST-VCCII-LOG-FLAG        PIC  X(001).               
RT1794             10 :FTL:-ST-REQSTD-REIMB-ATTRIB   PIC  X(001).               
RT1794             10 :FTL:-ST-QUALIFICATION-FLAG1   PIC  X(001).               
RT1794             10 :FTL:-ST-QUALIFICATION-FLAG2   PIC  X(001).               
RT1794             10 :FTL:-ST-QUALIFICATION-FLAG3   PIC  X(001).               
RT1794             10 :FTL:-ST-QUALIFICATION-FLAG4   PIC  X(001).               
RT1794             10 :FTL:-ST-REASON-TO-DECLINE-W57 PIC  X(001).               
RT1794             10 :FTL:-ST-F62X15-RPS-IND        PIC  X(001).               
RT0712             10 :FTL:-ST-ACCT-RNF-CUR-CD       PIC  X(002).               
RT1925             10 :FTL:-ST-IGOTS-TRANS-DESCR     PIC  X(002).               
RT2013             10 :FTL:-ST-REASON-TO-DECLINE-W91 PIC  X(001).               
RT1864             10 :FTL:-ST-F120-ORIGINAL-MSG-TYPE PIC 9(004).               
RT1628             10 :FTL:-ST-ATM-FORMAT-STATUS     PIC  X(001).               
RT2395             10 :FTL:-ST-F100-RCV-INST-ID      PIC  X(011).               
PMXTLR             10 :FTL:-ST-AMT-CRDHLDR-BILL      PIC  S9(13)                
PMXTLR                                               COMP-3.                    
RT2433             10 FILLER                         PIC  X(100).               
*******-----------------------------------------------------------              
CHIP1A*        *-------------------------------------------------------*        
CHIP1A*        *   CHIP  CONTINUATION FIELDS                           *        
CHIP1A*        *-------------------------------------------------------*        
RT2433     03  :FTL:-CH-GROUP.                                                  
CHIP1A         05  :FTL:-CH-FIELDS.                                             
CHIP1A             10 :FTL:-CH-CARD-SEQ-NUM          PIC S9(03)                 
CHIP1A                                                   COMP-3.                
CHIP1A             10 :FTL:-CH-TERM-CAP-PROFILE      PIC X(003).                
CHIP1B             10 :FTL:-CH-TERM-VER-RESULTS.                                
CHIP1B             20 :FTL:-CH-A2TVR01-TVRBYTE1      PIC X(001).                
CHIP1B                88 :FTL:-CH-A2XTSAN-NON-DATA-AUTH   VALUE X'80'.          
CHIP1B                88 :FTL:-CH-A2XTSAF-FAIL-DATA-AUTH  VALUE X'40'.          
CHIP1B                88 :FTL:-CH-A2XTCDM-CCPS-DATA-MISS  VALUE X'20'.          
CHIP1B                88 :FTL:-CH-A2XTPNX-PAN-ON-EXCPLST  VALUE X'10'.          
CHIP1B             20 :FTL:-CH-A2TVR02-TVRBYTE2     PIC X(001).                 
CHIP1B                88 :FTL:-CH-A2XTDIF-VER-MISMATCH    VALUE X'80'.          
CHIP1B                88 :FTL:-CH-A2XTEXP-APPL-EXPIRED    VALUE X'40'.          
CHIP1B                88 :FTL:-CH-A2XTAPA-APPL-NOT-EFFECT VALUE X'20'.          
CHIP1B                88 :FTL:-CH-A2XTSVC-SVC-NOT-ALLOWED VALUE X'10'.          
CHIP1B                88 :FTL:-CH-A2XTNCD-NEW-CARD        VALUE X'08'.          
CHIP1B             20 :FTL:-CH-A2TVR03-TVRBYTE3     PIC X(001).                 
CHIP1B                88 :FTL:-CH-A2XTCHF-CRDHLD-VER-FAIL VALUE X'80'.          
CHIP1B                88 :FTL:-CH-A2XTUCV-UNKNOWN-CVM     VALUE X'40'.          
CHIP1B                88 :FTL:-CH-A2XTPEX-PIN-TRIES-EXC   VALUE X'20'.          
CHIP1B                88 :FTL:-CH-A2XTPPD-NO-PIN-PAD      VALUE X'10'.          
CHIP1B                88 :FTL:-CH-A2XTPNE-PIN-REQUIRED    VALUE X'08'.          
CHIP1B                88 :FTL:-CH-A2XTOLP-PIN-ENTERED     VALUE X'04'.          
CHIP1B             20 :FTL:-CH-A2TVR04-TVRBYTE4     PIC X(001).                 
CHIP1B                88 :FTL:-CH-A2XTEFL-XACT-EXC-LIMIT  VALUE X'80'.          
CHIP1B                88 :FTL:-CH-A2XTELO-LOW-LIMIT-EXC   VALUE X'40'.          
CHIP1B                88 :FTL:-CH-A2XTEUL-UPPER-LIMIT-EXC VALUE X'20'.          
CHIP1B                88 :FTL:-CH-A2XTRAN-RNDOM-XACT      VALUE X'10'.          
CHIP1B                88 :FTL:-CH-A2XTMCF-MERCH-FORCED    VALUE X'08'.          
CHIP1B             20 :FTL:-CH-A2TVR05-TVRBYTE5     PIC X(001).                 
CHIP1B*          RESERVED FOR FUTURE USE                VALUE X'80'             
CHIP1B                88 :FTL:-CH-A2XTISF-ISS-AUTH-FAIL VALUE X'40'.            
CHIP1B                88 :FTL:-CH-A2XTSCF-SCRIP-PRE-CRY VALUE X'20'.            
CHIP1B                88 :FTL:-CH-A2XTSCA-SCRIP-AFT-CRY VALUE X'10'.            
CHIP1A             10 :FTL:-CH-UNPREDICTABLE-NUM     PIC X(004).                
CHIP1A             10 :FTL:-CH-TERM-SERIAL-NUM       PIC X(008).                
CHIP1A             10 :FTL:-CH-DERIV-KEY-INDEX       PIC X(001).                
CHIP1A             10 :FTL:-CH-CRYPT-VERSION         PIC X(001).                
CHIP1B            10 :FTL:-CH-CARD-VER-RESULTS.                                 
CHIP1B               20  :FTL:-CH-A2CVR01-CARD-LEN-BYTE PIC X(001).             
CHIP1B               20  :FTL:-CH-A2CVR02-CARD-BYTE-TWO PIC X(001).             
CHIP1B*                  BIT 1-2 = SECOND CRYPTOG                               
CHIP1B*                       00 = ACC RETURNED IN LAS                          
CHIP1B*                            GENERATED ACC                                
CHIP1B                 88 :FTL:-CH-A2XCNR2-2ND-CRY-NOTREQ VALUE X'80'.          
CHIP1B                 88 :FTL:-CH-A2XCTC2-MASK-FOR-TC    VALUE X'40'.          
CHIP1B*           B'11000000' RESERVED FOR FUTURE USE                           
CHIP1B*               BIT 3-4 = FIRST CRYTOGRA                                  
CHIP1B*                    00 = ACC RETURNED IN FIRST GENERA                    
CHIP1B                 88 :FTL:-CH-A2XCARQ-ARQC-1ST-AC    VALUE X'20'.          
CHIP1B                 88 :FTL:-CH-A2XCTC1-TC-IN-1ST-AC   VALUE X'10'.          
CHIP1B                 88 :FTL:-CH-A2XCAAR-AAR-1ST-AC     VALUE X'30'.          
CHIP1B*                    (NOT SUPPORTED IN RELEASE 1A)                        
CHIP1B                 88 :FTL:-CH-A2XCISF-ISS-AUTH-FAIL  VALUE X'08'.          
CHIP1B                 88 :FTL:-CH-A2XCOPP-OFFPIN-VER-OK  VALUE X'04'.          
CHIP1B                 88 :FTL:-CH-A2XCOPF-OFFPIN-VER-BAD VALUE X'02'.          
CHIP1B                 88 :FTL:-CH-A2XCNOL-ONLINE-NOAVAIL VALUE X'01'.          
CHIP1B               20  :FTL:-CH-A2CVR03-CARD-BYTE-3RD PIC X(001).             
CHIP1B                 88 :FTL:-CH-A2XCLOI-XACT-NOT-CMP   VALUE X'80'.          
CHIP1B                 88 :FTL:-CH-A2XCPNE-PIN-LIMIT-EXC  VALUE X'40'.          
CHIP1B                 88 :FTL:-CH-A2XCVCE-EXC-VELOC-CNT  VALUE X'20'.          
CHIP1B                 88 :FTL:-CH-A2XCNEW-NEW-CARD       VALUE X'10'.          
CHIP1B                 88 :FTL:-CH-A2XCLAF-ISS-AUTH-FAIL  VALUE X'08'.          
CHIP1B                 88 :FTL:-CH-A2XCLAN-ISS-AUTH-NOTDN VALUE X'04'.          
CHIP1B                 88 :FTL:-CH-A2XCPEB-CARD-BLKD-APPL VALUE X'02'.          
CHIP1B                 88 :FTL:-CH-A2XCSNL-LAST-STAT-FAIL VALUE X'01'.          
CHIP1B               20  :FTL:-CH-A2CVR04-CARD-BYTE-4TH PIC X(001).             
CHIP1B*       BIT 1-4, NUMBER OF SCRIPT COMMANDS RECEIVED ON LAST               
CHIP1B*                TRANSACTION                                              
CHIP1B                 88 :FTL:-CH-A2XCSCF-LAST-SCPT-FAIL VALUE X'08'.          
CHIP1B*                B'00000111'  -   RESERVED FOR FUTURE USE                 
CHIP1A             10 :FTL:-CH-ISS-DISCRETIONARY     PIC X(015).                
CHIP1A             10 :FTL:-CH-CRYPTOGRAM            PIC X(008).                
CHIP1A             10 :FTL:-CH-APPL-TRANS-COUNTER    PIC 9(04)                  
CHIP1A                                                   COMP.                  
CHIP1A             10 :FTL:-CH-APPL-INTERCHG-PROFILE PIC X(002).                
CHIP1A             10 :FTL:-CH-AUTH-RESP-CRYPTOGRAM  PIC X(008).                
CHIP1A             10 :FTL:-CH-ARPC-RESPONSE-CODE    PIC X(002).                
CHIP1A             10 :FTL:-CH-ISS-SCRIPT-RESULTS    PIC X(020).                
CHIP1A             10 :FTL:-CH-TRANS-TYPE            PIC X(002).                
CHIP1A             10 :FTL:-CH-TERM-COUNTRY-CODE     PIC X(003).                
CHIP1A             10 :FTL:-CH-TERM-TRAN-DATE        PIC S9(06)                 
CHIP1A                                                   COMP-3.                
CHIP1A             10 :FTL:-CH-CRYPTO-AMT            PIC S9(13)                 
CHIP1A                                                   COMP-3.                
CHIP1A             10 :FTL:-CH-CRYPTO-CURR-CODE      PIC X(003).                
CHIP1A             10 :FTL:-CH-CRYPTO-CASHBACK-AMT   PIC S9(013)                
CHIP1A                                                   COMP-3.                
CHIP1A             10 :FTL:-CH-LAST-CHIP-STATLAST-READ PIC X(001).              
CHIP1A             10 :FTL:-CH-GLOBAL-CCPS-DATA.                                
CHIP1A                15 :FTL:-CH-CCPS-LOG-LENGTH PIC 9(02).                    
CHIP1A                15 :FTL:-CH-CCPS-LOG-DATA.                                
CHIP1A                   20 :FTL:-CH-REASONS-NON-CHIP PIC X(01).                
CHIP1B                      88 :FTL:-CH-A1XCANCP  VALUE X'80'.                  
CHIP1B                      88 :FTL:-CH-A1XCAQNC  VALUE X'40'.                  
CHIP1B                      88 :FTL:-CH-A1XCN206  VALUE X'20'.                  
CHIP1B                      88 :FTL:-CH-A1XCINON  VALUE X'10'.                  
CHIP1A                      88 :FTL:-CH-A1XCDNRL  VALUE X'04'.                  
CHIP1B                      88 :FTL:-CH-A1XCSUPP  VALUE X'74'.                  
CHIP1A                   20 :FTL:-CH-CCPS-MISSING-DETAILS.                      
CHIP1A                      25 :FTL:-CH-FIRST-FLAG-BYTE PIC X(001).             
CHIP1A                         88 :FTL:-CH-A1XCPANM VALUE X'80'.                
CHIP1A                         88 :FTL:-CH-A1XCTVRM VALUE X'40'.                
CHIP1A                         88 :FTL:-CH-A1XCUPNM VALUE X'20'.                
CHIP1A                         88 :FTL:-CH-A1XCDKIM VALUE X'10'.                
CHIP1A                         88 :FTL:-CH-A1XCCVNM VALUE X'08'.                
CHIP1A                         88 :FTL:-CH-A1XCCVRM VALUE X'04'.                
CHIP1A                         88 :FTL:-CH-A1XCCRYM VALUE X'02'.                
CHIP1A                         88 :FTL:-CH-A1XCATCM VALUE X'01'.                
CHIP1A                      25 :FTL:-CH-SECOND-FLAG-BYTE PIC X(001).            
CHIP1A                         88 :FTL:-CH-A1XCAIPM VALUE X'80'.                
CHIP1A                         88 :FTL:-CH-A1XCCTTM VALUE X'40'.                
CHIP1A                         88 :FTL:-CH-A1XCTCCM VALUE X'20'.                
CHIP1A                         88 :FTL:-CH-A1XCTTDM VALUE X'10'.                
CHIP1A                         88 :FTL:-CH-A1XCCAMM VALUE X'08'.                
CHIP1A                         88 :FTL:-CH-A1XCCCCM VALUE X'04'.                
CHIP1A                      25 :FTL:-CH-THIRD-FLAG-BYTE PIC X(001).             
CHIP1A                         88 :FTL:-CH-A1XCCTTI VALUE X'40'.                
CHIP1A                         88 :FTL:-CH-A1XCTCCI VALUE X'20'.                
CHIP1A                         88 :FTL:-CH-A1XCTTDI VALUE X'10'.                
CHIP1A                         88 :FTL:-CH-A1XCCAMI VALUE X'08'.                
CHIP1A                         88 :FTL:-CH-A1XCCCCI VALUE X'04'.                
CHIP1A                         88 :FTL:-CH-A1XCCCBI VALUE X'02'.                
CHIP1A                      25 :FTL:-CH-FOURTH-BYTE-RESERV PIC X(001).          
CHIP1A                   20 :FTL:-CH-CCPS-PROCESS-FLAGS PIC X(001).             
CHIP1A                      88 :FTL:-CH-A1XCCPST VALUE X'80'.                   
CHIP1A                      88 :FTL:-CH-A1XCAMTR VALUE X'40'.                   
CHIP1A                      88 :FTL:-CH-A1XCVCAP VALUE X'20'.                   
CHIP1A                      88 :FTL:-CH-A1XCVCAF VALUE X'10'.                   
CHIP1A                      88 :FTL:-CH-A1XCVCAE VALUE X'30'.                   
CHIP1A                      88 :FTL:-CH-A1XC0595 VALUE X'08'.                   
CHIP1B                      88 :FTL:-CH-A1XCAOCM VALUE X'04'.                   
CHIP1A                      88 :FTL:-CH-A1XCICAR VALUE X'02'.                   
CHIP1A                      88 :FTL:-CH-A1XCIARP VALUE X'01'.                   
CHIP1A                   20 :FTL:-CH-CCPS-FROM-GLOBALS PIC X(001).              
CHIP1A                      88 :FTL:-CH-A1XCCRCV VALUE X'80'.                   
CHIP1A                      88 :FTL:-CH-A1XCSTCA VALUE X'40'.                   
CHIP1A                      88 :FTL:-CH-A1XCALCA VALUE X'20'.                   
CHIP1A                      88 :FTL:-CH-A1XCSTIA VALUE X'10'.                   
CHIP1A                      88 :FTL:-CH-A1XCALIA VALUE X'08'.                   
CHIP1A                      88 :FTL:-CH-A1XCACQC VALUE X'04'.                   
CHIP1A                      88 :FTL:-CH-A1XCACVV VALUE X'02'.                   
CHIP1B                      88 :FTL:-CH-A1XCAOCV VALUE X'01'.                   
CHIP1A                   20 :FTL:-CH-CCPS-MORE-GLOBALS PIC X(001).              
CHIP1A                      88 :FTL:-CH-A1XCICRT VALUE X'80'.                   
CHIP1A                      88 :FTL:-CH-A1XCICAM VALUE X'40'.                   
CHIP1A                      88 :FTL:-CH-A1XCAPCA VALUE X'20'.                   
CHIP1A                      88 :FTL:-CH-A1XCABCA VALUE X'10'.                   
CHIP1A                      88 :FTL:-CH-A1XCIFCH VALUE X'08'.                   
CHIP1A                      88 :FTL:-CH-A1XCIPCH VALUE X'04'.                   
CHIP1A                      88 :FTL:-CH-A1XCIECH VALUE X'02'.                   
CHIP1B                      88 :FTL:-CH-A1XCNONM VALUE X'0E'.                   
CHIP1B                      88 :FTL:-CH-A1XCPVTC VALUE X'01'.                   
CHIP1A                   20 :FTL:-CH-CCPS-CAM-VISIBLE PIC X(001).               
CHIP1A                      88 :FTL:-CH-A1XCIBRP VALUE X'80'.                   
CHIP1A                      88 :FTL:-CH-A1XCAPRP VALUE X'40'.                   
CHIP1A                      88 :FTL:-CH-A1XCABRP VALUE X'20'.                   
CHIP1A                      88 :FTL:-CH-A1XCAXRP VALUE X'10'.                   
CHIP1A                      88 :FTL:-CH-A1XCAQRP VALUE X'08'.                   
CHIP1B                   20 :FTL:-CH-A1CCPZX-RESP-SAVE PIC X(02).               
CHIP1B                   20 :FTL:-CH-A1CCGPRC-RESP-CODE PIC X(02).              
CHIP1B                   20 :FTL:-CH-A1CCSMRC-SECURE-RSP PIC X(01).             
CHIP1B                   20 :FTL:-CH-A1CCGLF3-MORE-CCPS PIC X(01).              
CHIP1B                      88 :FTL:-CH-A1XCAXNA VALUE X'80'.                   
CHIP1B                   20 :FTL:-CH-A1CCFG1-STATUS-IND PIC X(01).              
CHIP1B                      88 :FTL:-CH-A1XTRER VALUE  X'80'.                   
CHIP1B                      88 :FTL:-CH-A1XMDKM VALUE  X'40'.                   
CHIP1B                   20 :FTL:-CH-A1CCFRTE-STIP-CONDS PIC X(008).            
CHIP1B                   20 :FTL:-CH-A1CCSRSP-STIP-RESP PIC X(002).             
CHIP1B                   20 :FTL:-CH-A1CCSDCN-STIP-DEFLT PIC X(001).            
BER182* COPAC STSTUS IND FLG X'80'-TRANS QUALIF/X'40'-ISSUER CERTIF,            
BER182* X'20'-ACQ CERTIF                                                        
BER182                   20 :FTL:-CH-A1COPAC1-IND      PIC X(001).              
BER182                      88 :FTL:-CH-A1XCOPTR-TRANS-QUAL VALUE X'80'.        
BER182                      88 :FTL:-CH-A1XCOPI-ISS-CERTIF  VALUE X'40'.        
BER182                      88 :FTL:-CH-A1XCOPA-ACQ-CERTIF  VALUE X'20'.        
BER182                   20 FILLER                   PIC   X(12).               
CHIP1A             10 :FTL:-CH-CARD-AUTH-RESULTS     PIC   X(001).              
CHIP1A             10 :FTL:-CH-FIELDS-NOT-PRESENT    PIC   X(001).              
CHIP1A                88  :FTL:-CH-F22-NOT-PRESENT    VALUE X'01'.              
CHIP1A                88  :FTL:-CH-F130-NOT-PRESENT   VALUE X'02'.              
CHIP1A                88  :FTL:-CH-F139-1-NOT-PRESENT VALUE X'04'.              
CHIP1A                88  :FTL:-CH-F134-2-NOT-PRESENT VALUE X'08'.              
CHIP1B             10 :FTL:-CH-A0IPEC-TRANS-INDICATOR PIC  X(001).              
CHIP1B                88  :FTL:-CH-A0IPEC4-CCPS-OKAY  VALUE X'01'.              
CHIP1B                88  :FTL:-CH-A0IPEC4-NOT-CCPS   VALUE X'00'.              
CHIP1B             10 :FTL:-CH-A0IPEC-CARD-AUTH-DUI   PIC   X(001).             
CHIP1B                88  :FTL:-CH-A0IPEC4-NO-REL-PROBS   VALUE X'00'.          
CHIP1B                88  :FTL:-CH-A0IPEC4-RESLTS-NOT-REL VALUE X'01'.          
CHIP1B                88  :FTL:-CH-A0IPEC4-ACQ-INACT-AUTH VALUE X'02'.          
CHIP1B                88  :FTL:-CH-A0IPEC4-ISS-INACT-AUTH VALUE X'04'.          
CHIP1B             10 :FTL:-CH-USAGE-CODE            PIC   X(001).              
CHIP1B             10 :FTL:-CH-TRA-RET-FLAG          PIC   X(001).              
CHIP1B             10 :FTL:-CH-TRA-CRS-FLAG          PIC   X(001).              
CHIP1B             10 :FTL:-CH-TRA-RET-REAS          PIC   X(003).              
CHIP1B             10 :FTL:-CH-ACQ-CIB               PIC   X(006).              
CHIP1B             10 :FTL:-CH-ISS-CIB               PIC   X(006).              
BER182             10 :FTL:-CH-COPAC-DATA.                                      
BER182                15 :FTL:-CH-COPAC-CARDPROD     PIC X(01).                 
BER182                   88 :FTL:-CH-COPAC-ACCT           VALUE 'F'.            
BER182*ISO F54 ADDITIONAL AMOUNT                                                
BER182                15 :FTL:-CH-COPAC-ADDT-AMOUNTS.                           
BER182                   20 :FTL:-CH-ADDL-ACCT-TYPE    PIC X(02).               
BER182                   20 :FTL:-CH-ADDL-AMT-TYPE     PIC X(02).               
BER182                   20 :FTL:-CH-ADDL-AMT-CURR-CD  PIC X(03).               
BER182                   20 :FTL:-CH-ADDL-AMT-SIGN     PIC X(01).               
BER182                   20 :FTL:-CH-ADDL-AMOUNT       PIC S9(13)               
BER182                                                 COMP-3.                  
BER182*ISO F125 SUPPORTING INFORMATION - COPAC USAGE 0005                       
BER182                15 :FTL:-CH-COPAC-SUPP-INFORM.                            
BER182                   20 :FTL:-CH-NEG-FILE-VER-NUM  PIC X(02).               
BER182                   20 :FTL:-CH-COPAC-AUD-TRCE-NR.                         
BER182                      25 :FTL:-CH-PREAU-TRACE-NR PIC X(02).               
BER182                      25 :FTL:-CH-PURCH-TRACE-NR PIC X(02).               
BER182                      25 :FTL:-CH-CRYPT-TRACE-NR PIC X(02).               
BER182                   20 :FTL:-CH-COPAC-MERCH-AUDIT.                         
BER182                      25 :FTL:-CH-BATCH-TRCE-NR  PIC X(02).               
BER182                      25 :FTL:-CH-TRANS-TRCE-NR  PIC X(02).               
BER182*ISO F150 COPAC CARDHOLDER CARD SCHEME CRYPTOGRAM                         
BER182                15 :FTL:-CH-COPAC-SCHEME-CRYPT   PIC X(08).               
BER182             10 FILLER                           PIC X(100).              
*******-----------------------------------------------------------              
BER926*        *------------------------------------------------                
BER926*        *   CRIS ALERT CONTINUATION FIELDS *                             
BER926*        *------------------------------------------------                
RT2433     03  :FTL:-CR-GROUP.                                                  
BER926         05  :FTL:-CR-RECORD.                                             
BER926             10 :FTL:-CR-FWD-INST-ID           PIC X(06).                 
BER926             10 :FTL:-CR-ADDT-DATA-PRIVATE.                               
BER926                15 :FTL:-CR-ALERT-TYPE-ID-48   PIC X(01).                 
BER926                15 :FTL:-CR-ALERT-TYPE.                                   
BER926                   20 :FTL:-CR-ALERT-TYPE-POS1 PIC X(01).                 
BER926                   20 :FTL:-CR-ALERT-TYPE-POS2 PIC X(01).                 
BER926                   20 :FTL:-CR-ALERT-TYPE-POS3 PIC X(01).                 
BER926                15 :FTL:-CR-TRANS-RISK-SCORE   PIC 9(05).                 
BER926                15 :FTL:-CR-ACCT-NUMBER        PIC X(22).                 
BER926                15 :FTL:-CR-TRANS-AMT          PIC X(13).                 
BER926                15 :FTL:-CR-AMT-CARDHLDR-BILL  PIC X(13).                 
BER926                15 :FTL:-CR-TRANSMISSION-DT    PIC X(10).                 
BER926                15 :FTL:-CR-CONV-RATE-CARDHLDR PIC X(08).                 
BER926                15 :FTL:-CR-MERCH-CATEG-CD     PIC X(04).                 
BER926                15 :FTL:-CR-POS-ENTRY-MODE-CD  PIC X(04).                 
BER926                15 :FTL:-CR-ACQ-BIN            PIC X(11).                 
BER926                15 :FTL:-CR-RESP-CODE          PIC X(02).                 
BER926                15 :FTL:-CR-CARDACCPTR-TERM-ID PIC X(08).                 
BER926                15 :FTL:-CR-CARDACCPTR-ID-CODE PIC X(08).                 
BER926                15 :FTL:-CR-CARDACCPTR-NAM-LOC PIC X(40).                 
BER926                15 :FTL:-CR-RESP-SCRS-REAS-CD  PIC X(01).                 
BER926                15 :FTL:-CR-ADDR-VERF-RESL-CD  PIC X(01).                 
BER926                15 :FTL:-CR-CVV-RESL-CD        PIC X(01).                 
BER926                15 :FTL:-CR-CURR-CODE-TRANS    PIC X(03).                 
BER926                15 :FTL:-CR-CURR-CODE-CARDHLDR PIC X(03).                 
BER926                15 :FTL:-CR-NETWORK-ID-CODE    PIC X(04).                 
BER926                15 :FTL:-CR-STIP-SWITCH-RS-CD  PIC X(04).                 
BER926             10 :FTL:-CR-SUPP-INFO-DATA.                                  
BER926                15 :FTL:-CR-ALERT-TYPE-ID-125  PIC X(01).                 
BER926                15 :FTL:-CR-MERCH-LOC-US.                                 
BER926                   20 :FTL:-CR-MERCH-CITY-US   PIC X(17).                 
BER926                   20 :FTL:-CR-MERCH-US-FIL    PIC X(01).                 
BER926                   20 :FTL:-CR-MERCH-US-STATE  PIC X(02).                 
BER926                15 :FTL:-CR-MERCH-LOC-INTRN REDEFINES                     
BER926                   :FTL:-CR-MERCH-LOC-US.                                 
BER926                   20 :FTL:-CR-MERCH-CNTR-NAME PIC X(17).                 
BER926                   20 :FTL:-CR-MERCH-INTR-FIL  PIC X(03).                 
BER926                15 :FTL:-CR-TRACK-ID           PIC X(01).                 
BER926                15 :FTL:-CR-TRACK-LENGTH       PIC X(02).                 
BER926                15 :FTL:-CR-TRACK-DATA         PIC X(76).                 
BER926                15 FILLER                      PIC X(50).                 
*******-----------------------------------------------------------              
SEP98 *        *-------------------------------------------------------*        
SEPT98*        *   VSEC & CVV2 CONTINUATION FIELDS                     *        
SEPT98*        *-------------------------------------------------------*        
RT2433     03  :FTL:-SE-GROUP.                                                  
SEPT98         05  :FTL:-SE-SECURE05-RECORD.                                    
BER427             10 :FTL:-SE-ELEC-COMMERCE-IND   PIC X(002).                  
BER427                   88 :FTL:-SE-NOT-APPLIABLE        VALUE '00'.           
BER427                   88 :FTL:-SE-SEC-WITH-CERTIFICATE VALUE '05'.           
BER427                   88 :FTL:-SE-SEC-NO-CERTIFICATE   VALUE '06'.           
BER427                   88 :FTL:-SE-CHANEL-ENCRYPT-TXN   VALUE '07'.           
BER427                   88 :FTL:-SE-NON-SECURE-SEC-TXN   VALUE '08'.           
RT1401             10 :FTL:-SE-BITMAP-FIELD          PIC X(08).                 
RT1401             10 :FTL:-SE-VSEC-CARDHLDR-CERT-NR PIC X(17).                 
RT1401             10 :FTL:-SE-VSEC-MERCHANT-CERT-NR PIC X(17).                 
RT1401             10 :FTL:-SE-VSEC-TRANSACTION-ID   PIC X(20).                 
RT1401             10 :FTL:-SE-VSEC-TRANSTAIN        PIC X(20).                 
RT1268             10 :FTL:-SE-CVV2-DATA.                                       
RT1268                15 :FTL:-SE-CVV2-PRESENCE-IND  PIC X(01).                 
RT1268                15 :FTL:-SE-CVV2-RESP-TYPE     PIC X(01).                 
RT1268                15 :FTL:-SE-CVV2-VALUE         PIC X(04).                 
RT1268                15 :FTL:-SE-CVV2-AUTH-RESP     PIC X(01).                 
RT1268                   88 :FTL:-SE-CVV2-MATCH             VALUE 'M'.          
RT1268                   88 :FTL:-SE-CVV2-NO-MATCH          VALUE 'N'.          
RT1268                   88 :FTL:-SE-CVV2-NOT-PROCESS       VALUE 'P'.          
RT1268                   88 :FTL:-SE-CVV2-NOT-PRESENT       VALUE 'S'.          
RT1268                   88 :FTL:-SE-CVV2-ISS-NOT-PART      VALUE 'U'.          
RT1268                15 :FTL:-SE-CVV2-LOG-DATA.                                
RT1268                   20 :FTL:-SE-CVV2-FLAG1.                                
RT1268                      25 :FTL:-SE-CVV2-ACQ-CERT       PIC X(01).          
RT1268                         88 :FTL:-SE-CVV2-ACQ-CERT-YES VALUE '1'.         
RT1268                      25 :FTL:-SE-CVV2-ISS-CERT       PIC X(01).          
RT1268                         88 :FTL:-SE-CVV2-ISS-CERT-YES VALUE '1'.         
RT1268                      25 :FTL:-SE-CVV2-ISS-BIN-OPT1   PIC X(01).          
RT1268                         88 :FTL:-SE-CVV2-ISS-OPT-ALL VALUE '1'.          
RT1268                      25 :FTL:-SE-CVV2-ISS-BIN-OPT2   PIC X(01).          
RT1268                         88 :FTL:-SE-CVV2-ISS-OPT-NONE VALUE '1'.         
RT1268                      25 :FTL:-SE-CVV2-ISS-BIN-STIP   PIC X(01).          
RT1268                         88 :FTL:-SE-CVV2-ISS-STIP-SET VALUE '1'.         
RT1268                      25 :FTL:-SE-CVV2-F126-10-2      PIC X(01).          
RT1268                         88 :FTL:-SE-CVV2-F126-10-2-PRS VALUE '1'.        
RT1268                      25 :FTL:-SE-CVV2-ON-USE-1ST-KEYS PIC X(01).         
RT1268                         88 :FTL:-SE-CVV2-ON-USE-KEY1 VALUE '1'.          
RT1268                      25 :FTL:-SE-CVV2-ON-USE-2ND-KEYS PIC X(01).         
RT1268                         88 :FTL:-SE-CVV2-ON-USE-KEY2 VALUE '1'.          
RT1268                   20 :FTL:-SE-CVV2-FLAG2.                                
RT1268                      25 :FTL:-SE-CVV2-ACQ-SEND-F126-10 PIC X(01).        
RT1268                         88 :FTL:-SE-CVV2-ACQ-F126-10 VALUE '1'.          
RT1268                      25 :FTL:-SE-CVV2-F126-10-1-PRES PIC X(01).          
RT1268                         88 :FTL:-SE-CVV2-F126-10-1  VALUE '1'.           
RT1268                      25 :FTL:-SE-CVV2-F126-10-3-PRES PIC X(01).          
RT1268                         88 :FTL:-SE-CVV2-F126-10-3  VALUE '1'.           
RT1268                      25 :FTL:-SE-CVV2-TRANS          PIC X(01).          
RT1268                         88 :FTL:-SE-CVV2-TRANS-GOOD VALUE '1'.           
RT1268                      25 :FTL:-SE-CVV2-F14-NOT-IN-MSG PIC X(01).          
RT1268                         88 :FTL:-SE-CVV2-F14-MSG-NO VALUE '1'.           
RT1268                      25 :FTL:-SE-CVV2-STRIP-IS-NO-CVV2 PIC X(01).        
RT1268                         88 :FTL:-SE-CVV2-STRIP-NO   VALUE '1'.           
RT1268                      25 :FTL:-SE-CVV2-VRT1-GOT-STATION PIC X(01).        
RT1268                         88 :FTL:-SE-CVV2-VRT1-STATION VALUE '1'.         
RT1268                   20 :FTL:-SE-CVV2-PRES-IND-GIV PIC X(01).               
RT1268                   20 :FTL:-SE-CVV2-RESP-CODES.                           
RT1268                      25 :FTL:-SE-CVV2-RESP-TO-ACQ   PIC X(001).          
RT1268                      25 :FTL:-SE-CVV2-RESP-TO-ISS   PIC X(001).          
RT1268                      25 :FTL:-SE-CVV2-RESP-BY-ISS   PIC X(001).          
RT1268                      25 :FTL:-SE-CVV2-HSM-RESP-CD   PIC X(002).          
RT1268                      25 :FTL:-SE-CVV2-STIP-RESP-CD  PIC X(002).          
RT1682             10 :FTL:-SE-F44-POS9-DECL-REASON        PIC X(001).          
RT1682             10 FILLER                               PIC X(100).          
*******-----------------------------------------------------------              
RT2395*        *-------------------------------------------------------*        
RT2395*        *   POS CHECK CONVERSION SERVICE CONTINUATION FIELDS    *        
RT2395*        *-------------------------------------------------------*        
RT2433     03  :FTL:-CS-GROUP.                                                  
RT2395         05  :FTL:-CHECK-RECORD.                                          
RT2395             10 :FTL:-CS-CHK-CONVERSION-FLAGS  PIC X(001).                
RT2395                88 :FTL:-CS-CONVERSION-IND       VALUE X'80'.             
RT2395                88 :FTL:-CS-VERIFICATION-IND     VALUE X'40'.             
RT2395                88 :FTL:-CS-GUARANTEE-IND        VALUE X'20'.             
RT2395                88 :FTL:-CS-THIRD-PARTY-IND      VALUE X'10'.             
RT2395                88 :FTL:-CS-PREVIOUS-PART        VALUE X'08'.             
RT2395                88 :FTL:-CS-RESERVED-1           VALUE X'04'.             
RT2395                88 :FTL:-CS-RESERVED-2           VALUE X'02'.             
RT2395                88 :FTL:-CS-CHECK-TRANSACTION    VALUE X'01'.             
RT2395             10 :FTL:-CS-F44-12-CHK-STLMT-CODE PIC X(001).                
RT2395                88 :FTL:-CS-VISA-STLMT           VALUE '1'.               
RT2395                88 :FTL:-CS-ACH-STLMT            VALUE '2'.               
RT2395             10 :FTL:-CS-CHECK-NUMBER          PIC X(15).                 
RT2395             10 :FTL:-CS-F48-THIRD-PARTY-DATA  PIC X(100).                
RT2395             10 :FTL:-CS-F125-RAW-MICR-DATA    PIC X(064).                
RT2395             10 FILLER                       PIC X(100).                  
*******-----------------------------------------------------------              
*******- EPAY IS GOING AWAY - BUILD ALL NEW FIELDS BETWEEN THESE BOXES          
*******-----------------------------------------------------------              
115400         05  :FTL:-XX-TEMPORARY-LABEL.                                    
115410             10  FILLER PIC X(200).                                       
*******-----------------------------------------------------------              
*******- EPAY IS GOING AWAY - BUILD ALL NEW FIELDS BETWEEN THESE BOXES          
*******-----------------------------------------------------------              
EPAY1A*        *-------------------------------------------------------*        
EPAY1A*        *   ELECTRONIC BANKING FIELDS                           *        
EPAY1A*        *-------------------------------------------------------*        
RT2433     03  :FTL:-EB-GROUP.                                                  
EPAY1A         05  :FTL:-EBANK01-RECORD.                                        
EPAY1A             10  :FTL:-EB-PAYEE              PIC  X(25).                  
EPAY1A             10  :FTL:-EB-CUSTOMER-NAME      PIC  X(25).                  
EPAY1A             10  :FTL:-EB-CUSTOMER-ADDR      PIC  X(57).                  
EPAY1A             10  :FTL:-EB-BILLER-PHONE       PIC  X(18).                  
EPAY1A             10  FILLER                      PIC  X(29).                  
EPAY1A             10  :FTL:-EB-ACQ-INST-ID        PIC  X(11).                  
EPAY1A             10  :FTL:-EB-ISS-INST-ID        PIC  X(11).                  
EPAY1A             10  :FTL:-EB-BILLER-ADDR        PIC  X(57).                  
EPAY1A             10  :FTL:-EB-ADDITIONAL-AMOUNTS OCCURS 6 TIMES.              
EPAY1A                 15  :FTL:-EB-ADDL-ACCT-TYPE     PIC  X(02).              
EPAY1A                 15  :FTL:-EB-ADDL-AMT-TYPE      PIC  X(02).              
EPAY1A                 15  :FTL:-EB-ADDL-AMT-CURR-CODE PIC  X(03).              
EPAY1A                 15  :FTL:-EB-ADDL-AMOUNT    PIC S9(13) COMP-3.           
EPAY2              10  :FTL:-EB-AMT-REPL-BILL      PIC  9(12).          0000000 
EPAY2              10  :FTL:-EB-CHECK-ACPT-DATE    PIC  9(06).          0000000 
EPAY2              10  :FTL:-EB-ACCT-ID2           PIC  X(28).          0000000 
EPAY2              10  :FTL:-EB-PROCESS-BY-DATE    PIC  9(06).          0000000 
