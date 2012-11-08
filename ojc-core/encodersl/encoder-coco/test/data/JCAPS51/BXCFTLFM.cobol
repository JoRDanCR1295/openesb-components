000050*****************************************************************         
000100* YEAR 2000 COMPLIANT  ?  MM/DD/CCYY   REVIEWED BY: ______      *         
000150* YEAR 2000 UPDATED    _  MM/DD/CCYY    UPDATED BY: ______      *         
000200*****************************************************************         
000250* FTL - FORMATED TRANSACTION LOG.  CREATED BY STL EXTRACT.       *        
000300*                                  AND BY DEBIT DTF EXTRACT      *        
000350*----------------------------------------------------------------* D   N |
000400*                         CHANGE HISTORY                         *   P   |
000450*----------------------------------------------------------------*   L   |
000500* MOD DATE  BY WHOM   REASON FOR CHANGE - CHANGE MADE                A   |
000550* 07/29/99  F. BRADEN REBELLION           NEW COPY BOOK                   
000550* 03/29/00  F. BRADEN REBELLION           Variable support                
000600*----------------------------------------------------------------* |   |  
000650                                                                          
000700*----------------------------------------------------------------* |   |  
000750*    RECORD HEADER - COMMON DATA                                 * |   |  
000800*----------------------------------------------------------------* |   |  
000825     01  BXCFTLFM.
000850     03  :FTL:-FM-COMMON-DATA.                                     V   V  
000900         05  :FTL:-FM-REC-TYPE               PIC  X(001).         001-001 
000950             88  :FTL:-FM-HEADER             VALUE 'H'.                   
001000             88  :FTL:-FM-AUTHORIZATION      VALUE '0'.                   
001050             88  :FTL:-FM-FILE-MAINTENANCE   VALUE '1'.                   
001100             88  :FTL:-FM-CP-NONPACM         VALUE '3'.                   
EPAY1A             88  :FTL:-FM-CONTINUATION       VALUE '4'.                   
001200             88  :FTL:-FM-MIP-BILLING-STATS  VALUE '5'.                   
001250             88  :FTL:-FM-MIP-ONUS-STATS     VALUE '6'.                   
001300             88  :FTL:-FM-ARS-ADVICE         VALUE '7'.                   
001350             88  :FTL:-FM-INTER-VIC          VALUE '8'.                   
001400             88  :FTL:-FM-REJECTED-MSG       VALUE '9'.                   
SUSEG              88  :FTL:-FM-SUSEGMENT-MSG      VALUE 'S'.                   
001500             88  :FTL:-FM-TRAILER            VALUE 'T'.                   
001550         05  :FTL:-FM-VIC                    PIC  X(001).         002-002 
001600             88  :FTL:-FM-VIC-OCW            VALUE 'A'.                   
001650             88  :FTL:-FM-VIC-OCE            VALUE 'B'.                   
001700             88  :FTL:-FM-VIC-OCEMA          VALUE 'C'.                   
001750             88  :FTL:-FM-VIC-OCB            VALUE 'D'.                   
001800             88  :FTL:-FM-VIC-OCAP           VALUE 'E'.                   
001850         05  :FTL:-FM-DATE-YYDDD             PIC S9(005) COMP-3.  003-005 
001900         05  :FTL:-FM-TIME-HHMMSS            PIC S9(006) COMP-3.  006-009 
001950         05  :FTL:-FM-SRCE-STATION           PIC S9(006) COMP-3.  010-013 
002000*----------------------------------------------------------------*        
002050*       EXCEPTION, PVV, AND MCFS FILE MAINTENANCE.  F 'TYPE 1'   *0000000 
002100*----------------------------------------------------------------*        
002150     03  :FTL:-FILE-MAINTENCE-RECORD.                                     
002200         05  :FTL:-FM-DEST-STATION           PIC S9(006) COMP-3.  014-017 
002250         05  :FTL:-FM-UPDT-STATUS            PIC  X(001).         018-018 
002300             88  :FTL:-FM-UPDATE-VALID       VALUE 'X'.                   
002350             88  :FTL:-FM-UPDATE-ERROR       VALUE 'Z'.                   
002400             88  :FTL:-FM-UPDATE-ERRBIL      VALUE 'Y'.                   
002450         05  :FTL:-FM-TRAN-CODE.                                          
002500             10  :FTL:-FM-TRAN-CODE-1        PIC  X(001).         019-019 
002550                 88  :FTL:-FM-EXCEPTION      VALUE 'E'.                   
002600                 88  :FTL:-FM-AVS-PVV-COMBO  VALUE 'C'.                   
002650                 88  :FTL:-FM-AVS            VALUE 'A'.                   
002700                 88  :FTL:-FM-PVV            VALUE 'P'.                   
002750                 88  :FTL:-FM-MCFS-MCDSC     VALUE 'P'.                   
002800                 88  :FTL:-FM-TELECD         VALUE 'P'.                   
002850                 88  :FTL:-FM-CARDHOLDER     VALUE 'P'.                   
002900                 88  :FTL:-FM-MCFS           VALUE 'M'.                   
002950                 88  :FTL:-FM-RISK           VALUE 'R'.                   
003000                 88  :FTL:-FM-FULL-AUTH      VALUE 'F'.                   
003050                 88  :FTL:-FM-INCR-DECR      VALUE 'I'.                   
003100             10  :FTL:-FM-TRAN-CODE-2        PIC  X(001).         020-020 
003150                 88  :FTL:-FM-ADD            VALUE 'A'.                   
003200                 88  :FTL:-FM-CHANGE         VALUE 'C'.                   
003250                 88  :FTL:-FM-DELETE         VALUE 'D'.                   
003300                 88  :FTL:-FM-REPLACE        VALUE 'R'.                   
003350         05  :FTL:-FM-CARD-NUMBER            PIC  X(028).         021-048 
003400         05  :FTL:-FM-MCFS-KEY-DATA REDEFINES                     021-048 
003450             :FTL:-FM-CARD-NUMBER.                                        
003500             10  :FTL:-FM-MCFS-KEY.                                       
003550                 15  :FTL:-FM-MCFS-CRDACCPT-ID-F42                        
003600                                             PIC  X(15).          021-035 
003650                 15  :FTL:-FM-MCFS-CRDACPT-TRMID-F41                      
003700                                             PIC  X(08).          036-043 
003750             10  FILLER                      PIC  X(05).          044-048 
003800         05  :FTL:-FM-CARDTYPE               PIC  X(01).          049-049 
003850             88  :FTL:-FM-VISA   VALUE '4' 'P' 'E' 'B' 'R' 'S'.           
003900             88  :FTL:-FM-VISA-CLASSIC       VALUE '4'.                   
003950             88  :FTL:-FM-VISA-PREMIER       VALUE 'P'.                   
004000             88  :FTL:-FM-VISA-ELECTRON      VALUE 'E'.                   
004050             88  :FTL:-FM-VISA-BUSINESS      VALUE 'B'.                   
004100             88  :FTL:-FM-VISA-CORPORATE     VALUE 'R'.                   
004150             88  :FTL:-FM-VISA-PROCUREMENT   VALUE 'S'.                   
004200         05  :FTL:-FM-OPER-ID                PIC  X(003).         050-052 
004250             88  :FTL:-FM-LAST-PICKUP-RESPONSE VALUE 'PKU'.               
004300             88  :FTL:-FM-LAST-PICKUP-RESP-REGN VALUE 'PKR'.              
004350             88  :FTL:-FM-LAST-DECLINE-AND-ADD VALUE 'DCA'.               
004400             88  :FTL:-FM-LAST-REFER-AND-ADD   VALUE 'RFA'.               
004450             88  :FTL:-FM-LAST-APPROVE-AND-REMOV VALUE 'APR'.             
004500             88  :FTL:-FM-LAST-ATS             VALUE 'ATS'.               
004550             88  :FTL:-FM-LAST-RIS             VALUE 'RIS'.               
004600             88  :FTL:-FM-LAST-VTSC            VALUE 'VTC'.               
004650             88  :FTL:-FM-LAST-MBR-ONLINE      VALUE 'MRO'.               
004700             88  :FTL:-FM-LAST-MBR-BATCH       VALUE 'MRB'.               
004750             88  :FTL:-FM-LAST-MBR-MIP         VALUE 'MIP'.               
004800             88  :FTL:-FM-LAST-3RD-PTY-BATCH   VALUE 'TPB'.               
004850             88  :FTL:-FM-LAST-3RD-PTY-ONLINE  VALUE 'TPO'.               
004900             88  :FTL:-FM-LAST-3RD-PTY-MIP     VALUE 'TPM'.               
004950             88  :FTL:-FM-LAST-BLANKS          VALUE '   '.               
005000             88  :FTL:-FM-LAST-EAR             VALUE 'EAR'.               
005050         05  :FTL:-FM-TEXT-DATA              PIC  X(025).         053-077 
005100         05  FILLER    REDEFINES                                  053-077 
005150             :FTL:-FM-TEXT-DATA.                                  053-077 
005200             10  :FTL:-FM-ERROR-CODE         PIC  X(002).         053-054 
005250             10  :FTL:-FM-ERROR-MSG          PIC  X(023).         055-077 
005300         05  :FTL:-FM-ISS-BIN                PIC S9(006) COMP-3.  078-081 
005350         05  :FTL:-FM-ISS-PCR                PIC  X(004).         082-085 
005400         05  :FTL:-FM-UPDATING-PCR REDEFINES :FTL:-FM-ISS-PCR             
005450                                             PIC  X(004).         082-085 
005500         05  :FTL:-FM-ISS-REGION             PIC  X(001).         086-086 
005550         05  :FTL:-FM-FILETYPE               PIC  X(001).         087-087 
005600             88  :FTL:-FM-FILETYPE-EXCEPTION VALUE 'E'.                   
005650             88  :FTL:-FM-FILETYPE-PVV       VALUE 'P'.                   
005700             88  :FTL:-FM-FILETYPE-MCFS      VALUE 'M'.                   
005750             88  :FTL:-FM-FILETYPE-VISAPHONE VALUE 'V'.                   
005800             88  :FTL:-FM-FILETYPE-AVS       VALUE 'A'.                   
005850         05  :FTL:-FM-PURGE-DATE-YYDDD       PIC S9(005) COMP-3.  088-090 
005900         05  :FTL:-FM-EXC-DATA.                                           
005950             10  :FTL:-FM-REGIONS            PIC  X(016).         091-106 
006000             10  :FTL:-FM-ACTION-CODE        OCCURS 2 TIMES.              
006050                 15  :FTL:-FM-ACT-CODE       PIC  X(002).         107-110 
006100         05  :FTL:-FM-ISS-INST-ID            PIC  X(011).         111-121 
006150         05  :FTL:-FM-UPDT-MODE              PIC  X(001).         122-122 
006200             88  :FTL:-FM-ONLINE-MODE        VALUE 'O'.                   
006250             88  :FTL:-FM-BATCH-MODE         VALUE 'B'.                   
006300         05  :FTL:-FM-UPDT-FMT               PIC  X(001).         123-123 
006350             88  :FTL:-FM-NONPCAS-FMT        VALUE 'N'.                   
006400             88  :FTL:-FM-PCAS-FMT           VALUE 'P'.                   
006450         05  :FTL:-FM-CRB-SUB-REGIONS        PIC  X(006).         124-129 
006500         05  :FTL:-FM-CRB-SUBREG-REDEF       REDEFINES                    
006550             :FTL:-FM-CRB-SUB-REGIONS.                                    
006600             10  :FTL:-FM-CRB-SUB-REGA       PIC  X(001).         124-124 
006650             10  :FTL:-FM-CRB-SUB-REGB       PIC  X(001).         125-125 
006700             10  :FTL:-FM-CRB-SUB-REGC       PIC  X(001).         126-126 
006750             10  :FTL:-FM-CRB-SUB-REGD       PIC  X(001).         127-127 
006800             10  :FTL:-FM-CRB-SUB-REGE       PIC  X(001).         128-128 
006850             10  :FTL:-FM-CRB-SUB-REGF       PIC  X(001).         129-129 
006900         05  :FTL:-FM-EXC-FLAG               PIC  X(001).         130-130 
006950*            POSSIBLE NUMERIC VALUES AND THEIR EXPLANATIONS:              
007000*            X'80' = RCL IS SOURCE OF THIS RECORD                         
007050*            X'40' = RESPONSE CODE IS ISO FORMAT                          
007100*            X'20' = RESPONSE CODE FLD IS 'SPEC PROC CODE'                
007150*            X'10' = (CMI) DUAL RESIDENCY  RECORD                         
007200*            X'00' = NO VALUE PRESENT IN   RECORD                         
007250         05  :FTL:-FM-PIN-ALG                PIC  X(002).         131-132 
007300             88  :FTL:-FM-PALG-NO-PIN-DATA   VALUE  '00'.                 
007350             88  :FTL:-FM-PALG-PVV-ALGORITHM VALUE  '01'.                 
007400             88  :FTL:-FM-PALG-ATALLA-SERVICE VALUE '02'.                 
007450             88  :FTL:-FM-PALG-IBM-PIN-OFFSET VALUE '04'.                 
007500             88  :FTL:-FM-PALG-MCFS-RECORD   VALUE  '09'.                 
007550         05  :FTL:-FM-FILE-NAME              PIC  X(002).         133-134 
007600             88  :FTL:-FM-PVV-CARDHOLDER-AVS VALUE  'A2'.                 
007650             88  :FTL:-FM-PVV-CARDHOLDER-PIN VALUE  'P2'.                 
007700             88  :FTL:-FM-PVV-CARDHOLDER     VALUE  'C2'.                 
007750             88  :FTL:-FM-PVV-CMI-DEBIT      VALUE  'C4'.                 
007800             88  :FTL:-FM-EXC-SPECIAL        VALUE  'E1'.                 
007850             88  :FTL:-FM-EXC-NORMAL         VALUE  'E2'.                 
007900             88  :FTL:-FM-EXC-CMI-DUAL       VALUE  'E3'.                 
007950             88  :FTL:-FM-EXC-CMI-DEBIT      VALUE  'E4'.                 
008000             88  :FTL:-FM-CRB-CANADA-SUB     VALUE  'E9'.                 
008050             88  :FTL:-FM-RISK-LVL-FILE      VALUE  'R2'.                 
008100             88  :FTL:-FM-FULL-AUTH-FILE     VALUE  'F2'.                 
008150             88  :FTL:-FM-INCR-DECRMNT       VALUE  'I2'.                 
008200             88  :FTL:-FM-PALG-MCFS-RECORD   VALUE  'M9'.                 
008250             88  :FTL:-FM-TELECODE-VISAPHONE VALUE  'S2'.                 
008300         05  :FTL:-FM-TELECODE               PIC  X(004).         135-138 
008350         05  :FTL:-FM-VBOC-ID                PIC  X(001).         139-139 
008400         05  :FTL:-FM-AVS-POST-CODE          PIC  X(009).         140-148 
008450         05  :FTL:-FM-AVS-RESERV-ADDR-VER    PIC  X(005).         149-153 
008500         05  :FTL:-FM-PIN-KEY-INDEX          PIC  X(002).         154-155 
008550         05  :FTL:-FM-PIN-VERIF-VALUE        PIC  X(004).         156-159 
008600         05  :FTL:-FM-HIST-ACDB-UPD-SOURCE   PIC  X(001).         160-160 
008650         05  :FTL:-FM-HIST-OTHER-UPD-SOURCE  PIC  X(001).         161-161 
008700         05  :FTL:-FM-MCFS-HIST-UPD-SOURCE REDEFINES              161-161 
008750             :FTL:-FM-HIST-OTHER-UPD-SOURCE  PIC  X(001).                 
008800         05  :FTL:-FM-AVS-HIST-UPD-SOURCE REDEFINES               161-161 
008850             :FTL:-FM-HIST-OTHER-UPD-SOURCE  PIC  X(001).                 
008900         05  :FTL:-FM-LAST-NON-APPR-UPD-SRCE PIC  X(001).         162-162 
008950         05  :FTL:-FM-LAST-TELE-UPD-SRCE REDEFINES                        
009000               :FTL:-FM-LAST-NON-APPR-UPD-SRCE PIC X(001).        162-162 
009050             88 :FTL:-FM-TELE-SRCE-IDSADDM   VALUE '1'.                   
009100             88 :FTL:-FM-TELE-SRCE-IDSADDV   VALUE '2'.                   
009150             88 :FTL:-FM-TELE-SRCE-BADVER    VALUE '3'.                   
009200             88 :FTL:-FM-TELE-SRCE-TOOMANY   VALUE '4'.                   
009250             88 :FTL:-FM-TELE-SRCE-MEMONL    VALUE 'M'.                   
009300             88 :FTL:-FM-TELE-SRCE-MEMBAT    VALUE 'B'.                   
009350             88 :FTL:-FM-TELE-SRCE-MEMVAP    VALUE 'V'.                   
009400         05  :FTL:-FM-AVS-LAST-UPD-SRCE REDEFINES                 162-162 
009450               :FTL:-FM-LAST-NON-APPR-UPD-SRCE PIC X(001).                
009500* ---------------------------------------------------                     
009550         05  :FTL:-FM-MCFS-LAST-UPD-SRCE REDEFINES                162-162 
009600             :FTL:-FM-LAST-NON-APPR-UPD-SRCE PIC X(001).                  
009650             88  :FTL:-FM-MCFS-LAST-MBR-ONLINE  VALUE 'M'.                
009700             88  :FTL:-FM-MCFS-LAST-MBR-BATCH   VALUE 'B'.                
009750             88  :FTL:-FM-MCFS-LAST-MBR-VAP     VALUE 'V'.                
009800             88  :FTL:-FM-MCFS-LAST-3RD-PTY-ONLN VALUE 'O'.               
009850             88  :FTL:-FM-MCFS-LAST-3RD-PTY-BATCH VALUE 'U'.              
009900             88  :FTL:-FM-MCFS-LAST-3RD-PTY-VAP VALUE 'P'.                
009950         05  :FTL:-FM-I110R-CODE             PIC  X(002).         163-164 
010000         05  :FTL:-FM-ISSUING-PCR            PIC  X(004).         165-168 
010050         05  :FTL:-FM-MCFS-MEM-IN-TRN-CODE   PIC  X(001).         169-169 
010100             88 :FTL:-FM-ADD-PROCESSED-AS-CHANGE VALUE 'A'.               
010150             88 :FTL:-FM-CHANGE-PROCESSED-AS-ADD VALUE 'C'.               
010200         05  :FTL:-FM-MCFS-F42-LGTH          PIC  9(002).         170-171 
010250         05  :FTL:-FM-MCFS-F41-LGTH          PIC  9(001).         172-172 
010300         05  :FTL:-FM-MCFS-SERVICE-TYPE      PIC  X(001).         173-173 
010350             88  :FTL:-FM-MCFS-OLD-SERVICE   VALUE ' '.                   
010400             88  :FTL:-FM-MCFS-SVC-VISA      VALUE 'V'.                   
010450             88  :FTL:-FM-MCFS-SVC-AMEX      VALUE 'X'.                   
010500             88  :FTL:-FM-MCFS-SVC-DISCVR    VALUE 'D'.                   
010550             88  :FTL:-FM-MCFS-SVC-CHK-ACC   VALUE 'A'.                   
010600             88  :FTL:-FM-MCFS-SVC-MC        VALUE 'M'.                   
010650             88  :FTL:-FM-MCFS-SVC-UNIV      VALUE 'U'.                   
010700         05  :FTL:-FM-MCFS-ERR-DATA.                                      
010750             10  :FTL:-FM-MCFS-ERR-ACQ-BIN-LGTH PIC X(002).       174-175 
010800             10  :FTL:-FM-MCFS-ERR-ACQ-BIN     PIC X(011).        176-186 
010850             10  :FTL:-FM-MCFS-ERR-UPDATE-CODE PIC X(001).        187-187 
010900             10  :FTL:-FM-MCFS-ERR-PURGE-DATE  PIC X(006).        188-193 
010950             10  :FTL:-FM-MCFS-ERRU-SVCDATA.                              
011000                 15  :FTL:-FM-MCFS-ERRU-MCC-F18                   194-197 
011050                                             PIC X(004).                  
011100                 15  :FTL:-FM-MCFS-ERRU-NAME-LOC-F43.                     
011150                     20  :FTL:-FM-MCFS-ERRU-NAME-F43              198-222 
011200                                             PIC X(025).                  
011250                     20  :FTL:-FM-MCFS-ERRU-CITY-F43              223-235 
011300                                             PIC X(013).                  
011350                     20  :FTL:-FM-MCFS-ERRU-CTRY-F43              236-237 
011400                                             PIC X(002).                  
011450                         88  :FTL:-FM-ERRU-US     VALUE 'US'.             
011500                         88  :FTL:-FM-ERRU-CN     VALUE 'CN'.             
011550                 15  :FTL:-FM-MCFS-ERRU-F59-LGTH                  238-239 
011600                                             PIC X(002).                  
011650                 15  :FTL:-FM-MCFS-ERRU-USA-F59.                          
011700                     20  :FTL:-FM-MCFS-ERRU-USA-STATE-F59         240-241 
011750                                             PIC X(002).                  
011800                     20  :FTL:-FM-MCFS-ERRU-USA-CNTY-F59          242-244 
011850                                             PIC X(003).                  
011900                     20  :FTL:-FM-MCFS-ERRU-USA-ZIP-F59           245-253 
011950                                             PIC X(009).                  
012000                 15  :FTL:-FM-MCFS-ERRU-CAN-F59                           
012050                     REDEFINES :FTL:-FM-MCFS-ERRU-USA-F59.                
012100                     20  :FTL:-FM-MCFS-ERRU-CAN-PROV-F59          240-241 
012150                                             PIC X(002).                  
012200                     20  FILLER              PIC X(012).          242-253 
012250                 15  :FTL:-FM-MCFS-ERRU-WRLD-F59                  240-253 
012300                     REDEFINES :FTL:-FM-MCFS-ERRU-USA-F59                 
012350                                             PIC X(014).                  
012400             10  :FTL:-FM-MCFS-ERRV-SVCDATA                               
012450                 REDEFINES :FTL:-FM-MCFS-ERRU-SVCDATA.                    
012500                 15  :FTL:-FM-MCFS-ERRV-MCC  PIC X(004).                  
012550                 15  FILLER                  PIC X(056).          198-253 
012600             10  :FTL:-FM-MCFS-ERRX-ERR-SVCDATA                           
012650                 REDEFINES :FTL:-FM-MCFS-ERRU-SVCDATA.                    
012700                 15  :FTL:-FM-MCFS-ERRX-TERMID PIC X(015).                
012750                 15  FILLER                  PIC X(045).          209-253 
012800             10  :FTL:-FM-MCFS-ERRD-ERR-SVCDATA                           
012850                 REDEFINES :FTL:-FM-MCFS-ERRU-SVCDATA.                    
012900                 15  :FTL:-FM-MCFS-ERRD-TERMID PIC X(015).                
012950                 15  FILLER                  PIC X(045).          209-253 
013000             10  :FTL:-FM-MCFS-ERRA-SVCDATA                               
013050                 REDEFINES :FTL:-FM-MCFS-ERRU-SVCDATA.                    
013100                 15  :FTL:-FM-MCFS-ERRA-TERMID                    194-208 
013150                                             PIC X(015).                  
013200                 15  :FTL:-FM-MCFS-ERRA-VENDID                    209-209 
013250                                             PIC X(01).                   
013300                 15  FILLER                  PIC X(044).          210-253 
013350             10  :FTL:-FM-MCFS-ERRM-ERR-SVCDATA                           
013400                 REDEFINES :FTL:-FM-MCFS-ERRU-SVCDATA.                    
RT1852                 15  :FTL:-FM-MCFS-ERRM-MCC     PIC X(004).       194-197 
RT1852                 15  :FTL:-FM-MCFS-ERRM-POST-CODE PIC X(009).     198-206 
RT1852                 15  :FTL:-FM-MCFS-ERRM-NAME-F43 PIC X(025).      207-253 
RT1852                 15  :FTL:-FM-MCFS-ERRM-CITY-F43 PIC X(013).      207-253 
RT1852                 15  :FTL:-FM-MCFS-ERRM-CTRY-F43 PIC X(002).      207-253 
RT1852                 15  FILLER                  PIC X(007).          207-253 
013750         05  :FTL:-FM-AVS-UPDATE-IND         PIC X(001).          254-254 
013800             88  :FTL:-FM-AVS-IND-ADDED      VALUE 'A'.                   
013850             88  :FTL:-FM-AVS-IND-CHANGED    VALUE 'C'.                   
013900             88  :FTL:-FM-AVS-IND-DELETED    VALUE 'D'.                   
013950             88  :FTL:-FM-AVS-IND-NOTHING    VALUE 'N'.                   
014000         05  :FTL:-FM-PVV-UPDATE-IND         PIC X(001).          255-255 
014050             88  :FTL:-FM-PVV-IND-ADDED      VALUE 'A'.                   
014100             88  :FTL:-FM-PVV-IND-CHANGED    VALUE 'C'.                   
014150             88  :FTL:-FM-PVV-IND-DELETED    VALUE 'D'.                   
014200             88  :FTL:-FM-PVV-IND-NOTHING    VALUE 'N'.                   
014250         05  :FTL:-FM-STATION-PCR            PIC X(004).          551-551 
014300         05  :FTL:-FM-STATION-VAP            PIC X(006).          551-551 
