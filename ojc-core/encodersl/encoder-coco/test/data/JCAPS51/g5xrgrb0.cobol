************************************************************************        
*********            GUARDIAN LOCAL MODIFICATIONS (GLM)                *        
************************************************************************        
*********            TO BE INCLUDED WITH EACH NEW RELEASE              *        
*********  THESE MODS ARE IDENTIFIED IN THE PROGRAM WITH 'GLM#' ...    *        
*********                                                              *        
********                                                               *        
******** GLM#1 7/2/2000 - ADDED OPTION OF 'E' TO GR-OPT4               *        
************************************************************************        
000010****************************************************************          
000020*        GF GROUP RECORD    :    G5XRGRB0                      *          
000030*        KEY LENGTH         :    K-GR-KEY-LENGTH (24)          *          
000040*        RECORD LENGTH      :    K-GR-REC-LENGTH (2000)        *          
000050*                                                              *          
000060*   ONE RECORD PER GROUP.   RECORD CONTAINS GROUP              *          
000070*   INDICATIVE INFORMATION; EFFECTIVE DATES OF VARIOUS         *          
000080*   GROUP EVENTS; AND POLICY NUMBERS.                          *          
000090*                                                              *          
000100*                                                              *          
000110****************************************************************          
000120*    SKIP3                                                                
000130****************************************************************          
000140*         **  PROPRIETARY PROPERTY OF ERISCO, INC.  **         *          
000150*         **        COPYRIGHT 1984 - 1998           **         *          
000160****************************************************************          
000170****************************************************************          
000180*       -----    RELEASE 6.0 MODIFICATION NOTES     -----      *          
000190*                                                              *          
000200*    -   NEW FIELDS:                                           *          
000210*        GR-OPT13         THRU GR-OPT16                        *          
000220*        GR-ELIG-OPT9     THRU GR-ELIG-OPT16                   *          
000230*                                                              *          
000240*    -   CHANGED:                                              *          
000250*        GR-OPT-UNUSED-FIL             FROM 8   TO 4   BYTES   *          
000260*        GR-UNUSED-FIL-3               FROM 110 TO 104 BYTES   *          
000270*        GR-UNUSED-FIL-1               FROM 38  TO 32  BYTES   *          
000280*        GR-RS-FIELD-D                 FROM 7   TO 8   BYTES   *          
000290*        GR-RS-FIELD-E                 FROM 7   TO 8   BYTES   *          
000300*        GR-RS-FIELD-F                 FROM 7   TO 8   BYTES   *          
000310*        GR-RS-FIELD-G                 FROM 7   TO 8   BYTES   *          
000320*        GR-RS-UNUSED-FIL              FROM 15  TO 9   BYTES   *          
000330*        GR-MPP-FIL                    FROM 29  TO 26  BYTES   *          
000340*                                                              *          
000350*    -   CONVERSION NOTES:                                     *          
000360*        ALL DATE FIELDS WERE EXPANDED TO INCLUDE CENTURY      *          
000370*                                                              *          
000380****************************************************************          
000390****************************************************************          
000400*       -----    MODIFICATIONS AFTER RELEASE 6.0    -----      *          
000410****************************************************************          
000420*                                                              *          
000430*                                                              *          
000440*--->REF#002 CHANGES MADE 01/07/98 THRU 02/17/98               *          
000450*    ENH491   GF96                                             *          
000460*     1. ADDED CODE FOR HIPAA REQUIREMENTS.                    *          
000470*                                                8/18/97       *          
000480*    ENH483  SY03 SY07                                         *          
000490*     1. UPDATED FOR RELEASE 6.0                               *          
000500*                                                              *          
000510****************************************************************          
000520****************************************************************          
000530*       -----          RELEASE STAMP                -----      *          
000540*                                                              *          
000550*     G5XRGRB0  REL.6.03  03/24/98  15:12:45                   *          
000560****************************************************************          
000570     05  GR-KEY.                                                          
000580         10  GR-PZ-ID                    PIC X(02).                       
000590             88  GR-GROUPFACTS             VALUE 'GF'.                    
000600         10  GR-CI-ID                    PIC X(02).                       
000610         10  GR-REC-ID                   PIC X(02).                       
000620             88  GR-GROUP-FILE             VALUE 'GR'.                    
000630         10  GR-KEY1.                                                     
000640             15  GR-GR-ID.                                                
000650                 20  GR-GR-ID-PFX        PIC X(02).                       
000660                 20  GR-GR-ID-SFX        PIC X(06).                       
000670             15  GR-DV-ID-FIL            PIC X(04).                       
000680                 88  GR-DV-ID-FIL-VALID    VALUE SPACES.                  
000690             15  GR-SUB-REC-ID           PIC X(02).                       
000700                 88  GR-GROUP-REC        VALUE 'GR'.                      
000710             15  GR-ID-GEN-X.                                             
000720                 20  GR-ID-GEN           PIC 9(02).                       
000730         10  GR-KEY-FIL                  PIC X(02).                       
000740     05  GR-ODO-CTRS             COMP.                                    
000750         10  GR-FILL-ODO-CTR             PIC S9(04).                      
000760         10  GR-CUST-ODO-CTR             PIC S9(04).                      
000770     05  GR-SYSTEM-DATA.                                                  
000780         10  GR-LAST-UPD-DATA.                                            
000790             15  GR-LAST-UPD-ONLINE-DATA.                                 
000800                  20  GR-LAST-UPD-DATE               PIC 9(08).           
000810                  20  GR-LAST-UPD-DATE-X REDEFINES                        
000820                     GR-LAST-UPD-DATE.                                    
000830                      25  GR-LAST-UPD-CC             PIC 9(02).           
000840                      25  GR-LAST-UPD-DT             PIC 9(06).           
000850                      25  GR-LAST-UPD-DT-X                                
000860                          REDEFINES GR-LAST-UPD-DT.                       
000870                          30  GR-LAST-UPD-YY         PIC X(02).           
000880                          30  GR-LAST-UPD-MM         PIC X(02).           
000890                          30  GR-LAST-UPD-DD         PIC X(02).           
000900                 20  GR-GF-LAST-UPD-DT                                    
000910                      REDEFINES GR-LAST-UPD-DATE     PIC 9(08).           
000920                 20  GR-GF-LAST-UPD-DT-X                                  
000930                     REDEFINES GR-LAST-UPD-DATE.                          
000940                     25  GR-GF-LAST-UPD-YY          PIC 9(04).            
000950                     25  GR-GF-LAST-UPD-YY-X                              
000960                         REDEFINES GR-GF-LAST-UPD-YY.                     
000970                         30  GR-GF-LAST-UPD-CC      PIC 9(02).            
000980                         30  GR-GF-LAST-UPD-YY2     PIC 9(02).            
000990                     25  GR-GF-LAST-UPD-MM          PIC 9(02).            
001000                     25  GR-GF-LAST-UPD-DD          PIC 9(02).            
001010                 20  GR-LAST-UPD-TIME               PIC 9(06).            
001020                 20  GR-LAST-UPD-TIME-X                                   
001030                     REDEFINES GR-LAST-UPD-TIME.                          
001040                     25  GR-LAST-UPD-TIME-HH        PIC 9(02).            
001050                     25  GR-LAST-UPD-TIME-MM        PIC 9(02).            
001060                     25  GR-LAST-UPD-TIME-SS        PIC 9(02).            
001070                 20  GR-LAST-UPD-US-ID              PIC X(10).            
001080                 20  GR-LOCK-SW                     PIC X(01).            
001090                 20  GR-LAST-UPD-PC-ID              PIC X(02).            
001100                 20  GR-LAST-UPD-REL                PIC X(03).            
001110             15  GR-LAST-UPD-BATCH-DATA.                                  
001120                 20  GR-LAST-UPD-BATCH-DT           PIC 9(08).            
001130                 20  GR-LAST-UPD-BATCH-DT-X                               
001140                     REDEFINES GR-LAST-UPD-BATCH-DT.                      
001150                     25  GR-LAST-UPD-BATCH-YY       PIC 9(04).            
001160                     25  GR-LAST-UPD-BATCH-YY-X                           
001170                         REDEFINES GR-LAST-UPD-BATCH-YY.                  
001180                         30  GR-LAST-UPD-BATCH-CC   PIC 9(02).            
001190                         30  GR-LAST-UPD-BATCH-YY2  PIC 9(02).            
001200                     25  GR-LAST-UPD-BATCH-MM       PIC 9(02).            
001210                     25  GR-LAST-UPD-BATCH-DD       PIC 9(02).            
001220                 20  GR-LAST-UPD-BATCH-PROG-ID      PIC X(08).            
001230                 20  GR-LAST-UPD-BATCH-FIL          PIC X(04).            
001240         10  GR-USER-AREA-TBL.                                            
001250             15  GR-USER-AREA-ITEM       PIC X(10)  OCCURS 3.             
001260     05  GR-INDIC-DATA.                                                   
001270         10  GR-EFF-LIM-DT-X.                                             
001280             15  GR-EFF-DT.                                               
001290                 20  GR-E-YY.                                             
001300                     25  GR-E-CC         PIC X(02).                       
001310                     25  GR-E-YY2        PIC X(02).                       
001320                 20  GR-E-MM             PIC X(02).                       
001330                 20  GR-E-DD             PIC X(02).                       
001340             15  GR-GEN-EFF-DATE                                          
001350                 REDEFINES GR-EFF-DT     PIC X(08).                       
001360             15  GR-LIMIT-DT.                                             
001370                 20  GR-L-YY.                                             
001380                     25  GR-L-CC         PIC X(02).                       
001390                     25  GR-L-YY2        PIC X(02).                       
001400                 20  GR-L-MM             PIC X(02).                       
001410                 20  GR-L-DD             PIC X(02).                       
001420             15  GR-GEN-LIMIT-DATE                                        
001430                 REDEFINES GR-LIMIT-DT   PIC X(08).                       
001440         10  GR-GEN-ID                   PIC X(12).                       
001450         10  GR-MEMO                     PIC X(70).                       
001460         10  GR-OPTIONS.                                                  
001470             15  GR-OPT1                 PIC X(01).                       
001480                 88  GR-OPT1-VALID         VALUES ' ' 'N' 'Y'.            
001490                 88  GR-CLAIM-NO           VALUE  ' ' 'N'.                
001500                 88  GR-CLAIM-YES          VALUE  'Y'.                    
001510             15  GR-OPT2                 PIC X(01).                       
001520                 88  GR-OPT2-VALID         VALUES ' ' 'G' 'D'.            
001530                 88  GR-XP-NONE            VALUE  ' '.                    
001540                 88  GR-XP-GROUP           VALUE  'G'.                    
001550                 88  GR-XP-DIV             VALUE  'D'.                    
001560             15  GR-OPT3                 PIC X(01).                       
001570                 88  GR-OPT3-VALID         VALUES ' ' 'Y' 'N' 'B'.        
001580                 88  GR-RETN-UNUSED        VALUE  ' '.                    
001590                 88  GR-RETN-YES           VALUE  'Y'.                    
001600                 88  GR-RETN-NO            VALUE  'N'.                    
001610                 88  GR-RETN-BY-BN         VALUE  'B'.                    
001620             15  GR-OPT4                 PIC X(01).                       
001630                 88  GR-OPT4-VALID           VALUE ' ' 'G' 'N'            
001640                                                   'P' 'Y'                
GLM#1                                                    'E'.                   
GLM#1 *                88  GR-CF-NO-PAID-THRU      VALUE ' ' 'N'.               
GLM#1                  88  GR-CF-NO-PAID-THRU      VALUE ' ' 'N' 'E'.           
001660                 88  GR-CF-PAID-THRU-ONLY    VALUE 'P'.                   
001670                 88  GR-CF-PAID-THRU-N-GRACE VALUE 'Y' 'G'.               
001680                 88  GR-CF-GRACE-ON-NO-PAY   VALUE 'G'.                   
001690             15  GR-OPT5                 PIC X(01).                       
001700                 88  GR-OPT5-VALID         VALUES ' ' 'G' 'N'.            
001710                 88  GR-OPT5-UNUSED        VALUE  ' '.                    
001720                 88  GR-OPT5-GEN-CERTS     VALUE  'G'.                    
001730                 88  GR-OPT5-NO-GEN-CERTS  VALUE  'N'.                    
001740             15  GR-OPT6                 PIC X(01).                       
001750                 88  GR-OPT6-VALID         VALUES ' ' 'E' 'F'.            
001760                 88  GR-ENGLISH-VERSION    VALUES ' ' 'E'.                
001770                 88  GR-FRENCH-VERSION     VALUE  'F'.                    
001780             15  GR-OPT7                 PIC X(01).                       
001790                 88  GR-OPT7-VALID         VALUES ' ' 'C' 'I'             
001800                                                  'N' 'S'.                
001810                 88  GR-OPT7-GX-USE-CI     VALUES ' ' 'C'.                
001820                 88  GR-OPT7-GX-NOT-USED   VALUE  'N'.                    
001830                 88  GR-OPT7-GX-BY-MMID    VALUE  'I'.                    
001840                 88  GR-OPT7-GX-BY-SSN     VALUE  'S'.                    
001850             15  GR-OPT8                 PIC X(01).                       
001860                 88  GR-OPT8-VALID         VALUES ' ' 'B' 'H'.            
001870                 88  GR-APRVL-CI-DEFAULT   VALUE  ' '.                    
001880                 88  GR-APRVL-BAL-FWD      VALUE  'B'.                    
001890                 88  GR-APRVL-HOLD         VALUE  'H'.                    
001900             15  GR-OPT9                 PIC X(01).                       
001910                 88  GR-OPT9-VALID         VALUES ' ' 'B' 'C' 'G'.        
001920                 88  GR-OPT9-UPD-GROUPFACTS VALUE  ' ' 'G'.               
001930                 88  GR-OPT9-UPD-CLAIMFACTS VALUE  'C'.                   
001940                 88  GR-OPT9-UPD-BOTH-CF-GF VALUE  'B'.                   
001950             15  GR-OPT10                PIC X(01).                       
001960                 88  GR-OPT10-VALID        VALUES ' ' 'G' 'N'.            
001970                 88  GR-OPT10-DEFAULT-CI   VALUES  ' '.                   
001980                 88  GR-OPT10-GR-LST-PD    VALUES  'G'.                   
001990                 88  GR-OPT10-GR-LST-PD-NO VALUES  'N'.                   
002000             15  GR-OPT11                PIC X(01).                       
002010                 88  GR-OPT11-VALID         VALUES ' ' 'G' 'N'.           
002020                 88  GR-OPT11-UNUSED        VALUE  ' '.                   
002030                 88  GR-OPT11-GEN-CERTS     VALUE  'G'.                   
002040                 88  GR-OPT11-NO-GEN-CERTS  VALUE  'N'.                   
002050             15  GR-OPT12                PIC X(01).                       
002060                 88  GR-OPT12-VALID        VALUES ' '.                    
002070             15  GR-OPT13                PIC X(01).                       
002080                 88  GR-OPT13-VALID        VALUES ' '.                    
002090             15  GR-OPT14                PIC X(01).                       
002100                 88  GR-OPT14-VALID        VALUES ' '.                    
002110             15  GR-OPT15                PIC X(01).                       
002120                 88  GR-OPT15-VALID        VALUES ' '.                    
002130             15  GR-OPT16                PIC X(01).                       
002140                 88  GR-OPT16-VALID        VALUES ' '.                    
002150             15  GR-OPT-UNUSED-FIL       PIC X(04).                       
002160         10  GR-ELIG-OPTIONS.                                             
002170             15  GR-ELIG-OPT1            PIC X(01).                       
002180                 88  GR-ELIG-OPT1-VALID     VALUES ' ' 'E'                
002190                                                   'M' 'S'.               
002200                 88  GR-E01-CF-MM-IN-MM-ID  VALUE  ' ' 'M'.               
002210                 88  GR-E01-CF-MM-IN-EMP-ID VALUE  'E'.                   
002220                 88  GR-E01-CF-MM-IN-SSN    VALUE  'S'.                   
002230             15  GR-ELIG-OPT2            PIC X(01).                       
002240                 88  GR-ELIG-OPT2-VALID      VALUES ' ' 'C' 'G'.          
002250                 88  GR-ELIG-ON-GROUP-FACTS  VALUES ' ' 'G'.              
002260                 88  GR-ELIG-ON-CLAIM-FACTS  VALUE  'C'.                  
002270             15  GR-ELIG-OPT3            PIC X(01).                       
002280                 88  GR-ELIG-OPT3-VALID    VALUE ' ' 'Z'.                 
002290                 88  GR-ELIG-UNUSED        VALUE ' '.                     
002300                 88  GR-ELIG-MM-ZIP-REQD   VALUE 'Z'.                     
002310             15  GR-ELIG-OPT4            PIC X(01).                       
002320                 88  GR-ELIG-OPT4-VALID    VALUES ' ' 'D'.                
002330                 88  GR-ELIG-OPT4-CF-DEP   VALUES 'D'.                    
002340             15  GR-ELIG-OPT5            PIC X(01).                       
002350                 88  GR-ELIG-OPT5-VALID    VALUES ' ' 'C' 'D'.            
002360                 88  GR-ELIG-OPT5-DOB      VALUES ' '.                    
002370                 88  GR-ELIG-OPT5-CF-AI    VALUES 'C'.                    
002380                 88  GR-ELIG-OPT5-DP-CF-AI VALUES 'D'.                    
002390             15  GR-ELIG-OPT6            PIC X(01).                       
002400                 88  GR-ELIG-OPT6-VALID    VALUES ' ' 'A'.                
002410                 88  GR-ELIG-OPT6-ADDR     VALUES ' '.                    
002420                 88  GR-ELIG-OPT6-ADDR-OPT VALUES 'A'.                    
002430             15  GR-ELIG-OPT7            PIC X(01).                       
002440                 88  GR-ELIG-OPT7-VALID    VALUES ' ' 'C'.                
002450                 88  GR-ELIG-OPT7-CHECK-CF VALUES 'C'.                    
002460             15  GR-ELIG-OPT8            PIC X(01).                       
002470                 88  GR-ELIG-OPT8-VALID    VALUES ' ' 'N' 'P'.            
002480                 88  GR-ELIG-OPT8-PCP      VALUES 'P'.                    
002490             15  GR-ELIG-OPT9            PIC X(01).                       
002500                 88  GR-ELIG-OPT9-VALID    VALUES ' '.                    
002510             15  GR-ELIG-OPT10           PIC X(01).                       
002520                 88  GR-ELIG-OPT10-VALID   VALUES ' '.                    
002530             15  GR-ELIG-OPT11           PIC X(01).                       
002540                 88  GR-ELIG-OPT11-VALID   VALUES ' '.                    
002550             15  GR-ELIG-OPT12           PIC X(01).                       
002560                 88  GR-ELIG-OPT12-VALID   VALUES ' '.                    
002570             15  GR-ELIG-OPT13           PIC X(01).                       
002580                 88  GR-ELIG-OPT13-VALID   VALUES ' '.                    
002590             15  GR-ELIG-OPT14           PIC X(01).                       
002600                 88  GR-ELIG-OPT14-VALID   VALUES ' '.                    
002610             15  GR-ELIG-OPT15           PIC X(01).                       
002620                 88  GR-ELIG-OPT15-VALID   VALUES ' '.                    
002630             15  GR-ELIG-OPT16           PIC X(01).                       
002640                 88  GR-ELIG-OPT16-VALID   VALUES ' '.                    
002650         10  GR-NAME-N-ADDR.                                              
002660             15  GR-AD-OPTIONS.                                           
002670                 20  GR-AD-OPT1          PIC X(01).                       
002680                     88  GR-AD-OPT1-VALID     VALUE ' ' 'B' 'C'           
002690                                                    'U'.                  
002700                     88  GR-AD-ADDR-USA       VALUE ' ' 'U'.              
002710                     88  GR-AD-ADDR-BRITISH   VALUE 'B'.                  
002720                     88  GR-AD-ADDR-CANADIAN  VALUE 'C'.                  
002730                 20  GR-AD-OPT2          PIC X(01).                       
002740                     88  GR-AD-OPT2-VALID     VALUE ' '.                  
002750                 20  GR-AD-OPT3          PIC X(01).                       
002760                     88  GR-AD-OPT3-VALID     VALUE ' '.                  
002770                 20  GR-AD-OPT4          PIC X(01).                       
002780                     88  GR-AD-OPT4-VALID     VALUE ' '.                  
002790                 20  GR-AD-OPT-FIL       PIC X(04).                       
002800             15  GR-NAME.                                                 
002810                 20  GR-NAME1            PIC X(30).                       
002820                 20  GR-NAME2            PIC X(30).                       
002830             15  GR-CONTACT              PIC X(30).                       
002840             15  GR-ADDR.                                                 
002850                 20  GR-ADDR1            PIC X(30).                       
002860                 20  GR-ADDR2            PIC X(30).                       
002870                 20  GR-ADDR3            PIC X(30).                       
002880                 20  GR-ADDR4.                                            
002890                     25  GR-CITY         PIC X(19).                       
002900                     25  GR-STATE        PIC X(02).                       
002910                     25  GR-ZIP          PIC X(11).                       
002920                 20  GR-COUNTRY          PIC X(03).                       
002930         10  GR-PHONE.                                                    
002940             15  FILLER                  PIC X(01).                       
002950             15  GR-PHONE-AREA           PIC X(03).                       
002960             15  GR-PHONE-EXCHANGE       PIC X(03).                       
002970             15  GR-PHONE-NUMBER         PIC X(04).                       
002980             15  GR-PHONE-FIL            PIC X(04).                       
002990         10  GR-GP-TBL.                                                   
003000             15  GR-GP-ITEM  OCCURS 3 TIMES                               
003010                             INDEXED BY GR-GP-IDX.                        
003020                 20  GR-GP-CHK-NO          PIC X(08).                     
003030                 20  GR-GP-CHK-AMT         PIC S9(07)V99  COMP-3.         
003040                 20  GR-GP-CHK-APP-ADJ-AMT PIC S9(07)V99  COMP-3.         
003050                 20  GR-GP-CHK-DT.                                        
003060                     25  GR-GP-CHK-YY.                                    
003070                         30  GR-GP-CHK-CC  PIC X(02).                     
003080                         30  GR-GP-CHK-YY2 PIC X(02).                     
003090                     25  GR-GP-CHK-MM      PIC X(02).                     
003100                     25  GR-GP-CHK-DD      PIC X(02).                     
003110                 20  GR-GP-ACT-CD          PIC X(01).                     
003120         10  GR-UNUSED-FIL-3             PIC X(103).                      
003130         10  GR-KEY-BUILDING-CODES.                                       
003140             15  GR-BIL-RP-VERSION       PIC X(02).                       
003150             15  GR-RN-PTR               PIC X(08).                       
003160             15  GR-TD-PTR               PIC X(13).                       
003170             15  GR-ALT-ADDR-PTRS.                                        
003180                 20  GR-ALT-ADDR1        PIC X(02).                       
003190                 20  GR-ALT-ADDR2        PIC X(02).                       
003200                 20  GR-ALT-ADDR3        PIC X(02).                       
003210             15  GR-KEY-BLD-UNUSED-FIL   PIC X(30).                       
003220         10  GR-CODES.                                                    
003230             15  GR-TYPE-CD              PIC X(01).                       
003240                 88  GR-TYPE-CD-VALID      VALUE 'A' 'C' 'I' 'M'          
003250                                                 'P' 'S' 'T' 'O'.         
003260                 88  GR-TYPE-ASSOC         VALUE 'A'.                     
003270                 88  GR-TYPE-IND           VALUE 'I'.                     
003280                 88  GR-TYPE-MET           VALUE 'M'.                     
003290                 88  GR-TYPE-OTHER         VALUE 'O'.                     
003300                 88  GR-TYPE-MPP           VALUE 'P'.                     
003310                 88  GR-TYPE-ASO           VALUE 'S'.                     
003320                 88  GR-TYPE-COMB-ASO-PLUS VALUE 'C'.                     
003330                 88  GR-TYPE-TRUE          VALUE 'T'.                     
003340             15  GR-ADMIN-CD             PIC X(01).                       
003350                 88  GR-ADMIN-CD-VALID     VALUE '0' THRU '4'.            
003360                 88  GR-ADMIN-NONE         VALUE '0'.                     
003370                 88  GR-ADMIN-HO-GRS       VALUE '1'.                     
003380                 88  GR-ADMIN-HO-NET       VALUE '2'.                     
003390                 88  GR-ADMIN-SA-GRS       VALUE '3'.                     
003400                 88  GR-ADMIN-SA-NET       VALUE '4'.                     
003410             15  GR-CS-CD                PIC X(01).                       
003420                 88  GR-CS-CD-VALID        VALUE 'G' 'D'.                 
003430                 88  GR-CS-CD-GROUP        VALUE 'G'.                     
003440                 88  GR-CS-CD-DIV          VALUE 'D'.                     
003450             15  GR-STS-CD               PIC X(01).                       
003460                 88  GR-STS-CD-VALID       VALUE 'A' 'P' 'T'.             
003470                 88  GR-STS-ACTIVE         VALUE 'A'.                     
003480                 88  GR-STS-PENDED         VALUE 'P'.                     
003490                 88  GR-STS-TERMN8         VALUE 'T'.                     
003500             15  GR-DVDND-ELIG-CD        PIC X(01).                       
003510                 88  GR-DVDND-ELIG-CD-VALID VALUE ' ' 'N' 'Y'.            
003520                 88  GR-DVDND-ELIG-NO       VALUE ' ' 'N'.                
003530                 88  GR-DVDND-ELIG-YES      VALUE 'Y'.                    
003540             15  GR-AUTO-CERT-ASSIGN-CD  PIC X(01).                       
003550                 88  GR-AUTO-CERT-CD-VALID VALUE ' ' 'N' 'Y'.             
003560                 88  GR-AUTO-CERT-NO       VALUE ' ' 'N'.                 
003570                 88  GR-AUTO-CERT-YES      VALUE 'Y'.                     
003580             15  GR-POOL-CD              PIC X(02).                       
003590             15  GR-STATE-CD             PIC X(02).                       
003600             15  GR-UND-ID               PIC X(02).                       
003610             15  GR-AGE-CHANGE-CD        PIC X(01).                       
003620                 88  GR-AC-CD-VALID         VALUE ' ' 'A' 'B' 'E'         
003630                                                  'F' 'H' 'I' 'J'         
003640                                                  'M' 'N' 'R' 'Y'.        
003650                 88  GR-AC-DOB              VALUE ' ' 'B'.                
003660                 88  GR-AC-ANNIV            VALUE 'A'.                    
003670                 88  GR-AC-NRST-POL-ANNIV   VALUE 'E'.                    
003680                 88  GR-AC-1ST-OF-MO-OF-DOB VALUE 'F'.                    
003690                 88  GR-AC-NRST-MM-ANNIV    VALUE 'H'.                    
003700                 88  GR-AC-EFF-INIT-RENEW   VALUE 'I'.                    
003710                 88  GR-AC-AS-OF-DUE-DATE   VALUE 'J'.                    
003720                 88  GR-AC-ANNYR-YOB        VALUE 'M'.                    
003730                 88  GR-AC-NEAREST-BDAY     VALUE 'N'.                    
003740                 88  GR-AC-RENEWAL-DATE     VALUE 'R'.                    
003750                 88  GR-AC-YEAR-OF-BIRTH    VALUE 'Y'.                    
003760             15  GR-RN-CESSION-NO        PIC X(08).                       
003770             15  GR-POL-PER-MONTHS       PIC S9(03) COMP-3.               
003780             15  GR-TERMN8-RSN           PIC X(02).                       
003790             15  GR-MARKET-DATA.                                          
003800                 20  GR-MD-FIELD-OFF     PIC X(03).                       
003810                 20  GR-MD-REP           PIC X(04).                       
003820                 20  GR-MD-PROD          PIC X(09).                       
003830             15  GR-DISPUTE-IND          PIC X(01).                       
003840                 88  GR-DISPUTE-VALID      VALUE ' ' 'N' 'D'              
003850                                                 'S' 'C' 'O'.             
003860                 88  GR-DISPUTE-EXISTS     VALUE 'D' 'S' 'C' 'O'.         
003870                 88  GR-DISPUTE-REMIT      VALUE 'D' 'S'.                 
003880                 88  GR-DISPUTE-COMM       VALUE 'C' 'O'.                 
003890                 88  GR-DISPUTE-NONE       VALUE 'N' ' '.                 
003900             15  GR-NG-CHECK             PIC X(01).                       
003910                 88  GR-NG-CHECK-VALID     VALUE ' ' 'N' 'Y'.             
003920                 88  GR-NG-CHECK-RCVD      VALUE 'Y'.                     
003930         10  GR-MIN-PTC-IND              PIC X(01).                       
003940             88  GR-MIN-PTC-IND-VALID      VALUE ' ' 'A' 'B'.             
003950             88  GR-MIN-PTC-IND-ABOVE      VALUE ' ' 'A'.                 
003960             88  GR-MIN-PTC-IND-BELOW      VALUE 'B'.                     
003970         10  GR-CLERK-CD                 PIC X(03).                       
003980         10  GR-REG-OFF-CD               PIC X(04).                       
003990         10  GR-SVC-OFF-CD               PIC X(04).                       
004000         10  GR-PLAN-TYPE                PIC X(01).                       
004010         10  GR-MULT-DV-IND              PIC X(01).                       
004020             88  GR-MULT-DV-VALID          VALUE ' ' 'Y' 'N'.             
004030             88  GR-MULT-DV                VALUE 'Y'.                     
004040         10  GR-SMRY                     PIC X(01).                       
004050             88  GR-SMRY-VALID             VALUE 'Y' ' '.                 
004060             88  GR-SMRY-PRINT             VALUE 'Y'.                     
004070             88  GR-SMRY-NO-PRINT          VALUE ' '.                     
004080         10  GR-UNUSED-FIL-2             PIC X(23).                       
004090         10  GR-POLICIES     OCCURS 10 TIMES                              
004100                             INDEXED BY GR-POL-IDX.                       
004110             15  GR-POL                  PIC X(09).                       
004120         10  GR-DATES.                                                    
004130             15  GR-ORIG-EFF-DT.                                          
004140                 20  GR-ORIG-EFF-YY.                                      
004150                     25  GR-ORIG-EFF-CC          PIC X(02).               
004160                     25  GR-ORIG-EFF-YY2         PIC X(02).               
004170                 20  GR-ORIG-EFF-MM              PIC X(02).               
004180                 20  GR-ORIG-EFF-DD              PIC X(02).               
004190             15  GR-LAST-ANNIV-DT.                                        
004200                 20  GR-LAST-ANNIV-YY.                                    
004210                     25  GR-LAST-ANNIV-CC        PIC X(02).               
004220                     25  GR-LAST-ANNIV-YY2       PIC X(02).               
004230                 20  GR-LAST-ANNIV-MM            PIC X(02).               
004240                 20  GR-LAST-ANNIV-DD            PIC X(02).               
004250             15  GR-CURR-ANNIV-DT.                                        
004260                 20  GR-CURR-ANNIV-YY.                                    
004270                     25  GR-CURR-ANNIV-CC        PIC X(02).               
004280                     25  GR-CURR-ANNIV-YY2       PIC X(02).               
004290                 20  GR-CURR-ANNIV-MM-DD.                                 
004300                     25  GR-CURR-ANNIV-MM        PIC X(02).               
004310                     25  GR-CURR-ANNIV-DD        PIC X(02).               
004320             15  GR-NEXT-ANNIV-DT.                                        
004330                 20  GR-NEXT-ANNIV-YY.                                    
004340                     25  GR-NEXT-ANNIV-CC        PIC X(02).               
004350                     25  GR-NEXT-ANNIV-YY2       PIC X(02).               
004360                 20  GR-NEXT-ANNIV-MM            PIC X(02).               
004370                 20  GR-NEXT-ANNIV-DD            PIC X(02).               
004380             15  GR-RENEW-RPT-DT.                                         
004390                 20  GR-RENEW-RPT-YY.                                     
004400                     25  GR-RENEW-RPT-CC         PIC X(02).               
004410                     25  GR-RENEW-RPT-YY2        PIC X(02).               
004420                 20  GR-RENEW-RPT-MM             PIC X(02).               
004430                 20  GR-RENEW-RPT-DD             PIC X(02).               
004440             15  GR-TERMN8-DT.                                            
004450                 20  GR-TERMN8-YY.                                        
004460                     25  GR-TERMN8-CC            PIC X(02).               
004470                     25  GR-TERMN8-YY2           PIC X(02).               
004480                 20  GR-TERMN8-MM                PIC X(02).               
004490                 20  GR-TERMN8-DD                PIC X(02).               
004500             15  GR-REINSTATE-DT.                                         
004510                 20  GR-REINSTATE-YY.                                     
004520                     25  GR-REINSTATE-CC         PIC X(02).               
004530                     25  GR-REINSTATE-YY2        PIC X(02).               
004540                 20  GR-REINSTATE-MM             PIC X(02).               
004550                 20  GR-REINSTATE-DD             PIC X(02).               
004560             15  GR-PURGE-DT.                                             
004570                 20  GR-PURGE-YY.                                         
004580                     25  GR-PURGE-CC             PIC X(02).               
004590                     25  GR-PURGE-YY2            PIC X(02).               
004600                 20  GR-PURGE-MM                 PIC X(02).               
004610                 20  GR-PURGE-DD                 PIC X(02).               
004620             15  GR-INFRC-EFF-DT.                                         
004630                 20  GR-INFRC-EFF-YY.                                     
004640                     25  GR-INFRC-EFF-CC         PIC X(02).               
004650                     25  GR-INFRC-EFF-YY2        PIC X(02).               
004660                 20  GR-INFRC-EFF-MM             PIC X(02).               
004670                 20  GR-INFRC-EFF-DD             PIC X(02).               
004680             15  GR-INFRC-TERMN8-DT.                                      
004690                 20  GR-INFRC-TERMN8-YY.                                  
004700                     25  GR-INFRC-TERMN8-CC      PIC X(02).               
004710                     25  GR-INFRC-TERMN8-YY2     PIC X(02).               
004720                 20  GR-INFRC-TERMN8-MM          PIC X(02).               
004730                 20  GR-INFRC-TERMN8-DD          PIC X(02).               
004740             15  GR-INFRC-REINSTATE-DT.                                   
004750                 20  GR-INFRC-REINSTATE-YY.                               
004760                     25  GR-INFRC-REINSTATE-CC   PIC X(02).               
004770                     25  GR-INFRC-REINSTATE-YY2  PIC X(02).               
004780                 20  GR-INFRC-REINSTATE-MM       PIC X(02).               
004790                 20  GR-INFRC-REINSTATE-DD       PIC X(02).               
004800             15  GR-RENEW-DT.                                             
004810                 20  GR-RENEW-MM                 PIC X(02).               
004820                 20  GR-RENEW-DD                 PIC X(02).               
004830             15  GR-LAST-PD-DT.                                           
004840                 20  GR-LAST-PD-YY.                                       
004850                     25  GR-LAST-PD-CC           PIC X(02).               
004860                     25  GR-LAST-PD-YY2          PIC X(02).               
004870                 20  GR-LAST-PD-MM               PIC X(02).               
004880                 20  GR-LAST-PD-DD               PIC X(02).               
004890             15  GR-UNUSED-FIL-1                 PIC X(32).               
004900****************************************************************          
004910*            REPORT SELECTION SECTION                          *          
004920****************************************************************          
004930     05  GR-RS-TBL.                                                       
004940         10  GR-RS-ITEM      OCCURS 6 TIMES                               
004950                             INDEXED BY GR-RS-IDX.                        
004960             15  GR-RS-RPT-TYPE          PIC X(03).                       
004970             15  GR-RS-VERSION           PIC X(02).                       
004980             15  GR-RS-OPTIONS.                                           
004990                 20  GR-RS-OPT1          PIC X(01).                       
005000                     88  GR-RS-OPT1-VALID    VALUE ' '.                   
005010                 20  GR-RS-OPT2          PIC X(01).                       
005020                     88  GR-RS-OPT2-VALID    VALUE ' '.                   
005030                 20  GR-RS-OPT3          PIC X(01).                       
005040                     88  GR-RS-OPT3-VALID    VALUE ' '.                   
005050                 20  GR-RS-OPT4          PIC X(01).                       
005060                     88  GR-RS-OPT4-VALID    VALUE ' '.                   
005070                 20  GR-RS-OPT5          PIC X(01).                       
005080                     88  GR-RS-OPT5-VALID    VALUE ' '.                   
005090                 20  GR-RS-OPT6          PIC X(01).                       
005100                     88  GR-RS-OPT6-VALID    VALUE ' '.                   
005110                 20  GR-RS-OPT7          PIC X(01).                       
005120                     88  GR-RS-OPT7-VALID    VALUE ' '.                   
005130                 20  GR-RS-OPT8          PIC X(01).                       
005140                     88  GR-RS-OPT8-VALID    VALUE ' '.                   
005150             15  GR-RS-REQ-DT.                                            
005160                 20  GR-RS-REQ-YY.                                        
005170                     25  GR-RS-REQ-CC    PIC X(02).                       
005180                     25  GR-RS-REQ-YY2   PIC X(02).                       
005190                 20  GR-RS-REQ-MM        PIC X(02).                       
005200                 20  GR-RS-REQ-DD        PIC X(02).                       
005210             15  GR-RS-FIELD-A           PIC X(02).                       
005220             15  GR-RS-FIELD-B           PIC X(02).                       
005230             15  GR-RS-FIELD-C           PIC X(03).                       
005240             15  GR-RS-FIELD-D           PIC X(08).                       
005250             15  GR-RS-FIELD-E           PIC X(08).                       
005260             15  GR-RS-FIELD-F           PIC X(08).                       
005270             15  GR-RS-FIELD-G           PIC X(08).                       
005280             15  GR-RS-UNUSED-FIL        PIC X(09).                       
005290     05  GR-MPP-TBL.                                                      
005300         10  GR-MPP-ITEM      OCCURS 10 TIMES                             
005310                              INDEXED BY GR-MPP-IDX.                      
005320             15  GR-MPP-BN-PL.                                            
005330                 20  GR-MPP-BN-ID           PIC X(02).                    
005340                 20  GR-MPP-PLAN            PIC X(06).                    
005350             15  GR-MPP-OPTIONS.                                          
005360                 20  GR-MPP-OPT1            PIC X(01).                    
005370                     88  GR-MPP-OPT1-VALID    VALUES ' ' 'A' 'C'          
005380                                                     'F' 'M' 'N'.         
005390                     88  GR-MPP-OPT1-UNUSED           VALUE ' '.          
005400                     88  GR-MPP-OPT1-ANNUAL           VALUE 'A'.          
005410                     88  GR-MPP-OPT1-MTHLY-CARRY      VALUE 'M'.          
005420                     88  GR-MPP-OPT1-MTHLY-NO-CARRY   VALUE 'N'.          
005430                     88  GR-MPP-OPT1-BILL-CLAIMS-FEES VALUE 'C'.          
005440                     88  GR-MPP-OPT1-BILL-FEES        VALUE 'F'.          
005450                 20  GR-MPP-OPT2            PIC X(01).                    
005460                     88  GR-MPP-OPT2-VALID    VALUES ' ' 'M' 'B'.         
005470                     88  GR-MPP-OPT2-UNUSED   VALUES ' '.                 
005480                     88  GR-MPP-OPT2-MIN      VALUES 'M'.                 
005490                     88  GR-MPP-OPT2-BOTH     VALUES 'B'.                 
005500                 20  GR-MPP-OPT3            PIC X(01).                    
005510                     88  GR-MPP-OPT3-VALID    VALUES ' ' 'M' 'C'          
005520                                                     'P' 'I'.             
005530                     88  GR-MPP-OPT3-UNUSED          VALUES ' '.          
005540                     88  GR-MPP-OPT3-CM-CONV         VALUES 'C'.          
005550                     88  GR-MPP-OPT3-CM-MP           VALUES 'M'.          
005560                     88  GR-MPP-OPT3-CM-PER-CLAIM    VALUES 'P'.          
005570                     88  GR-MPP-OPT3-CM-PER-MM       VALUES 'I'.          
005580                 20  GR-MPP-OPT4            PIC X(01).                    
005590                     88  GR-MPP-OPT4-VALID    VALUES ' ' 'N' 'S'.         
005600                     88  GR-MPP-OPT4-UNUSED          VALUES ' '.          
005610                     88  GR-MPP-OPT4-NO-STOP-LOSS    VALUES 'N'.          
005620                     88  GR-MPP-OPT4-STOP-LOSS       VALUES 'S'.          
005630             15  GR-MPP-UNUSED-FIL1         PIC X(8).                     
005640             15  GR-MPP-STOP-LOSS-AMT       PIC S9(07)V99 COMP-3.         
005650             15  GR-MPP-UNUSED-AMT1         PIC S9(09)    COMP-3.         
005660             15  GR-MPP-FIL                 PIC X(26).                    
005670****************************************************************          
005680*       END OF **  G 5 X R G R B 0  **                         *          
005690****************************************************************          
