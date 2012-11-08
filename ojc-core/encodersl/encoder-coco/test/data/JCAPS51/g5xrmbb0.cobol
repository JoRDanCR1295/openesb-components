000010****************************************************************          
000020*    GF MEMBER BILLING RECORD  :    G5XRMBB0                   *          
000030*    KEY LENGTH                :    K-MB-KEY-LENGTH (32)       *          
000040*    RECORD LENGTH             :    K-MB-REC-LENGTH (120-      *          
000050*                                                    11950)    *          
000060****************************************************************          
000070*   RECORD CONTAINS MEMBER BILLING ITEMS. THE REVISED          *          
000080*   BILLING WILL CONTAIN A TYPE CODE TO INDICATED THE DATA     *          
000090*   IN THE ITEM, WHICH COULD BE BILLED, ADJUSTED, UNLOADED,    *          
000100*   REINSURANCE OR COMMISSIONS. MULTIPLE RECORDS CAN BE        *          
000110*   PRESENT.                                                   *          
000120*                                                              *          
000130*   ANY CHANGES MADE TO G5XRMBBN SHOULD BE SYNCHRONIZED WITH   *          
000140*   G5XRMBB0.                                                  *          
000150*                                                              *          
000160*    NOTE: THIS IS THE  FIXED   VERSION OF THE MEMBER          *          
000170*          BILLING  RECORD.  CHANGES TO THIS VERSION OF THE    *          
000180*          RECORD MAY REQUIRE CHANGE TO THE  COPYBOOKS  FOR    *          
000190*          THE   COMPRESSED   VERSIONS   OF   THE   RECORD.    *          
000200*          (G5XRMBR0/G5XRMBW0).                                *          
000210*                                                              *          
000220*        THIS COPYBOOK SHOULD BE USED IN CONJUNCTION WITH:     *          
000230*        'G5XRMBR0'            (READ)                          *          
000240*        'G5XRMBW0'            (WRITE)                         *          
000250*        'G5XKMBB0'            (KEY)                           *          
000260*                                                              *          
000270*                                                              *          
000280****************************************************************          
000290*     SKIP2                                                                
000300****************************************************************          
000310*         **  PROPRIETARY PROPERTY OF ERISCO, INC.  **         *          
000320*         **        COPYRIGHT 1984 - 1997           **         *          
000330****************************************************************          
000340*     SKIP2                                                                
000350****************************************************************          
000360*       -----    RELEASE 6.0 MODIFICATION NOTES     -----      *          
000370*                                                              *          
000380*    -   REC LENGTH CHANGED FROM 120-11950 TO 250-11950        *          
000390*                                                              *          
000400*    -   NEW FIELDS:                                           *          
000410*        MB-BIL-COV-PYRL-VOL                                   *          
000420*                                                              *          
000430*    -   DELETED:                                              *          
000440*        FILLER (AFTER MB-BIL-UNUSED-SUBKEY-FIL)               *          
000450*        MB-BIL-UNUSED-FIL-2                                   *          
000460*        MB-BIL-ELIG-EFF-DT                                    *          
000470*                                                              *          
000480*    -   CHANGED:                                              *          
000490*        MB-BIL-UNUSED-SUBKEY-FIL      FROM 6   TO 1   BYTES   *          
000500*        MB-BIL-VOL-MM                 FROM 7   TO 9   BYTES   *          
000510*        MB-BIL-VOL-DP                 FROM 7   TO 9   BYTES   *          
000520*        MB-BIL-PREM-UNLOAD-MM         FROM 5   TO 7   BYTES   *          
000530*        MB-BIL-PREM-UNLOAD-DP         FROM 5   TO 7   BYTES   *          
000540*        MB-BIL-CONV-PREM-MM           FROM 5   TO 7   BYTES   *          
000550*        MB-BIL-CONV-PREM-DP           FROM 5   TO 7   BYTES   *          
000560*        MB-BIL-PREM-LOADED-MM         FROM 5   TO 7   BYTES   *          
000570*        MB-BIL-PREM-LOADED-DP         FROM 5   TO 7   BYTES   *          
000580*        MB-BIL-MIN-PREM-MM            FROM 5   TO 7   BYTES   *          
000590*        MB-BIL-MIN-PREM-DP            FROM 5   TO 7   BYTES   *          
000600*        MB-BIL-RN-VOL-MM              FROM 7   TO 9   BYTES   *          
000610*        MB-BIL-RN-VOL-DP              FROM 7   TO 9   BYTES   *          
000620*        MB-BIL-RN-PREM-MM             FROM 5   TO 7   BYTES   *          
000630*        MB-BIL-RN-PREM-DP             FROM 5   TO 7   BYTES   *          
000640*        MB-BIL-LIAB-LIM-MM            FROM 5   TO 7   BYTES   *          
000650*        MB-BIL-LIAB-LIM-DP            FROM 5   TO 7   BYTES   *          
000660*        MB-BIL-UNUSED-FIL-1           FROM 11  TO 8   BYTES   *          
000670*                                                              *          
000680*    -   CONVERSION NOTES:                                     *          
000690*        ALL DATE FIELDS WERE EXPANDED TO INCLUDE CENTURY      *          
000700*                                                              *          
000710****************************************************************          
000720*    SKIP2                                                                
000730****************************************************************          
000740*       -----    MODIFICATIONS AFTER RELEASE 6.0    -----      *          
000750****************************************************************          
000760*                                                              *          
000770*--->REF#001 RLSE 6.00  CHGS MADE 05/24/96 THRU 08/18/97       *          
000780*    ENH483  SY03 SY07                                         *          
000790*     1. UPDATED FOR RELEASE 6.0                               *          
000800*                                                              *          
000810****************************************************************          
000820*    SKIP2                                                                
000830****************************************************************          
000840*       -----          RELEASE STAMP                -----      *          
000850*                                                              *          
000860*     G5XRMBB0  REL.6.00  09/12/97  13:19:40                   *          
000870****************************************************************          
000880     05  MB-KEY.                                                         1
000890         10  MB-PZ-ID                    PIC X(02).                      1
000900             88  MB-GROUPFACTS             VALUE 'GF'.                    
000910         10  MB-CI-ID                    PIC X(02).                      3
000920         10  MB-REC-ID                   PIC X(02).                      5
000930             88  MB-MEMBER-DATA-REC        VALUE 'MM'.                    
000940         10  MB-KEY1.                                                    7
000950             15  MB-GR-DV-MM.                                            7
000960                 20  MB-GR-ID.                                           7
000970                     25  MB-GR-ID-PFX    PIC X(02).                      7
000980                     25  MB-GR-ID-SFX    PIC X(06).                      9
000990                 20  MB-DV-ID            PIC X(04).                     15
001000                 20  MB-MM-ID            PIC X(09).                     19
001010             15  MB-KEY1-FIL             PIC X(01).                     28
001020             15  MB-SUB-REC-ID           PIC X(02).                     29
001030                 88  MB-BILLING-SUB-REC    VALUE 'MB'.                    
001040         10  MB-KEY-FIL                  PIC X(02).                     31
001050     05  MB-ODO-CTRS             COMP.                                  33
001060         10  MB-BIL-CTR                  PIC S9(04).                    33
001070         10  MB-FILL-ODO-CTR             PIC S9(04).                    35
001080         10  MB-CUST-ODO-CTR             PIC S9(04).                    37
001090     05  MB-SYSTEM-DATA.                                                  
001100         10  MB-LAST-UPD-DATA.                                            
001110             15  MB-LAST-UPD-ONLINE-DATA.                                 
001120                  20  MB-LAST-UPD-DATE               PIC 9(08).           
001130                  20  MB-LAST-UPD-DATE-X REDEFINES                        
001140                     MB-LAST-UPD-DATE.                                    
001150                      25  MB-LAST-UPD-CC             PIC 9(02).           
001160                      25  MB-LAST-UPD-DT             PIC 9(06).           
001170                      25  MB-LAST-UPD-DT-X                                
001180                          REDEFINES MB-LAST-UPD-DT.                       
001190                          30  MB-LAST-UPD-YY         PIC X(02).           
001200                          30  MB-LAST-UPD-MM         PIC X(02).           
001210                          30  MB-LAST-UPD-DD         PIC X(02).           
001220                 20  MB-GF-LAST-UPD-DT                                    
001230                      REDEFINES MB-LAST-UPD-DATE     PIC 9(08).           
001240                 20  MB-GF-LAST-UPD-DT-X                                  
001250                     REDEFINES MB-LAST-UPD-DATE.                          
001260                     25  MB-GF-LAST-UPD-YY          PIC 9(04).            
001270                     25  MB-GF-LAST-UPD-YY-X                              
001280                         REDEFINES MB-GF-LAST-UPD-YY.                     
001290                         30  MB-GF-LAST-UPD-CC      PIC 9(02).            
001300                         30  MB-GF-LAST-UPD-YY2     PIC 9(02).            
001310                     25  MB-GF-LAST-UPD-MM          PIC 9(02).            
001320                     25  MB-GF-LAST-UPD-DD          PIC 9(02).            
001330                 20  MB-LAST-UPD-TIME               PIC 9(06).            
001340                 20  MB-LAST-UPD-TIME-X                                   
001350                     REDEFINES MB-LAST-UPD-TIME.                          
001360                     25  MB-LAST-UPD-TIME-HH        PIC 9(02).            
001370                     25  MB-LAST-UPD-TIME-MM        PIC 9(02).            
001380                     25  MB-LAST-UPD-TIME-SS        PIC 9(02).            
001390                 20  MB-LAST-UPD-US-ID              PIC X(10).            
001400                 20  MB-LOCK-SW                     PIC X(01).            
001410                 20  MB-LAST-UPD-PC-ID              PIC X(02).            
001420                 20  MB-LAST-UPD-REL                PIC X(03).            
001430             15  MB-LAST-UPD-BATCH-DATA.                                  
001440                 20  MB-LAST-UPD-BATCH-DT           PIC 9(08).            
001450                 20  MB-LAST-UPD-BATCH-DT-X                               
001460                     REDEFINES MB-LAST-UPD-BATCH-DT.                      
001470                     25  MB-LAST-UPD-BATCH-YY       PIC 9(04).            
001480                     25  MB-LAST-UPD-BATCH-YY-X                           
001490                         REDEFINES MB-LAST-UPD-BATCH-YY.                  
001500                         30  MB-LAST-UPD-BATCH-CC   PIC 9(02).            
001510                         30  MB-LAST-UPD-BATCH-YY2  PIC 9(02).            
001520                     25  MB-LAST-UPD-BATCH-MM       PIC 9(02).            
001530                     25  MB-LAST-UPD-BATCH-DD       PIC 9(02).            
001540                 20  MB-LAST-UPD-BATCH-PROG-ID      PIC X(08).            
001550                 20  MB-LAST-UPD-BATCH-FIL          PIC X(04).            
001560     05  MB-INDIC-INFO.                                                 89
001570         10  MB-USE-CTR                  PIC S9(03) COMP-3.             89
001580         10  MB-INDIC-UNUSED-FIL         PIC X(30).                     91
001590     05  MB-BILLING-DATA.                                              121
001600         10  MB-BILLING-ITEM     OCCURS      91 TIMES                  121
001610*****    10  MB-BILLING-ITEM     OCCURS 0 TO 91 TIMES                     
001620*****                            DEPENDING ON MB-BIL-CTR                  
001630                                 INDEXED BY MB-BIL-IDX                    
001640                                            MB-BIL-IDX1                   
001650                                            MB-BIL-IDX2.                  
001660             15  MB-BIL-ITEM-KEY.                                      121
001670                 20  MB-BIL-BN-PL.                                     121
001680                     25  MB-BIL-BN-ID        PIC X(02).                121
001690                     25  MB-BIL-PLAN         PIC X(06).                123
001700                 20  MB-BIL-FAM-CD           PIC X(01).                129
001710                     88  MB-BIL-FAM-VALID      VALUES 'F'.                
001720                     88  MB-BIL-FAM-FAMILY     VALUE  'F'.                
001730                 20  MB-BIL-UNUSED-SUBKEY-FIL PIC X(01).               130
001740             15  MB-BIL-PER-FROM-DT.                                   138
001750                 20  MB-BIL-PER-FROM-YY.                                  
001760                     25  MB-BIL-PER-FROM-CC      PIC X(02).               
001770                     25  MB-BIL-PER-FROM-YY2     PIC X(02).               
001780                 20  MB-BIL-PER-FROM-MM          PIC X(02).               
001790                 20  MB-BIL-PER-FROM-DD          PIC X(02).               
001800             15  MB-BIL-PER-TO-DT.                                     144
001810                 20  MB-BIL-PER-TO-YY.                                    
001820                     25  MB-BIL-PER-TO-CC        PIC X(02).               
001830                     25  MB-BIL-PER-TO-YY2       PIC X(02).               
001840                 20  MB-BIL-PER-TO-MM            PIC X(02).               
001850                 20  MB-BIL-PER-TO-DD            PIC X(02).               
001860             15  MB-BIL-DUE-ADJ-DT.                                       
001870                 20  MB-BIL-DUE-ADJ-YY.                                   
001880                     25  MB-BIL-DUE-ADJ-CC       PIC X(02).               
001890                     25  MB-BIL-DUE-ADJ-YY2      PIC X(02).               
001900                 20  MB-BIL-DUE-ADJ-MM           PIC X(02).               
001910                 20  MB-BIL-DUE-ADJ-DD           PIC X(02).               
001920             15  MB-BIL-NO-MONTHS        PIC S9(03)    COMP-3.         156
001930             15  MB-BIL-ADJ-CD                PIC X(02).               158
001940                 88  MB-BIL-ADJ-VALID                                     
001950                                 VALUES '  ' 'A ' 'B ' 'C ' 'D '          
001960                                'E ' 'F ' 'G ' 'H ' 'I ' 'J '             
001970                                'K ' 'L ' 'M ' 'N ' 'O ' 'P ' 'Q '        
001980                                'R ' 'S ' 'T ' 'U ' 'V ' 'W ' 'X '        
001990                                'Y ' 'Z ' '$ ' '1 ' '2 ' '3 '             
002000                                '4 ' '5 ' '6 ' '7 ' '8 ' '9 '.            
002010                 88  MB-BIL-ADJ-NO-ADJ           VALUE '  '.              
002020                 88  MB-BIL-ADJ-ADD-MEMBER       VALUE 'A '.              
002030                 88  MB-BIL-ADJ-BN-REC-CHG       VALUE 'B '.              
002040                 88  MB-BIL-ADJ-ELEC-CHG-CS      VALUE 'C '.              
002050                 88  MB-BIL-ADJ-DP-CHG           VALUE 'D '.              
002060                 88  MB-BIL-ADJ-ELEC-CHG-EV      VALUE 'E '.              
002070                 88  MB-BIL-ADJ-ELEC-CHG-FI      VALUE 'F '.              
002080                 88  MB-BIL-ADJ-GR-REC-CHG       VALUE 'G '.              
002090                 88  MB-BIL-ADJ-DV-REC-CHG       VALUE 'H '.              
002100                 88  MB-BIL-ADJ-REINSTATE        VALUE 'I '.              
002110                 88  MB-BIL-ADJ-ELEC-CHG-SB      VALUE 'J '.              
002120                 88  MB-BIL-ADJ-REINS-CHG        VALUE 'K '.              
002130                 88  MB-BIL-ADJ-ELEC-CHG-LP      VALUE 'L '.              
002140                 88  MB-BIL-ADJ-MR-REC-CHG       VALUE 'M '.              
002150                 88  MB-BIL-ADJ-NR-REC-CHG       VALUE 'N '.              
002160                 88  MB-BIL-ADJ-ONLY-MANUAL      VALUE 'O '.              
002170                 88  MB-BIL-ADJ-ELEC-CHG-SL      VALUE 'P '.              
002180                 88  MB-BIL-ADJ-ELEC-CHG-GN      VALUE 'Q '.              
002190                 88  MB-BIL-ADJ-RETIRE           VALUE 'R '.              
002200                 88  MB-BIL-ADJ-ELEC-CHG-BS      VALUE 'S '.              
002210                 88  MB-BIL-ADJ-TERMINATE        VALUE 'T '.              
002220                 88  MB-BIL-ADJ-ELEC-CHG-EU      VALUE 'U '.              
002230                 88  MB-BIL-ADJ-FT-REC-CHG       VALUE 'V '.              
002240                 88  MB-BIL-ADJ-OTHER            VALUE 'W '.              
002250                 88  MB-BIL-ADJ-CORRECTION       VALUE 'X '.              
002260                 88  MB-BIL-ADJ-AGE-CHG          VALUE 'Y '.              
002270                 88  MB-BIL-ADJ-RATING-ZIP       VALUE 'Z '.              
002280                 88  MB-BIL-ADJ-ELEC-CHG-SA      VALUE '$ '.              
002290                 88  MB-BIL-ADJ-ELEC-CHG-VO      VALUE '1 '.              
002300                 88  MB-BIL-ADJ-ELEC-CHG-DS      VALUE '2 '.              
002310                 88  MB-BIL-ADJ-ELEC-CHG-PI      VALUE '3 '.              
002320                 88  MB-BIL-ADJ-MM-KEY-CHG       VALUE '4 '.              
002330                 88  MB-BIL-ADJ-FR-REC-CHG       VALUE '5 '.              
002340                 88  MB-BIL-ADJ-DH-REC-CHG       VALUE '6 '.              
002350                 88  MB-BIL-ADJ-MX-REC-CHG       VALUE '7 '.              
002360                 88  MB-BIL-ADJ-ME-REC-CHG       VALUE '8 '.              
002370                 88  MB-BIL-ADJ-DR-REC-CHG       VALUE '9 '.              
002380             15  MB-BIL-ADJ-TYP          PIC X(01).                    160
002390                 88  MB-BIL-ADJ-TYP-VALID      VALUE 'D' 'M' ' '.         
002400                 88  MB-BIL-ADJ-TYP-UNDEFINED  VALUE ' '.                 
002410                 88  MB-BIL-ADJ-DAYS           VALUE 'D'.                 
002420                 88  MB-BIL-ADJ-MOS            VALUE 'M'.                 
002430             15  MB-BIL-MODE             PIC X(02).                    166
002440                 88  MB-BIL-MODE-VALID         VALUE '01' '02'            
002450                                                     '04' '09'            
002460                                                     '10' '12'            
002470                                                     '13' '24'            
002480                                                     '26' '52'.           
002490                 88  MB-BIL-MODE-ANNUAL       VALUE '01'.                 
002500                 88  MB-BIL-MODE-SEMI-AN      VALUE '02'.                 
002510                 88  MB-BIL-MODE-QTRLY        VALUE '04'.                 
002520                 88  MB-BIL-MODE-NINTHLY      VALUE '09'.                 
002530                 88  MB-BIL-MODE-TENTHLY      VALUE '10'.                 
002540                 88  MB-BIL-MODE-MTHLY        VALUE '12'.                 
002550                 88  MB-BIL-MODE-LUNAR        VALUE '13'.                 
002560                 88  MB-BIL-MODE-HALF-MTHLY   VALUE '24'.                 
002570                 88  MB-BIL-MODE-BIWKLY       VALUE '26'.                 
002580                 88  MB-BIL-MODE-WKLY         VALUE '52'.                 
002590             15  MB-BIL-BILLS-PER-YEAR                                 166
002600                    REDEFINES MB-BIL-MODE    PIC 9(02).                   
002610             15  MB-BIL-ELIG-SPC.                                      168
002620                 20  MB-BIL-ELIG-SCREEN  PIC X(01).                    168
002630                 20  MB-BIL-ELIG-PLAN    PIC X(06).                    169
002640                 20  MB-BIL-ELIG-COV     PIC X(02).                    175
002650             15  MB-BIL-FAM-IND          PIC X(01).                    183
002660                 88  MB-BIL-FAM-IND-VALID  VALUE 'A' THRU 'G'.            
002670                 88  MB-BIL-FAM-FAM        VALUE 'A'.                     
002680                 88  MB-BIL-FAM-MB-SP      VALUE 'B'.                     
002690                 88  MB-BIL-FAM-MM         VALUE 'C'.                     
002700                 88  MB-BIL-FAM-MB-DP      VALUE 'D'.                     
002710                 88  MB-BIL-FAM-SP-DP      VALUE 'E'.                     
002720                 88  MB-BIL-FAM-SP         VALUE 'F'.                     
002730                 88  MB-BIL-FAM-DP         VALUE 'G'.                     
002740             15  MB-BIL-STS              PIC X(01).                    184
002750                 88  MB-BIL-STS-VALID      VALUE 'A' 'T'.                 
002760                 88  MB-BIL-STS-ACTIVE     VALUE 'A'.                     
002770                 88  MB-BIL-STS-TERMN8-BN  VALUE 'T'.                     
002780             15  MB-BIL-PREM-TYPE        PIC X(01).                    185
002790                 88  MB-BIL-PREM-TYPE-VALID  VALUE 'M' 'N'                
002800                                                   'W' 'L'                
002810                                                   'R'.                   
002820                 88  MB-BIL-PREM-MEMO        VALUE 'M'.                   
002830                 88  MB-BIL-PREM-NORMAL      VALUE 'N'.                   
002840                 88  MB-BIL-PREM-WAIVED      VALUE 'W' 'R'.               
002850                 88  MB-BIL-PREM-LAPSE       VALUE 'L'.                   
002860                 88  MB-BIL-PREM-RN-WAIVED   VALUE 'R'.                   
002870             15  MB-BIL-LVS.                                           186
002880                 20  MB-BIL-LVS-MM       PIC S9(03)    COMP-3.         186
002890                 20  MB-BIL-LVS-DP       PIC S9(03)    COMP-3.         188
002900             15  MB-BIL-VOL.                                           190
002910                 20  MB-BIL-VOL-MM       PIC S9(09)    COMP-3.         190
002920                 20  MB-BIL-VOL-DP       PIC S9(09)    COMP-3.         194
002930             15  MB-BIL-PREM-UNLOAD.                                   198
002940                 20  MB-BIL-PREM-UNLOAD-MM  PIC S9(07)V99 COMP-3.      198
002950                 20  MB-BIL-PREM-UNLOAD-DP  PIC S9(07)V99 COMP-3.      202
002960             15  MB-BIL-CONV-PREM REDEFINES                            198
002970                 MB-BIL-PREM-UNLOAD.                                      
002980                 20  MB-BIL-CONV-PREM-MM    PIC S9(07)V99 COMP-3.      198
002990                 20  MB-BIL-CONV-PREM-DP    PIC S9(07)V99 COMP-3.      202
003000             15  MB-BIL-PREM-LOADED.                                   206
003010                 20  MB-BIL-PREM-LOADED-MM  PIC S9(07)V99 COMP-3.      206
003020                 20  MB-BIL-PREM-LOADED-DP  PIC S9(07)V99 COMP-3.      210
003030             15  MB-BIL-MIN-PREM REDEFINES                             206
003040                 MB-BIL-PREM-LOADED.                                      
003050                 20  MB-BIL-MIN-PREM-MM     PIC S9(07)V99 COMP-3.      206
003060                 20  MB-BIL-MIN-PREM-DP     PIC S9(07)V99 COMP-3.      210
003070             15  MB-BIL-RN-LVS.                                        214
003080                 20  MB-BIL-RN-LVS-MM       PIC S9(03)    COMP-3.      214
003090                 20  MB-BIL-RN-LVS-DP       PIC S9(03)    COMP-3.      216
003100             15  MB-BIL-RN-VOL.                                        218
003110                 20  MB-BIL-RN-VOL-MM       PIC S9(09)    COMP-3.      218
003120                 20  MB-BIL-RN-VOL-DP       PIC S9(09)    COMP-3.      222
003130             15  MB-BIL-RN-PREM.                                       226
003140                 20  MB-BIL-RN-PREM-MM      PIC S9(07)V99 COMP-3.      226
003150                 20  MB-BIL-RN-PREM-DP      PIC S9(07)V99 COMP-3.      230
003160             15  MB-BIL-LIAB-LIM-PREM REDEFINES                        226
003170                 MB-BIL-RN-PREM.                                          
003180                 20  MB-BIL-LIAB-LIM-MM     PIC S9(07)V99 COMP-3.         
003190                 20  MB-BIL-LIAB-LIM-DP     PIC S9(07)V99 COMP-3.         
003200             15  MB-BIL-COV-PYRL-VOL        PIC S9(07)V99 COMP-3.         
003210             15  MB-BIL-CR-ID               PIC X(02).                    
003220             15  MB-BIL-PREM-IND            PIC X(01).                    
003230                 88  MB-BIL-PREM-IND-VALID   VALUE ' ' 'A' 'M'            
003240                                                   'N' 'P'.               
003250                 88  MB-BIL-PREM-IND-NORMAL  VALUE 'N' ' '.               
003260                 88  MB-BIL-PREM-IND-MPP     VALUE 'M'.                   
003270                 88  MB-BIL-PREM-IND-ASO     VALUE 'A'.                   
003280                 88  MB-BIL-PREM-IND-PYRL    VALUE 'P'.                   
003290             15  MB-BIL-RETN-CD          PIC X(01).                    237
003300                 88  MB-BIL-RETN-VALID       VALUE ' ' 'N' 'Y'.           
003310                 88  MB-BIL-RETN-UNUSED      VALUE ' '.                   
003320                 88  MB-BIL-RETN-NO          VALUE 'N'.                   
003330                 88  MB-BIL-RETN-YES         VALUE 'Y'.                   
003340             15  MB-BIL-PYRL-ID          PIC X(01).                    238
003350             15  MB-BIL-REQS-PROR8-SW    PIC X(01).                    239
003360                 88  MB-BIL-REQS-PROR8       VALUE 'Y'.                   
003370             15  MB-BIL-UNUSED-FIL-1     PIC X(08).                    240
003380****************************************************************          
003390*            END  OF  G 5 X R M B B 0                          *          
003400****************************************************************          
