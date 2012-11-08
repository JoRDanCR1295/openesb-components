000010****************************************************************          
000020*  GF MEMBER ADDRESS RECORD :    G5XRMPB0                      *          
000030*        KEY LENGTH         :    K-MP-KEY-LENGTH (32)          *          
000040*        RECORD LENGTH      :    K-MP-REC-LENGTH (500)         *          
000050*                                                              *          
000060*   ANY CHANGES MADE TO G5XRMPB0 SHOULD BE SYNCHRONIZED WITH   *          
000070*   G5XRMPN0 ('NEW-' VERSION OF G5XRMPB0).                     *          
000080*                                                              *          
000090****************************************************************          
000100*    SKIP2                                                                
000110****************************************************************          
000120*         **  PROPRIETARY PROPERTY OF ERISCO, INC.  **         *          
000130*         **        COPYRIGHT 1984 - 1997           **         *          
000140****************************************************************          
000150*    SKIP2                                                                
000160****************************************************************          
000170*       -----    RELEASE 6.0 MODIFICATION NOTES     -----      *          
000180*                                                              *          
000190*    -   CHANGED:                                              *          
000200*        MP-ZIP                        FROM 10  TO 11  BYTES   *          
000210*        MP-UNUSED-FIL3                FROM 35  TO 34  BYTES   *          
000220*                                                              *          
000230*    -   CONVERSION NOTES:                                     *          
000240*        ALL DATE FIELDS WERE EXPANDED TO INCLUDE CENTURY      *          
000250*                                                              *          
000260****************************************************************          
000270*    SKIP2                                                                
000280****************************************************************          
000290*       -----    MODIFICATIONS AFTER RELEASE 6.0    -----      *          
000300****************************************************************          
000310*                                                              *          
000320*--->REF#001 RLSE 6.00  CHGS MADE 05/24/96 THRU 08/18/97       *          
000330*    ENH483  SY03 SY07                                         *          
000340*     1. UPDATED FOR RELEASE 6.0                               *          
000350*                                                              *          
000360****************************************************************          
000370*    SKIP2                                                                
000380****************************************************************          
000390*       -----          RELEASE STAMP                -----      *          
000400*                                                              *          
000410*     G5XRMPB0  REL.6.00  08/18/97  12:54:38                   *          
000420****************************************************************          
000430     05  MP-KEY.                                                          
000440         10  MP-PZ-ID                        PIC X(02).                   
000450             88  MP-GROUPFACTS                 VALUE  'GF'.               
000460         10  MP-CI-ID                        PIC X(02).                   
000470         10  MP-REC-ID                       PIC X(02).                   
000480             88  MP-MEMBER-DATA-REC            VALUE  'MM'.               
000490         10  MP-KEY1.                                                     
000500             15  MP-GR-DV-MM.                                             
000510                 20  MP-GR-DV.                                            
000520                     25  MP-GR-ID.                                        
000530                         30  MP-GR-ID-PFX   PIC X(02).                    
000540                         30  MP-GR-ID-SFX   PIC X(06).                    
000550                     25  MP-DV-ID           PIC X(04).                    
000560                 20  MP-MM-ID               PIC X(09).                    
000570             15  MP-KEY1-FIL                PIC X.                        
000580         10  MP-SUB-REC-ID                  PIC XX.                       
000590             88  MP-ADDRESS-SUB-REC         VALUE 'MP'.                   
000600         10  MP-KEY-FIL                     PIC X(02).                    
000610*    SKIP2                                                                
000620     05  MP-ODO-CTRS                 COMP.                                
000630         10  MP-FILL-ODO-CTR                  PIC S9(04).                 
000640         10  MP-CUST-ODO-CTR                  PIC S9(04).                 
000650*    SKIP1                                                                
000660     05  MP-SYSTEM-DATA.                                                  
000670         10  MP-LAST-UPD-DATA.                                            
000680             15  MP-LAST-UPD-ONLINE-DATA.                                 
000690                  20  MP-LAST-UPD-DATE               PIC 9(08).           
000700                  20  MP-LAST-UPD-DATE-X REDEFINES                        
000710                     MP-LAST-UPD-DATE.                                    
000720                      25  MP-LAST-UPD-CC             PIC 9(02).           
000730                      25  MP-LAST-UPD-DT             PIC 9(06).           
000740                      25  MP-LAST-UPD-DT-X                                
000750                          REDEFINES MP-LAST-UPD-DT.                       
000760                          30  MP-LAST-UPD-YY         PIC X(02).           
000770                          30  MP-LAST-UPD-MM         PIC X(02).           
000780                          30  MP-LAST-UPD-DD         PIC X(02).           
000790                 20  MP-GF-LAST-UPD-DT                                    
000800                      REDEFINES MP-LAST-UPD-DATE     PIC 9(08).           
000810                 20  MP-GF-LAST-UPD-DT-X                                  
000820                     REDEFINES MP-LAST-UPD-DATE.                          
000830                     25  MP-GF-LAST-UPD-YY          PIC 9(04).            
000840                     25  MP-GF-LAST-UPD-YY-X                              
000850                         REDEFINES MP-GF-LAST-UPD-YY.                     
000860                         30  MP-GF-LAST-UPD-CC      PIC 9(02).            
000870                         30  MP-GF-LAST-UPD-YY2     PIC 9(02).            
000880                     25  MP-GF-LAST-UPD-MM          PIC 9(02).            
000890                     25  MP-GF-LAST-UPD-DD          PIC 9(02).            
000900                 20  MP-LAST-UPD-TIME               PIC 9(06).            
000910                 20  MP-LAST-UPD-TIME-X                                   
000920                     REDEFINES MP-LAST-UPD-TIME.                          
000930                     25  MP-LAST-UPD-TIME-HH        PIC 9(02).            
000940                     25  MP-LAST-UPD-TIME-MM        PIC 9(02).            
000950                     25  MP-LAST-UPD-TIME-SS        PIC 9(02).            
000960                 20  MP-LAST-UPD-US-ID              PIC X(10).            
000970                 20  MP-LOCK-SW                     PIC X(01).            
000980                 20  MP-LAST-UPD-PC-ID              PIC X(02).            
000990                 20  MP-LAST-UPD-REL                PIC X(03).            
001000*    SKIP1                                                                
001010             15  MP-LAST-UPD-BATCH-DATA.                                  
001020                 20  MP-LAST-UPD-BATCH-DT           PIC 9(08).            
001030                 20  MP-LAST-UPD-BATCH-DT-X                               
001040                     REDEFINES MP-LAST-UPD-BATCH-DT.                      
001050                     25  MP-LAST-UPD-BATCH-YY       PIC 9(04).            
001060                     25  MP-LAST-UPD-BATCH-YY-X                           
001070                         REDEFINES MP-LAST-UPD-BATCH-YY.                  
001080                         30  MP-LAST-UPD-BATCH-CC   PIC 9(02).            
001090                         30  MP-LAST-UPD-BATCH-YY2  PIC 9(02).            
001100                     25  MP-LAST-UPD-BATCH-MM       PIC 9(02).            
001110                     25  MP-LAST-UPD-BATCH-DD       PIC 9(02).            
001120                 20  MP-LAST-UPD-BATCH-PROG-ID      PIC X(08).            
001130                 20  MP-LAST-UPD-BATCH-FIL          PIC X(04).            
001140*    SKIP1                                                                
001150     05  MP-INDIC-DATA.                                                   
001160         10  MP-USE-CTR                  PIC S9(03) COMP-3.               
001170         10  MP-OPTIONS.                                                  
001180             15  MP-OPT1                 PIC X(01).                       
001190                 88  MP-OPT1-VALID         VALUE ' '.                     
001200             15  MP-OPT2                 PIC X(01).                       
001210                 88  MP-OPT2-VALID         VALUE ' '.                     
001220             15  MP-OPT3                 PIC X(01).                       
001230                 88  MP-OPT3-VALID         VALUE ' '.                     
001240             15  MP-OPT4                 PIC X(01).                       
001250                 88  MP-OPT4-VALID         VALUE ' '.                     
001260             15  MP-OPT5                 PIC X(01).                       
001270                 88  MP-OPT5-VALID         VALUE ' '.                     
001280             15  MP-OPT6                 PIC X(01).                       
001290                 88  MP-OPT6-VALID         VALUE ' '.                     
001300             15  MP-OPT7                 PIC X(01).                       
001310                 88  MP-OPT7-VALID         VALUE ' '.                     
001320             15  MP-OPT8                 PIC X(01).                       
001330                 88  MP-OPT8-VALID         VALUE ' '.                     
001340*    SKIP1                                                                
001350         10  MP-GEN-ID                   PIC X(12).                       
001360*    SKIP1                                                                
001370         10  MP-NM-N-ADR.                                                 
001380             15  MP-ADDR-OPTIONS.                                         
001390                 20  MP-ADDR-OPT         PIC X(01).                       
001400                     88  MP-ADDR-OPT1-VALID  VALUE ' ' 'U' 'B'            
001410                                                   'C' 'N'.               
001420                     88  MP-ADDR-USA         VALUE ' ' 'U'.               
001430                     88  MP-ADDR-BRITISH     VALUE 'B'.                   
001440                     88  MP-ADDR-CANADIAN    VALUE 'C'.                   
001450                     88  MP-ADDR-NO-EDIT     VALUE 'N'.                   
001460*    SKIP1                                                                
001470                 20  MP-ADDR-OPT2        PIC X(01).                       
001480                     88  MP-ADDR-OPT2-VALID  VALUE ' '.                   
001490                 20  MP-ADDR-OPT3        PIC X(01).                       
001500                     88  MP-ADDR-OPT3-VALID  VALUE ' '.                   
001510                 20  MP-ADDR-OPT4        PIC X(01).                       
001520                     88  MP-ADDR-OPT4-VALID  VALUE ' '.                   
001530*    SKIP1                                                                
001540                 20  MP-ADDR-OPT-FIL     PIC X(04).                       
001550*    SKIP1                                                                
001560             15  MP-NAME.                                                 
001570                 20  MP-NAME1            PIC X(30).                       
001580                 20  MP-NAME2            PIC X(30).                       
001590*    SKIP1                                                                
001600             15  MP-CONTACT              PIC X(30).                       
001610*    SKIP1                                                                
001620             15  MP-ADDRESS.                                              
001630                 20  MP-ADDR1            PIC X(30).                       
001640                 20  MP-ADDR2            PIC X(30).                       
001650                 20  MP-ADDR3            PIC X(30).                       
001660                 20  MP-ADDR4.                                            
001670                     25  MP-CITY         PIC X(19).                       
001680                     25  MP-STATE        PIC X(02).                       
001690                     25  MP-ZIP          PIC X(11).                       
001700                 20  MP-COUNTRY          PIC X(03).                       
001710*    SKIP1                                                                
001720         10  MP-PHONE.                                                    
001730             15 FILLER                   PIC X(01).                       
001740             15 MP-PHONE-AREA            PIC X(03).                       
001750             15 MP-PHONE-EXCHANGE        PIC X(03).                       
001760             15 MP-PHONE-NUMBER          PIC X(04).                       
001770*    SKIP1                                                                
001780         10  MP-UNUSED-FIL2              PIC X(30).                       
001790*    SKIP1                                                                
001800         10  MP-GENERAL-FIELDS.                                           
001810             15 MP-GEN-FLD-1             PIC X(30).                       
001820             15 MP-GEN-FLD-2             PIC X(16).                       
001830             15 MP-GEN-FLD-3             PIC X(16).                       
001840             15 MP-GEN-FLD-4             PIC X(16).                       
001850             15 MP-GEN-FLD-5             PIC X(16).                       
001860*    SKIP1                                                                
001870         10  MP-UNUSED-FIL3              PIC X(34).                       
001880*    SKIP1                                                                
001890****************************************************************          
001900*     END OF ***  G 5 X R M P B 0 ***                          *          
001910****************************************************************          
