000010****************************************************************          
000020* MEMBER KEY CHANGE RECORD   :  G5XRMKB0                       *          
000030* KEY LENGTH                 :  K-MK-KEY-LENGTH (32)           *          
000040* RECORD LENGTH              :  K-MK-REC-LENGTH (264 - 2500)   *          
000050****************************************************************          
000060*    SKIP2                                                                
000070****************************************************************          
000080*         **  PROPRIETARY PROPERTY OF ERISCO, INC.  **         *          
000090*         **        COPYRIGHT 1984 - 1997           **         *          
000100****************************************************************          
000110*    SKIP2                                                                
000120****************************************************************          
000130*       -----    RELEASE 6.0 MODIFICATION NOTES     -----      *          
000140*                                                              *          
000150*    -   REC LENGTH CHANGED FROM 184-2900  TO  264-2500        *          
000160*                                                              *          
000170*    -   CHANGED:                                              *          
000180*        MK-UNUSED-FIL                 FROM 82  TO 60  BYTES   *          
000190*        MK-SK-UNUSED-FIL              FROM 40  TO 20  BYTES   *          
000200*                                                              *          
000210*    -   CONVERSION NOTES:                                     *          
000220*        ALL DATE FIELDS WERE EXPANDED TO INCLUDE CENTURY      *          
000230*                                                              *          
000240****************************************************************          
000250*    SKIP2                                                                
000260****************************************************************          
000270*       -----    MODIFICATIONS AFTER RELEASE 6.0    -----      *          
000280****************************************************************          
000290*                                                              *          
000300*--->REF#001 RLSE 6.00  CHGS MADE 05/24/96 THRU 08/18/97       *          
000310*    ENH483  GF09                                              *          
000320*     1. UPDATED FOR RELEASE 6.0                               *          
000330*                                                              *          
000340****************************************************************          
000350*    SKIP2                                                                
000360****************************************************************          
000370*       -----          RELEASE STAMP                -----      *          
000380*                                                              *          
000390*     G5XRMKB0  REL.6.00  08/18/97  12:29:04                   *          
000400****************************************************************          
000410     05  MK-KEY.                                                          
000420         10  MK-PZ-ID                    PIC X(02).                       
000430             88  MK-GROUPFACTS             VALUE 'GF'.                    
000440         10  MK-CI-ID                    PIC X(02).                       
000450         10  MK-REC-ID                   PIC X(02).                       
000460             88  MK-MEMBER-KEY-CHANGE-REC  VALUE  'MK'.                   
000470         10  MK-KEY1.                                                     
000480             15  MK-GR-ID.                                                
000490                 20  MK-GR-ID-PFX        PIC X(02).                       
000500                 20  MK-GR-ID-SFX        PIC X(06).                       
000510             15  MK-DV-ID                PIC X(04).                       
000520             15  MK-MM-ID                PIC X(09).                       
000530         10  MK-KEY-FIL                  PIC X(05).                       
000540*    SKIP1                                                                
000550     05  MK-ODO-CTRS             COMP.                                    
000560         10  MK-SK-CTR                   PIC S9(04).                      
000570         10  MK-FILL-ODO-CTR             PIC S9(04).                      
000580         10  MK-CUST-ODO-CTR             PIC S9(04).                      
000590*    SKIP1                                                                
000600     05  MK-SYSTEM-DATA.                                                  
000610         10  MK-LAST-UPD-DATA.                                            
000620             15  MK-LAST-UPD-ONLINE-DATA.                                 
000630                  20  MK-LAST-UPD-DATE               PIC 9(08).           
000640                  20  MK-LAST-UPD-DATE-X REDEFINES                        
000650                     MK-LAST-UPD-DATE.                                    
000660                      25  MK-LAST-UPD-CC             PIC 9(02).           
000670                      25  MK-LAST-UPD-DT             PIC 9(06).           
000680                      25  MK-LAST-UPD-DT-X                                
000690                          REDEFINES MK-LAST-UPD-DT.                       
000700                          30  MK-LAST-UPD-YY         PIC X(02).           
000710                          30  MK-LAST-UPD-MM         PIC X(02).           
000720                          30  MK-LAST-UPD-DD         PIC X(02).           
000730                 20  MK-GF-LAST-UPD-DT                                    
000740                      REDEFINES MK-LAST-UPD-DATE     PIC 9(08).           
000750                 20  MK-GF-LAST-UPD-DT-X                                  
000760                     REDEFINES MK-LAST-UPD-DATE.                          
000770                     25  MK-GF-LAST-UPD-YY          PIC 9(04).            
000780                     25  MK-GF-LAST-UPD-YY-X                              
000790                         REDEFINES MK-GF-LAST-UPD-YY.                     
000800                         30  MK-GF-LAST-UPD-CC      PIC 9(02).            
000810                         30  MK-GF-LAST-UPD-YY2     PIC 9(02).            
000820                     25  MK-GF-LAST-UPD-MM          PIC 9(02).            
000830                     25  MK-GF-LAST-UPD-DD          PIC 9(02).            
000840                 20  MK-LAST-UPD-TIME               PIC 9(06).            
000850                 20  MK-LAST-UPD-TIME-X                                   
000860                     REDEFINES MK-LAST-UPD-TIME.                          
000870                     25  MK-LAST-UPD-TIME-HH        PIC 9(02).            
000880                     25  MK-LAST-UPD-TIME-MM        PIC 9(02).            
000890                     25  MK-LAST-UPD-TIME-SS        PIC 9(02).            
000900                 20  MK-LAST-UPD-US-ID              PIC X(10).            
000910                 20  MK-LOCK-SW                     PIC X(01).            
000920                 20  MK-LAST-UPD-PC-ID              PIC X(02).            
000930                 20  MK-LAST-UPD-REL                PIC X(03).            
000940*    SKIP1                                                                
000950             15  MK-LAST-UPD-BATCH-DATA.                                  
000960                 20  MK-LAST-UPD-BATCH-DT           PIC 9(08).            
000970                 20  MK-LAST-UPD-BATCH-DT-X                               
000980                     REDEFINES MK-LAST-UPD-BATCH-DT.                      
000990                     25  MK-LAST-UPD-BATCH-YY       PIC 9(04).            
001000                     25  MK-LAST-UPD-BATCH-YY-X                           
001010                         REDEFINES MK-LAST-UPD-BATCH-YY.                  
001020                         30  MK-LAST-UPD-BATCH-CC   PIC 9(02).            
001030                         30  MK-LAST-UPD-BATCH-YY2  PIC 9(02).            
001040                     25  MK-LAST-UPD-BATCH-MM       PIC 9(02).            
001050                     25  MK-LAST-UPD-BATCH-DD       PIC 9(02).            
001060                 20  MK-LAST-UPD-BATCH-PROG-ID      PIC X(08).            
001070                 20  MK-LAST-UPD-BATCH-FIL          PIC X(04).            
001080*    SKIP1                                                                
001090         10  MK-USER-AREA-TBL.                                            
001100             15  MK-USER-AREA-ITEM       PIC X(10)  OCCURS 3.             
001110*    SKIP1                                                                
001120     05  MK-INDIC-DATA.                                                   
001130         10  MK-UNUSED-FIL               PIC X(60).                       
001140*    SKIP1                                                                
001150     05  MK-SUBS-KEYS-TBL.                                                
001160         10  MK-SK-ITEM     OCCURS      27 TIMES                          
001170*****    10  MK-SK-ITEM     OCCURS 0 TO 27 TIMES                          
001180*****                       DEPENDING   ON  MK-SK-CTR                     
001190                            INDEXED BY    MK-SK-IDX.                      
001200*    SKIP1                                                                
001210             15  MK-SK-KEY1.                                              
001220                 20  MK-SK-GR-ID.                                         
001230                     25  MK-SK-GR-ID-PFX     PIC X(02).                   
001240                     25  MK-SK-GR-ID-SFX     PIC X(06).                   
001250                 20  MK-SK-DV-ID             PIC X(04).                   
001260                 20  MK-SK-MM-ID             PIC X(09).                   
001270             15  MK-SK-KEY-FIL               PIC X(01).                   
001280*    SKIP1                                                                
001290             15  MK-SK-CHNG-EFF-DT.                                       
001300                 20  MK-SK-CHNG-EFF-YY.                                   
001310                     25  MK-SK-CHNG-EFF-CC   PIC X(02).                   
001320                     25  MK-SK-CHNG-EFF-YY2  PIC X(02).                   
001330                 20  MK-SK-CHNG-EFF-MM       PIC X(02).                   
001340                 20  MK-SK-CHNG-EFF-DD       PIC X(02).                   
001350*    SKIP1                                                                
001360             15  MK-SK-TRANSFER-RSN          PIC X(20).                   
001370*    SKIP1                                                                
001380             15  MK-SK-EFF-OR-SYS-DT.                                     
001390                 20  MK-SK-EFF-OR-SYS-YY.                                 
001400                     25  MK-SK-EFF-OR-SYS-CC     PIC X(02).               
001410                     25  MK-SK-EFF-OR-SYS-YY2    PIC X(02).               
001420                 20  MK-SK-EFF-OR-SYS-MM         PIC X(02).               
001430                 20  MK-SK-EFF-OR-SYS-DD         PIC X(02).               
001440*    SKIP1                                                                
001450             15  MK-SK-ACT-DT.                                            
001460                 20  MK-SK-ACT-YY.                                        
001470                     25  MK-SK-ACT-CC        PIC X(02).                   
001480                     25  MK-SK-ACT-YY2       PIC X(02).                   
001490                 20  MK-SK-ACT-MM            PIC X(02).                   
001500                 20  MK-SK-ACT-DD            PIC X(02).                   
001510*    SKIP1                                                                
001520             15  MK-SK-UNUSED-FIL            PIC X(20).                   
001530*    SKIP2                                                                
001540****************************************************************          
001550*            END  OF  G 5 X R M K B 0                          *          
001560****************************************************************          
