      **** 04/02/1999 - LG-LAST-UPD-DT IS ADDED *********                       
      ***************************************************                       
000580**** 05  LG-LOG-PRE-LENTH                PIC X(4).                        
000580     05  LG-LOG-PREFIX.                                                   
000640         10  LG-B-A                      PIC X.                           
               10  LG-LAST-UPD-DT              PIC 9(8).                        
000990         10  LG-TXN-ID                   PIC X(4).                        
000810         10  LG-SUB-FUNCTION             PIC X.                          1
000520     05  CLR-KEY.                                                        1
000530         10  CLR-PZ-ID                       PIC XX.                     1
000540             88  CLR-CLAIMFACTS              VALUE 'CF'.                  
000550         10  CLR-CI-ID                       PIC XX.                     3
000560         10  CLR-REC-ID                      PIC XX.                     5
000570             88  CLR-CLAIM-REC               VALUE 'CL'.                  
000580         10  CLR-KEY1.                                                   7
000590             15  CLR-PA-KEY1.                                            7
000600                 20  CLR-ME-KEY1.                                        7
000610                     25  CLR-PA-GROUP        PIC X(08).                  7
000620                     25  CLR-PA-ID           PIC X(09).                 15
000630                 20  CLR-PA-REL-NAME.                                   24
000640                     25  CLR-PA-REL          PIC X.                     24
000650                     25  CLR-PA-ID-NAME      PIC X(06).                 25
000660             15  CLR-SYS-ID                  PIC X.                     31
000670                 88  CLR-DENTAL              VALUE 'D'.                   
000680                 88  CLR-DISABILITY          VALUE 'K' 'L'.               
000690                 88  CLR-STD                 VALUE 'K'.                   
000700                 88  CLR-LTD                 VALUE 'L'.                   
000710                 88  CLR-MEDICAL             VALUE 'M'.                   
000720             15  CLR-CL-KEY1.                                           32
000730                 20  CLR-CL-ID               PIC X(09).                 32
000740                 20  CLR-CL-GEN-X.                                      41
000750                     25  CLR-CL-GEN          PIC 99.                    41
000760         10  CLR-KEY-FIL                     PIC XX.                    43
000770*    SKIP2                                                                
000780     05  CLR-ODO-CTRS.                                                  45
000790         10  CLR-C-ODO-CTR                   PIC S9(4)    COMP.         45
000800         10  CLR-NFD-ODO-CTR                 PIC S9(4)    COMP.         47
000810         10  CLR-FILL-ODO-CTR                PIC S9(4)    COMP.         49
000820         10  CLR-CUST-ODO-CTR                PIC S9(4)    COMP.         51
000830*    SKIP2                                                                
Y2K  C     05  CLR-FIXED-DATA.                                                53
Y2K  C         10  FILLER                          PIC X(38).                 53
000860         10  CLR-STATUS                      PIC X(02).                 89
Y2K  C         10  FILLER                          PIC X(530).                91
000880*    SKIP2                                                                
000890     05  CLR-C-INFO-G.                                                 601
Y2K  C         10  CLR-C-INFO                      PIC X(102)                601
000910                           OCCURS  0  TO  3  TIMES                        
000920                           DEPENDING ON CLR-C-ODO-CTR.                    
000930*    SKIP2                                                                
000940     05  CLR-NON-FIXED-DATA.                                             1
000950         10  CLR-NFD-BYTE                    PIC X                       1
Y2K  C                           OCCURS  0  TO  3262  TIMES                     
000970                           DEPENDING ON CLR-NFD-ODO-CTR.                  
000980*    SKIP2                                                                
000990     05  CLR-FILL-AREA-G.                                             4097
001000         10  CLR-FILL-BYTE                   PIC X                    4097
Y2K  C                           OCCURS  0  TO  402  TIMES                      
001020                           DEPENDING ON CLR-FILL-ODO-CTR.                 
001030*    SKIP2                                                                
001040     05  CLR-CUST-AREA-G.                                             8193
001050         10  CLR-CUST-BYTE                   PIC X                    8193
001060                           OCCURS  0  TO  100  TIMES                      
001070                           DEPENDING ON CLR-CUST-ODO-CTR.                 
001080*    SKIP1                                                                
001090**********************************************************                
001100*            END OF ***  C F E R R C L R ***             *                
001110**********************************************************                
