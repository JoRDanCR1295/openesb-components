000050*****************************************************************         
000100* YEAR 2000 COMPLIANT  ?  MM/DD/CCYY   REVIEWED BY: ______      *         
000150* YEAR 2000 UPDATED    _  MM/DD/CCYY    UPDATED BY: ______      *         
000200*****************************************************************         
000250* FTL - FORMATED TRANSACTION LOG.  CREATED BY STL EXTRACT.       *        
000300*                                  AND BY DEBIT DTF EXTRACT      *        
000350*----------------------------------------------------------------* D   N |
000400*                         CHANGE HISTORY                         *   P   |
000450*----------------------------------------------------------------*   L   |
000500* MOD DATE  BY WHOM    REASON    CHANGE MADE                              
000550*                                                                         
000550* 03/29/00  F. Braden  RTN 2433  Variable support                         
000600*----------------------------------------------------------------* |   |  
000650                                                                          
000700*----------------------------------------------------------------* |   |  
000750*    RECORD HEADER - COMMON DATA                                 * |   |  
000800*----------------------------------------------------------------* |   |  
000825     01  BXCFTLMS.
000850     03  :FTL:-MS-COMMON-DATA.                                     V   V  
000900         05  :FTL:-MS-REC-TYPE               PIC  X(001).         001-001 
000950             88  :FTL:-MS-HEADER             VALUE 'H'.                   
001000             88  :FTL:-MS-AUTHORIZATION      VALUE '0'.                   
001050             88  :FTL:-MS-FILE-MAINTENANCE   VALUE '1'.                   
001100             88  :FTL:-MS-CP-NONPACM         VALUE '3'.                   
EPAY1A             88  :FTL:-MS-CONTINUATION       VALUE '4'.                   
001200             88  :FTL:-MS-MIP-BILLING-STATS  VALUE '5'.                   
001250             88  :FTL:-MS-MIP-ONUS-STATS     VALUE '6'.                   
001300             88  :FTL:-MS-ARS-ADVICE         VALUE '7'.                   
001350             88  :FTL:-MS-INTER-VIC          VALUE '8'.                   
001400             88  :FTL:-MS-REJECTED-MSG       VALUE '9'.                   
SUSEG              88  :FTL:-MS-SUSEGMENT-MSG      VALUE 'S'.                   
001500             88  :FTL:-MS-TRAILER            VALUE 'T'.                   
001550         05  :FTL:-MS-VIC                    PIC  X(001).         002-002 
001600             88  :FTL:-MS-VIC-OCW            VALUE 'A'.                   
001650             88  :FTL:-MS-VIC-OCE            VALUE 'B'.                   
001700             88  :FTL:-MS-VIC-OCEMA          VALUE 'C'.                   
001750             88  :FTL:-MS-VIC-OCB            VALUE 'D'.                   
001800             88  :FTL:-MS-VIC-OCAP           VALUE 'E'.                   
001850         05  :FTL:-MS-DATE-YYDDD             PIC S9(005) COMP-3.  003-005 
001900         05  :FTL:-MS-TIME-HHMMSS            PIC S9(006) COMP-3.  006-009 
001950         05  :FTL:-MS-SRCE-STATION           PIC S9(006) COMP-3.  010-013 
002000*----------------------------------------------------------------*        
002050* STL EXTRACT FILE TYPE 5 RECORD.  BILLING STATISTICS SEGMENTS.  *        
002100*----------------------------------------------------------------*        
002150     03  :FTL:-MS-BILLING-STATS-RECORD.                                   
002200         05  :FTL:-MS-BILLING-DATA.                                       
002250             10  :FTL:-MS-ACQ-PCR            PIC  X(004).         014-017 
002300             10  :FTL:-MS-BILLING-COUNTS.                                 
002350                 15  :FTL:-MS-COUNT          PIC S9(004) COMP.    018-019 
002400                 15  :FTL:-MS-ISS-BIN        PIC S9(006) COMP-3.  020-023 
002450                 15  :FTL:-MS-ISS-INST-ID    PIC  X(011).         024-034 
002500              10  :FTL:-MS-ACQ-PCR-COUNTRY   PIC  9(003).         035-037 
002550              10  :FTL:-MS-ACQ-PCR-REGION    PIC  X(001).         038-038 
002600              10  :FTL:-MS-TOD               PIC  X(012).         039-050 
002650              10  :FTL:-MS-TOD-REDEF         REDEFINES                    
002700                  :FTL:-MS-TOD.                                           
002750                  15  :FTL:-MS-TOD-MMDDYY    PIC  9(006).         039-044 
002800                  15  :FTL:-MS-TOD-HHMMSS    PIC  9(006).         045-050 
002850              10  :FTL:-MS-BLK               PIC  X(002).         051-052 
002900              10  :FTL:-MS-BLK-REDEF         REDEFINES                    
002950                  :FTL:-MS-BLK.                                           
003000                  15  :FTL:-MS-BLK-NUM       PIC  9(004) COMP.    051-052 
