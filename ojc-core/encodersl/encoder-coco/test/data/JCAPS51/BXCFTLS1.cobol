000050*****************************************************************         
000100* YEAR 2000 COMPLIANT  ?  MM/DD/CCYY   REVIEWED BY: ______      *         
000150* YEAR 2000 UPDATED    _  MM/DD/CCYY    UPDATED BY: ______      *         
000200*****************************************************************         
000250* FTL - FORMATED TRANSACTION LOG.  CREATED BY STL EXTRACT.       *        
000300*                                  AND BY DEBIT DTF EXTRACT      *        
000350*----------------------------------------------------------------* D   N |
000400*                         CHANGE HISTORY                         *   P   |
000450*----------------------------------------------------------------*   L   |
000500* MOD DATE  BY WHOM  REASON FOR CHANGE - CHANGE MADE             *   A   |
000550*                                                                         
000600* 08/02/99  F. BRADEN   STAGING   COPY BOOK SPLIT                         
000600* 03/29/00  F. BRADEN   RTN 2433  Truncate filler for variable            
000650*                                                                         
000700*----------------------------------------------------------------* |   |  
000750                                                                          
000800*----------------------------------------------------------------* |   |  
000850*    RECORD HEADER - COMMON DATA                                 * |   |  
000900*----------------------------------------------------------------* |   | 
000925     01  BXCFTLS1. 
000950     03  :FTL:-S1-COMMON-DATA.                                     V   V  
001000         05  :FTL:-S1-REC-TYPE               PIC  X(001).         001-001 
001050             88  :FTL:-S1-HEADER             VALUE 'H'.                   
001100             88  :FTL:-S1-AUTHORIZATION      VALUE '0'.                   
001150             88  :FTL:-S1-FILE-MAINTENANCE   VALUE '1'.                   
001200             88  :FTL:-S1-CP-NONPACM         VALUE '3'.                   
EPAY1A             88  :FTL:-S1-CONTINUATION       VALUE '4'.                   
001300             88  :FTL:-S1-MIP-BILLING-STATS  VALUE '5'.                   
001350             88  :FTL:-S1-MIP-ONUS-STATS     VALUE '6'.                   
001400             88  :FTL:-S1-ARS-ADVICE         VALUE '7'.                   
001450             88  :FTL:-S1-INTER-VIC          VALUE '8'.                   
001500             88  :FTL:-S1-REJECTED-MSG       VALUE '9'.                   
SUSEG              88  :FTL:-S1-SUSEGMENT-MSG      VALUE 'S'.                   
001600             88  :FTL:-S1-TRAILER            VALUE 'T'.                   
001650         05  :FTL:-S1-VIC                    PIC  X(001).         002-002 
001700             88  :FTL:-S1-VIC-OCW            VALUE 'A'.                   
001750             88  :FTL:-S1-VIC-OCE            VALUE 'B'.                   
001800             88  :FTL:-S1-VIC-OCEMA          VALUE 'C'.                   
001850             88  :FTL:-S1-VIC-OCB            VALUE 'D'.                   
001900             88  :FTL:-S1-VIC-OCAP           VALUE 'E'.                   
001950         05  :FTL:-S1-DATE-YYDDD             PIC S9(005) COMP-3.  003-005 
002000         05  :FTL:-S1-TIME-HHMMSS            PIC S9(006) COMP-3.  006-009 
002050         05  :FTL:-S1-SRCE-STATION           PIC S9(006) COMP-3.  010-013 
002100*----------------------------------------------------------------*        
002150* STL EXTRACT FILE TYPE 6 RECORD.  S1 MIP  STATISTICS SEGMENTS.  *        
002200*----------------------------------------------------------------*        
002250     03  :FTL:-S1-ONUS-STATS.                                             
002300         05  :FTL:-S1-TRAN-CODE              PIC  X(002).         014-015 
002350         05  :FTL:-S1-DATA.                                               
002400            10  :FTL:-S1-ACQ-PCR             PIC  X(004).         016-019 
002450            10  :FTL:-S1-ON-US-COUNT-VISA    PIC  9(005).         020-024 
002500            10  :FTL:-S1-ON-US-COUNT-MC      PIC  9(005).         025-029 
002550            10  :FTL:-S1-ON-US-COUNT-OTHERS  PIC  9(005).         030-034 
002600            10  :FTL:-S1-LCS-FILE-UPDATES    PIC  9(006).         035-040 
002650            10  :FTL:-S1-SPOOL-BLOCKS-SENT   PIC  9(005).         041-045 
002700            10  :FTL:-S1-ACQ-PCR-COUNTRY     PIC  9(003).         046-048 
002750            10  :FTL:-S1-ACQ-PCR-REGION      PIC  X(001).         049-049 
