000050*****************************************************************         
000100* YEAR 2000 COMPLIANT  ?  MM/DD/CCYY   REVIEWED BY: ______      *         
000150* YEAR 2000 UPDATED    _  MM/DD/CCYY    UPDATED BY: ______      *         
000200*****************************************************************         
000250* FTL - FORMATED TRANSACTION LOG.  CREATED BY STL EXTRACT.       *        
000300*                                  AND BY DEBIT DTF EXTRACT      *        
000350*----------------------------------------------------------------* D   N |
000400*                         CHANGE HISTORY                         *   P   |
000450*----------------------------------------------------------------*   L   |
000500* MOD DATE  BY WHOM   REASON     CHANGE MADE                         A   |
000550*                                                                         
000600* 07/29/99  F. BRADEN WHIM       NEW COPY BOOK                            
000600* 03/29/00  F. BRADEN RTN 2433   Variable version                         
000650*----------------------------------------------------------------* |   |  
000700                                                                          
000750*----------------------------------------------------------------* |   |  
000800*    RECORD HEADER - COMMON DATA                                 * |   |  
000850*----------------------------------------------------------------* |   |  
000875     01  BXCFTLCP.
000900     03  :FTL:-CP-COMMON-DATA.                                     V   V  
000950         05  :FTL:-CP-REC-TYPE               PIC  X(001).         001-001 
001000             88  :FTL:-CP-HEADER             VALUE 'H'.                   
001050             88  :FTL:-CP-AUTHORIZATION      VALUE '0'.                   
001100             88  :FTL:-CP-FILE-MAINTENANCE   VALUE '1'.                   
001150             88  :FTL:-CP-CP-NONPACM         VALUE '3'.                   
EPAY1A             88  :FTL:-CP-CONTINUATION       VALUE '4'.                   
001250             88  :FTL:-CP-MIP-BILLING-STATS  VALUE '5'.                   
001300             88  :FTL:-CP-MIP-ONUS-STATS     VALUE '6'.                   
001350             88  :FTL:-CP-ARS-ADVICE         VALUE '7'.                   
001400             88  :FTL:-CP-INTER-VIC          VALUE '8'.                   
001450             88  :FTL:-CP-REJECTED-MSG       VALUE '9'.                   
SUSEG              88  :FTL:-CP-SUSEGMENT-MSG      VALUE 'S'.                   
001550             88  :FTL:-CP-TRAILER            VALUE 'T'.                   
001600         05  :FTL:-CP-VIC                    PIC  X(001).         002-002 
001650             88  :FTL:-CP-VIC-OCW            VALUE 'A'.                   
001700             88  :FTL:-CP-VIC-OCE            VALUE 'B'.                   
001750             88  :FTL:-CP-VIC-OCEMA          VALUE 'C'.                   
001800             88  :FTL:-CP-VIC-OCB            VALUE 'D'.                   
001850             88  :FTL:-CP-VIC-OCAP           VALUE 'E'.                   
001900         05  :FTL:-CP-DATE-YYDDD             PIC S9(005) COMP-3.  003-005 
001950         05  :FTL:-CP-TIME-HHMMSS            PIC S9(006) COMP-3.  006-009 
002000         05  :FTL:-CP-SRCE-STATION           PIC S9(006) COMP-3.  010-013 
002050*----------------------------------------------------------------*        
002100* FTL EXTRACT FILE TYPE 3 RECORD. FTL PROCESSOR DATA 'CP' SEGMENT*        
002150*----------------------------------------------------------------*        
002200     03  :FTL:-CP-PCR-DATA-PCR-RECORD.                            014-600 
002250         05  :FTL:-CP-PCR-DATA OCCURS 8 TIMES.                            
002300             10  :FTL:-CP-PCR-PCR            PIC  X(04).                  
002350             10  :FTL:-CP-PCR-HIGH-DIV-LVL   PIC  X(02).                  
002400             10  :FTL:-CP-PCR-DEB-VOL        PIC  S9(09) COMP-3.          
002450             10  :FTL:-CP-PCR-ACQ-VOL        PIC  S9(09) COMP-3.          
002500             10  :FTL:-CP-PCR-FM-VOL         PIC  S9(09) COMP-3.          
002550             10  :FTL:-CP-PCR-ADV-VOL        PIC  S9(09) COMP-3.          
002600             10  :FTL:-CP-PCR-OTH-VOL        PIC  S9(09) COMP-3.          
002650             10  FILLER                      PIC  X(04).                  
