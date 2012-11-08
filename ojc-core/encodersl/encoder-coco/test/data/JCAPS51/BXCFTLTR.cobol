000050*****************************************************************         
000100* YEAR 2000 COMPLIANT  ?  MM/DD/CCYY   REVIEWED BY: ______      *         
000150* YEAR 2000 UPDATED    _  MM/DD/CCYY    UPDATED BY: ______      *         
000200*****************************************************************         
000250* FTL - FORMATED TRANSACTION LOG.  CREATED BY STL EXTRACT.       *        
000300*                                  AND BY DEBIT DTF EXTRACT      *        
000350*----------------------------------------------------------------* D   N |
000400*                         CHANGE HISTORY                         *   P   |
000450*----------------------------------------------------------------*   L   |
000500* MOD DATE  BY WHOM    REASON    CHANGE - CHANGE MADE             *   A   
|                                                                               
000550*                                                                         
000600* 07/23/99  F. BRADEN  staging   NEW FORMAT TRAILER                       
000600* 03/29/00  F. BRADEN  RTN 2433  Variable support                         
000650*----------------------------------------------------------------* |   |  
000700                                                                          
000750*----------------------------------------------------------------* |   |  
000800*    RECORD HEADER - COMMON DATA                                 * |   |  
000850*----------------------------------------------------------------* |   |  
000899     01  BXCFTLTR.
000900     03  :FTL:-TR-COMMON-DATA.                                     V   V  
000950         05  :FTL:-TR-REC-TYPE               PIC  X(001).         001-001 
001000             88  :FTL:-TR-HEADER             VALUE 'H'.                   
001050             88  :FTL:-TR-AUTHORIZATION      VALUE '0'.                   
001100             88  :FTL:-TR-FILE-MAINTENANCE   VALUE '1'.                   
001150             88  :FTL:-TR-CP-NONPACM         VALUE '3'.                   
EPAY1A             88  :FTL:-TR-CONTINUATION       VALUE '4'.                   
001250             88  :FTL:-TR-MIP-BILLING-STATS  VALUE '5'.                   
001300             88  :FTL:-TR-MIP-ONUS-STATS     VALUE '6'.                   
001350             88  :FTL:-TR-ARS-ADVICE         VALUE '7'.                   
001400             88  :FTL:-TR-INTER-VIC          VALUE '8'.                   
001450             88  :FTL:-TR-REJECTED-MSG       VALUE '9'.                   
SUSEG              88  :FTL:-TR-SUSEGMENT-MSG      VALUE 'S'.                   
001550             88  :FTL:-TR-TRAILER            VALUE 'T'.                   
001600         05  :FTL:-TR-VIC                    PIC  X(001).         002-002 
001650             88  :FTL:-TR-VIC-OCW            VALUE 'A'.                   
001700             88  :FTL:-TR-VIC-OCE            VALUE 'B'.                   
001750             88  :FTL:-TR-VIC-OCEMA          VALUE 'C'.                   
001800             88  :FTL:-TR-VIC-OCB            VALUE 'D'.                   
001850             88  :FTL:-TR-VIC-OCAP           VALUE 'E'.                   
001900         05  :FTL:-TR-DATE-YYDDD             PIC S9(005) COMP-3.  003-005 
001950         05  :FTL:-TR-TIME-HHMMSS            PIC S9(006) COMP-3.  006-009 
002000         05  :FTL:-TR-SRCE-STATION           PIC S9(006) COMP-3.  010-013 
002050*----------------------------------------------------------------*        
002100*    TRAILER RECORD.   TYPE 'T'.                                 *        
002150*----------------------------------------------------------------*        
002200     03  :FTL:-TRAILER-RECORD.                                            
002250         05  FILLER                          PIC  X(040).         014-053 
002300         05  :FTL:-TR-COUNTS.                                             
002350            10  :FTL:-TR-TYH-COUNT           PIC S9(009) COMP-3.  054-058 
002400            10  :FTL:-TR-TY0-COUNT           PIC S9(009) COMP-3.  059-063 
002450            10  :FTL:-TR-TY1-COUNT           PIC S9(009) COMP-3.  064-068 
002500            10  :FTL:-TR-TY2-COUNT           PIC S9(009) COMP-3.  069-073 
002550            10  :FTL:-TR-TY3-COUNT           PIC S9(009) COMP-3.  074-078 
002600            10  :FTL:-TR-TY4-COUNT           PIC S9(009) COMP-3.  079-083 
002650            10  :FTL:-TR-TY5-COUNT           PIC S9(009) COMP-3.  084-088 
002700            10  :FTL:-TR-TY6-COUNT           PIC S9(009) COMP-3.  089-093 
002750            10  :FTL:-TR-TY7-COUNT           PIC S9(009) COMP-3.  094-098 
002800            10  :FTL:-TR-TY8-COUNT           PIC S9(009) COMP-3.  099-103 
002850            10  :FTL:-TR-TY9-COUNT           PIC S9(009) COMP-3.  104-108 
002900            10  :FTL:-TR-TYT-COUNT           PIC S9(009) COMP-3.  109-113 
002950            10  :FTL:-TR-TOT-COUNT           PIC S9(009) COMP-3.  114-118 
SUSEG             10  :FTL:-TR-TYS-COUNT           PIC S9(009) COMP-3.  1040108 
003050         05  :FTL:-TR-COUNTS-REDEF    REDEFINES :FTL:-TR-COUNTS.   054-118
003100            10  :FTL:-TR-TYPE-COUNT          OCCURS 14 TIMES      0000000 
003150                                             PIC S9(009) COMP-3.          
003200         05  FILLER                          PIC  X(003).         119-121 
003250         05  :FTL:-TR-BEGIN-ORDINAL-NO       PIC S9(008) COMP.    122-125 
003300         05  :FTL:-TR-END-ORDINAL-NO         PIC S9(008) COMP.    126-129 
003400*----------------------------------------------------------------*        
