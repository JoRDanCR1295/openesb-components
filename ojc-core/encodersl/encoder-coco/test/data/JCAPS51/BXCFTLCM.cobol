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
000610* 03/29/00  F. BRADEN RTN 2433   Make max size for variable recs          
000650*----------------------------------------------------------------* |   |  
000700                                                                          
003200*----------------------------------------------------------------*        
003250*    RECORD HEADER - COMMON DATA                                 *        
003300*----------------------------------------------------------------*   
003325     01  BXCFTLCM.     
003350     03  :FTL:-CM-COMMON-DATA.                                            
003400         05  :FTL:-CM-REC-TYPE               PIC  X(001).                 
003450             88  :FTL:-CM-HEADER             VALUE 'H'.                   
003500             88  :FTL:-CM-AUTHORIZATION      VALUE '0'.                   
003550             88  :FTL:-CM-FILE-MAINTENANCE   VALUE '1'.                   
003600             88  :FTL:-CM-CP-NONPACM         VALUE '3'.                   
003700             88  :FTL:-CM-MIP-BILLING-STATS  VALUE '5'.                   
003750             88  :FTL:-CM-MIP-ONUS-STATS     VALUE '6'.                   
003800             88  :FTL:-CM-ARS-ADVICE         VALUE '7'.                   
003850             88  :FTL:-CM-INTER-VIC          VALUE '8'.                   
003900             88  :FTL:-CM-REJECTED-MSG       VALUE '9'.                   
SUSEG              88  :FTL:-CM-SUSEGMENT-MSG      VALUE 'S'.                   
004000             88  :FTL:-CM-TRAILER            VALUE 'T'.                   
004050         05  :FTL:-CM-VIC                    PIC  X(001).                 
004100             88  :FTL:-CM-VIC-OCW            VALUE 'A'.                   
004150             88  :FTL:-CM-VIC-OCE            VALUE 'B'.                   
004200             88  :FTL:-CM-VIC-OCEMA          VALUE 'C'.                   
004250             88  :FTL:-CM-VIC-OCB            VALUE 'D'.                   
004300             88  :FTL:-CM-VIC-OCAP           VALUE 'E'.                   
001550             88  :FTL:-CM-TRAILER            VALUE 'T'.                   
001900         05  :FTL:-CM-DATE-YYDDD             PIC S9(005) COMP-3.  003-005 
001950         05  :FTL:-CM-TIME-HHMMSS            PIC S9(006) COMP-3.  006-009 
002000         05  :FTL:-CM-SRCE-STATION           PIC S9(006) COMP-3.  010-013 
002050*----------------------------------------------------------------*        
002100* VARIABLE DATA FOLLOWS                                                   
002150*----------------------------------------------------------------*        
002200     03  :FTL:-CM-VARIABLE-DATA PIC X(8175).                      014-600 
