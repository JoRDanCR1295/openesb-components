000050* INSTALL.PROD VIP/BX BXCFTLN 01.60                                       
000100*****************************************************************         
000150* YEAR 2000 COMPLIANT  ?  MM/DD/CCYY   REVIEWED BY: ______      *         
000200* YEAR 2000 UPDATED    _  MM/DD/CCYY    UPDATED BY: ______      *         
000250*****************************************************************         
000300* FTL - FORMATED TRANSACTION LOG. CREATED BY STL EXTRACT.                 
000350*                                  AND BY DEBIT DTF EXTRACT               
000400*----------------------------------------------------------------* D   N |
000450*                         CHANGE HISTORY                         *   P   |
000500*----------------------------------------------------------------*   L   |
000550* MOD DATE  BY WHOM    REASON    CHANGE MADE                              
000600*                                                                         
000650* 07/23/99  F. BRADEN  staging   NEW FORMAT BXCFTLHR                      
000650* 03/29/00  F. BRADEN  RTN 2433  Variable support                         
000700*----------------------------------------------------------------* |   |  
000750                                                                          
000800*----------------------------------------------------------------* |   |  
000850*    RECORD HEADER - COMMON DATA                                 * |   |  
000900*----------------------------------------------------------------* |   |  
000925     01  BXCFTLHR.
000950     03  :FTL:-HR-COMMON-DATA.                                     V   V  
001000         05  :FTL:-HR-REC-TYPE               PIC  X(001).         001-001 
001050             88  :FTL:-HR-HEADER             VALUE 'H'.                   
001100             88  :FTL:-HR-AUTHORIZATION      VALUE '0'.                   
001150             88  :FTL:-HR-FILE-MAINTENANCE VALUE '1'.                     
001200             88  :FTL:-HR-CP-NONPACM         VALUE '3'.                   
EPAY1A             88  :FTL:-HR-CONTINUATION       VALUE '4'.                   
001300             88  :FTL:-HR-MIP-BILLING-STATS VALUE '5'.                    
001350             88  :FTL:-HR-MIP-ONUS-STATS     VALUE '6'.                   
001400             88  :FTL:-HR-ARS-ADVICE         VALUE '7'.                   
001450             88  :FTL:-HR-INTER-VIC          VALUE '8'.                   
001500             88  :FTL:-HR-REJECTED-MSG       VALUE '9'.                   
SUSEG              88  :FTL:-HR-SUSEGMENT-MSG      VALUE 'S'.                   
001600             88  :FTL:-HR-TRAILER            VALUE 'T'.                   
001650         05  :FTL:-HR-VIC                    PIC  X(001).         002-002 
001700             88  :FTL:-HR-VIC-OCW            VALUE 'A'.                   
001750             88  :FTL:-HR-VIC-OCE            VALUE 'B'.                   
001800             88  :FTL:-HR-VIC-OCEMA          VALUE 'C'.                   
001850             88  :FTL:-HR-VIC-OCB            VALUE 'D'.                   
001900             88  :FTL:-HR-VIC-OCAP           VALUE 'E'.                   
001950         05  :FTL:-HR-DATE-YYDDD             PIC S9(005) COMP-3.  003-005 
002000         05  :FTL:-HR-TIME-HHMMSS            PIC S9(006) COMP-3.  006-009 
002050         05  :FTL:-HR-SRCE-STATION           PIC S9(006) COMP-3.  010-013 
002100*----------------------------------------------------------------*        
002150*    HEADER RECORD.  TYPE 'H'.                                   *        
002200*----------------------------------------------------------------*        
002250     03  :FTL:-HEADER-RECORD.                                             
002300         05  :FTL:-HR-DATE                   PIC  9(006).         014-019 
002350         05  :FTL:-HR-MEDIA-DATE      REDEFINES :FTL:-HR-DATE             
002400                                             PIC  9(006).                 
002450         05  :FTL:-HR-DATE-YYMMDD     REDEFINES :FTL:-HR-DATE.            
002500             10  :FTL:-HR-DATE-YY            PIC  9(002).                 
002550             10  :FTL:-HR-DATE-MM            PIC  9(002).                 
002600             10  :FTL:-HR-DATE-DD            PIC  9(002).                 
002650         05  :FTL:-HR-RUN-DATE-YYMMDD        PIC  9(006).         020-025 
002700         05  :FTL:-HR-RUN-TIME-HHMMSS        PIC  9(006).         026-031 
002750         05  :FTL:-HR-VERSION                PIC  X(002).         032-033 
002800         05  :FTL:-HR-VERSION-COMPILED       PIC  X(020).         034-053 
002850         05  FILLER                          PIC  X(002).         054-055 
002900         05  :FTL:-HR-VIC-IDS                PIC  X(003).         056-058 
002950         05  FILLER                          PIC  X(001).         059-059 
003000         05  :FTL:-HR-BEGIN-ORDINAL-NO       PIC S9(008) COMP.    060-063 
003050         05  :FTL:-HR-END-ORDINAL-NO         PIC S9(008) COMP.    064-067 
003050         05  FILLER                          PIC  X(8125).        064-067 
