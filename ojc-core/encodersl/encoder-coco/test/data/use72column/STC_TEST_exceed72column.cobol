       01  HQMR-DG-RECORD.                                                      
      *****************************************************************         
      * COPYBOOK ID: HQMR2RC  - HEALTHQUEST TO DATAGATE MEDICAL RECORD*         
      *              INTERFACE FILE.  FOR BATCH INTERFACE.            *         
      *****************************************************************         
           03  HQMR-DG-FIXED-INFO.                                              
               05  HQMR-DG-REC-KEY.                                             
                   10  HQMR-DG-HOSP                 PIC X.                      
                   10  HQMR-DG-ACCT-NO-PFX          PIC X.                      
                   10  HQMR-DG-ACCT-NO              PIC S9(13).                 
                   10  HQMR-DG-ADM-VST-DATE.                                    
                       15 HQMR-DG-ADM-VST-CC        PIC XX.                     
                       15 HQMR-DG-ADM-VST-YY        PIC XX.                     
                       15 HQMR-DG-ADM-VST-MM        PIC XX.                     
                       15 HQMR-DG-ADM-VST-DD        PIC XX.                     
                   10  HQMR-DG-PAT-PROCESS-TYPE     PIC X.                      
                   10  HQMR-DG-PAT-TYPE             PIC X(2).                   
                   10  HQMR-DG-REC-TYPE             PIC X.                      
                       88 HQMR-DG-ABSTRACT          VALUE 'A'.                  
                   10  HQMR-DG-REC-SEQ              PIC 99.                     
               05  HQMR-DG-MED-REC-NO               PIC S9(9).                  
               05  HQMR-DG-ABSTRACT-NO              PIC S9(10).                 
               05  HQMR-DG-PAT-DEMO-INFO.                                       
                   10  HQMR-DG-SSN                  PIC S9(9).                  
                   10  HQMR-DG-PAT-NAME             PIC X(25).                  
                   10  HQMR-DG-PAT-NAME-TITLE       PIC X(3).                   
                   10  HQMR-DG-PAT-NAME-SUFFIX      PIC X(3).                   
                   10  HQMR-DG-ADDR1                PIC X(25).                  
                   10  HQMR-DG-ADDR2                PIC X(25).                  
                   10  HQMR-DG-CITY                 PIC X(18).                  
                   10  HQMR-DG-STATE                PIC XX.                     
                   10  HQMR-DG-ZIP                  PIC X(9).                   
                   10  HQMR-DG-ZIP-FILLER           PIC X(2).                   
                   10  HQMR-DG-COUNTY               PIC X(5).                   
                   10  HQMR-DG-COUNTRY              PIC X(3).                   
                   10  HQMR-DG-PLAN-DISTRICT        PIC X(4).                   
                   10  HQMR-DG-CENSUS-TRACT         PIC X(6).                   
                   10  HQMR-DG-PHONE                PIC X(10).                  
                   10  HQMR-DG-DOB                  PIC S9(5).                  
                   10  HQMR-DG-AGE-YEARS            PIC S9(3).                  
                   10  HQMR-DG-BIRTHPLACE           PIC X(25).                  
                   10  HQMR-DG-CONFIDENTIAL-LEVEL   PIC X(2).                   
                   10  HQMR-DG-PRIOR-CPI-NBR-PFX    PIC X.                      
                   10  HQMR-DG-PRIOR-CPI-NBR        PIC S9(9).                  
                   10  HQMR-DG-SEX                  PIC X.                      
                   10  HQMR-DG-RACE                 PIC X.                      
                   10  HQMR-DG-NATIONALITY          PIC X(3).                   
                   10  HQMR-DG-MARITAL-STATUS       PIC X.                      
                   10  HQMR-DG-RELIGION             PIC X(3).                   
               05  HQMR-DG-ADMIT-VISIT-INFO.                                    
                   10  HQMR-DG-CHIEF-COMPLAINT      PIC X(55).                  
                   10  HQMR-DG-ONSET-DATE.                                      
                       15 HQMR-DG-ONSET-CC          PIC XX.                     
                       15 HQMR-DG-ONSET-YY          PIC XX.                     
                       15 HQMR-DG-ONSET-MM          PIC XX.                     
                       15 HQMR-DG-ONSET-DD          PIC XX.                     
                   10  HQMR-DG-ONSET-TIME           PIC S9(4).                  
                   10  HQMR-DG-ONSET-AMPM           PIC X.                      
                       88  HQMR-DG-ONSET-A          VALUE 'A'.                  
                       88  HQMR-DG-ONSET-P          VALUE 'P'.                  
                   10  HQMR-DG-LMP-DATE.                                        
                       15 HQMR-DG-LMP-CC            PIC XX.                     
                       15 HQMR-DG-LMP-YY            PIC XX.                     
                       15 HQMR-DG-LMP-MM            PIC XX.                     
                       15 HQMR-DG-LMP-DD            PIC XX.                     
                   10  HQMR-DG-REFER-INST-ID        PIC X(15).                  
                   10  HQMR-DG-REFER-INST-NAME      PIC X(25).                  
                   10  HQMR-DG-REFER-PHY-ID         PIC X(15).                  
                   10  HQMR-DG-REFER-PHY-NAME       PIC X(25).                  
                   10  HQMR-DG-PRIOR-REFER-IND      PIC X.                      
                       88  HQMR-DG-PRIOR            VALUE 'P'.                  
                       88  HQMR-DG-REFERRING        VALUE 'R'.                  
                   10  HQMR-DG-PAT-CLASS            PIC X(2).                   
                   10  HQMR-DG-FIN-CLASS            PIC X(2).                   
                   10  HQMR-DG-TEACH-CASE           PIC X.                      
                   10  HQMR-DG-ADM-TIME             PIC S9(4).                  
                   10  HQMR-DG-ADM-AMPM             PIC X.                      
                       88  HQMR-DG-ADM-A            VALUE 'A'.                  
                       88  HQMR-DG-ADM-P            VALUE 'P'.                  
                   10  HQMR-DG-ADM-HOUR             PIC 99.                     
                   10  HQMR-DG-READMIT              PIC X.                      
                   10  HQMR-DG-PRE-ADM-TEST         PIC X.                      
                   10  HQMR-DG-PRE-ADM-TEST-DATE.                               
                       15 HQMR-DG-PRE-ADM-TEST-CC   PIC XX.                     
                       15 HQMR-DG-PRE-ADM-TEST-YY   PIC XX.                     
                       15 HQMR-DG-PRE-ADM-TEST-MM   PIC XX.                     
                       15 HQMR-DG-PRE-ADM-TEST-DD   PIC XX.                     
                   10  HQMR-DG-ADM-PAVILION         PIC X.                      
                   10  HQMR-DG-ADM-SOURCE           PIC X.                      
                   10  HQMR-DG-ADM-TYPE             PIC X.                      
                   10  HQMR-DG-ADM-PHY-NO           PIC X(6).                   
               05  HQMR-DG-ACCIDENT-INFO.                                       
                   10  HQMR-DG-ACC-PLACE            PIC X(25).                  
                   10  HQMR-DG-ACC-DATE.                                        
                       15 HQMR-DG-ACC-CC            PIC XX.                     
                       15 HQMR-DG-ACC-YY            PIC XX.                     
                       15 HQMR-DG-ACC-MM            PIC XX.                     
                       15 HQMR-DG-ACC-DD            PIC XX.                     
                   10  HQMR-DG-ACC-TIME             PIC S9(4).                  
                   10  HQMR-DG-ACC-AMPM             PIC X.                      
                       88  HQMR-DG-ACC-A            VALUE 'A'.                  
                       88  HQMR-DG-ACC-P            VALUE 'P'.                  
                   10  HQMR-DG-ACC-TYPE             PIC X(2).                   
               05  HQMR-DG-DISCHARGE-INFO.                                      
                   10  HQMR-DG-DSCH-DATE.                                       
                       15 HQMR-DG-DSCH-CC           PIC XX.                     
                       15 HQMR-DG-DSCH-YY           PIC XX.                     
                       15 HQMR-DG-DSCH-MM           PIC XX.                     
                       15 HQMR-DG-DSCH-DD           PIC XX.                     
                   10  HQMR-DG-DSCH-TIME            PIC S9(4).                  
                   10  HQMR-DG-DSCH-AMPM            PIC X.                      
                       88  HQMR-DG-DSCH-A           VALUE 'A'.                  
                       88  HQMR-DG-DSCH-P           VALUE 'P'.                  
                   10  HQMR-DG-DSCH-HOUR            PIC 99.                     
                   10  HQMR-DG-DSCH-PAVILION        PIC X.                      
                   10  HQMR-DG-DSCH-ACCOM           PIC X(2).                   
                   10  HQMR-DG-DSCH-DISP            PIC X(2).                   
                   10  HQMR-DG-DSCH-STATUS          PIC X.                      
                   10  HQMR-DG-TRANS-TO-INST-ID     PIC X(15).                  
                   10  HQMR-DG-TRANS-TO-INST-NAME   PIC X(25).                  
                   10  HQMR-DG-DELIVERY             PIC X.                      
                   10  HQMR-DG-DELIVERY-DATE.                                   
                       15 HQMR-DG-DELIVERY-CC       PIC XX.                     
                       15 HQMR-DG-DELIVERY-YY       PIC XX.                     
                       15 HQMR-DG-DELIVERY-MM       PIC XX.                     
                       15 HQMR-DG-DELIVERY-DD       PIC XX.                     
                   10  HQMR-DG-GESTATION-WK         PIC 99.                     
                   10  HQMR-DG-PREG-ABORT-STERIL    PIC X.                      
                   10  HQMR-DG-BABY-INFO OCCURS 3 TIMES.                        
                       15  HQMR-DG-BIRTH-WEIGHT     PIC S9(4).                  
                       15  HQMR-DG-GRAMS-LBOZ       PIC X.                      
                           88  HQMR-DG-GRAMS        VALUE 'G'.                  
                           88  HQMR-DG-LBOZ         VALUE 'L'.                  
                       15  HQMR-DG-STILL-AUT        PIC X.                      
                       15  HQMR-DG-BABY-SEX         PIC X.                      
                       15  HQMR-DG-DELIVERY-TIME    PIC S9(4).                  
                       15  HQMR-DG-DELIVERY-AMPM    PIC X.                      
                           88  HQMR-DG-DELIVERY-A   VALUE 'A'.                  
                           88  HQMR-DG-DELIVERY-P   VALUE 'P'.                  
                   10  HQMR-DG-LOS                  PIC S9(3).                  
                   10  HQMR-DG-PRI-PAY-SOURCE       PIC X.                      
                   10  HQMR-DG-SEC-PAY-SOURCE       PIC X.                      
               05  HQMR-DG-CHARGES.                                             
                   10  HQMR-DG-ROUTINE              PIC S9(9)V99.               
                   10  HQMR-DG-LABORATORY           PIC S9(9)V99.               
                   10  HQMR-DG-PHARMACY             PIC S9(9)V99.               
                   10  HQMR-DG-RADIOLOGY            PIC S9(9)V99.               
                   10  HQMR-DG-OTH-ANCILLARY        PIC S9(9)V99.               
                   10  HQMR-DG-TOTAL                PIC S9(9)V99.               
                   10  HQMR-DG-TOTAL-COVERED        PIC S9(9)V99.               
               05  HQMR-DG-SURG-WITHIN-6HR          PIC X.                      
               05  HQMR-DG-MAJOR-PHY-NO             PIC X(6).                   
               05  HQMR-DG-MAJOR-SERV               PIC X(3).                   
               05  HQMR-DG-ABSTRACTOR-ID            PIC X(3).                   
               05  HQMR-DG-ABSTRACTOR-DATE.                                     
                   10 HQMR-DG-ABSTRACTOR-CC         PIC XX.                     
                   10 HQMR-DG-ABSTRACTOR-YY         PIC XX.                     
                   10 HQMR-DG-ABSTRACTOR-MM         PIC XX.                     
                   10 HQMR-DG-ABSTRACTOR-DD         PIC XX.                     
               05  HQMR-DG-CODER-ID                 PIC X(3).                   
               05  HQMR-DG-CODER-DATE.                                          
                   10 HQMR-DG-CODER-CC              PIC XX.                     
                   10 HQMR-DG-CODER-YY              PIC XX.                     
                   10 HQMR-DG-CODER-MM              PIC XX.                     
                   10 HQMR-DG-CODER-DD              PIC XX.                     
               05  HQMR-DG-LIV-ARRANG               PIC X.                      
               05  HQMR-DG-CASE-CATEGORY            PIC X.                      
               05  HQMR-DG-MODE-ARRIVAL             PIC X.                      
               05  HQMR-DG-FOLLOW-UP                PIC X.                      
               05  HQMR-DG-DUR-MED-ATT              PIC S9(4).                  
               05  HQMR-DG-ADM-ACCT-NO-PFX          PIC X.                      
               05  HQMR-DG-ADM-ACCT-NO              PIC S9(13).                 
               05  HQMR-DG-MOST-RECENT-REG.                                     
                   10  HQMR-DG-MOST-REC-REG-DATE.                               
                       15 HQMR-DG-MOST-REC-REG-CC   PIC XX.                     
                       15 HQMR-DG-MOST-REC-REG-YY   PIC XX.                     
                       15 HQMR-DG-MOST-REC-REG-MM   PIC XX.                     
                       15 HQMR-DG-MOST-REC-REG-DD   PIC XX.                     
                   10  HQMR-DG-MOST-REC-REG-PAVILION PIC X.                     
               05  HQMR-DG-FOCUS-IND                PIC X.                      
                   88  HQMR-DG-FOCUSED              VALUE  'F'.                 
                   88  HQMR-DG-MANUALLY-UNFOCUSED   VALUE 'U'.                  
                   88  HQMR-DG-SYSTEM-UNFOCUSED     VALUE 'S'.                  
                   88  HQMR-DG-NOT-FOCUSED          VALUES "B|U|S".         
               05  HQMR-DG-EXPIRED-INFO.                                        
                   10  HQMR-DG-AUTOPSY-IND          PIC X.                      
                       88  HQMR-DG-AUTOPSY-PERFORMED VALUE 'Y'.                 
                   10  HQMR-DG-OR-IND               PIC X.                      
                       88  HQMR-DG-DIED-IN-OR         VALUE 'I'.                
                       88  HQMR-DG-DIED-POST-OP       VALUE 'P'.                
                   10  HQMR-DG-CORONER-IND          PIC X.                      
                       88  HQMR-DG-CORONER-CASE       VALUE 'Y'.                
               05  HQMR-DG-UNUSED-AREA.                                         
                   10  HQMR-DG-UNUSED-MREC          PIC X(143).                 
                   10  HQMR-DG-UNUSED-MREC-NYH                                  
                        REDEFINES HQMR-DG-UNUSED-MREC.                          
                       15 FILLER                    PIC X(133).                 
                       15 HQMR-DG-DNR-ORDER         PIC X.                      
                       15 HQMR-DG-SPARCS-COMPLETE-IND PIC X.                    
                       15 HQMR-DG-PRE-HOSP-CARE-RPT PIC 9(8).                   
                   10  HQMR-DG-UNUSED-USER          PIC X(15).                  
                   10  HQMR-DG-UNUSED-USER-NYH                                  
                        REDEFINES HQMR-DG-UNUSED-USER.                          
                       15 HQMR-DG-MOTH-MED-REC-NUMBER PIC X(10).                
                       15 FILLER                    PIC X(5).                   
               05  HQMR-DG-COMPLETED-IND            PIC X.                      
               05  HQMR-DG-BATCH-PROCESS-IND        PIC X.                      
               05  HQMR-DG-LAST-UPDATE.                                         
                   10  HQMR-DG-LU-TERMINAL          PIC X(4).                   
                   10  HQMR-DG-LU-OPID              PIC X(3).                   
                   10  HQMR-DG-LU-DATE.                                         
                       15 HQMR-DG-LU-CC             PIC XX.                     
                       15 HQMR-DG-LU-YY             PIC XX.                     
                       15 HQMR-DG-LU-MM             PIC XX.                     
                       15 HQMR-DG-LU-DD             PIC XX.                     
                   10  HQMR-DG-LU-TIME              PIC S9(4).                  
                   10  HQMR-DG-LU-TRANSID           PIC X(4).                   
                   10  HQMR-DG-LU-MPACID            PIC X(10).                  
               05  HQMR-DG-DEPEND-COUNTERS.                                     
                   10  HQMR-DG-NO-OF-PRIOR          PIC S9(4).                  
                   10  HQMR-DG-NO-OF-ACC            PIC S9(4).                  
                   10  HQMR-DG-NO-OF-W-DIAG         PIC S9(4).                  
                   10  HQMR-DG-NO-OF-F-DIAG         PIC S9(4).                  
                   10  HQMR-DG-NO-OF-SURG-EP        PIC S9(4).                  
                   10  HQMR-DG-NO-OF-PROC           PIC S9(4).                  
                   10  HQMR-DG-NO-OF-ATT-PHY        PIC S9(4).                  
                   10  HQMR-DG-NO-OF-CONS-PHY       PIC S9(4).                  
                   10  HQMR-DG-NO-OF-SERV           PIC S9(4).                  
                   10  HQMR-DG-NO-OF-DRG            PIC S9(4).                  
           03  HQMR-DG-VARIABLE-INFO.                                           
               05  HQMR-DG-PRIOR-STAYS OCCURS   2 TIMES.                        
                   10  HQMR-DG-PRIOR-ADM-DATE.                                  
                       15 HQMR-DG-PRIOR-ADM-CC      PIC XX.                     
                       15 HQMR-DG-PRIOR-ADM-YY      PIC XX.                     
                       15 HQMR-DG-PRIOR-ADM-MM      PIC XX.                     
                       15 HQMR-DG-PRIOR-ADM-DD      PIC XX.                     
                   10  HQMR-DG-PRIOR-DSCH-DATE.                                 
                       15 HQMR-DG-PRIOR-DSCH-CC     PIC XX.                     
                       15 HQMR-DG-PRIOR-DSCH-YY     PIC XX.                     
                       15 HQMR-DG-PRIOR-DSCH-MM     PIC XX.                     
                       15 HQMR-DG-PRIOR-DSCH-DD     PIC XX.                     
                   10  HQMR-DG-PRIOR-INST-ID        PIC X(15).                  
                   10  HQMR-DG-PRIOR-INST-NAME      PIC X(25).                  
               05  HQMR-DG-ACCIDENT-CODES OCCURS 5 TIMES.                       
                   10  HQMR-DG-ACC-CODE             PIC X(7).                   
               05  HQMR-DG-WORK-DIAGNOSES OCCURS    10 TIMES.                   
                   10  HQMR-DG-W-DIAG-NO            PIC 99.                     
                   10  HQMR-DG-W-DIAG-TYPE          PIC X.                      
                   10  HQMR-DG-W-DIAG-CODE          PIC X(7).                   
                   10  HQMR-DG-W-DIAG-CDE-IND       PIC X.                      
                   10  HQMR-DG-W-DIAG-FR-DATE.                                  
                       15 HQMR-DG-W-DIAG-FR-CC      PIC XX.                     
                       15 HQMR-DG-W-DIAG-FR-YY      PIC XX.                     
                       15 HQMR-DG-W-DIAG-FR-MM      PIC XX.                     
                       15 HQMR-DG-W-DIAG-FR-DD      PIC XX.                     
                   10  HQMR-DG-W-DIAG-TO-DATE.                                  
                       15 HQMR-DG-W-DIAG-TO-CC      PIC XX.                     
                       15 HQMR-DG-W-DIAG-TO-YY      PIC XX.                     
                       15 HQMR-DG-W-DIAG-TO-MM      PIC XX.                     
                       15 HQMR-DG-W-DIAG-TO-DD      PIC XX.                     
                   10  HQMR-DG-W-DIAG-PHY-NO        PIC X(6).                   
               05  HQMR-DG-FINAL-DIAGNOSES OCCURS   25 TIMES.                   
                   10  HQMR-DG-F-DIAG-NO            PIC 99.                     
                   10  HQMR-DG-F-DIAG-TYPE          PIC X.                      
                   10  HQMR-DG-F-DIAG-CODE          PIC X(7).                   
                   10  HQMR-DG-F-DIAG-CDE-IND       PIC X.                      
                   10  HQMR-DG-F-DIAG-FR-DATE.                                  
                       15 HQMR-DG-F-DIAG-FR-CC      PIC XX.                     
                       15 HQMR-DG-F-DIAG-FR-YY      PIC XX.                     
                       15 HQMR-DG-F-DIAG-FR-MM      PIC XX.                     
                       15 HQMR-DG-F-DIAG-FR-DD      PIC XX.                     
                   10  HQMR-DG-F-DIAG-TO-DATE.                                  
                       15 HQMR-DG-F-DIAG-TO-CC      PIC XX.                     
                       15 HQMR-DG-F-DIAG-TO-YY      PIC XX.                     
                       15 HQMR-DG-F-DIAG-TO-MM      PIC XX.                     
                       15 HQMR-DG-F-DIAG-TO-DD      PIC XX.                     
                   10  HQMR-DG-F-DIAG-PHY-NO        PIC X(6).                   
               05  HQMR-DG-SURGICAL-EPISODES OCCURS 10 TIMES.                   
                   10  HQMR-DG-SURG-EP-NO           PIC 99.                     
                   10  HQMR-DG-SURG-EP-DATE.                                    
                       15 HQMR-DG-SURG-EP-CC        PIC XX.                     
                       15 HQMR-DG-SURG-EP-YY        PIC XX.                     
                       15 HQMR-DG-SURG-EP-MM        PIC XX.                     
                       15 HQMR-DG-SURG-EP-DD        PIC XX.                     
                   10  HQMR-DG-SURG-EP-FR-TIME      PIC S9(4).                  
                   10  HQMR-DG-SURG-EP-FR-AMPM      PIC X.                      
                       88  HQMR-DG-SURG-EP-FR-A     VALUE 'A'.                  
                       88  HQMR-DG-SURG-EP-FR-P     VALUE 'P'.                  
                   10  HQMR-DG-SURG-EP-TO-TIME      PIC S9(4).                  
                   10  HQMR-DG-SURG-EP-TO-AMPM      PIC X.                      
                       88  HQMR-DG-SURG-EP-TO-A     VALUE 'A'.                  
                       88  HQMR-DG-SURG-EP-TO-P     VALUE 'P'.                  
                   10  HQMR-DG-SURG-EP-PHY-NO OCCURS 5 TIMES PIC X(6).                   
                   10  HQMR-DG-SURG-EP-ANES OCCURS 3 TIMES.                     
                       15  HQMR-DG-SURG-EP-AN-EVAL  PIC X.                      
                       15  HQMR-DG-SURG-EP-AN-DUR   PIC S9(4).                  
                       15  HQMR-DG-SURG-EP-AN-METH  PIC X.                      
                       15  HQMR-DG-SURG-EP-AN-AGNT  PIC X.                      
                       15  HQMR-DG-SURG-EP-AN-CMPL  PIC X.                      
                       15  HQMR-DG-SURG-EP-AN-ANEST-NO PIC X(6).                
                       15  HQMR-DG-SURG-EP-AN-ANEST-IND PIC X.                  
               05  HQMR-DG-PROCEDURES OCCURS 25 TIMES.                          
                   10  HQMR-DG-PROC-EP-NO           PIC 99.                     
                   10  HQMR-DG-PROC-NO              PIC 99.                     
                   10  HQMR-DG-PROC-TYPE            PIC X.                      
                   10  HQMR-DG-PROC-CODE-TYPE       PIC X.                      
                   10  HQMR-DG-PROC-CODE            PIC X(7).                   
                   10  HQMR-DG-PROC-DATE.                                       
                       15 HQMR-DG-PROC-CC           PIC XX.                     
                       15 HQMR-DG-PROC-YY           PIC XX.                     
                       15 HQMR-DG-PROC-MM           PIC XX.                     
                       15 HQMR-DG-PROC-DD           PIC XX.                     
                   10  HQMR-DG-PROC-LOC             PIC X(4).                   
                   10  HQMR-DG-PROC-PHY-NO          PIC X(6).                   
                   10  HQMR-DG-PROC-TIS-TYPE        PIC X.                      
                   10  HQMR-DG-PROC-ABN-FIND        PIC X.                      
                   10  HQMR-DG-PROC-ANES.                                       
                       15  HQMR-DG-PROC-AN-METH     PIC X.                      
                       15  HQMR-DG-PROC-AN-AGNT     PIC X.                      
                   10  HQMR-DG-PROC-OR-IND          PIC X.                      
                       88  HQMR-DG-PROC-OUT-OF-SCOPE  VALUE 'O'.                
                       88  HQMR-DG-PROC-NON-ASC       VALUE 'N'.                
                       88  HQMR-DG-PROC-ASC           VALUES "1|2|3|4|5|6|7|8".        
                       88  HQMR-DG-PROC-ASC-GROUP-1   VALUE '1'.                
                       88  HQMR-DG-PROC-ASC-GROUP-2   VALUE '2'.                
                       88  HQMR-DG-PROC-ASC-GROUP-3   VALUE '3'.                
                       88  HQMR-DG-PROC-ASC-GROUP-4   VALUE '4'.                
                       88  HQMR-DG-PROC-ASC-GROUP-5   VALUE '5'.                
                       88  HQMR-DG-PROC-ASC-GROUP-6   VALUE '6'.                
                       88  HQMR-DG-PROC-ASC-GROUP-7   VALUE '7'.                
                       88  HQMR-DG-PROC-ASC-GROUP-8   VALUE '8'.                
                   10  HQMR-DG-PROC-FILLER          PIC X.                      
               05  HQMR-DG-ATTEND-PHYSICIANS OCCURS 10 TIMES.                   
                   10  HQMR-DG-ATT-PHY-NO           PIC X(6).                   
                   10  HQMR-DG-ATT-FR-DATE.                                     
                       15 HQMR-DG-ATT-FR-CC         PIC XX.                     
                       15 HQMR-DG-ATT-FR-YY         PIC XX.                     
                       15 HQMR-DG-ATT-FR-MM         PIC XX.                     
                       15 HQMR-DG-ATT-FR-DD         PIC XX.                     
                   10  HQMR-DG-ATT-TO-DATE.                                     
                       15 HQMR-DG-ATT-TO-CC         PIC XX.                     
                       15 HQMR-DG-ATT-TO-YY         PIC XX.                     
                       15 HQMR-DG-ATT-TO-MM         PIC XX.                     
                       15 HQMR-DG-ATT-TO-DD         PIC XX.                     
               05  HQMR-DG-CONSULT-PHYSICIANS OCCURS 10 TIMES.                  
                   10  HQMR-DG-CONS-PHY-NO          PIC X(6).                   
                   10  HQMR-DG-CONS-PHY-NAME        PIC X(25).                  
                   10  HQMR-DG-CONS-FR-DATE.                                    
                       15 HQMR-DG-CONS-FR-CC        PIC XX.                     
                       15 HQMR-DG-CONS-FR-YY        PIC XX.                     
                       15 HQMR-DG-CONS-FR-MM        PIC XX.                     
                       15 HQMR-DG-CONS-FR-DD        PIC XX.                     
                   10  HQMR-DG-CONS-TO-DATE.                                    
                       15 HQMR-DG-CONS-TO-CC        PIC XX.                     
                       15 HQMR-DG-CONS-TO-YY        PIC XX.                     
                       15 HQMR-DG-CONS-TO-MM        PIC XX.                     
                       15 HQMR-DG-CONS-TO-DD        PIC XX.                     
                   10  HQMR-DG-CONS-TYPE            PIC X.                      
                   10  HQMR-DG-CONS-SERV            PIC X(3).                   
                   10  HQMR-DG-CONS-NO              PIC 99.                     
               05  HQMR-DG-SERVICES OCCURS 20 TIMES.                            
                   10  HQMR-DG-SERV-CODE            PIC X(3).                   
                   10  HQMR-DG-SERV-NURS-STA        PIC X(4).                   
                   10  HQMR-DG-SERV-ENTER-DATE.                                 
                       15 HQMR-DG-SERV-ENTER-CC     PIC XX.                     
                       15 HQMR-DG-SERV-ENTER-YY     PIC XX.                     
                       15 HQMR-DG-SERV-ENTER-MM     PIC XX.                     
                       15 HQMR-DG-SERV-ENTER-DD     PIC XX.                     
                   10  HQMR-DG-SERV-LEFT-DATE.                                  
                       15 HQMR-DG-SERV-LEFT-CC      PIC XX.                     
                       15 HQMR-DG-SERV-LEFT-YY      PIC XX.                     
                       15 HQMR-DG-SERV-LEFT-MM      PIC XX.                     
                       15 HQMR-DG-SERV-LEFT-DD      PIC XX.                     
                   10  HQMR-DG-SERV-DAYS            PIC S9(3).                  
                   10  HQMR-DG-SERV-NURS-WORKLOAD   PIC S9(3)V99.               
                   10  HQMR-DG-SERV-NURS-ACUITY     PIC S9(3)V99.               
               05  HQMR-DG-DIAG-RELATED-GROUPS OCCURS 4 TIMES.                  
                   10  HQMR-DG-DRG-TYPE             PIC X.                      
                   10  HQMR-DG-DRG-DRG              PIC X(3).                   
                   10  HQMR-DG-DRG-MDC              PIC XX.                     
                   10  HQMR-DG-DRG-AVG-LOS          PIC S9(3)V99.               
                   10  HQMR-DG-DRG-MEAN-LOS         PIC S9(3)V99.               
                   10  HQMR-DG-DRG-REL-WEIGHT       PIC S9(3)V9(4).             
                   10  HQMR-DG-DRG-COST-THRESHOLD   PIC S9(7)V99.               
                   10  HQMR-DG-DRG-RTC              PIC X.                      
                   10  HQMR-DG-DRG-MPR              PIC X(7).                   
                   10  HQMR-DG-DRG-MPR-NO           PIC 99.                     
                   10  HQMR-DG-DRG-ADX              PIC X(7).                   
                   10  HQMR-DG-DRG-ADX-NO           PIC 99.                     
                   10  HQMR-DG-DRG-SDX              PIC X(7).                   
                   10  HQMR-DG-DRG-SDX-NO           PIC 99.                     
                   10  HQMR-DG-DRG-OUTLIER          PIC X.                      
                   10  HQMR-DG-DRG-EST-REIMB        PIC S9(9)V99.               
                   10  HQMR-DG-DRG-EST-OUTLIER-REIMB PIC S9(9)V99.              
                05 HQMR-DG-AMBSURG.                                             
                   10  HQMR-DG-AS-TIME-IN           PIC 9(4).                   
                   10  HQMR-DG-AS-TIME-OUT          PIC 9(4).                   
                   10  HQMR-DG-AS-TIME-ELAPSED      PIC X(4).                   
