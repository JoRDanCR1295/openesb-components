       01  HQMR-DG-TEST.                                                      
      *****************************************************************         
      * COPYBOOK ID: HQMR2RC  - HEALTHQUEST TO DATAGATE MEDICAL RECORD*         
      *              INTERFACE FILE.  FOR BATCH INTERFACE.            *         
      *****************************************************************         
           03  HQMR-DG-FIXED-INFO.                                              
               05  HQMR-DG-REC-KEY.                                             
                   10  HQMR-DG-HOSP                 PIC X.                      
                   10  HQMR-DG-ADM-VST-DATE.                                    
                       15 HQMR-DG-ADM-VST-CC        PIC XX.                     
                       15 HQMR-DG-ADM-VST-DD        PIC XX.                     
                   10  HQMR-DG-PAT-PROCESS-TYPE     PIC X.                      
                   10  HQMR-DG-REC-TYPE             PIC X.                      
                       88 HQMR-DG-ABSTRACT          VALUES 
                                                          'A'.                  
                   10  HQMR-DG-REC-SEQ              PIC 99.                     
