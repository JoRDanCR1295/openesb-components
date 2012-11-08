      **==============================================================*         
      **                                                              *         
      **    COPY: P124CLAM (FORMERLY PCLAM124)   SYSTEM: PCDS         *         
      **                   (PCLAM124 WAS FORMERLY PCDSCLAM)           *         
      **                                                              *         
      ** ...................... CHANGE HISTORY ...................... *         
      **                                                              *         
      **   !ADD LATEST COMMENTS AT THE TOP OF CHANGE HISTORY!         *         
      **                                            PROJECTED PRODDATE*         
      ** USER:  START:   TICKET # AND COMMENTS                  YYMMDD*         
      ** ------ -------- ------------------------------------- -------*         
      ** MCO    20040917 04081228445 ADDED THE FOLLOWING       E0409__*         
      **                 FIELD: D1-PATIENT-ID-2                E0409__*         
      **                 CHANGED FILLER FROM 55 TO 25          E0409__*         
      ** LIBR'N 09/02/04 04081228438 HIPAA-BYPASS LEGACY CHGS  B0409__*         
      ** MCO    20040901 ADDED THE FOLLOWING FIELDS:           E040901*         
      **                 F6-SERV-LN-ORD-PROV-CONT-NM           E040901*         
      **                 F7-SERV-LN-ORD-PROV-TEL-EXT           E040901*         
0407JW** WALKER 07/28/04 99999999999 - 1. ADD TYPE 'KZ'               *         
0407JW**                    FOR CORN AND ERROR MESSAGES.              *         
0407JW**                 2. EXPAND SR-UNIQUE-JOBNUM TO 8              *         
      **                                                              *         
0312RR** RUBIN  12/29/03 03100801318 -                         E      *         
0312RR**                 ADD UNIQUE SUBMITTER ID TO CZ REC     E      *         
      ** LIBR'N 10/10/03 20031010-01 RENAMED P124CLAM          E031010*         
      **   HIPAA PATH 1 - MOVED TO PROD FOR FIRST TIME 17 APR  E030417*         
      ** JOV    02/19/03 20030113-07 COVENTRY-GROUP-25141      E030307*         
      **  SET UP NEW ID FOR PYR=25141. LEFT IN PLACE EDITS FOR E030307*         
      **  INACTIVE PYR=00340 (LEVEL 88 GRP-HEALTH-COVENTRY).   E030307*         
      ** JOV    02/04/03 20020531-03 SECURE-HEALTH (25830)     E030214*         
      ** JOV    02/04/03 20030130-02 COVENTRY-SPECIAL          E030214*         
      **        25126,25127,25128,25129,25130,25132,25133,     E030214*         
      **        25134,25135,25136,25137,25139,25140,25141,     E030214*         
      **        25142,25143,25144                              E030214*         
      ** JTL    01/29/03 20030122-12 APEX (34196)              E030203*         
      ** JOV    01/15/03 20020603-09 HEALTHNET-CA-ENC(95570)   B0301??*         
      ** JOV    01/06/03 20020612-01 WINTERBROOK-73159         B0301??*         
      ** JOV    11/27/02 20020107-02 ALLMERICA/69140 A.K.A.    E02121_*         
      **  NORTHERN DATA SYS. - COMPLETION OF 88 LEVEL NAME     E02121_*         
      **  NO CHANGE IN MCDV2CLM (BUT CHANGED OTHER COPYBOOKS)  E02121_*         
      ** JOV    11/12/02 20021001-02 MERCY HEALTH PLANS-43166  E021203*         
      ** JOV    10/14/02 20020820-03 MMO-CAREWORKS-29076       E021107*         
      ** JOV    10/14/02 20020820-03 MMO-CAREWORKS-10010       E021007*         
      ** CARMEL 01/16/03 CZ-CORN                                      *         
      ** JOV    08/26/02 20020807-07 SENTARA (54154)           E020917*         
      ** AWH    08/27/02 20020814-01 AVMED (59274)             E020916*         
      ** JOV    07/22/02 20020620-02 XANTUS (62153 62154)      B020731*         
      **        NOTE: CHG MADE TO V21-PHOENIX-HEALTHCARE-TN    B020731*         
      **              (ADDED 62154 TO PHOENIX-HEALTHCARE-TN)   B020731*         
      **              OLD NAME KEPT FOR EDIT COMPATIBILITY.    B020731*         
      ** MALLIK 07/24/02 20020712-04 CREATE P TO E INDICATOR   B020726*         
      **  IN B0 RECORD POS 185. VALUE = 'P' FOR PCDS TO EMCDS. B020726*         
      ** JOV    07/01/02 20011211-10 CARITEN HEALTH            B020702*         
      **                 PAYOR(62702 62073)                    B020702*         
      ** JOV    06/19/02 20020304-05 EMPLOYEE BENEFIT CONCEPTS B020621*         
      **                 (38241)                               B020621*         
      ** JOV    06/13/02 20020213-02 MANAGED HEALTH SVC (39187)B020617*         
      ** RDM    05/16/02 20020515-10 CARESOURCE, ID=31114      B020523*         
      ** JOV    05/16/02 20011129-14  INFOTRUST                B020517*         
      **                PYR=36364 38337 39185 42142 58246      B020517*         
      **                58247 58248 84141                      B020517*         
      ** RDM    05/14/02 20020503-14 CHC PAYER ID=48145.       B020516*         
      ** CARMEL 05/01/02 20011116-06 MERCY CARE (86052)               *         
      ** JOV    04/24/02 20011108-17 CORESOURCE-NC(35180)      B020428*         
      ** JOV    02/22/02 20020214-01 V21-ADVOCATE-ORIG ADDED   B022602*         
      **                 PAYR=(65093) - DIFFERINTIATE (36320)  B022602*         
      ** JOV    02/20/02 20020207-02 CIGNA DENTAL (10050)      B022602*         
      ** RDM    02/12/02 20020211-08 PYR=SB741,BCBSMO TO OKC.  B022602*         
      ** FBT    06/25/01 20010626-11 DATA NAME CLARIFICATION E6,X0    **        
      **==============================================================**        
                                                                                
                                                                                
      *****************************************************************         
      *                                                               *         
      *                   P 1 2 4 C L A M                             *         
      *    (RENAMED FROM  P C L A M 1 2 4  10 OCT 2003)               *         
      *                                                               *         
      *****************************************************************         
                                                                                
       01  WS-PCDSCLAIM-REC.                                                    
                                                                                
      *----------------------------------------------------------------*        
      *             COMMON DATA AREAS   (RT00)                         *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-00-RECORD.                                                  
               10  PCDS-00-REC-TYPE           PIC X(002).                       
                   88  A0-FILE-HEADER-REC           VALUE 'A0'.                 
                   88  AA-NSF-SUBMITTER-REC         VALUE 'AA'.                 
                   88  AB-NSF-OVRFL-SUBM-REC        VALUE 'AB'.                 
                   88  AN-HIPAA-ISA-SUBM-REC        VALUE 'AN'.                 
                   88  B0-BATCH-HEADER-REC          VALUE 'B0'.                 
                   88  BA-NSF-BILL-PROV-REC         VALUE 'BA'.                 
                   88  BB-NSF-OVRFL-BP-REC          VALUE 'BB'.                 
                   88  BC-NSF-PAY-TO-PROV-REC       VALUE 'BC'.                 
                   88  BP-HIPAA-BILL-PROV-REC       VALUE 'BP'.                 
                   88  BQ-HIPAA-BILL-PROV-NAME-REC  VALUE 'BQ'.                 
                   88  BR-HIPAA-PAY-TO-PROV-REC     VALUE 'BR'.                 
                   88  BS-BP-SEC-REF-REC            VALUE 'BS'.                 
                   88  BU-PAY-TO-SEC-REF-REC        VALUE 'BU'.                 
                   88  BV-BP-CR-DB-REC              VALUE 'BV'.                 
                   88  C0-CLAIM-HEADER-REC          VALUE 'C0'.                 
                   88  C2-LEGAL-REP-REC             VALUE 'C2'.                 
                   88  CA-NSF-PATIENT-REC           VALUE 'CA'.                 
                   88  CB-NSF-OVRFL-PATIENT-REC     VALUE 'CB'.                 
                   88  CC-NSF-OVRFL-LEGAL-REP-REC   VALUE 'CC'.                 
                   88  CN-HIPAA-PAT-DATA-1-REC      VALUE 'CN'.                 
                   88  CP-HIPAA-PAT-DATA-2-REC      VALUE 'CP'.                 
                   88  CZ-INTERNAL-TRACKING-REC     VALUE 'CZ'.                 
                   88  D0-PAYER-REC                 VALUE 'D0'.                 
                   88  D1-INSURED-REC               VALUE 'D1'.                 
                   88  D2-PAYER-ADDR-REC            VALUE 'D2'.                 
                   88  D3-REF-PRIOR-AUTH-REC        VALUE 'D3'.                 
                   88  D6-TPO-ROUT-REC              VALUE 'D6'.                 
                   88  DA-ADDL-PAY-INS-1-REC        VALUE 'DA'.                 
                   88  DB-ADDL-PAY-INS-2-REC        VALUE 'DB'.                 
                   88  DC-INSURED-EMPLOY-REC        VALUE 'DC'.                 
                   88  DD-PAYER-PYMT-REC            VALUE 'DD'.                 
                   88  DN-INSURED-SEC-ID-REC        VALUE 'DN'.                 
                   88  DP-PAYER-SEC-ID-REC          VALUE 'DP'.                 
                   88  DQ-PAYER-CONTACT-REC         VALUE 'DQ'.                 
                   88  DR-PAT-SEC-ID-REC            VALUE 'DR'.                 
                   88  DS-OTH-PAY-PROV-NUM-REC      VALUE 'DS'.                 
                   88  DT-CLAIM-ADJ-REASONS-REC     VALUE 'DT'.                 
                   88  DU-COB-REC                   VALUE 'DU'.                 
                   88  DV-CR-CARD-REC               VALUE 'DV'.                 
                   88  E0-CLAIM-DATA-REC            VALUE 'E0'.                 
                   88  E1-TPO-REC                   VALUE 'E1'.                 
                   88  E2-ORTHO-SVC-LINE-REC        VALUE 'E2'.                 
                   88  E3-DENTI-CAL-CLAIM-REC       VALUE 'E3'.                 
                   88  E4-PROV-REF-REC              VALUE 'E4'.                 
                   88  E5-TOOTH-STATUS-REC          VALUE 'E5'.                 
                   88  E6-PROV-NAME-REC             VALUE 'E6'.                 
                   88  E7-PROV-SEC-REF-REC          VALUE 'E7'.                 
                   88  E8-PROV-ADDRESS-REC          VALUE 'E8'.                 
                   88  E9-MCAID-RECLAM-REC          VALUE 'E9'.                 
                   88  EA-ADDL-CLAIM-REC            VALUE 'EA'.                 
                   88  EB-ADDL-CLAIM-REC            VALUE 'EB'.                 
                   88  EK-STATE-LOCAL-REC           VALUE 'EK'.                 
                   88  EM-DATE-TIME-REC             VALUE 'EM'.                 
                   88  EN-ADDL-CLAIM-REC            VALUE 'EN'.                 
                   88  EP-AMBULANCE-TRANS-REC       VALUE 'EP'.                 
                   88  EQ-AMBULANCE-VISION-REC      VALUE 'EQ'.                 
                   88  ER-CHIRO-CERT-REC            VALUE 'ER'.                 
                   88  ES-CHIRO-COND-REC            VALUE 'ES'.                 
                   88  EW-CLAIM-ATTACH-REC          VALUE 'EW'.                 
                   88  EX-HOME-HEALTH-REC           VALUE 'EX'.                 
                   88  EY-NOTE-REC                  VALUE 'EY'.                 
                   88  F0-SERVICES-PERF-REC         VALUE 'F0'.                 
                   88  F1-TPO-REPRICE-REC           VALUE 'F1'.                 
                   88  F2-MCARE-SUPPL-REC           VALUE 'F2'.                 
                   88  F3-DENTI-CAL-CLAIM-SVCS-REC  VALUE 'F3'.                 
                   88  F4-REF-NUM-SVC-LINE-REC      VALUE 'F4'.                 
                   88  F5-TOOTH-STATUS-REC          VALUE 'F5'.                 
                   88  F6-PROV-NAME-REC             VALUE 'F6'.                 
                   88  F7-PROV-SEC-REF-REC          VALUE 'F7'.                 
                   88  F8-PROV-ADDR-REC             VALUE 'F8'.                 
                   88  F9-PHCS-TPO-OUT-REC          VALUE 'F9'.                 
                   88  FA-ADDL-SVC-LN-1-REC         VALUE 'FA'.                 
                   88  FB-ADDL-SVC-LN-2-REC         VALUE 'FB'.                 
                   88  FD-ORTHO-SVC-LN-REC          VALUE 'FD'.                 
                   88  FK-STATE-LOCAL-REC           VALUE 'FK'.                 
                   88  FN-ADDL-SVC-LN-REC           VALUE 'FN'.                 
                   88  FP-ADDL-SVC-LN-REC           VALUE 'FP'.                 
                   88  G0-DME-REC                   VALUE 'G0'.                 
                   88  GA-AMB-CERT-REC              VALUE 'GA'.                 
                   88  GB-DMEPOS-CERT-REC           VALUE 'GB'.                 
                   88  GC-OXY-CERT-REC              VALUE 'GC'.                 
                   88  GD-OXY-MED-NEC-REC           VALUE 'GD'.                 
                   88  GP-AMB-TRANS-REASON-REC      VALUE 'GP'.                 
                   88  GQ-AMB-DME-OCOND-REC         VALUE 'GQ'.                 
                   88  GR-AMB-DME-COND-REC          VALUE 'GR'.                 
                   88  GS-CHIRO-DESC-REC            VALUE 'GS'.                 
                   88  GT-OXY-TEST-RESULTS-REC      VALUE 'GT'.                 
                   88  GU-TEST-RESULTS-REC          VALUE 'GU'.                 
                   88  GX-HOME-HEALTH-REC           VALUE 'GX'.                 
                   88  GY-SUPPORT-DOC-REC           VALUE 'GY'.                 
                   88  H0-NARRATIVE-REC             VALUE 'H0'.                 
                   88  KP-OTHER-PAYER-REC           VALUE 'KP'.                 
                   88  KR-PRIOR-AUTH-REC            VALUE 'KR'.                 
                   88  KS-LINE-ADJUD-REC            VALUE 'KS'.                 
                   88  KT-ADJUST-REASON-REC         VALUE 'KT'.                 
0407JW             88  KZ-XTEND-VALIDATOR-MSGS      VALUE 'KZ'.                 
                   88  X0-CLAIM-TRAILER-REC         VALUE 'X0'.                 
                   88  XA-ADDL-CLAIM-TRAILER-REC    VALUE 'XA'.                 
                   88  Y0-BATCH-TRAILER-REC         VALUE 'Y0'.                 
                   88  Z0-FILE-TRAILER-REC          VALUE 'Z0'.                 
                   88  SR-SEPARATOR-REC             VALUE '**'.                 
                                                                                
               10  CLAIM-RECORDS REDEFINES PCDS-00-REC-TYPE                     
                                           PIC X(002).                          
                   88  CLAIM-RECS      VALUE                                    
                                    'C0' 'C2'                                   
                                    'CA' 'CB' 'CC' 'CN' 'CP' 'CZ'               
                                    'D0' 'D1' 'D2' 'D3' 'D6'                    
                                    'DA' 'DB' 'DC' 'DD' 'DN' 'DP'               
                                    'DQ' 'DR' 'DS' 'DT' 'DU' 'DV'               
                                    'E0' 'E1' 'E2' 'E3' 'E4' 'E5'               
                                    'E6' 'E7' 'E8' 'E9'                         
                                    'EA' 'EB' 'EK' 'EM' 'EN' 'EP'               
                                    'EQ' 'ER' 'ES' 'EW' 'EX' 'EY'               
                                    'F0' 'F1' 'F2' 'F3' 'F4' 'F5'               
                                    'F6' 'F7' 'F8' 'F9'                         
                                    'FA' 'FB' 'FD' 'FK' 'FN' 'FP'               
                                    'G0'                                        
                                    'GA' 'GB' 'GC' 'GD' 'GP' 'GQ'               
                                    'GR' 'GS' 'GT' 'GU' 'GV' 'GX' 'GY'          
                                    'H0'                                        
0407JW                              'KP' 'KR' 'KS' 'KT' 'KZ'                    
                                         'X0' 'XA'.                             
                                                                                
               10  PCDS-SEQ-NO             PIC 9(002).                          
               10  PCDS-SEQ-NO-X REDEFINES PCDS-SEQ-NO                          
                                           PIC X(002).                          
               10  PCDS-PCN.                                                    
                   15  PCDS-PCN1           PIC X(001).                          
                   15  FILLER              PIC X(016).                          
               10  FILLER                  PIC X(171).                          
                                                                                                                                                 
      *----------------------------------------------------------------*        
      *       FILE HEADER RECORD SUBMITTER INFORMATION (A0)            *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-A0-RECORD REDEFINES PCDS-00-RECORD.                         
               10  A0-REC-TYPE               PIC X(002).                        
                   88  A0-REC                         VALUE 'A0'.               
               10  A0-SUB-EIN                PIC 9(009).                        
               10  A0-SUB-EIN-X REDEFINES                                       
                   A0-SUB-EIN                PIC X(009).                        
               10  A0-FORMAT-VERS-CODE       PIC X(007).                        
                   88 MCDS-WEBMD-MEDICAL-21       VALUE '0002M01'.              
                   88 DCDS-WEBMD-DENTAL-21        VALUE '0002D01'.              
                   88 MCDS-WEBMD-MEDICAL-30       VALUE '0003M00'.              
                   88 DCDS-WEBMD-DENTAL-30        VALUE '0003D00'.              
                   88 PCDS-320-WEBMD-PROF-100     VALUE '0001P00'.              
                   88 PCDS-320-WEBMD-ADA-100      VALUE '0001A00'.              
                   88 X12N-837-PROF-4010          VALUE '4010X98'.              
                   88 X12N-837-DENTAL-4010        VALUE '4010X97'.              
                   88 NSF-200-MEDICAL             VALUE 'NSF2M00'.              
                   88 NSF-200-DENTAL              VALUE 'NSF2D00'.              
                                                                                
                   88 VALID-FORMAT-VERSION-CODE   VALUE '0002M01'               
                                                        '0002D01'               
                                                        '0003M00'               
                                                        '0003D00'               
                                                        '0001P00'               
                                                        '0001A00'               
                                                        '4010X98'               
                                                        '4010X97'               
                                                        'NSF2M00'               
                                                        'NSF2D00'.              
                                                                                
               10  A0-SENDER-ID.                                                
                   15  A0-SENDER-ID-1-9      PIC X(009).                        
                   15  FILLER                PIC X(002).                        
               10  A0-ALL-PAYER-FLAG         PIC X(001).                        
      ** ON 11-14-02, MOVED TPO FLAG TO COL 183 TO MATCH V1.24 SPECS            
      ** IT WAS LOCATED BETWEEN ALL-PAYER-FLAG AND RECEIVER-TYPE-CD             
               10  A0-RECEIVER-TYPE-CODE     PIC X(001).                        
                   88  A0-LIABILITY-MED                   VALUE '1'.            
                   88  A0-MED-RISK-HMO                    VALUE '2'.            
                   88  A0-AUTO-MED                        VALUE '3'.            
                   88  A0-MUTUAL-DEF                      VALUE '4'.            
                   88  A0-DISABILITY                      VALUE '5'.            
                   88  A0-SELF-PAY                        VALUE 'A'.            
                   88  A0-WORKERS-COMP                    VALUE 'B'.            
                   88  A0-MEDICARE                        VALUE 'C'.            
                   88  A0-MEDICAID                        VALUE 'D'.            
                   88  A0-OTHER-FED-PGM                   VALUE 'E'.            
                   88  A0-COMMERCIAL-INSURANCE            VALUE 'F'.            
                   88  A0-BLUE-SHIELD                     VALUE 'G'.            
                   88  A0-CHAMPUS                         VALUE 'H'.            
                   88  A0-FED-EMP-PGM                     VALUE 'J'.            
                   88  A0-CENTRAL-CERT                    VALUE 'K'.            
                   88  A0-SELF-ADMIN                      VALUE 'L'.            
                   88  A0-FAMILY-FRIENDS                  VALUE 'M'.            
                   88  A0-MNG-CARE-NON-HMO                VALUE 'N'.            
                   88  A0-BLUE-CROSS                      VALUE 'P'.            
                   88  A0-POS                             VALUE 'Q'.            
                   88  A0-EPO                             VALUE 'R'.            
                   88  A0-INDEM-INS                       VALUE 'S'.            
                   88  A0-TITLE-V                         VALUE 'T'.            
                   88  A0-VA-PLAN                         VALUE 'V'.            
                   88  A0-LIABILITY                       VALUE 'W'.            
                   88  A0-PPO                             VALUE 'X'.            
                   88  A0-HMO                             VALUE 'Y'.            
                   88  A0-OTHER-NON-FED                   VALUE 'Z'.            
                   88  A0-VALID-RECEIVER-TYPE                                   
                         VALUE 'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'J'              
                               'K' 'L' 'M' 'N' 'P' 'Q' 'R' 'S' 'T'              
                               'V' 'W' 'X' 'Y' 'Z'                              
                               '1' '2' '3' '4' '5'.                             
               10 A0-RECEIVER-ID.                                               
                   15  A0-RECEIVER-ID-NUM    PIC 9(005).                        
                                                                                
                       88  A0-NEIC-TIN-FIRST-5                                  
                           VALUE 13305.                                         
                   15  A0-RECEIVER-SUB-ID    PIC X(004).                        
                                                                                
                       88  A0-NEIC-TIN-LAST-4                                   
                           VALUE '2274'.                                        
               10  A0-PROCESSING-DATE.                                          
                   15  A0-PROC-MM-DD.                                           
                       20  A0-PROC-MM        PIC 9(002).                        
                       20  A0-PROC-DD        PIC 9(002).                        
                   15  A0-PROC-YY            PIC 9(002).                        
               10  A0-SUB-NAME               PIC X(021).                        
               10  A0-NEIC-ADDRESS.                                             
                   15  A0-NEIC-ADDR          PIC X(018).                        
                   15  A0-NEIC-CITY          PIC X(015).                        
                   15  A0-NEIC-STATE         PIC X(002).                        
                   15  A0-NEIC-ZIP           PIC X(009).                        
               10  A0-NEIC-PHONE-NUM         PIC 9(010).                        
               10  A0-NEIC-PHONE-NUM-X REDEFINES                                
                   A0-NEIC-PHONE-NUM         PIC X(010).                        
               10  A0-XMIT-STATUS            PIC 9(001).                        
                   88  A0-NORMAL-TRANSMISSION              VALUE 0.             
                   88  A0-NON-CHRGBL-REXMIT                VALUE 8.             
                   88  A0-CHRGBL-REXMIT                    VALUE 9.             
               10  A0-REEL-SERIAL-NO         PIC X(006).                        
               10  FILLER                    PIC X(016).                        
               10  A0-TEST-PROD-IND          PIC X(004).                        
               10  A0-NCCS-VENDOR-ID         PIC X(008).                        
               10  A0-MCDS-DATE-TIME.                                           
                   15 A0-MCDS-DATE.                                             
                      20 A0-MCDS-D-YY        PIC X(002).                        
                      20 A0-MCDS-D-MM-DD.                                       
                         25 A0-MCDS-D-MM     PIC X(002).                        
                         25 A0-MCDS-D-DD     PIC X(002).                        
                   15 A0-MCDS-TIME.                                             
                      20 A0-MCDS-T-HH-MM.                                       
                         25 A0-MCDS-T-HH     PIC X(002).                        
                         25 A0-MCDS-T-MM     PIC X(002).                        
                      20 A0-MCDS-T-SS        PIC X(002).                        
               10  FILLER                    PIC X(001).                        
               10  A0-JOB-NUMBER             PIC X(005).                        
      * 20010110-01 PART OF FILLER USED FOR UNIQUE JOBNUM C.N. 1/22/01          
      *        10  FILLER                    PIC X(012).                        
      * 05/15/01 THE UNIQUE-JOBNUM NEEDS TO BE EXPANDED TO 8 BYTES              
      * IN PCDS FROM 7 BYTES IN MCDS ACCORDING TO CARMEL AND RICHARD            
               10  A0-UNIQUE-JOBNUM          PIC X(008).                        
               10  A0-TPO-FLAG               PIC X(001).                        
               10  FILLER                    PIC X(002).                        
      * 20010110-01 END..                                                       
               10  A0-VERSION-INFORMATION.                                      
                   15  A0-UPDATE-FIELDS.                                        
                       20  A0-UPDATE-CODE    PIC X(002).                        
                       20  A0-UPDATE-RLSE    PIC X(001).                        
                   15  A0-VERSION-FIELDS.                                       
                       20  A0-NEW-VERS-CODE  PIC X(003).                        
                       20  A0-NEW-VERS-RLSE  PIC X(001).                        
                   15  A0-VERSION-CODES    REDEFINES                            
                       A0-VERSION-FIELDS.                                       
                       20  FILLER            PIC X(001).                        
                       20  A0-VERSION-CODE   PIC X(003).                        
                           88  A0-VERSION-012    VALUE '012'.                   
                           88  A0-VALID-VERSION-CODE                            
                                                 VALUE '012'.                   
                                                                                                                                                
      *----------------------------------------------------------------*        
      *       FILE HEADER RECORD: NSF SUBMITTER INFORMATION (AA)       *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-AA-RECORD REDEFINES PCDS-00-RECORD.                         
               10  AA-REC-TYPE                PIC X(002).                       
                   88  AA-REC                         VALUE 'AA'.               
               10  AA-NSF-SUBM-ADDRESS-2      PIC X(030).                       
               10  AA-SUBMISSION-TYPE         PIC X(006).                       
               10  AA-SUBMITTER-REGION        PIC X(005).                       
               10  AA-SUBMISSION-TIME         PIC X(006).                       
               10  AA-SUBMISSION-TIME-X REDEFINES                               
                   AA-SUBMISSION-TIME.                                          
                   15 AA-ST-HH                PIC X(002).                       
                   15 AA-ST-MM                PIC X(002).                       
                   15 AA-ST-SS                PIC X(002).                       
               10  AA-VERSION-CODE-LOCAL      PIC 9(3)V99.                      
               10  AA-VERSION-CODE-LOCAL-X REDEFINES                            
                   AA-VERSION-CODE-LOCAL      PIC X(005).                       
               10  AA-ORIG-SUBMITTER-ID       PIC X(016).                       
               10  AA-VEND-APPL-CATEGORY      PIC X(001).                       
               10  AA-COB-FILE-IND            PIC X(001).                       
               10  AA-PROCESS-FROM-DATE.                                        
                   15  AA-PROCESS-FROM-CC     PIC X(002).                       
                   15  AA-PROCESS-FROM-YY     PIC X(002).                       
                   15  AA-PROCESS-FROM-MM     PIC X(002).                       
                   15  AA-PROCESS-FROM-DD     PIC X(002).                       
               10  AA-PROCESS-THRU-DATE.                                        
                   15  AA-PROCESS-THRU-CC     PIC X(002).                       
                   15  AA-PROCESS-THRU-YY     PIC X(002).                       
                   15  AA-PROCESS-THRU-MM     PIC X(002).                       
                   15  AA-PROCESS-THRU-DD     PIC X(002).                       
               10  AA-ACK-REQUEST             PIC X(001).                       
               10  AA-RECEIPT-DATE.                                             
                   15  AA-RECEIPT-THRU-CC     PIC X(002).                       
                   15  AA-RECEIPT-THRU-YY     PIC X(002).                       
                   15  AA-RECEIPT-THRU-MM     PIC X(002).                       
                   15  AA-RECEIPT-THRU-DD     PIC X(002).                       
               10  FILLER                     PIC X(095).                       
                                                                                
      *----------------------------------------------------------------*        
      * FILE HEADER RECORD: NSF OVERFLOW SUBMITTER INFORMATION (AB)    *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-AB-RECORD REDEFINES PCDS-00-RECORD.                         
               10  AB-REC-TYPE                PIC X(002).                       
                   88  AB-REC                         VALUE 'AB'.               
               10  AB-NSF-SUBMITTER-NAME      PIC X(035).                       
               10  AB-NSF-SUBMITTER-ADDR      PIC X(030).                       
               10  AB-NSF-SUBMITTER-CITY      PIC X(020).                       
               10  AB-NSF-SUBMITTER-CONTACT   PIC X(035).                       
               10  FILLER                     PIC X(070).                       
                                                                                
      *----------------------------------------------------------------*        
      * FILE HEADER RECORD: HIPAA ISA SUBMITTER INFORMATION (AN)       *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-AN-RECORD REDEFINES PCDS-00-RECORD.                         
               10  AN-REC-TYPE                PIC X(002).                       
                   88  AN-REC                         VALUE 'AN'.               
               10  FILLER                     PIC X(002).                       
               10  AN-SUBMITTER-FIRST-NAME    PIC X(012).                       
               10  AN-SUBMITTER-MI            PIC X(001).                       
               10  FILLER                     PIC X(007).                       
               10  AN-IMPLEMENTATION-GUIDE-ID PIC X(015).                       
               10  AN-ADDL-SUBMITTER-NAME     PIC X(060).                       
               10  AN-HIPAA-SUBMITTER-FAX-NO  PIC X(010).                       
               10  AN-HIPAA-SUBMITTER-EMAIL   PIC X(050).                       
               10  AN-HIPAA-SUBMITTER-EDI-NO  PIC X(015).                       
               10  AN-HIPAA-PHONE-EXT         PIC X(006).                       
               10  FILLER                     PIC X(012).                       
                                                                                
      *----------------------------------------------------------------*        
      *     BILLING PROVIDER INFORMATION RECORD (B0)                   *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-B0-RECORD REDEFINES PCDS-00-RECORD.                         
               10  B0-REC-TYPE                    PIC X(002).                   
                   88  B0-REC                              VALUE 'B0'.          
                                                                                
               10  B0-BATCH-TYPE                  PIC X(003).                   
                   88  B0-MCDS-BATCH                       VALUE '100'.         
                   88  B0-SUPPLEMENTAL-BILLING             VALUE '101'.         
                   88  B0-MEDICAL-ENCOUNTERS               VALUE '102'.         
                   88  B0-DENTAL-BATCH                     VALUE 'D01'.         
                   88  B0-DENTAL-ENCOUNTERS                VALUE 'D02'.         
                   88  B0-PHARMACY-BATCH                   VALUE 'P01'.         
                   88  B0-VALID-BATCH-TYPE VALUE '100' '101' '102'              
                                                 'D01' 'D02' 'P01'.             
               10  B0-BATCH-NUM                   PIC 9(002).                   
                   88  B0-VALID-BATCH-NUM  VALUE 1 THRU 99.                     
               10  B0-BATCH-NUM-X REDEFINES                                     
                   B0-BATCH-NUM                   PIC X(002).                   
               10  B0-PROV-ID.                                                  
                   15  B0-FEDERAL-TAX-NUM                                       
                                                  PIC 9(009).                   
                   15  B0-FEDERAL-TAX-NUM-X REDEFINES                           
                       B0-FEDERAL-TAX-NUM                                       
                                                  PIC X(009).                   
                   15  B0-PROV-SUB-ID             PIC X(004).                   
               10  B0-TAX-ID-TYPE                 PIC X(001).                   
                   88  B0-EMPL-ID-NUM              VALUE  'E'.                  
                   88  B0-SOCIAL-SECURITY-NUM      VALUE  'S'.                  
                   88  B0-VALID-TAXID-TYPE         VALUES 'E'  'S'.             
               10  B0-MEDICAID-RECLM-FLAG         PIC X(001).                   
               10  B0-NPI                         PIC X(010).                   
               10  B0-PROV-MEDICAID-NO            PIC X(012).                   
               10  FILLER                         PIC X(004).                   
               10  B0-BS-PROV-ID-NO               PIC X(013).                   
               10  B0-DENTIST-LIC-NO              PIC X(009).                   
               10  B0-BP-PHONE                    PIC 9(010).                   
               10  B0-BP-PHONE-X REDEFINES                                      
                   B0-BP-PHONE                    PIC X(010).                   
               10  B0-BP-ORG-NAME                 PIC X(018).                   
               10  B0-BP-ADDRESS-1                PIC X(025).                   
               10  B0-BP-CITY                     PIC X(015).                   
               10  B0-BP-STATE                    PIC X(002).                   
               10  B0-BP-ZIP-CODE                 PIC 9(009).                   
               10  B0-BP-ZIP-CODE-X  REDEFINES                                  
                   B0-BP-ZIP-CODE.                                              
                   15  B0-ZIP3                    PIC X(003).                   
                   15  FILLER                     PIC X(006).                   
               10  B0-INDIV-PROVIDER.                                           
                   15  B0-IND-PROV-LNAME          PIC X(020).                   
                   15  B0-IND-PROV-FNAME          PIC X(010).                   
                   15  B0-IND-PROV-MI             PIC X(001).                   
               10  B0-BP-SPEC-CD                  PIC X(003).                   
      *****************************************************************         
      ** THE FOLLOWING BYTE WAS CHANGED FROM FILLER TO B0-SUBSYSTEM ***         
      ** ON 02-11-2003. WE NEED TO CONTACT NORMA ABOUT A SPEC CHANGE **         
      *****************************************************************         
               10  B0-SUBSYSTEM                   PIC X(001).                   
               10  B0-NEIC-BATCH-NO.                                            
                   15  B0-NEIC-BATCH-NUMBER       PIC 9(009) COMP-3.            
                   15  B0-NEIC-BATCH-NUMBER-X  REDEFINES                        
                       B0-NEIC-BATCH-NUMBER       PIC X(005).                   
                   15  FILLER                     PIC X(001).                   
               10  B0-BCBS-FLG                    PIC X(001).                   
               10  B0-BP-MEDCR-ID-IND             PIC X(001).                                                                                       
      *----------------------------------------------------------------*        
      *      NSF BILLING PROVIDER INFORMATION RECORD (BA)              *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-BA-RECORD REDEFINES PCDS-00-RECORD.                         
               10  BA-REC-TYPE                    PIC X(002).                   
                   88  BA-REC                              VALUE 'BA'.          
               10  BA-EMC-PROVIDER-ID             PIC X(015).                   
               10  BA-BATCH-ID                    PIC X(006).                   
               10  BA-BP-UPIN                     PIC X(006).                   
               10  FILLER                         PIC X(006).                   
               10  BA-BP-CHAMPUS-NO               PIC X(015).                   
               10  BA-BP-COMMERCIAL-NO            PIC X(015).                   
               10  BA-BP-ORG-NO-1                 PIC X(015).                   
               10  BA-BP-ORG-NO-2                 PIC X(015).                   
               10  BA-BP-SPECIALTY-LICENSE        PIC X(015).                   
               10  BA-BP-STATE-LICENSE            PIC X(015).                   
               10  BA-BP-ANEST-LICENSE            PIC X(015).                   
               10  BA-PROV-TYPE-ORG               PIC X(003).                   
                   88  SOLO-PRACTICE                  VALUE '001'.              
                   88  PARTNERSHIP                    VALUE '002'.              
                   88  PROF-ASSOC                     VALUE '003'.              
                   88  CLINIC                         VALUE '004'.              
                   88  ONE-FACILITY-HOSP              VALUE '005'.              
                   88  DISTINCT-PART-F-H              VALUE '006'.              
                   88  INDIVIDUAL                     VALUE '007'.              
                   88  CORPORATION                    VALUE '008'.              
               10  BA-BP-ADDRESS-2                PIC X(030).                   
               10  BA-BP-SUFFIX                   PIC X(010).                   
               10  BA-BP-UPIN-USIN-ID             PIC X(006).                   
               10  FILLER                         PIC X(003).                   
                                                                                
      *----------------------------------------------------------------*        
      *   NSF OVERFLOW BILLING PROVIDER INFORMATION RECORD (BB)        *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-BB-RECORD REDEFINES PCDS-00-RECORD.                         
               10  BB-REC-TYPE                    PIC X(002).                   
                   88  BB-REC                              VALUE 'BB'.          
               10  FILLER                         PIC X(015).                   
               10  BB-BATCH-ID                    PIC X(006).                   
               10  BB-NSF-BP-ORG-NAME             PIC X(035).                   
               10  BB-NSF-BP-FNAME                PIC X(012).                   
               10  BB-NSF-BP-ADDRESS-1            PIC X(030).                   
               10  BB-NSF-BP-CITY                 PIC X(020).                   
               10  FILLER                         PIC X(016).                   
               10  FILLER                         PIC X(016).                   
               10  BB-NSF-BP-PARTICIPATING-IND    PIC X(001).                   
      ** FRED  20020613-11 ADD BB-12 TO COLUMNS 158-192                         
               10  FILLER                         PIC X(004).                   
               10  BB-NSF-BP-LNAME                PIC X(035).                   
      ** FRED  20020613-11 END                                                  
                                                                                
      *----------------------------------------------------------------*        
      *   NSF PAY-TO PROVIDER INFORMATION RECORD (BC)                  *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-BC-RECORD REDEFINES PCDS-00-RECORD.                         
               10  BC-REC-TYPE                    PIC X(002).                   
                   88  BC-REC                              VALUE 'BC'.          
               10  BC-BATCH-ID                    PIC X(006).                   
               10  FILLER                         PIC X(009).                   
               10  BC-PTP-ADDRESS-1               PIC X(030).                   
               10  BC-PTP-ADDRESS-2               PIC X(030).                   
               10  BC-PTP-CITY                    PIC X(020).                   
               10  BC-PTP-STATE                   PIC X(002).                   
               10  BC-PTP-ZIP                     PIC X(009).                   
               10  BC-PTP-PHONE                   PIC X(010).                   
               10  FILLER                         PIC X(074).                   
                                                                                
      *----------------------------------------------------------------*        
      *   HIPAA BILLING PROVIDER INFORMATION RECORD (BP)               *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-BP-RECORD REDEFINES PCDS-00-RECORD.                         
               10  BP-REC-TYPE                    PIC X(002).                   
                   88  BP-REC                              VALUE 'BP'.          
               10  FILLER                         PIC X(002).                   
               10  BP-BATCH-ID                    PIC X(006).                   
               10  FILLER                         PIC X(009).                   
               10  BP-HIPAA-BP-COUNTRY-CD         PIC X(003).                   
               10  BP-HIPAA-BP-PHONE-EXT          PIC X(005).                   
               10  BP-HIPAA-BP-CONTACT-NAME       PIC X(060).                   
               10  BP-HIPAA-BP-FAX-NO             PIC X(010).                   
               10  BP-HIPAA-BP-EMAIL              PIC X(040).                   
               10  BP-HIPAA-BP-TAXONOMY-CD        PIC X(010).                   
               10  BP-HIPAA-BP-CURRENCY-CD        PIC X(003).                   
               10  BP-HIPAA-BP-HL-ID-NO           PIC X(006).                   
               10  FILLER                         PIC X(036).                   
                                                                                
      *----------------------------------------------------------------*        
      * HIPAA PAY-TO PROVIDER ADDITIONAL NAME NFORMATION RECORD (BQ)*           
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-BQ-RECORD REDEFINES PCDS-00-RECORD.                         
               10  BQ-REC-TYPE                    PIC X(002).                   
                   88  BQ-REC                              VALUE 'BQ'.          
               10  BQ-BATCH-ID                    PIC X(006).                   
               10  FILLER                         PIC X(009).                   
               10  BQ-HIPAA-BP-ADDL-NAME          PIC X(060).                   
               10  FILLER                         PIC X(115).                   
                                                                                
      *----------------------------------------------------------------*        
      * HIPAA PAY-TO PROVIDER INFORMATION RECORD (BR)                  *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-BR-RECORD REDEFINES PCDS-00-RECORD.                         
               10  BR-REC-TYPE                    PIC X(002).                   
                   88  BR-REC                             VALUE 'BR'.           
               10  BR-BATCH-ID                    PIC X(006).                   
               10  FILLER                         PIC X(009).                   
               10  BR-PTP-ORG-OR-LNAME            PIC X(035).                   
               10  BR-PTP-FNAME                   PIC X(012).                   
               10  BR-PTP-MI                      PIC X(001).                   
               10  BR-PTP-COUNTRY-CD              PIC X(003).                   
               10  BR-PTP-ADDL-NAME               PIC X(060).                   
               10  BR-PTP-SUFFIX                  PIC X(010).                   
               10  BR-PTP-TAX-ID-TYPE             PIC X(001).                   
               10  BR-PTP-TAX-ID                  PIC 9(009).                   
               10  BR-PTP-TAX-ID-X REDEFINES                                    
                   BR-PTP-TAX-ID                  PIC X(009).                   
               10  BR-PTP-NPI                     PIC X(010).                   
               10  BR-PTP-TAXONOMY-CD             PIC X(010).                   
               10  FILLER                         PIC X(024).                   
                                                                                
      *----------------------------------------------------------------*        
      * BILLING PROVIDER SECONDARY REFERENCE NUMBERS RECORD (BS)                
      *  (THE FABLED 'BS' REOCRD!!! )                                           
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-BS-RECORD REDEFINES PCDS-00-RECORD.                         
               10  BS-REC-TYPE                    PIC X(002).                   
                   88  BS-REC                              VALUE 'BS'.          
               10  BS-BATCH-ID                    PIC X(006).                   
               10  FILLER                         PIC X(009).                   
               10  BS-BP-SECONDARY-REF-NUMBERS OCCURS 5 TIMES.                                                          
                   15  BS-BPS-REF-QUAL            PIC X(002).                   
                   15  BS-BPS-REF-NO              PIC X(030).                   
               10  FILLER                         PIC X(015).                   
                                                                                
      *----------------------------------------------------------------*        
      *  PROVIDER PAY-TO SECONDARY REFERENCE NUMBERS RECORD (BU)       *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-BU-RECORD REDEFINES PCDS-00-RECORD.                         
               10  BU-REC-TYPE                    PIC X(002).                   
                   88  BU-REC                              VALUE 'BU'.          
               10  BU-BATCH-ID                    PIC X(006).                   
               10  FILLER                         PIC X(009).                   
               10  BU-PTP-SECONDARY-REF-NUMBERS OCCURS 5 TIMES                  
                     INDEXED BY BU-INX.                                         
                   15  BU-PTP-REF-QUAL            PIC X(002).                   
                   15  BU-PTP-REF-NO              PIC X(030).                   
               10  FILLER                         PIC X(015).                   
                                                                                
      *----------------------------------------------------------------*        
      * BILLING PROVIDER CREDIT/DEBIT INFORMATION RECORD (BV)          *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-BV-RECORD REDEFINES PCDS-00-RECORD.                         
               10  BV-REC-TYPE                    PIC X(002).                   
                   88  BV-REC                              VALUE 'BV'.          
               10  BV-BATCH-ID                    PIC X(006).                   
               10  FILLER                         PIC X(002).                   
               10  BV-SUB-SEQ-NO                  PIC 9(002).                   
               10  BV-BP-CREDIT-DEBIT-NUMBERS OCCURS 4 TIMES                    
                                              INDEXED BY BV-INX.                
                   15  BV-BP-CREDIT-DEBIT-QUAL    PIC X(002).                   
                   15  BV-BP-CR-DR-NO             PIC X(030).                   
               10  FILLER                         PIC X(052).                   
                                                                                
      *----------------------------------------------------------------*        
      *         CLAIM LEVEL PATIENT INFORMATION RECORD (C0)            *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-C0-RECORD REDEFINES PCDS-00-RECORD.                         
               10  C0-REC-TYPE                    PIC X(002).                   
                   88  C0-REC                              VALUE 'C0'.          
               10  C0-PARTICIPATING-PAYOR-IND.                                  
                   15  C0-PARTICIPATING-IND       PIC X(001).                   
                        88  C0-PARTICIPATING          VALUES                    
                            ' '  'A'.                                           
                        88  C0-NON-PARTICIPATING      VALUE 'B'.                
                        88  C0-HUMANA-ENCOUNTERS      VALUE '3'.                
                        88  C0-GENERIC-ROUTE          VALUE 'R'.                
                        88  C0-VALID-PARTIC-CODES     VALUES                    
                            ' ' 'A' 'B' 'E' 'R'.                                
                   15  C0-TRANSACTION-TYPE        PIC X(001).                   
                        88  RTC0-CLAIM                  VALUE '1'.              
                        88  RTC0-ENCOUNTER              VALUE '2'.              
                        88  RTC0-VALID-PART2-CODES      VALUE '1'  '2'.         
               10  C0-PAT-CNTL-NUM                PIC X(017).                   
               10  C0-PATIENT-NAME.                                             
                   15  C0-PAT-L-NAME.                                           
                       20  C0-PAT-L-NAME1         PIC X(001).                   
                       20  FILLER                 PIC X(019).                   
                   15  C0-PAT-F-NAME.                                           
                       20  C0-PAT-F-NAME1         PIC X(001).                   
                       20  FILLER                 PIC X(009).                   
                   15  C0-PAT-M-INIT              PIC X(001).                   
               10  C0-PAT-SEX                     PIC X(001).                   
               10  C0-PAT-BIRTH-DATE.                                           
                   15  C0-PAT-BDATE-YYYY.                                       
                       20  C0-PAT-BDATE-YY1       PIC 9(002).                   
                       20  C0-PAT-BDATE-YY2       PIC 9(002).                   
                   15  C0-PAT-BDATE-MM            PIC 9(002).                   
                   15  C0-PAT-BDATE-DD            PIC 9(002).                   
               10  C0-PAT-BIRTH-DATE-X REDEFINES                                
                   C0-PAT-BIRTH-DATE.                                           
                   15  C0-PAT-BDATE-CC-X          PIC X(002).                   
                   15  C0-PAT-BDATE-YY-X          PIC X(002).                   
                   15  C0-PAT-BDATE-MM-X          PIC X(002).                   
                   15  C0-PAT-BDATE-DD-X          PIC X(002).                   
               10  C0-MARR-STATUS                 PIC X(001).                   
                   88  C0-MARR-STAT-NOT-ENTERED          VALUE ' '.             
                   88  C0-PATIENT-IS-SINGLE              VALUE 'S'.             
                   88  C0-PATIENT-IS-MARRIED             VALUE 'M'.             
                   88  C0-PATIENT-IS-LEGALLY-SEP         VALUE 'X'.             
                   88  C0-PATIENT-IS-DIVORCED            VALUE 'D'.             
                   88  C0-PATIENT-IS-WIDOWED             VALUE 'W'.             
                   88  C0-UNKNOWN-MARR-STAT              VALUE 'U'.             
                   88  C0-PATIENT-IS-PARTNER             VALUE 'P'.             
                   88  C0-VALID-MARR-STAT                VALUES                 
                       'S' 'M' 'X' 'D' 'W' 'U' 'P'.                             
               10  FILLER                         PIC X(004).                   
               10  C0-PAT-ADDRESS.                                              
                   15  C0-PAT-ADDR-1.                                           
                       20  C0-PAT-ADDR1-C1        PIC X(001).                   
                       20  FILLER                 PIC X(017).                   
                   15  C0-PAT-ADDR2.                                            
                       20  C0-PAT-ADDR2-C1        PIC X(001).                   
                       20  FILLER                 PIC X(017).                   
                   15  C0-PAT-CITY.                                             
                       20  C0-PAT-CITY-C1         PIC X(001).                   
                       20  FILLER                 PIC X(014).                   
                   15  C0-PAT-STATE               PIC X(002).                   
                   15  C0-PAT-ZIP.                                              
                       20  C0-PAT-ZIP5            PIC X(005).                   
                       20  FILLER                 PIC X(004).                   
                   15  C0-PAT-ZIP9    REDEFINES                                 
                       C0-PAT-ZIP                 PIC 9(009).                   
               10  C0-PAT-TEL-NUM                 PIC 9(010).                   
               10  C0-PAT-TEL-NUM-X REDEFINES                                   
                   C0-PAT-TEL-NUM                 PIC X(010).                   
               10  C0-PAT-EMP-STAT                PIC X(001).                   
                   88  C0-PAT-EMP-STAT-NOT-ENTERED         VALUE ' '.           
                   88  C0-FULL-TIME-EMPL                   VALUE 'F'.           
                   88  C0-PART-TIME-EMPL                   VALUE 'P'.           
                   88  C0-PATIENT-IS-RETIRED               VALUE 'R'.           
                   88  C0-PATIENT-IS-UNEMPLOYED            VALUE 'N'.           
                   88  C0-VALID-PAT-EMP-STAT               VALUES               
                           'F' 'P' 'R' 'N'.                                     
                   88  C0-SELF-EMPLOYED                    VALUE 'S'.           
                   88  C0-ACTIVE-MILITARY                  VALUE 'M'.           
                   88  C0-VALID-PAT-EMP-STAT-ENH           VALUES               
                       'S' 'M' ' '.                                             
               10  C0-PAT-STUD-STAT        PIC X(001).                          
                   88  C0-PAT-STUD-STAT-NOT-ENTERED        VALUE ' '.           
                   88  C0-FULL-TIME-STUDENT                VALUE 'F'.           
                   88  C0-PART-TIME-STUDENT                VALUE 'P'.           
                   88  C0-PATIENT-NOT-STUDENT              VALUE 'N'.           
                   88  C0-VALID-PAT-STUD-STAT              VALUES               
                          'F' 'P' 'N'.                                          
               10  C0-DEATH-IND            PIC X(001).                          
                   88  C0-PATIENT-DECEASED                 VALUE 'D'.           
                   88  C0-PATIENT-NOT-DECEASED             VALUE ' '.           
               10  C0-OTHER-INS            PIC X(001).                          
                   88  C0-OTHER-INS-NOT-ENTERED            VALUE ' '.           
                   88  C0-OTHER-INS-YES                    VALUE 'Y'.           
                   88  C0-OTHER-INS-NO                     VALUE 'N'.           
                   88  C0-OL-NOT-REFLECTED                 VALUE 'X'.           
                   88  C0-VALID-OTHER-INS                  VALUES               
                       'Y' 'N'.                                                 
               10  C0-DATE-OF-DEATH.                                            
                   15 C0-DEATH-CC          PIC 9(002).                          
                   15 C0-DEATH-YY          PIC 9(002).                          
                   15 C0-DEATH-MM          PIC 9(002).                          
                   15 C0-DEATH-DD          PIC 9(002).                          
               10  C0-DATE-OF-DEATH-X REDEFINES                                 
                   C0-DATE-OF-DEATH.                                            
                   15 C0-DEATH-CC-X        PIC X(002).                          
                   15 C0-DEATH-YY-X        PIC X(002).                          
                   15 C0-DEATH-MM-X        PIC X(002).                          
                   15 C0-DEATH-DD-X        PIC X(002).                          
               10  C0-HIPAA-PAT-CNTL-NO    PIC X(020).                          
               10  C0-PAT-SUFFIX-GEN       PIC X(010).                          
               10  FILLER                  PIC X(012).                          
                                                                                
      *----------------------------------------------------------------*        
      *      LEGAL REPRESENTATIVE INFORMATION RECORD (C2)              *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-C2-RECORD REDEFINES PCDS-00-RECORD.                         
               10  C2-REC-TYPE                    PIC X(002).                   
                   88  C2-REC                              VALUE 'C2'.          
               10  FILLER                         PIC X(002).                   
               10  C2-PAT-CNTL-NO                 PIC X(017).                   
               10  C2-LEGAL-REP-IND               PIC X(001).                   
               10  C2-LEGAL-REP-LNAME             PIC X(020).                   
               10  C2-LEGAL-REP-FNAME             PIC X(010).                   
               10  C2-LEGAL-REP-MI                PIC X(001).                   
               10  C2-LEGAL-REP-ADDRESS-1         PIC X(018).                   
               10  C2-LEGAL-REP-ADDRESS-2         PIC X(018).                   
               10  C2-LEGAL-REP-CITY              PIC X(015).                   
               10  C2-LEGAL-REP-STATE             PIC X(002).                   
               10  C2-LEGAL-REP-ZIP-CODE          PIC X(009).                   
               10  C2-LEGAL-REP-PHONE             PIC X(010).                   
               10  C2-LEGAL-REP-SUFFIX            PIC X(010).                   
               10  C2-LEGAL-REP-COUNTRY-CD        PIC X(003).                   
               10  FILLER                         PIC X(054).                   
                                                                                
      *----------------------------------------------------------------*        
      *           NSF PATIENT INFORMATION RECORD (CA)                  *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-CA-RECORD REDEFINES PCDS-00-RECORD.                         
               10  CA-REC-TYPE                    PIC X(002).                   
                   88  CA-REC                              VALUE 'CA'.          
               10  FILLER                         PIC X(002).                   
               10  CA-PAT-CNTL-NO                 PIC X(017).                   
               10  FILLER                         PIC X(003).                   
               10  CA-PAT-TYPE-OF-RES             PIC X(001).                   
               10  FILLER                         PIC X(001).                   
               10  CA-CLAIM-TYPE-IND              PIC X(002).                   
               10  CA-ORIGIN-CODE                 PIC X(009).                   
               10  CA-PAYER-CLAIM-CONTROL-NO      PIC X(017).                   
               10  CA-SUBMITTER-CLAIM-ID-NO       PIC X(025).                   
               10  CA-PURCHASE-ORDER-NO           PIC X(010).                   
               10  CA-TRIBE                       PIC 9(003).                   
               10  CA-TRIBE-X  REDEFINES                                        
                   CA-TRIBE                       PIC X(003).                   
               10  CA-RESIDENCY-CODE              PIC 9(007).                   
               10  CA-RESIDENCY-CODE-X    REDEFINES                             
                   CA-RESIDENCY-CODE              PIC X(007).                   
               10  CA-PAT-HEALTH-REC-NO           PIC 9(006).                   
               10  CA-PAT-HEALTH-REC-NO-X REDEFINES                             
                   CA-PAT-HEALTH-REC-NO           PIC X(006).                   
               10  CA-AUTH-FAC-NO                 PIC X(009).                   
               10  CA-MULTIPLE-CLAIM-IND          PIC X(001).                   
               10  FILLER                         PIC X(077).                   
                                                                                
      *----------------------------------------------------------------*        
      *      NSF OVERFLOW PATIENT INFORMATION RECORD (CB)              *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-CB-RECORD REDEFINES PCDS-00-RECORD.                         
               10  CB-REC-TYPE                    PIC X(002).                   
                   88  CB-REC                              VALUE 'CB'.          
               10  FILLER                         PIC X(002).                   
               10  CB-PAT-CNTL-NO                 PIC X(020).                   
               10  CB-NSF-PATIENT-NAME.                                         
                   15  CB-NSF-PAT-LNAME           PIC X(035).                   
                   15  CB-NSF-PAT-FNAME           PIC X(012).                   
               10  CB-NSF-PATIENT-ADDRESS.                                      
                   15  CB-NSF-PAT-ADDRESS-1       PIC X(030).                   
                   15  CB-NSF-PAT-ADDRESS-2       PIC X(030).                   
                   15  CB-NSF-PAT-CITY            PIC X(020).                   
               10  FILLER                         PIC X(041).                   
                                                                                
      *----------------------------------------------------------------*        
      *      NSF OVERFLOW LEGAL REPRESENTATIVE RECORD (CC)             *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-CC-RECORD REDEFINES PCDS-00-RECORD.                         
               10  CC-REC-TYPE                    PIC X(002).                   
                   88  CC-REC                              VALUE 'CC'.          
               10  FILLER                         PIC X(002).                   
               10  CC-PAT-CNTL-NO                 PIC X(020).                   
               10  CC-NSF-RESPONSIBLE-PERSON-INFO.                              
                   15  CC-NSF-RP-NAME.                                          
                       20  CC-NSF-RP-LNAME        PIC X(035).                   
                       20  CC-NSF-RP-FNAME        PIC X(012).                   
                   15  CC-NSF-RP-ADDRESS-1        PIC X(030).                   
                   15  CC-NSF-RP-ADDRESS-2        PIC X(030).                   
                   15  CC-NSF-RP-CITY             PIC X(020).                   
               10  FILLER                         PIC X(041).                   
                                                                                
      *----------------------------------------------------------------*        
      *          HIPAA PATIENT INFORMATION RECORD (CN)                 *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-CN-RECORD REDEFINES PCDS-00-RECORD.                         
               10  CN-REC-TYPE                    PIC X(002).                   
                   88  CN-REC                              VALUE 'CN'.          
               10  FILLER                         PIC X(002).                   
               10  CN-PAT-CNTL-NO                 PIC X(020).                   
               10  CN-ADDITIONAL-PAT-NAME         PIC X(060).                   
               10  CN-PAT-COUNTRY-CD              PIC X(003).                   
               10  CN-PAT-HL-ID-NO                PIC X(006).                   
               10  CN-PAT-HL-PARENT-ID-NO         PIC X(006).                   
               10  CN-PAT-PC-CLAIM-NO             PIC X(025).                   
               10  FILLER                         PIC X(068).                   
                                                                                
      *----------------------------------------------------------------*        
      *       HIPAA ADDITIONAL PATIENT INFORMATION RECORD (CP)         *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-CP-RECORD REDEFINES PCDS-00-RECORD.                         
               10  CP-REC-TYPE                    PIC X(002).                   
                   88  CP-REC                              VALUE 'CP'.          
               10  FILLER                         PIC X(002).                   
               10  CP-PAT-CNTL-NO                 PIC X(020).                   
               10  CP-PAT-WEIGHT-BASIS-OF-MEAS    PIC X(002).                   
               10  CP-PAT-WEIGHT                  PIC X(004).                   
               10  CP-PREGNANCY-IND               PIC X(001).                   
               10  CP-PLACE-SVC-CD                PIC X(002).                   
               10  FILLER                         PIC X(002).                   
               10  CP-RESPONSIBLE-PERSON-ADDL-NM  PIC X(060).                   
               10  FILLER                         PIC X(097).                   
                                                                                
      *----------------------------------------------------------------*        
      *          INTERNAL TRACKING INFORMATION RECORD (CZ)             *        
      *----------------------------------------------------------------*        
           05  PCDS-CZ-RECORD REDEFINES PCDS-00-RECORD.                         
               10  CZ-REC-TYPE                    PIC X(002).                   
                   88  CZ-REC                              VALUE 'CZ'.          
               10  FILLER                         PIC X(002).                   
               10  CZ-PAT-CNTL-NO                 PIC X(020).                   
               10  CZ-CORN                        PIC X(020).                   
0312RR         10  CZ-SUBM-UNIQ-CLAIM-ID.                                       
0312RR             12  CZ-VENDOR-CLAIM-NUM    PIC X(20).                        
0312RR             12  FILLER                 PIC X(10).                        
0312RR         10  FILLER                     PIC X(118).                       
                                                                                
      *----------------------------------------------------------------*        
      *          PAYER INFORMATION RECORD (D0)                         *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-D0-RECORD REDEFINES PCDS-00-RECORD.                         
             07  D0-GROUP-MOVE.                                                 
               10  D0-REC-SEQ-NUM.                                              
                   15  D0-REC-TYPE                PIC X(002).                   
                       88  D0-REC                           VALUE 'D0'.         
                   15  D0-SEQ-NUM                 PIC 9(002).                   
               10  D0-PAT-CNTL-NUM                PIC X(017).                   
      **                                                                        
      * NOTE: WHEN ADDING NEW SOP CODES, BE SURE TO INCLUDE THEM IN             
      *       PROGRAM MCARDATA SO THEY DO NOT REJECT                            
      **                                                                        
               10  D0-PYMT-SRCE-CODE              PIC X(001).                   
                   88  D0-SELF-PAY                          VALUE 'A'.          
                   88  D0-WORKERS-COMP                      VALUE 'B'.          
                   88  D0-MEDICARE                          VALUE 'C'.          
                   88  D0-MEDICAID-SOP                      VALUE 'D'.          
                   88  D0-OTHER-FED-PGM                     VALUE 'E'.          
                   88  D0-COMMERCIAL-INSURANCE              VALUE 'F'.          
                   88  D0-ALL-COMMERCIAL                    VALUE               
                       'F' 'D' 'G' 'H' 'I' 'P' 'X'.                             
                   88  D0-BC-BS                             VALUE 'G'.          
                   88  D0-CHAMPUS                           VALUE 'H'.          
                   88  D0-OTHER-SOURCE-OF-PAYMENT           VALUE 'I'.          
                   88  D0-HMO                               VALUE 'Y'.          
                   88  D0-FEP                               VALUE 'J'.          
                   88  D0-CENTRAL-CERT                      VALUE 'K'.          
                   88  D0-SELF-ADMIN                        VALUE 'L'.          
                   88  D0-FAMILY-FRIENDS                    VALUE 'M'.          
                   88  D0-MNGCR-NON-HMO                     VALUE 'N'.          
                   88  D0-BLUE-CROSS-EMCD                   VALUE 'P'.          
                   88  D0-TITLE-V                           VALUE 'T'.          
                   88  D0-VETERANS-ADMIN                    VALUE 'V'.          
                   88  D0-PPO                               VALUE 'X'.          
                   88  D0-TRIG-VALID-SOP                                        
                           VALUE 'A' 'B' 'C' 'D' 'E' 'F'                        
                                 'G' 'H' 'J' 'K' 'L' 'M'                        
                                 'N' 'P' 'Q' 'R' 'S' 'T'                        
                                 'V' 'W' 'X' 'Y' 'Z'                            
                                 '1' '2' '3' '4' '5' '8'.                       
                   88  D0-TRIG-VALID-SOP-PROF                                   
                           VALUE '1' THRU '5'                                   
                                 'A' THRU 'H'                                   
                                 'K' 'M' 'P' 'Q' 'R' 'S'                        
                                 'T' 'V' 'W' 'X' 'Y' 'Z'.                       
                   88  D0-TRIG-VALID-SOP-DENT                                   
                           VALUE '1' '2' '4' '5' '8'                            
                                 'A' THRU 'H'                                   
                                 'J' 'L' 'M' 'N' 'P' 'Q'                        
                                 'R' 'S' 'V' 'X' 'Y' 'Z'.                       
                   88  D0-CLAIM-VALID-SOP                                       
                           VALUE 'C' 'D' 'F' 'G' 'H' 'K'                        
                                 'L' 'M' 'N' 'P' 'Q' 'R'                        
                                 'S' 'T' 'V' 'W' 'X' 'Z'                        
                                 '1' '2' '3' '4' '5' '8'.                       
                                                                                
               10  D0-PAYING-OFFICE.                                            
                   15  D0-PAYOR-ID                PIC X(005).                   
                   15  D0-SYN-PAY-ID REDEFINES D0-PAYOR-ID.                     
                       20  D0-SYN-PYR-1           PIC X(001).                   
                           88 SYNAPTEK-PYR  VALUE 'S' 'T' 'H'.                  
                           88 CPS-DCDS-PYR  VALUE 'C'.                          
                       20  FILLER                 PIC X(004).                   
                   15  D0-EMC-PAYER REDEFINES  D0-PAYOR-ID.                     
                       20  D0-EMC-PAYER-2         PIC X(002).                   
                           88 EMC-PAYER  VALUE 'SB' 'SK' 'SM'.                  
                           88 VIVRA      VALUE 'VN'.                            
                       20  FILLER                 PIC X(003).                   
                   15  D0-CLAIM-OFFICE.                                         
                       20 D0-CLAIM-OFFICE-1       PIC X(001).                   
                       20 FILLER                  PIC X(003).                   
               10  D0-INSURED-ID                  PIC X(017).                   
               10  D0-CLAIM-CERT-ID-NUM REDEFINES D0-INSURED-ID.                
                   15  D0-CLAIM-CERT-ID-NUM3      PIC X(003).                   
                   15  D0-CLAIM-CERT-ID-NUM5      PIC X(002).                   
                   15  FILLER                     PIC X(008).                   
                   15  D0-CLAIM-CERT-ID-REST      PIC X(004).                   
               10  D0-PAYOR-NAME                  PIC X(017).                   
               10  D0-INS-GRP-PLCY-NUM.                                         
                   15  D0-INS-GRP-PLCY-NUM1       PIC X(001).                   
                   15  FILLER                     PIC X(019).                   
               10  D0-INS-GROUP-NAME              PIC X(017).                   
               10  D0-INSURED-NAME.                                             
                   15  D0-INS-LAST-NAME.                                        
                       20  D0-LAST-NAME1          PIC X(001).                   
                       20  FILLER                 PIC X(019).                   
                   15  D0-INS-FIRST-NAME.                                       
                       20  D0-FIRST-NAME1         PIC X(001).                   
                       20  FILLER                 PIC X(009).                   
                   15  D0-INS-MID-INTL            PIC X(001).                   
               10  D0-INS-SEX                     PIC X(001).                   
                   88  D0-INSURED-SEX-NOT-ENTERED          VALUE ' '.           
                   88  D0-INSURED-IS-MALE                  VALUE 'M'.           
                   88  D0-INSURED-IS-FEMALE                VALUE 'F'.           
                   88  D0-INSURED-IS-UNKNOWN               VALUE 'U'.           
                   88  D0-VALID-INSURED-SEX                                     
                           VALUE 'M' 'F' 'U'.                                   
               10  D0-RLSE-INFO-CERT-IND          PIC X(001).                   
               10  D0-BENEF-ASGN-CERT-IND         PIC X(001).                   
                   88  D0-BENEFITS-ASSIGNED                VALUE 'Y'.           
                   88  D0-BENEFITS-NON-ASSIGNED            VALUE 'N'.           
                                                                                
               10  D0-PAT-REL-TO-INS              PIC X(002).                   
                   88  D0-PATIENT-IS-THE-INSURED           VALUE '01'.          
                   88  D0-PATIENT-IS-SPOUSE-OF-INS         VALUE '02'.          
                   88  D0-PATIENT-IS-CHILD-OF-INS          VALUE '03'.          
                   88  D0-VERSION1-EMCDS-REL-CODE                               
                                            VALUE '04' THRU '19'.               
                   88  D0-CHILD-NO-RSP                     VALUE '04'.          
                   88  D0-STEP-CHILD                       VALUE '05'.          
                   88  D0-FOSTER-CHILD                     VALUE '06'.          
                   88  D0-WARD-OF-COURT                    VALUE '07'.          
                   88  D0-EMPLOYEE                         VALUE '08'.          
                   88  D0-UNKNOWN                          VALUE '09'.          
                   88  D0-HANDICAP-DEP                     VALUE '10'.          
                   88  D0-ORGAN-DONOR                      VALUE '11'.          
                   88  D0-CADAVER-DONOR                    VALUE '12'.          
                   88  D0-GRANDCHILD                       VALUE '13'.          
                   88  D0-NIECE-NEPHEW                     VALUE '14'.          
                   88  D0-INJURED-PLAINTIFF                VALUE '15'.          
                   88  D0-SPONSRD-DEP                      VALUE '16'.          
                   88  D0-MIN-DEP-OF-MIN                   VALUE '17'.          
                   88  D0-PARENT                           VALUE '18'.          
                   88  D0-GRANDPARENT                      VALUE '19'.          
                   88  D0-LIFE-PARTNER                     VALUE '20'.          
                   88  D0-EMANCIPATED-MINOR                VALUE '21'.          
                   88  D0-SIGNIFICANT-OTHER                VALUE '22'.          
                   88  D0-DEPENDENT                        VALUE '23'.          
                   88  D0-ADOPTED-CHILD                    VALUE '24'.          
                   88  D0-OTHER-ADULT                      VALUE '25'.          
                   88  D0-MEDICAL-ONLY          VALUES '04' THRU '07'           
                                                       '11' THRU '21'.          
                   88  D0-OTHER-PATIENT-RELATION           VALUE '99'.          
                   88  D0-VALID-PATIENT-REL-CODE                                
                                            VALUE '01' THRU '25' '99'.          
                                                                                
               10  D0-PAT-SIGN-IND                PIC X(001).                   
                   88  D0-AUTH-FRM-BLK-12-13               VALUE 'B'.           
                   88  D0-HCFA-FRM                         VALUE 'C'.           
                   88  D0-AUTH-FRM-BLK-13                  VALUE 'M'.           
                   88  D0-PROV-SIGNED                      VALUE 'P'.           
                   88  D0-AUTH-FRM-BLK-12                  VALUE 'S'.           
                   88  D0-VALID-SIGN-IND               VALUE                    
                                                'B' 'C' 'M' 'P'  'S'.           
               10  D0-INSUR-BIRTH-DATE.                                         
                   15 D0-INSUR-BIRTH-DATE-YYYY.                                 
                      20 D0-INSUR-BIRTH-DATE-YY1  PIC 9(002).                   
                      20 D0-INSUR-BIRTH-DATE-YY2  PIC 9(002).                   
                   15 D0-INSUR-BIRTH-DATE-MM      PIC 9(002).                   
                   15 D0-INSUR-BIRTH-DATE-DD      PIC 9(002).                   
               10  D0-INSUR-BIRTH-DATE-X REDEFINES                              
                   D0-INSUR-BIRTH-DATE.                                         
                   15 D0-INSUR-BIRTH-DATE-CC-X    PIC X(002).                   
                   15 D0-INSUR-BIRTH-DATE-YY-X    PIC X(002).                   
                   15 D0-INSUR-BIRTH-DATE-MM-X    PIC X(002).                   
                   15 D0-INSUR-BIRTH-DATE-DD-X    PIC X(002).                   
                                                                                
               10  D0-PROVIDER-ID                 PIC X(013).                   
               10  FILLER                         PIC X(002).                   
               10  D0-INSURANCE-TYPE-CODE         PIC X(002).                   
                   88  D0-AUTO-POLICY                       VALUE 'AP'.         
                   88  D0-COMMERCIAL                        VALUE 'C1'.         
                   88  D0-MEDICARE-COND-PRIMARY             VALUE 'CP'.         
                   88  D0-GROUP-POLICY                      VALUE 'GP'.         
                   88  D0-HMO                               VALUE 'HM'.         
                   88  D0-INDIVIDUAL-POLICY                 VALUE 'IP'.         
                   88  D0-LONG-TERM-POLICY                  VALUE 'LD'.         
                   88  D0-LITIGATION                        VALUE 'LT'.         
                   88  D0-MEDICARE-B                        VALUE 'MB'.         
                   88  D0-MEDICAID                          VALUE 'MC'.         
                   88  D0-MEDIGAP-POLICY                    VALUE 'MG'.         
                   88  D0-MEDIGAP-B                         VALUE 'MI'.         
                   88  D0-MEDICARE-PRIME                    VALUE 'MP'.         
                   88  D0-OTHER                             VALUE 'OT'.         
                   88  D0-PERSONAL-PAYMENT                  VALUE 'PP'.         
                   88  D0-SUPP-POLICY                       VALUE 'SP'.         
                   88  D0-WORKING-AGED-BENEFIT              VALUE '12'.         
                   88  D0-ESRD                              VALUE '13'.         
                   88  D0-AUTO-NO-FAULT                     VALUE '14'.         
                   88  D0-WORKERS-COMP                      VALUE '15'.         
                   88  D0-PHS-OR-OFED                       VALUE '16'.         
                   88  D0-BLACK-LUNG                        VALUE '41'.         
                   88  D0-VA                                VALUE '42'.         
                   88  D0-DISABLE-UNDER-65                  VALUE '43'.         
                   88  D0-ANY-LIABILTY-INS                  VALUE '47'.         
                   88  D0-VALID-INS-TYPE-1        VALUES                        
                       'AP' 'C1' 'CP' 'GP' 'HM' 'IP' 'LD' 'LT'                  
                       'MB' 'MC' 'MI' 'OT' 'PP' 'SP'.                           
                   88  D0-VALID-INS-TYPE-2        VALUES                        
                       'MG' 'MP' '12' '13' '14' '15' '16' '41'                  
                       '42' '43' '47'.                                          
               10  D0-INSURED-ID-QUALIFIER        PIC X(002).                   
                   88  D0-MEMBER-ID-NUMBER                 VALUE 'MI'.          
                   88  D0-MUTUALLY-DEFINED                 VALUE 'ZZ'.          
                   88  D0-VALID-INS-ID-QUALIFIERS    VALUES 'MI' 'ZZ'.          
                                                                                
               10  FILLER                         PIC X(015).                   
               10  D0-PRIOR-PYMT-AMNT             PIC S9(008)V99.               
               10  D0-PRIOR-PYMT-AMNT-U REDEFINES                               
                   D0-PRIOR-PYMT-AMNT             PIC 9(008)V99.                
               10  D0-CLAIM-PYMT-TRIGGER          PIC X(001).                   
                   88  D0-CLAIM-TRIGGER-OFF                VALUE ' '.           
                   88  D0-CLAIM-TRIGGER-SET                VALUE 'X'.           
                   88  D0-VALID-CLAIM-TRIGGER              VALUE 'X'.           
                                                                                
           05  D0-ALPHA REDEFINES PCDS-00-RECORD.                               
               10  FILLER                         PIC X(002).                   
               10  D0-SEQ-NUM-X                   PIC X(002).                   
               10  FILLER                         PIC X(017).                   
               10  D0-PYMT-SRCE-CODE-X            PIC X(001).                   
               10  D0-PAYOR-ID-X                  PIC X(005).                   
      **** THE FOLLOWING 88 LEVELS ARE FOR CARRIER SPECIFIC EDITS.              
      **** ADD CARRIERS AS NEEDED, AND KEEP IN ALPHABETICAL ORDER - H.E.        
                   88 ACCORDIA-NATIONAL         VALUE '87815'.                  
                   88 ADMAR                     VALUE '95285'.                  
                   88 ADMINISTAR                VALUE '35175'.                  
                   88 ADVOCATE                  VALUE '65093'                   
                                                      '36320'.                  
                   88 ADVOCATE-ORIG             VALUE '65093'.                  
                   88 AETNA-COMMERCIAL          VALUE '60054'.                  
                   88 AETNA-DDD-ENC             VALUE '60055'.                  
                   88 AFL-CIO                   VALUE '34444'.                  
                   88 ALLIANCE-PPO              VALUE '52149'.                  
                   88 ALLMERICA                 VALUE '69140'.                  
                   88 AMERICAID                 VALUE '27514'                   
                                                      '27515'.                  
                   88 AMERI-HLTH-HMO-TX         VALUE '76047'.                  
                   88 APEX                      VALUE '34196'.                  
                   88 APWU                      VALUE '44444'.                  
                   88 AVMED                     VALUE '59274'.                  
                   88 BCCA-ENC                  VALUE '47199'.                  
                   88 BCCA-SPECIAL              VALUE '47198'                   
                                                      '80314'                   
                                                      '65099'.                  
                   88 BC-BS-ALA                 VALUE 'CBAL1'.                  
                   88 BC-BS-ARKANSAS            VALUE 'CBAR1'.                  
                   88 BC-BS-GEORGIA             VALUE 'TB600'.                  
                   88 BC-BS-ILLINOIS            VALUE 'CB621'.                  
                   88 BC-BS-KANSAS              VALUE 'BSKS ' 'CB951'.          
                   88 BC-BS-LA-DENTAL           VALUE '23739'.                  
                   88 BC-BS-LA-MEDICAL          VALUE '23738'.                  
                   88 BC-BS-MA                  VALUE 'CBMA1'.                  
                   88 BC-BS-MISSOURI            VALUE '00241'.                  
                   88 BC-BS-MO                  VALUE 'SB741'.                  
                   88 BC-BS-PENNSYLVANIA        VALUE 'CB865'.                  
                   88 BC-BS-WISCONSIN           VALUE 'CB950'.                  
                   88 BC-CALIFORNIA             VALUE '47198'.                  
                   88 BC-CONNECTICUT            VALUE '65358'.                  
                   88 BC-IDAHO-1                VALUE 'CBID1'.                  
                   88 BC-IDAHO-2                VALUE 'CBID2'.                  
                   88 BC-IDAHO-3                VALUE 'CKID1'.                  
                   88 BENEFIT-PLANNERS          VALUE '74223'.                  
                   88 BENEFIT-TRUST-LIFE        VALUE '61425'.                  
                   88 BLUE-ALLIANCE             VALUE '10316'.                  
                   88 BLUE-CROSS-CALIFORNIA     VALUE '47198'.                  
                   88 BLUE-CROSS-CT             VALUE '65358'.                  
                   88 BLUE-CROSS-SC             VALUE '38250'.                  
                   88 BLUE-SHIELD-NJ            VALUE '22099'.                  
                   88 BLUE-SHIELD-TX            VALUE 'CB900'.                  
                   88 BOON-CHAPMAN              VALUE '74230'                   
                                                      '74232'                   
                                                      '77230'.                  
                   88 BUYERS-HEALTH-CARE        VALUE '62137'.                  
                   88 CALOPTIMA                 VALUE '33059'.                  
                   88 CAPE-HEALTH               VALUE '38245'.                  
                   88 CARE-NETWORK              VALUE '96148'.                  
                   88 CAREFIRST                 VALUE 'SB580'                   
                                                      'SB690'                   
                                                      'SB691'.                  
                   88 CARESOURCE                VALUE '31114'.                  
                   88 CARITEN                   VALUE '62072'                   
                                                      '62073'.                  
                   88 CASCADE                   VALUE '93040'.                  
                   88 CATERPILLAR-INC           VALUE '37060'.                  
                   88 CHAMPUS-PALMETTO          VALUE '57106'.                  
                   88 CENTRAL-RESERVE-LIFE      VALUE '34097'.                  
                   88 CHAMPUS-NE-REGION         VALUE '35175'.                  
                   88 CHC                       VALUE '48145'.                  
                   88 CHIN                      VALUE '47191'                   
                                                      '94038'                   
                                                      '95569'                   
                                                      '68247'                   
                                                      '88269'.                  
                   88 CHIN-ENCOUNTERS           VALUE '47193'                   
                                                      '94032'                   
                                                      '95570'                   
                                                      '68249'                   
                                                      '88270'.                  
                   88 CHIN-GROUP                VALUE '47191'                   
                                                      '47193'                   
                                                      '68247'                   
                                                      '68249'                   
                                                      '88269'                   
                                                      '88270'                   
                                                      '94032'                   
                                                      '94038'                   
                                                      '95569'.                  
                   88 CHUBB-LIFE                VALUE '22083'.                  
                   88 CIGNA                     VALUE '62308'.                  
                   88 CIGNA-DENTAL              VALUE '10050'.                  
                   88 CIGNA-ENCOUNTERS          VALUE '62309'.                  
                   88 CIGNA-HLTH-PROV-MED       VALUE '55555'.                  
                   88 CLEVELAND-HEALTH          VALUE '34172'.                  
                   88 CNA                       VALUE '62413'.                  
                   88 CNA-INSURANCE             VALUE '36094'.                  
                   88 COALITION-CARE            VALUE '22373'.                  
                   88 COLUMBIA-UNITED           VALUE '91162'.                  
                   88 COMM-HEALTH-PLAN          VALUE '90010'.                  
                   88 CONNECTICARE              VALUE '06105'.                  
                   88 CORESOURCE                VALUE '48153'.                  
                   88 CORESOURCE-NC             VALUE '35180'.                  
                   88 COVENTRY                  VALUE '25127'                   
                                             '25129' THRU '25138'.              
                   88 COVENTRY-SPECIAL          VALUE '25126'                   
                                                      '25127'                   
                                                      '25128'                   
                                                      '25129'                   
                                                      '25130'                   
                                                      '25132'                   
                                                      '25133'                   
                                                      '25134'                   
                                                      '25135'                   
                                                      '25136'                   
                                                      '25137'                   
                                                      '25139'                   
                                                      '25140'                   
                                                      '25141'                   
                                                      '25142'                   
                                                      '25143'                   
                                                      '25144'.                  
                   88 CPS-DENTAL-ALL-PAYER      VALUE '06126'.                  
                   88 DAYMED                    VALUE '31111'.                  
                   88 DEATH                     VALUE '00000'.                  
                   88 DELTANET                  VALUE '77777'.                  
                   88 DELTA-DENTAL-ARIZONA      VALUE '86027'.                  
                   88 DELTA-DENTAL-GROUP        VALUE '11198'                   
                      '43090' '99010' '23166' '66043' '85022'                   
                      '51022' '52147' '31096' '94276' '02027'.                  
                   88 DELTA-DENTAL-COMPANY      VALUE '02027'                   
                      '05029' '11198' '23166' '31096' '51022'                   
                      '52147' '56101' '77777' '85022' '86027'.                  
                   88 DELTA-DENTAL-IA           VALUE 'CDIA1'.                  
                   88 DELTA-DENTAL-IOWA         VALUE 'CDIA1'.                  
                   88 DELTA-DENTAL-MICH-1       VALUE 'CDMI0'.                  
                   88 DELTA-DENTAL-MICH-2       VALUE 'CDOH1'.                  
                   88 DELTA-DENTAL-MICH-3       VALUE 'CDIN1'.                  
                   88 DELTA-DENTAL-MN           VALUE 'CDMN1'.                  
                   88 DELTA-DENTAL-NC           VALUE '56101'.                  
                   88 DELTA-DENTAL-ND           VALUE 'CDND1'.                  
                   88 DELTA-DENTAL-NEBRASKA     VALUE 'CDNE1'.                  
                   88 DELTA-DENTAL-WYOMING      VALUE 'CDWY1'.                  
                   88 DELTA-USA                 VALUE 'CDUS1'.                  
                   88 DENTAL-WY                 VALUE 'CKWY1'.                  
                   88 DENTI-CAL                 VALUE '94146'.                  
                   88 EASTBAY-SUTTER            VALUE '94318'.                  
                   88 EMERALD-HEALTH-NET        VALUE '34167'.                  
                   88 EMP-BENEFITS              VALUE '38241'.                  
                   88 EMP-HEALTH                VALUE '73288'.                  
                   88 EMPLOYERS-MUTUAL          VALUE '59298'.                  
                   88 ETHIX                     VALUE '91135'.                  
                   88 EQUICOR                   VALUE '62944'.                  
                   88 FAMILY-HLTH-GROUP         VALUE '43173'                   
                                                      '43174'                   
                                                      '43175'.                  
                   88 FAMILY-MEMBER-DENTAL      VALUE 'CX002'.                  
                   88 FIRSTGUARD                VALUE '90060'.                  
                   88 FHP                       VALUE '84506'.                  
                   88 FIRST-HEALTH              VALUE '52180'.                  
                   88 FIRST-PRIORITY            VALUE '23241'.                  
                   88 FLORIDA-FIRST             VALUE '59276'.                  
                   88 FLORIDA-HOSPITAL-CSC      VALUE '59321'.                  
                   88 FORTIS-BENEFITS           VALUE '70408'.                  
                   88 FORTIS-TIME-INS           VALUE '39065'.                  
                   88 FORTIS-WOODBURY           VALUE '81017'.                  
                   88 GATEWAY                   VALUE '25169'.                  
                   88 GEISINGER                 VALUE '75273'.                  
                   88 GENERAL-AMERICAN          VALUE '63665'.                  
                   88 GEORGIA-MEDICAID          VALUE 'SKGA0'.                  
                   88 GEORGIA-STATE-MERIT       VALUE 'TX004'.                  
                   88 GREAT-WEST                VALUE '80705'.                  
                   88 GREAT-WEST-HPS-GRP        VALUE '51459'                   
                     '66893' '80705' '95379' '95388' '95412' '96559'.           
                   88 GROUP-HEALTH              VALUE '13551'.                  
                   88 GRP-HLTH-COOP-EAST        VALUE '91121'.                  
                   88 GRP-HLTH-COVENTRY         VALUE '00340'.                  
                   88 COVENTRY-GROUP-25141      VALUE '25141'.                  
                   88 GUARDIAN-LIFE             VALUE '64246'.                  
                   88 HALLEY                                                    
                                           VALUE 'HX001' THRU 'HX047',          
                                                 'HX049' THRU 'HX052',          
                                                 'HX055', 'HB801'               
                                                 'HB580', 'HB690'               
                                                 'HB833', 'HB950'.              
                   88 HARRIS-METH               VALUE '75201'.                  
                   88 HARVARD-COMMUNITY         VALUE '04245'.                  
                   88 HARVARD-PILGRIM           VALUE '04271'.                  
                   88 HEALTH-ALLIANCE-PLAN      VALUE '38224'.                  
                   88 HEALTH-AMERICA            VALUE '25126'                   
                                                      '25139'.                  
                   88 HEALTH-CARE-COALITION     VALUE '34193'.                  
                   88 HEALTHEASE                VALUE '59608'.                  
                   88 HEALTH-CARE-PLAN          VALUE '16107'.                  
                   88 HEALTH-ECONOMICS          VALUE '75196'.                  
                   88 HEALTH-FIRST              VALUE '31081'.                  
                   88 HEALTHFIRST-INC           VALUE '80141'.                  
                   88 HEALTHLINK-HMO            VALUE '90001'.                  
                   88 HEALTHLINK-PPO            VALUE '96475'.                  
                   88 HEALTHNET                 VALUE '95568'.                  
                   88 HEALTHNET-CA-ENC          VALUE '95570'.                  
                   88 HEALTHNET-KS              VALUE '43132'.                  
                   88 HEALTH-INS-PLAN-NY        VALUE '55247'.                  
                   88 HEALTH-PARTNERS           VALUE '94267'.                  
                   88 HEALTH-PARTNERS-PA        VALUE '80142'.                  
                   88 HEALTH-PLAN-REDWOODS      VALUE '94254'.                  
                   88 HEALTH-PLAN-SERVICES      VALUE '59140'.                  
                   88 HEALTH-POWER              VALUE '31106'.                  
                   88 HEALTH-SOURCE             VALUE '02038'                   
                                                      '06119'                   
                                                      '01041'                   
                                                      '71074'.                  
                   88 HEALTH-SOURCE-AR          VALUE '71075'.                  
                   88 HEALTH-SOURCE-GA          VALUE '58210'.                  
                   88 HEALTH-SOURCE-IN          VALUE '35167'.                  
                   88 HEALTH-SOURCE-KY          VALUE '61127'.                  
                   88 HEALTH-SOURCE-NC          VALUE '56147'.                  
                   88 HEALTH-SOURCE-NC-ENC      VALUE '56148'.                  
                   88 HEALTH-SOURCE-NH          VALUE '02039'.                  
                   88 HEALTH-SOURCE-NTX         VALUE '75255'.                  
                   88 HEALTH-SOURCE-N-TX        VALUE '75255'.                  
                   88 HEALTH-SOURCE-NY          VALUE '16126'.                  
                   88 HEALTH-SOURCE-OH          VALUE '31141'.                  
                   88 HEALTH-SOURCE-TN          VALUE '62129'.                  
                   88 HEALTH-SOURCE-TN-MED      VALUE '62130'.                  
                   88 HEALTH-SOURCE-TX          VALUE '74276'.                  
                   88 HERITAGE                  VALUE '59230'.                  
                   88 HILL                      VALUE '00046'.                  
                   88 HUMANA                    VALUE '61101'.                  
                   88 HUMANA-ENCOUNTERS         VALUE '61102'.                  
                   88 HUMANA-MILITARY           VALUE '61125'.                  
                   88 IBA                       VALUE '38234'.                  
                   88 INHEALTH-INC              VALUE '31112'.                  
                   88 INFOTRUST                 VALUE '36364'                   
                                                      '38337'                   
                                                      '39185'                   
                                                      '42142'                   
                                                      '58246'                   
                                                      '58247'                   
                                                      '58248'                   
                                                      '84141'.                  
                   88 INFO-NETWORK              VALUE '86047'                   
                                                      '86048'                   
                                                      '86049'                   
                                                      '86001'                   
                                                      '86002'                   
                                                      '86003'.                  
                   88 INTEGRA                   VALUE '31127'.                  
                   88 INTERACTIVE-DIAG-SVC      VALUE '94315'.                  
                   88 JARDIN-GROUP-SERVICES     VALUE '14168'.                  
                   88 JOHN-ALDEN                VALUE '41099'.                  
                   88 JOHN-DEERE                VALUE '95378'.                  
                   88 JOHN-HANCOCK              VALUE '65099'.                  
                   88 KAISER-N-CALIF            VALUE '94135'                   
                                                      '98765'.                  
                   88 KAISER-S-CALIF            VALUE '54321'                   
                                                      '94132'                   
                                                      '94134'.                  
                   88 KAISER-NC                 VALUE '21313'.                  
                   88 KAISER-MD                 VALUE '52095'.                  
                   88 KEYSTONE-MERCY            VALUE '23284'                   
                                                      '22248'                   
                                                      '22326'                   
                                                      '43172'.                  
                   88 LIBERTY-MUTUAL            VALUE '11123'.                  
                   88 LIFE-OF-GEORGIA           VALUE '65471'.                  
                   88 MACHIGONNE                VALUE '10317'.                  
                   88 MANAGED-HEALTH-SVC        VALUE '39187'.                  
                   88 MASS-MUTUAL               VALUE '80314'.                  
                   88 MATTHEW-THORNTON          VALUE '02030'.                  
                   88 MCARE                     VALUE '38264'.                  
                   88 MEDICA                    VALUE '94265'.                  
                   88 MEDCOST-INC               VALUE '56162'.                  
                   88 MEDICAID-OF-INDIANA       VALUE 'CKIN1'.                  
                   88 MEDICAID-OF-KANSAS        VALUE 'KSMD ' 'CKKS1'.          
                   88 MEDICAID-KENTUCKY         VALUE 'CKKY1'.                  
                   88 MEDICAID-MISSOURI         VALUE 'SKMO0'.                  
                   88 MEDICAID-OF-MN            VALUE 'CKMN1'.                  
                   88 MEDICAID-NC               VALUE 'CKNC1'.                  
                   88 MEDICAID-OF-OHIO          VALUE 'SKOH0'.                  
                   88 MEDICAID-OF-TEXAS         VALUE 'CKTX1'.                  
                   88 MEDICAID-OF-WASHINGTN     VALUE 'CKWA1'.                  
                   88 MEDICAID-OF-WASHINGTON    VALUE 'CKWA1'.                  
                   88 MEDNET                    VALUE '63105'.                  
                   88 METROPOLITAN-LIFE         VALUE '65978'.                  
                   88 MERCY-CARE                VALUE '86052'.                  
                   88 MERCY-HEALTH-PLANS        VALUE '43166'.                  
                   88 MID-ATLANTIC-MED-SVC      VALUE '52148'.                  
                   88 MMO-CAREWORKS-29076       VALUE '29076'.                  
                   88 MMO-CAREWORKS-10010       VALUE '10010'.                  
                   88 MOHAWK-VALLEY             VALUE '14165'.                  
                   88 MID-ATL-MED-SVC           VALUE '52148'.                  
                   88 MUTUAL-OF-OMAHA           VALUE '71412'.                  
                   88 NAMM                      VALUE '93094' THRU              
                                                      '93111'.                  
                   88 NEW-YORK-LIFE             VALUE '66915'.                  
                   88 NORTH-AM-MED-MGMT         VALUE 'E3510'.                  
                   88 NORTH-DAKOTA-DENTAL       VALUE 'CX004'.                  
                   88 NYLCARE                   VALUE '66917'                   
                                                      '66918' '66919'           
                                                      '66920' '66921'           
                                                      '66920' '66921'           
                                                      '66922' '66923'           
                                                      '66924' '66925'           
                                                      '66926'.                  
                   88 NYLCARE-NW                VALUE '91166'.                  
                   88 NYMI                      VALUE '14178' '14179'           
                                                      '14180' '14181'           
                                                      '14182' '14183'.          
                   88 OAKTREE-HEALTH-PLAN       VALUE '95356'.                  
                   88 OATH                      VALUE '63092'.                  
                   88 OCHSNER                   VALUE '72127'.                  
                   88 OPT-CHOICE-CAROLINA       VALUE '52152'.                  
                   88 OPTIMUM-CHOICE-PA         VALUE '52151'.                  
                   88 OXFORD                    VALUE '06111'.                  
                   88 PA-BS-DENTAL              VALUE 'CBPA2'.                  
                   88 PACIFICARE                VALUE '95956'                   
                                                      '95959'.                  
                   88 PACIFICARE-2              VALUE '95958'                   
                                                      '95977'                   
                                                      '95975'                   
                                                      '95969'                   
                                                      '95973'.                  
                   88 PACIFICARE-AZNV           VALUE '95964'                   
                                                      '95965'                   
                                                      '95970'                   
                                                      '95971'.                  
                   88 PACIFICARE-ENCOUNTERS     VALUE '95958'                   
                                                      '95977'                   
                                                      '95975'                   
                                                      '95969'                   
                                                      '95973'.                  
                   88 PARKLAND-HEALTH-FIRST     VALUE '66917'.                  
                   88 PARTNERS-NATIONAL         VALUE '56152'.                  
                   88 PASSPORT-HEALTH           VALUE '61129'.                  
                   88 PCA-HEALTH-FLA            VALUE '65018'.                  
                   88 PHILA-AMERICAN            VALUE '67784'.                  
                   88 PHOENIX-HEALTHCARE-TN     VALUE '62153'                   
                                                      '62154'.                  
                   88 PHOENIX-HOME-LIFE         VALUE '67814'                   
                                                      '67815'.                  
                   88 PHCP                      VALUE '65031'.                  
                   88 PHP                       VALUE '16105'.                  
                   88 PHYS-HLTH-SVC             VALUE '06108'.                  
                   88 POE-N-BROWN               VALUE '59087'.                  
                   88 PREFERRED-HEALTH          VALUE '35173'.                  
                   88 PREFERRED-ONE             VALUE '41147'.                  
                   88 PREFERRED-HEALTH-SYS      VALUE '60110'.                  
                   88 PREMIER-BENEFITS          VALUE '43169'.                  
                   88 PRINCIPAL-MUTUAL-LIFE     VALUE '61271'.                  
                   88 PRIMECARE                 VALUE '00003'.                  
                   88 PRIMEHEALTH               VALUE '63088'.                  
                   88 PRIME-VISION              VALUE '56190'.                  
                   88 PROHEALTH-COMPARE         VALUE '31132'.                  
                   88 PROVIDENT                 VALUE '68195'.                  
                   88 PRUDENTIAL                VALUE '68241'.                  
                   88 PRU-CHIN-ENC              VALUE '68249'.                  
                   88 PRU-DENTAL-ENC            VALUE '68246'.                  
                   88 PRU-ENCOUNTERS            VALUE '68245'.                  
                   88 PUGET-SOUND               VALUE '91051'.                  
                   88 QUAL-MED-SEATTLE          VALUE '22300'                   
                      '22310' '22320' '22340' '22350'.                          
                   88 QUAL-CHOICE-AR            VALUE '35174'.                  
                   88 QUAL-CHOICE-NC            VALUE '35172'.                  
                   88 QUAL-CHOICE-VA            VALUE '35171'.                  
                   88 REDWOOD                   VALUE '94234'.                  
                   88 RIMS                      VALUE '06102'                   
                                                      '06103'                   
                                                      '27231'                   
                                                      '27233'                   
                                                      '27236'                   
                                                      '28777'                   
                                                      '35204'                   
                                                      '36404'                   
                                                      '37110'                   
                                                      '37111'                   
                                                      '37112'                   
                                                      '37113'                   
                                                      '37114'                   
                                                      '37115'                   
                                                      '37116'                   
                                                      '37117'                   
                                                      '37118'                   
                                                      '37119'                   
                                                      '37120'                   
                                                      '37121'                   
                                                      '37123'                   
                                                      '37125'                   
                                                      '37126'                   
                                                      '37127'                   
                                                      '37128'                   
                                                      '37210'                   
                                                      '37211'                   
                                                      '37212'                   
                                                      '37213'                   
                                                      '37214'                   
                                                      '37215'                   
                                                      '37216'                   
                                                      '37217'                   
                                                      '37218'                   
                                                      '37220'                   
                                                      '37221'                   
                                                      '37222'                   
                                                      '37223'                   
                                                      '37224'                   
                                                      '37225'                   
                                                      '37226'                   
                                                      '37227'                   
                                                      '37228'                   
                                                      '37229'                   
                                                      '37230'                   
                                                      '37231'                   
                                                      '37232'                   
                                                      '37233'                   
                                                      '37234'                   
                                                      '37235'                   
                                                      '37236'                   
                                                      '44030'                   
                                                      '51037'                   
                                                      '56151'                   
                                                      '59069'.                  
                   88 SAGAMORE                  VALUE '35164'.                  
                   88 SEABURY-SMITH             VALUE '13310'.                  
                   88 SECURE-HEALTH             VALUE '28530'.                  
                   88 SELECTCARE                VALUE '38253'.                  
                   88 SELECTCARE-00014          VALUE '00014'.                  
                   88 SENTARA                   VALUE '54154'.                  
                   88 SOUTHERN-HEALTH           VALUE '25128'.                  
                   88 STATE-FARM                VALUE '31053'.                  
                   88 STJOSEPH                  VALUE '68033'.                  
                   88 SUMMACARE-HMO             VALUE '95202'.                  
                   88 THE-NEW-ENGLAND           VALUE '66893'.                  
                   88 THE-MUTUAL-GROUP          VALUE '70491'.                  
                   88 THREE-RIVERS              VALUE '25175'                   
                                                      '62183'.                  
                   88 TLC                       VALUE '36193'.                  
                   88 TRAVELERS                 VALUE '87726'.                  
                   88 TRAVELERS-MED-RECL        VALUE '88888'.                  
                   88 TRICARE-CA                VALUE 'CDTC1'.                  
                   88 TUFTS                     VALUE '04298'.                  
                   88 UNICARE-OF-TEXAS          VALUE '47195'.                  
                   88 UNITED-CONCORDIA          VALUE 'CX007'.                  
                   88 UPMC                      VALUE '23281'                   
                                                      '23282'                   
                                                      '23283'.                  
                   88 USAA                      VALUE '74095'.                  
                   88 USHC                      VALUE '23222'.                  
                   88 UTMB-HEALTHCARE           VALUE '76049'.                  
                   88 UTILIMED                  VALUE '36369'.                  
                   88 VYTRA                     VALUE '22264'.                  
                   88 WAUSAU                    VALUE '39026'.                  
                   88 WELLCARE                  VALUE '14164'.                  
                   88 WELLCARE-HMO              VALUE '14163'.                  
                   88 WELLPATH                  VALUE '22334'.                  
                   88 WINTERBROOK               VALUE '73159'.                  
                                                                                
               10  FILLER                         PIC X(109).                   
               10  D0-PAT-REL-TO-INS-X            PIC X(002).                   
               10  FILLER                         PIC X(043).                   
               10  D0-PRIOR-PYMT-AMNT-X.                                        
                   15 FILLER                      PIC X(009).                   
                   15 D0-PP-AMT-LAST-BYTE         PIC X(001).                   
               10  FILLER                         PIC X(001).                   
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      *             INSURED INFORMATION RECORD (D1)                    *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-D1-RECORD REDEFINES PCDS-00-RECORD.                         
               10  D1-REC-SEQ-NUM.                                              
                   15  D1-REC-TYPE                PIC X(002).                   
                       88  D1-REC                          VALUE 'D1'.          
                   15  D1-SEQ-NUM                 PIC 9(002).                   
                   15  D1-SEQ-NUM-X REDEFINES D1-SEQ-NUM                        
                                                  PIC X(002).                   
               10  D1-PAT-CNTL-NUM                PIC X(017).                   
               10  D1-INSURED-ADDRESS.                                          
                  15  D1-INSUR-STREET.                                          
                      20  D1-INSUR-STREET-C1      PIC X(001).                   
                      20  FILLER                  PIC X(029).                   
                  15  D1-INSUR-STREET-ADD.                                      
                      20  D1-INSUR-STREET-ADD-C1  PIC X(001).                   
                      20  FILLER                  PIC X(029).                   
                  15  D1-INSUR-CITY.                                            
                      20  D1-INSUR-CITY-C1        PIC X(001).                   
                      20  FILLER                  PIC X(019).                   
                  15  D1-INSUR-STATE              PIC X(002).                   
                  15  D1-INSUR-ZIP.                                             
                      20  D1-ZIP5.                                              
                          30 D1-INSUR-ZIP3        PIC X(003).                   
                          30 FILLER               PIC X(002).                   
                      20  FILLER                  PIC X(004).                   
                  15  D1-INSUR-ZIP-9 REDEFINES D1-INSUR-ZIP                     
                                                  PIC X(009).                   
               10  D1-INSUR-COUNTRY-CD            PIC X(003).                   
               10  D1-PATIENT-ID                  PIC X(010).                   
               10  D1-INSUR-PHONE                 PIC 9(010).                   
               10  D1-INSUR-PHONE-X REDEFINES                                   
                   D1-INSUR-PHONE                 PIC X(010).                   
               10  D1-PATIENT-ID-QUAL             PIC X(002).                   
               10  D1-PATIENT-ID-2                PIC X(030).                   
               10  FILLER                         PIC X(025).                   
                                                                                
      *----------------------------------------------------------------*        
      *             PAYER ADDRESS INFORMATION RECORD (D2)              *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-D2-RECORD REDEFINES PCDS-00-RECORD.                         
               10  D2-REC-SEQ-NUM.                                              
                   15  D2-REC-TYPE                PIC X(002).                   
                       88  D2-REC                          VALUE 'D2'.          
                   15  D2-SEQ-NUM                 PIC 9(002).                   
                   15  D2-SEQ-NUM-X REDEFINES D2-SEQ-NUM                        
                                                  PIC X(002).                   
               10  D2-PAT-CNTL-NUM                PIC X(017).                   
               10  D2-PAYERS-ADDRESS.                                           
                  15  D2-STREET-ADDR-A.                                         
                      20  D2-STREET-ADDR-A-1      PIC X(001).                   
                      20  FILLER                  PIC X(029).                   
                  15  D2-STREET-ADDR-B.                                         
                      20  D2-STREET-ADDR-B-1      PIC X(001).                   
                      20  FILLER                  PIC X(029).                   
                  15  D2-CITY                     PIC X(020).                   
                  15  D2-STATE                    PIC X(002).                   
                  15  D2-ZIP-CODE.                                              
                      20  D2-ZIP5.                                              
                          25 D2-ZIP-CODE3         PIC X(003).                   
                          25 FILLER               PIC X(002).                   
                      20  FILLER                  PIC X(004).                   
                  15  D2-ZIP-9                                                  
                      REDEFINES D2-ZIP-CODE       PIC X(009).                   
               10  D2-PAYER-COUNTRY-CD            PIC X(003).                   
               10  D2-PAYER-NAME                  PIC X(030).                   
               10  FILLER                         PIC X(047).                   
                                                                                
      *----------------------------------------------------------------*        
      *      REFERRAL AND PRIOR AUTHORIZATION NUMBER RECORD (D3)       *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-D3-RECORD REDEFINES PCDS-00-RECORD.                         
               10  D3-REC-SEQ-NUM.                                              
                   15  D3-REC-TYPE                PIC X(002).                   
                       88  D3-REC                           VALUE 'D3'.         
                   15  D3-SEQ-NUM                 PIC 9(002).                   
                   15  D3-SEQ-NUM-X    REDEFINES                                
                       D3-SEQ-NUM                 PIC X(002).                   
                   15  D3-SUB-SEQ-NUM             PIC 9(002).                   
                   15  D3-SUB-SEQ-NUM-X    REDEFINES                            
                       D3-SUB-SEQ-NUM             PIC X(002).                   
               10  D3-PAT-CNTL-NUM                PIC X(017).                   
               10  D3-REFERRRAL-AUTH-NOS OCCURS 2 TIMES.                                           
                   15  D3-REFERRAL-NO             PIC X(030).                   
                   15  D3-PRIOR-AUTH-NO           PIC X(030).                   
               10  FILLER                         PIC X(049).                   
                                                                                
      *----------------------------------------------------------------*        
      *             TPO ROUTING INFORMATION RECORD (D6)                *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-D6-RECORD REDEFINES PCDS-00-RECORD.                         
               10  D6-REC-SEQ-NUM.                                              
                   15  D6-REC-TYPE                PIC X(002).                   
                       88  D6-REC                     VALUE 'D6'.               
                   15  D6-SEQ-NUM                 PIC 9(002).                   
                   15  D6-SEQ-NUM-X                                             
                       REDEFINES D6-SEQ-NUM       PIC X(002).                   
               10  D6-PAT-CNTL-NO                 PIC X(017).                   
               10  D6-PRIMARY-ORG-TYPE-IND        PIC 9(001).                   
               10  D6X-PRIMARY-ORG-TYPE  REDEFINES                              
                   D6-PRIMARY-ORG-TYPE-IND        PIC X(001).                   
                   88  PRIMARY-THIRD-PARTY-PPO        VALUE '1'.                
                   88  PRIMARY-PPO                    VALUE '2'.                
                   88  PRIMARY-HMO                    VALUE '3'.                
                   88  PRIMARY-TPA                    VALUE '4'.                
                   88  PRIMARY-OTHER-TPO              VALUE '5'.                
                   88  PRIMARY-OTHER                  VALUE '6'.                
                   88  VALID-PRIMARY-ORG-IND          VALUES                    
                       '1' THRU '6'.                                            
               10  D6-PRIMARY-TPO.                                              
                   15  D6-PRIMARY-TPO-ID          PIC X(005).                   
                   15  D6-PRIMARY-TPO-SUB-ID      PIC X(004).                   
               10  D6-SECONDARY-ORG-TYPE-IND      PIC 9(001).                   
               10  D6X-SECONDARY-ORG-TYPE  REDEFINES                            
                   D6-SECONDARY-ORG-TYPE-IND      PIC X(001).                   
                   88  SECONDARY-THIRD-PARTY-PPO      VALUE '1'.                
                   88  SECONDARY-PPO                  VALUE '2'.                
                   88  SECONDARY-HMO                  VALUE '3'.                
                   88  SECONDARY-TPA                  VALUE '4'.                
                   88  SECONDARY-OTHER-TPO            VALUE '5'.                
                   88  SECONDARY-OTHER                VALUE '6'.                
                   88  VALID-SECONDARY-ORG-IND        VALUES                    
                       '1' THRU '6'.                                            
               10  D6-SECONDARY-TPO.                                            
                   15  D6-SECONDARY-TPO-ID        PIC X(005).                   
                   15  D6-SECONDARY-TPO-SUB-ID    PIC X(004).                   
               10  D6-AUTHORIZATION-NUMBER        PIC X(020).                   
               10  FILLER                         PIC X(003).                   
               10  D6-QUALIFICATION-DEGREE        PIC 9(001).                   
                   88  QUALIFICATION-MD               VALUE  1.                 
                   88  QUALIFICATION-MSW              VALUE  2.                 
                   88  QUALIFICATION-PHD              VALUE  3.                 
                   88  QUALIFICATION-RN               VALUE  4.                 
                   88  QUALIFICATION-OTHER            VALUE  5.                 
                   88  VALID-QUALIFICATION            VALUES                    
                       1 THRU 5.                                                
               10  D6X-QUALIFICATION-DEGREE REDEFINES                           
                   D6-QUALIFICATION-DEGREE        PIC X(001).                   
               10  D6-FREE-FORM-REMARKS           PIC X(070).                   
               10  FILLER                         PIC X(047).                   
      **** ACTUALLY THIS LAST FILLER IS RESERVED FOR N.E.I.C. - H.E.            
               10  D6-RESERVED-FIELD.                                           
                   15  D6-TPO-SITE               PIC X(001).                    
                   15  FILLER                    PIC X(009).                    
                                                                                
      *----------------------------------------------------------------*        
      *             NSF PAYER INFORMATION RECORD (DA)                  *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-DA-RECORD REDEFINES PCDS-00-RECORD.                         
               10  DA-REC-SEQ-NUM.                                              
                   15  DA-REC-TYPE                PIC X(002).                   
                       88  DA-REC                     VALUE 'DA'.               
                   15  DA-SEQ-NUM                 PIC 9(002).                   
                   15  DA-SEQ-NUM-X                                             
                       REDEFINES DA-SEQ-NUM       PIC X(002).                   
               10  DA-PAT-CNTL-NO                 PIC X(017).                   
               10  FILLER                         PIC X(002).                   
               10  DA-PPO-HMO-INDICATOR           PIC X(001).                   
               10  DA-PPO-HMO-ID                  PIC X(015).                   
               10  FILLER                         PIC X(003).                   
               10  DA-INSUREDS-EMPL-STATUS-CD     PIC X(001).                   
               10  DA-SUPPL-INSURANCE-IND         PIC X(001).                   
               10  DA-INSURANCE-LOCATION-ID       PIC X(007).                   
               10  DA-MEDICAID-ID-NO              PIC X(025).                   
               10  FILLER                         PIC X(014).                   
               10  DA-ALLOWED-AMT                 PIC 9(005)V99.                
               10  DA-ALLOWED-AMT-X                                             
                   REDEFINES DA-ALLOWED-AMT       PIC X(007).                   
               10  FILLER                         PIC X(014).                   
               10  DA-ZERO-PMT-INDICATOR          PIC X(001).                   
               10  FILLER                         PIC X(006).                   
               10  DA-CHAMPUS-SPONSOR-BRANCH      PIC X(001).                   
               10  DA-CHAMPUS-SPONSOR-GRADE       PIC X(002).                   
               10  DA-CHAMPUS-SPONSOR-STATUS      PIC X(001).                   
               10  DA-INSURANCE-CARD-EFF-DATE.                                  
                   15  DA-INS-CARD-EFF-CC         PIC 9(002).                   
                   15  DA-INS-CARD-EFF-YY         PIC 9(002).                   
                   15  DA-INS-CARD-EFF-MM         PIC 9(002).                   
                   15  DA-INS-CARD-EFF-DD         PIC 9(002).                   
                                                                                
               10  DA-INSURANCE-CARD-EFF-DATE-X                                 
                   REDEFINES DA-INSURANCE-CARD-EFF-DATE.                        
                   15  DA-INS-CARD-EFF-CC-X       PIC X(002).                   
                   15  DA-INS-CARD-EFF-YY-X       PIC X(002).                   
                   15  DA-INS-CARD-EFF-MM-X       PIC X(002).                   
                   15  DA-INS-CARD-EFF-DD-X       PIC X(002).                   
                                                                                
               10  DA-INSURANCE-CARD-TERM-DATE.                                 
                   15  DA-INS-CARD-TERM-CC        PIC 9(002).                   
                   15  DA-INS-CARD-TERM-YY        PIC 9(002).                   
                   15  DA-INS-CARD-TERM-MM        PIC 9(002).                   
                   15  DA-INS-CARD-TERM-DD        PIC 9(002).                   
                                                                                
               10  DA-INSURANCE-CARD-TERM-DATE-X                                
                   REDEFINES DA-INSURANCE-CARD-TERM-DATE.                       
                   15  DA-INS-CARD-TERM-CC-X      PIC X(002).                   
                   15  DA-INS-CARD-TERM-YY-X      PIC X(002).                   
                   15  DA-INS-CARD-TERM-MM-X      PIC X(002).                   
                   15  DA-INS-CARD-TERM-DD-X      PIC X(002).                   
               10  DA-BALANCE-DUE                 PIC 9(005)V99.                
               10  DA-BALANCE-DUE-X                                             
                   REDEFINES DA-BALANCE-DUE       PIC X(007).                   
               10  DA-INSURED-RETIRE-DATE.                                      
                   15  DA-INS-RETIRE-CC         PIC 9(002).                     
                   15  DA-INS-RETIRE-YY         PIC 9(002).                     
                   15  DA-INS-RETIRE-MM        PIC 9(002).                      
                   15  DA-INS-RETIRE-DD        PIC 9(002).                      
                                                                                
               10  DA-INSURED-RETIRE-DATE-X                                     
                   REDEFINES DA-INSURED-RETIRE-DATE.                            
                   15  DA-INS-RETIRE-CC-X         PIC X(002).                   
                   15  DA-INS-RETIRE-YY-X         PIC X(002).                   
                   15  DA-INS-RETIRE-MM-X         PIC X(002).                   
                   15  DA-INS-RETIRE-DD-X         PIC X(002).                   
                                                                                
               10  DA-INSURED-SPOUSE-RETIRE-DATE.                               
                   15  DA-INS-SPSE-RETIRE-CC      PIC 9(002).                   
                   15  DA-INS-SPSE-RETIRE-YY      PIC 9(002).                   
                   15  DA-INS-SPSE-RETIRE-MM      PIC 9(002).                   
                   15  DA-INS-SPSE-RETIRE-DD      PIC 9(002).                   
                                                                                
               10  DA-INSURED-SPOUSE-RETIRE-DT-X REDEFINES                      
                   DA-INSURED-SPOUSE-RETIRE-DATE.                               
                   15  DA-INS-SPSE-RETIRE-CC-X    PIC X(002).                   
                   15  DA-INS-SPSE-RETIRE-YY-X    PIC X(002).                   
                   15  DA-INS-SPSE-RETIRE-MM-X    PIC X(002).                   
                   15  DA-INS-SPSE-RETIRE-DD-X    PIC X(002).                   
               10  DA-BCBS-PLAN-CODE              PIC X(005).                   
               10  DA-INSURED-SUFFIX-GEN          PIC X(010).                   
               10  DA-DISALLOW-OTHER-AMT          PIC 9(005)V99.                
               10  DA-DISALLOW-OTHER-AMT-X  REDEFINES                           
                   DA-DISALLOW-OTHER-AMT          PIC X(007).                   
               10  DA-ADJUDICATION-IND-1          PIC X(001).                   
               10  DA-ADJUDICATION-IND-2          PIC X(001).                   
               10  FILLER                         PIC X(007).                   
                                                                                
      *----------------------------------------------------------------*        
      *      NSF OVERFLOW PAYER/INSURED INFORMATION RECORD (DB)        *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-DB-RECORD REDEFINES PCDS-00-RECORD.                         
               10  DB-REC-SEQ-NUM.                                              
                   15  DB-REC-TYPE                PIC X(002).                   
                       88  DB-REC                     VALUE 'DB'.               
                   15  DB-SEQ-NUM                 PIC 9(002).                   
                   15  DB-SEQ-NUM-X                                             
                       REDEFINES DB-SEQ-NUM       PIC X(002).                   
               10  DB-PAT-CNTL-NO                 PIC X(020).                   
               10  DB-NSF-PAYER-NAME              PIC X(035).                   
               10  DB-NSF-GROUP-NAME              PIC X(035).                   
               10  DB-NSF-INSURED-ID              PIC X(025).                   
               10  DB-NSF-INSURED-LNAME           PIC X(035).                   
               10  DB-NSF-INSURED-FNAME           PIC X(012).                   
               10  FILLER                         PIC X(026).                   
                                                                                
      *----------------------------------------------------------------*        
      *    NSF OVERFLOW INSURED ADDRESS INFORMATION RECORD (DC)        *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-DC-RECORD REDEFINES PCDS-00-RECORD.                         
               10  DC-REC-SEQ-NUM.                                              
                   15  DC-REC-TYPE                PIC X(002).                   
                       88  DC-REC                     VALUE 'DC'.               
                   15  DC-SEQ-NUM                 PIC 9(002).                   
                   15  DC-SEQ-NUM-X                                             
                       REDEFINES DC-SEQ-NUM       PIC X(002).                   
               10  DC-PAT-CNTL-NO                 PIC X(017).                   
               10  DC-INSURED-EMPLOYERS-ADDRESS.                                
                   15  DC-INS-EMPLOYER-ADDR-1     PIC X(030).                   
                   15  DC-INS-EMPLOYER-ADDR-2     PIC X(030).                   
                   15  DC-INS-EMPLOYER-CITY       PIC X(020).                   
                   15  DC-INS-EMPLOYER-STATE      PIC X(002).                   
                   15  DC-ZIP-CODE.                                             
                       20  DC-ZIP5.                                             
                           25 DC-ZIP-CODE3         PIC X(003).                  
                           25 FILLER               PIC X(002).                  
                       20  FILLER                  PIC X(004).                  
                   15  DC-ZIP-9                                                 
                       REDEFINES DC-ZIP-CODE       PIC X(009).                  
               10  FILLER                          PIC X(003).                  
               10  DC-INS-EMPLOYER-ID              PIC X(012).                  
               10  DC-INS-EMPLOYER-NAME            PIC X(033).                  
               10  FILLER                          PIC X(032).                  
                                                                                
      *----------------------------------------------------------------*        
      *         NSF PAYER PAYMENT INFORMATION RECORD (DD)              *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-DD-RECORD REDEFINES PCDS-00-RECORD.                         
               10  DD-REC-SEQ-NUM.                                              
                   15  DD-REC-TYPE                PIC X(002).                   
                       88  DD-REC                     VALUE 'DD'.               
                   15  DD-SEQ-NUM                 PIC 9(002).                   
                   15  DD-SEQ-NUM-X                                             
                       REDEFINES DD-SEQ-NUM       PIC X(002).                   
               10  DD-PAT-CNTL-NO                 PIC X(020).                   
               10  FILLER                         PIC X(025).                   
               10  DD-4081-CLAIM-ASSIGN-IND       PIC X(001).                   
               10  DD-COB-ROUTING-IND             PIC X(001).                   
               10  DD-EOMB-DATE-TBL OCCURS 4 TIMES                              
                   INDEXED BY DD-INX.                                           
                   15  DD-EOMB-DATE.                                            
                       20  DD-EOMB-CC             PIC 9(002).                   
                       20  DD-EOMB-YY             PIC 9(002).                   
                       20  DD-EOMB-MM             PIC 9(002).                   
                       20  DD-EOMB-DD             PIC 9(002).                   
                                                                                
                   15  DD-EOMB-DATE-X                                           
                       REDEFINES DD-EOMB-DATE.                                  
                       20  DD-EOMB-CC-X           PIC X(002).                   
                       20  DD-EOMB-YY-X           PIC X(002).                   
                       20  DD-EOMB-MM-X           PIC X(002).                   
                       20  DD-EOMB-DD-X           PIC X(002).                   
               10  DD-CLAIM-RECEIPT-DATE.                                       
                   15  DD-CLAIM-RCPT-CC           PIC 9(002).                   
                   15  DD-CLAIM-RCPT-YY           PIC 9(002).                   
                   15  DD-CLAIM-RCPT-MM           PIC 9(002).                   
                   15  DD-CLAIM-RCPT-DD           PIC 9(002).                   
                                                                                
               10  DD-CLAIM-RECEIPT-DATE-X                                      
                   REDEFINES DD-CLAIM-RECEIPT-DATE.                             
                   15  DD-CLAIM-RCPT-CC-X         PIC X(002).                   
                   15  DD-CLAIM-RCPT-YY-X         PIC X(002).                   
                   15  DD-CLAIM-RCPT-MM-X         PIC X(002).                   
                   15  DD-CLAIM-RCPT-DD-X         PIC X(002).                   
               10  DD-BENEFICIARY-PAID-AMT        PIC 9(007)V99.                
               10  DD-BENEFICIARY-PAID-AMT-X                                    
                   REDEFINES DD-BENEFICIARY-PAID-AMT                            
                                                  PIC X(009).                   
               10  DD-BENEFIT-CHK-EFT-TRACE-NO    PIC X(015).                   
               10  DD-BENEFIT-CHECK-DATE.                                       
                   15  DD-BENEFIT-CHECK-CC        PIC 9(002).                   
                   15  DD-BENEFIT-CHECK-YY        PIC 9(002).                   
                   15  DD-BENEFIT-CHECK-MM        PIC 9(002).                   
                   15  DD-BENEFIT-CHECK-DD        PIC 9(002).                   
                                                                                
               10  DD-BENEFIT-CHECK-DATE-X                                      
                   REDEFINES DD-BENEFIT-CHECK-DATE.                             
                   15  DD-BENEFIT-CHECK-CC-X      PIC X(002).                   
                   15  DD-BENEFIT-CHECK-YY-X      PIC X(002).                   
                   15  DD-BENEFIT-CHECK-MM-X      PIC X(002).                   
                   15  DD-BENEFIT-CHECK-DD-X      PIC X(002).                   
               10  DD-PAID-TO-PROVIDER-AMT        PIC 9(007)V99.                
               10  DD-PAID-TO-PROVIDER-AMT-X                                    
                   REDEFINES DD-PAID-TO-PROVIDER-AMT  PIC X(009).               
               10  DD-PROVIDER-CHK-EFT-TRACE-NO   PIC X(015).                   
               10  DD-PROVIDER-CHECK-DATE.                                      
                   15  DD-PROVIDER-CHECK-CC       PIC 9(002).                   
                   15  DD-PROVIDER-CHECK-YY       PIC 9(002).                   
                   15  DD-PROVIDER-CHECK-MM       PIC 9(002).                   
                   15  DD-PROVIDER-CHECK-DD       PIC 9(002).                   
                                                                                
               10  DD-PROVIDER-CHECK-DATE-X                                     
                   REDEFINES DD-PROVIDER-CHECK-DATE.                            
                   15  DD-PROVIDER-CHECK-CC-X     PIC X(002).                   
                   15  DD-PROVIDER-CHECK-YY-X     PIC X(002).                   
                   15  DD-PROVIDER-CHECK-MM-X     PIC X(002).                   
                   15  DD-PROVIDER-CHECK-DD-X     PIC X(002).                   
               10  DD-INTEREST-AMT                PIC 9(007)V99.                
               10  DD-INTEREST-AMT-X                                            
                   REDEFINES DD-INTEREST-AMT      PIC X(009).                   
               10  DD-APPROVED-AMT                PIC 9(007)V99.                
               10  DD-APPROVED-AMT-X                                            
                   REDEFINES DD-APPROVED-AMT      PIC X(009).                   
               10  DD-CONTRACT-AGREEMENT-IND      PIC X(001).                   
               10  DD-ORIGINAL-APPROVED-AMT       PIC 9(005)V99.                
               10  DD-ORIGINAL-APPROVED-AMT-X                                   
                   REDEFINES DD-ORIGINAL-APPROVED-AMT                           
                                                  PIC X(007).                   
               10  FILLER                         PIC X(011).                   
                                                                                
      *----------------------------------------------------------------*        
      *     HIPAA INSURED SECONDARY REFERENCE NUMBERS RECORD (DN)      *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-DN-RECORD REDEFINES PCDS-00-RECORD.                         
               10  DN-REC-SEQ-NUM.                                              
                   15  DN-REC-TYPE                PIC X(002).                   
                       88  DN-REC                     VALUE 'DN'.               
                   15  DN-SEQ-NUM                 PIC 9(002).                   
                   15  DN-SEQ-NUM-X                                             
                       REDEFINES DN-SEQ-NUM       PIC X(002).                   
               10  DN-PAT-CNTL-NO                 PIC X(020).                   
               10  DN-ADDITIONAL-INSURED-NAME     PIC X(060).                   
               10  FILLER                         PIC X(003).                   
               10  DN-INSURED-SEC-ID-QUALIFIERS                                 
                   OCCURS 4 TIMES                                               
                   INDEXED BY DN-INX.                                           
                   15  DN-INSURED-SEC-ID-QUAL     PIC X(002).                   
                   15  DN-INSURED-SEC-ID          PIC X(015).                   
               10  DN-HL-ID-NO                    PIC X(006).                   
               10  DN-HL-PARENT-ID-NO             PIC X(006).                   
               10  FILLER                         PIC X(025).                   
                                                                                
      *----------------------------------------------------------------*        
      *     HIPAA PAYER SECONDARY REFERENCE NUMBERS RECORD (DP)        *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-DP-RECORD REDEFINES PCDS-00-RECORD.                         
               10  DP-REC-SEQ-NUM.                                              
                   15  DP-REC-TYPE                PIC X(002).                   
                       88  DP-REC                     VALUE 'DP'.               
                   15  DP-SEQ-NUM                 PIC 9(002).                   
                   15  DP-SEQ-NUM-X                                             
                       REDEFINES DP-SEQ-NUM       PIC X(002).                   
               10  DP-PAT-CNTL-NO                 PIC X(020).                   
               10  DP-ADDITIONAL-PAYER-NAME       PIC X(060).                   
               10  FILLER                         PIC X(003).                   
               10  DP-PAYER-SEC-ID-QUALIFIERS                                   
                   OCCURS 3 TIMES                                               
                   INDEXED BY DP-INX.                                           
                   15  DP-PAYER-SEC-ID-QUAL       PIC X(002).                   
                   15  DP-PAYER-SEC-ID            PIC X(015).                   
               10  FILLER                         PIC X(054).                   
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      *         HIPAA PAYER CONTACT INFORMATION RECORD (DQ)            *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-DQ-RECORD REDEFINES PCDS-00-RECORD.                         
               10  DQ-REC-SEQ-NUM.                                              
                   15  DQ-REC-TYPE                PIC X(002).                   
                       88  DQ-REC                     VALUE 'DQ'.               
                   15  DQ-SEQ-NUM                 PIC 9(002).                   
                   15  DQ-SEQ-NUM-X                                             
                       REDEFINES DQ-SEQ-NUM       PIC X(002).                   
               10  DQ-PAT-CNTL-NO                 PIC X(020).                   
               10  DQ-PAYER-PHONE                 PIC 9(010).                   
               10  DQ-PAYER-PHONE-X                                             
                   REDEFINES DQ-PAYER-PHONE       PIC X(010).                   
               10  DQ-PAYER-FAX                   PIC X(010).                   
               10  DQ-PAYER-EMAIL                 PIC X(050).                   
               10  DQ-PAYER-EDI-NO                PIC X(015).                   
               10  DQ-PAYER-PHONE-EXT             PIC X(006).                   
               10  DQ-OTHER-PAYER-CONTACT-NAME    PIC X(060).                   
               10  FILLER                         PIC X(017).                   
                                                                                
      *---------------------------------------------------------------*         
      *                 HIPAA OTHER PAYER PATIENT AND                 *         
      *               PRIOR AUTHORIZATION NUMBERS RECORD (DR)         *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-DR-RECORD REDEFINES PCDS-00-RECORD.                         
               10  DR-REC-SEQ-NUM.                                              
                   15  DR-REC-TYPE                PIC X(002).                   
                       88  DR-REC                     VALUE 'DR'.               
                   15  DR-SEQ-NUM                 PIC 9(002).                   
                   15  DR-SEQ-NUM-X  REDEFINES                                  
                       DR-SEQ-NUM                 PIC X(002).                   
                   15  DR-SUB-SEQ-NUM             PIC 9(002).                   
               10  DR-PAT-CNTL-NO                 PIC X(020).                   
               10  FILLER                         PIC X(010).                   
               10  DR-PATIENT-SEC-ID-QUALIFIERS                                 
                   OCCURS 3 TIMES                                               
                   INDEXED BY DR-INX.                                           
                   15  DR-PATIENT-SEC-ID-QUAL     PIC X(002).                   
                   15  DR-PATIENT-SEC-ID          PIC X(030).                   
               10  FILLER                         PIC X(060).                   
                                                                                
      *----------------------------------------------------------------*        
      * HIPAA OTHER PAYER PROVIDER REFERENCE NUMBERS RECORD (DS)       *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-DS-RECORD REDEFINES PCDS-00-RECORD.                         
               10  DS-REC-SEQ-NUM.                                              
                   15  DS-REC-TYPE                PIC X(002).                   
                       88  DS-REC                     VALUE 'DS'.               
                   15  DS-SEQ-NUM                 PIC 9(002).                   
                   15  DS-SEQ-NUM-X                                             
                       REDEFINES DS-SEQ-NUM       PIC X(002).                   
                   15  DS-SUB-SEQ-NUM             PIC 9(002).                   
                   15  DS-SUB-SEQ-NUM-X    REDEFINES                            
                       DS-SUB-SEQ-NUM             PIC X(002).                   
               10  DS-PAT-CNTL-NO                 PIC X(020).                   
               10  DS-NAME-QUALIFIER              PIC X(002).                   
               10  DS-ORG-OR-LNAME                PIC X(035).                   
               10  DS-PAYER-PROVIDER-TBL                                        
                   OCCURS 3 TIMES                                               
                   INDEXED BY DS-INX.                                           
                   15  DS-PAYER-PROV-SEC-ID-QUAL  PIC X(002).                   
                   15  DS-PAYER-PROV-SEC-ID       PIC X(030).                   
               10  FILLER                         PIC X(033).                   
                                                                                
      *----------------------------------------------------------------*        
      *        HIPAA CLAIM ADJUSTMENT REASON CODES RECORD (DT)         *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-DT-RECORD REDEFINES PCDS-00-RECORD.                         
               10  DT-REC-SEQ-NUM.                                              
                   15  DT-REC-TYPE                PIC X(002).                   
                       88  DT-REC                     VALUE 'DT'.               
                   15  DT-SEQ-NUM                 PIC 9(002).                   
                   15  DT-SEQ-NUM-X                                             
                       REDEFINES DT-SEQ-NUM       PIC X(002).                   
                   15  DT-SUB-SEQ-NUM             PIC 9(002).                   
                   15  DT-SUB-SEQ-NUM-X    REDEFINES                            
                       DT-SUB-SEQ-NUM             PIC X(002).                   
               10  DT-PAT-CNTL-NO                 PIC X(020).                   
               10  DT-CAS-GROUP-CODE              PIC X(002).                   
               10  DT-CLAIM-ADJUSTMENT-TABLE                                    
                   OCCURS 6 TIMES                                               
                   INDEXED BY DT-INX.                                           
                   15  DT-CAS-CODE                PIC X(005).                   
                   15  DT-CAS-AMOUNT              PIC 9(006)V99.                
                   15  DT-CAS-AMOUNT-X                                          
                       REDEFINES DT-CAS-AMOUNT    PIC X(008).                   
                   15  DT-CAS-QUANTITY            PIC X(014).                   
               10  FILLER                         PIC X(002).                   
                                                                                
      *----------------------------------------------------------------*        
      *             HIPAA COB INFORMATION RECORD (DU)                  *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-DU-RECORD REDEFINES PCDS-00-RECORD.                         
               10  DU-REC-SEQ-NUM.                                              
                   15  DU-REC-TYPE                PIC X(002).                   
                       88  DU-REC                     VALUE 'DU'.               
                   15  DU-SEQ-NUM                 PIC 9(002).                   
                   15  DU-SEQ-NUM-X                                             
                       REDEFINES DU-SEQ-NUM       PIC X(002).                   
               10  DU-PAT-CNTL-NO                 PIC X(020).                   
               10  DU-COB-PAT-RSPBILITY-AMT       PIC 9(007)V99.                
               10  DU-COB-PAT-RSPBILITY-AMT-X                                   
                   REDEFINES DU-COB-PAT-RSPBILITY-AMT                           
                                                  PIC X(009).                   
               10  DU-COB-PAYER-COVERED-AMT       PIC 9(007)V99.                
               10  DU-COB-PAYER-COVERED-AMT-X                                   
                   REDEFINES DU-COB-PAYER-COVERED-AMT                           
                                                  PIC X(009).                   
               10  DU-COB-PAYER-DISCOUNT-AMT      PIC 9(007)V99.                
               10  DU-COB-PAYER-DISCOUNT-AMT-X                                  
                   REDEFINES DU-COB-PAYER-DISCOUNT-AMT                          
                                                  PIC X(009).                   
               10  DU-COB-PAYER-PER-DAY-LIMIT     PIC 9(007)V99.                
               10  DU-COB-PAYER-PER-DAY-LIMIT-X                                 
                   REDEFINES DU-COB-PAYER-PER-DAY-LIMIT                         
                                                  PIC X(009).                   
               10  DU-COB-TAX-AMT                 PIC 9(007)V99.                
               10  DU-COB-TAX-AMT-X  REDEFINES                                  
                   DU-COB-TAX-AMT                 PIC X(009).                   
               10  FILLER                         PIC X(036).                   
               10  DU-COB-PRE-TAX-CLAIM-TOT       PIC 9(007)V99.                
               10  DU-COB-PRE-TAX-CLAIM-TOT-X                                   
                   REDEFINES DU-COB-PRE-TAX-CLAIM-TOT                           
                                                  PIC X(009).                   
               10  FILLER                         PIC X(001).                   
               10  DU-MOA-REIMB-RATE              PIC 9(007)V99.                
               10  DU-MOA-REIMB-RATE-X                                          
                   REDEFINES DU-MOA-REIMB-RATE    PIC X(009).                   
               10  DU-MOA-HCPCS-PAYABLE-AMT       PIC 9(007)V99.                
               10  DU-MOA-HCPCS-PAYABLE-AMT-X                                   
                   REDEFINES DU-MOA-HCPCS-PAYABLE-AMT                           
                                                  PIC X(009).                   
               10  DU-MOA-CLAIM-PMT-REMARK-CODES                                
                   OCCURS 5 TIMES                                               
                   INDEXED BY DU-INX.                                           
                   15  DU-MOA-CLM-PMT-REMARK-CD   PIC X(005).                   
               10  DU-MOA-ESRD-AMT-PAID           PIC 9(007)V99.                
               10  DU-MOA-ESRD-AMT-PAID-X                                       
                   REDEFINES DU-MOA-ESRD-AMT-PAID PIC X(009).                   
               10  DU-MOA-PROFESSIONAL-COMPONENT  PIC 9(007)V99.                
               10  DU-MOA-PROF-COMPONENT-X                                      
                   REDEFINES DU-MOA-PROFESSIONAL-COMPONENT                      
                                                  PIC X(009).                   
               10  DU-COB-CLAIM-DETAIL-LINE-CNT   PIC X(002).                   
               10  DU-COB-CLAIM-ADJUSTMENT-IND    PIC X(001).                   
               10  DU-AMT-PAID-TO-PATIENT         PIC 9(007)V99.                
               10  DU-AMT-PAID-TO-PATIENT-X                                     
                   REDEFINES DU-AMT-PAID-TO-PATIENT                             
                                                  PIC X(009).                   
               10  FILLER                         PIC X(004).                   
                                                                                
      *----------------------------------------------------------------*        
      *          HIPAA CREDIT CARD INFORMATION RECORD (DV)             *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-DV-RECORD REDEFINES PCDS-00-RECORD.                         
               10  DV-REC-SEQ-NUM.                                              
                   15  DV-REC-TYPE                PIC X(002).                   
                       88  DV-REC                     VALUE 'DV'.               
                   15  DV-SEQ-NUM                 PIC 9(002).                   
                   15  DV-SEQ-NUM-X                                             
                       REDEFINES DV-SEQ-NUM       PIC X(002).                   
               10  DV-PAT-CNTL-NO                 PIC X(020).                   
               10  DV-CR-DR-CARD-HOLD-INFO.                                     
                   15  DV-CARD-HOLDER-LNAME-OR-ORG                              
                                                  PIC X(035).                   
                   15  DV-CARD-HOLDER-FNAME       PIC X(025).                   
                   15  DV-CARD-HOLDER-MI          PIC X(001).                   
                   15  DV-CARD-HOLDER-SUFFIX      PIC X(010).                   
                   15  DV-CR-DB-CARD-NUM          PIC X(020).                   
                   15  DV-CR-DB-CARD-AUTH-NO-QUAL-1                             
                                                  PIC X(002).                   
                   15  DV-CR-DB-CARD-AUTH-NO-1    PIC X(030).                   
                   15  DV-CREDIT-CARD-MAX-AMT     PIC 9(006)V99.                
                   15  DV-CREDIT-CARD-MAX-AMT-X                                 
                       REDEFINES DV-CREDIT-CARD-MAX-AMT                         
                                                  PIC X(008).                   
                   15  DV-CR-DB-CARD-AUTH-NO-QUAL-2                             
                                                  PIC X(002).                   
                   15  DV-CR-DB-CARD-AUTH-NO-2    PIC X(030).                   
               10  FILLER                         PIC X(005).                   
                                                                                
      *----------------------------------------------------------------*        
      *          CLAIM INFORMATION RECORD (E0)                         *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-E0-RECORD REDEFINES PCDS-00-RECORD.                         
               10  E0-REC-TYPE                    PIC X(002).                   
                   88  E0-REC                              VALUE 'E0'.          
               10  FILLER                         PIC X(002).                   
      *********10  PCDS-E0-TYPE     PIC X(002).**                               
      *            88  PCDS-E0-NEIC    VALUE '  '.                              
      *            88  PCDS-E0-NSF-EA  VALUE 'EA'.                              
      *            88  PCDS-E0-NSF-EB  VALUE 'EB'.                              
      *************88  PCDS-E0-NSF-EC  VALUE 'EC'.                              
               10  E0-PAT-CNTL-NUM                PIC X(017).                   
               10  E0-PROV-ACCEPTS-ASSIGNMENT-IND PIC X(001).                   
                   88  E0-PROV-ACT-ASSIGNED                VALUE 'A'.           
                   88  E0-PROV-ACT-NOT-ASSIGNED            VALUE 'N'.           
                   88  E0-HOSPITAL-BASED-PHY               VALUE 'H'.           
                   88  E0-CLINICAL-LAB-ONLY                VALUE 'B'.           
                   88  E0-PAT-REFUSE-ASGN-BEN              VALUE 'P'.           
                   88  E0-VALID-PROV-ACCT                                       
                           VALUE 'A' 'N' 'H'.                                   
                   88  E0-VALID-EMCDS-PROV-ACCT                                 
                           VALUE 'A' 'N' 'H' 'B'  'P'.                          
               10  E0-DIAGNOSIS.                                                
                   15  E0-PDC.                                                  
                       20  E0-PRIM-DIAG-1         PIC X(001).                   
                       20  FILLER                 PIC X(007).                   
                   15  E0-SEC-DIAG-1              PIC X(008).                   
                   15  E0-TERCIARY-DIAG-1         PIC X(008).                   
                   15  E0-OTHER-DIAG-1            PIC X(008).                   
               10  FILLER REDEFINES  E0-DIAGNOSIS.                              
                   15 E0-DIAG              OCCURS 4 TIMES                       
                                           INDEXED BY RTE0-DIAG-INDEX.          
                      20 E0-DIAG-CODE             PIC X(008).                   
               10  E0-SAME-SIM-SYMPTOM-IND        PIC X(001).                   
                   88  E0-SAME-SYM-IND-NOT-ENTERED         VALUE ' '.           
                   88  E0-SAME-SYM-YES                     VALUE 'Y'.           
                   88  E0-SAME-SYM-NO                      VALUE 'N'.           
                   88  E0-VALID-SAME-SYM-IND          VALUES 'Y' 'N'.           
               10  E0-DATE-FIRST-CONSULT.                                       
                   15  E0-DATE-FIRST-CONSULT-CC    PIC 9(002).                  
                   15  E0-DATE-FIRST-CONSULT-YY    PIC 9(002).                  
                   15  E0-DATE-FIRST-CONSULT-MM    PIC 9(002).                  
                   15  E0-DATE-FIRST-CONSULT-DD    PIC 9(002).                  
               10  E0-DATE-FIRST-CONSULT-X REDEFINES                            
                   E0-DATE-FIRST-CONSULT           PIC X(008).                  
               10  E0-FIRST-SYMPTOM-IND            PIC 9(001).                  
                   88  E0-FIRST-SYM-IND-NOT-ENTERED        VALUE 0.             
                   88  E0-FIRST-SYM-ILLNESS                VALUE 1.             
                   88  E0-LAST-MENSTRUAL-PERIOD            VALUE 2.             
                   88  E0-VALID-FIRST-SYMPTOM-IND        VALUE 1 2.             
               10  E0-FIRST-SYMPTOM-IND-X REDEFINES                             
                   E0-FIRST-SYMPTOM-IND           PIC X(001).                   
               10  E0-DATE-FIRST-SYM.                                           
                   15  E0-DATE-FIRST-SYM-CC       PIC 9(002).                   
                   15  E0-DATE-FIRST-SYM-YY       PIC 9(002).                   
                   15  E0-DATE-FIRST-SYM-MM       PIC 9(002).                   
                   15  E0-DATE-FIRST-SYM-DD       PIC 9(002).                   
               10  E0-DATE-FIRST-SYM-X REDEFINES                                
                   E0-DATE-FIRST-SYM              PIC X(008).                   
               10  E0-AUTO-ACC-IND                PIC X(001).                   
                   88  E0-AUTO-MOBILE-ACCIDENT             VALUE 'A'.           
                   88  E0-NON-AUTO-MOBILE-ACCIDENT         VALUE 'N'.           
                   88  E0-ANOTHER-RESP-PARTY               VALUE 'P'.           
                   88  E0-NOT-APPLIC-AUTO-ACC-IND          VALUE ' '.           
                   88  E0-BOTH-A-OR-N                  VALUE 'A' 'N'.           
                   88  E0-VALID-AUTO-ACC-IND   VALUE 'A' 'N' 'P' ' '.           
               10  E0-AUTO-ACC-DATE.                                            
                   15  E0-AUTO-ACC-DATE-CC        PIC 9(002).                   
                   15  E0-AUTO-ACC-DATE-YY        PIC 9(002).                   
                   15  E0-AUTO-ACC-DATE-MM        PIC 9(002).                   
                   15  E0-AUTO-ACC-DATE-DD        PIC 9(002).                   
               10  E0-AUTO-ACC-DATE-X REDEFINES                                 
                   E0-AUTO-ACC-DATE               PIC X(008).                   
               10  E0-HOUR-ACC.                                                 
                   15  E0-HOUR-ACC-HH             PIC X(002).                   
                   15  E0-HOUR-ACC-MM             PIC X(002).                   
               10  E0-ACC-STATE-CODE              PIC X(002).                   
               10  E0-EMP-REL-IND                 PIC X(001).                   
                   88  E0-EMP-RELATED-ACCIDENT             VALUE 'Y'.           
                   88  E0-NOT-EMP-RELATED-ACCIDENT         VALUE 'N'.           
                   88  E0-NOT-EMP-REL-IND-UNKNOWN          VALUE 'U'.           
               10  E0-EMER-URG-IND                PIC X(001).                   
                   88  E0-EMER-URG-YES                     VALUE 'Y'.           
                   88  E0-EMER-URG-IND-NOT-ENTERED         VALUE ' '.           
                   88  E0-VALID-EMER-URG-IND               VALUE 'Y'.           
               10  E0-CONFINE-FROM-DATE.                                        
                   15  E0-CONFINE-FROM-DATE-CC    PIC 9(002).                   
                   15  E0-CONFINE-FROM-DATE-YY    PIC 9(002).                   
                   15  E0-CONFINE-FROM-DATE-MM    PIC 9(002).                   
                   15  E0-CONFINE-FROM-DATE-DD    PIC 9(002).                   
               10  E0-CONFINE-FROM-DATE-X REDEFINES                             
                   E0-CONFINE-FROM-DATE PIC X(008).                             
               10  E0-CONFINE-THRU-DATE.                                        
                   15  E0-CONFINE-THRU-DATE-CC    PIC 9(002).                   
                   15  E0-CONFINE-THRU-DATE-YY    PIC 9(002).                   
                   15  E0-CONFINE-THRU-DATE-MM    PIC 9(002).                   
                   15  E0-CONFINE-THRU-DATE-DD    PIC 9(002).                   
               10  E0-CONFINE-THRU-DATE-X REDEFINES                             
                   E0-CONFINE-THRU-DATE           PIC X(008).                   
               10  E0-DISABLED-FROM-DATE.                                       
                   15  E0-DISABLED-FROM-DATE-CC   PIC 9(002).                   
                   15  E0-DISABLED-FROM-DATE-YY   PIC 9(002).                   
                   15  E0-DISABLED-FROM-DATE-MM   PIC 9(002).                   
                   15  E0-DISABLED-FROM-DATE-DD   PIC 9(002).                   
               10  E0-DISABLED-FROM-DATE-X REDEFINES                            
                   E0-DISABLED-FROM-DATE          PIC X(008).                   
               10  E0-DISABLED-THRU-DATE.                                       
                   15  E0-DISABLED-THRU-DATE-CC   PIC 9(002).                   
                   15  E0-DISABLED-THRU-DATE-YY   PIC 9(002).                   
                   15  E0-DISABLED-THRU-DATE-MM   PIC 9(002).                   
                   15  E0-DISABLED-THRU-DATE-DD   PIC 9(002).                   
               10  E0-DISABLED-THRU-DATE-X REDEFINES                            
                   E0-DISABLED-THRU-DATE          PIC X(008).                   
               10  E0-CLAIM-DATE.                                               
                   15  E0-CLAIM-CC                PIC 9(002).                   
                   15  E0-CLAIM-YY                PIC 9(002).                   
                   15  E0-CLAIM-MM                PIC 9(002).                   
                   15  E0-CLAIM-DD                PIC 9(002).                   
               10  E0-CLAIM-DATE-X REDEFINES                                    
                   E0-CLAIM-DATE                  PIC X(008).                   
               10  E0-TYPE-OF-CLAIM               PIC X(003).                   
                   88  E0-VALID-CLAIM-TYPE                                      
                           VALUE '100' '101' '102' 'D01' 'D02'.                 
               10  E0-TYPE-CLAIM         REDEFINES                              
                   E0-TYPE-OF-CLAIM.                                            
                   15  E0-TYPE-CLAIM-1-2          PIC X(002).                   
                   15  FILLER                     PIC X(001).                   
               10  E0-PRIOR-AUTH-NUM              PIC X(015).                   
               10  E0-DIAG-CD-QUAL                PIC X(003).                   
               10  E0-TOT-CLAIM-AMT               PIC 9(008)V99.                
               10  E0-TOT-CLAIM-AMT-X    REDEFINES                              
                   E0-TOT-CLAIM-AMT               PIC X(010).                   
               10  E0-PAT-PAID-AMT                PIC 9(008)V99.                
               10  E0-PAT-PAID-AMT-X     REDEFINES                              
                   E0-PAT-PAID-AMT                PIC X(010).                   
               10  E0-PRE-DETER-IND               PIC X(001).                   
               10  E0-ATTACH-XMIT-TYPE-CD         PIC X(002).                   
                   88  E0-ATT-TYPE-SPACES VALUE '  '.                           
                   88  E0-ATT-TYPE-AA     VALUE 'AA'.                           
                   88  E0-ATT-TYPE-BM     VALUE 'BM'.                           
                   88  E0-ATT-TYPE-EL     VALUE 'EL'.                           
                   88  E0-ATT-TYPE-EM     VALUE 'EM'.                           
                   88  E0-ATT-TYPE-FX     VALUE 'FX'.                           
                   88  E0-VALID-ATT-TYPE  VALUE                                 
                                  '  ' 'AA' 'BM' 'EL' 'EM' 'FX'.                
               10  E0-CLAIM-REF-NUM               PIC X(015).                   
               10  FILLER                         PIC X(002).                   
               10  E0-ATTACH-TYPE-CODE-1          PIC X(002).                   
                                                                                
      *----------------------------------------------------------------*        
      *  THIRD PARTY ORGANIZATION/REPRICING INFO RECORD (E1)           *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-E1-RECORD REDEFINES PCDS-00-RECORD.                         
               10  E1-REC-SEQ-NUM.                                              
                   15  E1-REC-TYPE                PIC X(002).                   
                       88  E1-REC              VALUE 'E1'.                      
                   15  E1-SEQ-NUM                 PIC 9(002).                   
                   15  E1-SEQ-NUM-X REDEFINES E1-SEQ-NUM                        
                                                  PIC X(002).                   
               10  E1-PATIENT-CONTROL-NO          PIC X(017).                   
               10  E1-TPO-ID-NUMBER.                                            
                   15  E1-TPO-ID                  PIC X(005).                   
                   15  E1-TPO-SUB-ID              PIC X(004).                   
               10  E1-TPO-REFERENCE-NUMBER        PIC X(015).                   
               10  E1-REJECTION-MESSAGE-IND       PIC X(001).                   
                   88  E1-CANT-ID-PROV-AS-TPO-PARTIC  VALUE '1'.                
                   88  E1-CANT-ID-PAYR-AS-TPO-PARTIC  VALUE '2'.                
                   88  E1-CANT-ID-INSR-AS-TPO-PARTIC  VALUE '3'.                
                   88  E1-PAYOR-NAME-IS-MISSING       VALUE '4'.                
                   88  E1-CERTIFICATION-INFO-MISSING  VALUE '5'.                
                   88  E1-NOT-ENOUGH-CLAIM-INFO       VALUE '6'.                
                   88  E1-VALID-REJECTION-INDICATOR VALUE '1' THRU '6'.         
               10  FILLER                         PIC X(020).                   
               10  E1-PRICING-METHODOLOGY         PIC X(002).                   
                   88 VALID-PRICING-METHODOLOGY VALUE '00' THRU '05'            
                                                      '07' THRU '14'.           
               10  E1-ALLOWED-AMOUNT              PIC 9(006)V99.                
               10  E1X-ALLOWED-AMOUNT REDEFINES E1-ALLOWED-AMOUNT               
                                       PIC X(008).                              
               10  E1-SAVINGS-AMOUNT              PIC 9(006)V99.                
               10  E1X-SAVINGS-AMOUNT REDEFINES E1-SAVINGS-AMOUNT               
                                                  PIC X(008).                   
               10  FILLER                         PIC X(001).                   
               10  FILLER                         PIC X(002).                   
               10  FILLER                         PIC X(005).                   
               10  FILLER                         PIC X(002).                   
               10  FILLER                         PIC X(004).                   
               10  FILLER                         PIC X(050).                   
               10  E1-POLICY-COMPLIANCE-CODE      PIC X(002).                   
               10  E1-EXCEPTION-CODE              PIC X(002).                   
               10  E1-PRIC-PER-DIEM-OR-FLAT       PIC 9(006)V99.                
               10  E1-PRIC-PER-DIEM-OR-FLAT-X  REDEFINES                        
                   E1-PRIC-PER-DIEM-OR-FLAT       PIC X(008).                   
               10  E1-REP-APPROV-AMB-GRP-CD       PIC X(003).                   
               10  E1-REP-APPROV-AMB-GRP-AMT      PIC 9(006)V99.                
               10  E1-REP-APPROV-AMB-GRP-AMT-X  REDEFINES                       
                   E1-REP-APPROV-AMB-GRP-AMT      PIC X(008).                   
               10  E1-ADJUSTED-REPRICED-CLAIM-NO  PIC X(015).                   
               10  FILLER                         PIC X(006).                   
                                                                                
      *----------------------------------------------------------------*        
      *          ORTHODONTIC INFORMATION RECORD (E2)                   *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-E2-RECORD REDEFINES PCDS-00-RECORD.                         
               10  E2-REC-TYPE                    PIC X(002).                   
                   88  E2-REC          VALUE 'E2'.                              
               10  FILLER                         PIC X(002).                   
               10  E2-PATIENT-CNTL-NO             PIC X(017).                   
               10  FILLER                         PIC X(001).                   
               10  FILLER                         PIC X(008).                   
               10  FILLER                         PIC X(001).                   
               10  E2-ORTHODONTICS-INDICATOR      PIC X(001).                   
               10  E2-ORTHODONTICS-TOTAL-MONTHS   PIC 9(002).                   
               10  E2-ORTHODONTICS-TOTAL-MONTHS-X   REDEFINES                   
                   E2-ORTHODONTICS-TOTAL-MONTHS   PIC X(002).                   
               10  E2-DATE-APPLIANCE-INSERTED OCCURS 5 TIMES                    
                                           INDEXED BY RTE2-APPL-INDEX.          
                   15  E2-DATE-APPL-INSERT.                                     
                       20  E2-DATE-APPL-INSERT-CC PIC 9(002).                   
                       20  E2-DATE-APPL-INSERT-YY PIC 9(002).                   
                       20  E2-DATE-APPL-INSERT-MM PIC 9(002).                   
                       20  E2-DATE-APPL-INSERT-DD PIC 9(002).                   
                   15  E2-DATE-APPL-INSERT-X REDEFINES                          
                       E2-DATE-APPL-INSERT        PIC X(008).                   
               10  FILLER                         PIC X(008).                   
               10  FILLER                         PIC X(010).                   
               10  FILLER                         PIC X(008).                   
               10  E2-MONTHS-REMAINING            PIC 9(002).                   
               10  E2-MONTHS-REMAINING-X   REDEFINES                            
                   E2-MONTHS-REMAINING PIC X(002).                              
                   88  E2-VALID-MONTHS-REMAINING                                
                                            VALUE '01' THRU '36'.               
               10  E2-FIRST-VISIT-DATE.                                         
                   15  E2-FIRST-VISIT-CC PIC 9(002).                            
                   15  E2-FIRST-VISIT-YY PIC 9(002).                            
                   15  E2-FIRST-VISIT-MM PIC 9(002).                            
                   15  E2-FIRST-VISIT-DD PIC 9(002).                            
               10  E2-FIRST-VISIT-DATE-X          REDEFINES                     
                   E2-FIRST-VISIT-DATE PIC X(008).                              
               10  FILLER                        PIC  X(010).                   
               10  FILLER                        PIC  X(002).                   
               10  FILLER                        PIC  X(070).                   
                                                                                
      *----------------------------------------------------------------*        
      *          DENTI-CAL INFORMATION RECORD (E3)                     *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-E3-RECORD REDEFINES PCDS-00-RECORD.                         
               10  E3-REC-SEQ-NUM.                                              
                   15  E3-REC-TYPE                PIC X(002).                   
                       88  E3-REC                          VALUE 'E3'.          
                   15  FILLER                     PIC X(002).                   
               10  E3-PAT-CNTL-NUM                PIC X(017).                   
               10  E3-DENTICAL-DOC-CNTL-NUM       PIC X(011).                   
               10  E3-PAT-MEDI-CAL-ID-NUM         PIC X(014).                   
               10  E3-PAT-DENTAL-REC-NUM          PIC X(010).                   
               10  E3-REFERRING-PROV-NUM          PIC X(008).                   
               10  E3-XRAY-IND                    PIC X(001).                   
               10  E3-NUMBER-OF-XRAYS             PIC X(003).                   
               10  E3-OTHER-ATTACHMENT-IND        PIC X(001).                   
               10  E3-ACCIDENT-IND                PIC X(001).                   
               10  E3-EMPLOYMENT-REL-IND          PIC X(001).                   
               10  E3-ELIGIBILITY-PENDING-IND     PIC X(001).                   
               10  E3-MEDICARE-DENTAL-COV-IND     PIC X(001).                   
               10  E3-RETRO-ELIGIBILITY-IND       PIC X(001).                   
               10  E3-CHILD-HLTH-DISAB-PREV-IND   PIC X(001).                   
               10  E3-CALIF-CHILD-SVCS-IND        PIC X(001).                   
               10  E3-MAXILLO-ORTHO-SVCS-IND      PIC X(001).                   
               10  E3-REEVAL-REQ-IND              PIC X(001).                   
               10  E3-EXTEND-TIME-REQ-IND         PIC X(001).                   
               10  E3-PLACE-OF-SVC-CD             PIC X(002).                   
               10  E3-OTHER-PLACE-OF-SVC-CD       PIC X(016).                   
               10  E3-MISSING-PRIMARY-TEETH.                                    
                 15  E3-MISSING-PRIMARY-TOOTH  OCCURS 20                        
                                                  PIC X(001).                   
               10  E3-MISSING-PERM-TEETH.                                       
                 15  E3-MISSING-PERM-TOOTH     OCCURS 32                        
                                                  PIC X(002).                   
               10  FILLER                         PIC X(008).                   
               10  E3-CLAIM-TYPE                  PIC X(003).                   
                                                                                
      *----------------------------------------------------------------*        
      *        PAYER CLAIM REFERENCE NUMBERS RECORD (E4)               *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-E4-RECORD REDEFINES PCDS-00-RECORD.                         
               10  E4-REC-SEQ-NUM.                                              
                   15  E4-REC-TYPE                PIC X(002).                   
                       88  E4-REC                          VALUE 'E4'.          
                   15  FILLER                     PIC X(002).                   
               10  E4-SUB-SEQ-NUM                 PIC 9(002).                   
               10  E4-PAT-CNTL-NUM                PIC X(017).                   
               10  FILLER                         PIC X(002).                   
               10  E4-CLAIM-REFERENCE        OCCURS 5 TIMES                     
                                      INDEXED BY E4-CLAIM-REF-INDEX.            
                   15  E4-CLAIM-REF-QUAL          PIC X(002).                   
                   15  E4-CLAIM-REF-NUM           PIC X(030).                   
               10  FILLER                         PIC X(007).                   
                                                                                
      *----------------------------------------------------------------*        
      *        TOOTH STATUS INFORMATION RECORD (E5)                    *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-E5-RECORD REDEFINES PCDS-00-RECORD.                         
               10  E5-REC-SEQ-NUM.                                              
                   15  E5-REC-TYPE                PIC X(002).                   
                       88  E5-REC                          VALUE 'E5'.          
                   15  FILLER                     PIC X(002).                   
               10  E5-SUB-SEQ-NUM                 PIC 9(002).                   
               10  E5-PAT-CNTL-NUM                PIC X(017).                   
               10  E5-TOOTH-STATUS-INFO      OCCURS 19 TIMES                    
                                      INDEXED BY E5-TOOTH-STAT-INDEX.           
                   15  E5-TOOTH-NUMBER            PIC X(002).                   
                   15  FILLER                     PIC X(005).                   
                   15  E5-TOOTH-STATUS-CODE       PIC X(001).                   
               10  FILLER                         PIC X(017).                   
                                                                                
      *---------------------------------------------------------------*         
      *        PROVIDER INFORMATION RECORD (E6)                       *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-E6-RECORD REDEFINES PCDS-00-RECORD.                         
               10  E6-REC-SEQ-NUM.                                              
                   15  E6-REC-TYPE                PIC X(002).                   
                       88  E6-REC                          VALUE 'E6'.          
                   15  FILLER                     PIC X(002).                   
                   15  E6-SUB-SEQ-NUM             PIC 9(002).                   
                   15  E6-SUB-SEQ-NUM-X REDEFINES E6-SUB-SEQ-NUM                
                                                  PIC X(002).                   
               10  E6-PAT-CNTL-NUM                PIC X(017).                   
               10  E6-PROV-NAME-QUAL              PIC X(002).                   
               10  E6-ORG-TYPE                    PIC X(001).                   
               10  E6-PROV-NAME.                                                
                   15  E6-PROV-LNAME.                                           
                       20 E6-PROV-LNAME-1         PIC X(001).                   
                       20 FILLER                  PIC X(034).                   
                   15  E6-PROV-FNAME.                                           
                       20 E6-PROV-FNAME-1         PIC X(001).                   
                       20 FILLER                  PIC X(011).                   
                   15  E6-PROV-MINIT              PIC X(001).                   
               10  E6-ORG-NAME     REDEFINES                                    
                   E6-PROV-NAME.                                                
                       20 E6-ONAME                PIC X(035).                   
                       20 FILLER                  PIC X(013).                   
               10  E6-PROV-SUFFIX                 PIC X(010).                   
               10  E6-NAT-PROV-ID                 PIC X(015).                   
               10  E6-TAX-ID-NUM-QUAL             PIC X(001).                   
               10  E6-TAX-ID-NUM                  PIC X(009).                   
               10  E6-PROV-SPECIALTY              PIC X(003).                   
               10  E6-PROV-NET-ID                 PIC X(015).                   
               10  E6-PROV-TAXONOMY-CODE          PIC X(011).                   
               10  FILLER                         PIC X(035).                   
               10  E6-PROV-TELEPHONE              PIC 9(010).                   
               10  E6-PROV-TELEPHONE-X  REDEFINES                               
                   E6-PROV-TELEPHONE              PIC X(010).                   
               10  FILLER                         PIC X(009).                   
                                                                                
      *---------------------------------------------------------------*         
      *    PROVIDER SECONDARY REFERENCE NUMBERS RECORD (E7)           *         
      *---------------------------------------------------------------*         
           05  PCDS-E7-RECORD REDEFINES PCDS-00-RECORD.                         
               10  E7-REC-SEQ-NUM.                                              
                   15  E7-REC-TYPE                PIC X(002).                   
                       88  E7-REC                          VALUE 'E7'.          
                   15  FILLER                     PIC X(002).                   
               10  E7-SUB-SEQ-NUM                 PIC 9(002).                   
               10  E7-PAT-CNTL-NUM                PIC X(017).                   
               10  E7-PROV-NAME-QUAL              PIC X(002).                   
               10  E7-PROV-SECONDARY-ID-INFO    OCCURS 5 TIMES                  
                                      INDEXED BY E7-SECD-ID-INDEX.              
                   15  E7-PROV-SEC-ID-REF-QUAL    PIC X(002).                   
                   15  E7-PROV-SEC-ID-REF-NUM     PIC X(015).                   
               10  FILLER                         PIC X(082).                   
                                                                                
      *---------------------------------------------------------------*         
      *    PROVIDER ADDRESS INFORMATION RECORD (E8)                   *         
      *---------------------------------------------------------------*         
           05  PCDS-E8-RECORD REDEFINES PCDS-00-RECORD.                         
               10  E8-REC-SEQ-NUM.                                              
                   15  E8-REC-TYPE                PIC X(002).                   
                       88  E8-REC                          VALUE 'E8'.          
                   15  FILLER                     PIC X(002).                   
               10  E8-SUB-SEQ-NUM                 PIC 9(002).                   
               10  E8-PAT-CNTL-NUM                PIC X(017).                   
               10  E8-PROV-NAME-QUAL              PIC X(002).                   
               10  E8-PROV-ADDITIONAL-NAME        PIC X(060).                   
               10  E8-PROV-ADDRESS-LINE-1         PIC X(030).                   
               10  E8-PROV-ADDRESS-LINE-2         PIC X(030).                   
               10  E8-PROV-CITY                   PIC X(020).                   
               10  E8-PROV-STATE                  PIC X(002).                   
               10  E8-PROV-ZIP                    PIC X(009).                   
               10  E8-PROV-COUNTRY-CODE           PIC X(003).                   
               10  FILLER                         PIC X(013).                   
                                                                                
      *---------------------------------------------------------------*         
      *    MEDICAID RECLAMATION INFORMATION RECORD (E9)               *         
      *---------------------------------------------------------------*         
           05  PCDS-E9-RECORD REDEFINES PCDS-00-RECORD.                         
               10  E9-REC-SEQ-NUM.                                              
                   15  E9-REC-TYPE                PIC X(002).                   
                       88  E9-REC                          VALUE 'E9'.          
                   15  FILLER                     PIC X(002).                   
               10  E9-PAT-CNTL-NUM                PIC X(017).                   
               10  FILLER                         PIC X(003).                   
               10  E9-MCAID-PAID-AMT              PIC 9(006)V99.                
               10  E9-MCAID-PAID-AMT-X    REDEFINES                             
                   E9-MCAID-PAID-AMT              PIC X(008).                   
               10  E9-PROV-ID                     PIC X(015).                   
               10  E9-NAME-QUAL                   PIC X(001).                   
               10  E9-ORG-OR-LNAME                PIC X(020).                   
               10  E9-MCAID-RECLAM-PAYER-FNAME    PIC X(010).                   
               10  E9-MCAID-RECLAM-PAYER-M-INIT   PIC X(001).                   
               10  E9-MCAID-RECLAM-PAYER-ADDR     PIC X(018).                   
               10  E9-MCAID-RECLAM-PAYER-CITY     PIC X(015).                   
               10  E9-MCAID-RECLAM-PAYER-STATE    PIC X(002).                   
               10  E9-MCAID-RECLAM-PAYER-ZIP      PIC X(009).                   
               10  FILLER                         PIC X(069).                   
                                                                                
      *---------------------------------------------------------------*         
      *            NSF CLAIM INFORMATION RECORD (EA)                  *         
      *---------------------------------------------------------------*         
           05  PCDS-EA-RECORD REDEFINES PCDS-00-RECORD.                         
               10  EA-REC-SEQ-NUM.                                              
                   15  EA-REC-TYPE                PIC X(002).                   
                       88  EA-REC                          VALUE 'EA'.          
                   15  FILLER                     PIC X(002).                   
               10  EA-PAT-CNTL-NUM                PIC X(020).                   
               10  FILLER                         PIC X(007).                   
               10  EA-INFO-RELEASE-DATE.                                        
                   15  EA-INFO-REL-CC             PIC X(002).                   
                   15  EA-INFO-REL-YY             PIC X(002).                   
                   15  EA-INFO-REL-MM             PIC X(002).                   
                   15  EA-INFO-REL-DD             PIC X(002).                   
               10  EA-SAME-SIMILAR-SYMP-DATE.                                   
                   15  EA-SAM-SIM-SYM-CC           PIC X(002).                  
                   15  EA-SAM-SIM-SYM-YY PIC X(002).                            
                   15  EA-SAM-SIM-SYM-MM PIC X(002).                            
                   15  EA-SAM-SIM-SYM-DD PIC X(002).                            
               10  EA-DISABILITY-TYPE             PIC X(001).                   
                   88  EA-SHORT-TERM                     VALUE '1'.             
                   88  EA-LONG-TERM                      VALUE '2'.             
                   88  EA-PERMANANENT                    VALUE '3'.             
                   88  EA-NO-DISABILITY                  VALUE '4'.             
               10  EA-LAB-IND                     PIC X(001).                   
               10  FILLER                         PIC X(001).                   
               10  EA-TOT-PURCH-SVC-CHRG-AMT      PIC 9(004)V99.                
               10  EA-TOT-PURCH-SVC-CHRG-AMT-X  REDEFINES                       
                   EA-TOT-PURCH-SVC-CHRG-AMT      PIC X(006).                   
               10  EA-PROV-SIG-IND                PIC X(001).                   
                   88  EA-PROV-SIG-ON-FILE               VALUE 'Y'.             
                   88  EA-PROV-SIGN-NOT-ON-FILE          VALUE 'N'.             
               10  FILLER                         PIC X(001).                   
               10  EA-FUNCTIONAL-STATUS           PIC X(002).                   
               10  EA-CHAMPUS-NON-AVAIL-IND       PIC X(001).                   
               10  EA-SUP-PROV-IND                PIC X(001).                   
                   88  SUP-PROV-OTHER-THAN-REND       VALUE 'Y'.                
                   88  NOT-SUP-PROV-OTHER-THAN-REND   VALUE 'N'.                
               10  EA-RESUBM-CODE                 PIC X(002).                   
               10  EA-RESUBM-REF-NUM              PIC X(015).                   
               10  EA-DATE-LAST-SEEN.                                           
                   15  EA-LAST-SEEN-CC            PIC X(002).                   
                   15  EA-LAST-SEEN-YY            PIC X(002).                   
                   15  EA-LAST-SEEN-MM            PIC X(002).                   
                   15  EA-LAST-SEEN-DD            PIC X(002).                   
               10  EA-DATE-DOC-SENT.                                            
                   15  EA-DOC-SENT-CC             PIC X(002).                   
                   15  EA-DOC-SENT-YY             PIC X(002).                   
                   15  EA-DOC-SENT-MM             PIC X(002).                   
                   15  EA-DOC-SENT-DD             PIC X(002).                   
               10  EA-HOME-BOUND-IND              PIC X(001).                   
               10  EA-CLAM-PROC-SPEC-1            PIC X(008).                   
               10  EA-CLAM-PROC-SPEC-2            PIC X(008).                   
               10  EA-CLAM-PROC-SPEC-3            PIC X(008).                   
               10  EA-DATE-CARE-RELINQUISHED.                                   
                   15  EA-CARE-RELINQ-CC          PIC X(002).                   
                   15  EA-CARE-RELINQ-YY          PIC X(002).                   
                   15  EA-CARE-RELINQ-MM          PIC X(002).                   
                   15  EA-CARE-RELINQ-DD          PIC X(002).                   
               10  EA-ATTACH-CNTL-NUM             PIC X(035).                   
               10  FILLER                         PIC X(029).                   
                                                                                
      *---------------------------------------------------------------*         
      *            NSF CLAIM INFORMATION RECORD (EB)                  *         
      *---------------------------------------------------------------*         
           05  PCDS-EB-RECORD REDEFINES PCDS-00-RECORD.                         
               10  EB-REC-SEQ-NUM.                                              
                   15  EB-REC-TYPE                PIC X(002).                   
                       88  EB-REC                          VALUE 'EB'.          
                   15  FILLER                     PIC X(002).                   
               10  EB-PAT-CNTL-NUM                PIC X(020).                   
               10  EB-MED-REC-NUM                 PIC X(017).                   
               10  EB-WORK-RETURN-DATE.                                         
                   15  EB-WORK-RET-CC             PIC X(002).                   
                   15  EB-WORK-RET-YY             PIC X(002).                   
                   15  EB-WORK-RET-MM             PIC X(002).                   
                   15  EB-WORK-RET-DD             PIC X(002).                   
               10  FILLER                         PIC X(008).                   
               10  EB-ADM-DATE-2.                                               
                   15  EB-ADM-DATE-2-CC           PIC X(002).                   
                   15  EB-ADM-DATE-2-YY           PIC X(002).                   
                   15  EB-ADM-DATE-2-MM           PIC X(002).                   
                   15  EB-ADM-DATE-2-DD           PIC X(002).                   
               10  EB-DISCH-DATE.                                               
                   15  EB-DISCH-DATE-2-CC         PIC X(002).                   
                   15  EB-DISCH-DATE-2-YY         PIC X(002).                   
                   15  EB-DISCH-DATE-2-MM         PIC X(002).                   
                   15  EB-DISCH-DATE-2-DD         PIC X(002).                   
               10  EB-EMT-PARAMEDIC-NAME.                                       
                   15  EB-PRMEDIC-LNAME           PIC X(020).                   
                   15  EB-PRMEDIC-FNAME           PIC X(012).                   
                   15  EB-PRMEDIC-MINIT           PIC X(001).                   
               10  EB-CARE-ASSUMED-DATE.                                        
                   15  EB-CARE-ASSUM-CC           PIC X(002).                   
                   15  EB-CARE-ASSUM-YY           PIC X(002).                   
                   15  EB-CARE-ASSUM-MM           PIC X(002).                   
                   15  EB-CARE-ASSUM-DD           PIC X(002).                   
               10  EB-DIAGNOSIS-CODES-5-6-7-8.                                  
                   15 EB-DIAG     OCCURS 4 TIMES                                
                                           INDEXED BY EB-DIAG-INDEX.            
                      20 EB-DIAG-CODE             PIC X(008).                   
               10  EB-MILITARY-ACC-IND            PIC X(001).                   
               10  EB-NON-AVAIL-STMT-NUM          PIC X(011).                   
               10  EB-SPEC-PROC-IND               PIC X(001).                   
               10  EB-REASON-ZERO-OHI-IND         PIC X(002).                   
               10  EB-SPEC-PGM-IND                PIC X(002).                   
                   88  EB-EPSDT-CHAP                       VALUE '01'.          
                   88  EB-PHYS-HNDCP-CHILD-PGM             VALUE '02'.          
                   88  EB-SPECIAL-FED-FUNDING              VALUE '03'.          
                   88  EB-DISABILITY                       VALUE '05'.          
                   88  EB-PPV-MEDICARE-100-PCT-PYMT        VALUE '06'.          
                   88  EB-INDUCED-ABORTN-LIFE-THRT         VALUE '07'.          
                   88  EB-INDUCED-ABORTN-RAPE-VCTM         VALUE '08'.          
                   88  EB-SECOND-SURGICAL-OPINION          VALUE '09'.          
                   88  EB-MEDICAID-FAMILY-PLANNING         VALUE '10'.          
                   88  EB-MEDICARE-DEMO-PROJECT-LUNG       VALUE '30'.          
                   88  EB-CHAMP-PGM-PAT-SPONSOR            VALUE 'A '.          
                   88  EB-CHAMP-PGM-PAT-SPOUSE             VALUE 'B '.          
                   88  EB-CHAMP-PGM-PAT-CHILD    VALUE 'C1' THRU 'C9'.          
                   88  EB-CHAMP-PGM-PAT-WIDOW              VALUE 'D '.          
                   88  EB-CHAMP-PGM-PAT-UNKNOWN            VALUE 'W '.          
                   88  EB-VALID-SPEC-PGM-IND               VALUE                
                              '01' THRU '03'                                    
                              '05' THRU '10'                                    
                              '30' 'A ' 'B ' 'D ' 'W '                          
                              'C1' THRU 'C9'.                                   
               10  EB-PROV-CERT-STMT-IND          PIC X(001).                   
               10  EB-CLAIM-ADJ-CODE              PIC X(001).                   
                   88  EB-CLAIM-ADJ     VALUE '1'.                              
                   88  EB-CLAIM-NOT-ADJ VALUE '2'.                              
               10  EB-BILLING-EXCP-CODE           PIC X(002).                   
               10  FILLER                         PIC X(002).                   
               10  EB-REJ-CLAM-CRN                PIC X(010).                   
               10  EB-REJ-CLAM-RA                 PIC X(002).                   
               10  EB-MEDICAID-TYPE-CLAM          PIC X(003).                   
               10  FILLER                         PIC X(008).                   
                                                                                
      *---------------------------------------------------------------*         
      *       HIPAA STATE LEGISLATIVE INFORMATION RECORD (EK)         *         
      *---------------------------------------------------------------*         
           05  PCDS-EK-RECORD REDEFINES PCDS-00-RECORD.                         
               10  EK-REC-SEQ-NUM.                                              
                   15  EK-REC-TYPE                PIC X(002).                   
                       88  EK-REC                          VALUE 'EK'.          
                   15  FILLER                     PIC X(002).                   
               10  EK-SUB-SEQ-NUM                 PIC 9(002).                   
               10  EK-PAT-CNTL-NUM                PIC X(020).                   
               10  EK-STATE-LEGIS-CLAIM-INFO-1    PIC X(080).                   
               10  EK-STATE-LEGIS-CLAIM-INFO-2    PIC X(080).                   
               10  FILLER                         PIC X(006).                   
                                                                                
      *---------------------------------------------------------------*         
      *       HIPAA DATE/TIME INFORMATION RECORD (EM)                 *         
      *---------------------------------------------------------------*         
           05  PCDS-EM-RECORD REDEFINES PCDS-00-RECORD.                         
               10  EM-REC-SEQ-NUM.                                              
                   15  EM-REC-TYPE                PIC X(002).                   
                       88  EM-REC                          VALUE 'EM'.          
                   15  FILLER                     PIC X(002).                   
               10  EM-SUB-SEQ-NUM                 PIC 9(002).                   
               10  EM-PAT-CNTL-NUM                PIC X(020).                   
               10  EM-DATE-TIME-INFO    OCCURS 8 TIMES                          
                                        INDEXED BY EM-INX.                      
                   15  EM-DATE-TIME-QUAL          PIC X(003).                   
                   15  EM-DATE.                                                 
                       20  EM-DATE-CC             PIC X(002).                   
                       20  EM-DATE-YY             PIC X(002).                   
                       20  EM-DATE-MM             PIC X(002).                   
                       20  EM-DATE-DD             PIC X(002).                   
                   15  EM-HOUR.                                                 
                       20  EM-HOUR-HH             PIC X(002).                   
                       20  EM-HOUR-MM             PIC X(002).                   
               10  FILLER                         PIC X(046).                   
                                                                                
      *----------------------------------------------------------------*        
      *              HIPAA CLAIM INFORMATION RECORD (EN)               *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-EN-RECORD REDEFINES PCDS-00-RECORD.                         
               10  EN-REC-SEQ-NUM.                                              
                   15  EN-REC-TYPE                PIC X(002).                   
                       88  EN-REC                     VALUE 'EN'.               
                   15  FILLER                     PIC X(002).                   
               10  EN-PAT-CNTL-NO                 PIC X(020).                   
               10  EN-PLACE-SERVICE-CD            PIC X(002).                   
               10  EN-CLAIM-FREQ-CD               PIC X(001).                   
               10  EN-AUTO-ACCIDENT-COUNTRY-CD    PIC X(003).                   
               10  EN-PARTIC-AGREEMENT-CD         PIC X(001).                   
               10  EN-DELAY-REASON-CD             PIC X(002).                   
               10  EN-CR-DR-CARD-MAX-AMT          PIC X(017).                   
               10  FILLER                         PIC X(015).                   
               10  EN-CONTRACT-TYPE-CD            PIC X(002).                   
               10  EN-CONTRACT-AMT                PIC 9(008)V99.                
               10  EN-CONTRACT-AMT-X                                            
                   REDEFINES EN-CONTRACT-AMT      PIC X(010).                   
               10  EN-CONTRACT-ALLOW-OR-CHG-PCT   PIC X(006).                   
               10  EN-CONTRACT-CODE               PIC X(030).                   
               10  EN-TERMS-DISCOUNT-PCT          PIC X(006).                   
               10  EN-CONTRACT-VERSION            PIC X(020).                   
               10  EN-SERVICE-AUTH-EXCEPTION-CD   PIC X(001).                   
               10  EN-AUTO-ACCIDENT-CD-2          PIC X(001).                   
               10  EN-AUTO-ACCIDENT-CD-3          PIC X(001).                   
               10  FILLER                         PIC X(050).                   
                                                                                
      *----------------------------------------------------------------*        
      *   HIPAA AMBULANCE TRANSPORT INFORMATION RECORD (EP)            *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-EP-RECORD REDEFINES PCDS-00-RECORD.                         
               10  EP-REC-SEQ-NUM.                                              
                   15  EP-REC-TYPE                PIC X(002).                   
                       88  EP-REC                     VALUE 'EP'.               
                   15  FILLER                     PIC X(002).                   
               10  EP-PAT-CNTL-NO                 PIC X(020).                   
               10  EP-PAT-WEIGHT-LBS              PIC X(004).                   
               10  EP-TRANSPORT-CODE              PIC X(001).                   
               10  EP-TRANSPORT-REASON-CD         PIC X(001).                   
               10  EP-TRANSPORT-DISTANCE-MILES    PIC X(004).                   
               10  EP-TRANSPORT-PURPOSE-DESC      PIC X(079).                   
               10  EP-TRANSPT-STRETCHER-PURP-DESC PIC X(079).                   
                                                                                
      *----------------------------------------------------------------*        
      *   HIPAA AMBULANCE / VISION CONDDITION CODES RECORD (EQ)        *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-EQ-RECORD REDEFINES PCDS-00-RECORD.                         
               10  EQ-REC-SEQ-NUM.                                              
                   15  EQ-REC-TYPE                PIC X(002).                   
                       88  EQ-REC                     VALUE 'EQ'.               
                   15  FILLER                     PIC X(002).                   
               10  EQ-PAT-CNTL-NO                 PIC X(020).                   
                                                                                
               10  EQ-CERT-CONDITION-CODES-TABLE                                
                   OCCURS 3 TIMES                                               
                   INDEXED BY EQ-INX.                                           
                                                                                
                   15  EQ-CERTIFICATION-CATEGORY  PIC X(002).                   
                   15  EQ-CERT-CONDITION-IND      PIC X(001).                   
                                                                                
                   15  EQ-CONDITION-CODES                                       
                       OCCURS 5 TIMES                                           
                       INDEXED BY EQ-CC-INX.                                    
                       20  EQ-CONDITION-CODE      PIC X(002).                   
                                                                                
               10  FILLER                         PIC X(129).                   
                                                                                
      *----------------------------------------------------------------*        
      *        HIPAA CHIROPRACTIC INFORMATION RECORD (ER)              *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-ER-RECORD REDEFINES PCDS-00-RECORD.                         
               10  ER-REC-SEQ-NUM.                                              
                   15  ER-REC-TYPE                PIC X(002).                   
                       88  ER-REC                     VALUE 'ER'.               
                   15  FILLER                     PIC X(002).                   
               10  ER-PAT-CNTL-NO                 PIC X(020).                   
               10  FILLER                         PIC X(002).                   
               10  ER-TREATMENT-SERIES-NO         PIC X(007).                   
               10  ER-TREATMENT-COUNT             PIC X(007).                   
               10  ER-SUBLUXATION-LVL-CODE-1      PIC X(003).                   
               10  ER-SUBLUXATION-LVL-CODE-2      PIC X(003).                   
               10  ER-TMT-SERIES-UNIT-BASIS-MEAS  PIC X(002).                   
               10  ER-TREATMENT-SERIES-PERIOD-CNT PIC X(005).                   
               10  ER-MONTHLY-TREATMENT-CNT       PIC X(003).                   
               10  ER-COMPLICATION-IND            PIC X(001).                   
               10  ER-XRAY-AVAIL-IND              PIC X(001).                   
               10  ER-LAST-XRAY-DATE.                                           
                   15  ER-LAST-XRAY-DATE-CC       PIC 9(002).                   
                   15  ER-LAST-XRAY-DATE-YY       PIC 9(002).                   
                   15  ER-LAST-XRAY-DATE-MM       PIC 9(002).                   
                   15  ER-LAST-XRAY-DATE-DD       PIC 9(002).                   
               10  ER-LAST-XRAY-DATE-X                                          
                   REDEFINES ER-LAST-XRAY-DATE.                                 
                   15  ER-LAST-XRAY-DATE-CC-X     PIC X(002).                   
                   15  ER-LAST-XRAY-DATE-YY-X     PIC X(002).                   
                   15  ER-LAST-XRAY-DATE-MM-X     PIC X(002).                   
                   15  ER-LAST-XRAY-DATE-DD-X     PIC X(002).                   
               10  ER-INITIAL-TREATMENT-DATE.                                   
                   15  ER-INITIAL-TMT-DATE-CC     PIC 9(002).                   
                   15  ER-INITIAL-TMT-DATE-YY     PIC 9(002).                   
                   15  ER-INITIAL-TMT-DATE-MM     PIC 9(002).                   
                   15  ER-INITIAL-TMT-DATE-DD     PIC 9(002).                   
               10  ER-INITIAL-TREATMENT-DATE-X                                  
                   REDEFINES ER-INITIAL-TREATMENT-DATE.                         
                   15  ER-INITIAL-TMT-DATE-CC-X   PIC X(002).                   
                   15  ER-INITIAL-TMT-DATE-YY-X   PIC X(002).                   
                   15  ER-INITIAL-TMT-DATE-MM-X   PIC X(002).                   
                   15  ER-INITIAL-TMT-DATE-DD-X   PIC X(002).                   
               10  ER-ACUTE-MANIFESTATION-DATES                                 
                   OCCURS 5 TIMES                                               
                   INDEXED BY ER-INX.                                           
                   15  ER-ACUTE-MANIFEST-DATE.                                  
                       20  ER-ACUTE-MANIFEST-CC   PIC 9(002).                   
                       20  ER-ACUTE-MANIFEST-YY   PIC 9(002).                   
                       20  ER-ACUTE-MANIFEST-MM   PIC 9(002).                   
                       20  ER-ACUTE-MANIFEST-DD   PIC 9(002).                   
                   15  ER-ACUTE-MANIFEST-DATE-X                                 
                       REDEFINES ER-ACUTE-MANIFEST-DATE.                        
                       20  ER-ACUTE-MANIFEST-CC-X PIC X(002).                   
                       20  ER-ACUTE-MANIFEST-YY-X PIC X(002).                   
                       20  ER-ACUTE-MANIFEST-MM-X PIC X(002).                   
                       20  ER-ACUTE-MANIFEST-DD-X PIC X(002).                   
               10  FILLER                         PIC X(078).                   
                                                                                
      *----------------------------------------------------------------*        
      *                                              (RTES)            *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-ES-RECORD REDEFINES PCDS-00-RECORD.                         
               10  ES-REC-TYPE                    PIC X(002).                   
                       88  ES-REC                     VALUE 'ES'.               
               10  FILLER                         PIC X(002).                   
               10  FILLER                         PIC X(002).                   
               10  ES-PAT-CNTL-NO                 PIC X(020).                   
               10  ES-PAT-COND-CD                 PIC X(001).                   
               10  ES-PAT-COND-DESC-1             PIC X(080).                   
               10  ES-PAT-COND-DESC-2             PIC X(080).                   
               10  FILLER                         PIC X(005).                   
                                                                                
      *----------------------------------------------------------------*        
      *                                              (RTEW)            *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-EW-RECORD REDEFINES PCDS-00-RECORD.                         
               10  EW-REC-TYPE                    PIC X(002).                   
                       88  EW-REC                     VALUE 'EW'.               
               10  FILLER                         PIC X(002).                   
               10  EW-SUB-SEQ-NUM                 PIC 9(002).                   
               10  EW-PAT-CNTL-NO                 PIC X(020).                   
               10  EW-ATTACHMENT-INFO         OCCURS 4 TIMES                    
                                              INDEXED BY EW-INX.                
                   15  EW-ATT-CD                  PIC X(002).                   
                   15  EW-ATT-XMIT-CD             PIC X(002).                   
                   15  EW-ATT-CNTL-NUM            PIC X(035).                   
               10  FILLER                         PIC X(010).                   
                                                                                
      *----------------------------------------------------------------*        
      *                                              (RTEX)            *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-EX-RECORD REDEFINES PCDS-00-RECORD.                         
               10  EX-REC-TYPE                    PIC X(002).                   
                       88  EX-REC                     VALUE 'EX'.               
               10  FILLER                         PIC X(002).                   
               10  EX-SUB-SEQ-NUM                 PIC 9(002).                   
               10  EX-PAT-CNTL-NO                 PIC X(020).                   
               10  EX-DISCIP-TYPE                 PIC X(002).                   
               10  EX-TOT-VISITS-REND             PIC 9(004).                   
               10  EX-PROJ-VISIT-CNT              PIC 9(004).                   
               10  EX-NUM-VISITS-1                PIC X(004).                   
               10  EX-MOD-UBM-CD-1                PIC X(002).                   
               10  EX-MOD-AMT-1                   PIC X(006).                   
               10  EX-TIME-PRD-QUAL-1             PIC X(002).                   
               10  EX-DUR-VISITS-1                PIC X(003).                   
               10  EX-CAL-PTRN-CD-1               PIC X(002).                   
               10  EX-DEL-TIME-PTRN-CD-1          PIC X(001).                   
               10  FILLER                         PIC X(011).                   
               10  EX-NUM-VISITS-2                PIC X(004).                   
               10  EX-MOD-UBM-CD-2                PIC X(002).                   
               10  EX-MOD-AMT-2                   PIC X(006).                   
               10  EX-TIME-PRD-QUAL-2             PIC X(002).                   
               10  EX-DUR-VISITS-2                PIC X(003).                   
               10  EX-CAL-PTRN-CD-2               PIC X(002).                   
               10  EX-DEL-TIME-PTRN-CD-2          PIC X(001).                   
               10  EX-NUM-VISITS-3                PIC X(004).                   
               10  EX-MOD-UBM-CD-3                PIC X(002).                   
               10  EX-MOD-AMT-3                   PIC X(006).                   
               10  EX-TIME-PRD-QUAL-3             PIC X(002).                   
               10  EX-DUR-VISITS-3                PIC X(003).                   
               10  EX-CAL-PTRN-CD-3               PIC X(002).                   
               10  EX-DEL-TIME-PTRN-CD-3          PIC X(001).                   
               10  FILLER                         PIC X(085).                   
                                                                                
      *----------------------------------------------------------------*        
      *                                              (RTEY)            *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-EY-RECORD REDEFINES PCDS-00-RECORD.                         
               10  EY-REC-TYPE                    PIC X(002).                   
                       88  EY-REC                     VALUE 'EY'.               
               10  FILLER                         PIC X(002).                   
               10  EY-SUB-SEQ-NUM                 PIC 9(002).                   
               10  EY-PAT-CNTL-NO                 PIC X(020).                   
               10  EY-CLAIM-NOTE-QUAL-1           PIC X(003).                   
               10  EY-CLAIM-NOTE-1                PIC X(080).                   
               10  EY-CLAIM-NOTE-QUAL-2           PIC X(003).                   
               10  EY-CLAIM-NOTE-2                PIC X(080).                   
                                                                                
      *---------------------------------------------------------------*         
      *            SERVICES  PERFORMED RECORD        (RTF0)           *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-F0-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  F0-REC-SEQ-NUM.                                              
                   15  F0-REC-TYPE         PIC X(002).                          
                       88  F0-REC                          VALUE 'F0'.          
                   15  F0-SEQ-NUM          PIC 9(002).                          
                   15  F0-SEQ-NUM-X REDEFINES F0-SEQ-NUM                        
                                           PIC X(002).                          
               10  F0-PAT-CNTL-NUM         PIC X(017).                          
               10  FILLER                  PIC X(003).                          
               10  F0-LN-ITEM-CNTL-NUM     PIC X(020).                          
               10  F0-DATE-OF-SERVICE.                                          
                   15  F0-FROM-DATE.                                            
                       20 F0-FROM-DATE-CC                                       
                                           PIC 9(002).                          
                       20 F0-FROM-DATE-YY                                       
                                           PIC 9(002).                          
                       20 F0-FROM-DATE-MM                                       
                                           PIC 9(002).                          
                       20 F0-FROM-DATE-DD                                       
                                           PIC 9(002).                          
                   15  F0-THRU-DATE.                                            
                       25 F0-THRU-DATE-CC                                       
                                           PIC 9(002).                          
                       25 F0-THRU-DATE-YY                                       
                                           PIC 9(002).                          
                       25 F0-THRU-DATE-MM                                       
                                           PIC 9(002).                          
                       25 F0-THRU-DATE-DD                                       
                                           PIC 9(002).                          
               10  F0-DATE-OF-SERVICE-X   REDEFINES                             
                   F0-DATE-OF-SERVICE.                                          
                   15  F0-FROM-DATE-X.                                          
                       20 F0-FROM-DATE-CC-X                                     
                                           PIC X(002).                          
                       20 F0-FROM-DATE-YY-X                                     
                                           PIC X(002).                          
                       20 F0-FROM-DATE-MM-X                                     
                                           PIC X(002).                          
                       20 F0-FROM-DATE-DD-X                                     
                                           PIC X(002).                          
                   15  F0-THRU-DATE-X.                                          
                       20 F0-THRU-DATE-CC-X                                     
                                           PIC X(002).                          
                       20 F0-THRU-DATE-YY-X                                     
                                           PIC X(002).                          
                       20 F0-THRU-DATE-MM-X                                     
                                           PIC X(002).                          
                       20 F0-THRU-DATE-DD-X                                     
                                           PIC X(002).                          
      * C.N. ADDED VALID PLACE OF SERVICE CODES ACCORDING TO EXHIBIT 18         
               10  F0-SERVICE-PLACE        PIC X(002).                          
                   88  F0-INDEPENDENT-LAB           VALUE '81'.                 
                   88  F0-INPATIENT-HOSPITAL        VALUE '21'.                 
                   88  F0-INTERMEDIATE-CARE         VALUE '32'.                 
                   88  F0-RESIDENTIAL-TREATMENT     VALUE '56'.                 
                   88  F0-SKILLED-NURSING           VALUE '31'.                 
                   88  F0-VALID-PLACE-SERVICE       VALUES '11' '12'            
                       '21' '22' '23' '24' '25' '26' '31' '32' '33'             
                       '34' '41' '42' '50' '51' '52' '53' '54' '55'             
                       '56' '60' '61' '62' '65' '71' '72' '81' '99'.            
                   88  F0-VALID-PLACE-SERVICE-DCDS  VALUES                      
                       '11' '12' '21' '22' '31' '34'.                           
               10  F0-SERVICE-TYPE         PIC X(002).                          
               10  F0-PROC-CODE-QUAL       PIC X(002).                          
               10  F0-PROC-CODE.                                                
                   15  F0-PROC-CODE-POS-1                                       
                                           PIC X(001).                          
                       88  F0-STATE-SPECIFIC-PROC-CODE VALUES                   
                           'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J'              
                           'K' 'L' 'M' 'N' 'O' 'P' 'Q' 'R' 'S' 'T'              
                           'U' 'V' 'W' 'X' 'Y' 'Z'.                             
                   15  FILLER              PIC X(004).                          
               10  F0-PROC-CODE-MODIFIERS.                                      
                   15  F0-PROC-MOD-1.                                           
                       20  F0-PROC-MOD1-POS1   PIC X(001).                      
                       20  F0-PROC-MOD1-POS2   PIC X(001).                      
                   15  F0-PROC-MOD-2.                                           
                       20  F0-PROC-MOD2-POS1   PIC X(001).                      
                       20  F0-PROC-MOD2-POS2   PIC X(001).                      
                   15  F0-PROC-MOD-3.                                           
                       20  F0-PROC-MOD3-POS1   PIC X(001).                      
                       20  F0-PROC-MOD3-POS2   PIC X(001).                      
                   15  F0-PROC-MOD-4.                                           
                       20  F0-PROC-MOD4-POS1   PIC X(001).                      
                       20  F0-PROC-MOD4-POS2   PIC X(001).                      
               10  F0-LINE-CHRG            PIC S9(006)V99.                      
               10  F0-LINE-CHRG-X REDEFINES                                     
                   F0-LINE-CHRG.                                                
                   15 FILLER               PIC X(007).                          
                   15 F0-LINE-CHRG-LAST-BYTE   PIC X(001).                      
               10  F0-LINE-CHRG-U REDEFINES                                     
                   F0-LINE-CHRG            PIC 9(006)V99.                       
               10  FILLER                  PIC X(002).                          
                                                                                
               10  F0-DIAGNOSIS-CODE-POINTERS.                                  
                   15  F0-DIAG-PTR-1       PIC X(001).                          
                       88  F0-VALID-DIAG-PTR-1                                  
                                           VALUES '0' THRU '8'.                 
                   15  F0-DIAG-PTR-1-9  REDEFINES                               
                       F0-DIAG-PTR-1       PIC 9(001).                          
                                                                                
                   15  F0-DIAG-PTR-2       PIC X(001).                          
                       88  F0-VALID-DIAG-PTR-2                                  
                                           VALUES '0' THRU '8'.                 
                   15  F0-DIAG-PTR-2-9  REDEFINES                               
                       F0-DIAG-PTR-2       PIC 9(001).                          
                                                                                
                   15  F0-DIAG-PTR-3       PIC X(001).                          
                       88  F0-VALID-DIAG-PTR-3                                  
                                           VALUES '0' THRU '8'.                 
                   15  F0-DIAG-PTR-3-9  REDEFINES                               
                       F0-DIAG-PTR-3       PIC 9(001).                          
                                                                                
                   15  F0-DIAG-PTR-4       PIC X(001).                          
                       88  F0-VALID-DIAG-PTR-4                                  
                                           VALUES '0' THRU '8'.                 
                   15  F0-DIAG-PTR-4-9  REDEFINES                               
                       F0-DIAG-PTR-4       PIC 9(001).                          
               10  F0-SERVICE-UNIT-QUAL    PIC X(002).                          
               10  F0-SERVICE-UNIT         PIC 9(014)V9.                        
               10  F0-SERVICE-UNIT-X    REDEFINES                               
                   F0-SERVICE-UNIT         PIC X(015).                          
               10  F0-NDC-CODE             PIC X(011).                          
               10  F0-RX-NUM               PIC X(015).                          
               10  F0-TOOTH-NUM-QUAL       PIC X(003).                          
               10  F0-TOOTH-1              PIC X(002).                          
               10  F0-TOOTH-1-SURF-CD      PIC X(005).                          
               10  FILLER                  PIC X(001).                          
               10  F0-ORAL-CAVITY-CODES  OCCURS 5 TIMES                         
                                         INDEXED BY F0-OCC-INX.                 
                   15  F0-ORAL-CAV-CD      PIC X(002).                          
               10  F0-PRO-CRO-INL-IND      PIC X(001).                          
               10  F0-PRIOR-PLACE-DATE.                                         
                   15  F0-PP-CC            PIC X(002).                          
                   15  F0-PP-YY            PIC X(002).                          
                   15  F0-PP-MM            PIC X(002).                          
                   15  F0-PP-DD            PIC X(002).                          
               10  FILLER                  PIC X(007).                          
               10  FILLER                  PIC X(009).                          
               10  F0-SALES-TAX-AMT        PIC 9(006)V99.                       
               10  F0-SALES-TAX-AMT-X   REDEFINES                               
                   F0-SALES-TAX-AMT        PIC X(008).                          
               10  FILLER                  PIC X(002).                          
                                                                                
      *---------------------------------------------------------------*         
      *            THIRD PARTY ORGANIZATION (MANAGED CARE) = RTF1     *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-F1-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  F1-REC-SEQ-NUM.                                              
                   15  F1-REC-TYPE             PIC X(002).                      
                       88  F1-REC              VALUE 'F1'.                      
                   15  F1-SEQ-NUM              PIC 9(002).                      
                   15  F1-SEQ-NUM-X REDEFINES F1-SEQ-NUM                        
                                               PIC X(002).                      
               10  F1-PAT-CNTL-NUM             PIC X(017).                      
               10  F1-TPO-ID-NUMBER.                                            
                   15  F1-TPO-ID               PIC X(005).                      
                   15  F1-TPO-SUB-ID           PIC X(004).                      
               10  F1-TPO-REFERENCE-NUMBER     PIC X(015).                      
               10  F1-REJECTION-MESSAGE-IND    PIC X(001).                      
                   88  CANT-ID-PROV-AS-TPO-PARTIC  VALUE '1'.                   
                   88  CANT-ID-PAYR-AS-TPO-PARTIC  VALUE '2'.                   
                   88  CANT-ID-INSR-AS-TPO-PARTIC  VALUE '3'.                   
                   88  PAYOR-NAME-IS-MISSING       VALUE '4'.                   
                   88  CERTIFICATION-INFO-MISSING  VALUE '5'.                   
                   88  NOT-ENOUGH-CLAIM-INFO       VALUE '6'.                   
                   88  VALID-REJECTION-INDICATOR   VALUE '1' THRU '6'.          
               10  F1-AUTHORIZATION-NUMBER     PIC X(020).                      
               10  F1-PRICING-METHODOLOGY      PIC X(002).                      
                   88 VALID-PRICING-METHOD      VALUE '00' THRU '14'.           
               10  F1-ALLOWED-AMOUNT           PIC 9(006)V99.                   
               10  F1-ALLOWED-AMOUNT-X REDEFINES F1-ALLOWED-AMOUNT              
                                               PIC X(008).                      
               10  F1-SAVINGS-AMOUNT           PIC 9(006)V99.                   
               10  F1-SAVINGS-AMOUNT-X REDEFINES F1-SAVINGS-AMOUNT              
                                               PIC X(008).                      
               10  F1-APPROVED-PROC-CD-QUAL    PIC X(002).                      
               10  F1-PROC-LINE-CHANGE-IND     PIC 9(001).                      
                   88 CHANGE-TO-PROCEDURE-CODE     VALUE 1.                     
                   88 CHANGE-TO-UNIT-OF-SERVICE    VALUE 2.                     
                   88 CHANGE-TO-PROCEDURE-UNITS    VALUE 3.                     
                   88 VALID-PROCEDURE-CHANGE-IND   VALUE 1 THRU 3.              
               10  F1-LINE-CHG-X  REDEFINES  F1-PROC-LINE-CHANGE-IND            
                                               PIC X(001).                      
               10  F1-APPROVED-PROC-CD         PIC X(005).                      
               10  F1-APPROVED-UNITS-B-M       PIC X(002).                      
               10  F1-APPROVED-UNITS           PIC 9(003)V9.                    
               10  F1-APPROVED-UNITS-X  REDEFINES                               
                   F1-APPROVED-UNITS.                                           
                   15  F1-APPROVED-UNITS-X3    PIC X(003).                      
                   15  FILLER                  PIC X.                           
               10  FILLER                      PIC X(050).                      
               10  F1-POLICY-COMP-CD           PIC X(002).                      
               10  F1-EXCEPTION-CD             PIC X(002).                      
               10  F1-PPD-FR-AMT               PIC 9(006)V99.                   
               10  F1-PPD-FR-AMT-X      REDEFINES                               
                   F1-PPD-FR-AMT               PIC X(008).                      
               10  F1-REP-APP-A-G-CD           PIC X(003).                      
               10  F1-REP-APP-A-G-AMT          PIC 9(006)V99.                   
               10  F1-REP-APP-A-G-AMT-X  REDEFINES                              
                   F1-REP-APP-A-G-AMT          PIC X(008).                      
               10  F1-ADJ-REP-CLAIM-NUM        PIC X(015).                      
               10  FILLER                      PIC X(006).                      
                                                                                
      **-------------------------------------------------------------*          
      * LINE LEVEL: MEDICARE SUPPLEMENT INFORMATION (F2)             *          
      *--------------------------------------------------------------*          
                                                                                
           05  PCDS-F2-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  F2-REC-SEQ-NUM.                                              
                   15  F2-REC-TYPE                PIC X(002).                   
                       88  F2-REC      VALUE 'F2'.                              
                   15  F2-SEQ-NUM                 PIC 9(002).                   
                   15  F2-SEQ-NUM-X REDEFINES F2-SEQ-NUM                        
                                                  PIC X(002).                   
               10  F2-PATIENT-CONTROL-NO          PIC X(017).                   
               10  F2-MEDICARE-INT-CNTL-NO        PIC X(017).                   
               10  F2-MEDICARE-CARRIER-NAME       PIC X(015).                   
               10  F2-MEDICARE-CARRIER-ID         PIC X(010).                   
               10  F2-MEDICARE-DEDUCT-AMT         PIC S9(006)V99.               
               10  F2-MEDICARE-DEDUCT-AMTX REDEFINES                            
                   F2-MEDICARE-DEDUCT-AMT   PIC X(008).                         
               10  F2-MEDICARE-DEDUCT-AMTU REDEFINES                            
                   F2-MEDICARE-DEDUCT-AMT   PIC 9(006)V99.                      
               10  F2-SUMMARY-DETAIL-QL           PIC X(001).                   
               10  F2-PROCEDURE-CODE              PIC X(005).                   
               10  F2-PROCEDURE-MOD-1             PIC X(002).                   
               10  F2-PROCEDURE-MOD-2             PIC X(002).                   
               10  F2-SERV-START-DATE.                                          
                   15  F2-SERV-START-DATE-MM      PIC 9(002).                   
                   15  F2-SERV-START-DATE-DD      PIC 9(002).                   
                   15  F2-SERV-START-DATE-YY      PIC 9(002).                   
               10  F2-SERV-START-DATE-X REDEFINES                               
                   F2-SERV-START-DATE   PIC X(006).                             
               10  F2-SERV-END-DATE.                                            
                   15  F2-SERV-END-DATE-MM        PIC 9(002).                   
                   15  F2-SERV-END-DATE-DD        PIC 9(002).                   
                   15  F2-SERV-END-DATE-YY        PIC 9(002).                   
               10  F2-SERV-END-DATE-X REDEFINES                                 
                   F2-SERV-END-DATE   PIC X(006).                               
               10  F2-SERV-UNITS                  PIC 9(003).                   
               10  F2-SUBMITTED-CHARGES           PIC S9(006)V99.               
               10  F2-SUBMITTED-CHARGESX REDEFINES                              
                   F2-SUBMITTED-CHARGES           PIC X(008).                   
               10  F2-SUBMITTED-CHARGESU REDEFINES                              
                   F2-SUBMITTED-CHARGES           PIC 9(006)V99.                
               10  F2-MEDICARE-ALLOWED-AMT        PIC S9(006)V99.               
               10  F2-MEDICARE-ALLOWED-AMTX REDEFINES                           
                   F2-MEDICARE-ALLOWED-AMT        PIC X(008).                   
               10  F2-MEDICARE-ALLOWED-AMTU REDEFINES                           
                   F2-MEDICARE-ALLOWED-AMT        PIC 9(006)V99.                
               10  F2-NON-COVER-REASON-CD1        PIC X(004).                   
               10  F2-NON-COVER-REASON-CD2        PIC X(004).                   
               10  F2-NON-COVER-REASON-CD3        PIC X(004).                   
               10  F2-AMOUNT-PAID-TO-PATNT        PIC S9(006)V99.               
               10  F2-AMOUNT-PAID-TO-PATNTX REDEFINES                           
                   F2-AMOUNT-PAID-TO-PATNT        PIC X(008).                   
               10  F2-AMOUNT-PAID-TO-PATNTU REDEFINES                           
                   F2-AMOUNT-PAID-TO-PATNT        PIC 9(006)V99.                
               10  F2-AMOUNT-PAID-TO-PROV         PIC S9(006)V99.               
               10  F2-AMOUNT-PAID-TO-PROVX REDEFINES                            
                   F2-AMOUNT-PAID-TO-PROV         PIC X(008).                   
               10  F2-AMOUNT-PAID-TO-PROVU REDEFINES                            
                   F2-AMOUNT-PAID-TO-PROV         PIC 9(006)V99.                
               10  FILLER                         PIC X(052).                   
                                                                                
      *--------------------------------------------------------------*          
      *       LINE LEVEL: DENTI-CAL INFORMATION (F3)                 *          
      *--------------------------------------------------------------*          
                                                                                
           05  PCDS-F3-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  F3-REC-SEQ-NUM.                                              
                   15  F3-REC-TYPE                PIC X(002).                   
                       88  F3-REC      VALUE 'F3'.                              
                   15  F3-SEQ-NUM                 PIC 9(002).                   
                   15  F3-SEQ-NUM-X REDEFINES F3-SEQ-NUM                        
                                                  PIC X(002).                   
               10  F3-PATIENT-CONTROL-NO          PIC X(017).                   
               10  F3-TOOTH-NUMBER                PIC X(002).                   
                   88  F3-VALID-TOOTH-NUMBER   VALUE                            
                       'A ' 'B ' 'C ' 'D ' 'E ' 'F ' 'G ' 'H ' 'I '             
                       'J ' 'K ' 'L ' 'M ' 'N ' 'O ' 'P ' 'Q ' 'R '             
                       'S ' 'T ' 'U ' 'V ' 'W ' 'X ' 'Y ' 'Z '                  
                       '01'  THRU '40'.                                         
               10  F3-TOOTH-SURFACE               PIC X(005).                   
                   88  F3-VALID-TOOTH-SURFACE  VALUE                            
                       'B' 'D' 'F' 'G'                                          
                       'I' 'L' 'M' 'O'.                                         
               10  F3-ORAL-CAVITY-AREA            PIC  X(002).                  
                   88  VALID-F3-ORAL-CAVITY-AREA VALUE                          
                       'LA' 'LR' 'LL'                                           
                       'UA' 'UR' 'UL'.                                          
               10  F3-PROCEDURE-CODE              PIC X(005).                   
               10  F3-SVC-UNITS                   PIC 9(003).                   
               10  F3-SVC-UNITS-X REDEFINES                                     
                   F3-SVC-UNITS    PIC X(003).                                  
               10  F3-DATE-OF-SERVICE.                                          
                   15  F3-DATE-SVC-CC             PIC 9(002).                   
                   15  F3-DATE-SVC-YY             PIC 9(002).                   
                   15  F3-DATE-SVC-MM             PIC 9(002).                   
                   15  F3-DATE-SVC-DD             PIC 9(002).                   
               10  F3-DATE-OF-SERVICE-X REDEFINES                               
                   F3-DATE-OF-SERVICE    PIC X(008).                            
               10  F3-DATE-PROSTH-ORDER.                                        
                   15  F3-DATE-PROSTH-ORD-MM      PIC 9(002).                   
                   15  F3-DATE-PROSTH-ORD-DD      PIC 9(002).                   
                   15  F3-DATE-PROSTH-ORD-YY      PIC 9(002).                   
               10  F3-DATE-PROSTH-ORDER-X REDEFINES                             
                   F3-DATE-PROSTH-ORDER    PIC X(006).                          
               10  F3-LINE-CHARGES                PIC 9(006)V99.                
               10  F3-LINE-CHARGES-X REDEFINES                                  
                   F3-LINE-CHARGES       PIC X(008).                            
               10  F3-SVC-DESCR                   PIC X(035).                   
               10  F3-REND-SVC-PROV-ID.                                         
                   15  F3-REND-SVC-PROV-1         PIC X(001).                   
                      88  F3-VALID-REND-SVC-PROV-1   VALUES                     
                          'A' 'D' 'N' 'O' 'R' 'X' 'Y'.                          
                      88  F3-VALID-REND-SVC-PROV-1A  VALUES                     
                          'A' 'D' 'N' 'O' 'R' 'X'.                              
                      88  F3-VALID-REND-SVC-PROV-1B  VALUE 'Y'.                 
                   15  F3-RSP-DENT-LIC-NUM        PIC X(005).                   
               10  F3-REND-PROV-ID-1-6 REDEFINES                                
                                        F3-REND-SVC-PROV-ID.                    
                   15  F3-REND-PROV-ID-1-5        PIC X(005).                   
                   15  F3-REND-PROV-ID-6          PIC X(001).                   
               10  FILLER                         PIC X(009).                   
               10  F3-SVC-LINE-COMMENTS           PIC X(035).                   
               10  FILLER                         PIC X(047).                   
                                                                                
      *--------------------------------------------------------------*          
      *       LINE LEVEL: SERVICE LINE REFERENCE NUMBERS (F4)        *          
      *--------------------------------------------------------------*          
                                                                                
           05  PCDS-F4-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  F4-REC-SEQ-NUM.                                              
                   15  F4-REC-TYPE                PIC X(002).                   
                       88  F4-REC      VALUE 'F4'.                              
                   15  F4-SEQ-NUM                 PIC 9(002).                   
                   15  F4-SEQ-NUM-X REDEFINES F4-SEQ-NUM                        
                                                  PIC X(002).                   
               10  F4-SUB-SEQ-NUM                 PIC 9(002).                   
               10  F4-PATIENT-CONTROL-NO          PIC X(017).                   
               10  FILLER                         PIC X(002).                   
               10  F4-SVC-LINE-REFERENCE     OCCURS 5 TIMES                     
                                   INDEXED BY F4-SVC-LINE-REF-INDEX.            
                   15  F4-SVC-LINE-REF-QUAL       PIC X(002).                   
                   15  F4-SVC-LINE-REF-NUM        PIC X(030).                   
               10  FILLER                         PIC X(007).                   
                                                                                
      *--------------------------------------------------------------*          
      *       LINE LEVEL: TOOTH STATUS INFORMATION (F5)              *          
      *--------------------------------------------------------------*          
                                                                                
           05  PCDS-F5-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  F5-REC-SEQ-NUM.                                              
                   15  F5-REC-TYPE                PIC X(002).                   
                       88  F5-REC      VALUE 'F5'.                              
                   15  F5-SEQ-NUM                 PIC 9(002).                   
                   15  F5-SEQ-NUM-X REDEFINES F5-SEQ-NUM                        
                                                  PIC X(002).                   
               10  F5-SUB-SEQ-NUM                 PIC 9(002).                   
               10  F5-PATIENT-CONTROL-NO          PIC X(017).                   
               10  FILLER                         PIC X(002).                   
               10  FILLER                         PIC X(005).                   
               10  FILLER                         PIC X(001).                   
               10  F5-TOOTH-STATUS-INFO  OCCURS 18 TIMES                        
                                         INDEXED BY F5-INX.                     
                   15  F5-TOOTH-NUMBER            PIC X(002).                   
                   15  F5-TOOTH-SURFACE-CODE      PIC X(005).                   
                   15  FILLER                     PIC X(001).                   
               10  FILLER                         PIC X(017).                   
      *****************************************************************         
      *** OLD MCDS CODES***********************************************         
      *************15  RTF3-TOOTH-NUMBER       PIC X(002).                      
      *            88  RTF3-VALID-TOOTH-NUMBER   VALUE                          
      *                'A ' 'B ' 'C '  'D '  'E '  'F ' 'G ' 'H ' 'I '          
      *                'J ' 'K '  'L '  'M '  'N ' 'O ' 'P ' 'Q ' 'R '          
      *                'S '  'T '  'U '  'V ' 'W ' 'X ' 'Y'  'Z '               
      *                '01'  THRU '40'.                                         
      *                                                                         
      *        10  RTF3-TOOTH-SURFACES.                                         
      *            15  RTF3-TOOTH-SURFACE  OCCURS 5                             
      *                                    PIC  X(001).                         
      *                88  RTF3-VALID-TOOTH-SURFACE                             
      *                                            VALUE                        
      *                                                'B' 'D' 'F' 'G'          
      *************************************************'I' 'L' 'M' 'O'.         
      *****************************************************************         
                                                                                
      *--------------------------------------------------------------*          
      *       LINE LEVEL: PROVIDER NAME INFORMATION (F6)             *          
      *--------------------------------------------------------------*          
                                                                                
           05  PCDS-F6-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  F6-REC-SEQ-NUM.                                              
                   15  F6-REC-TYPE                PIC X(002).                   
                       88  F6-REC      VALUE 'F6'.                              
                   15  F6-SEQ-NUM                 PIC 9(002).                   
                   15  F6-SEQ-NUM-X REDEFINES F6-SEQ-NUM                        
                                                  PIC X(002).                   
               10  F6-SUB-SEQ-NUM                 PIC 9(002).                   
               10  F6-PATIENT-CONTROL-NO          PIC X(017).                   
               10  F6-LL-SERV-PROV-NAME-QUAL      PIC X(002).                   
               10  F6-LL-SERV-PROV-NAME.                                        
                   15  F6-LL-SERV-PROV-LASTNAME.                                
                       20 F6-LL-SERV-PROV-LNAME   PIC X(001).                   
                       20 FILLER                  PIC X(035).                   
                   15  F6-LL-SERV-PROV-FIRSTNAME.                               
                       20 F6-LL-SERV-PROV-FNAME   PIC X(001).                   
                       20 FILLER                  PIC X(011).                   
               10  F6-ORG-TYPE-AND-ORG-NAME REDEFINES                           
                   F6-LL-SERV-PROV-NAME.                                        
                   15  F6-ORG-TYPE                PIC X(001).                   
                   15  F6-ORG-NAME                PIC X(035).                   
                   15  FILLER                     PIC X(012).                   
               10  F6-LL-SERV-PROV-MINIT          PIC X(001).                   
               10  F6-PROV-SUFFIX                 PIC X(010).                   
               10  F6-NAT-PROV-ID                 PIC X(015).                   
               10  F6-TAX-ID-NUM-QUAL             PIC X(001).                   
               10  F6-TAX-ID-NUM                  PIC X(009).                   
               10  F6-LL-SERV-PROV-SPECIALTY      PIC X(003).                   
               10  F6-LL-SERV-PROV-NET-ID         PIC X(015).                   
               10  F6-PROV-TAXONOMY-CODE          PIC X(011).                   
               10  F6-SERV-LN-ORD-PROV-CONT-NM    PIC X(035).                   
               10  F6-REFERRING-PROV-TELEPHONE    PIC 9(010).                   
               10  FILLER                         PIC X(009).                   
                                                                                
      *--------------------------------------------------------------*          
      *   LINE LEVEL: PROVIDER SECONDARY REFERENCE INFO (F7)         *          
      *--------------------------------------------------------------*          
                                                                                
           05  PCDS-F7-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  F7-REC-SEQ-NUM.                                              
                   15  F7-REC-TYPE                PIC X(002).                   
                       88  F7-REC      VALUE 'F7'.                              
                   15  F7-SEQ-NUM                 PIC 9(002).                   
                   15  F7-SEQ-NUM-X REDEFINES F7-SEQ-NUM                        
                                                  PIC X(002).                   
               10  F7-SUB-SEQ-NUM                 PIC 9(002).                   
               10  F7-PATIENT-CONTROL-NO          PIC X(017).                   
               10  F7-LL-PROV-NAME-QUAL           PIC X(002).                   
                                                                                
               10  F7-LL-PROV-SECONDARY-ID-INFO    OCCURS 5 TIMES               
                                                   INDEXED BY F7-INX.           
                   15  F7-LL-PROV-SEC-ID-REF-QUAL PIC X(002).                   
                   15  F7-LL-PROV-SEC-ID-REF-NUM  PIC X(015).                   
               10  F7-SERV-LN-ORD-PROV-TEL-EXT    PIC X(005).                   
               10  F7-LL-PROV-FAX-NUM             PIC X(010).                   
               10  F7-LL-PROV-EMAIL-ADD           PIC X(040).                   
               10  FILLER                         PIC X(027).                   
                                                                                
      *--------------------------------------------------------------*          
      *       LINE LEVEL: PROVIDER ADDRESS INFORMATION (F8)          *          
      *--------------------------------------------------------------*          
                                                                                
           05  PCDS-F8-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  F8-REC-SEQ-NUM.                                              
                   15  F8-REC-TYPE                PIC X(002).                   
                       88  F8-REC      VALUE 'F8'.                              
                   15  F8-SEQ-NUM                 PIC 9(002).                   
                   15  F8-SEQ-NUM-X REDEFINES F8-SEQ-NUM                        
                                                  PIC X(002).                   
               10  F8-SUB-SEQ-NUM                 PIC 9(002).                   
               10  F8-PATIENT-CONTROL-NO          PIC X(017).                   
               10  F8-LL-PROV-NAME-QUAL           PIC X(002).                   
               10  F8-LL-PROV-ADDITIONAL-NAME     PIC X(060).                   
               10  F8-LL-PROV-ADDRESS-LINE-1      PIC X(030).                   
               10  F8-LL-PROV-ADDRESS-LINE-2      PIC X(030).                   
               10  F8-LL-PROV-CITY                PIC X(020).                   
               10  F8-LL-PROV-STATE               PIC X(002).                   
               10  F8-LL-PROV-ZIP                 PIC X(009).                   
               10  F8-LL-PROV-COUNTRY-CODE        PIC X(003).                   
               10  FILLER                         PIC X(013).                   
                                                                                
      *--------------------------------------------------------------*          
      *       LINE LEVEL: PHCS REPRICING INFORMATION (F9)            *          
      *--------------------------------------------------------------*          
                                                                                
           05  PCDS-F9-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  F9-REC-SEQ-NUM.                                              
                   15  F9-REC-TYPE                PIC X(002).                   
                       88  F9-REC      VALUE 'F9'.                              
                   15  F9-SEQ-NUM                 PIC 9(002).                   
                   15  F9-SEQ-NUM-X REDEFINES F9-SEQ-NUM                        
                                                  PIC X(002).                   
               10  F9-PATIENT-CONTROL-NO          PIC X(017).                   
               10  FILLER                         PIC X(003).                   
               10  F9-LL-PHCS-CLAIM-ID            PIC X(013).                   
               10  F9-LL-PHCS-PLAN-TYPE-IND       PIC X(001).                   
               10  F9-LL-PART-PROV-IND            PIC X(001).                   
               10  F9-LL-PRE-CERT-IND             PIC X(001).                   
               10  F9-LL-BEN-RECOMMEND-IND        PIC X(001).                   
               10  F9-LL-TRANS-CODE-IND           PIC X(001).                   
               10  F9-LL-AUTH-REMARK-CODE         PIC X(002).                   
               10  F9-LL-CLAIM-MESSAGE-CODE       PIC X(002).                   
               10  F9-LL-MESSAGE-TEXT             PIC X(055).                   
               10  F9-LL-REFERRING-PROV-ID        PIC X(012).                   
               10  F9-LL-REFERRING-PROV-TAX-ID    PIC X(012).                   
               10  F9-LL-REFERRING-PROV-LNAME     PIC X(015).                   
               10  F9-LL-REFERRING-PROV-FNAME     PIC X(010).                   
               10  F9-LL-REFERRING-PROV-MINIT     PIC X(001).                   
               10  F9-LL-ALLOWED-AMOUNT           PIC S9(007)V99.               
               10  F9-LL-ALLOWED-AMOUNTX       REDEFINES                        
                   F9-LL-ALLOWED-AMOUNT.                                        
                   15 FILLER                      PIC X(008).                   
                   15 F9-LL-ALLOW-AMT-LBYTE       PIC X(001).                   
               10  F9-LL-BILLED-AMOUNT            PIC S9(007)V99.               
               10  F9-LL-BILLED-AMOUNTX        REDEFINES                        
                   F9-LL-BILLED-AMOUNT.                                         
                   15 FILLER                      PIC X(008).                   
                   15 F9-LL-BILL-AMT-LBYTE        PIC X(001).                   
               10  F9-LL-TTL-ALLOWED-AMT          PIC S9(007)V99.               
               10  F9-LL-TTL-ALLOWED-AMTX      REDEFINES                        
                   F9-LL-TTL-ALLOWED-AMT.                                       
                   15 FILLER                      PIC X(008).                   
                   15 F9-LL-TTL-ALLOW-AMT-LBYTE   PIC X(001).                   
               10  F9-LL-TTL-ESCROW               PIC S9(005)V99.               
               10  F9-LL-TTL-ESCROWX           REDEFINES                        
                   F9-LL-TTL-ESCROW.                                            
                   15 FILLER                      PIC X(006).                   
                   15 F9-LL-TTL-ESCROW-LBYTE      PIC X(001).                   
               10  F9-LL-PRV-PENALTY              PIC S9(005)V99.               
               10  F9-LL-PRV-PENALTYX          REDEFINES                        
                   F9-LL-PRV-PENALTY.                                           
                   15 FILLER                      PIC X(006).                   
                   15 F9-LL-PRV-PENALTY-LBYTE     PIC X(001).                   
                                                                                
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTFA)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-FA-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  FA-REC-SEQ-NUM.                                              
                   15  FA-REC-TYPE         PIC X(002).                          
                       88  FA-REC                          VALUE 'FA'.          
                   15  FA-SEQ-NUM          PIC 9(002).                          
                   15  FA-SEQ-NUM-X REDEFINES FA-SEQ-NUM                        
                                           PIC X(002).                          
               10  FA-PAT-CNTL-NUM.                                             
                   15 FA-PCN-01-17         PIC X(017).                          
                   15 FA-PCN-18-20         PIC X(003).                          
               10  FA-EMERG-IND            PIC X(001).                          
               10  FA-COB-IND              PIC X(001).                          
               10  FA-HPSA-IND             PIC X(001).                          
               10  FA-PURCH-SVC-IND        PIC X(001).                          
               10  FA-RVW-BY-CD-IND        PIC X(001).                          
               10  FA-MULT-PROC-IND        PIC X(001).                          
               10  FA-MAMM-CERT-NUM        PIC X(010).                          
               10  FA-CLASS-FIND           PIC X(009).                          
               10  FA-POD-SVC-COND         PIC X(003).                          
               10  FA-CLIA-NUM             PIC X(015).                          
               10  FA-POD-THER-IND         PIC X(001).                          
               10  FA-POD-THER-TYPE        PIC X(001).                          
               10  FA-HOSP-EMP-PROV-IND    PIC X(001).                          
               10  FA-HGB-HCT-DATE.                                             
                   15 FA-HH-CC             PIC X(002).                          
                   15 FA-HH-YY             PIC X(002).                          
                   15 FA-HH-MM             PIC X(002).                          
                   15 FA-HH-DD             PIC X(002).                          
               10  FILLER                  PIC X(003).                          
               10  FA-SERUM-CREAT-DATE.                                         
                   15 FA-SC-CC             PIC X(002).                          
                   15 FA-SC-YY             PIC X(002).                          
                   15 FA-SC-MM             PIC X(002).                          
                   15 FA-SC-DD             PIC X(002).                          
               10  FA-OBLIG-ACC-AMT        PIC 9(005)V99.                       
               10  FA-OBLIG-ACC-AMT-X  REDEFINES                                
                   FA-OBLIG-ACC-AMT        PIC X(007).                          
               10  FA-DRUG-DISC-AMT        PIC 9(005)V99.                       
               10  FA-DRUG-DISC-AMT-X  REDEFINES                                
                   FA-DRUG-DISC-AMT        PIC X(007).                          
               10  FA-TYPE-UNIT-IND        PIC X(001).                          
               10  FA-APPROV-AMT           PIC 9(005)V99.                       
               10  FA-APPROV-AMT-X     REDEFINES                                
                   FA-APPROV-AMT           PIC X(007).                          
               10  FA-PAID-AMT             PIC 9(005)V99.                       
               10  FA-PAID-AMT-X       REDEFINES                                
                   FA-PAID-AMT             PIC X(007).                          
               10  FA-BENEFIT-LIAB         PIC 9(007).                          
               10  FA-BAL-BILL-LMT-CHRG    PIC 9(005)V99.                       
               10  FA-BAL-BILL-LMT-CHRG-X REDEFINES                             
                   FA-BAL-BILL-LMT-CHRG    PIC X(007).                          
               10  FA-LIMIT-CHRG-PCT       PIC 9(007).                          
               10  FA-PRETRANS-IND         PIC X(001).                          
               10  FA-PROD-CD-QUAL         PIC X(002).                          
               10  FA-UNIV-PROD-NUM        PIC X(014).                          
               10  FA-DIAG-CD-PTR-5        PIC X(001).                          
               10  FA-DIAG-CD-PTR-6        PIC X(001).                          
               10  FA-DIAG-CD-PTR-7        PIC X(001).                          
               10  FA-DIAG-CD-PTR-8        PIC X(001).                          
               10  FILLER                  PIC X(032).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTFB)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-FB-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  FB-REC-SEQ-NUM.                                              
                   15  FB-REC-TYPE         PIC X(002).                          
                       88  FB-REC                          VALUE 'FB'.          
                   15  FB-SEQ-NUM          PIC 9(002).                          
                   15  FB-SEQ-NUM-X REDEFINES FB-SEQ-NUM                        
                                           PIC X(002).                          
               10  FB-PAT-CNTL-NUM.                                             
                   15 FB-PCN-01-17         PIC X(017).                          
                   15 FB-PCN-18-20         PIC X(003).                          
               10  FB-PURCH-SVC-CHRGS      PIC 9(005)V99.                       
               10  FB-PURCH-SVC-CHRGS-X  REDEFINES                              
                   FB-PURCH-SVC-CHRGS      PIC X(007).                          
               10  FB-ALLOW-AMT            PIC 9(005)V99.                       
               10  FB-ALLOW-AMT-X        REDEFINES                              
                   FB-ALLOW-AMT            PIC X(007).                          
               10  FB-DISALLOW-AMT         PIC 9(005)V99.                       
               10  FB-DISALLOW-AMT-X     REDEFINES                              
                   FB-DISALLOW-AMT         PIC X(007).                          
                                                                                
               10  FILLER                  PIC X(007).                          
               10  FB-PEN-GRAMS-PROTEIN    PIC 9(004).                          
               10  FB-PEN-GRAMS-PROTEIN-X REDEFINES                             
                   FB-PEN-GRAMS-PROTEIN    PIC X(004).                          
               10  FB-PEN-CALORIES         PIC 9(004).                          
               10  FB-PEN-CALORIES-X      REDEFINES                             
                   FB-PEN-CALORIES         PIC X(004).                          
               10  FB-NAT-DRUG-UNITS       PIC 9(007).                          
               10  FB-NAT-DRUG-UNITS-X    REDEFINES                             
                   FB-NAT-DRUG-UNITS       PIC X(007).                          
               10  FB-RX-DATE.                                                  
                   15 FB-RX-CC             PIC X(002).                          
                   15 FB-RX-YY             PIC X(002).                          
                   15 FB-RX-MM             PIC X(002).                          
                   15 FB-RX-DD             PIC X(002).                          
               10  FB-RX-NO-MONTHS         PIC 9(002).                          
               10  FB-SPEC-PRICE-IND       PIC X(001).                          
               10  FB-COPAY-STAT-IND       PIC X(001).                          
               10  FB-EPSDT-IND            PIC X(001).                          
               10  FB-FAMILY-PLAN-IND      PIC X(001).                          
               10  FB-DME-CHRG-IND         PIC X(001).                          
               10  FB-HPSA-FAC-ID          PIC X(015).                          
               10  FB-HPSA-FAC-ZIP         PIC X(009).                          
               10  FB-DRUG-DAYS-SUPP       PIC 9(003).                          
               10  FB-DRUG-DAYS-SUPP-X  REDEFINES                               
                   FB-DRUG-DAYS-SUPP       PIC X(003).                          
               10  FB-PAYMT-TYPE-IND       PIC X(001).                          
               10  FILLER                  PIC X(082).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTFD)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-FD-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  FD-REC-SEQ-NUM.                                              
                   15  FD-REC-TYPE         PIC X(002).                          
                       88  FD-REC                          VALUE 'FD'.          
                   15  FD-SEQ-NUM          PIC 9(002).                          
                   15  FD-SEQ-NUM-X REDEFINES FD-SEQ-NUM                        
                                           PIC X(002).                          
               10  FD-PAT-CNTL-NUM.                                             
                   15 FD-PCN-01-17         PIC X(017).                          
                   15 FD-PCN-18-20         PIC X(003).                          
               10  FD-INIT-PLACE-IND       PIC X(001).                          
               10  FD-IMP-PRESCRIP-DATE.                                        
                   15 FD-IP-CC             PIC X(002).                          
                   15 FD-IP-YY             PIC X(002).                          
                   15 FD-IP-MM             PIC X(002).                          
                   15 FD-IP-DD             PIC X(002).                          
               10  FD-REPLACE-REASON       PIC X(001).                          
               10  FD-ORTHO-TRMT-IND       PIC X(001).                          
               10  FD-TRMT-LENGTH          PIC X(002).                          
               10  FD-APP-INSERT-DATE.                                          
                   15 FD-AI-CC             PIC X(002).                          
                   15 FD-AI-YY             PIC X(002).                          
                   15 FD-AI-MM             PIC X(002).                          
                   15 FD-AI-DD             PIC X(002).                          
               10  FD-APP-REMOVE-DATE.                                          
                   15 FD-AREM-CC           PIC X(002).                          
                   15 FD-AREM-YY           PIC X(002).                          
                   15 FD-AREM-MM           PIC X(002).                          
                   15 FD-AREM-DD           PIC X(002).                          
               10  FILLER                  PIC X(010).                          
               10  FD-APP-REPLACE-DATE.                                         
                   15 FD-AREP-CC           PIC X(002).                          
                   15 FD-AREP-YY           PIC X(002).                          
                   15 FD-AREP-MM           PIC X(002).                          
                   15 FD-AREP-DD           PIC X(002).                          
               10  FD-MON-TRMT-REMAIN      PIC X(002).                          
               10  FD-1ST-VISIT-DATE.                                           
                   15 FD-1V-CC             PIC X(002).                          
                   15 FD-1V-YY             PIC X(002).                          
                   15 FD-1V-MM             PIC X(002).                          
                   15 FD-1V-DD             PIC X(002).                          
               10  FILLER                  PIC X(010).                          
               10  FD-TOOTH-POCKET-MEAS    PIC X(002).                          
               10  FILLER                  PIC X(099).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTFK)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-FK-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  FK-REC-SEQ-NUM.                                              
                   15  FK-REC-TYPE         PIC X(002).                          
                       88  FK-REC                          VALUE 'FK'.          
                   15  FK-SEQ-NUM          PIC 9(002).                          
                   15  FK-SEQ-NUM-X     REDEFINES                               
                       FK-SEQ-NUM          PIC X(002).                          
                   15  FK-SUB-SEQ-NUM      PIC 9(002).                          
                   15  FK-SUB-SEQ-NUM-X REDEFINES                               
                       FK-SUB-SEQ-NUM      PIC X(002).                          
               10  FK-PAT-CNTL-NUM.                                             
                   15 FK-PCN-01-17         PIC X(017).                          
                   15 FK-PCN-18-20         PIC X(003).                          
               10  FK-STATE-DATA-REQ-1     PIC X(080).                          
               10  FK-STATE-DATA-REQ-2     PIC X(080).                          
               10  FILLER                  PIC X(006).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTFN)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-FN-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  FN-REC-SEQ-NUM.                                              
                   15  FN-REC-TYPE         PIC X(002).                          
                       88  FN-REC                          VALUE 'FN'.          
                   15  FN-SEQ-NUM          PIC 9(002).                          
                   15  FN-SEQ-NUM-X     REDEFINES                               
                       FN-SEQ-NUM          PIC X(002).                          
               10  FN-PAT-CNTL-NUM.                                             
                   15 FN-PCN-01-17         PIC X(017).                          
                   15 FN-PCN-18-20         PIC X(003).                          
               10  FN-REFER-DATE.                                               
                   15 FN-REF-CC            PIC X(002).                          
                   15 FN-REF-YY            PIC X(002).                          
                   15 FN-REF-MM            PIC X(002).                          
                   15 FN-REF-DD            PIC X(002).                          
               10  FN-ORDER-DATE.                                               
                   15 FN-ORD-CC            PIC X(002).                          
                   15 FN-ORD-YY            PIC X(002).                          
                   15 FN-ORD-MM            PIC X(002).                          
                   15 FN-ORD-DD            PIC X(002).                          
               10  FN-LAST-SEEN-DATE-1.                                         
                   15 FN-LS1-CC            PIC X(002).                          
                   15 FN-LS1-YY            PIC X(002).                          
                   15 FN-LS1-MM            PIC X(002).                          
                   15 FN-LS1-DD            PIC X(002).                          
               10  FN-LAST-SEEN-DATE-2.                                         
                   15 FN-LS2-CC            PIC X(002).                          
                   15 FN-LS2-YY            PIC X(002).                          
                   15 FN-LS2-MM            PIC X(002).                          
                   15 FN-LS2-DD            PIC X(002).                          
               10  FILLER                  PIC X(008).                          
               10  FN-SHIP-DATE.                                                
                   15 FN-SD-CC             PIC X(002).                          
                   15 FN-SD-YY             PIC X(002).                          
                   15 FN-SD-MM             PIC X(002).                          
                   15 FN-SD-DD             PIC X(002).                          
               10  FN-ONSET-DATE.                                               
                   15 FN-OD-CC             PIC X(002).                          
                   15 FN-OD-YY             PIC X(002).                          
                   15 FN-OD-MM             PIC X(002).                          
                   15 FN-OD-DD             PIC X(002).                          
               10  FN-SAME-SIM-ILL-DATE.                                        
                   15 FN-SS-CC             PIC X(002).                          
                   15 FN-SS-YY             PIC X(002).                          
                   15 FN-SS-MM             PIC X(002).                          
                   15 FN-SS-DD             PIC X(002).                          
               10  FN-ANESTHESIA-MOD-UNITS   OCCURS 5 TIMES                     
                                             INDEXED BY FN-INX.                 
                   15 FN-AMU-QUAL          PIC X(002).                          
                   15 FN-AMU               PIC X(002).                          
               10  FN-REF-CLIA-NUM         PIC X(014).                          
               10  FN-IMMUN-BATCH-NUM      PIC X(030).                          
               10  FN-AMB-PAT-GROUP-1      PIC X(003).                          
               10  FN-AMB-PAT-GROUP-2      PIC X(003).                          
               10  FN-AMB-PAT-GROUP-3      PIC X(003).                          
               10  FN-AMB-PAT-GROUP-4      PIC X(003).                          
               10  FN-POSTAGE-AMT          PIC 9(005)V99.                       
               10  FN-POSTAGE-AMT-X    REDEFINES                                
                   FN-POSTAGE-AMT          PIC X(007).                          
               10  FILLER                  PIC X(021).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTFP)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-FP-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  FP-REC-SEQ-NUM.                                              
                   15  FP-REC-TYPE         PIC X(002).                          
                       88  FP-REC                          VALUE 'FP'.          
                   15  FP-SEQ-NUM          PIC 9(002).                          
                   15  FP-SEQ-NUM-X     REDEFINES                               
                       FP-SEQ-NUM          PIC X(002).                          
               10  FP-PAT-CNTL-NUM.                                             
                   15 FP-PCN-01-17         PIC X(017).                          
                   15 FP-PCN-18-20         PIC X(003).                          
               10  FP-RLI-REF-NUM          PIC X(020).                          
               10  FP-ARLI-REF-NUM         PIC X(020).                          
               10  FP-CONTR-TYPE-CD        PIC X(002).                          
               10  FP-CONTR-AMT            PIC 9(016)V99.                       
               10  FP-CONTR-AMT-X       REDEFINES                               
                   FP-CONTR-AMT            PIC X(018).                          
               10  FP-CONTR-ALLOW          PIC X(006).                          
               10  FP-CONTR-CD             PIC X(030).                          
               10  FP-CONTR-TERM-DISC-PCT  PIC X(006).                          
               10  FP-CONTR-VERSION        PIC X(030).                          
               10  FP-CONTR-PREDET-NUM     PIC X(030).                          
               10  FILLER                  PIC X(006).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTG0)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-G0-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  G0-REC-SEQ-NUM.                                              
                   15  G0-REC-TYPE         PIC X(002).                          
                       88  G0-REC                          VALUE 'G0'.          
                   15  G0-SEQ-NUM          PIC 9(002).                          
                   15  G0-SEQ-NUM-X REDEFINES G0-SEQ-NUM                        
                                           PIC X(002).                          
               10  G0-PAT-CNTL-NUM         PIC X(017).                          
               10  FILLER                  PIC X(017).                          
               10  G0-LENGTH-MED-NEC       PIC 9(003).                          
               10  G0-LENGTH-MED-NEC-X     REDEFINES                            
                   G0-LENGTH-MED-NEC       PIC X(003).                          
               10  G0-DME-PURCH-PRICE      PIC S9(004)V99.                      
               10  G0-DME-PURCH-PRICE-X    REDEFINES                            
                   G0-DME-PURCH-PRICE.                                          
                   15 FILLER                  PIC X(005).                       
                   15 G0-PURCH-PR-LAST-BYTE   PIC X(001).                       
               10  G0-DME-PURCH-PRICE-U    REDEFINES                            
                   G0-DME-PURCH-PRICE      PIC  9(004)V99.                      
               10  G0-PURCH-EQUIP-IND      PIC X(001).                          
                   88  G0-VALID-PURCH-EQUIP-IND                                 
                       VALUES 'N' 'U'.                                          
               10  G0-DME-RENTAL-UNIT-PRICE                                     
                                           PIC X(001).                          
                   88  G0-VALID-RENTAL-UNIT-PRICE                               
                       VALUES 'D' 'W' 'M'.                                      
               10  G0-DME-RENT-PRICE       PIC S9(004)V99.                      
               10  G0-DME-RENT-PRICE-X REDEFINES G0-DME-RENT-PRICE.             
                   15 FILLER               PIC X(005).                          
                   15 G0-RENT-PR-LAST-BYTE PIC X(001).                          
               10  G0-DME-RENT-PRICE-U REDEFINES                                
                   G0-DME-RENT-PRICE       PIC  9(004)V99.                      
               10  G0-COMMENTS             PIC X(100).                          
               10  G0-PROCEDURE-CODE.                                           
                   15  G0-PROC-CODE-POS-1  PIC X(001).                          
                       88  G0-STATE-SPECIFIC-PROC-CODE VALUES                   
                           'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J'              
                           'K' 'L' 'M' 'N' 'O' 'P' 'Q' 'R' 'S' 'T'              
                           'U' 'V' 'W' 'X' 'Y' 'Z'.                             
                   15  FILLER              PIC X(004).                          
               10  G0-PRESCRIP-MED-NEC-DOC-IND                                  
                                           PIC X(001).                          
                   88  G0-VALID-PRESCRIP-MED-NEC                                
                       VALUES 'N' 'Y'.                                          
               10  G0-EQUIP-WARRANTY-IND   PIC X(001).                          
                   88  G0-VALID-WARRANTY-MED-NEC                                
                       VALUES 'N' 'Y'.                                          
               10  G0-DME-CERT-TYPE-CD     PIC X(001).                          
               10  FILLER                  PIC X(029).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGA)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GA-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  GA-REC-SEQ-NUM.                                              
                   15  GA-REC-TYPE         PIC X(002).                          
                       88  GA-REC                          VALUE 'GA'.          
                   15  GA-SEQ-NUM          PIC 9(002).                          
                   15  GA-SEQ-NUM-X REDEFINES GA-SEQ-NUM                        
                                           PIC X(002).                          
               10  GA-PAT-CNTL-NUM.                                             
                   15  GA-PCN-01-17        PIC X(017).                          
                   15  GA-PCN-18-20        PIC X(003).                          
               10  GA-HOSP-ADMIT           PIC X(001).                          
               10  GA-BED-CONFINE-BEF      PIC X(001).                          
               10  GA-BED-CONFINE-AFT      PIC X(001).                          
               10  GA-MOVE-BY-STRETCHER    PIC X(001).                          
               10  GA-UNCON-SHOCK          PIC X(001).                          
               10  GA-EMERGENCY            PIC X(001).                          
               10  GA-RESTRAINTS           PIC X(001).                          
               10  GA-VIS-HEMOR            PIC X(001).                          
               10  GA-MED-NECESSARY        PIC X(001).                          
               10  GA-ORIGIN-INFO          PIC X(040).                          
               10  GA-DEST-INFO            PIC X(040).                          
               10  GA-PAT-DISCHARGED       PIC X(001).                          
               10  GA-PAT-ADMITTED         PIC X(001).                          
               10  GA-SVC-AVAILABLE        PIC X(001).                          
               10  FILLER                  PIC X(076).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGB)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GB-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  GB-REC-SEQ-NUM.                                              
                   15  GB-REC-TYPE         PIC X(002).                          
                       88  GB-REC                          VALUE 'GB'.          
                   15  GB-SEQ-NUM          PIC 9(002).                          
                   15  GB-SEQ-NUM-X REDEFINES GB-SEQ-NUM                        
                                           PIC X(002).                          
               10  GB-PAT-CNTL-NUM.                                             
                   15  GB-PCN-01-17        PIC X(017).                          
                   15  GB-PCN-18-20        PIC X(003).                          
               10  GB-PLACE-OF-SVC         PIC X(002).                          
               10  GB-REPLACE-ITEM         PIC X(001).                          
               10  FILLER                  PIC X(005).                          
               10  GB-HCPCS-MOD            PIC X(002).                          
               10  GB-WARRANTY-REP         PIC X(001).                          
               10  GB-WARRANTY-LEN         PIC X(002).                          
               10  GB-WARRANTY-TYP         PIC X(001).                          
               10  GB-DIAG-CD-1            PIC X(005).                          
               10  GB-DIAG-CD-2            PIC X(005).                          
               10  GB-DIAG-CD-3            PIC X(005).                          
               10  GB-DIAG-CD-4            PIC X(005).                          
               10  GB-PAT-HGT              PIC X(002).                          
               10  GB-PAT-WGT              PIC X(003).                          
               10  GB-LAST-EXAM-DATE.                                           
                   15 GB-LED-CC            PIC X(002).                          
                   15 GB-LED-YY            PIC X(002).                          
                   15 GB-LED-MM            PIC X(002).                          
                   15 GB-LED-DD            PIC X(002).                          
               10  GB-INITIAL-DATE.                                             
                   15 GB-ID-CC             PIC X(002).                          
                   15 GB-ID-YY             PIC X(002).                          
                   15 GB-ID-MM             PIC X(002).                          
                   15 GB-ID-DD             PIC X(002).                          
               10  GB-REV-RECERT-DATE.                                          
                   15 GB-RRD-CC            PIC X(002).                          
                   15 GB-RRD-YY            PIC X(002).                          
                   15 GB-RRD-MM            PIC X(002).                          
                   15 GB-RRD-DD            PIC X(002).                          
               10  GB-LENGTH-NEED          PIC X(002).                          
               10  GB-CERT-SIGN-DATE.                                           
                   15 GB-CSD-CC            PIC X(002).                          
                   15 GB-CSD-YY            PIC X(002).                          
                   15 GB-CSD-MM            PIC X(002).                          
                   15 GB-CSD-DD            PIC X(002).                          
               10  GB-ORD-PROV-PHONE       PIC X(010).                          
               10  GB-CERT-ON-FILE         PIC X(001).                          
               10  FILLER                  PIC X(084).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGC)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GC-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  GC-REC-SEQ-NUM.                                              
                   15  GC-REC-TYPE         PIC X(002).                          
                       88  GC-REC                          VALUE 'GC'.          
                   15  GC-SEQ-NUM          PIC 9(002).                          
                   15  GC-SEQ-NUM-X REDEFINES GC-SEQ-NUM                        
                                           PIC X(002).                          
               10  GC-PAT-CNTL-NUM.                                             
                   15  GC-PCN-01-17        PIC X(017).                          
                   15  GC-PCN-18-20        PIC X(003).                          
               10  FILLER                  PIC X(001).                          
               10  GC-OXYGEN-SYS           PIC X(001).                          
               10  GC-LENGTH-NEED          PIC X(002).                          
               10  GC-EQUIP-TYPE-1         PIC X(001).                          
               10  GC-EQUIP-TYPE-2         PIC X(001).                          
               10  GC-EQUIP-REASON         PIC X(064).                          
               10  GC-PRESCRIBE-FROM-DATE.                                      
                   15 GC-PFD-CC            PIC X(002).                          
                   15 GC-PFD-YY            PIC X(002).                          
                   15 GC-PFD-MM            PIC X(002).                          
                   15 GC-PFD-DD            PIC X(002).                          
               10  GC-PRESCRIBE-TO-DATE.                                        
                   15 GC-PTD-CC            PIC X(002).                          
                   15 GC-PTD-YY            PIC X(002).                          
                   15 GC-PTD-MM            PIC X(002).                          
                   15 GC-PTD-DD            PIC X(002).                          
               10  GC-PRESCRIBED-DATE.                                          
                   15 GC-PD-CC             PIC X(002).                          
                   15 GC-PD-YY             PIC X(002).                          
                   15 GC-PD-MM             PIC X(002).                          
                   15 GC-PD-DD             PIC X(002).                          
               10  GC-EVALUATED-DATE.                                           
                   15 GC-ED-CC             PIC X(002).                          
                   15 GC-ED-YY             PIC X(002).                          
                   15 GC-ED-MM             PIC X(002).                          
                   15 GC-ED-DD             PIC X(002).                          
               10  GC-FREQUENCY            PIC X(002).                          
               10  GC-DURATION             PIC X(002).                          
               10  GC-ABG-4LPM             PIC 9(003).                          
               10  GC-OXI-4LPM             PIC 9(003).                          
               10  FILLER                  PIC X(008).                          
               10  GC-INPAT-OUTPAT         PIC X(001).                          
               10  FILLER                  PIC X(047).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGD)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GD-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  GD-REC-SEQ-NUM.                                              
                   15  GD-REC-TYPE         PIC X(002).                          
                       88  GD-REC                          VALUE 'GD'.          
                   15  GD-SEQ-NUM          PIC 9(002).                          
                   15  GD-SEQ-NUM-X REDEFINES GD-SEQ-NUM                        
                                           PIC X(002).                          
               10  GD-PAT-CNTL-NUM.                                             
                   15  GD-PCN-01-17        PIC X(017).                          
                   15  GD-PCN-18-20        PIC X(003).                          
               10  GD-ENT-PERF-OXI-TEST    PIC X(033).                          
               10  FILLER                  PIC X(001).                          
               10  GD-PORT-OXY-FLOW-RATE   PIC X(003).                          
               10  GD-ORDER-PROV-ID        PIC X(015).                          
               10  GD-ORDER-PROV-PHONE     PIC X(010).                          
               10  GD-DIAG-CD-1            PIC X(005).                          
               10  GD-DIAG-CD-2            PIC X(005).                          
               10  GD-DIAG-CD-3            PIC X(005).                          
               10  GD-DIAG-CD-4            PIC X(005).                          
               10  GD-DELIVERY-SYS         PIC X(001).                          
               10  FILLER                  PIC X(085).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGP)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GP-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  GP-REC-SEQ-NUM.                                              
                   15  GP-REC-TYPE         PIC X(002).                          
                       88  GP-REC                          VALUE 'GP'.          
                   15  GP-SEQ-NUM          PIC 9(002).                          
                   15  GP-SEQ-NUM-X REDEFINES GP-SEQ-NUM                        
                                           PIC X(002).                          
               10  GP-PAT-CNTL-NUM.                                             
                   15  GP-PCN-01-17        PIC X(017).                          
                   15  GP-PCN-18-20        PIC X(003).                          
               10  GP-PAT-WGT-LBS          PIC X(004).                          
               10  GP-TRANSPORT-CD         PIC X(001).                          
               10  GP-TRANSPORT-REASON     PIC X(001).                          
               10  GP-TRANSPORT-DISTANCE   PIC X(004).                          
               10  GP-TRANSPORT-PURPOSE    PIC X(080).                          
               10  FILLER                  PIC X(078).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGQ)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GQ-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  GQ-REC-SEQ-NUM.                                              
                   15  GQ-REC-TYPE         PIC X(002).                          
                       88  GQ-REC                          VALUE 'GQ'.          
                   15  GQ-SEQ-NUM          PIC 9(002).                          
                   15  GQ-SEQ-NUM-X REDEFINES GQ-SEQ-NUM                        
                                           PIC X(002).                          
               10  GQ-PAT-CNTL-NUM.                                             
                   15  GQ-PCN-01-17        PIC X(017).                          
                   15  GQ-PCN-18-20        PIC X(003).                          
               10  GQ-CONDITION-INFO    OCCURS 3 TIMES                          
                                        INDEXED BY GQ-INX.                      
                   15  GQ-COND-CODE-CAT    PIC X(002).                          
                   15  GQ-CERT-COND-IND    PIC X(001).                          
                   15  GQ-CONDITION-CODES OCCURS 5 TIMES                        
                                          INDEXED BY GQ-CC-INX.                 
                       20 GQ-COND-CODE     PIC X(002).                          
                                                                                
               10  GQ-STRETCHER-PURP       PIC X(080).                          
               10  GQ-ATTACH-XMIT-CD       PIC X(002).                          
               10  GQ-CERT-REV-DATE.                                            
                   15 GQ-CERT-REV-CC       PIC X(002).                          
                   15 GQ-CERT-REV-YY       PIC X(002).                          
                   15 GQ-CERT-REV-MM       PIC X(002).                          
                   15 GQ-CERT-REV-DD       PIC X(002).                          
               10  GQ-BEGIN-THERAPY-DATE.                                       
                   15 GQ-BEG-THER-CC       PIC X(002).                          
                   15 GQ-BEG-THER-YY       PIC X(002).                          
                   15 GQ-BEG-THER-MM       PIC X(002).                          
                   15 GQ-BEG-THER-DD       PIC X(002).                          
               10  GQ-LAST-CERT-DATE.                                           
                   15 GQ-LAST-CERT-CC      PIC X(002).                          
                   15 GQ-LAST-CERT-YY      PIC X(002).                          
                   15 GQ-LAST-CERT-MM      PIC X(002).                          
                   15 GQ-LAST-CERT-DD      PIC X(002).                          
               10  GQ-DMERC-CERT           PIC X(001).                          
               10  GQ-DMERC-LENGTH         PIC 9(003).                          
               10  FILLER                  PIC X(019).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGR)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GR-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  GR-REC-SEQ-NUM.                                              
                   15  GR-REC-TYPE         PIC X(002).                          
                       88  GR-REC                          VALUE 'GR'.          
                   15  GR-SEQ-NUM          PIC 9(002).                          
                   15  GR-SEQ-NUM-X     REDEFINES                               
                       GR-SEQ-NUM          PIC X(002).                          
                   15  GR-SUB-SEQ-NUM      PIC 9(002).                          
                   15  GR-SUB-SEQ-NUM-X REDEFINES                               
                       GR-SUB-SEQ-NUM      PIC X(002).                          
               10  GR-PAT-CNTL-NUM.                                             
                   15  GR-PCN-01-17        PIC X(017).                          
                   15  GR-PCN-18-20        PIC X(003).                          
               10  GR-TRMT-SERIES-NUM      PIC X(007).                          
               10  GR-TRMT-CNT             PIC X(007).                          
               10  GR-SUBLUX-LEVEL-1       PIC X(003).                          
               10  GR-SUBLUX-LEVEL-2       PIC X(003).                          
               10  GR-TRMT-SERIES-PUBM     PIC X(003).                          
               10  GR-TRMT-SERIES-PRD      PIC X(005).                          
               10  GR-MO-TRMT-CNT          PIC X(003).                          
               10  GR-COMPLIC-IND          PIC X(001).                          
               10  GR-XRAY-AVAIL-IND       PIC X(001).                          
               10  GR-LAST-XRAY-DATE.                                           
                   15 GR-LX-CC             PIC X(002).                          
                   15 GR-LX-YY             PIC X(002).                          
                   15 GR-LX-MM             PIC X(002).                          
                   15 GR-LX-DD             PIC X(002).                          
               10  GR-INIT-TRMT-DATE.                                           
                   15 GR-IT-CC             PIC X(002).                          
                   15 GR-IT-YY             PIC X(002).                          
                   15 GR-IT-MM             PIC X(002).                          
                   15 GR-IT-DD             PIC X(002).                          
               10  GR-ACUTE-MANIF-DATE.                                         
                   15 GR-AM-CC             PIC X(002).                          
                   15 GR-AM-YY             PIC X(002).                          
                   15 GR-AM-MM             PIC X(002).                          
                   15 GR-AM-DD             PIC X(002).                          
               10  FILLER                  PIC X(109).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGS)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GS-RECORD   REDEFINES   PCDS-00-RECORD.                     
               10  GS-REC-SEQ-NUM.                                              
                   15  GS-REC-TYPE         PIC X(002).                          
                       88  GS-REC                          VALUE 'GS'.          
                   15  GS-SEQ-NUM          PIC 9(002).                          
                   15  GS-SEQ-NUM-X     REDEFINES                               
                       GS-SEQ-NUM          PIC X(002).                          
                   15  GS-SUB-SEQ-NUM      PIC 9(002).                          
                   15  GS-SUB-SEQ-NUM-X REDEFINES                               
                       GS-SUB-SEQ-NUM      PIC X(002).                          
               10  GS-PAT-CNTL-NUM.                                             
                   15  GS-PCN-01-17        PIC X(017).                          
                   15  GS-PCN-18-20        PIC X(003).                          
               10  GS-PAT-COND-CD          PIC X(001).                          
               10  GS-PAT-COND-DESC-1      PIC X(080).                          
               10  GS-PAT-COND-DESC-2      PIC X(080).                          
               10  FILLER                  PIC X(005).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGT)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GT-RECORD   REDEFINES   PCDS-00-RECORD.                     
               10  GT-REC-SEQ-NUM.                                              
                   15  GT-REC-TYPE         PIC X(002).                          
                       88  GT-REC                          VALUE 'GT'.          
                   15  GT-SEQ-NUM          PIC 9(002).                          
                   15  GT-SEQ-NUM-X     REDEFINES                               
                       GT-SEQ-NUM          PIC X(002).                          
               10  GT-PAT-CNTL-NUM.                                             
                   15  GT-PCN-01-17        PIC X(017).                          
                   15  GT-PCN-18-20        PIC X(003).                          
               10  GT-TRMT-PRD-CNT         PIC X(002).                          
               10  GT-ABG-QUAN             PIC X(003).                          
               10  GT-OXY-SAT-QUAN         PIC X(003).                          
               10  GT-OXY-TEST-COND-CD     PIC X(001).                          
               10  GT-OXY-TEST-FIND-1      PIC X(001).                          
               10  GT-OXY-TEST-FIND-2      PIC X(001).                          
               10  GT-OXY-TEST-FIND-3      PIC X(001).                          
               10  GT-CERT-TYPE-CD         PIC X(001).                          
               10  FILLER                  PIC X(011).                          
               10  GT-OXY-TEST-PERF-DATE.                                       
                   15 GT-OTP-CC            PIC X(002).                          
                   15 GT-OTP-YY            PIC X(002).                          
                   15 GT-OTP-MM            PIC X(002).                          
                   15 GT-OTP-DD            PIC X(002).                          
               10  GT-ABG-TEST-DATE.                                            
                   15 GT-ABG-CC            PIC X(002).                          
                   15 GT-ABG-YY            PIC X(002).                          
                   15 GT-ABG-MM            PIC X(002).                          
                   15 GT-ABG-DD            PIC X(002).                          
               10  GT-OXY-SAT-TEST-DATE.                                        
                   15 GT-OST-CC            PIC X(002).                          
                   15 GT-OST-YY            PIC X(002).                          
                   15 GT-OST-MM            PIC X(002).                          
                   15 GT-OST-DD            PIC X(002).                          
               10  GT-OXY-FLOW-RATE        PIC X(003).                          
               10  FILLER                  PIC X(117).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGU)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GU-RECORD   REDEFINES   PCDS-00-RECORD.                     
               10  GU-REC-SEQ-NUM.                                              
                   15  GU-REC-TYPE         PIC X(002).                          
                       88  GU-REC                          VALUE 'GU'.          
                   15  GU-SEQ-NUM          PIC 9(002).                          
                   15  GU-SEQ-NUM-X     REDEFINES                               
                       GU-SEQ-NUM          PIC X(002).                          
                   15  GU-SUB-SEQ-NUM      PIC 9(002).                          
                   15  GU-SUB-SEQ-NUM-X REDEFINES                               
                       GU-SUB-SEQ-NUM      PIC X(002).                          
               10  GU-PAT-CNTL-NUM.                                             
                   15  GU-PCN-01-17        PIC X(017).                          
                   15  GU-PCN-18-20        PIC X(003).                          
               10  GU-MEASUREMENT-INFO  OCCURS 5 TIMES                          
                                        INDEXED BY GU-INX.                      
                   15 GU-MSRMT-ID          PIC X(002).                          
                   15 GU-MSRMT-ID-QUAL     PIC X(003).                          
                   15 GU-TEST-RESULT       PIC X(020).                          
               10  GU-PAT-WGT              PIC X(004).                          
               10  FILLER                  PIC X(037).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGV)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GV-RECORD   REDEFINES   PCDS-00-RECORD.                     
               10  GV-REC-SEQ-NUM.                                              
                   15  GV-REC-TYPE          PIC X(002).                         
                       88  GV-REC                   VALUE 'GV'.                 
                   15  GV-SEQ-NUM           PIC 9(002).                         
                   15  GV-SEQ-NUM-X      REDEFINES                              
                       GV-SEQ-NUM           PIC X(002).                         
                   15  GV-SUB-SEQ-NUM       PIC 9(002).                         
                   15  GV-SUB-SEQ-NUM-X REDEFINES                               
                       GV-SUB-SEQ-NUM       PIC X(002).                         
               10  GV-PAT-CNTL-NUM.                                             
                   15  GV-PCN-01-17         PIC X(017).                         
                   15  GV-PCN-18-20         PIC X(003).                         
               10  GV-NDC-INFO          OCCURS 2 TIMES                          
                                        INDEXED BY GV-NDC-INX.                  
                   15  GV-RX-NUM                PIC X(030).                     
                   15  GV-NDC-CODE-QUAL         PIC X(002).                     
                   15  GV-NDC-CODE              PIC 9(011).                     
                   15  GV-NDC-CODE-X           REDEFINES                        
                       GV-NDC-CODE              PIC X(011).                     
                   15  GV-NDC-UNIT-PRICE        PIC 9(006)V99.                  
                   15  GV-NDC-UNIT-PRICE-X     REDEFINES                        
                       GV-NDC-UNIT-PRICE        PIC X(008).                     
                   15  GV-NDC-UNIT-QUAL         PIC X(002).                     
                   15  GV-NAT-DRUG-UNIT-CNT     PIC 9(006)V9.                   
                   15  GV-NAT-DRUG-UNIT-CNT-X REDEFINES                         
                       GV-NAT-DRUG-UNIT-CNT     PIC X(007).                     
               10  FILLER                       PIC X(046).                     
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGX)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GX-RECORD   REDEFINES   PCDS-00-RECORD.                     
               10  GX-REC-SEQ-NUM.                                              
                   15  GX-REC-TYPE         PIC X(002).                          
                       88  GX-REC                          VALUE 'GX'.          
                   15  GX-SEQ-NUM          PIC 9(002).                          
                   15  GX-SEQ-NUM-X     REDEFINES                               
                       GX-SEQ-NUM          PIC X(002).                          
               10  GX-PAT-CNTL-NUM.                                             
                   15  GX-PCN-01-17        PIC X(017).                          
                   15  GX-PCN-18-20        PIC X(003).                          
               10  FILLER                  PIC X(002).                          
               10  FILLER                  PIC X(002).                          
               10  FILLER                  PIC X(004).                          
               10  FILLER                  PIC X(004).                          
               10  GX-NUM-VISITS           PIC X(004).                          
               10  GX-UBM                  PIC X(002).                          
               10  GX-SAMP-SEL-MOD         PIC X(006).                          
               10  GX-TIME-PRD-QUAL        PIC X(002).                          
               10  GX-NUM-PRDS             PIC X(003).                          
               10  GX-SD-CAL-PTRN          PIC X(002).                          
               10  GX-SD-TIME-PTRN-CO      PIC X(001).                          
               10  FILLER                  PIC X(136).                          
                                                                                
      *---------------------------------------------------------------*         
      *                                      (RTGY)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-GY-RECORD   REDEFINES   PCDS-00-RECORD.                     
               10  GY-REC-SEQ-NUM.                                              
                   15  GY-REC-TYPE         PIC X(002).                          
                       88  GY-REC                          VALUE 'GY'.          
                   15  GY-SEQ-NUM          PIC 9(002).                          
                   15  GY-SEQ-NUM-X     REDEFINES                               
                       GY-SEQ-NUM          PIC X(002).                          
                   15  GY-SUB-SEQ-NUM      PIC 9(003).                          
                   15  GY-SUB-SEQ-NUM-X REDEFINES                               
                       GY-SUB-SEQ-NUM      PIC X(003).                          
               10  GY-PAT-CNTL-NUM.                                             
                   15  GY-PCN-01-17        PIC X(017).                          
                   15  GY-PCN-18-20        PIC X(003).                          
               10  GY-FORM-ID-QUAL         PIC X(002).                          
               10  GY-FORM-ID              PIC X(030).                          
               10  GY-QUES-NUM-LTR         PIC X(020).                          
               10  GY-QUES-RESP-IND        PIC X(001).                          
               10  GY-QUES-RESP-TEXT       PIC X(030).                          
               10  GY-QUES-RESP-DATE.                                           
                   15 GY-QRD-CC            PIC X(002).                          
                   15 GY-QRD-YY            PIC X(002).                          
                   15 GY-QRD-MM            PIC X(002).                          
                   15 GY-QRD-DD            PIC X(002).                          
               10  GY-QUES-RESP-PCNT       PIC X(006).                          
               10  FILLER                  PIC X(068).                          
                                                                                
      *----------------------------------------------------------------*        
      *                                      (RTH0)                    *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-H0-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  H0-REC-TYPE             PIC X(002).                          
                   88  H0-REC                              VALUE 'H0'.          
               10  H0-SEQ-NUM              PIC 9(002).                          
               10  H0-SUB-SEQ-NUM          PIC 9(002).                          
               10  H0-PAT-CNTL-NUM         PIC X(017).                          
               10  FILLER                  PIC X(003).                          
               10  H0-EX-NARR-DATA-QUAL-1  PIC X(003).                          
               10  H0-EX-NARR-DATA-1       PIC X(080).                          
               10  H0-EX-NARR-DATA-QUAL-2  PIC X(003).                          
               10  H0-EX-NARR-DATA-2       PIC X(080).                          
                                                                                
      *----------------------------------------------------------------*        
      *                                      (RTKP)                    *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-KP-RECORD  REDEFINES  PCDS-00-RECORD.                       
               10  KP-REC-TYPE             PIC X(002).                          
                   88  KP-REC                              VALUE 'KP'.          
               10  KP-SEQ-NUM              PIC 9(002).                          
               10  KP-SUB-SEQ-NUM          PIC 9(002).                          
               10  KP-PAT-CNTL-NUM.                                             
                   15  KP-PCN-01-17        PIC X(017).                          
                   15  KP-PCN-18-20        PIC X(003).                          
               10  KP-OTH-PAYER-ID-QUAL    PIC X(002).                          
               10  KP-OTH-PAYER-ID         PIC X(030).                          
               10  KP-OTH-PAYER-NAME       PIC X(035).                          
               10  KP-SL-APP-AMT           PIC 9(005)V99.                       
               10  FILLER                  PIC X(092).                          
                                                                                
      *----------------------------------------------------------------*        
      *                                      (RTKR)                    *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-KR-RECORD  REDEFINES  PCDS-00-RECORD.                       
               10  KR-REC-TYPE             PIC X(002).                          
                   88  KR-REC                              VALUE 'KR'.          
               10  KR-SEQ-NUM              PIC 9(002).                          
               10  KR-SUB-SEQ-NUM          PIC 9(002).                          
               10  KR-PAT-CNTL-NUM.                                             
                   15  KR-PCN-01-17        PIC X(017).                          
                   15  KR-PCN-18-20        PIC X(003).                          
               10  KR-OTH-PAYER-ID-QUAL    PIC X(002).                          
               10  KR-OTH-PAYER-ID         PIC X(030).                          
               10  KR-REF-NUM-1            PIC X(030).                          
               10  KR-PRIOR-AUTH-NUM-1     PIC X(030).                          
               10  KR-REF-NUM-2            PIC X(030).                          
               10  KR-PRIOR-AUTH-NUM-2     PIC X(030).                          
               10  FILLER                  PIC X(014).                          
                                                                                
      *----------------------------------------------------------------*        
      *                                      (RTKS)                    *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-KS-RECORD  REDEFINES  PCDS-00-RECORD.                       
               10  KS-REC-TYPE             PIC X(002).                          
                   88  KS-REC                              VALUE 'KS'.          
               10  KS-SEQ-NUM              PIC 9(002).                          
               10  KS-SUB-SEQ-NUM          PIC 9(002).                          
               10  KS-PAT-CNTL-NUM.                                             
                   15  KS-PCN-01-17        PIC X(017).                          
                   15  KS-PCN-18-20        PIC X(003).                          
               10  KS-OTH-PAYER-ID         PIC X(015).                          
               10  KS-SVC-LN-PD-AMT        PIC 9(006)V99.                       
               10  KS-SVC-LN-PD-AMT-X  REDEFINES                                
                   KS-SVC-LN-PD-AMT        PIC X(008).                          
               10  KS-PROC-CODE-QUAL       PIC X(002).                          
               10  KS-PROC-CODE            PIC X(005).                          
               10  KS-PROC-MOD-1           PIC X(002).                          
               10  KS-PROC-MOD-2           PIC X(002).                          
               10  KS-PROC-MOD-3           PIC X(002).                          
               10  KS-PROC-MOD-4           PIC X(002).                          
               10  KS-PROC-CODE-DESC       PIC X(080).                          
               10  KS-PAID-UOS             PIC 9(014)V9.                        
               10  KS-PAID-UOS-X     REDEFINES                                  
                   KS-PAID-UOS             PIC X(015).                          
               10  KS-BUN-UNBUN-LN-NO      PIC X(020).                          
               10  KS-ADJUD-PYMT-DATE.                                          
                   15  KS-ADJ-PD-CC        PIC X(002).                          
                   15  KS-ADJ-PD-YY        PIC X(002).                          
                   15  KS-ADJ-PD-MM        PIC X(002).                          
                   15  KS-ADJ-PD-DD        PIC X(002).                          
               10  FILLER                  PIC X(005).                          
                                                                                
      *----------------------------------------------------------------*        
      *                                      (RTKT)                    *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-KT-RECORD  REDEFINES  PCDS-00-RECORD.                       
               10  KT-REC-TYPE             PIC X(002).                          
                   88  KT-REC                              VALUE 'KT'.          
               10  KT-SEQ-NUM              PIC 9(002).                          
               10  KT-SUB-SEQ-NUM          PIC 9(002).                          
               10  KT-PAT-CNTL-NUM.                                             
                   15  KT-PCN-01-17        PIC X(017).                          
                   15  KT-PCN-18-20        PIC X(003).                          
               10  KT-CAS-GROUP-CD         PIC X(002).                          
               10  KT-CAS-CD-AMT-QTY   OCCURS 6 TIMES                           
                                       INDEXED BY KT-INX.                       
                   15 KT-CAS-CD            PIC X(005).                          
                   15 KT-CAS-AMT           PIC 9(006)V99.                       
                   15 KT-CAS-AMT-X      REDEFINES                               
                      KT-CAS-AMT           PIC X(008).                          
                   15 KT-CAS-QTY           PIC X(014).                          
               10  KT-CAS-REC-CTR          PIC 9(002).                          
               10  KT-CAS-REC-CTR-X     REDEFINES                               
                   KT-CAS-REC-CTR          PIC X(002).                          
                                                                                
      *----------------------------------------------------------------*        
0407JW*             EXTENDED CONTENT VALIDATOR  RECORD(KZ)             *        
0407JW*----------------------------------------------------------------*        
0407JW     05  KZ-RECORD    REDEFINES PCDS-00-RECORD.                           
0407JW         10  KZ-REC-TYPE                 PIC X(02).                       
0407JW             88  KZ-REC                      VALUE 'KZ'.                  
0407JW         10  KZ-REC-TYPE-SEQ             PIC 9(02).                       
0407JW         10  KZ-PAT-CNTL-NUM.                                             
0407JW             15  KZ-PCN-01-17            PIC X(17).                       
0407JW             15  FILLER                  PIC X(03).                       
0407JW                                                                          
0407JW         10  KZ-CORN.                                                     
0407JW             15  KZ-CORN-CH-ID           PIC X(02).                       
0407JW                 88  KZ-ENVOY-NEIC-P         VALUE 'EP'.                  
0407JW                 88  KZ-ENVOY-NEIC-HIPAA-P   VALUE 'HP'.                  
0407JW                 88  KZ-ENVOY-OKC            VALUE 'OK'.                  
0407JW                 88  KZ-ENVOY-PO             VALUE 'PO'.                  
0407JW             15  KZ-CORN-SYS-MMDDYY.                                      
0407JW                 20  KZ-CORN-SYS-MM      PIC 9(02).                       
0407JW                 20  KZ-CORN-SYS-DD      PIC 9(02).                       
0407JW                 20  KZ-CORN-SYS-YY      PIC 9(02).                       
0407JW             15  KZ-CORN-ID-SEQ.                                          
0407JW                 20  KZ-CORN-ID          PIC 9(01).                       
0407JW                     88  KZ-CORN-PCDS-GW     VALUE 9.                     
0407JW                     88  KZ-CORN-PCDS-DAK    VALUE 6.                     
0407JW                     88  KZ-CORN-MCDS-GW     VALUE 8.                     
0407JW                     88  KZ-CORN-MCDS-COML   VALUE 5.                     
0407JW                 20  KZ-CORN-SEQ         PIC 9(08).                       
0407JW         10  KZ-DESTINATION-ID           PIC X(01).                       
0407JW             88  KZ-SENT-TO-DAKOTA           VALUE 'D'.                   
0407JW             88  KZ-SENT-TO-OKC              VALUE 'O'.                   
0407JW             88  KZ-SENT-TO-POSI             VALUE 'P'.                   
0407JW             88  KZ-SENT-TO-XBILL            VALUE 'X'.                   
0407JW                                                                          
               10  KZ-ERROR-TABLE.                                              
                   15  KZ-ERROR-DATA  OCCURS 4 TIMES                            
                                      INDEXED BY KZ-TBL-INDEX.                  
                       20  KZ-TBL-ERR-CODE     PIC X(05).                       
                       20  KZ-TBL-ERR-RT       PIC X(03).                       
                       20  KZ-TBL-ERR-SEQ      PIC X(02).                       
                       20  KZ-TBL-ERR-FIELD    PIC X(02).                       
                       20  KZ-TBL-ERR-DATA     PIC X(20).                       
                       20  FILLER              PIC X(02).                       
               10  KZ-ERRORS-1-4  REDEFINES  KZ-ERROR-TABLE.                    
                   15 KZ-ERROR-CODE-1          PIC X(05).               27550000
                   15 KZ-ERR1-REC-TYPE         PIC X(03).               27550000
                   15 KZ-ERR1-SEQ              PIC X(02).               27550000
                   15 KZ-ERR1-FIELD            PIC X(02).               27550000
                   15 KZ-ERR1-DATA             PIC X(20).               27550000
                   15 FILLER                   PIC X(02).               27550000
                   15 KZ-ERROR-CODE-2          PIC X(05).               27550000
                   15 KZ-ERR2-REC-TYPE         PIC X(03).               27550000
                   15 KZ-ERR2-SEQ              PIC X(02).               27550000
                   15 KZ-ERR2-FIELD            PIC X(02).               27550000
                   15 KZ-ERR2-DATA             PIC X(20).               27550000
                   15 FILLER                   PIC X(02).               27550000
                   15 KZ-ERROR-CODE-3          PIC X(05).               27550000
                   15 KZ-ERR3-REC-TYPE         PIC X(03).               27550000
                   15 KZ-ERR3-SEQ              PIC X(02).               27550000
                   15 KZ-ERR3-FIELD            PIC X(02).               27550000
                   15 KZ-ERR3-DATA             PIC X(20).               27550000
                   15 FILLER                   PIC X(02).               27550000
                   15 KZ-ERROR-CODE-4          PIC X(05).               27550000
                   15 KZ-ERR4-REC-TYPE         PIC X(03).               27550000
                   15 KZ-ERR4-SEQ              PIC X(02).               27550000
                   15 KZ-ERR4-FIELD            PIC X(02).               27550000
                   15 KZ-ERR4-DATA             PIC X(20).               27550000
                   15 FILLER                   PIC X(02).               27550000
               10 FILLER                       PIC X(14).               27550000
                                                                                
      *----------------------------------------------------------------*        
      *          CLAIM TRAILER RECORD        (RTX0)                    *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-X0-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  X0-REC-TYPE             PIC X(002).                          
                   88  X0-REC                              VALUE 'X0'.          
               10  FILLER                  PIC X(002).                          
               10  X0-PAT-CNTL-NUM         PIC X(017).                          
               10  X0-RECORD-COUNTS.                                            
                   15  X0-PHYS-REC-CNT     PIC 9(006).                          
                   15  X0-RECNOCX          PIC 9(002).                          
                   15  X0-RECNODX          PIC 9(003).                          
                   15  X0-RECNOEX          PIC 9(003).                          
                   15  X0-RECNOFX          PIC 9(004).                          
                   15  X0-RECNOGX          PIC 9(005).                          
                   15  X0-RECNOHX          PIC 9(003).                          
                   15  X0-RECNOKX          PIC 9(006).                          
               10  X0-TOTALS.                                                   
                   15  X0-NUM-SERV-LN      PIC 9(003).                          
                                                                                
      *** THE FOLLOWING 10 BYTE FILLER IS NEW DUE TO RECORD COUNT               
      *** ADJUSTMENTS (TENTATIVE) MADE ON 7-17-2002.                            
                                                                                
                   15  FILLER              PIC X(010).                          
                                                                                
                   15  X0-CLAIM-TOT-CHRG   PIC S9(008)V99.                      
                   15  X0-CLAIM-TOT-CHRG-U REDEFINES                            
                       X0-CLAIM-TOT-CHRG   PIC 9(008)V99.                       
                                                                                
                   15  FILLER              PIC X(002).                          
                                                                                
                   15  X0-PAT-PAID         PIC S9(008)V99.                      
                   15  X0-PAT-PAID-U REDEFINES                                  
                       X0-PAT-PAID         PIC 9(008)V99.                       
      ****     10  FILLER                  PIC X(003).                          
               10  FILLER                  PIC X(009).                          
      ****     10  X0-REMARKS              PIC X(100).                          
               10  X0-REMARKS              PIC X(080).                          
               10  X0-WEBMD-ID.                                                 
                   15  X0-WEBMD-PROC-DATE.                                      
                       20  X0-WEBMD-PROC-MM  PIC X(002).                        
                       20  X0-WEBMD-PROC-DD  PIC X(002).                        
                       20  X0-WEBMD-PROC-YY  PIC X(002).                        
                   15  X0-WEBMD-SEQ-NO.                                         
                       20  X0-WEBMD-SYS-NO   PIC X(001).                        
                       20  X0-WEBMD-SEQ-NUM  PIC X(008).                        
                                                                                
           05  PCDS-X0-ALPHA     REDEFINES PCDS-00-RECORD.                      
               10  FILLER                  PIC X(021).                          
               10  X0-RECORD-COUNTS-X.                                          
                   15  X0-PHYS-REC-CNT-X   PIC X(006).                          
                   15  X0-RECNOCX-X        PIC X(002).                          
                   15  X0-RECNODX-X        PIC X(003).                          
                   15  X0-RECNOEX-X        PIC X(003).                          
                   15  X0-RECNOFX-X        PIC X(004).                          
                   15  X0-RECNOGX-X        PIC X(005).                          
                   15  X0-RECNOHX-X        PIC X(003).                          
                   15  X0-RECNOKX-X        PIC X(006).                          
               10  X0-TOTALS-X.                                                 
                   15  X0-NUM-SERV-LN-X    PIC X(003).                          
                                                                                
      *** THE FOLLOWING 10 BYTE FILLER IS NEW DUE TO RECORD COUNT               
      *** ADJUSTMENTS (TENTATIVE) MADE ON 7-17-2002.                            
                                                                                
                   15  FILLER              PIC X(010).                          
                                                                                
                   15  X0-CLAIM-TOT-CHRG-X.                                     
                       20 X0-TOT-CHRG-FIRST-BYTES    PIC X(009).                
                       20 X0-TOT-CHRG-LAST-BYTE      PIC X(001).                
                          88 X0-VALID-SIGN  VALUE 'A' 'B' 'C' 'D' 'E'           
                                                  'F' 'G' 'H' 'I' 'J'           
                                                  'K' 'L' 'M' 'N' 'O'           
                                                  'P' 'Q' 'R' '}' '{'.          
                   15  FILLER              PIC X(002).                          
                   15  X0-PAT-PAID-X.                                           
                       20 FILLER           PIC X(009).                          
                       20 X0-PAT-PAID-LAST-BYTE                                 
                                           PIC X(001).                          
               10  FILLER                  PIC X(104).                          
                                                                                
      *----------------------------------------------------------------*        
      *                                      (RTXA)                    *        
      *----------------------------------------------------------------*        
                                                                                
           05  PCDS-XA-RECORD REDEFINES PCDS-00-RECORD.                         
               10  XA-REC-TYPE             PIC X(002).                          
                   88  XA-REC        VALUE 'XA'.                                
               10  FILLER                  PIC X(002).                          
               10  XA-PAT-CNTL-NUM         PIC X(020).                          
                                                                                
               10  XA-DIS-COST-CONT        PIC 9(008)V99.                       
               10  XA-DIS-COST-CONT-X    REDEFINES                              
                   XA-DIS-COST-CONT        PIC X(010).                          
                                                                                
               10  XA-DIS-OTHR-CHRGS       PIC 9(008)V99.                       
               10  XA-DIS-OTHR-CHRGS-X   REDEFINES                              
                   XA-DIS-OTHR-CHRGS       PIC X(010).                          
                                                                                
               10  XA-ALLOW-AMT            PIC 9(008)V99.                       
               10  XA-ALLOW-AMT-X        REDEFINES                              
                   XA-ALLOW-AMT            PIC X(010).                          
                                                                                
               10  XA-DEDUC-AMT            PIC 9(008)V99.                       
               10  XA-DEDUC-AMT-X        REDEFINES                              
                   XA-DEDUC-AMT            PIC X(010).                          
                                                                                
               10  XA-COINS-AMT            PIC 9(008)V99.                       
               10  XA-COINS-AMT-X        REDEFINES                              
                   XA-COINS-AMT            PIC X(010).                          
                                                                                
               10  XA-PAYER-AMT            PIC 9(008)V99.                       
               10  XA-PAYER-AMT-X        REDEFINES                              
                   XA-PAYER-AMT            PIC X(010).                          
                                                                                
               10  XA-PURCH-SVC-CHRG       PIC 9(008)V99.                       
               10  XA-PURCH-SVC-CHRG-X   REDEFINES                              
                   XA-PURCH-SVC-CHRG       PIC X(010).                          
                                                                                
               10  XA-PROV-DISC-INFO       PIC X(016).                          
               10  FILLER                  PIC X(082).                          
                                                                                
      *---------------------------------------------------------------*         
      *             PROVIDER BATCH CONTROL   (RTY0)                   *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-Y0-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  Y0-REC-TYPE             PIC X(002).                          
                   88  Y0-REC                              VALUE 'Y0'.          
               10  Y0-FED-TAX-NUM          PIC 9(009).                          
               10  Y0-FED-TAX-NUM-X REDEFINES Y0-FED-TAX-NUM                    
                                           PIC X(009).                          
               10  Y0-RECEIVER.                                                 
                   15  Y0-REC-ID           PIC 9(005).                          
                   15  Y0-REC-ID-X REDEFINES Y0-REC-ID                          
                                           PIC X(005).                          
                   15  Y0-REC-SUB-ID       PIC X(004).                          
               10  Y0-BATCH-TYPE           PIC X(003).                          
               10  Y0-EMC-PROV-ID          PIC X(015).                          
               10  Y0-BATCH-NO             PIC 9(004).                          
               10  Y0-BATCH-ID             PIC X(006).                          
               10  FILLER                  PIC X(006).                          
                                                                                
               10  Y0-NUM-BATCH-SVC-LINES  PIC 9(007).                          
               10  Y0-NUM-BATCH-SVC-LINES-X  REDEFINES                          
                   Y0-NUM-BATCH-SVC-LINES  PIC X(007).                          
                                                                                
               10  Y0-NUM-BATCH-RECS       PIC 9(007).                          
               10  Y0-NUM-BATCH-RECS-X   REDEFINES                              
                   Y0-NUM-BATCH-RECS       PIC X(007).                          
                                                                                
               10  Y0-NUM-BATCH-CLAIMS     PIC 9(007).                          
               10  Y0-NUM-BATCH-CLAIMS-X REDEFINES                              
                   Y0-NUM-BATCH-CLAIMS     PIC X(007).                          
                                                                                
               10  Y0-BATCH-TOTAL-CHARGES  PIC S9(010)V99.                      
               10  Y0-BATCH-TOTAL-CHARGES-U    REDEFINES                        
                   Y0-BATCH-TOTAL-CHARGES  PIC 9(010)V99.                       
               10  Y0-BATCH-TOTAL-CHARGES-X    REDEFINES                        
                   Y0-BATCH-TOTAL-CHARGES.                                      
                   15 FILLER                PIC X(011).                         
                   15 Y0-TOT-CHRG-LAST-BYTE PIC X(001).                         
                                                                                
               10  FILLER                  PIC X(096).                          
               10  Y0-ORIG-SUB-ID          PIC 9(009).                          
                                                                                
      *---------------------------------------------------------------*         
      *              TAPE CONTROL    (RTZ0)                           *         
      *---------------------------------------------------------------*         
                                                                                
           05  PCDS-Z0-RECORD  REDEFINES PCDS-00-RECORD.                        
               10  Z0-REC-TYPE             PIC X(002).                          
                   88  Z0-REC                              VALUE 'Z0'.          
               10  Z0-SUB-EIN              PIC 9(009).                          
               10  Z0-RECEIVER.                                                 
                   15  Z0-REC-ID           PIC 9(005).                          
                   15  Z0-REC-ID-X     REDEFINES                                
                       Z0-REC-ID           PIC X(005).                          
                   15  Z0-REC-SUB-ID       PIC X(004).                          
                                                                                
               10  Z0-BATCH-CNT            PIC 9(004).                          
               10  Z0-BATCH-CNT-X        REDEFINES                              
                   Z0-BATCH-CNT            PIC X(004).                          
                                                                                
               10  Z0-SVC-LINE-CNT         PIC 9(007).                          
               10  Z0-SVC-LINE-CNT-X     REDEFINES                              
                   Z0-SVC-LINE-CNT         PIC X(007).                          
                                                                                
               10  Z0-REC-CNT              PIC 9(007).                          
               10  Z0-REC-CNT-X          REDEFINES                              
                   Z0-REC-CNT              PIC X(007).                          
                                                                                
               10  Z0-CLAIM-CNT            PIC 9(007).                          
               10  Z0-CLAIM-CNT-X       REDEFINES                               
                   Z0-CLAIM-CNT            PIC X(007).                          
                                                                                
               10  Z0-PAID-AMT             PIC S9(013)V99.                      
               10  Z0-PAID-AMT-U        REDEFINES                               
                   Z0-PAID-AMT             PIC  9(013)V99.                      
               10  Z0-PAID-AMT-X        REDEFINES                               
                   Z0-PAID-AMT             PIC X(015).                          
                                                                                
               10  Z0-ALLOWED-AMT          PIC S9(013)V99.                      
               10  Z0-ALLOWED-AMT-U     REDEFINES                               
                   Z0-ALLOWED-AMT          PIC  9(013)V99.                      
               10  Z0-ALLOWED-AMT-X     REDEFINES                               
                   Z0-ALLOWED-AMT          PIC X(015).                          
                                                                                
               10  FILLER                  PIC X(074).                          
                                                                                
               10  Z0-TOT-CHARGES          PIC S9(013)V99.                      
               10  Z0-TOT-CHARGES-U     REDEFINES                               
                   Z0-TOT-CHARGES          PIC  9(013)V99.                      
               10  Z0-TOT-CHARGES-X     REDEFINES                               
                   Z0-TOT-CHARGES.                                              
                   15  FILLER              PIC X(014).                          
                   15  Z0-TOT-CHRG-LAST-BYTE                                    
                                           PIC X(001).                          
                       88 Z0-VALID-SIGN   VALUE 'A' 'B' 'C' 'D' 'E'             
                                                'F' 'G' 'H' 'I' 'J'             
                                                'K' 'L' 'M' 'N' 'O'             
                                                'P' 'Q' 'R' '}' '{'.            
                                                                                
               10  FILLER                  PIC X(028).                          
                                                                                
      *****************************************************************         
      ***            NEW AND IMPROVED PCDS SEPARATOR RECORD                     
      *****************************************************************         
                                                                                
           05  PCDS-SR-RECORD    REDEFINES PCDS-00-RECORD.                      
               10  SR-REC-TYPE                 PIC X(002).                      
               10  SR-SEPARATOR-LITERAL        PIC X(038).                      
      *** (05-10-01) AT CARMEL'S REQUEST, 'MCDS' SHOULD BE USED IN THE          
      *** START SEPARATOR 88 INSTEAD OF 'PCDS' BECAUSE A NUMBER OF              
      *** PROGRAMS RECOGNIZE A START SEP REC BY THIS.  FOR CONSISTENCY,         
      *** THE 88 FOR THE END SEP REC WILL ALSO USE 'MCDS'.                      
                   88  START-SEPARATOR-REC     VALUE                            
                       'THIS IS AN MCDS START SEPARATOR RECORD'.                
                   88  END-SEPARATOR-REC       VALUE                            
                       'THIS IS AN MCDS  END  SEPARATOR RECORD'.                
               10  FILLER                      PIC X(001).                      
               10  SR-SEPARATOR-TIME           PIC X(008).                      
               10  SR-SEPARATOR-TIME-X REDEFINES SR-SEPARATOR-TIME.             
                   15 SR-SEPARATOR-T-HH        PIC X(002).                      
                   15 FILLER                   PIC X(001).                      
                   15 SR-SEPARATOR-T-MM        PIC X(002).                      
                   15 FILLER                   PIC X(001).                      
                   15 SR-SEPARATOR-T-SS        PIC X(002).                      
               10  FILLER                      PIC X(001).                      
               10  SR-SEPARATOR-DATE           PIC X(008).                      
               10  SR-SEPARATOR-DATE-X REDEFINES SR-SEPARATOR-DATE.             
                   15 SR-SEPARATOR-D-MM        PIC X(002).                      
                   15 FILLER                   PIC X(001).                      
                   15 SR-SEPARATOR-D-DD        PIC X(002).                      
                   15 FILLER                   PIC X(001).                      
                   15 SR-SEPARATOR-D-YY        PIC X(002).                      
               10  FILLER                      PIC X(001).                      
               10  SR-SEPARATOR-JOB-PREFIX     PIC X(001).                      
               10  SR-SEPARATOR-JOB-NUMBER     PIC X(005).                      
               10  FILLER                      PIC X(001).                      
               10  SR-GATEWAY-ID               PIC X(002).                      
                                                                                
0407JW*        10  SR-UNIQUE-JOBNUM            PIC X(007).                      
0407JW*        10  FILLER                      PIC X(001).                      
0407JW                                                                          
0407JW** -- EXPAND UNIQUE-JOBNUM TO EIGHT BYTES -- **                           
0407JW         10  SR-UNIQUE-JOBNUM            PIC X(008).                      
               10  FILLER                      PIC X(116).                      
                                                                                
      **************  E N D   O F   P C L A M 1 2 4  ******************         
