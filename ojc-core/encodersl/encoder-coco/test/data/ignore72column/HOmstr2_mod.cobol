       01  SAF-HO-POLICY-MASTER-REC. 
         02  HEADERS.                                
           05  CDE-HEADER.                                   
              10  RECONCILIATION-KEY     PIC  X(16). 
              10  LOGGING-LEVEL          PIC  X(1).  
              10  FILLER                 PIC  X(23). 
           05  RESPONSE-HEADER.                              
              10 TPH-LLBB-LL             PIC X(2).          
              10 TPH-LLBB-BB             PIC X(2).          
              10 HEX-17-FOR-TEN          PIC X(10).         
              10 POLICY-NUMBER           PIC X(10).         
              10 FILLER                  PIC X.             
              10 SAFA-SAFB-TYPE          PIC X(4).          
                 88 ITS-SAFA                   VALUE 'SAFA'.
                 88 ITS-SAFB                   VALUE 'SAFB'. 
              10 FILLER                  PIC X.             
              10 SAFARI-SEQ-NUM          PIC X(6).          
              10 FILLER                  PIC X.             
              10 DEST-INFO.                                 
                 15 DEST-TERM-ID        PIC X.              
                 15 DEST-BUS-CTR        PIC 9(3).      
              10 FILLER                  PIC X(2).     
              10 JULIAN-DAY              PIC 9(3).      
              10 FILLER                  PIC X(1).     
              10 SOURCE-SEQ-NUM          PIC 9(4).     
              10 TEST-IND                PIC X.        
              10 MESSAGE-SEQUENCE        PIC X(5).     
              10 APOF-SW                 PIC X(1).     
              10 POLICY-SYMBOL           PIC X(2).     
              10 GEMINI-NBIS-IND         PIC X(1).     
              10 TRANSACTION-CODE        PIC X(4).     
              10 UNSOLICITED             PIC X(1).     
              10 QUEUE-IN-DAY            PIC X(3).     
              10 FILLER                  PIC X(1).     
              10 QUEUE-IN-HH             PIC X(2).     
              10 TRANSACTION-STATUS      PIC X(1).     
              10 QUEUE-IN-MM             PIC X(2).     
              10 FRONT-END-IND           PIC X(1).     
                 88 ITS-CFI                  VALUE 'C'.
              10 QUEUE-IN-SS             PIC X(2).     
              10 TERMINAL-ID-1-2         PIC X(2).          
              10 ROUTE-MODE              PIC X(2).          
              10 HEX-IF-AT-THE-END       PIC X(1). 
            05  ACK-AREA                 PIC X(3917).  
            05  RESPONSE-ACK REDEFINES ACK-AREA.                   
              10 RESPONSE-ACK-RETURN-CODE       PIC S9(5).   
              10 RESPONSE-ACK-REASON-CODE       PIC S9(5).   
              10 RESPONSE-ACK-TRX-TYPE          PIC X(4).    
              10 RESPONSE-ACK-MESSAGES OCCURS 20 TIMES.      
                 15 RESP-ACK-SAF-CDE-MSG-CODE   PIC X(5).    
                 15 RESP-ACK-SAF-CDE-ASTERISK   PIC X.       
                 15 RESP-ACK-SAF-CDE-TIER-KBMS  PIC X(7).    
                 15 RESP-ACK-SAF-CDE-MSG-OVRD   PIC X(3).    
                 15 RESP-ACK-SAF-CDE-MSG-TEXT   PIC X(120).  
                 15 RESP-ACK-SEVERITY-CODE      PIC X.       
              10 RESPONSE-ACK-FILLER            PIC X(1163). 
SRMSRM*================================================================*00001003
SRMSRM*                                                                *00001103
SRMSRM*  !!!!! ALWAYS KEEP THIS FIRST SET OF COMMENTS FIRST  !!!!      *00001203
SRMSRM*        AND PLEASE READ THEM - INFORMATION ON INITIALIZE AND    *00001304
SRMSRM*        FILLER AND RECOMPILE IS EXTERMELY IMPORTANT!!!!!        *00001404
SRMSRM*                                                                *00001504
SRMSRM*   MLD 12/02/1999 ADDED    LEVEL FOR VALID POLICY STATUS        *00002003
SRMSRM*   IF ANY NEW STATUSES ARE ADDED, YOU MUST INCLUDE              *00003004
SRMSRM*   THE NEW VALUES FOR IT UNDER THE    LEVEL FOR                 *00004004
SRMSRM*   MPOL-VALID-POLICY-STATUS                                     *00005004
SRMSRM*                                                                *00005104
SRMSRM*    RENAMED ALL FILLER ITEMS TO BE MFILLER-NN (WHERE-NN IS A    *00005206
SRMSRM*    NUMBER). THIS WILL ALLOW THE ITEMS TO BE SET TO THE PROPER  *00005304
SRMSRM*    NUMERIC VALUE OF ZEROES.                                    *00005404
SRMSRM*                                                                *00005504
SRMSRM*   IF CHANGES ARE MADE TO THIS COPY RECORD - YOU MUST ALWAYS    *00005604
SRMSRM*   RECOMPILE THE SRM MODULE  M2728024 AS THIS MODULE IS THE ONE *00005704
SRMSRM*   THAT INITIALIZES THE SAFARI  MASTER RECORD FOR INIIAL NEW    *00005804
SRMSRM*   BUSINESS TRANSACTIONS.                                       *00005904
SRMSRM*   FIND SRMSRM                                                  *00006004
SRMSRM*================================================================*00007004
940530*   ADDITION OF COSTIMATOR INFORMATION   SEARCH ON 940530  BRM   *00006004
940530*================================================================*00007004
943030*    DONT USE REX-FILLER!!!!!                                    *
943030*================================================================*00007004
949470*  07/21/2001 KAH                                                *00007004
949470*  TWO NEW FIELDS ADDED FOR CDE         SEARCH ON PCMS 949470    *00007004
949470*================================================================*00007004
962190*  10/21/2001 DJC  ADD ADDRESS FIELD REDEFINITIONS FOR ATLAS3/CDE*
962190*  SEPARATE DEFINITIONS FOR CITY STATE AND ZIP CODE.             *
962190*                                       SEARCH ON PCMS 962190    *
962190*================================================================*00007004
IDFRAD*  11/18/2001 BRM  ADD ID FRAUD ORIGINATION FIELD.  PROJECT 2661 *
IDFRAD*  D6HR003, D6HR004, D6HR035, D2HR244, KN,KW,KX AND NEW MODULE   *
IDFRAD*  M2728MI0 FOR ONLINE, P2HO010, P2HO048 OFFLINE.SEARCH ON IDFRAD*
IDFRAD*================================================================*00007004
ELGIBL*  03/23/2002 PMP  ADD FIELDS FOR CDE ELIGIBILITY REDESIGN       *
ELGIBL*                  PROJECT 2332
ELGIBL*                  D6HR001, D6HR003, D6HR004
ELGIBL*                  SEARCH ON ELGIBL                              *
ELGIBL*================================================================*00007004
TXMOLD*  05/19/2002 SCR  ADD NEW FIELDS FOR MOLD SUPPORT               *
TXMOLD*                  INITIALLY BROUGHT UP FOR TEXAS MOLD PROJECT   *
TXMOLD*                  PROJECT 2746                                  *
TXMOLD*                  SEARCH ON TXMOLD                              *
TXMOLD*================================================================*
TXCOND*  10/27/2002 LGP  ADD NEW FIELD FOR TX CONDO/TENANT SUPPORT     *
TXCOND*                  PROJECT 3481                                  *
TXCOND*                  SEARCH ON TXCOND                              *
000000*================================================================*
CWMLD2*  12/21/2002 LGP  ADD NEW FIELDS FOR CW MOLD PHRASE II SUPPORT  *
CWMLD2*                  PROJECT 3585                                  *
CWMLD2*                  SEARCH ON CWMLD2                              *
000000*================================================================*
NYMOLD*  12/21/2002 SCR  ADD NEW FIELDS FOR NY MOLD SUPPORT            *
NYMOLD*                  PROJECT 3615                                  *
NYMOLD*                  SEARCH ON NYMOLD                              *
000000*================================================================*
PL2602*  05/18/2003 JJA  ADD NEW FIELDS FOR PLUS SUPPORT OF BCFE ELIM  *
PL2602*                  D6HR001, D6HR003, D6HR004                     *
PL2602*                  SEARCH ON PL2602          PROJECT PLPM #2602  *
PL2602*================================================================*
PL2257*  05/18/2003 JJA  ADD NEW FIELDS FOR GEICO SUPPORT OF BCFE ELIM *
PL2257*                  D6HR001, D6HR003, D6HR004                     *
PL2257*                  SEARCH ON PL2257          PROJECT PLPM #2257  *
PL2257*================================================================*
PL4051*  05/18/2003 LAC  PREM LEVEL DEFAULT SUPORT FOR OMR AGENTS      *
PL4051*                  SEARCH ON PL4051          PROJECT PLPM #4051  *
PL4051*================================================================*
PL3427*  06/22/2003 SEV SUPPORT FOR PLUS ELIG INDICATORS               *
PL3427*                                            PLPM #3427          *
PL3427*================================================================*
PL3991*  06/22/2003 LGP ADD NEW FIELD FOR PLUS EDITION DATE            *
PL3991*                                            PLPM #3991          *
PL3991*================================================================*
PL4055*  09/21/2003 TNT  ADD NEW FIELD FOR UW ELIGIBILITY              *
PL4055*                  PLPM #4055/A3P-0483      SEARCH ON PL4055     *
PL4055*================================================================*
PL4089*  10/19/2003 MTO ADD NEW FIELD FOR AGGREGATED LOSSES            *
PL4089*                                            PLPM #4089          *
PL4089*================================================================*
PL3785*================================================================*
PL3785*  11/23/2003 RXC EXPANDING TEXAS DEVIATION FACTORS              *
PL3785*  ADDED FOUR NEW FIELDS: MRESV-TX-LLOYDS-DEV-CR-EXPND           *
PL3785*                         MRESV-NB-OR-RE-DEV-USED-EXPND          *
PL3785*                         MRESV-HTFD-STD-DEV-CR-EXPND            *
PL3785*                         MRESV-STD-HTFD-CR-PERC-EXPND           *
PL3785*  THEY WILL REPLACE THE EXISTING FIELDS:                        *
PL3785*                         MRESV-TX-LLOYDS-DEV-CR                 *
PL3785*                         MRESV-NB-OR-RE-DEV-USED                *
PL3785*                         MRESV-HTFD-STD-DEV-CR                  *
PL3785*                         MRESV-STD-HTFD-CR-PERC                 *
PL3785*  DIFFERENCE BETWEEN THE FIELDS IS:                             *
PL3785*  NEEDED TO INCREASE THE SIZE OF THE FIELDS                     *
PL3785*  FROM S9V99  TO S9V9999                                        *
PL3785*  ALSO ADDED A SWITCH MRES-TX-DEV-EXPND-SW                      *
PL3785*  TO HELP US DETERMINE WHICH FIELDS WE SHOULD USE               *
PL3785*                                                                *
PL3785*                                            PLPM #3785          *
PL3785*================================================================*
PL4667*  11/23/2003 SCR/TNT ADDED FIELDS FOR RSA PRICE STABILIZATION   *
PL4667*                     CREDIT                 PLPM #4667          *
PL4667*================================================================*
NCLANN*  12/21/2003 LGP ADDING NEW FIELDS FOR NCOIL ANNIVERSARY        *
NCLANN*  PROCESSING. AND BUFFERED TIER PROCESSING                      *
NCLANN*     LOCATE ON: NCLANN                      PLPM #4417          *
000000*================================================================*
DC1203*  12/21/2003 DLC ADD NEW IND FOR NCOIL                          *
DC1203*                                            PLPM #4547          *
000000*================================================================*
UP2257*  PRODUCTION DATE: 12/21/2003                INITIALS: MDO      *
UP2257*  EXPANDING THE GEICO SOURCE OF ADVERTISING FIELD TO 20 BYTES   *
UP2257*                                            PLPM #2257          *
UP2257*                                            SEARCH ON: UP2257   *
UP2257*================================================================*
NBPLET*  02/22/2004 PMP ADDING NEW FIELDS FOR NCOIL NOT BEST PRICE     *
NBPLET*  LETTER PROCESSING.                                            *
NBPLET*                                            PLPM #4417          *
NBPLET*                                            SEARCH NBPLET       *
000000*================================================================*
NCLAN2*  02/22/2004 LGP NEED TO ADD 2 NEW FIELDS FOR NCOIL ANNIVERSARY *
NCLAN2*  PROCESSING THAT WILL BE USED BY MI.                           *
NCLAN2*                                            PLPM #4417          *
NCLAN2*                                            SEARCH NCLAN2       *
000000*================================================================*
PL4383* 03/21/2004 PLUS-SUPPLEMENT YOUTHFUL DRIVER TO INEXPERIENCED    *
PL4383*                                               OPERATOR         *
PL4383*  ADDED FOUR NEW FIELDS :                                       *
PL4383*      DATE OF FIRST LICENESED                                   * 
PL4383*      NUMBER OF YEARS DRIVING                                   *
PL4383*      INEXPERIENCED DRIVER INDICATOR                            *
PL4383*      INEXPERIENCED DRIVER SWITCH                               *
PL4383*                             SEARCH ON:  PL4383                 *
PL4383******************************************************************
PL4489* 03/21/2004 MTO ADD NEW FIELD FOR CAPTURING THE PAID AMOUNT FOR *
PL4489*             LOSSES.                                            *
PL4489*                                            PLPM #4489          *
PL4489*================================================================*
PL4507*  PRODUCTION DATE: 04/18/2004               INITIALS: KCS       *
PL4507*  FIELDS ADDED FOR PDB RATING                                   *
PL4507*                                            PLPM #4507          *
PL4507*                                            SEARCH ON: PL4507   *
PL4507*================================================================*
PL4507*  PRODUCTION DATE: 05/23/2004               INITIALS: JLB       *
PL4507*  OREGON OIGA SURCHARGE - CREATING GENERIC FIELDS FOR USE BY    *
PL4507*  FUTURE STATES   SEARCH ON: PL5061         PLPM #5061          *
PL4507*================================================================*
PL4511*  PRODUCTION DATE:  06/20/2004              INITIALS:  SEV
PL4511*  SUPPORT FOR STM 2B, THE NEW REASON CODE TABLE
PL4511******************************************************************
PL4827*  PRODUCTION DATE:  06/20/2004              INITIALS:  SEV
PL4827*  SUPPORT FOR PLPM 4827, WHOLE RISK APPROVAL
PL4827******************************************************************
PL5119*  PRODUCTION DATE: 07/18/2004               INITIALS:  TNT      *
PL5119*  22G MI SOURCE SYSTEM ENHANCEMENTS - PASS DATE & TIME STAMP    *
PL5119*  PLPM #5119                                SEARCH ON: PL5119   *
PL5119*================================================================*
000010* 01  SAF-HO-POLICY-MASTER-REC.                                    D6H00010
000020*    THIS 01 LEVEL IS THE HISTORIC SECTION OF THE HOMEOWNERS      D6H00020
000030*    MASTER RECORD.                                               D6H00030
000040*                                                                 D6H00040
000050*                                                                 D6H00050
000060*        INDEX OF 05 LEVELS.                                      D6H00060
000070*    M-REX-FILLER-SEC                    CARD 100700.             D6H00070
000080*    M-SAF-RECORD-NO-SEC                 CARD 100710.             D6H00080
000090*    M-PROCESSING-VERSIONS-SEC           CARD 100740.             D6H00090
000100*    M-KEY-POLICY-FILE-SEC               CARD 100770.             D6H00100
000110*    M-POLICY-INFO-SEC                   CARD 101710.             D6H00110
000120*    M-DATES-POLICY-SEC                  CARD 102050.             D6H00120
000130*    M-CANC-INFO-SEC                     CARD 102160.             D6H00130
000140*    M-FIELD-OFFICE-INFO-SEC             CARD 102740.             D6H00140
000150*    M-MISC-UNDERWRITING-INFO-SEC        CARD 102910.             D6H00150
000160*    M-OTHER-SAF-POLICY-INFO-SEC         CARD 103270.             D6H00160
000170*    M-REINSURANCE-INFO-SEC              CARD 103470.             D6H00170
000180*    M-RISK-STATISTICS-SEC               CARD 103670.             D6H00180
000190*    M-COVERAGE-INFORMATION-SEC          CARD 103790.             D6H00190
000200*    M-DEDUCTIBLE-INFO-SEC               CARD 103980.             D6H00200
000210*    M-ENDT-COVG-INFORMATION-SEC         CARD 104210.             D6H00210
000220*    M-INSURED-AND-ADDRESS-SEC           CARD 106830.             D6H00220
000230*    M-DWELLING-INFO-SEC                 CARD 107070.             D6H00230
000240*    M-CODING-INFORMATION-SEC            CARD 107360.             D6H00240
000250*    M-APPRAISAL-INFO-SEC                CARD 107920.             D6H00250
000260*    M-ACTUAL-PREMISES-LOCATION-SEC      CARD 108600.             D6H00260
000270*    M-ADDL-RES-PREMISES-INFO-SEC        CARD 108700.             D6H00270
000280*    M-FIRST-MORTGAGEE-INFO-SEC          CARD 108820.             D6H00280
000290*    M-SECOND-MORTGAGEE-INFO-SEC         CARD 108970.             D6H00290
000300*    M-ALTERNATE-PAYOR-INFO-SEC          CARD 109120.             D6H00300
000310*    M-EXCEPTION-INFO-SEC                CARD 109260.             D6H00310
000320*    M-PREM-INFO-SEC                     CARD 109430.             D6H00320
000330*    M-OVERRIDE-INFO-SEC                 CARD 109960.             D6H00330
000340*    M-STAT-AND-ACCT-CODES-SEC           CARD 110010.             D6H00340
000350*    M-AETNA-STAT-CODES-SEC              CARD 110840.             D6H00350
000360*    M-HISTORIC-BILLING-INFO-SEC         CARD 111110.             D6H00360
000370*    M-EARTHQUAKE-REINS-PREM-SEC         CARD 111350.             D6H00370
000380*    M-NY-XL-INFO-SEC                    CARD 111400.             D6H00380
000390*    M-PERCENT-OF-INS-INFO-SEC           CARD 111490.             D6H00390
000400*    M-PART-PROVISION-FORM-NO            CARD 111530              D6H00400
000410*    M-CBS-OVERRIDE-IND                  CARD 111560              D6H00410
000420*    M-NAME-INS-FOR-BILLING              CARD 111600.             D6H00420
000430*    M-MODERNIZATION-INFO-SEC            CARD 111610.             D6H00430
000440*    M-REVISED-REINSURANCE-SEC           CARD 111710.             D6H00440
000450*    M-TRANS-SORT-KEY-SEC                CARD 111750.             D6H00450
000460*    M-BRANCH-AGENCY-INFO-SEC            CARD 111780.             D6H00460
000470*    M-TOWN-ROW-HOUSE-SEC                CARD 111910.             D6H00470
000480*    M-UPGRADING-INFO-SEC                CARD 111930.             D6H00480
000490*    M-CALIF-SECTION-2-SEC               CARD 112050.             D6H00490
000500*    M-CONTINGENCY-AREA-SEC              CARD 112080.             D6H00500
000510*    M-TRANSACTION-COMMISSION-INFO                                D6H00510
000520*    M-SCHEDULED-PERSONAL-PROPERTY                                D6H00520
000530*    M-REVISED-LOAN-NUMBERS                                       D6H00530
000540*    M-REVISED-RVN                                                D6H00540
000550*    M-TEXAS-ADDITIONAL-INFO                                      D6H00550
000560*    M-RESERVE-AREA-II                                            D6H00560
000570*    M-RESERVE-AREA-III                                           D6H00570
000571*    M-PLSP-82-CODES-SEC                                          D6H00580
000572*    M-WORKERS-COMP                                               D6H00590
000573*    MQUEST-INFO-AREA                                             D6H00600
000574*    MREAL-ENDT-MISC-REAL-PROP                                    D6H00610
000575*    MP82-DED-AMOUNT-ONS                                          D6H00620
000575*    MP82-DED-AMOUNT-OFF                                          D6H00630
000576*    MP82-WATERCRAFT-EXPOSURE-ONS                                 D6H00640
000577*    MP82-WATERCRAFT-EXPOSURE-OFF                                 D6H00650
000578*    MP82-ENDT-OFF                                                D6H00660
000579*    MP82-ENDT-ONS                                                D6H00670
000580*    MP82-CONDO-LIMIT-OFF                                         D6H00680
000581*    MP82-CONDO-LIMIT-ONS                                         D6H00690 
000582*    MRESIII-FL-CNTRSIG-IND                                       D6H00700
000583*    MRESIII-1982-VIE-MIN                                         D6H00710
000584*    MRESIII-HA-188-SW                                            D6H00720
000585*    MRESIII-RCVY-AEST                                            D6H00730
000586*    MGEM-ACTION-TYPE                                             D6H00740
000587*    MGEM-CUST-NUMBER                                             D6H00750
000588*    M-RESERVE-AREA-IV                                            D6H00760
000590*    M-RESERVE-AREA-V                                             D6H00770
000600*    M-HIST-SPMR-DATA                                             D6H00780
HO5093*    MSUR-SURE-OVERRIDE-INFO-AREA                                 D6H00790
000610*                                                                 D6H00800
000620*                                                                 D6H00810
000630* THE FOLLOWING 05 LEVELS ARE FOUND IN THE                        D6H00820
000640* SAF-HO-NON-HIST-MASTER-REC.                                     D6H00830
000650*                                                                 D6H00840
000660*    M-BEFORE-AETNA-CLAIMS-SEC          CARD 124150.              D6H00850
000670*    M-DIARY-INFO-SEC                   CARD 124270.              D6H00860
000680*    M-REPORT-SEC                       CARD 124670.              D6H00870
000690*    M-UND-CTL-RB-NOTE-ACT-SEC          CARD 125020.              D6H00880
000700*    M-CLAIM-DATA-SEC                   CARD 125750.              D6H00890
000710*    M-NON-HISTORIC-BILL-INFO-SEC       CARD 126690.              D6H00900
000720*    M-SPECIAL-CLAIM-INFO               CARD 126980.              D6H00910
000730*    M-NO-HIST-MR-DATA                                            D6H00920
000740*
000750*                                                                 D6H00940
000760*  ALL FIELDS IN THIS RECORD WHICH HAVE A PICTURE OF X ARE        D6H00950
000770*  INITIALIZED TO SPACES PRIOR TO PROCESSING.  THOSE FIELDS       D6H00960
000780*  WHICH ARE NOT AFFECTED BY PROCESSING WILL RETAIN THIS          D6H00970
000790*  INITIAL VALUE.                                                 D6H00980
000810   02  M-HIST-SPMR-DATA-FIELDS.
943030**** THE M-REX-FILLER-SEC IS NOT TO BE USED BY ANYONE!
943030**** THIS FIELD IS ALWAYS SET TO SPACES
*******
000820     05  M-REX-FILLER-SEC                       PIC X(384).
*******
943030**** THE M-REX-FILLER-SEC IS NOT TO BE USED BY ANYONE!
943030**** THIS FIELD IS ALWAYS SET TO SPACES
000830     05  M-SAF-RECORD-NO-SEC                    PIC S99    COMP-3.
000850         88  MSAF-SAF-HO-MASTER-REC-ID              VALUE +01.
000860     05  M-PROCESSING-VERSIONS-SEC.                               D6H01050
000870         10  MPRO-PCM-VERSION                   PIC X(4).
000880         10  MPRO-REX-VERSION                   PIC XX.
000890     05  M-KEY-POLICY-FILE-SEC.                                   D6H01080
000900         10  MKEY-ACTION-TYPE                   PIC X.
000910             88  MKEY-INITIAL-VALUE                 VALUE ' '.
000920             88  MKEY-ADD-ONLY                      VALUE 'A'.
000930             88  MKEY-CHANGE-ONLY                   VALUE 'C'.

000940         10  MKEY-UNDERWRITERS-INITIALS         PIC XXX.
000950         10  MKEY-SHORT-NAME-OF-INSURED         PIC XXX.

000960         10  MKEY-POLICY-NUMBER.                                  D6H01150
000970             15  MKEY-OFFICE-CODE               PIC S999   COMP-3.
000990             15  MKEY-POLICY-SYMBOL             PIC XXX.
001000             15  MKEY-POLICY-SERIAL-NUMBER      PIC S9(10) COMP-3.
001020             15  MKEY-POLICY-NUMBER-SUFFIX      PIC X(4).

001030         10  MKEY-AGENCY-COMM-INFO                         COMP-3.
001050             15  MKEY-AGENCY-CODE               PIC S9999.
001060             15  MKEY-COMM-RATE-VERSION-NO      PIC S9.
001070             15  MKEY-COUNTERSIG-OFFICE         PIC S999.
001080             15  MKEY-COUNTERSIG-AGENCY-CODE    PIC S9999.
001090             15  MKEY-COUNTERSIG-COMM-RATE      PIC SV999.

001100         10  MKEY-KEY-TRANS-INFO.                                 D6H01290
001110             15  MKEY-KEY-TRANS-TYPE            PIC AA.
001120                 88  MKEY-NEW-BUSINESS              VALUE 'NB'.
001130                 88  MKEY-POLICY-CHANGE             VALUE 'PC'.
001140                 88  MKEY-PRE-RENEWAL               VALUE 'PR'.
001150                 88  MKEY-DIARY-FOLLOWUP            VALUE 'DF'.
001160                 88  MKEY-RATE-ROLLBACK             VALUE 'RR'.
001170                 88  MKEY-RENEWAL                   VALUE 'RE'.
001180                 88  MKEY-TRANSFER                  VALUE 'CT'.
001190                 88  MKEY-CLAIM-REGISTER            VALUE 'CL'.
001200                 88  MKEY-CANCELLATION              VALUE 'CA'.
001210                 88  MKEY-CLAIM-UPDATE              VALUE 'CU'.
001220                 88  MKEY-INQUIRY                   VALUE 'IQ'.
001230                 88  MKEY-INQUIRY-FIX-AND-VAR       VALUE 'IF'.
001240                 88  MKEY-DECS-PAGE                 VALUE 'DP'.
001250             15  MKEY-KEY-TRANS-SUB-TYPE        PIC AA.
001260*                                                                 D6H01450
001270*  FOLLOWING SUB-TYPES VALID FOR USE WITH 'NEW BUSINESS' ONLY.    D6H01460
001280*                                                                 D6H01470
001290                 88  MKEY-ISSUE                     VALUE 'IS'.
001300                 88  MKEY-HOLD-ISSUANCE             VALUE 'HD'.
001310                 88  MKEY-NB-SPOIL-REISSUE          VALUE 'SR'.
001320                 88  MKEY-WIPEOUT                   VALUE 'WO'.
001330                 88  MKEY-DECLINE                   VALUE 'DL'.
001340                 88  MKEY-QUOTE-PREMIUM             VALUE 'QT'.
001350                 88  MKEY-ISSUE-A-HOLD              VALUE 'IH'.
001360                 88  MKEY-REVISE                    VALUE 'RE'.
001370                 88  MKEY-RENEWAL-CONVERSION        VALUE 'RC'.
001380*                                                                 D6H01570
001390*  FOLLOWING SUB-TYPES VALID FOR USE WITH 'POL. CHANGE' ONLY.     D6H01580
001400*                                                                 D6H01590
001410                 88  MKEY-DECS-REQUESTED            VALUE 'DR'.
001420                 88  MKEY-FILE-UPDATE               VALUE 'UP'.
001430*                                                                 D6H01620
001440*  FOLLOWING SUB-TYPES VALID FOR USE WITH 'CANCELLATION' ONLY.    D6H01630
001450*                                                                 D6H01640
001460                 88  MKEY-INITIAL-CANC              VALUE 'CA'.
001470                 88  MKEY-RST-CANC-POL              VALUE 'RS'.
001480                 88  MKEY-CBS-TERMINATION           VALUE 'TE'.
001490                 88  MKEY-CBS-CANC-LESS-PREM-DUE    VALUE 'AP'.
001500                 88  MKEY-CANC-SUSP                 VALUE 'SP'.
001510                 88  MKEY-EQUITY-CANC               VALUE 'EQ'.
001520*                                                                 D6H01710
001530*  FOLLOWING SUB-TYPES VALID FOR USE WITH 'PRE-RENEWAL' ONLY.     D6H01720
001540*                                                                 D6H01730
001550                 88  MKEY-PRE-RENEWAL-SCAN          VALUE 'AP'.
001560                 88  MKEY-PRE-RENEWAL-FO            VALUE 'FO'.
001570*                                                                 D6H01760
001580*  FOLLOWING SUB-TYPES VALID FOR USE WITH 'RENEWAL' ONLY.         D6H01770
001590*                                                                 D6H01780
001600                 88  MKEY-AUTOMATIC-RENEWAL         VALUE 'AR'.
001610                 88  MKEY-RENEW-AS-IS               VALUE 'AI'.
001620                 88  MKEY-RENEW-CHANGES-ISSUE       VALUE 'CI'.
001630                 88  MKEY-INITIAL-DNR               VALUE 'IN'.
001640                 88  MKEY-DIARIED-DNR               VALUE 'DN'.
001650                 88  MKEY-FINAL-DIARY-DNR           VALUE 'FN'.
001660                 88  MKEY-RENEW-LAPSED              VALUE 'RL'.
001670                 88  MKEY-RENL-REVISE-ISSUE         VALUE 'RI'.
001680                 88  MKEY-TERMINATED-AGENT          VALUE 'TA'.
001690*                                                                 D6H01880
001700*  FOLLOWING SUB-TYPES VALID FOR USE WITH 'CLAIM' ONLY.           D6H01890
001710*                                                                 D6H01900
001720                 88  MKEY-FACE-SHEET-REQ            VALUE 'FS'.
001730                 88  MKEY-NO-FACE-SHEET-REQ         VALUE 'NR'.
001740*                                                                 D6H01930
001750*  FOLLOWING SUB-TYPES VALID FOR USE WITH 'INQUIRY' ONLY.         D6H01940
001760*                                                                 D6H01950
001770                 88  MKEY-SPECIFIC-DATE             VALUE 'SD'.
001780                 88  MKEY-HISTORIC                  VALUE 'HI'.
001790                 88  MKEY-IMM-RESPONSE              VALUE 'IR'.
001800                 88  MKEY-AGENCY                    VALUE 'AY'.
001810*                                                                 D6H02000
001820*  FOLLOWING SUB-TYPE VALID FOR USE WITH 'DECS PAGE' ONLY         D6H02010
001830*                                                                 D6H02020
001840                 88  MKEY-RECREATE                  VALUE 'RA'.


001850     05  M-POLICY-INFO-SEC.                                       D6H02040
001860         10  MPOL-ACTION-TYPE                   PIC X.
001870             88  MPOL-INITIAL-VALUE                 VALUE ' '.
001880             88  MPOL-ADD-ONLY                      VALUE 'A'.
001890             88  MPOL-CHANGE-ONLY                   VALUE 'C'.

001900         10  MPOL-POLICY-STATUS                 PIC S99    COMP-3.
SRMSRM*================================================================*02170003
SRMSRM*================================================================*02170103
SRMSRM*   MLD 12/02/1999 ADDED 88 LEVEL FOR VALID POLICY STATUS        *02171003
SRMSRM*            IF ANY NEW STATUSES ARE ADDED, PLEASE INCLUDE       *02180003
SRMSRM*            THE NEW VALUES FOR IT UNDER THE ALL VALID-STATUS    *02190003
SRMSRM*                                                                *02200003
SRMSRM*================================================================*02201003
SRMSRM             88  MPOL-VALID-POLICY-STATUS VALUES ARE              02202003
                       +60, +61, +62, +63, +64, +67, +68, +40,          02203001
                       +42, +30, +31, +20, +80, +97, +98, +99.          02204001
001920             88  MPOL-ACTIVE                        VALUE +60.
001930             88  MPOL-ACTIVE-RENEWAL                VALUE +61.
001940             88  MPOL-ACTIVE-REWRITE                VALUE +62.
001950             88  MPOL-ACTIVE-NOT-TO-BE-RENEWED      VALUE +63.
001960             88  MPOL-ACTIVE-FOR-REVISING-CANC      VALUE +64.
001970             88  MPOL-ACTIVE-REINSTATED             VALUE +67.
001980             88  MPOL-ACTIVE-FO-AGY-TRANS-PEND      VALUE +68.
001990             88  MPOL-CANCELLED                     VALUE +40.
002000             88  MPOL-CANC-HAS-BEEN-REVISED         VALUE +42.
002010             88  MPOL-NOT-RENEWED-UNDT-REASONS      VALUE +30.
002020             88  MPOL-NOT-RENEWED-AGCY-REASONS      VALUE +31.
002030             88  MPOL-EXPIRED                       VALUE +20.
002040             88  MPOL-INELIGIBLE                    VALUE +80.
002050             88  MPOL-PENDING-COVG-NOT-PROVIDED     VALUE +97.
002060             88  MPOL-BOUND-COVG-IS-PROVIDED        VALUE +98.
002070             88  MPOL-DECLINED-NB                   VALUE +99.

002080         10  MPOL-STATUS-EFF-DATE               PIC S9(5)  COMP-3.
002100         10  MPOL-STATUS-ENTRY-DATE             PIC S9(5)  COMP-3.

002120         10  MPOL-COMPANY-CODE                  PIC X.
002130             88  MPOL-AETNA-CASUALTY                VALUE '1'.
002140             88  MPOL-STANDARD-FIRE                 VALUE '2'.
002150             88  MPOL-AETNA-C-S-ILLINOIS            VALUE '3'.
002160             88  MPOL-HTFD-AUTO                     VALUE '4'.
                   88  MPOL-TEXAS-LLOYDS                  VALUE '5'.
HO9079             88  MPOL-AETNA-INSURANCE-COMPANY       VALUE '6'.
HO9079             88  MPOL-AETNA-PERSONAL-SECURITY       VALUE '7'.
HO9079             88  MPOL-AETNA-INSURANCE-ILLINOIS      VALUE '8'.
HO9079             88  MPOL-VALID-FAMCO-COMPANY           VALUE '2'
HO9079                                                          '4'
HO9079                                                          '5'
HO9079                                                          '6'
HO9079                                                          '7'
HO9079                                                          '8'.
HO9079             88  MPOL-VALID-NON-FAMCO-COMPANY       VALUE '1'
HO9079                                                          '2'
HO9079                                                          '3'
HO9079                                                          '4'
HO9079                                                          '5'.
002170         10  MPOL-HO-FORM-NO                    PIC X(4).
002180         10  MPOL-HO-CONTR-SERIES-NO            PIC X(4).
002190         10  MPOL-HO-FORM-ED-DATE               PIC X(4).


002200     05  M-DATES-POLICY-SEC                                COMP-3.
002220         10  MDAT-FIRST-POL-EFF-DATE            PIC S9(5).
002230         10  MDAT-EFFECTIVE-DATE                PIC S9(5).
002240         10  MDAT-EXPIRATION-DATE               PIC S9(5).
002250         10  MDAT-CURR-MR-EFF-DATE              PIC S9(5).
002260         10  MDAT-CURR-MR-ENTRY-DATE            PIC S9(5).
002270         10  MDAT-CURR-DECL-ISSUE-DATE          PIC S9(5).
002280         10  MDAT-CURR-MR-ENTRY-TIME            PIC S9(4).
002290         10  MDAT-DECL-SEQUENCE-NO              PIC S999.
002300         10  MDAT-OLDEST-HR-EFF-DATE            PIC S9(5).


002310     05  M-CANC-INFO-SEC.                                         D6H02580
002320         10  MCAN-CANCEL-NON-REN-INFO.                            D6H02590
002330             15  MCAN-CANC-TRX-TYPE             PIC X.
002340                 88  MCAN-NO-INFORMATION            VALUE '0'.
002350                 88  MCAN-CANCELLATION              VALUE '1'.
002360                 88  MCAN-NON-RENEWAL               VALUE '2'.
002370                 88  MCAN-NON-RENEWAL-CANC          VALUE '3'.
002380             15  MCAN-INS-CANC-EFF-DATE         PIC S9(5)  COMP-3.
002400             15  MCAN-CANC-NON-REN-REASON       PIC XX.
002410                 88  MCAN-INS-REQUEST               VALUE '10'.
002420                 88  MCAN-CHANGE-OF-CONDITIONS      VALUE '11'.
002430                 88  MCAN-FRAUD-MISREP              VALUE '12'.
002440                 88  MCAN-VIOLATION-POL-TERMS       VALUE '13'.
002450                 88  MCAN-NON-PHYSICAL-HAZARD       VALUE '14'.
002460                 88  MCAN-CLM-FREQ-OR-SEVERITY      VALUE '15'.
002470                 88  MCAN-FRAUDULENT-CLAIM          VALUE '16'.
002480                 88  MCAN-CANC-OTHER-REASONS        VALUE '17'.
002490                 88  MCAN-NON-PAY-REINST-OPT        VALUE '18'.
002500                 88  MCAN-NON-PAY-NO-REINST-OPT     VALUE '19'.
002510                 88  MCAN-NR-AGENCY-TERM            VALUE '20'.
002520                 88  MCAN-NR-OTHER-REASONS          VALUE '21'.
002530                 88  MCAN-NR-OUTSIDE-REPORT         VALUE '33'.
002540                 88  MCAN-CANC-NR-REWRITE           VALUE '34'.
002550                 88  MCAN-PREMISES-COND             VALUE '35'.
HO3511                 88  MCAN-NR-HURRICANE-LOSS         VALUE '36'.
HO3511                 88  MCAN-CANC-HURRICANE-LOSS       VALUE '37'.
002560                 88  MCAN-BAD-CHECK-HO              VALUE '98'.
002570                 88  MCAN-BAD-CHECK                 VALUE '99'.
002580             15  MCAN-CANC-NON-REN-EVIDENCE     PIC X.
002590                 88  MCAN-NOTICE-SAFARI-ISSUE       VALUE '1'.
002600                 88  MCAN-NOTICE-FIELD-ISSUE        VALUE '2'.
002610                 88  MCAN-LETTER-FROM-INSURED       VALUE '4'.
002620                 88  MCAN-LOST-POLICY-RELEASE       VALUE '5'.
002630                 88  MCAN-LAST-DECS-PAGE            VALUE '6'.
002640                 88  MCAN-FINAN-PREM-CANC-NOTICE    VALUE '7'.
002650                 88  MCAN-CBS-TERM                  VALUE '8'.
002660             15  MCAN-PRIOR-CANC-EFF-DATE       PIC S9(5)  COMP-3.

002680         10  MCAN-CANC-REINST-INFO.                               D6H02970
002690             15  MCAN-REINST-TRX-TYPE           PIC X.
002700                 88  MCAN-NO-INFORMATION            VALUE '0'.
002710                 88  MCAN-REINSTATE                 VALUE '1'.
002720                 88  MCAN-RENEW-LAPSED              VALUE '2'.
002730             15  MCAN-CANC-RST-EFF-DATE         PIC S9(5)  COMP-3.
002750             15  MCAN-CANC-RST-REASON           PIC X.
002760                 88  MCAN-REFUND-CR                 VALUE '0'.
002770                 88  MCAN-INSURED-REQUEST           VALUE '1'.
002780                 88  MCAN-MORTGAGEE-REQUEST         VALUE '2'.
002790                 88  MCAN-COMPANY-REQUEST           VALUE '3'.
002800                 88  MCAN-CO-CANC-IN-ERROR          VALUE '4'.
002810                 88  MCAN-PAYMENT-OF-PREMIUM        VALUE '5'.
002820                 88  MCAN-CANC-REVISED              VALUE '6'.
002830                 88  MCAN-CBS-REINST                VALUE '9'.
002840             15  MCAN-REINST-TALLY              PIC S99    COMP-3.

002860         10  MCAN-CANC-NR-REIN-NOTICE-INFO.                       D6H03150
002870             15  MCAN-NOTICE-TYPE               PIC X.
002880                 88  MCAN-NOTICE-OF-TERM-CANC       VALUE '1'.
002890                 88  MCAN-NOTICE-OF-TERM-NR         VALUE '2'.
002900                 88  MCAN-NR-AGENCY-TERM            VALUE '3'.
002910                 88  MCAN-REINST                    VALUE '4'.
002920             15  MCAN-NOTICE-PROC-DATE          PIC S9(5)  COMP-3.


002940     05  M-FIELD-OFFICE-INFO-SEC.                                 D6H03230
002950         10  MFIE-ACTION-TYPE                   PIC X.
002960             88  MFIE-INITIAL-VALUE                 VALUE ' '.
002970             88  MFIE-ADD-ONLY                      VALUE 'A'.
002980             88  MFIE-CHANGE-ONLY                   VALUE 'C'.
002990             88  MFIE-ELIMINATE-ONLY                VALUE 'E'.
003000         10  MFIE-PRIOR-AETNA-COV               PIC X.
003010             88  MFIE-ONE-YEAR                      VALUE '1'.
003020             88  MFIE-TWO-YEARS                     VALUE '2'.
003030             88  MFIE-THREE-YEARS                   VALUE '3'.
003040             88  MFIE-MORE-THAN-3-YEARS             VALUE '4'.
003050         10  MFIE-SCOPE-IND                     PIC X.
003060             88  MFIE-SCOPE                         VALUE 'Y'.
003070             88  MFIE-NO-SCOPE                      VALUE 'N'.
003080         10  MFIE-OTHER-AETNA-INS-IND           PIC X.
003090             88  MFIE-OTHER-AETNA-INS               VALUE 'Y'.
003100             88  MFIE-NO-OTHER-AETNA-INS            VALUE 'N'.


003110     05  M-MISC-UNDERWRITING-INFO-SEC.                            D6H03400
003120         10  MMIS-ACTION-TYPE                   PIC X.
003130             88  MMIS-INITIAL-VALUE                 VALUE ' '.
003140             88  MMIS-ADD-ONLY                      VALUE 'A'.
003150             88  MMIS-CHANGE-ONLY                   VALUE 'C'.

003160       07  MMIS-NUMERIC-UND-DATA.
003170         10  MMIS-REPLACEMENT-COST              PIC S9(7)  COMP-3.
003190         10  MMIS-YEAR-BUILT                    PIC S9(4)  COMP-3.
003210         10  MMIS-PRIOR-CARRIER-CODE            PIC S9(3)  COMP-3.
003230         10  MMIS-PRIOR-CARRIER-ACT-TAKEN       PIC X.
003240             88  MMIS-CANCELLED-PRIOR               VALUE '1'.
003250             88  MMIS-DECLINED                      VALUE '2'.
003260             88  MMIS-RENEWAL-REFUSED               VALUE '3'.
003270             88  MMIS-NONE-OF-THE-ABOVE             VALUE '4'.

003280         10  MP82-DED-INL-OFF                   PIC S99    COMP-3.
003300         10  MMIS-RESIDENCE-PREMISES-IND        PIC X.
003310             88  MMIS-PRIMARY-RESIDENCE             VALUE '1'.
003320             88  MMIS-SECONDARY-RESIDENCE           VALUE '2'.

003330         10  MMIS-COURTESY-CLUB-INFO.                             D6H03620
003340             15  MMIS-COURT-C-REFER-IND         PIC X.
003350                 88  MMIS-NO-COURT-C-REFER          VALUE '0'.
003360                 88  MMIS-COURT-C-REFER             VALUE '1'.
003370             15  MMIS-COURT-C-REFER-OFFICE      PIC S999   COMP-3.
003390             15  MMIS-COURT-C-EFF-DATE          PIC S9(5)  COMP-3.
003410             15  MMIS-COURT-C-ENTRY-DATE        PIC S9(5)  COMP-3.
003430             15  MMIS-COURT-C-ACTION-IND        PIC X.
003440                 88  MMIS-INS-CONT-WITH-AETNA       VALUE '1'.
003450                 88  MMIS-INS-NOT-CONT-WITH-AETNA   VALUE '2'.
003460                 88  MMIS-INS-NO-LONGER-NEEDED      VALUE '3'.


003470     05  M-OTHER-SAF-POL-INFO-TBL.
003470       07  M-OTHER-SAF-POLICY-INFO-SEC          OCCURS 6 TIMES
003480                                                INDEXED BY
003480                                                MOTH-INDEX.
003490         10  MOTH-ACTION-TYPE                   PIC X.
003500             88  MOTH-INITIAL-VALUE                 VALUE ' '.
003510             88  MOTH-ADD-ONLY                      VALUE 'A'.
003520             88  MOTH-CHANGE-ONLY                   VALUE 'C'.
003530             88  MOTH-ELIMINATE-ONLY                VALUE 'E'.
003540         10  MOTH-OTHER-POLICY-CODE             PIC X.
003550             88  MOTH-SAF-HO-PRIMARY                VALUE '1'.
003560             88  MOTH-SAF-HO-SECONDARY              VALUE '2'.
003570             88  MOTH-SAF-AUTO                      VALUE '3'.
003580             88  MOTH-ANY-PRIOR                     VALUE '4'.
003590             88  MOTH-ANY-SUBSEQ                    VALUE '5'.
003600         10  MOTH-OTHER-SAF-POL-NUMBER.                           D6H03890
003610             15  MOTH-OTHER-OFFICE-CODE         PIC S9(3)  COMP-3.
003630             15  MOTH-OTHER-POLICY-SYMBOL       PIC XX.
003640             15  MOTH-OTHER-POLICY-SERIAL-NO    PIC S9(10) COMP-3.
003660             15  MOTH-OTHER-POLICY-SUFFIX       PIC XXXX.


003670     05  M-REINSURANCE-INFO-SEC.                                  D6H03960
003680         10  MREI-ACTION-TYPE                 PIC X.
003690             88  MREI-INITIAL-VALUE               VALUE ' '.
003700             88  MREI-ADD-ONLY                    VALUE 'A'.
003710             88  MREI-CHANGE-ONLY                 VALUE 'C'.
003720             88  MREI-ELIMINATE-ONLY              VALUE 'E'.
003730         10  MREI-REINS-CO-CODE               PIC S999     COMP-3.
003750         10  MREI-REINS-EFF-DATE              PIC S9(5)    COMP-3.
003770         10  MREI-PERCENT-CEDED               PIC SV999    COMP-3.
003790         10  MREI-GROSS-LIABILITY             PIC S9(7)    COMP-3.
003810         10  MREI-CESSION-NO                  PIC X(7).
003820         10  MREI-UND-INITIALS                PIC XXX.
003830         10  MREI-AETNA-REINS-COMM            PIC SV999    COMP-3.
003850         10  MREI-REINS-AMOUNT                PIC S9(6)V99 COMP-3.


003870     05  M-RISK-STATISTICS-SEC                             COMP-3.
003890         10  MRIS-NO-COVERAGE-ENDTS             PIC S99.
003900         10  MRIS-NO-MORTGAGEES                 PIC S9.
003910         10  MRIS-NO-WARRANTY-EXCEPTIONS        PIC S9.
003920         10  MRIS-NO-TIMES-RENEWED              PIC S99.
003930         10  MRIS-NO-AETNA-CLAIMS               PIC S99.
003940         10  MRIS-NO-CLMS-PRIOR-CARRIERS        PIC S9.
003950         10  MRIS-NO-UND-CONTROLS               PIC S9.
003960         10  MRIS-NO-UND-ACTIONS                PIC S9.
003970         10  MRIS-NO-DEDUCTIBLE-ENDTS           PIC S9.
003980         10  MRIS-NO-UND-NOTATIONS              PIC S9.


003990     05  M-COVERAGE-INFORMATION-SEC.                              D6H04280
004000         10  MCOV-ACTION-TYPE                   PIC X.
004010             88  MCOV-INITIAL-VALUE                 VALUE ' '.
004020             88  MCOV-ADD-ONLY                      VALUE 'A'.
004030             88  MCOV-CHANGE-ONLY                   VALUE 'C'.

004040       07  MCOV-SECTION-1-COV-INFO                         COMP-3.
004060         10  MCOV-LIM-DWELLING                  PIC S9(7).
004070         10  MCOV-FIRST-POL-LIM-DWELLING        PIC S9(7).
004080         10  MOVED-TO-MRESIII-AREA              PIC S9(5).
004090         10  MCOV-LIM-UNSCHED-PERS-PROP         PIC S9(7).
004100         10  MCOV-LIM-ADDL-LIVING-EXP           PIC S9(5).
004110         10  MCOV-SECT-1-RVN                    PIC S9.

004120       07  MCOV-SECTION-2-COV-INFO                         COMP-3.
HO5547         10  MCOV-LIM-PERS-LIAB-OCC             PIC S9(7).
004150         10  MCOV-LIM-MED-PAY-PERS              PIC S9(4).
004160         10  MCOV-LIM-MED-PAY-ACC               PIC S9(5).
004170         10  MCOV-TX-PHYSICAL-DMG               REDEFINES
004180              MCOV-LIM-MED-PAY-ACC              PIC S9(5).
004190         10  MCOV-SECT-2-RVN                    PIC S9.


004200     05  M-DEDUCTIBLE-INFO-SEC.                                   D6H04490
004210         10  MDED-ACTION-TYPE                   PIC X.
004220             88  MDED-INITIAL-VALUE                 VALUE ' '.
004230             88  MDED-ADD-ONLY                      VALUE 'A'.
004240             88  MDED-CHANGE-ONLY                   VALUE 'C'.
004250             88  MDED-ELIMINATE-ONLY                VALUE 'E'.
004260         10  MDED-DEDUCTIBLE-CODE               PIC X.
004270             88  MDED-FULL-COVG                     VALUE '1'.
004280             88  MDED-CLAUSE-1                      VALUE '2'.
004290             88  MDED-CLAUSE-1-AND-2                VALUE '3'.
004300             88  MDED-ALL-PERILS-SECT-1             VALUE '4'.
004310             88  MDED-FLAT-SECT-1                   VALUE '5'.
004320             88  MDED-ALL-OTHER-DEDUCT              VALUE '9'.


004330*                                                                 D6H04620
004340*    WE WILL ALLOW A MAXIMUM OF 10 COVERAGE ENDORSEMENTS ON 1     D6H04630
004350*    POLICY.  THE FOLLOWING SECTION HAS BEEN DESIGNED WITH 12     D6H04640
004360*    OCCURRENCES, HOWEVER, IN ORDER TO PREVENT UNNECESSARY REX    D6H04650
004370*    DATA MANIPULATION DIFFICULTIES IN THE INPUT MESSAGE          D6H04660
004380*    ACCEPTANCE HANDLER (IMAH) MODULES.  DIFFICULTIES WOULD       D6H04670
004390*    RESULT FROM DEFINING ONLY 10 OCCURRENCES OF DATA IN THE      D6H04680 
004400*    RECORD BUT ALLOWING THE USE OF 12 DIFFERENT COVERAGE         D6H04690
004410*    ENDORSEMENT FORM-NUMBER BLOCKS (6 EACH ON FORMS 52B, 54B).   D6H04700
004420*                                                                 D6H04710
004430     05  M-ONDT-COVG-INFORMATION-SEC.                             D6H04720
004440         10  MOND-COV-ENDT-INFO-AREA            OCCURS 12 TIMES
004450                                                INDEXED BY
004450                                                MOND-COV-INDEX.
004460             15  MOND-ACTION-TYPE               PIC X.
004470                 88  MOND-INITIAL-VALUE             VALUE ' '.
004480                 88  MOND-ADD-ONLY                  VALUE 'A'.
004490                 88  MOND-CHANGE-ONLY               VALUE 'C'.
004500                 88  MOND-ELIMINATE-ONLY            VALUE 'E'.

004510             15  MOND-COV-ENDT-FORM-NO.                           D6H04800
HO6268                 20  MOND-COV-ENDT-FORM-NO-1ST-6    PIC X(6).     D6H04810
HO6268                 20  MOND-COV-ENDT-FORM-NO-SUFFIX   PIC X(3).     D6H04820
HO3227                 20  MOND-COV-ENDT-CREDIT-IND       REDEFINES
HO3227                      MOND-COV-ENDT-FORM-NO-SUFFIX.
HO3227                     25  MOND-CREDIT-IND            PIC XX.
HO3227                     25  MOND-CREDIT-SUFFIX         PIC X.

004520             15  MOND-COV-ENDT-INFO.                              D6H04870
004530                 20  MOND-COV-ENDT-ED-DATE          PIC X(4).
004540                 20  MOND-COV-ENDT-INF-LOCATION-IND PIC XX.
004560                     88  MOND-SECT-B-ADD-INFO          VALUE '01'.
004570                     88  MOND-SECT-C-ADD-INFO          VALUE '02'.
004580                     88  MOND-SECT-D-ADD-INFO          VALUE '03'.
004590                     88  MOND-PROT-DEVICES             VALUE '04'.
004600                     88  MOND-OFF-OCC-ADD-INFO         VALUE '10'.
004610                     88  MOND-APP-STRUCT-ADD-INFO      VALUE '15'.
004620                     88  MOND-ADDL-INT-ADD-INFO        VALUE '20'.
004630                     88  MOND-SNOWMOBILE-ADD-INFO      VALUE '25'.
004640                     88  MOND-BUSINESS-PUR-ADD-INFO    VALUE '30'.
004650                     88  MOND-WATERCRAFT-ADD-INFO      VALUE '35'.
004660                     88  MOND-WORKERS-COMP             VALUE '40'.
004670                     88  MOND-PER-EX-INDEM-ADD-INFO    VALUE '45'.
004680                     88  MOND-EXT-THEFT-ADD-INFO       VALUE '50'.
004690                     88  MOND-XL-ADDL-INFO-FLA         VALUE '52'.
004700                     88  MOND-CREDIT-CARD-ADD-INFO     VALUE '55'.
004710                     88  MOND-ADDL-RES-ADD-INFO        VALUE '65'.
004720                     88  MOND-MISC-ENDT-ADD-INFO       VALUE '70'.
004730                     88  MOND-MISC-REAL-PROP-INFO      VALUE '72'.
004740                     88  MOND-RATED-INFL-GUARD-INFO    VALUE '75'.
004750                     88  MOND-SCHED-PERS-PROP-INFO     VALUE '80'.
004760                     88  MOND-LOSS-ASSESSMENT          VALUE '85'.
004770                     88  MOND-COVG-D-DIFF              VALUE '86'.
004780                     88  MOND-OTHER-RES-LIMIT-INFO     VALUE '90'.
004790                     88  MOND-INVALID-ENDT             VALUE '98'.
004800                     88  MOND-NO-ADD-INFO-FOR-COV-ENDT VALUE '99'.
004810                     88  MOND-TX-ADDL-INSURED          VALUE 'A '.
004820                     88  MOND-TX-OFF-PROF-OCC          VALUE 'B '.
004830                     88  MOND-TX-TV-RADIO-ANTENNA      VALUE 'C '.
004840                     88  MOND-TX-PHY-SURG-DENTIST      VALUE 'D '.
004850                     88  MOND-TX-MONIES-SECURITIES     VALUE 'E '.
004860                     88  MOND-TX-ADDL-AMT-PRIV-STR     VALUE 'F '.
004870                     88  MOND-TX-FCPL                  VALUE 'G '.
004880                     88  MOND-TX-WATERCRAFT            VALUE 'H '.
004890                     88  MOND-TX-BUS-PURSUITS          VALUE 'I '.
004900                     88  MOND-TX-ADDL-PREMISES         VALUE 'J '.
004910                     88  MOND-TX-ADDL-AMT-SEC-RES      VALUE 'K '.
004920                     88  MOND-TX-ADDL-LIM-JEWELRY-FURS VALUE 'L '.
004930                     88  MOND-TX-SCHED-PERS-PROP       VALUE 'M '.
004940                     88  MOND-TX-PERS-INJURY           VALUE 'N '.
004950                     88  MOND-TX-OUTBUILDINGS          VALUE 'O '.
004960                     88  MOND-TX-LOSS-ASSESMENT        VALUE 'P '.
004970                     88  MOND-TX-INF-PERCENT-INFO      VALUE 'Q '.
607810                     88  MOND-TX-ADDL-LIM-BUS-PROPERTY VALUE 'R '.
004980                     88  MOND-TX-HO390-INFO            VALUE 'Z '.
005040                     88  MOND-TEXAS-ENDTS              VALUE 'A '
005040                                                             'B '
005040                                                             'C '
005040                                                             'D '
005040                                                             'E '
005040                                                             'F '
005040                                                             'G '
005040                                                             'H '
005040                                                             'I '
005040                                                             'J '
005040                                                             'K '
005040                                                             'L '
005040                                                             'M '
005040                                                             'N '
005040                                                             'O '
005040                                                             'P '
005040                                                             'Q '
005040                                                             'R '
005040                                                             'Z '.
005090                 20  MOND-DEDUCT-ENDT-IND           PIC X.
005100                     88  MOND-DED-ENDT-1                VALUE '1'.
005110                     88  MOND-DED-ENDT-2                VALUE '2'.
005120                     88  MOND-NO-DEDUCT-ENDT            VALUE '0'.
005130                 20  MOND-TYPING-IND                PIC X.
005140                     88  MOND-TYPING-REQUIRED           VALUE '0'.
005150                     88  MOND-NO-TYPING-REQUIRED        VALUE '1'.
005160                 20  MOND-ATTACH-TO-DECL-IND        PIC X.
005170                     88  MOND-ATTACH-TO-DECL            VALUE '1'.
005180                     88  MOND-ALREADY-ATTACHED-TO-DECL  VALUE '0'.
005190                 20  MOND-COV-IND                   PIC X.
005200                     88  MOND-SEC-1-ONLY                VALUE '1'.
005210                     88  MOND-SEC-2-ONLY                VALUE '2'.
005220                     88  MOND-SEC-1-AND-SEC-2           VALUE '3'.
005230                     88  MOND-DOES-NOT-APPLY-TO-POL-COV VALUE '0'.
005240                 20  MOND-REST-COV-ENDT-IND         PIC X.
005250                     88  MOND-RESTRICTIVE               VALUE '1'.
005260                     88  MOND-NON-RESTRICTIVE           VALUE '0'.
005270                 20  MOND-SIGN-COV-ENDT-IND         PIC X.
005280                     88  MOND-SIGNATURE-OBTAINED        VALUE '1'.
005290                     88  MOND-SIGNATURE-NOT-OBTAINED    VALUE '2'.
005300                     88  MOND-SIGN-NOT-NECESSARY        VALUE '0'.
005310                 20  MOND-COV-END-FILE-IND          PIC X.
005320                     88  MOND-FILING-MADE               VALUE '1'.
005330                     88  MOND-FILING-NOT-MADE           VALUE '2'.
005340                     88  MOND-NO-FILING-NECESSARY       VALUE '0'.
005350                 20  MOND-CANC-CONSIDER-IND         PIC X.
005360                     88  MOND-MUST-CONSIDER             VALUE '1'.
005370                     88  MOND-NOT-NECESSARY-TO-CONSIDER VALUE '0'.
005380                 20  MOND-COV-ENDT-PREMIUM    PIC S9(5)V99 COMP-3.
005400                     88  MOND-NO-PREMIUM           VALUE +0000.00.
005410                 20  MOND-ENDT-ADDL-RETN-PREM-OFF                 D6H05760
005420                                              PIC S9(5)V99 COMP-3.
005440                 20  MOND-ENDT-ADDL-RETN-PREM-ONS                 D6H05790
005450                                              PIC S9(5)V99 COMP-3.

005470             15  MOND-COV-ENDT-EFF-DATE       PIC S9(5)    COMP-3.

005490         10  MEND-SECT-B-ADDL-INFO.                               D6H05840
005500             15  MEND-SECT-B-MED-PAY-IND        PIC X.
005510                 88  MEND-SECT-B-MED-PAY            VALUE 'Y'.
005520                 88  MEND-SECT-B-NO-MED-PAY         VALUE 'N'.
005530             15  MEND-SECT-B-INFO.                                D6H05880
HO3237                 20  MEND-SECT-B-LIMIT-OLD      PIC S9(5)  COMP-3.
HO3237                 20  MEND-TX-PHY-SURG-LIM-OLD   REDEFINES
HO3237                      MEND-SECT-B-LIMIT-OLD     PIC S9(5)  COMP-3.
005580                 20  MEND-SECT-B-NO-FAMILIES    PIC X.
005590                     88  MEND-3-FAMILIES            VALUE '3'.
005600                     88  MEND-4-FAMILIES            VALUE '4'.
005610                 20  MEND-OLT-ZONE              PIC XX.

005620         10  MEND-SECT-C-ADDL-INFO.                               D6H05970
005630             15  MEND-SECT-C-LIMIT-1            PIC S9(5)  COMP-3.
005650             15  MEND-TX-MONIES                 REDEFINES
005660                  MEND-SECT-C-LIMIT-1           PIC S9(5)  COMP-3.
005670             15  MEND-SECT-C-LIMIT-2            PIC S9(5)  COMP-3.
005690             15  MEND-TX-SECURITIES             REDEFINES
005700                  MEND-SECT-C-LIMIT-2           PIC S9(5) COMP-3.

005710         10  MEND-SECT-D-ADDL-INFO.                               D6H06060
005720             15  MEND-SECT-D-MED-PAY-IND        PIC X.
005730                 88  MEND-SECT-D-MED-PAY            VALUE 'Y'.
005740                 88  MEND-SECT-D-NO-MED-PAY         VALUE 'N'.
005750             15  MEND-TX-PERSONAL-INJ-IND       REDEFINES
005760                  MEND-SECT-D-MED-PAY-IND       PIC X.
005770             15  MEND-SECT-D-NO-FAMILIES        PIC X.
005780                 88  MEND-3-FAMILIES                VALUE '3'.
005790                 88  MEND-4-FAMILIES                VALUE '4'.
005800             15  MEND-TERR-ZONE                 PIC XX.
005810             15  MEND-EARTHQUAKE-ZONE           PIC X.
005820             15  MEND-SPEC-EXCL-IND             PIC X.
005830                 88  MEND-SPEC-EXCL-APPLIES         VALUE 'Y'.
005840                 88  MEND-SPEC-EXCL-NOT-APPLIC      VALUE 'N'.

005850         10  MEND-OFF-OCCUP-ADDL-INFO.                            D6H06200
005860             15  MEND-OCCUP-DESCRIPT            PIC X(20).
005870             15  MEND-OFF-OCC-MED-PAY-IND       PIC X.
005880                 88  MEND-OFF-OCC-MED-PAY           VALUE 'Y'.
005890                 88  MEND-OFF-OCC-NO-MED-PAY        VALUE 'N'.
005900             15  MEND-COV-C-INCR-IND            PIC X.
005910                 88  MEND-COV-C-INCREASED           VALUE 'Y'.
005920                 88  MEND-COV-C-NOT-INCREASED       VALUE 'N'.
005930             15  MEND-LOCATION-OF-ADDL-RES      PIC X(20).
005940             15  MEND-OFF-TERR-ZONE             PIC XX.

005950         10  MEND-APP-STR-ADDL-TBL.
005950           12  MEND-APP-STR-ADDL-INFO           OCCURS 2 TIMES
005960                                                INDEXED BY
005960                                                MEND-APP-INDEX.
005970             15  MEND-NY-ZONE                   PIC X.
005980             15  MEND-NY-PROT-CLASS             PIC XXX.
005990             15  MEND-NY-CONST                  PIC X.
006000             15  MEND-NY-COINSURANCE-IND        PIC X.
006010                 88  MEND-DO-NOT-ELIM-COINS         VALUE 'Y'.
006020                 88  MEND-ELIM-COINS-FOR-ADDL-PREM  VALUE 'N'.
006030             15  MEND-APPUR-STRUCT-LIMIT        PIC S9(5)  COMP-3.
006050             15  MEND-TX-ADDL-AMT-SEC-RES-LIM   REDEFINES
006060                  MEND-APPUR-STRUCT-LIMIT       PIC S9(5)  COMP-3.
006070             15  MEND-STRUCT-DESCRIPT           PIC X(20).
006080             15  MEND-APP-STR-ADDRESS-1         PIC X(30).
006090             15  MEND-TX-ADDL-AMT-SEC-ADDR-1    REDEFINES
006100                  MEND-APP-STR-ADDRESS-1        PIC X(30).
006110             15  MEND-APP-STR-ADDRESS-2         PIC X(30).
006120             15  MEND-TX-ADDL-AMT-SEC-ADDR-2    REDEFINES
006130                  MEND-APP-STR-ADDRESS-2        PIC X(30).
006140             15  MEND-APP-STR-ADDRESS-3         PIC X(10).
006150             15  MEND-TX-ADDL-AMT-SEC-ADDR-3    REDEFINES
006160                  MEND-APP-STR-ADDRESS-3        PIC X(10).
006170             15  MEND-APP-STR-ZIP-CODE          PIC S9(5)  COMP-3.
006190             15  MEND-TX-ADDL-AMT-SEC-ZIP       REDEFINES
006200                  MEND-APP-STR-ZIP-CODE         PIC S9(5)  COMP-3.
006210             15  MEND-AP-MED-PAY-IND            PIC X.
006220                 88  MEND-APP-STR-MED-PAY           VALUE 'Y'.
006230                 88  MEND-APP-STR-NO-MED-PAY        VALUE 'N'.
006240             15  MEND-APP-STR-NO-FAMILIES       PIC X.
006250                 88  MEND-1-FAMILY                  VALUE '1'.
006260                 88  MEND-2-FAMILIES                VALUE '2'.
006270             15  MEND-AP-TERR-ZONE              PIC XX.
006280             15  MEND-AP-STR-PREM           PIC S9(4)V99   COMP-3.

006300         10  MEND-ADDL-INTEREST-ADDL-INFO.                        D6H06650
006310             15  MEND-PREMISES-ADDRESS.                           D6H06660
006320                 20  MEND-PREMISES-ADDRESS-1    PIC X(30).
006330                 20  MEND-TX-OUTBLDG-DESC-1     REDEFINES
006340                      MEND-PREMISES-ADDRESS-1   PIC X(30).
006350                 20  MEND-PREMISES-ADDRESS-2    PIC X(20).
962190                 20  MEND-FILLER-1 REDEFINES
962190                             MEND-PREMISES-ADDRESS-2.
962190                   25 MEND-PREMISES-ADDR2-CITY  PIC X(17).
962190                   25 MEND-FILLER-2             PIC X(01).
962190                   25 MEND-PREMISES-ADDR2-STATE PIC X(02).
006360                 20  MEND-TX-OUTBLDG-DESC-2     REDEFINES
006370                      MEND-PREMISES-ADDRESS-2   PIC X(20).
006380                 20  MEND-PREMISES-ADDRESS-ZIP-CODE               D6H06730
006390                                                PIC S9(5)  COMP-3.
006410                 20  MEND-TX-OUTBLDG-LIMIT      REDEFINES
006420                      MEND-PREMISES-ADDRESS-ZIP-CODE
006390                                                PIC S9(5)  COMP-3.
006440             15  MEND-OTHER-PARTY-DATA.                           D6H06790
006450                 20  MEND-NAME-PERS-OR-ORGN     PIC X(30).
006460                 20  MEND-O-P-ADDRESS-1         PIC X(30).
006470                 20  MEND-O-P-ADDRESS-2         PIC X(20).
962190                 20  MEND-FILLER-3 REDEFINES
962190                               MEND-O-P-ADDRESS-2.
962190                   25 MEND-O-P-ADDR2-CITY       PIC X(17).
962190                   25 MEND-FILLER-4             PIC X(01).
962190                   25 MEND-O-P-ADDR2-STATE      PIC X(02).
006480                 20  MEND-O-P-ZIP-CODE          PIC S9(5)  COMP-3.
006500                 20  MEND-INT-IN-PREMISES       PIC X(20).

006510         10  MEND-SNOWMOBILE-ADDL-TBL.
006510           12  MEND-SNOWMOBILE-ADDL-INFO        OCCURS 2 TIMES
006520                                                INDEXED BY
006520                                                MEND-SNO-INDEX.
006530             15  MEND-YEAR                      PIC S99    COMP-3.
006550             15  MEND-TRADE-NAME                PIC X(10).
006560             15  MEND-ID-NUMBER                 PIC X(14).
006570             15  MEND-MODEL                     PIC X(6).
006580             15  MEND-SNOWMOBILE-HP             PIC S999   COMP-3.
006600             15  MEND-MAX-SPEED                 PIC S99    COMP-3.
006620             15  MEND-OPERATORS-UNDER-14        PIC S9     COMP-3.
006640                 88  MEND-NO-OP-UNDER-14            VALUE +0.
006650             15  MEND-SNOWMOBILE-PREM       PIC S9(4)V99   COMP-3.

006670         10  MEND-BUS-PURS-ADDL-TBL.
006670           12  MEND-BUS-PURS-ADDL-INFO          OCCURS 2 TIMES
006680                                                INDEXED BY
006680                                                MEND-BUS-INDEX.
006690             15  MEND-NAME-OF-INSURED           PIC X(20).
006700             15  MEND-OCCUPATION                PIC X(15).
006710             15  MEND-OCC-CLASS                 PIC X.
006720                 88  MEND-CLASS-A                   VALUE 'A'.
006730                 88  MEND-CLASS-B                   VALUE 'B'.
006740                 88  MEND-CLASS-C                   VALUE 'C'.
006750                 88  MEND-CLASS-D                   VALUE 'D'.
006760             15  MEND-BUS-PURS-MED-PAY-IND      PIC X.
006770                 88  MEND-BUS-PURS-MED-PAY          VALUE 'Y'.
006780                 88  MEND-BUS-PURS-NO-MED-PAY       VALUE 'N'.
006790             15  MEND-BUS-PURS-PREM         PIC S9(4)V99   COMP-3.

006810         10  MEND-WATERCRAFT-ADDL-TBL.
006810           12  MEND-WATERCRAFT-ADDL-INFO        OCCURS 2 TIMES
006820                                                INDEXED BY
006820                                                MEND-WAT-INDEX.
006830             15  MEND-W-CRAFT-TYPE              PIC X.
006840                 88  MEND-ENDT-OUTBOARD             VALUE '1'.
006850                 88  MEND-ENDT-INBOARD              VALUE '2'.
006860                 88  MEND-ENDT-SAILBOAT             VALUE '3'.
006870             15  MEND-W-CRAFT-LENGTH            PIC S99    COMP-3.
006890             15  MEND-W-CRAFT-DESCRIPT          PIC X(20).
006900             15  MEND-OWNER-IF-NOT-INSURED      PIC X(20).
006910             15  MEND-OB-MOTOR-DESCRIPT         PIC X(10).
006920             15  MEND-W-CRAFT-HP                PIC S9(3)  COMP-3.
006940             15  MEND-W-CRAFT-SPEED             PIC S9(3)  COMP-3.
006960*                                                                 D6H07310
006970*  THE FORMAT OF THE FOLLOWING TWO DATE FIELDS IS 'MMDD'.         D6H07320
006980*                                                                 D6H07330
006990             15  MEND-EFF-NAVIG-DATE            PIC S9(4)  COMP-3.
007010             15  MEND-EXP-NAVIG-DATE            PIC S9(4)  COMP-3.
007030             15  MEND-ENDT-W-CRAFT-PREM     PIC S9(4)V99   COMP-3.

007050         10  MEND-PER-X-INDEM-ADDL-INFO.                          D6H07400
007060             15  MEND-ADDL-INFO-1                          COMP-3.
007080                 20  MEND-RETAINED-LIMIT        PIC S9(5).
007090                 20  MEND-NO-OF-DWELLINGS       PIC S9.
007100                 20  MEND-NO-VEH-OWNED-FURN     PIC S9.
007110                     88  MEND-NONE-OWNED-FURN       VALUE +9.

007120             15  MEND-AUTO-LIMITS                          COMP-3.
007140                 20  MEND-BI-PERS               PIC S9(3).
007150                 20  MEND-BI-OCC                PIC S9(3).
007160                 20  MEND-PD-OCC                PIC S9(3).
007170                 20  MEND-SING-LIAB-LIM         PIC S9(3).

007180             15  MEND-XL-W-CRAFT-LIMITS                    COMP-3.
007200                 20  MEND-SING-LIM-LIAB         PIC S999.
007210                 20  MEND-BI-EACH-PERSON        PIC S999.
007220                 20  MEND-BI-EACH-OCC           PIC S999.
007230                 20  MEND-PD-EACH-OCC           PIC S999.

007240             15  MEND-XL-W-CRAFT-TBL.
007240               16  MEND-XL-W-CRAFT-INFO         OCCURS 2 TIMES
007250                                                INDEXED BY
007250                                                MEND-XL-INDEX.
007260                 20  MEND-UNDERLYING-COV-IND    PIC X.
007270                     88  MEND-P-AND-I               VALUE 'Y'.
007280                     88  MEND-NOT-P-AND-I           VALUE 'N'.
007290                 20  MEND-XL-W-CRAFT-TYPE       PIC X.
007300                     88  MEND-XL-OUTBOARD           VALUE '1'.
007310                     88  MEND-XL-INBOARD            VALUE '2'.
007320                     88  MEND-XL-SAILBOAT           VALUE '3'.

007330               17  MEND-NUMERIC-W-CRAFT-INFO               COMP-3.
007350                 20  MEND-XL-W-CRAFT-LENGTH     PIC S99.
007360                 20  MEND-XL-W-CRAFT-HP         PIC S999.
007370                 20  MEND-XL-W-CR-HULL-VAL      PIC S9(5).
007380                 20  MEND-XL-MOS-NAVIG          PIC S99.
007390                 20  MEND-XL-W-CRAFT-PREM       PIC S9(4)V99.

007400         10  MEND-EXT-THEFT-ADDL-INFO.                            D6H07750
007410             15  MEND-NY-COUNTY-CODE            PIC XX.
007420                 88  MEND-KINGS                     VALUE '01'.
007430                 88  MEND-BRONX                     VALUE '02'.
007440                 88  MEND-NEW-YORK                  VALUE '03'.
007450                 88  MEND-QUEENS                    VALUE '04'.
007460                 88  MEND-WESTCHESTER-RICHMOND      VALUE '05'.
007470                 88  MEND-NASSAU                    VALUE '06'.
007480                 88  MEND-ERIE-MONROE-ONONDAGA      VALUE '07'.
007490                 88  MEND-PUTNAM-ROCKLAND-SUFFOLK   VALUE '08'.
007500                 88  MEND-REMAINDER-OF-STATE        VALUE '09'.

007510         10  MEND-CREDIT-CARD-ADDL-INFO.                          D6H07860
007520             15  MEND-CREDIT-CARD-LIMIT         PIC S9(5)  COMP-3.
007540             15  MEND-TX-TV-RADIO-ANTENNA-LIM   REDEFINES
007550                  MEND-CREDIT-CARD-LIMIT        PIC S9(5)  COMP-3.

007560         10  MEND-ADDL-RES-ADDL-INFO.                             D6H07910
007570             15  MEND-ADDL-RES-TERR-NO          PIC S99    COMP-3.
007590             15  MEND-ADDL-RES-MED-PAY-IND      PIC X.
007600                 88  MEND-ADDL-RES-MED-PAY          VALUE 'Y'.
007610                 88  MEND-ADDL-RES-NO-MED-PAY       VALUE 'N'.
007620             15  MEND-TX-OFF-PROF-MED-PAY       REDEFINES
007630                  MEND-ADDL-RES-MED-PAY-IND     PIC X.
007640                 88  MEND-TX-OFF-PROF-YES-MED-PAY   VALUE 'Y'.
007650                 88  MEND-TX-OFF-PROF-NO-MED-PAY    VALUE 'N'.
007660             15  MEND-ADDL-RES-NO-OF-FAM        PIC X.
007670                 88  MEND-ADDL-RES-1-FAMILY         VALUE '1'.
007680                 88  MEND-ADDL-RES-2-FAMILIES       VALUE '2'.
007690             15  MEND-TX-LOC-IND                REDEFINES
007700                  MEND-ADDL-RES-NO-OF-FAM       PIC X.
007710                 88  MEND-TX-OFF-PROF-DESC-DWEL     VALUE '1'.
007720                 88  MEND-TX-OFF-PROF-ADDL-RES      VALUE '2'.
007730             15  MEND-ADDL-RES-ADDRESS-1        PIC X(30).
007740             15  MEND-TX-OFF-PROF-OCCUPANCY     REDEFINES
007750                  MEND-ADDL-RES-ADDRESS-1       PIC X(30).
007760             15  MEND-ADDL-RES-ADDRESS-2        PIC X(30).
007770             15  MEND-TX-OFF-PROF-LOC-1         REDEFINES
007780                  MEND-ADDL-RES-ADDRESS-2       PIC X(30).
007790             15  MEND-ADDL-RES-ADDRESS-3        PIC X(30).
962190             15  MEND-FILLER-5 REDEFINES
962190                              MEND-ADDL-RES-ADDRESS-3.
962190               20 MEND-ADDL-RES-ADDR3-CITY      PIC X(21).
962190               20 MEND-FILLER-6                 PIC X(01).
962190               20 MEND-ADDL-RES-ADDR3-ST        PIC X(02).
962190               20 MEND-FILLER-7                 PIC X(01).
962190               20 MEND-ADDL-RES-ADDR3-ZIP       PIC X(05).
007800             15  MEND-TX-OFF-PROF-LOC-2         REDEFINES
007810                  MEND-ADDL-RES-ADDRESS-3       PIC X(30).

007820         10  MEND-MISC-ENDT-ADDL-INFO.                            D6H08170
007830             15  MEND-DESCRIPTION               PIC X(40).


007840     05  M-INSURED-AND-ADDRESS-SEC.                               D6H08190
007850         10  MINS-ACTION-TYPE                   PIC X.
007860             88  MINS-INITIAL-VALUE                 VALUE ' '.
007870             88  MINS-ADD-ONLY                      VALUE 'A'.
007880             88  MINS-CHANGE-ONLY                   VALUE 'C'.

007890         10  MINS-MAIL-ADDRESS.                                   D6H08240
007900             15  MINS-MAIL-ADDRESS-1            PIC X(30).
007910             15  MINS-MAIL-ADDRESS-2            PIC X(30).
007920             15  MINS-MAIL-ADDRESS-3            PIC X(10).
007930             15  MINS-MAIL-ZIP-CODE             PIC S9(5)  COMP-3.

007950         10  MINS-NAMED-INSURED-INFO.                             D6H08300
007960             15  MINS-INSURED-NAME-1            PIC X(35).
007970             15  MINS-INSURED-NAME-2            PIC X(35).
007980             15  MINS-INSURED-LAST-NAME         PIC X(20).
007990             15  MINS-SOCIAL-SECURITY-NO        PIC X(11).
008000             15  MINS-OCCUPATION-CODE           PIC XX.
008010             15  MINS-ILL-MINE-SUB              REDEFINES
008020                  MINS-OCCUPATION-CODE          PIC XX.
008030             15  MINS-VIR-MINE-SUB              REDEFINES
008040                  MINS-OCCUPATION-CODE          PIC XX.

008050         10  MINS-STATE                         PIC S99    COMP-3.
008070         10  MINS-IDENT-TERR                    PIC S99    COMP-3.
008090         10  MINS-TAX-DISTRICT                  PIC S9(4)  COMP-3.
008110         10  MINS-CRSP-PLACE-CODE               PIC X(5).


008120     05  M-DWELLING-INFO-SEC.                                     D6H08470
008130         10  MDWE-ACTION-TYPE                   PIC X.
008140             88  MDWE-INITIAL-VALUE                 VALUE ' '.
008150             88  MDWE-ADD-ONLY                      VALUE 'A'.
008160             88  MDWE-CHANGE-ONLY                   VALUE 'C'.

008170         10  MDWE-ROOF-INDICATOR                PIC X.
936330             88  MDWE-ASPH-FIBER-COMP               VALUE 'O'.
936330             88  MDWE-CLAY-TILE-SLATE               VALUE 'P'.
936330             88  MDWE-WOODSHAKE                     VALUE 'Q'.
936330             88  MDWE-WOODSHINGLE                   VALUE 'R'.
936330             88  MDWE-ARCHSHINGLE                   VALUE 'S'.
936330             88  MDWE-TAR-GRAVEL                    VALUE 'T'.
936330             88  MDWE-RUBBER                        VALUE 'U'.
936330             88  MDWE-FOAM                          VALUE 'V'.
936330             88  MDWE-CORRSTEEL-CORRMETAL           VALUE 'W'.
936330             88  MDWE-CONCTILE                      VALUE 'X'.
936330             88  MDWE-OTHER                         VALUE 'Z'.

008200         10  MDWE-OCCUPANCY-CODE                PIC X.
008210             88  MDWE-ONE-FAMILY                    VALUE '1'.
008220             88  MDWE-TWO-FAMILIES                  VALUE '2'.
008230             88  MDWE-THREE-FAMILIES                VALUE '3'.
008240             88  MDWE-FOUR-FAMILIES                 VALUE '4'.
008250             88  MDWE-TENANT                        VALUE '5'.
008260             88  MDWE-CONDOMINIUM                   VALUE '6'.
008260             88  MDWE-COOPERATIVE                   VALUE '7'.

008270         10  MDWE-TX-RESIDENCE-TYPE             REDEFINES
008280              MDWE-OCCUPANCY-CODE               PIC X.
008290             88  MDWE-TX-DWELLING                   VALUE '1'.
008300             88  MDWE-TX-TOWN-ROW-HOUSE             VALUE '2'.
008310             88  MDWE-TX-CONDOMINIUM                VALUE '3'.
008320             88  MDWE-TX-TENANT-APARTMENT           VALUE '4'.
008330             88  MDWE-TX-TENANT-DWELLING            VALUE '5'.

008340         10  MDWE-FEET-FROM-HYDRANT             PIC S9(4)  COMP-3.
008360         10  MDWE-MILES-FROM-FIRE-DEPT          PIC S99    COMP-3.

008380         10  MDWE-CONSTRUCTION-TYPE             PIC X.
008390             88  MDWE-FRAME                         VALUE '1'.
008400             88  MDWE-BRICK-STONE-VENEER            VALUE '2'.
008410             88  MDWE-BRICK-STONE-MASONRY           VALUE '3'.
008420             88  MDWE-FIRE-RESISTIVE                VALUE '4'.
008430             88  MDWE-FRAME-WITH-SIDING             VALUE '5'.
HO3098             88  MDWE-BRICK-STONE-MV-10-33          VALUE '6'.
HO3098             88  MDWE-BRICK-STONE-MV-34-66          VALUE '7'.
008440             88  MDWE-MODULAR                       VALUE '9'.

008450         10  MDWE-TX-CONSTRUCTION-TYPE          REDEFINES
008460              MDWE-CONSTRUCTION-TYPE            PIC X.
008470             88  MDWE-TX-FRAME-ALUM                 VALUE '1'.
008480             88  MDWE-TX-FRAME                      VALUE '2'.
008490             88  MDWE-TX-BRICK-STONE-VENEER         VALUE '3'.
008500             88  MDWE-TX-BRICK-STONE-MASONRY        VALUE '4'.
008510             88  MDWE-TX-STUCCO-ASBESTOS            VALUE '5'.

008520         10  MDWE-HO4-GROUP                     PIC S99    COMP-3.
008540             88  MDWE-GROUP-ONE                     VALUE +1.
008550             88  MDWE-GROUP-TWO                     VALUE +2.


008560     05  M-CODING-INFORMATION-SEC.                                D6H08940
008570         10  MCOD-ACTION-TYPE                   PIC X.
008580             88  MCOD-INITIAL-VALUE                 VALUE ' '.
008590             88  MCOD-ADD-ONLY                      VALUE 'A'.
008600             88  MCOD-CHANGE-ONLY                   VALUE 'C'.

008610         10  MCOD-NEW-YORK-PROVISIONS.                            D6H08990
008620             15  MCOD-COINSURANCE               PIC X.
008630                 88  MCOD-COINS-APPLIES             VALUE 'Y'.
008640                 88  MCOD-COINS-DOESNT-APPLY        VALUE 'N'.
008650             15  MCOD-OFF-PREMISES-THEFT        PIC X.
008660                 88  MCOD-OFF-PREM-APPLIES          VALUE 'Y'.
008670                 88  MCOD-OFF-PREM-DOESNT-APPLY     VALUE 'N'.

008680         10  MCOD-CALIFORNIA-PROVISIONS.                          D6H09060
008690             15  MCOD-ROOMERS-BOARDERS-IND      PIC X.
008700                 88  MCOD-WITH-ROOMERS              VALUE 'Y'.
008710                 88  MCOD-WITHOUT-ROOMERS           VALUE 'N'.
008720             15  MCOD-CANTILEVER-IND            PIC X.
008730                 88  MCOD-WITH-CANTILEVER           VALUE 'Y'.
008740                 88  MCOD-WITHOUT-CANTILEVER        VALUE 'N'.
008750             15  MCOD-AVERAGE-CONDITION-IND     PIC X.
008760                 88  MCOD-BETTER-THAN-AVERAGE       VALUE 'Y'.
008770                 88  MCOD-NOT-BETTER-THAN-AVERAGE   VALUE 'N'.
008780             15  MCOD-COMMERCIAL-PROP-IND       PIC X.
008790                 88  MCOD-WITHIN-100-FEET           VALUE 'Y'.
008800                 88  MCOD-OVER-100-FEET             VALUE 'N'.
008810             15  MCOD-CALIFORNIA-BRUSH-AREA     PIC X.
008820                 88  MCOD-BRUSH-AREA                VALUE 'Y'.
008830                 88  MCOD-NOT-BRUSH-AREA            VALUE 'N'.

008840         10  MCOD-PROTECTION.                                     D6H09220
008850             15  MCOD-PROTECTION-CLASS          PIC X(4).
008860             15  MCOD-NIASA-PROT-CLASS          PIC S999   COMP-3.

008880         10  MCOD-FIRE-DISTRICT-NAME            PIC X(15).
008890         10  MCOD-BASIC-PREM-GROUP              PIC XXX.

008900       07  MCOD-NUMERIC-CODING-INFO                        COMP-3.
008920         10  MCOD-ZONE.                                           D6H09300
008930             15  MCOD-LOCATION-ZONE             PIC S99.
008940             15  MCOD-STAT-ZONE                 PIC S99.
008950         10  MCOD-SUB-ZONE                      PIC S9.
008960             88  MCOD-INLAND                        VALUE +1.
008970             88  MCOD-SEACOAST                      VALUE +2.
008980             88  MCOD-BEACH                         VALUE +3.
008990         10  MCOD-FIRE-DISTRICT-CODE            PIC S9(4).
009000         10  MCOD-SOUTHERN-LOCATION             PIC S9.
009010             88  MCOD-INSIDE-CITY-LIMITS            VALUE +1.
009020             88  MCOD-INSIDE-PROT-SUB-AREA          VALUE +2.
009030             88  MCOD-INSIDE-FIRE-DISTRICT          VALUE +3.
009040             88  MCOD-RURAL-FIRE-PROT               VALUE +4.
009050         10  MCOD-CAROLINA-MINN-VAL-CLAUSE      PIC S9(7).

009060       07  MCOD-NEW-ENGLAND-INFO.                                 D6H09440
009070         10  MCOD-PRIVATE-FIRE-SYSTEM           PIC X.
009080             88  MCOD-PRIVATE-SYSTEM                VALUE 'Y'.
009090             88  MCOD-NO-PRIVATE-SYSTEM             VALUE 'N'.
009100         10  MCOD-DWELLING-PROP-MIN-RATES       PIC X.
009110             88  MCOD-TABLE-IV-RATES                VALUE '4'.
009120             88  MCOD-TABLE-V-RATES                 VALUE '5'.


009130     05  M-APPRAISAL-INFO-SEC.                                    D6H09510
009140         10  MAPP-ACTION-TYPE                   PIC X.
009150             88  MAPP-INITIAL-VALUE                 VALUE ' '.
009160             88  MAPP-ADD-ONLY                      VALUE 'A'.
009170             88  MAPP-CHANGE-ONLY                   VALUE 'C'.
009180         10  MAPP-LAST-UPGRADE-DATE             PIC S9(5)  COMP-3.
009200         10  MAPP-TOTAL-INFO-IND                PIC S9     COMP-3.
009220             88  MAPP-USE-ONLY-LOCATION-CODE        VALUE +0.
009230             88  MAPP-ALL-INFO-PRESENT              VALUE +1.
009240         10  MAPP-LOCATION-CODE                 PIC S999   COMP-3.
009260         10  MAPP-MICH-AEST-IND                 REDEFINES
009270              MAPP-LOCATION-CODE                PIC S999   COMP-3.
009290         10  MAPP-CURRENT-APP-FACTOR            PIC S9V999 COMP-3.
009310         10  MAPP-PREVIOUS-APP-FACTOR           PIC S9V999 COMP-3.
009330         10  MAPP-BASEMENT-IND                  PIC X.
009340             88  MAPP-WITH-BASEMENT                 VALUE 'Y'.
009350             88  MAPP-WITHOUT-BASEMENT              VALUE 'N'.
009360             88  MAPP-WITH-CRAWL-SPACE              VALUE '1'.
009370             88  MAPP-SLAB-ON-GROUND                VALUE '2'.
009380             88  MAPP-BASEMENT-YES                  VALUE '3'.
009410         10  MAPP-CONSTR-QUALITY                PIC X.
009420             88  MAPP-LUXURY                        VALUE '1'.
009430             88  MAPP-CUSTOM                        VALUE '2'.
009440             88  MAPP-STANDARD                      VALUE '3'.
009450             88  MAPP-ECONOMY                       VALUE '4'.
009460             88  MAPP-CLASS-I                       VALUE 'A'.
009470             88  MAPP-CLASS-II                      VALUE 'B'.
009480             88  MAPP-CLASS-III                     VALUE 'C'.
009490             88  MAPP-CLASS-IV                      VALUE 'D'.
009500         10  MAPP-EXTERIOR-FINISH               PIC X.
009510             88  MAPP-WOOD-SIDING                   VALUE '1'.
009520             88  MAPP-BRICK-VENEER                  VALUE '2'.
009530             88  MAPP-STONE-VENEER                  VALUE '3'.
009540         10  MAPP-FLOOR-TYPE                    PIC X.
009550             88  MAPP-ONE-STORY                     VALUE  '1'.
009560             88  MAPP-ONE-AND-ONE-HALF-STORY        VALUE  '2'.
009570             88  MAPP-TWO-STORY                     VALUE  '3'.
009580             88  MAPP-THREE-STORY                   VALUE  '4'.
009590             88  MAPP-BI-LEVEL                      VALUE  '5'. 
009600             88  MAPP-TRI-LEVEL                     VALUE  '6'.
009610             88  MAPP-TWO-AND-ONE-HALF-STORY        VALUE  '7'.
009620         10  MAPP-FLOOR-AREA-FT                 PIC S9(4)  COMP-3.
009640         10  MAPP-NO-FULL-BATHS                 PIC S9     COMP-3.
009660         10  MAPP-NO-HALF-BATHS                 PIC S9     COMP-3.
009680         10  MAPP-FINISHED-ATTIC-VAL            PIC S9(5)  COMP-3.
009700         10  MAPP-FINISHED-BASEMENT-VAL         PIC S9(5)  COMP-3. 
009720         10  MAPP-ATTACHED-PORCHES-VAL          PIC S9(5)  COMP-3.
009740         10  MAPP-STOOPS-VAL                    PIC S9(5)  COMP-3.
009760         10  MAPP-BREEZEWAYS-VAL                PIC S9(5)  COMP-3.
009780         10  MAPP-GARAGE-IND                    PIC X.
009790             88  MAPP-ATTACHED                      VALUE '1'.
009800             88  MAPP-DETACHED                      VALUE '2'.
009810             88  MAPP-BASEMENT                      VALUE '3'.
009820             88  MAPP-BUILT-IN                      VALUE '4'.
009830             88  MAPP-CARPORT                       VALUE '5'.
009840             88  MAPP-NONE                          VALUE '6'.
009850         10  MAPP-NEW-GARAGE-IND                REDEFINES
009860              MAPP-GARAGE-IND                   PIC X.
009870             88  MAPP-ATT-FRAME                     VALUE '1'.
009880             88  MAPP-ATT-MASON                     VALUE '2'.
009890             88  MAPP-NEW-DETACHED                  VALUE '3'.
009900             88  MAPP-NEW-BSMT                      VALUE '4'.
009910             88  MAPP-NEW-BUILT-IN                  VALUE '5'.
009920             88  MAPP-CARPORT-ONLY                  VALUE '6'.
009930             88  MAPP-CARPORT-STOR                  VALUE '7'.
009940             88  MAPP-NEW-NONE                      VALUE '8'.
009950         10  MAPP-GARAGE-SIZE                   PIC X.
009960             88  MAPP-NO-GARAGE                     VALUE '0'.
009970             88  MAPP-ONE-CAR                       VALUE '1'.
009980             88  MAPP-TWO-CARS                      VALUE '2'.
009990             88  MAPP-THREE-CARS                    VALUE '3'.
010000             88  MAPP-FOUR-CARS                     VALUE '4'.
010010             88  MAPP-FIVE-CARS                     VALUE '5'.
010020         10  MAPP-FIREPLACE-VAL                 PIC S9(5)  COMP-3.
010040         10  MAPP-AIR-COND-VAL                  PIC S9(5)  COMP-3.


010060     05  M-ACTUAL-PREMISES-LOCATION-SEC.                          D6H10440
010070         10  MACT-ACTION-TYPE                   PIC X.
010080             88  MACT-INITIAL-VALUE                 VALUE ' '.
010090             88  MACT-ADD-ONLY                      VALUE 'A'.
010100             88  MACT-CHANGE-ONLY                   VALUE 'C'.
010110             88  MACT-ELIMINATE-ONLY                VALUE 'E'.
010120         10  MACT-RESID-PREMISES-ADDRESS.                         D6H10500
010130             15  MACT-RESID-ADDRESS-1           PIC X(30).
010140             15  MACT-RESID-ADDRESS-2           PIC X(30).
010150             15  MACT-RESID-ADDRESS-3           PIC X(30).
962190             15  MACT-FILLER-1 REDEFINES
962190                            MACT-RESID-ADDRESS-3.
962190               20 MACT-RESID-ADDR3-CITY         PIC X(21).
962190               20 MACT-FILLER-2                 PIC X(01).
962190               20 MACT-RESID-ADDR3-STATE        PIC X(02).
962190               20 MACT-FILLER-3                 PIC X(01).
962190               20 MACT-RESID-ADDR3-ZIP          PIC X(05).


010160     05  M-ADDL-RES-PREMISES-INFO-SEC.                            D6H10540
010170         10  MADD-ACTION-TYPE                   PIC X.
010180             88  MADD-INITIAL-VALUE                 VALUE ' '.
010190             88  MADD-ADD-ONLY                      VALUE 'A'.
010200             88  MADD-CHANGE-ONLY                   VALUE 'C'.
010210             88  MADD-ELIMINATE-ONLY                VALUE 'E'.
010220         10  MADD-ADDL-RES-PREMISES-ADDRESS.                      D6H10600
010230             15  MADD-ADDL-R-PREMISES-ADDR-1    PIC X(30).
010240             15  MADD-ADDL-R-PREMISES-ADDR-2    PIC X(30).
010250             15  MADD-ADDL-R-PREMISES-ADDR-3    PIC X(30).
962190             15  MADD-FILLER-1 REDEFINES
962190                                  MADD-ADDL-R-PREMISES-ADDR-3.
962190               20 MADD-ADDL-R-PREMISES-A3-CITY  PIC X(21).
962190               20 MADD-FILLER-2                 PIC X(01).
962190               20 MADD-ADDL-R-PREMISES-A3-STATE PIC X(02).
962190               20 MADD-FILLER-3                    PIC X(01).
962190               20 MADD-ADDL-R-PREMISES-ADDR3-ZIP   PIC X(05).
010260             15  MADD-ADDL-RES-TERR-ZONE        PIC S99    COMP-3.


010280     05  M-FIRST-MORTGAGEE-INFO-SEC.                              D6H10660
010290         10  MFIR-ACTION-TYPE                   PIC X.
010300             88  MFIR-INITIAL-VALUE                 VALUE ' '.
010310             88  MFIR-ADD-ONLY                      VALUE 'A'.
010320             88  MFIR-CHANGE-ONLY                   VALUE 'C'.
010330             88  MFIR-ELIMINATE-ONLY                VALUE 'E'.
010340         10  MFIR-FIRST-MORT-NAME-1             PIC X(30).
010350         10  MFIR-FIRST-MORT-NAME-2             PIC X(30).
010360         10  MFIR-FIRST-MORTGAGEE-ADDRESS.                        D6H10740
010370             15  MFIR-FIRST-MORT-ADDR-1         PIC X(20).
010380             15  MFIR-FIRST-MORT-ADDR-2         PIC X(20).
962190             15  MFIR-FILLER-1 REDEFINES
962190                                 MFIR-FIRST-MORT-ADDR-2.
962190               20  MFIR-FIRST-MORT-ADDR2-CITY   PIC X(17).
962190               20  MFIR-FILLER-2                PIC X(01).
962190               20  MFIR-FIRST-MORT-ADDR2-STAT   PIC X(02).
010390             15  MFIR-FIRST-MORT-ZIP-CODE       PIC S9(5)  COMP-3.
010410         10  MFIR-FIRST-MORT-LOAN-NO            PIC X(10).
010420         10  MP82-PROTECTION-OFF                PIC XX.


010430     05  M-SECOND-MORTGAGEE-INFO-SEC.                             D6H10810
010440         10  MSEC-ACTION-TYPE                   PIC X.
010450             88  MSEC-INITIAL-VALUE                 VALUE ' '.
010460             88  MSEC-ADD-ONLY                      VALUE 'A'.
010470             88  MSEC-CHANGE-ONLY                   VALUE 'C'.
010480             88  MSEC-ELIMINATE-ONLY                VALUE 'E'.
010490         10  MSEC-SECOND-MORT-NAME-1            PIC X(30).
010500         10  MSEC-SECOND-MORT-NAME-2            PIC X(30).
010510         10  MSEC-SECOND-MORTGAGEE-ADDRESS.                       D6H10890
010520             15  MSEC-SECOND-MORT-ADDR-1        PIC X(20).
010530             15  MSEC-SECOND-MORT-ADDR-2        PIC X(20).
962190             15  MSEC-FILLER-1 REDEFINES
962190                                 MSEC-SECOND-MORT-ADDR-2.
962190               20 MSEC-SECOND-MORT-ADDR2-CITY    PIC X(17).
962190               20 MSEC-FILLER-2                  PIC X(01).
962190               20 MSEC-SECOND-MORT-ADDR2-STATE   PIC X(02).
010540             15  MSEC-SECOND-MORT-ZIP-CODE      PIC S9(5)  COMP-3.
010560         10  MSEC-SECOND-MORT-LOAN-NO           PIC X(10).
010570         10  MP82-PROTECTION-ONS                PIC XX.


010580     05  M-ALTERNATE-PAYOR-INFO-SEC.                              D6H10960
010590         10  MALT-ACTION-TYPE                   PIC X.
010600             88  MALT-INITIAL-VALUE                 VALUE ' '.
010610             88  MALT-ADD-ONLY                      VALUE 'A'.
010620             88  MALT-CHANGE-ONLY                   VALUE 'C'.
010630             88  MALT-ELIMINATE-ONLY                VALUE 'E'.
010640         10  MALT-ALT-PAYOR-NAME                PIC X(30).
010650         10  MALT-ALT-PAYOR-ADDRESS.                              D6H11030
010660             15  MALT-ALT-PAYOR-ADDR-1          PIC X(20).
010670             15  MALT-ALT-PAYOR-ADDR-2          PIC X(20).
962190             15  MALT-FILLER-1 REDEFINES
962190                                  MALT-ALT-PAYOR-ADDR-2.
962190                20 MALT-ALT-PAYOR-ADDR2-CITY    PIC X(17).
962190                20 MALT-FILLER-2                PIC X(01).
962190                20 MALT-ALT-PAYOR-ADDR2-STATE   PIC X(02).
010680             15  MALT-ALT-PAYOR-ZIP-CODE        PIC S9(5)  COMP-3.
010700         10  MALT-ALT-PAYOR-ID-NUMBER           PIC X(10).
010710         10  MP82-FAMILY-INL-OFF                PIC X.
010720         10  MP82-FAMILY-INL-ONS                PIC X.


010730     05  M-EXCEPTION-INFO-SEC.                                    D6H11110
010740       07  MEXC-EXCEPTION-INFO                  OCCURS 4 TIMES
010750                                                INDEXED BY
010750                                                MEXC-INDEX.
010760         10  MEXC-ACTION-TYPE                   PIC X.
010770             88  MEXC-INITIAL-VALUE                 VALUE ' '.
010780             88  MEXC-ADD-ONLY                      VALUE 'A'.
010790             88  MEXC-CHANGE-ONLY                   VALUE 'C'.
010800             88  MEXC-ELIMINATE-ONLY                VALUE 'E'.
010810         10  MEXC-EXCEPTION-TYPE                PIC X.
010820             88  MEXC-DWELLING-NOT-SEASONAL         VALUE 'A'.
010830             88  MEXC-NO-BUSINESS-PURSUITS          VALUE 'B'.
010840             88  MEXC-DWELLING-IS-ONLY-PREMISES     VALUE 'C'.
010850             88  MEXC-NO-RESIDENCE-EMPLOYEES        VALUE 'D'.
010860             88  MEXC-NO-WATERCRAFT                 VALUE 'E'.
010870             88  MEXC-TX-NOT-MORE-2-FAM             VALUE '1'.
010880             88  MEXC-TX-DWEL-IS-ONLY-PREMISES      VALUE '2'.
010890             88  MEXC-TX-NO-BUSINESS-PURSUITS       VALUE '3'.
010900         10  MEXC-EXCEPTION-DESCRIPTION         PIC X(30).
010910       07  MEXC-NUMBER-RES-EMPL                 PIC S9     COMP-3.


010930     05  M-PREM-INFO-SEC.                                         D6H11310
010940         10  MPRE-RATE-MODE-IND                 PIC X.
010950             88  MPRE-SHORT-RATE                    VALUE '1'.
010960             88  MPRE-PRO-RATE                      VALUE '2'.
010970             88  MPRE-FLAT-RATE                     VALUE '3'.

010980       08  MPRE-POL-PREMIUMS                               COMP-3.
011000         10  MPRE-PREM-TOTAL-POLICY             PIC S9(5)V99.
011010         10  MPRE-PREM-BASIC-COVG               PIC S9(5)V99.
011020         10  MPRE-PREM-SEC-RES                  PIC S9(5)V99.
011030         10  MPRE-PREM-INCR-UNSCHED-PROP        PIC S9(5)V99.
011040         10  MPRE-PREM-INCR-ADDL-LIV-EXP        PIC S9(5)V99.
011050         10  MPRE-PREM-INCR-LIAB                PIC S9(5)V99.
011060         10  MPRE-PREM-RES-EMPLS                PIC S9(5)V99.
011070         10  MPRE-TX-PREM-DED-ADJ-CLS-1-3       REDEFINES
011080              MPRE-PREM-RES-EMPLS               PIC S9(5)V99.
011090         10  MPRE-PREM-COV-ENDTS                PIC S9(5)V99.
011100         10  MPRE-PREM-REINSURANCE              PIC S9(5)V99.
011110         10  MPRE-PREM-FAIR-PLAN                REDEFINES
011120              MPRE-PREM-REINSURANCE             PIC S9(5)V99.
011130         10  MPRE-TX-PREM-DED-ADJ-CLS-2         REDEFINES
011140              MPRE-PREM-REINSURANCE             PIC S9(5)V99.

011150       08  MPRE-PREMIUMS-OFFSET                            COMP-3.
011170         10  MPRE-PREM-TOTAL-ADDL-RETN-OFF      PIC S9(5)V99.
011180         10  MPRE-PREM-ADDL-RETN-BASIC-OFF      PIC S9(5)V99.
011190         10  MPRE-PREM-AR-SEC-RES-OFF           PIC S9(5)V99.
011200         10  MPRE-INCR-LIM-AR-INFO-OFF.                           D6H11580
011210             15  MPRE-PREM-AR-INC-UNSC-PROP-OFF PIC S9(5)V99.
011230             15  MPRE-PREM-AR-INC-AD-LIV-EX-OFF PIC S9(5)V99.
011250             15  MPRE-PREM-AR-INCR-LIAB-OFF     PIC S9(5)V99.
011260         10  MPRE-PREM-AR-RES-EMPLS-OFF         PIC S9(5)V99.
011270         10  MPRE-TX-DED-ADJ-CLS-1-3-OFF        REDEFINES
011280              MPRE-PREM-AR-RES-EMPLS-OFF        PIC S9(5)V99.
011290         10  MPRE-PREM-AR-COV-ENDTS-OFF         PIC S9(5)V99.
011300         10  MPRE-PREM-AR-REINSURANCE-OFF       PIC S9(5)V99.
011310         10  MPRE-PREM-AR-FAIR-PLAN-OFF         REDEFINES
011320              MPRE-PREM-AR-REINSURANCE-OFF      PIC S9(5)V99.
011330         10  MPRE-TX-DED-ADJ-CLS-2-OFF          REDEFINES
011340              MPRE-PREM-AR-REINSURANCE-OFF      PIC S9(5)V99.
011350         10  MPRE-SURCHARGE-AR-OFF              PIC S999V99.

011360       08  MPRE-PREMIUM-ONSET                              COMP-3.
011380         10  MPRE-PREM-TOTAL-ADDL-RETN-ONS      PIC S9(5)V99.
011390         10  MPRE-PREM-ADDL-RETN-BASIC-ONS      PIC S9(5)V99.
011400         10  MPRE-PREM-AR-SEC-RES-ONS           PIC S9(5)V99.
011410         10  MPRE-INC-LIM-AR-INFO-ONS.                            D6H11790
011420             15  MPRE-PREM-AR-INC-UNSC-PROP-ONS PIC S9(5)V99.
011440             15  MPRE-PREM-AR-INC-AD-LIV-EX-ONS PIC S9(5)V99.
011460             15  MPRE-PREM-AR-INCR-LIAB-ONS     PIC S9(5)V99.
011470         10  MPRE-PREM-RES-EMPLS-ONS            PIC S9(5)V99.
011480         10  MPRE-TX-DED-ADJ-CLS-1-3-ONS        REDEFINES
011490              MPRE-PREM-RES-EMPLS-ONS           PIC S9(5)V99.
011500         10  MPRE-PREM-AR-COV-ENDTS-ONS         PIC S9(5)V99.
011510         10  MPRE-PREM-AR-REINSURANCE-ONS       PIC S9(5)V99.
011520         10  MPRE-PREM-AR-FAIR-PLAN-ONS         REDEFINES
011530              MPRE-PREM-AR-REINSURANCE-ONS      PIC S9(5)V99.
011540         10  MPRE-TX-DED-ADJ-CLS-2-ONS          REDEFINES
011550              MPRE-PREM-AR-REINSURANCE-ONS      PIC S9(5)V99.
011560         10  MPRE-SURCHARGE-AR-ONS              PIC S999V99.

011570       08  MPRE-MISC-PREM-INFO                             COMP-3.
011590         10  MPRE-CANC-REFUND                   PIC S9(5)V99.
011600         10  MPRE-CANC-REINST-PREM              PIC S9(5)V99.
011610         10  MPRE-DIVIDEND-AMT                  PIC S9(5)V99.
011620         10  MPRE-SURCHARGE-AMT                 PIC S999V99.


011640     05  M-OVERRIDE-INFO-TBL.
011640         10  M-OVERRIDE-INFO-SEC                OCCURS 5 TIMES
011650                                                INDEXED BY
011650                                                MOVE-INDEX.
011660             15  MOVE-OVERRIDE                  PIC S999   COMP-3.
011680             15  MOVE-OV-UND-INITIALS           PIC XXX.


011690     05  M-STAT-AND-ACCT-CODES-SEC.                               D6H12070
011700       07  MSTA-STAT-ACCT-CODES-OFFSET.                           D6H12080
011710         10  MSTA-TERRITORY-OFF.                                  D6H12090
011720             15  MSTA-STATE-OFF                 PIC S99    COMP-3.
011740             15  MSTA-ZONE-OFF                  PIC S99    COMP-3.
011760             15  MSTA-PLACE-OFF                 PIC S9(5)  COMP-3.
011780         10  MSTA-LINE-OF-BUSINESS-OFF          PIC S999   COMP-3.
011800         10  MSTA-CLASSIFICATION-OFF.                             D6H12180
011810             15  MSTA-FORM-OFF                  PIC X.
011820             15  MSTA-NO-FAMILIES-OFF           PIC X.
011830             15  MSTA-COVERAGE-OFF              PIC X.
011840             15  MSTA-CONSTRUCTION-OFF          PIC X.
011850             15  MSTA-PUBLIC-PROTECTION-OFF     PIC X.
011860             15  MSTA-PER-CENT-MANUAL-OFF       PIC X.
011870             15  MSTA-DED-TYPE-OFF              PIC X.
011880             15  MSTA-DED-AMOUNT-OFF            PIC X.
011890         10  MSTA-LIABILITY-OFF.                                  D6H12270
011900             15  MSTA-AMT-INS-COV-A-OR-C-OFF    PIC 999.
011910             15  MSTA-UNSCHED-PERS-PROP-OFF     PIC XX.
011920             15  MSTA-MED-PAY-LIMIT-OFF         PIC X.
011930         10  MSTA-CREDIT-CARD-ENDOR-OFF         PIC X.
011940         10  MSTA-WATERCRAFT-ENDOR-OFF          PIC X.
011950         10  MSTA-WATERCRAFT-TYPE-OFF           REDEFINES
011960              MSTA-WATERCRAFT-ENDOR-OFF         PIC X.
011970         10  MSTA-FARM-COMP-PERS-ENDOR-OFF      PIC X.
011980         10  MSTA-INFLATION-GUARD-ENDOR-OFF     PIC X.
011990         10  MSTA-SNOWMOBILE-ENDOR-OFF          PIC X.
012000         10  MSTA-SNOWMOBILE-NUMBER-OFF         REDEFINES
012010              MSTA-SNOWMOBILE-ENDOR-OFF         PIC X.
012020         10  MSTA-AGE-OF-DWELLING-OFF           PIC XX.
012030         10  MSTA-YEAR-OF-CONSTRUCTION-OFF      REDEFINES
012040              MSTA-AGE-OF-DWELLING-OFF          PIC XX.
012050         10  MSTA-PROTECTION-X-OFF              PIC X.
012060         10  MSTA-WATERCRAFT-NUMBER-OFF         PIC X.
012070         10  MSTA-WATERCRAFT-EXPOSURE-OFF       PIC X.
012080         10  MSTA-COV-E-LIMIT-OFF               PIC X.
012090         10  MSTA-COV-F-LIMIT-OFF               PIC X.
012100         10  MSTA-FILLER-OFF                    PIC XX.
012110         10  MSTA-TX-PERCENT-MANUAL-OFF         REDEFINES
012120              MSTA-FILLER-OFF                   PIC XX.

012130       07  MSTA-STAT-ACCT-CODES-ONSET.                            D6H12510
012140         10  MSTA-TERRITORY-ONS.                                  D6H12520
012150             15  MSTA-STATE-ONS                 PIC S99    COMP-3.
012170             15  MSTA-ZONE-ONS                  PIC S99    COMP-3.
012190             15  MSTA-PLACE-ONS                 PIC S9(5)  COMP-3.
012210         10  MSTA-LINE-OF-BUSINESS-ONS          PIC S999   COMP-3.
012230         10  MSTA-CLASSIFICATION-ONS.                             D6H12610
012240             15  MSTA-FORM-ONS                  PIC X.
012250             15  MSTA-NO-FAMILIES-ONS           PIC X.
012260             15  MSTA-COVERAGE-ONS              PIC X.
012270             15  MSTA-CONSTRUCTION-ONS          PIC X.
012280             15  MSTA-PUBLIC-PROTECTION-ONS     PIC X.
012290             15  MSTA-PER-CENT-MANUAL-ONS       PIC X.
012300             15  MSTA-DED-TYPE-ONS              PIC X.
012310             15  MSTA-DED-AMOUNT-ONS            PIC X.
012320         10  MSTA-LIABILITY-ONS.                                  D6H12700
012330             15  MSTA-AMT-INS-COV-A-OR-C-ONS    PIC 999.
012340             15  MSTA-UNSCHED-PERS-PROP-ONS     PIC XX.
012350             15  MSTA-MED-PAY-LIMIT-ONS         PIC X.
012360         10  MSTA-CREDIT-CARD-ENDOR-ONS         PIC X.
012370         10  MSTA-WATERCRAFT-ENDOR-ONS          PIC X.
012380         10  MSTA-WATERCRAFT-TYPE-ONS           REDEFINES
012390              MSTA-WATERCRAFT-ENDOR-ONS         PIC X.
012400         10  MSTA-FARM-COMP-PERS-ENDOR-ONS      PIC X.
012410         10  MSTA-INFLATION-GUARD-ENDOR-ONS     PIC X.
012420         10  MSTA-SNOWMOBILE-ENDOR-ONS          PIC X.
012430         10  MSTA-SNOWMOBILE-NUMBER-ONS         REDEFINES
012440              MSTA-SNOWMOBILE-ENDOR-ONS         PIC X.
012450         10  MSTA-AGE-OF-DWELLING-ONS           PIC XX.
012460         10  MSTA-YEAR-OF-CONSTRUCTION-ONS      REDEFINES
012470              MSTA-AGE-OF-DWELLING-ONS          PIC XX.
012480         10  MSTA-PROTECTION-X-ONS              PIC X.
012490         10  MSTA-WATERCRAFT-NUMBER-ONS         PIC X.
012500         10  MSTA-WATERCRAFT-EXPOSURE-ONS       PIC X.
012510         10  MSTA-COV-E-LIMIT-ONS               PIC X.
012520         10  MSTA-COV-F-LIMIT-ONS               PIC X.
012530         10  MSTA-FILLER-ONS                    PIC XX.
012540         10  MSTA-TX-PERCENT-MANUAL-ONS         REDEFINES
012550              MSTA-FILLER-ONS                   PIC XX.


012560     05  M-AETNA-STAT-CODES-SEC.                                  D6H12940
012570       07  M-AETNA-STAT-CODES-OFFSET.                             D6H12950
012580         10  MAET-TAX-DISTRICT-OFF              PIC S9(4)  COMP-3.
012600         10  MAET-STAT-PLAN-OFF                 PIC S9     COMP-3.
012620         10  MAET-CONTROL-CODE-OFF              PIC S99    COMP-3.
012640         10  MAET-PAYMENT-CODE-OFF              PIC XXX.
012650         10  MAET-TRX-TYPE-CODE-OFF             PIC X.
012660         10  MAET-CAP-CODE-OFF                  PIC S9     COMP-3.
012680         10  MAET-IDENT-CODE-OFF                PIC S99    COMP-3.

012700       07  MAET-STAT-CODES-ONSET.                                 D6H13080
012710         10  MAET-TAX-DISTRICT-ONS              PIC S9(4)  COMP-3.
012730         10  MAET-STAT-PLAN-ONS                 PIC S9     COMP-3.
012750         10  MAET-CONTROL-CODE-ONS              PIC S99    COMP-3.
012770         10  MAET-PAYMENT-CODE-ONS              PIC XXX.
012780         10  MAET-TRX-TYPE-CODE-ONS             PIC X.
012790         10  MAET-CAP-CODE-ONS                  PIC S9     COMP-3.
012810         10  MAET-IDENT-CODE-ONS                PIC S99    COMP-3.


012830     05  M-HISTORIC-BILLING-INFO-SEC.                             D6H13210
012840       07  MHIS-NON-NUMERIC-BILL-INFO.
012850         10  MHIS-BILLING-TYPE                  PIC X.
012860             88  MHIS-CUSTOMER                      VALUE '1'.
012870             88  MHIS-AGENCY                        VALUE '2'.
012880             88  MHIS-BUDGET-RITE                   VALUE '3'.
012890             88  MHIS-EPIC                          VALUE '4'.
012900         10  MHIS-POLICY-TERM                   PIC X.
012910             88  MHIS-THREE-YEAR                    VALUE '1'.
012920             88  MHIS-ONE-YEAR                      VALUE '2'.
012930             88  MHIS-SIX-MONTH                     VALUE '3'.
012940             88  MHIS-THREE-MONTH                   VALUE '4'.
012950         10  MHIS-SEND-BILL-TO                  PIC X.
012960             88  MHIS-INSURED                       VALUE '1'.
012970             88  MHIS-FIRST-MORTGAGEE               VALUE '2'.
012980             88  MHIS-ALTERNATE-PAYOR               VALUE '3'.
012990         10  MHIS-ACCOUNT-NUMBER                PIC X(11).
013000         10  MHIS-TRX-TYPE                      PIC XX.

013010       07  MHIS-NUMERIC-BILL-INFO                          COMP-3.
013030         10  MHIS-INIT-PAY-CUST-BR-EPIC         PIC S9(5)V99.
013040         10  MHIS-AMOUNT-BILLED                 PIC S9(5)V99.
013050         10  MHIS-DEC-NO                        PIC S9(3).
013060         10  MHIS-COMM-RATE                     PIC SV999.


013070     05  M-EARTHQUAKE-REINS-PREM-SEC                       COMP-3.
013090         10  MEAR-PREM-EQ-REINS                 PIC S9(4)V99.
013100         10  MEAR-PREM-EQ-REINS-OFF             PIC S9(4)V99.
013110         10  MEAR-PREM-EQ-REINS-ONS             PIC S9(4)V99.


013120     05  M-NY-XL-INFO-SEC                                  COMP-3.
013140         10  MNYX-LOCATION                      PIC S9.
013150             88  MNYX-KINGS                         VALUE +1.
013160             88  MNYX-BRONX                         VALUE +2.
013170             88  MNYX-NEW-YORK                      VALUE +3.
013180             88  MNYX-QUEENS                        VALUE +4.
013190             88  MNYX-ROS                           VALUE +5.
013200         10  MNYX-NO-MALE-25-DRIVERS            PIC S9.


013210     05  M-PERCENT-OF-INS-INFO-SEC                         COMP-3.
013230         10  MPER-PERCENT-COVG                  PIC S99V999.
013240         10  MPER-APPRAISAL-CALC-COVG-A         PIC S9(7).


013250     05  M-PART-PROVISION-FORM-NO.                                D6H13630
013260         10  MPAR-PROVISION-PART-II             PIC X(7).
013270         10  MPAR-PROVISION-PART-III            PIC X(7).


013280     05  M-CBS-OVERRIDE-IND                     PIC X.
013290         88  MCBS-IND-ON                            VALUE 'Y'.
013300         88  MCBS-IND-OFF                           VALUE 'N'.
013310         88  MCBS-OVRD-UNUSED                       VALUE ' '.


013320     05  M-NAME-INS-FOR-BILLING                 PIC X(35).


013330     05  M-MODERNIZATION-INFO-SEC.                                D6H13710
013340         10  MMOD-DWELLING-OVER-30-HEAT         PIC X.
013350             88  MMOD-DWELLING-HEAT-MOD             VALUE 'Y'.
013360             88  MMOD-DWELLING-HEAT-NOT-MOD         VALUE 'N'.
013370         10  MMOD-DWELLING-OVER-30-ELEC         PIC X.
013380             88  MMOD-DWELLING-ELEC-MOD             VALUE 'Y'.
013390             88  MMOD-DWELLING-ELEC-NOT-MOD         VALUE 'N'.
013400         10  MMOD-DWELLING-OVER-30-PLUMB        PIC X.
013410             88  MMOD-DWELLING-PLUMB-MOD            VALUE 'Y'.
013420             88  MMOD-DWELLING-PLUMB-NOT-MOD        VALUE 'N'.


013430     05  M-REVISED-REINSURANCE-SEC                         COMP-3.
013450         10  MREV-PERCENT-CEDED                 PIC S9V9(4).
013460         10  MREV-AMOUNT-CEDED                  PIC S9(7).


013470     05  M-TRANS-SORT-KEY-SEC                              COMP-3.
013490         10  MTRA-CBS-TRANS-SORT-KEY            PIC S999.


013500     05  M-BRANCH-AGENCY-INFO-SEC.                                D6H13880
013510         10  MBRA-TRANSFER-TYPE                 PIC XX.
013520             88  MBRA-PARTIAL-TRANSFER              VALUE 'PT'.
013530             88  MBRA-MASS-TRANSFER                 VALUE 'MT'.
013540             88  MBRA-SINGLE-TRANSFER               VALUE 'ST'.
013550         10  MBRA-ADDL-INFO                                COMP-3.
013570             15  MBRA-OLD-BRANCH                PIC S999.
013580             15  MBRA-OLD-AGENT                 PIC S9(4).
013590             15  MBRA-NEW-BRANCH                PIC S999.
013600             15  MBRA-NEW-AGENT                 PIC S9(4).
013610             15  MBRA-EFF-DATE-OF-TRANSFER      PIC S9(5).
013620             15  MBRA-CLOSURE-DATE              PIC S9(5).


013630     05  M-TOWN-ROW-HOUSE-SEC.                                    D6H14010
013640         10  MTOW-NO-UNITS                      PIC XXX.


013650     05  M-UPGRADING-INFO-SEC.                                    D6H14030
013660         10  MUPG-UPGRADING-INDS.                                 D6H14040
013670             15  MUPG-SECTION-1-IND             PIC X.
013680                 88  MUPG-OK-UPGRADE-SEC-1          VALUE 'Y'.
013690                 88  MUPG-NOT-OK-UPGRADE-SEC-1      VALUE 'N'.
013700             15  MUPG-SECTION-2-IND             PIC X.
013710                 88  MUPG-OK-UPGRADE-SEC-2          VALUE 'Y'.
013720                 88  MUPG-NOT-OK-UPGRADE-SEC-2      VALUE 'N'.
013730         10  MUPG-UPGRADING-DATES                          COMP-3.
013750             15  MUPG-DATE-SEC-1-UPGRADED       PIC S9(5).
013760             15  MUPG-DATE-SEC-2-UPGRADED       PIC S9(5).


013770     05  M-CALIF-SECTION-2-SEC.                                   D6H14150
013780         10  MCAL-SECT-2-FORM                   PIC X(11).
013790         10  MCAL-SECT-2-ED-DATE                PIC X(4).


013800     05  M-CONTINGENCY-AREA-SEC.                                  D6H14180
013810         10  MCON-BUDGETRITE-ADDR-PASSED        PIC X.
013820         10  MCON-SEC-1-UP-INDEX-IND            PIC X.
013830         10  MCON-DEDUCTIBLE-IND-1              PIC XX.
013840         10  MCON-DEDUCTIBLE-IND-2              PIC XX.
013850         10  MCON-DEDUCTIBLE-IND-3              PIC XX.
013860         10  MCON-DEDUCTIBLE-IND-4              PIC XX.
013870         10  MCON-AR-RENEWAL-DIARY-COUNTER      PIC XX.
013880         10  MCON-ILL-PREF-IND                  PIC X.
013890         10  MCON-SEC-2-AMF-UPG-IND             PIC X.
013900         10  MCON-SORT-7-IND                    PIC X.
013910         10  MCON-CONVERSION-SW                 PIC X.
013920         10  MCON-ACE-IND                       PIC X.
013930             88  MCON-ACE-ON-POL                    VALUE '1'.
013940             88  MCON-ACE-REMOVED                   VALUE '2'.
013950         10  MCON-INF-PERCENT                   PIC S99V99 COMP-3.
013960         10  MCON-FORM-UPGRD-IND                PIC X.
013970             88  MCON-FORM-UPGRDED                  VALUE '1'.
013980             88  MCON-FORM-UPGRD-DECLNE             VALUE '2'.
013990         10  MCON-SIMP-DEC-IND                  PIC X.
014000         10  MCON-HO6-SUPPLEMENTAL-IND          PIC X.
014010             88  MCON-SUPPLE-ON                     VALUE 'Y'.
014020             88  MCON-SUPPLE-OFF                    VALUE ' '.
014030         10  MCON-JEWELRY-IND                   PIC X.
014040             88 M-JEWELRY                           VALUE '1'.
014050         10  MCON-SIMP-DED-AMT                  PIC X(4).
014060         10  MCON-CBS-IND                       PIC X.
014070             88  MCON-CBS-CONVRTD                   VALUE '1'.
014080             88  MCON-CBS-NOT-CONVRTD               VALUE '0'.
014090         10  MCON-NH-WC-ENDT-IND                PIC X.
014100             88  MCON-NH-WC-ENDT-REFUSED            VALUE '1'.
014110             88  MCON-NH-WC-ENDT-DESIRED            VALUE '0'.
014120         10  MCON-250-IND                       PIC X.
014130             88  MCON-250-ADDED                     VALUE '1'.
014140             88  MCON-NO-250                        VALUE '0'.
014150         10  MCON-TERR-REDEF-IND                PIC X.
014160             88  MTERR-REDEF-NA                     VALUE ' '.
014170             88  MTERR-REDEF-COMP                   VALUE '1'.
014180             88  MTERR-REDEF-1982                   VALUE '2'.
014190             88  MTERR-KY-REDEF-1983                VALUE '3'.
014200         10  MCON-PLSP-REDEF-IND                PIC X.
014210             88  M-PLSP-REDEF-NA                    VALUE ' '.
014220             88  M-PLSP-REDEF-COMP                  VALUE '1'.
014230         10  MCON-PROTECTIVE-DEVICES-CR         PIC S99    COMP-3.
014240         10  MCON-HO-4-6-UPGRD                  PIC X.
014250             88  MCON-C-UPGRADE                     VALUE '1'.
014260             88  MCON-C-DOWNGRADE                   VALUE '2'.
014270         10  MCON-90-PER-UPGRD                  PIC X.
014280             88  MCON-90-UPGRADE                    VALUE '1'.
014290         10  MCON-MINS-TELEPHONE-NO             PIC X(12).
014300         10  MCON-100-DED-UPGRD-IND             PIC X.
014310             88  MCON-SIMP-100-DED-UPGRD-YES        VALUE '1'. 
014320             88  MCON-SIMP-100-DED-UPGRD-NO         VALUE ' '.
014330         10  MCON-RESERVE-AREA                             COMP-3.
014350             15  MCON-100-DED-PREM              PIC S9(5)V99.
014360             15  MCON-100-DED-PREM-OFF          PIC S9(5)V99.
014370             15  MCON-100-DED-PREM-ONS          PIC S9(5)V99.
014380             15  MCON-ISSUE-RENEWAL-OFFICE      PIC S9(7).
014390             15  MCON-KENT-PROSE-IND            PIC S9(7).
014400             15  MCON-COV-C-DIFF                PIC S9(5)V99.
014410             15  MCON-COV-D-DIFF                PIC S9(5)V99.
014420             15  MCON-LOSS-ASSESSMENT-LIMIT     PIC S9(7).
014430             15  MCON-OTHER-RESID-LIMIT         PIC S9(7).
014440             15  MCON-SILVERWARE-AMT            PIC S9(7).
014450                 88  MCON-NO-SILVERWARE             VALUE ZEROS.
014460             15  MCON-LIM-ADDL-LIVING-EXP       PIC S9(7).
014470             15  MCON-TX-SCHED-PERS-PROP-LIM    REDEFINES
014480                  MCON-LIM-ADDL-LIVING-EXP      PIC S9(7).
014490             15  MCON-KY-TAX-RATE               PIC S9V9(6).
014500             15  MCON-3-WALL-ROOM-ADDITION      PIC S9(7).
014510             15  MCON-BALCONY-DECK              PIC S9(7).


014520     05  M-TRANSACTION-COMMISSION-INFO.                           D6H14900
014530         10  MTRA-TRANS-COMM-RATE               PIC SV999  COMP-3.


014540     05  M-SCHEDULED-PERS-PROPERTY.                               D6H14920
014550         10  MVIP-RATE-RVN                      PIC S99    COMP-3.
014560         10  MVIP-RATE-BILL-ACTION-TYPE         PIC X.
014570         10  MVIP-RATE-CODE                     PIC S99    COMP-3.
014580         10  MVIP-NEW-RENEWAL-IND               PIC X.
014590             88  MVIP-NEW-BUSINESS                  VALUE 'N'.
014600             88  MVIP-RENEWAL                       VALUE 'R'.
014610         10  MVIP-NEW-YORK-PREF-AGT-IND         PIC X.
014620             88  MVIP-NY-PREF-AGENT                 VALUE 'Y'.
014630             88  MVIP-NOT-NY-PREF-AGENT             VALUE 'N'.
014640         10  MVIP-COMMISSION-RATE               PIC SV999  COMP-3.
014650         10  MVIP-JEWELRY-INFO-SEC.                               D6H15030 
014660             15  MVIP-PARENT-SET-IND            PIC X.
014670                 88  MVIP-YES-PARENT-SET-COVG       VALUE 'Y'.
014680                 88  MVIP-NO-PARENT-SET-COVG        VALUE 'N'.
014690             15  MVIP-PARENT-SET-AMT            PIC S9(5)  COMP-3.
014700             15  MVIP-VALUED-JEWELRY-IND        PIC X.
014710                 88  MVIP-VJC                       VALUE 'Y'.
014720                 88  MVIP-ACV                       VALUE 'N'.
014730             15  MVIP-JEWELRY-VAULT-AMT         PIC S9(6)  COMP-3.
014740         10  MVIP-ANTIQUE-AUTO-IND              PIC X.
014750             88  MVIP-COLL-UPSET                    VALUE 'Y'.
014760             88  MVIP-NO-COLL-UPSET                 VALUE 'N'.
014770         10  MVIP-COINS-INFO-SEC.                                 D6H15150
014780             15  MVIP-COINS-UNATTEND-AUTO-IND   PIC X.
014790                 88  MVIP-COINS-THEFT-COVG          VALUE 'Y'.
014800                 88  MVIP-COINS-NO-THEFT-COVG       VALUE 'N'.
014810             15  MVIP-COINS-SAFE-CREDIT-IND     PIC X.
014820                 88  MVIP-COINS-YES-SAFE-CREDIT     VALUE 'Y'.
014830                 88  MVIP-COINS-NO-SAFE-CREDIT      VALUE 'N'.
014840         10  MVIP-DOG-INFO-SEC.                                   D6H15220
014850             15  MVIP-DOG-POISON-COVG-IND       PIC X.
014860                 88  MVIP-YES-DOG-POISON-COVG       VALUE 'Y'.
014870                 88  MVIP-NO-DOG-POISON-COVG        VALUE 'N'.
014880             15  MVIP-DOG-HIT-RUN-COVG-IND      PIC X.
014890                 88  MVIP-YES-DOG-HIT-RUN-COVG      VALUE 'Y'.
014900                 88  MVIP-NO-DOG-HIT-RUN-COVG       VALUE 'N'.
014910         10  MVIP-GOLF-ADDL-INTEREST-IND        PIC X.
014920             88  MVIP-YES-GOLF-ADDL-INTEREST        VALUE 'Y'.
014930             88  MVIP-NO-GOLF-ADDL-INTEREST         VALUE 'N'.
014940         10  MVIP-MUSICAL-INSTR-INFO-IND        PIC X.
014950             88  MVIP-MUSICAL-LIMITED-FORM          VALUE 'Y'.
014960             88  MVIP-NOT-MUSICAL-LIM-FORM          VALUE 'N'.
014970         10  MVIP-STAMPS-INFO-SEC.                                D6H15350
014980             15  MVIP-STAMPS-UNATTENDED-AUTO    PIC X.
014990                 88 MVIP-YES-STAMPS-THEFT-COVG      VALUE 'Y'.
015000                 88  MVIP-NO-STAMPS-THEFT-COVG      VALUE 'N'.
015010             15  MVIP-STAMPS-SAFE-CREDIT-IND    PIC X.
015020                 88 MVIP-YES-STAMPS-SAFE-CREDIT     VALUE 'Y'.
015030                 88 MVIP-NO-STAMPS-SAFE-CREDIT      VALUE 'N'.
015040         10  MP82-CONST-INL-ONS                 PIC X.
015050         10  MVIP-FILLER-2                      PIC S9(5)  COMP-3.

015060         10  MVIP-CLASS-DATA-TBL.
015060           12  MVIP-CLASS-DATA                OCCURS 9 TIMES
015070                                              INDEXED BY
015070                                              MVIP-CLASS-INDEX.
015080             15  MVIP-ACTION-TYPE             PIC X.
015090             15  MVIP-COV-FORM-NO             PIC S99      COMP-3.
015100             15  MVIP-CLASS-CODE              PIC S9(7)    COMP-3.
015110             15  MVIP-AMOUNT-OF-INS           PIC S9(6)    COMP-3.
015120             15  MVIP-FULL-TERM-PREM          PIC S9(4)V99 COMP-3.
015130             15  MVIP-ONSET-PREMIUM           PIC S9(4)V99 COMP-3.
015140             15  MVIP-OFFSET-PREMIUM          PIC S9(4)V99 COMP-3.
015150             15  MVIP-RATE-MODE-IND           PIC S9       COMP-3.


015180     05  M-REVISED-LOAN-NUMBERS.                                  D6H15560
015190         10  MREV-1ST-MORT-LOAN-NO              PIC X(20).
015200         10  MREV-2ND-MORT-LOAN-NO              PIC X(20).
015210         10  MREV-ALT-PAYOR-LOAN-NO             PIC X(20).


015220     05  M-REVISED-RVN.                                           D6H15600
015230         10  MREV-RVN                           PIC S99    COMP-3.


015240     05  M-TEXAS-ADDITIONAL-INFO.                                 D6H15620
015250         10  M-TX-POLICY-FORM-NO                PIC X(9).

015290         10  M-TX-INSRD-ADDRESS-INFO.                             D6H15670
015300             15  MINS-TX-LOT                    PIC XXX.
015310             15  MINS-TX-BLOCK                  PIC XXX.
015320             15  MINS-TX-ADDITION               PIC X(30).
015330             15  MINS-TX-NEW-CITY-BLOCK         PIC X(8).

015340         10  M-TX-DWELLING-INFO.                                  D6H15720
015350             15  MDWE-TX-HYDRANT-TYPE           PIC X.
015360                 88  MDWE-TX-HYD-PUB-STD            VALUE '1'.
015370                 88  MDWE-TX-HYD-NATL-STD           VALUE '2'.
015380             15  MDWE-TX-NO-OF-STORIES          PIC S99    COMP-3.
015390             15  MDWE-TX-TYPE-ENTRY             PIC X.
015400                 88  MDWE-TX-SINGLE-ENTRY           VALUE 'Y'.
015410                 88  MDWE-TX-NOT-SINGLE-ENTRY       VALUE 'N'.      D6H157
90
015420             15  MDWE-TX-NO-OF-UNITS            PIC S999   COMP-3.
015430             15  MDWE-TX-FT-COMML-EXPOSURE      PIC S9(4)  COMP-3.

015440         10  M-TX-DEDUCTIBLE-INFO.                                D6H15820
015450             15  MDED-TX-CLAUSE-1               PIC X.
015460             15  MDED-TX-CLAUSE-2               PIC X.
015470             15  MDED-TX-CLAUSE-3               PIC X.
015480             15  MDED-TX-CLAUSE-1-3-AMT         PIC S9(5)  COMP-3.
015490             15  MDED-TX-CLAUSE-2-AMT           PIC S9(5)  COMP-3.
015500*****  CLAUSE 1 AND 2 ARE FOR DWELLING POLICIES                   D6H15880
015510*****  CLAUSE 3 IS FOR TENANTS AND CONDOMINIUMS                   D6H15890
015520***********THE FOLLOWING ARE VALID VALUES FOR ALL 3               D6H15900
015530***** 1 = STANDARD // 2 = 1 HALF OF 1%                            D6H15910
015540***** 3 = 100*// 4 = 250 // 5 = NIL                               D6H15920

015550         10  M-TX-ENDORSEMENT-INFO.                               D6H15930
015560             15  MEND-TX-ADDL-INSRD.                              D6H15940
015570                 20  MEND-TX-ADDL-INSRD-NAME     PIC X(30).
015580                 20  MEND-TX-ADDL-INSRD-ADDR-1   PIC X(30).
015590                 20  MEND-TX-ADDL-INSRD-ADDR-2   PIC X(20).
962190                 20  MEND-FILLER-8 REDEFINES
962190                                   MEND-TX-ADDL-INSRD-ADDR-2.
962190                   25 MEND-TX-ADDL-INSRD-ADDR2-CITY  PIC X(17).
962190                   25 MEND-FILLER-9                  PIC X(01).
962190                   25 MEND-TX-ADDL-INSRD-ADDR2-STATE PIC X(02).
015600                 20  MEND-TX-ADDL-INSRD-ZIP-COD  PIC S9(5) COMP-3.
015610                 20  MEND-TX-ADDL-INSRD-INTRST   PIC X(20).
015620                 20  MEND-TX-ADDL-INSRD-OCC-CODE PIC X.           D6H16000
015630             15  MEND-TX-ADDL-AMT-PRIV-STRUCT    PIC S9(5) COMP-3.

015640             15  M-TX-FARMERS-COMP-TBL.
015640               17  M-TX-FARMERS-COMPREHENSIVE  OCCURS 2 TIMES
015650                                               INDEXED BY
015650                                               MEND-TX-FCPL-INDEX.
015660                 20  MEND-TX-FCPL-LOCATION    PIC X(60).          D6H16040
015670                 20  MEND-TX-FCPL-ACREAGE     PIC S9(5)    COMP-3.
015680                 20  MEND-TX-FCPL-RENTED      PIC X.              D6H16060
015690                 20  MEND-TX-FCPL-BLDGS       PIC X.              D6H16070
015700                 20  MEND-TX-FCPL-PREM        PIC S9(4)V99 COMP-3.D6H16080

015710             15  MEND-TX-BUS-PURS-CORP-PUN-IND   PIC X.
015720                 88  MEND-TX-YES-CORP-PUNISHMENT     VALUE 'Y'.
015730                 88  MEND-TX-NO-CORP-PUNISHMENT      VALUE 'N'.

015740             15  M-TX-ADDL-PREMISES-TBL.
015740               17  M-TX-ADDL-PREMISES     OCCURS 3 TIMES
015750                                          INDEXED BY
015750                                          MEND-TX-ADDL-PREM-INDEX.
015760                 20  MEND-TX-ADDL-PREM-ADDR-1   PIC X(20).
015770                 20  MEND-TX-ADDL-PREM-ADDR-2   PIC X(30).
015780                 20  MEND-TX-ADDL-PREM-ADDR-3   PIC X(20).
015790                 20  MEND-TX-ADDL-PREM-ADDR-4   PIC X(10).
015800                 20  MEND-TX-ADDL-PREM-ZIP      PIC S9(5) COMP-3.
015810                 20  MEND-TX-ADDL-PREM-MED-PAY  PIC X.
015820                 20  MEND-TX-ADDL-PREM-NO-FAM   PIC X.
015830                 20  MEND-TX-ADDL-PREM-INDICATORS.
015840                     25  MEND-TX-ADDL-PREM-SWIM-POOL  PIC X.
015850                     25  MEND-TX-ADDL-PREM-RENTED     PIC X.
015860                 20  MEND-TX-ADDL-PREM-TERR-NO        REDEFINES
015870                      MEND-TX-ADDL-PREM-INDICATORS    PIC 99.
015880                 20  MEND-TX-ADDL-PREM-PREM   PIC S9(4)V99 COMP-3.

015890             15  MEND-TX-ADDL-LIM-JEW-FURS       PIC S9(5) COMP-3.
015900             15  MEND-TX-LOSS-ASSESMENT-VAL      PIC S9(5) COMP-3.

015910         10  M-TX-CODING-INFO.                                    D6H16290
015920             15  MCOD-TX-CITY-LIMITS            PIC X.
015930                 88  MCOD-TX-INSIDE-CITY            VALUE 'Y'.
015940                 88  MCOD-TX-NOT-INSIDE-CITY        VALUE 'N'.
015950             15  MCOD-TX-CLASS-CODE             PIC X.
015960             15  MCOD-TX-TOWN-COUNTY-INFO.                        D6H16340
015970                 20  MCOD-TX-TOWN               PIC S9(5)  COMP-3.
015980                 20  MCOD-TX-COUNTY             PIC S9(4)  COMP-3.
015990                 20  MCOD-TX-TERRITORY          PIC XXX.
016000                 20  MCOD-TX-PLACE-CODE         PIC S9(5)  COMP-3.
016010                 20  MCOD-TX-INCORPORATE        PIC X.
016020                     88  MCOD-TX-INCORPORATED       VALUE 'I'.
016030                     88  MCOD-TX-UNINCORPORATED     VALUE 'U'.
016040                 20  MCOD-TX-KEY-RATE           PIC S9V99  COMP-3.
016050                 20  MCOD-TX-EFF-DATE           PIC S9(5)  COMP-3.
016060                 20  MCOD-TX-FIRE-CREDIT        PIC SV99   COMP-3.
016070                 20  MCOD-TX-PROTECTED-IND      PIC X.
016080             15  MCOD-TX-FRINGE-RATED-IND       PIC X.
016090                 88  MCOD-TX-FRINGE-RATED           VALUE 'Y'.
016100                 88  MCOD-TX-NOT-FRINGE-RATED       VALUE 'N'.
016110             15  MCOD-TX-FRINGE-RATED-INFO.                       D6H16490
016140                 20  MCOD-TX-FR-TOWN            PIC S9(5)  COMP-3.
016150                 20  MCOD-TX-FR-COUNTY          PIC S9(4)  COMP-3.
016160                 20  MCOD-TX-FR-TERRITORY       PIC XXX.
016170                 20  MCOD-TX-FR-PLACE-CODE      PIC S9(5)  COMP-3.
016180                 20  MCOD-TX-FR-INCORPRATED     PIC X.
016190                     88  MCOD-TX-FR-INCORPORATED    VALUE 'I'.
016200                     88  MCOD-TX-FR-UNINCORPORATED  VALUE 'U'.
016210                 20  MCOD-TX-FR-KEY-RATE        PIC S9V99  COMP-3.
016220                 20  MCOD-TX-FR-EFF-DATE        PIC S9(5)  COMP-3.
016230                 20  MCOD-TX-FR-FIRE-CREDIT     PIC SV99   COMP-3.
016240                 20  MCOD-TX-FR-PROTECTED-IND   PIC X.
016260             15  MCOD-TX-MERCANTILE-OCCUP       PIC X.
016270                 88  MCOD-TX-YES-MERCANTILE-OCC     VALUE 'Y'.
016280                 88  MCOD-TX-NO-MERCANTILE-OCC      VALUE 'N'.
016290             15  MCOD-TX-RURAL-FIRE-CREDIT      PIC X.
016300                 88  MCOD-TX-RURAL-CR-1             VALUE '1'.
016310                 88  MCOD-TX-RURAL-CR-2             VALUE '2'.

016320         10  M-TX-NOTE-BILLING-IND              PIC X.
016330             88  MHIS-TX-NOTES-BILLING              VALUE '1'.

016340         10  M-TX-RATE-FACTOR-ANAL-INFO                    COMP-3.
016350             15  MFACT-TX-BASIS                 PIC S9V99.
016360             15  MFACT-TX-ADJ-KEY-RATE          PIC SV99.
016370             15  MFACT-TX-RESULT-1              PIC S9V99.
016380             15  MFACT-TX-EXP-PER-CENT          PIC S99.
016390             15  MFACT-TX-EXP-RESULT            PIC SV999.
016400             15  MLOOK-DOWN-60-LINES            PIC SV999.
016410             15  MFACT-TX-FR-PER-CENT           PIC S99.
016420             15  LOOK-DOWN-54-LINES             PIC SV999.
016430             15  MLOOK-DOWN-58-LINES            PIC SV999.
016440             15  MFACT-TX-EC-RATE               PIC S9V999.
016450             15  MFACT-TX-1YR-FIRE-RATE         PIC S9V99.
016460             15  MFACT-TX-FINAL-RATE-FACT       PIC S9V999.
016470         10  M-TX-PREM-WO-UPG               PIC S9(5)V99   COMP-3.


016480     05  M-RESERVE-AREA-II.                                       D6H16860
016490         10  MRESII-RENL-CONV                   PIC X.
016500         10  MRESII-FL-100-DED-CONV-IND         PIC X.
016510             88  MRESII-FL-100-DED-CONVERTED        VALUE '1'.
016520             88  MRESII-FL-100-DED-NOT-CONVERT      VALUE ' '.
016530         10  MRESII-CONT-REPL-IND               PIC X.
016540             88  MRESII-CONT-REPL-YES               VALUE '1'.
016550             88  MRESII-CONT-REPL-NO                VALUE '0'.
016560         10  MRESII-EQ-CANC-IND                 PIC X.
016570             88  MRESII-EQ-CANC-YES                 VALUE '1'.
016580             88  MRESII-EQ-CANC-NO                  VALUE '0'.
016590         10  MRESII-NON-REN-REAS                PIC XX.
016600         10  MRESII-NON-REN-EVID                PIC X.
016610         10  MVIPII-LTD-ED-BRKAGE               PIC X.
016620         10  MVIPII-CHINA-BRKAGE                PIC X.
016630         10  MRESII-MASS-FAM-UNITS              PIC S9     COMP-3.
016640         10  MRESII-APP-NEW-CONST-QUALITY       PIC XXX.
016650         10  MRESII-WORK-CLASS-915              PIC X.
016660             88  MWORK-915                          VALUE '3'.
016670         10  MRESII-NJ-PRIMARY-DWLG-LOC-IND     PIC X.
016680         10  MRESII-ALT-HEATING-IND             PIC X.
016690         10  MRESII-UND-APP                     PIC X.
016700         10  MRESII-REPL-COST-PERCENT           PIC S999   COMP-3.
016710         10  MRESII-AMT-HAZ-CHG-IND             PIC X.
016720             88  MRESII-NO-PRIOR-CHG                VALUE ' '.
016730             88  MRESII-AMT-CHG                     VALUE '1'.
016740             88  MRESII-HAZ-CHG                     VALUE '2'.
016750             88  MRESII-AMT-HAZ-CHG                 VALUE '3'.
016760         10  MRESII-NUMERICS                               COMP-3.
016770             15  MRESII-NEW-HOME-CR             PIC SV99.
016780             15  MRESII-PC-RVN                  PIC S99.
016790             15  MRESII-TX-COIN-RESULT          PIC SV999.
016800             15  MRESII-TX-RESULT2A             PIC SV999.
016810             15  MRESII-RENL-FACTOR             PIC S99V999.
016820             15  MRESII-FACTORS-USED            PIC S9(5).
016830             15  MREIS-XL-UM-COV                PIC S9(7).
016840             15  MRESII-STAT-ZIP                PIC S9(7).
016850             15  MRESII-NEW-HOME-FULL           PIC S9(5)V99.
016860             15  MRESII-NEW-HOME-ONS            PIC S9(5)V99.
016870             15  MRESII-NEW-HOME-OFF            PIC S9(5)V99.
016880             15  MRESII-AMT-RETAINED            PIC S9(7).
016890             15  MP82-AMT-INS-INL-OFF           PIC S9(7).
016900             15  MP82-AMT-INS-INL-ONS           PIC S9(7).
016910             15  MP82-EXCEPT-CODE-OFF           PIC S9(7).


016920     05  M-RESERVE-AREA-III.                                      D6H17300
016930         10  MRESIII-NUMERICS                              COMP-3.
016940             15  MRESIII-XL-RECREATIONAL        PIC S99.
016950             15  MRESIII-WORK-NO-EMPL-915       PIC S99.
016970             15  MRESIII-OFF-OCC-FAM-IND        PIC S99.
016980             15  MRESIII-KY-FLAT-RATE           PIC S9V99.
016990             15  MP82-REPLACE-COST-CODE-OFF     PIC S9(3).
017000             15  MP82-REPLACE-COST-CODE-ONS     PIC S9(3).
017010             15  MP82-DED-INL-ONS               PIC S9(3).
017020             15  MRESII-FILLER-15               PIC S999.
017030             15  MFACT-TX-FR-RESULT             PIC S99V999.
017040             15  MRESIII-GUNS-LIMIT             PIC S9(5).
017050             15  MFACT-TX-RESULT-2              PIC S9V999.
017060             15  MFACT-TX-RESULT-3              PIC S9V999.
017070             15  MRESIII-SAVE-SILVER            PIC S9(7). 
017080             15  MRESIII-COV-B-DIFF             PIC S9(7).
017090             15  MCOV-LIM-APP-STRUCT            PIC S9(7).
017100             15  MP82-EXCEPT-CODE-ONS           PIC S9(7).
017110             15  MRESIII-WORK-PREM-915-FULLTERM PIC S9(5)V99.
017130             15  MRESIII-WORK-PREM-915-ONSET    PIC S9(5)V99.
017150             15  MRESIII-WORK-PREM-915-OFFSET   PIC S9(5)V99.
017170             15  MRESIII-COVA-CHG-HOLD          PIC S9(5)V99.
017180             15  MRESIII-FILLER-21              PIC S9(7).
017190             15  MRESIII-FILLER-22              PIC S9(7).
017200             15  MP82-CREDIT-AMOUNT-OFF         PIC S9(5)V99.
017210             15  MP82-CREDIT-AMOUNT-ONS         PIC S9(5)V99.
017220         10  MRESIII-MICH-UPGRD-IND             PIC X.
017230         10  MQUEST-Q-AT-PRE-RENEW-SW           PIC X.
017240             88  MQUEST-Q-AT-PRE-RENEW              VALUE '1'.
017250         10  MRESIII-MICH-ESSN-TERR-CONV        PIC X.
017260             88  MTERR-CHG-FOR-MICH-ESSN            VALUE '1'.
017270             88  MTERR-NOT-YET-CHG-MICH-ESSN        VALUE '0'.
017280         10  MEND-TX-FCPL-INSD-ON-PREM-SW       PIC X.
017290             88  MEND-TX-FCPL-INSD-ON-PREM          VALUE 'Y'.
017300             88  MEND-TX-FCPL-INSD-NOT-ON-PREM      VALUE 'N'.
017310         10  MRESIII-TX-3-YR-TERM-FACT-SW       PIC X.
017320         10  MRESIII-HO-65-HO-211-SW            PIC X.
017330         10  MRESIII-QUEST-OVERRIDE-IND         PIC X.
017340             88  MRESIII-OVERRIDE-560               VALUE '1'.


017350     05  M-PLSP-82-CODES-SEC.                                     D6H17730
017360         10  MP82-THEFT-OFF                     PIC XX.
017370         10  MP82-THEFT-ONS                     PIC XX.
017380         10  MP82-PROT-INL-OFF                  PIC XX.
017390         10  MP82-PROT-INL-ONS                  PIC XX.
017400         10  MP82-CONST-INL-OFF                 PIC X.
017410         10  MP82-FILLER                        PIC X(6).
017420         10  MP82-LIABILITY-OFF                 PIC XX.
017430         10  MP82-LIABILITY-ONS                 PIC XX.
017440         10  MP82-EQ-DED-SIZE-OFF               PIC XX.
017450         10  MP82-EQ-DED-SIZE-ONS               PIC XX.
017460******************************************************************D6H17840
017470*FIELDS FOR PLSP 82 CAN ALSO BE FOUND MMIS, MFIR, MSEC, MALT,    *D6H17850
017480*MVIP, MRESII, AND MRESIII SECTIONS.                             *D6H17860
017490******************************************************************D6H17870


017500     05  M-WORKERS-COMP.                                          D6H17880
017510         10  MWORK-CLASS-912                 PIC X.
017520             88  MWORK-912                       VALUE '1'.
017530         10  MWORK-NO-EMPL-912               PIC S99       COMP-3.
017540         10  MWORK-PREM-912-FULL-TERM        PIC S9(5)V99  COMP-3.
017550         10  MWORK-PREM-912-ONSET            PIC S9(5)V99  COMP-3.
017560         10  MWORK-PREM-912-OFFSET           PIC S9(5)V99  COMP-3.
017570         10  MWORK-CLASS-913                 PIC X.
017580             88  MWORK-913                       VALUE '2'.
017590         10  MWORK-NO-EMPL-913               PIC S99       COMP-3.
017600         10  MWORK-PREM-913-FULL-TERM        PIC S9(5)V99  COMP-3.
017610         10  MWORK-PREM-913-ONSET            PIC S9(5)V99  COMP-3.
017620         10  MWORK-PREM-913-OFFSET           PIC S9(5)V99  COMP-3.


017630     05  MQUEST-INFO-AREA.                                        D6H18010
017640         10  MQUEST-IND                         PIC X.
017650             88  MQUEST-INITIAL-QUESTIONNAIRE       VALUE '1'.
017660             88  MQUEST-FOLLOWUP-QUESTIONNAIRE      VALUE '2'.
017670             88  MQUEST-ABANDON-QUESTIONNAIRE       VALUE '3'.
017680         10  MQUEST-SENT-DATE-1                 PIC X(8).
017690         10  MQUEST-SENT-DATE-2                 PIC X(8).


017700      05  MREAL-ENDT-MISC-REAL-PROP.                              D6H18080
017710          10  MREAL-MISC-REAL-PROP              OCCURS 6 TIMES
017730                                                INDEXED BY
017730                                                M-REAL-INDEX. 
017740              15  MREAL-OCCURRENCE-NO           PIC X.
017750              15  MREAL-DESCRIPTION             PIC X(15).
017760              15  MREAL-LIMIT                   PIC S9(5)  COMP-3.


017770      05  MP82-DED-AMOUNT-ONS                   PIC XX.
017780      05  MP82-DED-AMOUNT-OFF                   PIC XX.
017790      05  MP82-WATERCRAFT-EXPOSURE-ONS          PIC XXX.
017800      05  MP82-WATERCRAFT-EXPOSURE-OFF          PIC XXX.
017810      05  MP82-ENDT-OFF                         PIC XX.
017820      05  MP82-ENDT-ONS                         PIC XX.
017830      05  MP82-CONDO-LIMIT-OFF                  PIC XX.
017840      05  MP82-CONDO-LIMIT-ONS                  PIC XX.
017850      05  MRESIII-FL-CNTRSIG-IND                PIC X.
017860      05  MRESIII-1982-VIE-MIN                  PIC X.
017870      05  MRESIII-HA-188-SW                     PIC X.
017880          88 MRESIII-HA-188-NO-CR-HO4               VALUE 'Y'.
017890      05  MRESIII-RCVY-AEST                     PIC X.
017900      05  MGEM-ACTION-TYPE                      PIC X.
017910      05  MGEM-CUST-NUMBER                      PIC X(10)
017910                                                JUSTIFIED RIGHT.


017930      05  MRESIV-FILLER-AREA.                                     D6H18310
017940          10  MRESIV-AEST-MISC-DESCRIP          PIC X(63).
017950          10  MRESIV-SEASONAL-DWLG-IND          PIC X.
017960              88  MRESIV-SEASONAL-DWLG              VALUE 'Y'.

017970          10  MRESIV-TX-NUM-FAM                 PIC X.
017980              88  MRESIV-TX-ONE-FAMILY              VALUE '1'.
017990              88  MRESIV-TX-TWO-FAMILY              VALUE '2'.
018000              88  MRESIV-TX-THREE-FAMILY            VALUE '3'.
018010              88  MRESIV-TX-FOUR-FAMILY             VALUE '4'.

018020          10  MRESIV-ROW-TOWN-HOUSE             PIC X.
018030              88  MRESIV-END-UNIT                   VALUE '1'.
018040              88  MRESIV-OTHER-UNIT                 VALUE '2'.

018050          10  MRESIV-OCCUPANCY-INFO.                              D6H18430
018060              15  MRESIV-OCC-ACTION-TYPE        PIC X.
018070              15  MRESIV-DAYTIME-OCC            PIC X.
018080                  88  MRESIV-DAY-OCC                VALUE 'Y'.
018090              15  MRESIV-WKS-UNOCC              PIC XX.

018100          10  MRESIV-RES-SECURITY-SECTION.                        D6H18480
018110              15  MRESIV-RES-SEC-ACTION-TYPE    PIC X.
GM0070              15  MRESIV-RES-SEC-CODES.                           D6H18500
GM0070                  20  MRESIV-RES-SECURITY-INFO  OCCURS 6 TIMES
GM0070                                                INDEXED BY
GM0070                                                MRESIV-SEC-INDEX.
GM0070                      25  MRESIV-RES-SECUR-TYPE PIC X.
018150              15  MRESIV-RES-SEC-DESCRIP        PIC X(25).

018160          10  MRESIV-ALT-HEAT-SECTION.                            D6H18550
018170              15  MRESIV-ALT-ACTION-TYPE        PIC X.
018180              15  MRESIV-ALT-HEAT-TBL.
018180                  20  MRESIV-ALT-HEAT-INFO      OCCURS 4 TIMES
GM0070                                                INDEXED BY
018190                                                MRESIV-ALT-INDEX.
018200                      25  MRESIV-ALT-HEAT-TYPE  PIC X.
018210              15  MRESIV-STOVE-INSTALL-PROF     PIC X.
018220                  88  MRESIV-STOVE-PROF             VALUE 'Y'.
018230              15  MRESIV-STOVE-INSPECT-IND      PIC X.
018240                  88  MRESIV-STOVE-INSPECTED        VALUE 'Y'.
018250              15  MRESIV-CHIMNEY-CLEAN-IND      PIC X.
018260                  88  MRESIV-CHIMNEY-CLEANED        VALUE 'Y'.

018270          10  MRESIV-PERS-PROP-SEP-INSD-INFO.                     D6H18660
018280              15  MRESIV-PERS-PROP-ACTION-TYPE  PIC X.
018290              15  MRESIV-SILV-SEP-INSD-IND      PIC X.
018300                  88  MRESIV-SILV-SEP-INSD          VALUE 'Y'.
018310              15  MRESIV-JEWL-SEP-INSD-IND      PIC X.
018320                  88  MRESIV-JEWL-SEP-INSD          VALUE 'Y'.
018330              15  MRESIV-FURS-SEP-INSD-IND      PIC X.
018340                  88  MRESIV-FURS-SEP-INSD          VALUE 'Y'.
018350              15  MRESIV-GUNS-SEP-INSD-IND      PIC X.
018360                  88  MRESIV-GUNS-SEP-INSD          VALUE 'Y'.
018370              15  MRESIV-CAMS-SEP-INSD-IND      PIC X.
018380                  88  MRESIV-CAMS-SEP-INSD          VALUE 'Y'.
018390              15  MRESIV-ARTS-SEP-INSD-IND      PIC X.
018400                  88  MRESIV-ARTS-SEP-INSD          VALUE 'Y'.

018410          10  MRESIV-BILLING-TERM               PIC X.
018420              88  MRESIV-ONE-YEAR                   VALUE '1'.
018430              88  MRESIV-THREE-YEAR                 VALUE '3'.

018440          10  MRESIV-MODERNIZATION-INFO.                          D6H18830
018450              15  MRESIV-MOD-ACTION-TYPE        PIC X.
018460              15  MRESIV-CENTRAL-HEAT-IND       PIC X.
018470                  88  MRESIV-CENTRAL-HEAT           VALUE 'Y'.
018480              15  MRESIV-RENOVATION-IND         PIC X.
018490                  88  MRESIV-RENOVATION             VALUE 'Y'.

018500          10  MRESIV-UNPROT-DWLG-INFO.                            D6H18890
018510              15  MRESIV-UNPROT-ACTION-TYPE      PIC X.
018520              15  MRESIV-UNPROT-WATER-SOURE-TBL.
018520                20  MRESIV-UNPROT-WATER-SOURE-INFO  OCCURS 3 TIMES
018530                                                    INDEXED BY
018530                                              MRESIV-UNPROT-INDEX.
018540                  25  MRESIV-UNPROT-WATER-SOURCE PIC X.
018550              15  MRESIV-UNPROT-FIRE-DEPT        PIC X.
018560                  88  MRESIV-PAID-FIRE-DEPT          VALUE '1'.
018570                  88  MRESIV-VOL-FIRE-DEPT           VALUE '2'.
018580              15  MRESIV-UNPROT-WATER-CAPACITY   PIC X(4).        D6H18970
018590              15  MRESIV-UNPROT-VISIBLE-IND      PIC X.           D6H18980
018600                  88  MRESIV-UNPROT-VISIBLE          VALUE 'Y'.
018610              15  MRESIV-UNPROT-ACCESS-IND       PIC X.           D6H19000
018620                  88  MRESIV-UNPROT-ACCESS           VALUE 'Y'.

018630          10  MRESIV-SEACOAST-PROP-INFO.                          D6H19020
018640              15  MRESIV-SEA-ACTION-TYPE        PIC X.
018650              15  MRESIV-SEA-FEET               PIC X(4).
018650              15  MRESIV-SEA-MILES              REDEFINES
018650                  MRESIV-SEA-FEET               PIC 99V99.
018660              15  MRESIV-SEA-ABOVE-TIDE         PIC X(4).
018670              15  MRESIV-SEA-WALL-PROT          PIC X.
018680                  88  MRESIV-SEAWALL-PROT           VALUE 'Y'.
018690              15  MRESIV-SEA-BARRIER-PROT       PIC X.
018700                  88  MRESIV-BARRIER-PROT           VALUE 'Y'.
018710              15  MRESIV-SEA-CONDO-FLOOR-NO     PIC XX.
018720              15  MRESIV-SEA-CONDO-FACING-IND   PIC X.
018730                  88  MRESIV-SEA-CONDO-FACING       VALUE 'Y'.
018740              15  MRESIV-SEA-ELIG-WIND-POOL-IND PIC X.
018750                  88  MRESIV-SEA-ELIG-WIND-POOL     VALUE 'Y'.
018760              15  MRESIV-SEA-FLOOD-INS-IND      PIC X.
018770                  88  MRESIV-SEA-FLOOD-INS          VALUE 'Y'.

018780          10  MRESIV-SUBURBAN-RATING-INFO.                        D6H19170
018790              15  MRESIV-SUB-FIRE-DIST          PIC X(15).
018800              15  MRESIV-SIB-PROT-RATING        PIC X(4).

018810          10  MRESIV-FULL-TIME-EMPL-IND         PIC X.
018820              88  MRESIV-FULL-TIME-EMPL             VALUE 'Y'.
 
018830          10  MRESIV-TENN-XL-IND                PIC X.
018840          10  MP83-FORM-ONS                     PIC XX.
018850          10  MP83-FORM-OFF                     PIC XX.
018860*********THE FOLLOWING FIELDS ARE FOR EXPANDED HA-61-B**********
018870          10  MRESIV-VIE-J-RVN                  PIC XX.
018880          10  MRESIV-VIE-F-RVN                  PIC XX.
018890          10  MRESIV-VIE-C-RVN                  PIC XX.
018900          10  MRESIV-VIE-FA-RVN                 PIC XX. 
018910          10  MRESIV-VIE-S-RVN                  PIC XX.
018920          10  MRESIV-VIE-G-RVN                  PIC XX.
018930          10  MRESIV-VIE-M-I-RVN                PIC XX.
018940          10  MRESIV-VIE-C-C-RVN                PIC XX.
018950          10  MRESIV-VIE-H-C-RVN                PIC XX.
018960          10  MRESIV-VIE-TEMP4-RVN              PIC XX.
018970          10  MRESIV-SILVER-06-IND              PIC X.
018980              88  MRESIV-SILVER-06-YES              VALUE '1'.
018990              88  MRESIV-SILVER-06-NO               VALUE SPACE.
019000          10  MRESIV-EXP-HA61B-IND              PIC X.
019010              88  MRESIV-EXP-HA61B-YES              VALUE '1'.
019020          10  MRESIV-FIRE-CODE                  PIC X.
019030          10  MRESIV-BURG-CODE                  PIC X.
019040          10  MRESIV-FIRE-ONS                   PIC X.
019050          10  MRESIV-BURG-ONS                   PIC X.
019060          10  MRESIV-FIRE-OFF                   PIC X. 
019070          10  MRESIV-BURG-OFF                   PIC X.

019090          10  MRESIV-BILL-METHOD                PIC X.
019150          10  MRESIV-SM-ACCT-NO                 PIC X(10).
019160          10  MRESIV-SM-ACCT-NUM                REDEFINES
019160               MRESIV-SM-ACCT-NO.
019170              15  MRESIV-SM-ACCT-NUMBER         PIC X(7).         D6H19560
019180              15  MRESIV-SM-ACCT-EXTRA          PIC X(3).         D6H19570
019230          10  MRESIV-ACP-ACCT-NO                PIC X(10).

019240          10  MRESIV-MNON-BILL-METHOD-TBL.
019240              15  MRESIV-MNON-BILL-METHOD   OCCURS 20 TIMES
019250                                            INDEXED BY
019260                                        MRESIV-MNON-BILL-INDEX-1. D6H19650
019270                  20 MRESIV-MNON-BILL-METH      PIC X.

019280          10  MRESIV-SM-ACCT-NAME               PIC X(4).
019300          10  MRESIV-TX-PC-SW                   PIC X.
019310          10  MRESIV-ADDL-ID                    PIC X(20).
019320          10  MRESIV-BILL-ACTION-TYPE           PIC X.
019330              88  MRESIV-INIT-VALUE                 VALUE  ' '.
019340              88  MRESIV-ADD-ONLY                   VALUE  'A'.
019350              88  MRESIV-CHANG-ONLY                 VALUE  'C'.
019360              88  MRESIV-ELIM-ONLY                  VALUE  'E'.
019410          10  MRESIV-AUTO-STAT-CODE             PIC X.
019420              88  MRESIV-AUTO-1                     VALUE '1'.
019430              88  MRESIV-AUTO-2                     VALUE '2'.
019440              88  MRESIV-AUTO-3                     VALUE '3'.
019450              88  MRESIV-NO-ACCT-ON-POL             VALUE '9'.
019460          10  MRESIV-SPLT-ACT-TYPE              PIC X.
019470          10  MRESIV-SPLT-RES1                  PIC XXX.
019480          10  MRESIV-SPLT-RES2                  PIC XX.
019490          10  MRESIV-AUTO-STAT-ONS              PIC X.
019500          10  MRESIV-AUTO-STAT-OFF              PIC X.
019510          10  MRESIV-SM-ACCT-PHONE              PIC X(14).
019520          10  MRESIV-ALTDIS-INDICATOR           PIC X.
019530          10  MRESIV-AGT-REC-NAME               PIC X(30).
019540          10  MRESIV-AGENT-CONVERT-IND          PIC X.
019550          10  MRESIV-BRICK-VENEER-IND           PIC X.
019560          10  MRESIV-AEA-FIRST-CONVERT          PIC X.
019570              88  M-AEA-FIRST-CONVERT-NO            VALUE 'N'.
019580              88  M-AEA-FIRST-CONVERT-YES           VALUE 'Y'.
019590          10  MRESIV-EQ-VENEER-IND              PIC X.
019600          10  MRESIV-DIARY-COMBILL-IND          PIC X.
019610          10  MRESIV-DIARY-BILL-ACTION-TYPE     PIC X.
019620              88  MRESIV-DIARY-INIT-VALUE           VALUE ' '.
019630              88  MRESIV-DIARY-ADD-ONLY             VALUE 'A'.
019640              88  MRESIV-DIARY-CHANG-ONLY           VALUE 'C'.
019650              88  MRESIV-DIARY-ELIM-ONLY            VALUE 'E'.
BL3983          10  MRESIV-COMB-BILL-ACCT-NO          PIC X(13).
BL3983          10  MRESIV-CBILL-ACCT-NO              REDEFINES
BL3983               MRESIV-COMB-BILL-ACCT-NO.
BL3983              15  MRESIV-CBILL-ACCT-PREFIX      PIC X(2).
BL3983              15  MRESIV-CBILL-ACCT-3-POS       PIC X(3).
BL3983              15  MRESIV-CBILL-ACCT-6-POS       PIC X(6).
BL3983              15  MRESIV-CBILL-ACCT-SUFFIX      PIC X(2).
BL3983          10  MRESIV-NEW-ASA-ACCT-NO            REDEFINES
BL3983               MRESIV-CBILL-ACCT-NO.
BL3983              15  MRESIV-ASA-ACCT-PREFIX        PIC X.
BL3983              15  MRESIV-ASA-ACCT-3-POS         PIC X(3).
BL3983              15  MRESIV-ASA-ACCT-6-POS         PIC X(6).
BL3983              15  MRESIV-ASA-ACCT-SUFFIX        PIC X(3).
BL3983          10  MRESIV-OLD-ASA-ACCT-NO            REDEFINES
BL3983               MRESIV-NEW-ASA-ACCT-NO.
BL3983              15  MRESIV-OLD-ASA-PREFIX         PIC X(2).
BL3983              15  MRESIV-OLD-ASA-8-POS          PIC X(8).
BL3983              15  MRESIV-FILLER                 PIC X(3).
BL3983          10  MRESIV-DIARY-COMB-BILL-ACCT-NO    PIC X(13).
BL3983          10  MRESIV-DIARY-CBILL-ACCT-NO        REDEFINES
BL3983               MRESIV-DIARY-COMB-BILL-ACCT-NO.
BL3983              15  MRESIV-DIARY-CBILL-ACCT-PREFIX   PIC X(2).
BL3983              15  MRESIV-DIARY-CBILL-ACCT-3-POS    PIC X(3).
BL3983              15  MRESIV-DIARY-CBILL-ACCT-6-POS    PIC X(6).
BL3983              15  MRESIV-DIARY-CBILL-ACCT-SUFFIX   PIC X(2).
BL3983          10  MRESIV-DIARY-NEW-ASA-ACCT-NO         REDEFINES
BL3983               MRESIV-DIARY-CBILL-ACCT-NO.
BL3983              15  MRESIV-DIARY-ASA-ACCT-PREFIX     PIC X.
BL3983              15  MRESIV-DIARY-ASA-ACCT-3-POS      PIC X(3).
BL3983              15  MRESIV-DIARY-ASA-ACCT-6-POS      PIC X(6).
BL3983              15  MRESIV-DIARY-ASA-ACCT-SUFFIX     PIC X(3).
BL3983          10  MRESIV-DIARY-OLD-ASA-ACCT-NO         REDEFINES
BL3983               MRESIV-DIARY-NEW-ASA-ACCT-NO.
BL3983              15  MRESIV-DIARY-OLD-ASA-PREFIX      PIC X(2).
BL3983              15  MRESIV-DIARY-OLD-ASA-8-POS       PIC X(8).
BL3983              15  MRESIV-DIARY-FILLER              PIC X(3).
BL3983          10  MRESIV-ACCT-CR-MSG-501               PIC X.

H03000          10  SAF-HO-NOTA-71-FIX-SW             PIC X.
HO3000              88  NOTA-71-PRERENEWAL-DONE           VALUE 'Y'.
HO3454          10  MRESIV-COASTAL-CONVERT-CO         PIC X.
HO3454          10  MRESIV-COASTAL-WIND-ROLL-ON       PIC X.
HO3454              88  MRESIV-NO-WIND                    VALUE '0'.
HO3454              88  MRESIV-WIND-HA195                 VALUE '1'.
HO3454              88  MRESIV-WIND-HA196                 VALUE '2'.
019720          10  MRESIV-DIARY-BILL-METHOD          PIC X.
019730          10  MRESIV-TX-RISK-IND                PIC X.
019740              88  MRESIV-TX-PC-ACV                  VALUE '1'.
019750              88  MRESIV-TX-PC-REPL-COST            VALUE '2'.
019760          10  MRESIV-NEW-AEST-IND               PIC X.
019770              88  MRESIV-OLD-AEST-PGM               VALUE SPACE.
019780              88  MRESIV-OLD-AEST-PGM-NRF           VALUE '1'.
019790              88  MRESIV-NEW-AEST-PGM               VALUE '2'.
019810          10  MRESIV-HO-216-STAT-IND            PIC X.
019810          10  MRESV-HA-6-RECOVERY-SW            PIC X.
                10  MRESIV-STAT-FORM-ONS              PIC 99.
                10  MRESIV-STAT-FORM-OFF              PIC 99.
HO2384          10  MRESIV-MICH-PREF-IND              PIC X.
HO2397          10  MRESIV-HA9-FORM-IND               PIC X.
HO2397              88  MRESIV-HA9-FORM-PRESENT           VALUE '1'.
W08390          10  MRESIV-SAVE-SORT-CODE             PIC X.


W08390      05  MRESIV-ENDT-GEN-IND-AREA.                               D6H20710
W08390          10  MRESIV-ENDT-AND-INDICATOR.                          D6H20720
W08390              15  MRESIV-CONT-REPL-COST-1       PIC X.
W08390              15  MRESIV-HOME-REPL-GUAR-2       PIC X.
W08390              15  MRESIV-ACE-3                  PIC X.
W08390              15  MRESIV-CRED-CARD-COV-4        PIC X.
W08390              15  MRESIV-EARTHQUAKE-5           PIC X.
W08390              15  MRESIV-THEFT-EXT-6            PIC X.
W08390              15  MRESIV-PER-PROP-SPECIAL-7     PIC X.
W08390              15  MRESIV-HIGH-VAL-HOME-8        PIC X.
W08390              15  MRESIV-PROT-DEVICES-9         PIC X.
W08390              15  MRESIV-BL-HULL-2500-10        PIC X.
W08390              15  MRESIV-BL-HULL-5000-11        PIC X.
W08390              15  MRESIV-INC-LIM-SPECIAL-12     PIC X.
W08390              15  MRESIV-CONDO-ADD-ALTER-13     PIC X.
W08390              15  MRESIV-CONDO-ADD-RISKS-14     PIC X.
W08390              15  MRESIV-CONDO-LOSS-15          PIC X.
W08390              15  MRESIV-CONDO-LOSS-RISKS-16    PIC X.
W08390              15  MRESIV-CONDO-RENT-OTHER-17    PIC X.
W08390              15  MRESIV-VALUABLE-ITEMS-18      PIC X.
W08390              15  MRESIV-ADD-INTEREST-19        PIC X.
W08390              15  MRESIV-ADD-RES-RENT-OTH-20    PIC X.
W08390              15  MRESIV-OFFICE-PROF-COV-21     PIC X.
W08390              15  MRESIV-OTH-STRUCT-INCR-22     PIC X.
W08390              15  MRESIV-OTH-STRUCT-RENT-23     PIC X.
W08390              15  MRESIV-OTH-STRUCT-CONDO-24    PIC X.
W08390              15  MRESIV-OTH-STRUCT-RISKS-25    PIC X.
W08390              15  MRESIV-WATERCRAFT-26          PIC X.
W08390              15  MRESIV-SNOWMOBILE-27          PIC X.
W08390              15  MRESIV-3-4-FAM-LIAB-28        PIC X.
W08390              15  MRESIV-XL-29                  PIC X.
W08390              15  MRESIV-XL-NY-30               PIC X.
W08390              15  MRESIV-XL-ADMEND-31           PIC X.
W08390              15  MRESIV-250-SPEC-THEFT-32      PIC X.
W08390              15  MRESIV-500-SPEC-THEFT-33      PIC X.
W08390              15  MRESIV-THEFT-NY-34            PIC X.
W08390              15  MRESIV-EARTHQKE-HO5-35        PIC X.
W08390              15  MRESIV-EARTHQKE-MV-36         PIC X.
W08390              15  MRESIV-3-4-FAM-HO4-37         PIC X.
W08390              15  MRESIV-OFFICE-OCCUPIED-INS-38 PIC X.
W08390              15  MRESIV-XL-FLA-UNINSURED-39    PIC X.
W08390              15  MRESIV-PER-PROP-CONDO-40      PIC X.
W08390              15  MRESIV-EARTHQUAKE-OLD-NEW-SW  PIC X.
HO3165              15  MRESIV-EQ-LETTER-SW           PIC X.
DEBDEB              15  MRESIV-FLOOD-ZONE             PIC X.
DEBDEB              15  MRESIV-COASTAL-SHORE          PIC X.
DEBDEB              15  MRESIV-RESOLUTION             PIC X.
HO3392              15  MRESIV-TX-HO160-CONVERT-SW    PIC X.
HO3454              15  MRESIV-COASTAL-WIND-OPTIONAL  PIC X.
HO3511              15  MRESIV-COAST-PRERENEW-MESSAGE PIC X.
HO3538              15  MRESIV-WINDPOOL-EXCL-HO194    PIC X.
W08390              15  MFILLER-50                    PIC X.            22110006

W08390          10  MRESIV-IND                        REDEFINES
W08390              MRESIV-ENDT-AND-INDICATOR         OCCURS 50 TIMES
W08390                                                INDEXED BY
W08390                                                M-IND-X-1.
W08390              15  MRESIV-INDICATOR-NO           PIC X.

W08390          10  MRESIV-ALARMS-SECURITY-AREA.                        D6H21340
W08390              15  MRESIV-FIRE.
W08390                  20  MRESIV-SMOKE-DETECTOR         PIC X.
HO3411                      88  MRESIV-SMK-DETECTOR           VALUE 'Y'.
HO3411                      88  MRESIV-NO-SMK-DETECTOR        VALUE 'N'.
W08390                  20  MRESIV-FIRE-EXTINGUISHER      PIC X.
HO3411                      88  MRESIV-FIRE-EXT               VALUE 'Y'.
HO3411                      88  MRESIV-NO-FIRE-EXT            VALUE 'N'.
W08390                  20  MRESIV-FIRE-ALARM             PIC X.
W08390                      88  MRESIV-FIRE-CENTRAL           VALUE 'A'.
W08390                      88  MRESIV-FIRE-DEPT              VALUE 'B'.
W08390                      88  MRESIV-FIRE-LOCAL             VALUE 'C'.
W08390                  20  MRESIV-SENSAPHONE-DUOPHONE    PIC X.
HO3411                      88  MRESIV-SENSAPHN-DUOPHN        VALUE 'Y'.
HO3411                      88  MRESIV-NO-SENSAPHN-DUOPHN     VALUE 'N'.
W08390                  20  MRESIV-SPRINKLERS             PIC X.
HO3411                      88  MRESIV-SPRKLER-ALL-AREAS      VALUE 'I'.
HO3411                      88  MRESIV-SPRKLER-LIVING-AREA    VALUE 'J'.

W08390              15  MRESIV-BURGLARY.
W08390                  20  MRESIV-DEAD-BOLT              PIC X.
HO3411                      88  MRESIV-DEAD-BLTS              VALUE 'Y'.
HO3411                      88  MRESIV-NO-DEAD-BLTS           VALUE 'N'.
W08390                  20  MRESIV-BURGLARY-ALARM         PIC X.
W08390                      88  MRESIV-BURG-CENTRAL           VALUE 'A'.
W08390                      88  MRESIV-BURG-POLICE-DEPT       VALUE 'B'.
W08390                      88  MRESIV-BURG-LOCAL             VALUE 'C'.
W08390                  20  MRESIV-APT-CONDO.
W08390                      25  MRESIV-LOCKED-ENTRY       PIC X.
HO3411                          88  MRESIV-LCKED-ENTRY        VALUE 'Y'.
HO3411                          88  MRESIV-NO-LCKED-ENTRY     VALUE 'N'.
W08390                      25  MRESIV-RES-MGR            PIC X.
HO3411                          88  MRESIV-RES-MNGR           VALUE 'Y'.
HO3411                          88  MRESIV-NO-RES-MNGR        VALUE 'N'.
W08390                      25  MRESIV-DOORMAN-GUARD      PIC X.
HO3411                          88  MRESIV-LIMITED-ACCESS     VALUE 'A'.
HO3411                          88  MRESIV-SELF-ENCLOSED      VALUE 'B'.
W08390              15  MRESIV-OTHER-DEVICE-CR            PIC 99 COMP-3.

W08390          10  MRESIV-ALTERNATE-HEATING-AREA.                      D6H21710
W08390              15  MRESIV-OTH-TYPE-HEAT          PIC X(15).
W08390              15  MRESIV-INSTALLED-BY-WHOM      PIC X(15).
W08390          10  MRESIV-3-OR-4-FAM-COVG.                             D6H21740
W08390              15  MRESIV-NO-FAMILIES            PIC X.
W08390              15  MRESIV-MED-PAY                PIC X.
W08390              15  MRESIV-OLT-TERR               PIC XX.
W08390          10  MRESIV-DED-AMT                    PIC 999.
HO5366          10  MRESIV-ACTION-TYPE                PIC X.
HO5366              88  MRESIV-PC-ADD-ONLY                VALUE 'A'.
HO5366              88  MRESIV-PC-CHANGE-ONLY             VALUE 'C'.
HO5366              88  MRESIV-PC-ELIMINATE-ONLY          VALUE 'E'.
HO5366          10  MRESIV-CONST-QUAL                 PIC X(3).
HO5360          10  MRESIV-HO365-SW                   PIC X.
BL5746          10  MRESIV-EP-TX-FORM-IND             PIC X.
BL5746          10  MRESIV-EP-CW-FORM-IND             PIC X.
HO9038          10  MRESIV-GEM-PCUP-UPDATE-IND        PIC X.
GM0068          10  MRESIV-BEU-VENDOR.                                  D6H21880
GM0068              15  MRESIV-BEU-VENDOR-CODE        PIC X.
GM0068              15  MRESIV-BEU-VENDOR-STATUS      PIC X.
GM0068          10  MRESIV-BEU-EXTRACT-IND            PIC X.
HO3104          10  MRESIV-ADDL-RESID-MSG-IND         PIC X.
HO3148          10  MRESIV-TX-OLD-TERRITORY           PIC X(3).
HO3148          10  MRESIV-TX-OLD-FR-TERRITORY        PIC X(3).
HO3228          10  MRESIV-WITHDRAW-LOB-VIA-REIN      PIC X.
HO3228              88  MRESIV-MICH-REIN-WITH-LETTER      VALUE 'Y'.
HO3228              88  MRESIV-MICH-REIN-WOUT-LETTER      VALUE 'N'.
HO3325              88  MRESIV-REIN-AGENCY-TERM           VALUE 'A'.
862210              88  MRESIV-NC-COASTAL-LETTER          VALUE 'C'.
882130              88  MRESIV-SECURE-NONREN-LETTER       VALUE 'S'.
SCHURR              88  MRESIV-SC-HURR-NONREN-LETTER      VALUE 'H'.
HO3227          10  MRESIV-GENERAL-CREDITS-STATS.                       D6H21990
HO3227              15  MRESIV-GENERAL-CR-1           PIC X.
HO3227              15  MRESIV-GENERAL-CR-2           PIC X.
HO3227              15  MRESIV-GENERAL-CR-3           PIC X.
HO3227              15  MRESIV-GENERAL-CR-4           PIC X.
HO3227              15  MRESIV-GENERAL-CR-5           PIC X.
HO3325          10  MRESIV-WITHDRAW-REIN-DONE-SW      PIC X.
HO3325              88  MRESIV-REIN-IS-DONE               VALUE 'Y'.
HO3325              88  MRESIV-REIN-IS-NOT-DONE           VALUE 'N'.
SC9202          10  MRESIV-BUSINESS-CENTER-CODE       PIC X(3).
SC9202          10  MRESIV-ASO-PSO-INDICATOR          PIC X.
SC9202          10  MRESIV-CROSS-SOLD-INDICATOR       PIC X.
HO3227          10  MRESIV-BCENTER-PURGE-IND          PIC X.


019820      05  MRESV-FILLER-AREA                                COMP-3.
019830          10  MRESV-PRIME-CREDIT-IND            PIC S9.
019840              88  MRESV-NEW-HOME-CREDIT             VALUE +0.
019850              88  MRESV-AGE-OF-DWLG-CREDIT          VALUE +1.
019860              88  MRESV-RENOVATED-CREDIT            VALUE +2.
019870          10  MRESV-SUB-RATED-IND               PIC S9   VALUE +0.
019880              88  MRESV-SUBURBAN-RATED              VALUE +1.
019890              88  MRESV-INVALID-FOR-SUB-RTNG        VALUE +9.
019900          10  MRESV-PRE-REN-INFO-CHECKED        PIC S9   VALUE +0.
019910          10  MRESV-SM-IND                      PIC S9.
019920              88  MRESV-SPONS-MKT                   VALUE +1.
019930              88  MRESV-SM-NO-ACCT                  VALUE +2.
019940              88  MRESV-SM-INV-ACCT                 VALUE +3.
019950          10  MRESV-CBS-ARS-IND                 PIC S9.
019960              88  MRESV-CBS-BILL-METHOD             VALUE +1.
019970              88  MRESV-ARS-BILL-METHOD             VALUE +2.
HO2027          10  MRESV-100-250-DED-SW              PIC S9.
HO2027              88  MRESV-DO-100-250-DED-CNV          VALUE +0.
HO2027              88  MRESV-100-250-DED-CNV-DONE        VALUE +1.
020000          10  MRESV-OUT-OF-SEQ-IND              PIC S9.
HO5366          10  MRESV-AEST-CODE                   PIC S9.
HO5366          10  MRESV-PC-AEST-INFO-IND            PIC S9.
HO5366              88  MRESV-PC-AEST-INFO-INCOMPLETE     VALUE +0.
HO5366              88  MRESV-PC-AEST-INFO-COMPLETE       VALUE +1.
PAG030          10  MRESV-AETNA-86-RENEW-IND          PIC S9   VALUE +0.
HO9038          10  MRESV-GEMINI-POLICY-IND           PIC S9   VALUE +0.
HO9038              88  MRESV-GEMINI-POLICY               VALUE +1.
HO9038          10  MRESV-GEMINI-PRODUCE-BEU-IND      PIC S9   VALUE +0.
HO9038              88  MRESV-GEM-PRODUCE-BEU-I           VALUE +1.
HO9038              88  MRESV-GEM-PRODUCE-BEU-II          VALUE +2.
HO3028          10  MRESV-HOT-PURE-IND                PIC S9.
HO3208              88  MRESV-UPGRADE-HOT                 VALUE +0.
HO3028              88  MRESV-UPGRADE-PURE                VALUE +1.
HO9079          10  MRESV-FAMCO-CONVERT-IND           PIC S9.
HO9079              88  MRESV-INITIAL-FAMCO-CONVERT       VALUE +1.
HO9079              88  MRESV-FAMCO-CONVERTED             VALUE +2.
HO9079              88  MRESV-EP-FULL-ASSM                VALUE +3.
HO9079              88  MRESV-NO-EP-FULL-ASSM             VALUE +4.
HO9079              88  MRESV-EP-NO-FAMCO-CONVERT         VALUE +5.
HO9079          10  MRESV-FAMCO-FILE-ACCESS-IND       PIC S9.
HO3088          10  MRESV-XL-NO-RENTAL-UNITS          PIC S9.
AU4152          10  MRESV-ROUTING-HEADER-IND.                           D6H22560
AU4152              15  MRESV-G2-ROUTE-HDR-BUILT      PIC S9.
AU4152              15  MRESV-Q1-ROUTE-HDR-BUILT      PIC S9.
AU4152              15  MRESV-E1-ROUTE-HDR-BUILT      PIC S9.
AU4152              15  MRESV-I1-ROUTE-HDR-BUILT      PIC S9.
AU4152              15  FUTURE-USE-3                  PIC S9.
AU4152              15  FUTURE-USE-4                  PIC S9.
AU4152              15  FUTURE-USE-5                  PIC S9.
WHFIX           10  MRESV-WH-INDICATOR                PIC S9.
WHFIX               88  WH-ADJUST-BASE-PREM               VALUE +1.
HO3237          10  MRESV-AETNA91-SW                  PIC S9.
HO3187          10  MLONG-CREDIT-IND                  PIC S9.
744710          10  MLONG-NUMBER-OF-LOSSES            PIC S9.
744710          10  MLONG-NUMBER-OF-LOSSES-3YR     REDEFINES
744710              MLONG-NUMBER-OF-LOSSES            PIC S9.
744710          10  MLONG-OVR-NUMB-OF-LOSSES          PIC S9.
744710          10  MLONG-OVR-NUMB-OF-LOSSES-3YR   REDEFINES
744710              MLONG-OVR-NUMB-OF-LOSSES          PIC S9.
HO3236          10  MRESV-VAP-STAT-IND                PIC S9.
HO3249          10  MRESV-FL-GOLF-IND                 PIC S9.
HO3256          10  MRESV-HO410-IND                   PIC S9.
HO3256          10  MRESV-MASS-LEAD-PAINT-IND         PIC S9.
HO3256          10  MRESV-HO410-BLOCK-ROLLON          PIC S9.
SC9201          10  MRESV-SERVICE-CENTER-POL-IND      PIC S9.
SC9201              88  MRESV-SERVICE-CENTER-POLICY       VALUE +1. 
SM3363          10  MRESV-VAP-ROLLED-ON               PIC S9.
HO3378          10  MRESV-MINE-SUB-MAND-IND           PIC S9.
HO3383          10  MRESV-NY-187-CLASS-CODE-CONV      PIC S9.
HO3392          10  MRESV-EP-TYPED-ENDT-PRESENT       PIC S9.
HO3405***************************************************************** D6H22830
HO3405***THIS SWITCH WAS ADDED FOR NC HO-31 CHANGE.  IF THE FIELD       D6H22840
HO3405***IS '1', INDICATES THAT THE LIMIT FOR HO-31 HAS ALREADY BEEN    D6H22850
HO3405***CONVERTED AND WILL NOT BE CONVERTED ON SEBSEQUENT TRXS.        D6H22860
HO3405***SW SET IN M2MR02A FOR NB AND RECI AND M2MW12F FOR RE'S.  GEM.  D6H22870
HO3405***************************************************************** D6H22880
HO3405          10  MRESV-ENDT-LIMIT-CONV-IND         PIC S9.
HO3405              88  ENDT-LIMIT-NOT-CONVERTED          VALUE +0.
HO3405              88  ENDT-LIMIT-CONVERTED              VALUE +1.
DEBDEB          10  MRESV-HO-CONDO-INCR-IND           PIC S9.
HO3411***************************************************************** D6H22930
HO3411***THIS SWITCH WAS ADDED FOR A FIX FOR THE COVERAGE D PORTION     D6H22940
HO3411***OF THE TENANT CONDO NICHE PROJECT.  ONCE COVERAGE D FOR A      D6H22950
HO3411***DAMAGED POLICY HAS BEEN CORRECTED, THE SWITCH IS SET SO THAT   D6H22960
HO3411***THE POLICY IS NOT AFFECTED AGAIN.    GEM.                      D6H22970
HO3411***************************************************************** D6H22980
HO3411          10  MRESV-COV-D-CONVERT               PIC S9.
HO3575          10  MRESV-COUNTERSIG-RATIO-SW         PIC S9.
HO3575              88  COUNTERSIG-NOT-ADJUSTED           VALUE +0.
HO3575              88  COUNTERSIG-ADJUSTED               VALUE +1.
661900          10  MLOSS-FREE-IND                    PIC S9.
744710          10  MLONG-NUMBER-OF-LOSSES-5YR        PIC S9.
744710          10  MLONG-OVR-NUMB-OF-LOSSES-5YR      PIC S9.
SRMSRM          10  MFILLER-1                         PIC S9.           23950006
SRMSRM          10  MFILLER-2                         PIC S9.           23960006
SRMSRM          10  MFILLER-3                         PIC S9.           23970006
SRMSRM          10  MFILLER-4                         PIC S9.           23980006
SRMSRM          10  MFILLER-5                         PIC S9.           23990006
020440          10  MRESV-MAX-NB                      PIC S999.
020450          10  MRESV-MAX-CREDIT-NB               REDEFINES
020450               MRESV-MAX-NB                     PIC SV999.
020470          10  MRESV-MAX-RE                      PIC S999.
020480          10  MRESV-MAX-CREDIT-RE               REDEFINES
020480               MRESV-MAX-RE                     PIC SV999.
020500          10  MRESV-RENEWAL-CR                  PIC S999.
020510          10  MRESV-REN-CR                      REDEFINES
020510               MRESV-RENEWAL-CR                 PIC SV999.
020530          10  MRESV-SM-SURCHG                   PIC S999.
020540          10  MRESV-SM-SURCHG-PCT               REDEFINES
020540               MRESV-SM-SURCHG                  PIC SV999.
020560          10  MRESV-SM-DEV                      PIC S999.
020570          10  MRESV-SM-DEV-PCT                  REDEFINES
020570               MRESV-SM-DEV                     PIC SV999.
020590          10  MRESV-SUB                         PIC S999.
BL3983          10  MRESV-DRAW-DATE                   PIC S999.
020610          10  MRESV-SPLT-OFFICE                 PIC S999.
020620          10  MRESV-ALTDIS-NB-COM               PIC S999.
020630          10  MRESV-ALTDIS-RE-COM               PIC S999.
020640          10  MRESV-AGT-REC-PERC                PIC S999.
020650          10  MRESV-AGT-PRIME-PERC              PIC S999.
020660          10  MRESV-TX-VMM-RATE                 PIC S9V99.
020670          10  MRESV-HTFD-STD-DEV-CR             PIC S9V99.
020680          10  MRESV-STD-HTFD-CR-PERC            PIC S9V99.
020690          10  MRESV-NB-OR-RE-DEV-USED           PIC S9V99.
BL3983          10  MRESV-DIARY-DRAW-DATE             PIC S999.
HO5366          10  MRESV-NUMBER-UNITS                PIC S999.
020720          10  MRESV-TX-LLOYDS-TERR              PIC S999.
020730          10  MRESV-SOURCE-CODE                 PIC S999.
HO7032          10  MRESV-TX-CREDIT-PERCENT           PIC S9V99.
HO7032          10  MRESV-APPROVED-ROOF-CR            PIC S999.
HO7032          10  MRESV-TX-LLOYDS-DEV-CR            PIC S9V99.
HO7081          10  MRESV-TX-ACCT-CR-PERCENT          PIC SV999.
HO3227          10  MRESV-TOTAL-GENERAL-CR            PIC SV999.
HO3187          10  MLONG-LC-CREDIT                   PIC S999.
HO3187          10  MLONG-LONG-CR                     REDEFINES
HO3187               MLONG-LC-CREDIT                  PIC SV999.
HO3187          10  MLONG-YRS-OF-INSURANCE            PIC S999.
HO3268          10  MRESV-TX-PREM-SURCH-PCT           PIC S999.
HO3498          10  M-EQ-COMM-RATE                    PIC SV999.
HO3584          10  M-EQ-CNTRSIG-COMM-RT-1            PIC SV999.
HO3584          10  M-EQ-CNTRSIG-COMM-RT-2            PIC SV999.
HO3584          10  M-UMB-COMM-RATE                   PIC SV999.
HO3584          10  M-UMBR-CNTRSIG-COMM-RT-1          PIC SV999.
HO3584          10  M-UMBR-CNTRSIG-COMM-RT-2          PIC SV999.
LOSSUR          10  MRESV-LOSS-SURCH-FACTOR           PIC S9V99.        24460006
SRMSRM          10  MFILLER-7                         PIC S999.         24470006
SRMSRM          10  MFILLER-8                         PIC S999.         24480006
SRMSRM          10  MFILLER-9                         PIC S999.         24490006
SRMSRM          10  MFILLER-10                        PIC S999.         24500006
SRMSRM          10  MFILLER-11                        PIC S999.         24510006
SRMSRM          10  MFILLER-12                        PIC S999.         24520006
SRMSRM          10  MFILLER-13                        PIC S999.         24530006
SRMSRM          10  MFILLER-14                        PIC S999.         24540006
SRMSRM          10  MFILLER-15                        PIC S999.         24550006
SRMSRM          10  MFILLER-16                        PIC S999.         24560006
SRMSRM          10  MFILLER-17                        PIC S999.         24570006
SRMSRM          10  MFILLER-18                        PIC S999.         24580006
SRMSRM          10  MFILLER-19                        PIC S999.         24590006
SRMSRM          10  MFILLER-20                        PIC S999.         24600006
SRMSRM          10  MFILLER-21                        PIC S999.         24610006

021040          10  MRESV-MODERNIZATION-INFO.                           D6H23700
021050              15  MRESV-MOD-HEAT-YR             PIC S9(5).
021060              15  MRESV-MOD-ELEC-YR             PIC S9(5).
021070              15  MRESV-MOD-PLUMB-YR            PIC S9(5).
021080              15  MRESV-MOD-ROOF-YR             PIC S9(5).
021090          10  MRESV-RENEWAL-CREDITS-INFO.                         D6H23750
021100              15  MRESV-RENEWAL-CREDITS-ONS     PIC S9(3)V99.
021110              15  MRESV-RENEWAL-CREDITS-FULL    PIC S9(3)V99.
021120              15  MRESV-RENEWAL-CREDITS-OFF     PIC S9(3)V99.
021130          10  MRESV-MAX-CREDIT-INFO.                              D6H23790
021140              15  MRESV-MAX-CREDIT-ONS          PIC S9(3)V99.
021150              15  MRESV-MAX-CREDIT-FULL         PIC S9(3)V99.
021160              15  MRESV-MAX-CREDIT-OFF          PIC S9(3)V99.
021170          10  MRESV-SM-DEV-DATE                 PIC S9(5).
021180          10  MRESV-BILL-ENTITY                 PIC S9(5).
021190          10  MRESV-SPLT-AGENCY                 PIC S9(5).
021200          10  MRESV-HO-AUTO-CREDIT              PIC S9(5).
021210          10  MRESV-AGT-REC-COM                 PIC S999V99.
021220          10  MRESV-AGT-PRIME-COM               PIC S999V99.
SRMSRM          10  MFILLER-22                        PIC S9(5).        24820006
HO2271          10  MRESV-WOOD-STOVE-INFO.                              D6H23900
HO2271              15  MRESV-WOOD-STOVE-FULL         PIC S9(3)V99.
HO2271              15  MRESV-WOOD-STOVE-ONS          PIC S9(3)V99.
HO2271              15  MRESV-WOOD-STOVE-OFF          PIC S9(3)V99.
HO5913          10  MRESV-RECOUPMENT-INFO.                              D6H23940
HO5913              15  MRESV-RECOUP-AMT              PIC S9(3)V99.
HO5913              15  MRESV-RECOUP-AR-ONS           PIC S9(3)V99.
HO5913              15  MRESV-RECOUP-AR-OFF           PIC S9(3)V99.
HO3025          10  MRESV-SOURCE-CODE-DATE            PIC S9(5).
HO3028          10  MRESV-REPL-COST-CHANGE-DATE       PIC S9(5).
HO3200          10  M-EQ-RES-RECOVERY-FUND            PIC S999V99.
HO3200          10  M-EQ-RES-RECOVERY-ADMIN           PIC S999V99.
HO3200          10  M-EQ-RES-RECOVERY-FUND-ONS        PIC S999V99.
HO3200          10  M-EQ-RES-RECOVERY-ADMIN-ONS       PIC S999V99.
HO3200          10  M-EQ-RES-RECOVERY-FUND-OFF        PIC S999V99.
HO3200          10  M-EQ-RES-RECOVERY-ADMIN-OFF       PIC S999V99.
HO3451          10  MRESV-NEW-RECOUPMENT-INFO.                          D6H24060
HO3451              15  MRESV-NEW-RECOUP-AMT          PIC S9(3)V99.
HO3451              15  MRESV-NEW-RECOUP-AR-ONS       PIC S9(3)V99.
HO3451              15  MRESV-NEW-RECOUP-AR-OFF       PIC S9(3)V99.
HO3575          10  MRESV-COUNTERSIG-RATIO            PIC S99V999.
GEMGEM          10  MRESV-CAT-SURCH-INFO.                               D6H24060
GEMGEM              15  MRESV-CAT-SURCH-AMT           PIC S9(3)V99.
728020              15  MRESV-FL-CAT-FUND-AMT         REDEFINES
728020                  MRESV-CAT-SURCH-AMT           PIC S9(5).
GEMGEM              15  MRESV-CAT-SURCH-ONS           PIC S9(3)V99.
728020              15  MRESV-FL-CAT-FUND-ONS         REDEFINES 
728020                  MRESV-CAT-SURCH-ONS           PIC S9(5).
GEMGEM              15  MRESV-CAT-SURCH-OFF           PIC S9(3)V99.
728020              15  MRESV-FL-CAT-FUND-OFF         REDEFINES
728020                  MRESV-CAT-SURCH-OFF           PIC S9(5).
607810          10  MRESV-TX-HO-111-INC-LMT           PIC S9(5).
SUMINT          10  MRESV-LOSS-SURCH-AREA.
SUMINT              15  MRESV-LOSS-SURCH-FULL         PIC S9(3)V99.
SUMINT              15  MRESV-LOSS-SURCH-ONS          PIC S9(3)V99.
SUMINT              15  MRESV-LOSS-SURCH-OFF          PIC S9(3)V99.
SUMINT              15  MRESV-DATE-OF-SURCH           PIC S9(5).
SUMINT          10  MRESV-LOSS-FREE-CREDITS-INFO.
SUMINT              15  MRESV-LOSS-FREE-CREDIT-ONS    PIC S9(3)V99.
SUMINT              15  MRESV-LOSS-FREE-CREDIT-FULL   PIC S9(3)V99.
SUMINT              15  MRESV-LOSS-FREE-CREDIT-OFF    PIC S9(3)V99.
SRMSRM          10  MFILLER-23                        PIC S9(5).        25240006
SRMSRM          10  MFILLER-24                        PIC S9(5).        25250006
SRMSRM          10  MFILLER-25                        PIC S9(5).        25260006
SRMSRM          10  MFILLER-26                        PIC S9(5).        25270006
021570          10  MRESV-AEST-MISC-VALUE             PIC S9(7).
021580          10  MRESV-PERS-PROP-VALUE-INFO.                         D6H24270
021590              15  MRESV-SILV-VALUE              PIC S9(7).
021600              15  MRESV-JEWL-VALUE              PIC S9(7).
021610              15  MRESV-FURS-VALUE              PIC S9(7).
021620              15  MRESV-GUNS-VALUE              PIC S9(7).
021630              15  MRESV-CAMERA-VALUE            PIC S9(7).
021640              15  MRESV-ARTS-VALUE              PIC S9(7).
021650          10  MRESV-MICH-FA-PREM                PIC S9(5)V99.
021660          10  MRESV-SM-CREDIT-ONS               PIC S9(5)V99.
021670          10  MRESV-SM-CREDIT-OFF               PIC S9(5)V99.
021680          10  MRESV-SM-CREDIT-FULL              PIC S9(5)V99.
021690          10  MRESV-TX-WH-CREDIT                PIC S9(5)V99.
021700          10  MRESV-CROSS-POL-CR-INFO.                            D6H24390
021710              15  MRESV-CROSS-CREDIT-FULL       PIC S9(5)V99.
021720              15  MRESV-CROSS-CREDIT-ONS        PIC S9(5)V99.
021730              15  MRESV-CROSS-CREDIT-OFF        PIC S9(5)V99.
021740          10  MRESV-TEXAS-BASIC-PREM            PIC S9(7).
021750          10  MRESV-GROSS-LIMIT                 PIC S9(7).
021760          10  MRESV-TX-COMPUTER-LIMIT           PIC S9(7).
021770          10  MRESV-TX-HO390-CLASS-CODE         PIC S9(7).
021780          10  MRESV-COMMISSION-FOR-LB00G        PIC S9(5)V99.
HO7032          10  MRESV-TX-TOT-CRDT-AMT             PIC S9(7).
HO3104          10  MRESV-OTHER-PRIV-STRUCT-INFO.                       D6H24490
HO3104              15  MRESV-OTHER-STRUCT-PREM-FULL  PIC S9(5)V99.
HO3104              15  MRESV-OTHER-STRUCT-PREM-ONS   PIC S9(5)V99.
HO3104              15  MRESV-OTHER-STRUCT-PREM-OFF   PIC S9(5)V99.
HO3165          10  MRESV-CA-EQ-PREM-QUOTE            PIC S9(5)V99.
HO3165          10  MRESV-CA-EQ-DISC-QUOTE            PIC S9(5)V99.
HO3227          10  MRESV-UNADJ-BASE-PREM             PIC S9(5)V99.
HO3237          10  MEND-SECT-B-LIMIT                 PIC S9(7).
HO3237          10  MEND-TX-PHY-SURG-LIM              REDEFINES
HO3237               MEND-SECT-B-LIMIT                PIC S9(7).
HO3307          10  MEND-APPUR-STRUCT-LIM             PIC S9(7).
HO3307          10  MEND-APPUR-STRUCT-LIM-2           PIC S9(7).
HO3411          10  MRESV-HA6-HA9-TOT-COV-A           PIC S9(7).
GEICO           10  MRESV-GEICO-POL-PREM              PIC S9(5)V99.
021920          10  MRESV-PROT-DEV-FULL               PIC S9(5)V99.
021920          10  MRESV-PROT-DEV-ONS                PIC S9(5)V99.
021920          10  MRESV-PROT-DEV-OFF                PIC S9(5)V99.
661900          10  MRESV-PF-CREDIT-FULL              PIC S9(5)V99.
661900          10  MRESV-PF-CREDIT-ONS               PIC S9(5)V99.
661900          10  MRESV-PF-CREDIT-OFF               PIC S9(5)V99.
TEXINT          10  MRESV-ASSOC-CR-FULL               PIC S9(5)V99.
TEXINT          10  MRESV-ASSOC-CR-ONS                PIC S9(5)V99.
TEXINT          10  MRESV-ASSOC-CR-OFF                PIC S9(5)V99.
LOSSUR          10  MRESV-LOSS-SURCH-AREA-2222222.
LOSSUR              15  MRESV-LOSS-SURCHARGE-FULL     PIC S9(5)V99.
LOSSUR              15  MRESV-LOSS-SURCHARGE-ONS      PIC S9(5)V99.
LOSSUR              15  MRESV-LOSS-SURCHARGE-OFF      PIC S9(5)V99.
SRMSRM          10  MFILLER-30                        PIC S9(7).        25770006
SRMSRM          10  MFILLER-31                        PIC S9(7).        25780006
SRMSRM          10  MFILLER-32                        PIC S9(7).        25790006
SRMSRM          10  MFILLER-33                        PIC S9(7).        25800006
SRMSRM          10  MFILLER-34                        PIC S9(7).        25810006
SRMSRM          10  MFILLER-35                        PIC S9(7).        25820006


HO5093     05  MSUR-SURE-OVERRIDE-INFO-TBL.
HO5093       07  MSUR-SURE-OVERRIDE-INFO-AREA         OCCURS 15 TIMES
HO5093                                                INDEXED BY
HO5093                                                MSUR-OI-INDEX.
HO5093         10  MSUR-SURE-OVERRIDE-INDICATOR.                        D6H24830
HO5093             15  MSUR-SURE-OI-1ST-2-BYTES       PIC X(2).
SRMSRM             15  MFILLER-36                     PIC X.            25910006
HO5093         10  MSUR-DATE-THE-OI-CONDITION-HIT     PIC S9(5)  COMP-3.
HO5093         10  MSUR-SURE-OI-STATUS                PIC X.
HO5093             88  MSUR-SURE-OI-IS-OUTSTANDING        VALUE '1'.    D6H24880
HO5093             88  MSUR-SURE-OI-HAS-BEEN-APPLIED      VALUE '2'.    D6H24890
HO5093             88  MSUR-SURE-OI-IS-OBSOLETE           VALUE '3'.    D6H24900
HO5093         10  MSUR-OI-APPLICATION-DATE           PIC S9(5)  COMP-3.
HO5093         10  MSUR-OI-APPLICATION-TRX            PIC X(2).
HO5093         10  MSUR-OI-APPLIED-BY-UND             PIC X(3).
HO5093         10  MSUR-OI-CONDITION-SURFACED-TRX     PIC X(2).
HO5093         10  MSUR-OI-APP-GEMINI-TAO-ORIGIN      PIC X.
HO5093             88  MSUR-OI-INPUT-VIA-TAO              VALUE 'T'.    D6H24960
HO5093             88  MSUR-OI-INPUT-VIA-GEMININ          VALUE 'G'.    D6H24970
SRMSRM         10  MFILLER-37                         PIC X(2).         26040006


GM0072     05  MGEM-INFO.                                               D6H24990
GM0072         10  MGEM-AGENCY-SEQ-NO                 PIC 9(4).


HO3088     05  M-XL-ADDL-INFO.                                          D6H25010
HO3088         10  M-XL-HO4-AETNA-AUTO                PIC X.
HO3088         10  M-XL-NOTATION-71-IND               PIC X.


SC9201     05  MSERVICE-CENTER-CUSTOMER-NO            PIC X(12).
SC9201     05  MSERVICE-CENTER-CPS-REACT-IND          PIC X.
SC9999     05  MCBS-ATTACH-COVER-SHEET                PIC X.
NYRECV     05  M-NY-HO40-48-RERATED                   PIC X.
HO3322     05  MCALIF-EQ-ZONE-QUOTE                   PIC X.
HO3305     05  MILLINOIS-EQ-NOTICE-SW                 PIC X.


HO3321     05  MEND-XL-WITHDRAWAL-IND                 PIC X.
HO3321         88  MEND-XL-WITHDRAWN                      VALUE 'Y'.
HO3321         88  MEND-XL-NOT-WITHDRAWN                  VALUE 'N'.


SC9202     05  MOLD-OUTSIDE-REPORT-CLAIM-TBL.
SC9202       07  MOLD-OUTSIDE-REPORT-CLAIM-INFO       OCCURS 2 TIMES
SC9202                                                INDEXED BY
SC9202                                                MOLD-CL-INDEX.
SC9202         10  MOLD-CL-ORDER-TYPE                 PIC XX.
SC9202             88  MOLD-CLAIM                         VALUE 'CL'.
SC9202         10  MOLD-CL-DATE-ORDERED               PIC S9(5)  COMP-3.
SC9202         10  MOLD-CL-TIME-OF-DAY                PIC S9(4)  COMP-3.
SC9202         10  MOLD-CL-DATE-RECEIVED              PIC S9(5)  COMP-3.
SC9202         10  MOLD-CL-DISPOSITION                PIC X.
SC9202             88  MOLD-CL-CLEAN                      VALUE 'C'.
SC9202             88  MOLD-CL-DIRTY                      VALUE 'D'.
SC9202             88  MOLD-CL-NOHIT                      VALUE 'N'.
SC9202         10  MOLD-CL-LAST-NAME                  PIC X(20).
SC9202         10  MOLD-CL-FIRST-NAME                 PIC X(15).
SC9202         10  MOLD-CL-MIDDLE-INIT                PIC X.
SC9202         10  MOLD-CL-SUFFIX-NAME                PIC X(5).
SC9202         10  MOLD-CL-UNDERWRITER-INITS          PIC XXX.


SC9202     05  MOLD-OUTSIDE-REPORT-CRDIT-TBL.
SC9202       07  MOLD-OUTSIDE-REPORT-CRDIT-INFO       OCCURS 2 TIMES
SC9202                                                INDEXED BY
SC9202                                                MOLD-CR-INDEX.
SC9202         10  MOLD-CR-ORDER-TYPE                 PIC XX.
SC9202             88  MOLD-CREDIT                        VALUE 'CR'.
SC9202         10  MOLD-CR-ORDER-COMPANY              PIC X.
SC9202             88  MOLD-TRW                           VALUE '1'.
SC9202             88  MOLD-CBI                           VALUE '2'.
SC9202             88  MOLD-TRANS-UNION                   VALUE '3'.
SC9202         10  MOLD-CR-DATE-ORDERED               PIC S9(5)  COMP-3. 
SC9202         10  MOLD-CR-TIME-OF-DAY                PIC S9(4)  COMP-3.
SC9202         10  MOLD-CR-DATE-RECEIVED              PIC S9(5)  COMP-3.
SC9202         10  MOLD-CR-DISPOSITION                PIC X.
SC9202             88  MOLD-CR-CLEAN                      VALUE 'C'.
SC9202             88  MOLD-CR-DIRTY                      VALUE 'D'.
SC9202             88  MOLD-CR-NOHIT                      VALUE 'N'.
SC9202         10  MOLD-CR-LAST-NAME                  PIC X(20).
SC9202         10  MOLD-CR-FIRST-NAME                 PIC X(15).
SC9202         10  MOLD-CR-MIDDLE-INIT                PIC X.
SC9202         10  MOLD-CR-SUFFIX-NAME                PIC X(5).
SC9202         10  MOLD-CR-UNDERWRITER-INITS          PIC XXX.


HO3362     05  MSURE-F2-RETAINED-IND                  PIC X.
HO3362         88  MSURE-F2-RETAINED                      VALUE 'Y'.
HO3297     05  M-POL-JACKET-ED-DATE                   PIC X(4).
HO3411     05  MTEN-CONDO-NICHE-INDICATORS.                             D6H25520
HO3411         10  M-T-C-COVC-UPGRADED                PIC X.
HO3411         10  M-T-C-COVD-CONVERTED               PIC X.
BC0002     05  M-INTERFACE-NBHD-NBIH-IND              PIC X.
BC0002     05  M-INTERFACE-NBHD-TERMINAL              PIC X(5).
BC0002     05  M-INTERFACE-NBHD-SEQ-NUMBER            PIC S9(4).
BC0002     05  M-INTERFACE-NBIH-DEC-IND               PIC X.
IF4624     05  MIVANS-AGTS-CUST-ID                    PIC X(30).
HO3431     05  M-ASSOC-CR-PCT                         PIC SV99   COMP-3.

CTLCTL     05  M-PC-AEST-CONSTR-QUAL-CODE-IND         PIC X.
CTLCTL         88  M-PC-AEST-CONSTR-QUAL-VALID            VALUE 'Y'.
CTLCTL         88  M-PC-AEST-CONSTR-QUAL-INVALID          VALUE 'N'.

HO3471     05  M-TX-NHC-PCT                           PIC SV99   COMP-3.
HO3471     05  M-GO-THRU-NEW-SUPPORT-SW               PIC X.
HO3471     05  M-PROT-DEV-CR-LINE-OUT-SW              PIC X.
HO3471     05  M-NHC-LINE-OUT-SW                      PIC X.
HO3471     05  M-ROOF-CR-LINE-OUT-SW                  PIC X.
HO3471     05  M-ACCT-CR-LINE-OUT-SW                  PIC X.
HO3458     05  M-HO-82-SUPPORT-SW                     PIC X.
HO3482     05  MSTA-REIN-AMT-INS-ONS                  PIC 9(5).
HO3482     05  MSTA-REIN-AMT-INS-OFF                  PIC 9(5).
HO3434     05  M-ORD-LAW-LETTER-SW                    PIC X.
HO3494     05  M-IN-HOME-DESCRIPTION                  PIC X(40).
HO3494     05  MRESV-IN-HOME-CODE                     PIC S999   COMP-3.
HO3502     05  MRESV-TX-WH-CONVERTED-SW               PIC X.
IF4624     05  M-ACORD-CROSS-SELL-IND                 PIC X.
IF4624     05  M-PRIVACY-BROCHURE-IND                 PIC X.

IF4624     05  M-INTERFACE-REMARKS-1.                                   D6H25790
IF4624         10  M-INTERFACE-RMKS-1-1-60            PIC X(60).
IF4624         10  M-INTERFACE-RMKS-1-61-120          PIC X(60).

IF4624     05  M-INTERFACE-REMARKS-2.                                   D6H25820
IF4624         10  M-INTERFACE-RMKS-2-1-60            PIC X(60).
IF4624         10  M-INTERFACE-RMKS-2-61-120          PIC X(60).

IF4624     05  M-ACORD-COV-ENDT-CONV.
IF4624         10  M-ACD-BOX-NUMBER-TABLE             OCCURS 15 TIMES
IF4624                                                INDEXED BY
IF4624                                                M-ACD-ENDT-INDEX.
IF4624             15  M-ACD-COV-ACTION-CODE          PIC X.
IF4624             15  M-ACD-COV-CODE                 PIC X(6).
IF4624             15  M-ACD-BOX-NUMBER               PIC 9(4).

HO3600     05  MEND-EQ-DED-CODE                       PIC X.
GEICO      05  M-GEICO-WORK-PHONE-NO                  PIC X(14).
GEICO      05  M-GEICO-ALT-SOC-SEC-NO                 PIC X(11).
GEICO      05  M-GEICO-SEP-VI-POL-IND                 PIC X.
607810     05  M-TX-HO111-CONVERSION-SW               PIC X.
GEICO      05  M-GEICO-DUEL-POL-IND                   PIC X.
790230     05  M-UL-CLASS-CODE                        PIC X.
823370     05  M-MIN-PREM-SW                          PIC X. 
822360     05  M-KY-EQ-GRANDFATHER-BLOCK-SW           PIC X.
828390     05  M-INLAND-CAT-MAND-ENDT-IND             PIC X.
828390     05  M-INLAND-CAT-OPT-ENDT-IND              PIC X.
828390     05  M-INLAND-CAT-OPT-REP-MAND-SW           PIC X.
SRMSRM     05  MFILLER-38                             PIC X(9).         27340006



MEMBER*NAME  D6HR002
000100*01  SAF-HO-NON-HIST-MASTER-REC.                                  D6H00010
000200*    THIS 01 LEVEL IS THE NON-HISTORIC SECTION OF THE             D6H00020
000300*    HOMEOWNERS MASTER RECORD.                                    D6H00030
000400*        INDEX OF 05 LEVELS.                                      D6H00040
000500*    M-BEFORE-AETNA-CLAIMS-SEC           CARD 124150.             D6H00050 
000600*    M-DIARY-INFO-SEC                    CARD 124270.             D6H00060
000700*    M-REPORT-SEC                        CARD 124670.             D6H00070
000800*    M-UND-CTL-RB-NOTE-ACT-SEC           CARD 125020.             D6H00080
000900*    M-CLAIM-DATA-SEC                    CARD 125750.             D6H00090
001000*    M-NON-HISTORIC-BILL-INFO-SEC        CARD 126690.             D6H00100
001100*    M-SPECIAL-CLAIM-INFO                CARD 126980              D6H00110
001200*  02  M-NON-HIST-MR-DATA-FIELDS.                                 D6H00120

954240     05  M-BEFORE-TRAVELERS-CLAIMS-TBL1.
001300       07  M-BEFORE-AETNA-CLAIMS-SEC            OCCURS 4 TIMES
001400                                                INDEXED BY
001400                                                MBEF-INDEX.
001500         10  MBEF-ACTION-TYPE                   PIC X.
001600             88  MBEF-INITIAL-VALUE                 VALUE ' '.
001700             88  MBEF-ADD-ONLY                      VALUE 'A'.
001800             88  MBEF-CHANGE-ONLY                   VALUE 'C'.
001900             88  MBEF-ELIMINATE-ONLY                VALUE 'E'.
002000         10  MBEF-LOSS-DATE                     PIC S9(5)  COMP-3.
002100         10  MBEF-LOSS-AMOUNT                   PIC S9(5)  COMP-3.
002200         10  MBEF-LOSS-CAUSE                    PIC X(10).
JANJAN         10  MBEF-LOSS-CAUSE-RDEF REDEFINES  MBEF-LOSS-CAUSE.
                 15 MBEF-CATASTROPHE-NUMBER           PIC X(2).
                 15 MBEF-LOSS-CAUSE-NEW               PIC X(8).

002300     05  M-DIARY-INFO-SEC.                                        D6H00230
002400         10  MDIA-REGULAR-DIARIES-TBL.
002400           12  MDIA-REGULAR-DIARIES             OCCURS 5 TIMES
002500                                                INDEXED BY
002500                                                MDIA-R-INDEX.
002600             15  MDIA-DIARY-ENTRY-DATE-R        PIC S9(5)  COMP-3.
002700             15  MDIA-DIARY-EFFECTIVE-DATE-R    PIC S9(5)  COMP-3.
002800             15  MDIA-DIARY-FOLLOWUP-COUNT-R    PIC S9     COMP-3.
002900             15  MDIA-DIARY-REASON-REGULAR      PIC S99    COMP-3.
003000                 88  MDIA-CRIT-ERR-ON-INPUT         VALUE +01.
003100                 88  MDIA-DECLINED-NB               VALUE +03.
003200                 88  MDIA-DELETE-POLICY             VALUE +04.
003300                 88  MDIA-NB-HOLD                   VALUE +06.
003400                 88  MDIA-REINSURANCE               VALUE +07.
003500                 88  MDIA-REVISED-CANC              VALUE +09.
003600                 88  MDIA-OUTSIDE-REPORT            VALUE +10.
                       88  MDIA-QUESTIONNAIRE             VALUE +65.
003700                 88  MDIA-PRE-RENEWAL               VALUE +70.
003800                 88  MDIA-RENEWAL                   VALUE +79.
003900                 88  MDIA-EXPIRATION                VALUE +83.
004000                 88  MDIA-DIARIED-NON-RENEWAL       VALUE +84.
                       88  MDIA-TRX-PURGE                 VALUE +86.
004100                 88  MDIA-CANC-ELIMINATE            VALUE +88.
004200                 88  MDIA-INCOMPLETE-CLAIM-INFO     VALUE +90.
004300                 88  MDIA-FINAL-PURGE               VALUE +98.

004400         10  MDIA-IRREGULAR-DIARIES-TBL.
004400           12  MDIA-IRREGULAR-DIARIES           OCCURS 2 TIMES
004500                                                INDEXED BY
004500                                                MDIA-I-INDEX.
004600             15  MDIA-ACTION-TYPE               PIC X.
004700                 88  MDIA-INITIAL-VALUE             VALUE ' '.
004800                 88  MDIA-ADD-ONLY                  VALUE 'A'.
004900                 88  MDIA-CHANGE-ONLY               VALUE 'C'.
005000                 88  MDIA-ELIMINATE-ONLY            VALUE 'E'.
005100             15  MDIA-DIARY-ENTRY-DATE-I        PIC S9(5)  COMP-3.
005200             15  MDIA-DIARY-EFFECTIVE-DATE-I    PIC S9(5)  COMP-3.
005300             15  MDIA-DIARY-FOLLOWUP-COUNT-I    PIC S9     COMP-3.
005400             15  MDIA-DIARY-REASON-IRREGULAR    PIC X(30).
005500             15  MDIA-DIARY-UND-INITIALS        PIC XXX.


005600     05  M-REPORT-SEC.                                            D6H00580
005700         10  MREP-ACTION-TYPE                   PIC X.
005800             88  MREP-INITIAL-VALUE                 VALUE ' '.
005900             88  MREP-ADD-ONLY                      VALUE 'A'.
006000             88  MREP-CHANGE-ONLY                   VALUE 'C'.
006100             88  MREP-ELIMINATE-ONLY                VALUE 'E'.

006200         10  MREP-OUTSIDE-REPORT.                                 D6H00640
006300             15  MREP-ORIGIN-DATE               PIC S9(5)  COMP-3.
006400             15  MREP-REPORT-COMPANY            PIC XX.
006500                 88  MREP-RETAIL-CREDIT             VALUE '10'.
006600                 88  MREP-HOOPER-HOLMES             VALUE '11'.
006700                 88  MREP-O-HANLON-REPORTS          VALUE '12'.
006800                 88  MREP-STANDARD-RPT              VALUE '13'.
006900                 88  MREP-UNDERWRITER-RPT           VALUE '14'.
007000                 88  MREP-UNITED-INVESTIGATION      VALUE '15'.
007100                 88  MREP-OTHER-1                   VALUE '16'.
007200                 88  MREP-INS-CO-INSP-BUR           VALUE '17'.
007300                 88  MREP-GEN-ADJUST-BUR            VALUE '18'.
007400                 88  MREP-UPFRO                     VALUE '19'.
007500                 88  MREP-S-R-FRANZ                 VALUE '20'.
007600                 88  MREP-IISCO                     VALUE '21'.
007700                 88  MREP-JENSEN-MORE               VALUE '22'.
007800                 88  MREP-PHOTOGRAPHIC-REPORTING    VALUE '23'.
007900                 88  MREP-EMPLYMNT-INVESTIGATORS    VALUE '24'.
008000                 88  MREP-IRA                       VALUE '25'.
                       88  MREP-UNDERWRITERS-SURVEY       VALUE '26'.
                       88  MREP-R-R-INSPECTION            VALUE '27'.
008100             15  MREP-UNDERWRITER-INITIALS      PIC XXX.
008200             15  MREP-SPECIAL-ATTENTION-AREA    PIC X(30).
008300             15  MREP-REPORT-RECEIVED-DATE      PIC S9(5)  COMP-3.
008400             15  MREP-PHOTO-CODE                PIC X.
008500                 88  MREP-NO-PHOTO                  VALUE ' '.
008600                 88  MREP-1-PHOTO                   VALUE '1'.
008700                 88  MREP-2-PHOTOS                  VALUE '2'.
008800                 88  MREP-3-PHOTOS                  VALUE '3'.
008900                 88  MREP-OTHER-PKG                 VALUE '4'.
009000             15  MREP-NEXT-ORDER-TIME           PIC X.
009100                 88  MREP-NONE                      VALUE ' '.
009200                 88  MREP-1-YEAR                    VALUE '1'.
009300                 88  MREP-2-YEARS                   VALUE '2'.
009400                 88  MREP-3-YEARS                   VALUE '3'.


009500     05  M-UND-CTL-RB-NOTE-ACT-SEC.                               D6H00990
009600         10  MUND-UND-CTL-RB.                                     D6H01000
009700             15  MUND-RB-ACTION-TYPE             PIC X.
009800                 88  MUND-RB-INITIAL-VALUE           VALUE ' '.
009900                 88  MUND-RB-ADD-ONLY                VALUE 'A'.
010000                 88  MUND-RB-CHANGE-ONLY             VALUE 'C'.
010100                 88  MUND-RB-ELIMINATE-ONLY          VALUE 'E'.
010200             15  MUND-UND-CTL-RB-TYPE            PIC S99   COMP-3.
010300                 88  MUND-RB-ACCEPT-NO-CHANGES       VALUE +32.
010400                 88  MUND-RB-DO-NOT-INCR-SECT-2      VALUE +33.
010500                 88  MUND-RB-DO-NOT-CHANGE-SECT-1    VALUE +34.
010600                 88  MUND-RB-DO-NOT-ADD-A-COV-ENDT   VALUE +35.
010700                 88  MUND-RB-DONT-CHANGE-A-COV-ENDT  VALUE +36.
010800                 88  MUND-RB-DO-NOT-ELIM-A-COV-ENDT  VALUE +37.
HO3454                 88  MUND-RB-DO-NOT-DO-COASTAL       VALUE +95.
DEBDEB                 88  MUND-RB-GEICO-COASTAL-ZIP       VALUE +97.
010900             15  MUND-UND-CTL-ESTAB-DATE         PIC S9(5) COMP-3.
011000             15  MUND-UND-CTL-UND-INITIALS       PIC XXX.

011100         10  MUND-UND-ACTIONS-TBL.
011100           12  MUND-UND-ACTIONS                 OCCURS 8 TIMES
011200                                                INDEXED BY
011200                                                MUND-ACT-INDEX.
011300             15  MUND-ACT-ACTION-TYPE           PIC X.
011400                 88  MUND-ACT-INITIAL-VALUE         VALUE ' '.
011500                 88  MUND-ACT-ADD-ONLY              VALUE 'A'.
011600                 88  MUND-ACT-CHANGE-ONLY           VALUE 'C'.
011700                 88  MUND-ACT-ELIMINATE-ONLY        VALUE 'E'.
011800             15  MUND-UND-ACTION-NUMBER         PIC S9     COMP-3.

011900             15  MUND-UND-ACTION-TAKEN-TBL.
011900               17  MUND-UND-ACTION-TAKEN        OCCURS 3 TIMES
012000                                                INDEXED BY
012000                                                MUND-TAK-INDEX
012000                                                PIC S99    COMP-3.
012100                 88  MUND-NO-ACTION                 VALUE +11.
012200                 88  MUND-CANCEL                    VALUE +13.
012300                 88  MUND-NOT-TO-BE-RENEWED         VALUE +14.
012400                 88  MUND-CONTD-FOR-AGCY-BUSINESS   VALUE +15.
012500                 88  MUND-REINSPECTION              VALUE +16.
012600                 88  MUND-DISCUSSED-WITH-AGENT      VALUE +17.
012700                 88  MUND-REDUCE-EXPOSURE           VALUE +18.
012800                 88  MUND-OTHER-ACT                 VALUE +19.
012900             15  MUND-UND-ACT-UND-INITIALS      PIC XXX.
013000             15  MUND-DATE-UND-ACT-TAKEN        PIC S9(5)  COMP-3.

013100         10  MUND-UND-ACTION-OVERFLOW-IND       PIC X.
013200             88  MUND-YES-ACTION-OVERFLOW           VALUE '1'.
013300             88  MUND-NO-ACTION-OVERFLOW            VALUE '0'.

013400         10  MUND-UND-NOTATIONS-TBL.
013400           12  MUND-UND-NOTATIONS               OCCURS 8 TIMES
013500                                                INDEXED BY
013500                                                MUND-NOTE-INDEX.
013600             15  MUND-NOTE-ACTION-TYPE          PIC X.
013700                 88  MUND-NOTE-INITIAL-VALUE        VALUE ' '.
013800                 88  MUND-NOTE-ADD-ONLY             VALUE 'A'.
013900                 88  MUND-NOTE-CHANGE-ONLY          VALUE 'C'.
014000                 88  MUND-NOTE-ELIMINATE-ONLY       VALUE 'E'.
014100             15  MUND-UND-NOTATIONS-NUMBER      PIC S9     COMP-3.
014200             15  MUND-UND-NOTATION-TYPE         PIC XX.
014300                 88  MUND-SUBSTANDARD               VALUE '11'.
014400                 88  MUND-GRADATION                 VALUE '12'.
014500                 88  MUND-ANTEDATED-OR-REFORMED     VALUE '13'.
014600                 88  MUND-CONFIDENTIAL-RISK-REPORT  VALUE '14'.
014700                 88  MUND-LARGE-LOSS                VALUE '15'.
014800                 88  MUND-MISC                      VALUE '16'.
014900                 88  MUND-SUPPORTING-BUSINESS       VALUE '17'.
015000                 88  MUND-NOTE-OTHER-B              VALUE '18'.
015100                 88  MUND-NOTE-OTHER-C              VALUE '19'.
015200                 88  MUND-NOTE-OTHER-D              VALUE '20'.
015300                 88  MUND-NOTE-NON-REN              VALUE '21'.
015400                 88  MUND-NOTE-MISC-NO-APOF         VALUE '22'.
015500                 88  MUND-NOTE-RISK-SUMMARY-REPORT  VALUE '43'.
015600                 88  MUND-NOTE-AETNA-REALTY         VALUE '97'.
015700                 88  MUND-NOTE-AETNA-BUS-CREDIT     VALUE '98'.
015800                 88  MUND-NOTE-URBAN-INVESTMENTS    VALUE '99'.
015900             15  MUND-NOTE-CAUSE-DATE           PIC S9(5)  COMP-3.
016000             15  MUND-UND-NOTE-DESCRIPTION      PIC X(40).
016100             15  MUND-UND-NOTE-UND-INITIALS     PIC XXX.
016200             15  MUND-UND-NOTE-DATE-MADE        PIC S9(5)  COMP-3.

016300         10  MUND-UND-NOTE-OVERFLOW-IND         PIC X.
016400             88  MUND-YES-NOTE-OVERFLOW             VALUE '1'.
016500             88  MUND-NO-NOTE-OVERFLOW              VALUE '0'.


016600     05  M-CLAIM-DATA-SEC.                                        D6H01700
016700       07  MCLA-CLAIM-DATA                 OCCURS 10 TIMES
016700                                           INDEXED BY
016700                                           MCLA-INDEX MCLA-X2.
016800         10  MCLA-ACTION-TYPE                   PIC X.
016900             88  MCLA-INITIAL-VALUE                 VALUE ' '.
017000             88  MCLA-ADD-ONLY                      VALUE 'A'.
017100             88  MCLA-CHANGE-ONLY                   VALUE 'C'.
017200             88  MCLA-ELIMINATE-ONLY                VALUE 'E'.
017300         10  MCLA-LOSS-NUMBER                   PIC S99    COMP-3.
017400         10  MCLA-LOSS-ENTRY-DATE               PIC S9(5)  COMP-3.
017500         10  MCLA-COV-ANTEDATED-REFORM-IND      PIC X.
017600             88  MCLA-YES-ANTE-REF                  VALUE '1'.
017700             88  MCLA-NO-ANTE-REF                   VALUE '0'.
017800         10  MCLA-DATE-OF-LOSS                  PIC S9(5)  COMP-3.
017900         10  MCLA-DESCRIPTION-OF-LOSS           PIC X(60).
018000         10  MCLA-CATASTROPHE                   PIC S999   COMP-3.
018100         10  MCLA-FAST-CLM-IND                  PIC X.
018200             88  MCLA-FAST                          VALUE 'Y'.
018300             88  MCLA-NON-FAST                      VALUE 'N'.
018400         10  MCLA-SAFA-CLM-IND                  PIC X.
018500             88  MCLA-PRE-SAFA-CLM                  VALUE '1'.
018600             88  MCLA-POST-SAFA-CLM                 VALUE '2'.
018700             88  MCLA-PRE-CDF-CLM                   VALUE '3'.
018800             88  MCLA-POST-CDF-CLM                  VALUE '4'.
018800             88  MCLA-CPCS-CLM                      VALUE '5'.

018900         10  MCLA-CLAIM-INFO.                                     D6H01930
019000             15  MCLA-CLAIM-NUMBER.                               D6H01940
019100                 20  MCLA-CLAIM-UNIT            PIC X.
019200                 20  MCLA-CLAIM-KEY             PIC S999   COMP-3.
019300                 20  MCLA-CLAIM-SERIAL-NUM      PIC S9(7)  COMP-3.
019400                 20  MCLA-CLAIM-NUM-SUFFIX      PIC XX.
019500             15  MCLA-DATE-CDF-CHANGE           PIC S9(5)  COMP-3.
019600             15  MCLA-TYPE-OF-LOSS-CODE         PIC X(4).
019700                 88  MCLA-FIRE-LIGHTNING-REMOVAL       VALUE '01'.
019800                 88  MCLA-WIND-AND-HAIL                VALUE '02'.
019900                 88  MCLA-WATER-DAMAGE-AND-FREEZING    VALUE '03'.
020000                 88  MCLA-THEFT-NOT-MYST-DISAPP        VALUE '04'.
020100                 88  MCLA-ALL-PHYS-DAM-INCL-VMM        VALUE '05'.
020200                 88  MCLA-LIAB-EXCEPT-SNOWMOBILES      VALUE '06'.
020300                 88  MCLA-CREDIT-CARD                  VALUE '07'.
020400                 88  MCLA-SNOWMOBILE-LIAB              VALUE '08'.
020500                 88  MCLA-MYSTERIOUS-DISAPPEARANCE     VALUE '09'.

020600             15  MCLA-COMPANION-FILE-TBL.
020600               17  MCLA-COMPANION-FILE-INFO     OCCURS 2 TIMES
020700                                                INDEXED BY
020700                                                MCLA-COMP-INDEX.
020800                 20  MCLA-COMP-UNIT             PIC X.
020900                 20  MCLA-COMP-OFF-CODE         PIC S999   COMP-3.
021000                 20  MCLA-COMP-CLASS-LETTERS    PIC X(4).
021100                 20  MCLA-COMP-SERIAL-NO        PIC S9(7)  COMP-3.
021200                 20  MCLA-COMP-SUFFIX           PIC XX.

021300             15  MCLA-CLAIM-INDICATORS.                           D6H02170
021400                 20  MCLA-MULT-CLMT-IND         PIC X.
021500                     88  MCLA-MULT-CLMT             VALUE 'Y'.
021600                     88  MCLA-NO-MULT-CLMT          VALUE 'N'.
021700                 20  MCLA-CLAIM-IND-B           PIC X.
021800                 20  MCLA-CLAIM-IND-C           PIC X.
021900                 20  MCLA-CLAIM-IND-D           PIC X.

022000             15  MCLA-MONEY-LEVEL-TBL.
022000               17  MCLA-MONEY-LEVEL             OCCURS 5 TIMES
022100                                                INDEXED BY
022100                                                MCLA-MON-INDEX.
022200                 20  MCLA-CLM-CLASS-LETTERS     PIC X(4). 
022300                 20  MCLA-SECONDARY-COV-ID      PIC X.
022400                     88  MCLA-NOT-APPLIC            VALUE '0'.
022500                     88  MCLA-LIABILITY             VALUE '1'.
022600                     88  MCLA-MED-PAY               VALUE '2'.
022700                     88  MCLA-LIAB-MED-PAY          VALUE '3'.
022800                 20  MCLA-CLAIM-AMT-INCURRED    PIC S9(9)  COMP-3.
022900                 20  MCLA-STATUS-OF-CLAIM       PIC X.
023000                     88  MCLA-CLAIM-OPEN            VALUE '1'.
023100                     88  MCLA-CLAIM-SPECIAL         VALUE '0'.
023200                     88  MCLA-CLAIM-CLOSED          VALUE '2'.
023300                 20  MCLA-SUBROGATION-IND       PIC X.
023400                     88  MCLA-YES-SUBROGATION       VALUE '1'.
023500                     88  MCLA-NO-SUBROGATION        VALUE '0'.
023600                 20  MCLA-LOSS-RPT-RECD-DATE    PIC S9(5)  COMP-3.
023700       07  MCLA-HIGHEST-LOSS-NUMBER             PIC S99    COMP-3.


023800     05  M-NON-HISTORIC-BILL-INFO-TBL.
023800       07  M-NON-HISTORIC-BILL-INFO-SEC         OCCURS 20 TIMES
023900                                                INDEXED BY
023900                                                MNON-INDEX-1.
024100        08  MNON-HIST-BILL-DATA.
024200         10  MNON-BILLING-TYPE                  PIC X.
024300             88  MNON-CUSTOMER                      VALUE '1'.
024400             88  MNON-AGENCY                        VALUE '2'.
024500             88  MNON-BUDGET-RITE                   VALUE '3'.
024600             88  MNON-EPIC                          VALUE '4'.
024700             88  MNON-CBS-3                         VALUE '9'.
024800         10  MNON-POLICY-TERM                   PIC X.
024900             88  MNON-THREE-YEAR                    VALUE '1'.
025000             88  MNON-ONE-YEAR                      VALUE '2'.
025100             88  MNON-SIX-MONTH                     VALUE '3'.
025200             88  MNON-THREE-MONTH                   VALUE '4'.
025300         10  MNON-SEND-BILL-TO                  PIC X.
025400             88  MNON-INSURED                       VALUE '1'.
025500             88  MNON-FIRST-MORTGAGEE               VALUE '2'.
025600             88  MNON-ALTERNATE-PAYOR               VALUE '3'.
025700         10  MNON-ACCOUNT-NUMBER                PIC X(11).
025800         10  MNON-TRX-TYPE                      PIC XX.

025900        09  MNON-NUMERIC-DETAIL-INFO                       COMP-3.
026000         10  MNON-INIT-PAY-CUST-BR-EPIC         PIC S9(5)V99.
026100         10  MNON-AMOUNT-BILLED                 PIC S9(5)V99.
026200         10  MNON-DEC-NO                        PIC S9(3).
026300         10  MNON-COMM-RATE                     PIC SV999.
026400         10  MNON-TRX-EFF-DATE                  PIC S9(5).
026500         10  MNON-BILLING-DATE                  PIC S9(5).
026600         10  MNON-TRX-DESCRIPTION-CODE          PIC S999.


026700     05  M-SPECIAL-CLAIM-TBL.
026700       07  M-SPECIAL-CLAIM-INFO                 OCCURS 10 TIMES
026800                                                INDEXED BY
026800                                                MSPE-INDEX.
026900         10  MSPE-DATES                                    COMP-3.
027000             15  MSPE-PRIOR-EFF-DATE            PIC S9(5).
027100             15  MSPE-PRIOR-EXP-DATE            PIC S9(5).
027200         10  MSPE-STAT-CODES.                                     D6H02760
027300             15  MSPE-TERRITORY-ID              PIC S99   COMP-3.
027400             15  MSPE-STATE                     PIC S99   COMP-3.
027500             15  MSPE-RATE                      PIC S99   COMP-3.
027600             15  MSPE-LINE-OF-BUS               PIC S999  COMP-3.
027700             15  MSPE-FORM                      PIC X.
027800             15  MSPE-NO-FAMILIES               PIC X.
027900             15  MSPE-COVERAGE                  PIC X.
028000             15  MSPE-CONSTRUCTION              PIC X.
028100             15  MSPE-PROT-CLASS                PIC X.
028200             15  MSPE-PERCENT-MANUAL            PIC X.
028300             15  MSPE-DED-TYPE                  PIC X.
028400             15  MSPE-DED-AMOUNT                PIC X.
028500             15  MSPE-COV-A-C-E-F-LIMIT         PIC XX.
028600             15  MSPE-CREDIT-WATER-SNOW-AGE     PIC XX.
028700             15  MSPE-INFLATION-GUARD           PIC X.
028800             15  MSPE-FILLER                    PIC X(10).
028900         10  MSPE-REINSURANCE-CO                PIC S999   COMP-3.


004430     05  M-ENDT-COVG-INFORMATION-SEC.                             D6H04720
004440         10  MEND-COV-ENDT-INFO-AREA            OCCURS 22 TIMES
004450                                                INDEXED BY
004450                                                MEND-COV-INDEX.
004460             15  MEND-ACTION-TYPE               PIC X.
004470                 88  MEND-INITIAL-VALUE             VALUE ' '.
004480                 88  MEND-ADD-ONLY                  VALUE 'A'.
004490                 88  MEND-CHANGE-ONLY               VALUE 'C'.
004500                 88  MEND-ELIMINATE-ONLY            VALUE 'E'.

004510             15  MEND-COV-ENDT-FORM-NO.                           D6H04800
HO6268                 20  MEND-COV-ENDT-FORM-NO-1ST-6    PIC X(6).     D6H04810
HO6268                 20  MEND-COV-ENDT-FORM-NO-SUFFIX   PIC X(3).     D6H04820
HO3227                 20  MEND-COV-ENDT-CREDIT-IND       REDEFINES
HO3227                      MEND-COV-ENDT-FORM-NO-SUFFIX.
HO3227                     25  MEND-CREDIT-IND            PIC XX.
HO3227                     25  MEND-CREDIT-SUFFIX         PIC X.
CWMOLD             15  MEND-COV-ENDT-FORM-NBR REDEFINES
CWMOLD                                           MEND-COV-ENDT-FORM-NO.
CWMOLD                 20  MEND-COV-ENDT-FORM-NO-1ST-8    PIC X(8).
CWMOLD                 20  MEND-COV-ENDT-FORM-NO-LAST     PIC X.

004520             15  MEND-COV-ENDT-INFO.                              D6H04870
004530                 20  MEND-COV-ENDT-ED-DATE          PIC X(4).
004540                 20  MEND-COV-ENDT-INF-LOCATION-IND PIC XX.
004560                     88  MEND-SECT-B-ADD-INFO          VALUE '01'.
004570                     88  MEND-SECT-C-ADD-INFO          VALUE '02'.
004580                     88  MEND-SECT-D-ADD-INFO          VALUE '03'.
004590                     88  MEND-PROT-DEVICES             VALUE '04'.
004600                     88  MEND-OFF-OCC-ADD-INFO         VALUE '10'.
004610                     88  MEND-APP-STRUCT-ADD-INFO      VALUE '15'.
004620                     88  MEND-ADDL-INT-ADD-INFO        VALUE '20'.
004630                     88  MEND-SNOWMOBILE-ADD-INFO      VALUE '25'.
004640                     88  MEND-BUSINESS-PUR-ADD-INFO    VALUE '30'.
004650                     88  MEND-WATERCRAFT-ADD-INFO      VALUE '35'.
004660                     88  MEND-WORKERS-COMP             VALUE '40'.
004670                     88  MEND-PER-EX-INDEM-ADD-INFO    VALUE '45'.
004680                     88  MEND-EXT-THEFT-ADD-INFO       VALUE '50'.
004690                     88  MEND-XL-ADDL-INFO-FLA         VALUE '52'.
004700                     88  MEND-CREDIT-CARD-ADD-INFO     VALUE '55'.
004710                     88  MEND-ADDL-RES-ADD-INFO        VALUE '65'.
004720                     88  MEND-MISC-ENDT-ADD-INFO       VALUE '70'.
004730                     88  MEND-MISC-REAL-PROP-INFO      VALUE '72'.
004740                     88  MEND-RATED-INFL-GUARD-INFO    VALUE '75'.
004750                     88  MEND-SCHED-PERS-PROP-INFO     VALUE '80'.
004760                     88  MEND-LOSS-ASSESSMENT          VALUE '85'.
004770                     88  MEND-COVG-D-DIFF              VALUE '86'.
004780                     88  MEND-OTHER-RES-LIMIT-INFO     VALUE '90'.
004790                     88  MEND-INVALID-ENDT             VALUE '98'.
004800                     88  MEND-NO-ADD-INFO-FOR-COV-ENDT VALUE '99'.
004810                     88  MEND-TX-ADDL-INSURED          VALUE 'A '.
004820                     88  MEND-TX-OFF-PROF-OCC          VALUE 'B '.
004830                     88  MEND-TX-TV-RADIO-ANTENNA      VALUE 'C '.
004840                     88  MEND-TX-PHY-SURG-DENTIST      VALUE 'D '.
004850                     88  MEND-TX-MONIES-SECURITIES     VALUE 'E '.
004860                     88  MEND-TX-ADDL-AMT-PRIV-STR     VALUE 'F '.
004870                     88  MEND-TX-FCPL                  VALUE 'G '.
004880                     88  MEND-TX-WATERCRAFT            VALUE 'H '.
004890                     88  MEND-TX-BUS-PURSUITS          VALUE 'I '.
004900                     88  MEND-TX-ADDL-PREMISES         VALUE 'J '.
004910                     88  MEND-TX-ADDL-AMT-SEC-RES      VALUE 'K '.
004920                     88  MEND-TX-ADDL-LIM-JEWELRY-FURS VALUE 'L '.
004930                     88  MEND-TX-SCHED-PERS-PROP       VALUE 'M '.
004940                     88  MEND-TX-PERS-INJURY           VALUE 'N '.
004950                     88  MEND-TX-OUTBUILDINGS          VALUE 'O '.
004960                     88  MEND-TX-LOSS-ASSESMENT        VALUE 'P '.
004970                     88  MEND-TX-INF-PERCENT-INFO      VALUE 'Q '.
                           88  MEND-TX-ADDL-LIM-BUS-PROPERTY VALUE 'R '.
957580                     88  MEND-TX-BUILDING-ORD-LAW      VALUE 'S '.
TXMOLD                     88  MEND-TX-MOLD                  VALUE 'T '.
004980                     88  MEND-TX-HO390-INFO            VALUE 'Z '.
004990*        THIS NEXT 88 LEVEL INCLUDES ALL TEXAS ENDTS AND IS USED
005000*        IN NB,PC,AND REN FINAL EDITS TO PREVENT SYSTEMS ERROR.
005040                     88  MEND-TEXAS-ENDTS              VALUE 'A '
005040                                                             'B '
005040                                                             'C '
005040                                                             'D '
005040                                                             'E '
005040                                                             'F '
005040                                                             'G '
005040                                                             'H '
005040                                                             'I '
005040                                                             'J '
005040                                                             'K '
005040                                                             'L '
005040                                                             'M '
005040                                                             'N '
005040                                                             'O '
005040                                                             'P '
005040                                                             'Q '
                                                                   'R '
957580                                                             'S '
TXMOLD                                                             'T '
005040                                                             'Z '.
005090                 20  MEND-DEDUCT-ENDT-IND           PIC X.
005100                     88  MEND-DED-ENDT-1                VALUE '1'.
005110                     88  MEND-DED-ENDT-2                VALUE '2'.
005120                     88  MEND-NO-DEDUCT-ENDT            VALUE '0'.
005130                 20  MEND-TYPING-IND                PIC X.
005140                     88  MEND-TYPING-REQUIRED           VALUE '0'.
005150                     88  MEND-NO-TYPING-REQUIRED        VALUE '1'.
005160                 20  MEND-ATTACH-TO-DECL-IND        PIC X.
005170                     88  MEND-ATTACH-TO-DECL            VALUE '1'.
005180                     88  MEND-ALREADY-ATTACHED-TO-DECL  VALUE '0'.
005190                 20  MEND-COV-IND                   PIC X.
005200                     88  MEND-SEC-1-ONLY                VALUE '1'.
005210                     88  MEND-SEC-2-ONLY                VALUE '2'.
005220                     88  MEND-SEC-1-AND-SEC-2           VALUE '3'.
005230                     88  MEND-DOES-NOT-APPLY-TO-POL-COV VALUE '0'.
005240                 20  MEND-REST-COV-ENDT-IND         PIC X.
005250                     88  MEND-RESTRICTIVE               VALUE '1'.
005260                     88  MEND-NON-RESTRICTIVE           VALUE '0'.
005270                 20  MEND-SIGN-COV-ENDT-IND         PIC X.
005280                     88  MEND-SIGNATURE-OBTAINED        VALUE '1'.
005290                     88  MEND-SIGNATURE-NOT-OBTAINED    VALUE '2'.
005300                     88  MEND-SIGN-NOT-NECESSARY        VALUE '0'.
005310                 20  MEND-COV-END-FILE-IND          PIC X.
005320                     88  MEND-FILING-MADE               VALUE '1'.
005330                     88  MEND-FILING-NOT-MADE           VALUE '2'.
005340                     88  MEND-NO-FILING-NECESSARY       VALUE '0'.
005350                 20  MEND-CANC-CONSIDER-IND         PIC X.
005360                     88  MEND-MUST-CONSIDER             VALUE '1'.
005370                     88  MEND-NOT-NECESSARY-TO-CONSIDER VALUE '0'.
005380                 20  MEND-COV-ENDT-PREMIUM   PIC S9(5)V99  COMP-3.
005400                     88  MEND-NO-PREMIUM           VALUE +0000.00.
005410                 20  MEND-ENDT-ADDL-RETN-PREM-OFF                 D6H05760
005420                                             PIC S9(5)V99  COMP-3.
005440                 20  MEND-ENDT-ADDL-RETN-PREM-ONS                 D6H05790
005450                                             PIC S9(5)V99  COMP-3.
005470             15  MEND-COV-ENDT-EFF-DATE      PIC S9(5)     COMP-3.

 
006440     05  MEND-OTHER-PARTY-DATA-2.
006450         10  MEND-NAME-PERS-OR-ORGN-2           PIC X(30).
006460         10  MEND-O-P-2-ADDRESS-1               PIC X(30).
006470         10  MEND-O-P-2-ADDRESS-2               PIC X(20).
962190         10  MEND-FILLER-10 REDEFINES
962190                            MEND-O-P-2-ADDRESS-2.
962190             15  MEND-O-P-2-ADDR2-CITY        PIC X(17).
962190             15  MEND-FILLER-11               PIC X(01).
962190             15  MEND-O-P-2-ADDR2-STATE       PIC X(02).
006480         10  MEND-O-P-2-ZIP-CODE                PIC S9(5)  COMP-3.
006500         10  MEND-INT-IN-PREMISES-2             PIC X(20).


010280     05  MTHR-THIRD-MORTGAGEE-INFO-SEC.
010290         10  MTHR-ACTION-TYPE                   PIC X.
010300             88  MTHR-INITIAL-VALUE                 VALUE ' '.
010310             88  MTHR-ADD-ONLY                      VALUE 'A'.
010320             88  MTHR-CHANGE-ONLY                   VALUE 'C'.
010330             88  MTHR-ELIMINATE-ONLY                VALUE 'E'.
010340         10  MTHR-THIRD-MORT-NAME-1             PIC X(30).
010350         10  MTHR-THIRD-MORT-NAME-2             PIC X(30).
010360         10  MTHR-THIRD-MORTGAGEE-ADDRESS.                        D6H10740 
010370             15  MTHR-THIRD-MORT-ADDR-1         PIC X(20).
010380             15  MTHR-THIRD-MORT-ADDR-2         PIC X(20).
962190             15  MTHR-FILLER-1 REDEFINES
962190                              MTHR-THIRD-MORT-ADDR-2.
962190                 20  MTHR-THIRD-MORT-ADDR2-CITY  PIC X(17).
962190                 20  MTHR-FILLER-2               PIC X(01).
962190                 20  MTHR-THIRD-MORT-ADDR2-STATE PIC X(02).
010390             15  MTHR-THIRD-MORT-ZIP-CODE       PIC S9(5)  COMP-3.
010410         10  MTHR-THIRD-MORT-LOAN-NO            PIC X(10).


HO3603     05  AORS-OUTSIDE-RPT-CLAIM-TBL.
HO3603       07  AORS-OUTSIDE-REPORT-CLAIM-INFO       OCCURS 6 TIMES
HO3603                                                INDEXED BY
HO3603                                                AMCL-INDEX.
HO3603         10  AORS-CL-ORDER-TYPE                 PIC XX.
HO3603             88  AORS-CLAIM                         VALUE 'CL'.
HO3603         10  AORS-CL-DATE-ORDERED               PIC S9(5)  COMP-3.
HO3603         10  AORS-CL-TIME-OF-DAY                PIC S9(4)  COMP-3.
HO3603         10  AORS-CL-DATE-RECEIVED              PIC S9(5)  COMP-3.
HO3603         10  AORS-CL-DISPOSITION                PIC X.
HO3603             88  AORS-CL-CLEAN                      VALUE 'C'.
HO3603             88  AORS-CL-DIRTY                      VALUE 'D'.
HO3603             88  AORS-CL-NOHIT                      VALUE 'N'.
HO3603         10  AORS-CL-LAST-NAME                  PIC X(20).
HO3603         10  AORS-CL-FIRST-NAME                 PIC X(15).
HO3603         10  AORS-CL-MIDDLE-INIT                PIC X.
HO3603         10  AORS-CL-SUFFIX-NAME                PIC X(5).
HO3603         10  AORS-CL-UNDERWRITER-INITS          PIC XXX.


HO3603     05  AORS-OUTSIDE-RPT-CRDIT-TBL.
HO3603       07  AORS-OUTSIDE-REPORT-CRDIT-INFO       OCCURS 6 TIMES
HO3603                                                INDEXED BY
HO3603                                                AMCR-INDEX.
HO3603         10  AORS-CR-ORDER-TYPE                 PIC XX.
HO3603             88 AORS-CREDIT                         VALUE 'CR'.
HO3603         10  AORS-CR-ORDER-COMPANY              PIC X.
HO3603             88 AORS-TRW                            VALUE '1'.
HO3603             88 AORS-CBI                            VALUE '2'.
HO3603             88 AORS-TRANS-UNION                    VALUE '3'.
HO3603         10  AORS-CR-DATE-ORDERED               PIC S9(5)  COMP-3.
HO3603         10  AORS-CR-TIME-OF-DAY                PIC S9(4)  COMP-3.
HO3603         10  AORS-CR-DATE-RECEIVED              PIC S9(5)  COMP-3.
HO3603         10  AORS-CR-DISPOSITION                PIC X.
HO3603             88 AORS-CR-CLEAN                       VALUE 'C'.
HO3603             88 AORS-CR-DIRTY                       VALUE 'D'.
HO3603             88 AORS-CR-NOHIT                       VALUE 'N'.


HO3603     05  AUND-UND-NOTATIONS-TBL.
HO3603       07  AUND-UND-NOTATIONS                   OCCURS 15 TIMES
HO3603                                                INDEXED BY
HO3603                                                AUND-NOTE-INDEX.
HO3603         10  AUND-NOTE-ACTION-TYPE              PIC X.
HO3603             88  AUND-NOTE-INITIAL-VALUE            VALUE ' '.
HO3603             88  AUND-NOTE-ADD-ONLY                 VALUE 'A'.
HO3603             88  AUND-NOTE-CHANGE-ONLY              VALUE 'C'.
HO3603             88  AUND-NOTE-ELIMINATE-ONLY           VALUE 'E'.
HO3603         10  AUND-UND-NOTATIONS-NUMBER          PIC S9     COMP-3.
HO3603         10  AUND-UND-NOTATION-TYPE             PIC XX.
HO3603             88  AUND-SUBSTANDARD                   VALUE '11'.
HO3603             88  AUND-GRADATION                     VALUE '12'.
HO3603             88  AUND-ANTEDATED-OR-REFORMED         VALUE '13'.
HO3603             88  AUND-CONFIDENTIAL-RISK-REPORT      VALUE '14'.
HO3603             88  AUND-LARGE-LOSS                    VALUE '15'.
HO3603             88  AUND-MISC                          VALUE '16'.
HO3603             88  AUND-SUPPORTING-BUSINESS           VALUE '17'.
HO3603             88  AUND-NOTE-OTHER-B                  VALUE '18'.
HO3603             88  AUND-NOTE-OTHER-C                  VALUE '19'.
HO3603             88  AUND-NOTE-OTHER-D                  VALUE '20'.
HO3603             88  AUND-NOTE-NON-REN                  VALUE '21'.
HO3603             88  AUND-NOTE-MISC-NO-APOF             VALUE '22'.
HO3603             88  AUND-NOTE-RISK-SUMMARY-REPORT      VALUE '43'.
HO3603             88  AUND-NOTE-AETNA-REALTY             VALUE '97'.
HO3603             88  AUND-NOTE-AETNA-BUS-CREDIT         VALUE '98'.
HO3603             88  AUND-NOTE-URBAN-INVESTMENTS        VALUE '99'.
HO3603         10  AUND-NOTE-CAUSE-DATE               PIC S9(5)  COMP-3.
HO3603         10  AUND-UND-NOTE-DESCRIPTION          PIC X(40).
HO3603         10  AUND-UND-NOTE-UND-INITIALS         PIC XXX.
HO3603         10  AUND-UND-NOTE-DATE-MADE            PIC S9(5)  COMP-3.

954240     05  M-BEFORE-TRAVELERS-CLAIMS-TBL2.
954240       07  M-BEFORE-TRAVELERS-CLAIMS-SEC2       OCCURS 4 TIMES
954240                                                INDEXED BY
954240                                                MBEF-INDEX2.
954240         10  MBEF-LOSS-CAUSE-CODE               PIC 9(3).
954240     05  MFILLER-39                             PIC X(188).

HO3603     05  MEND-UMBRELLA-INFO-AREA.
               10  MEND-UMBRELLA-LIMITS                          COMP-3.
                   15  MEND-UMB-POLICY-LIMITS         PIC 99.
SRMSRM             15  MFILLER-40                     PIC 9(3).         33120006
SRMSRM             15  MFILLER-41                     PIC 9(3).         33130006
                   15  MEND-UMB-UM-UIM-STAT-CODE      PIC 99.
SRMSRM             15  MFILLER-42                     PIC 99.           33150006
               10  MEND-UMBRELLA-INDICATORS.
                   15  MEND-UMB-AETNA-AUTO-IND        PIC X.
                   15  MEND-UMB-ADD-RESID-IND         PIC X.
                   15  MEND-UMB-PREM-RENTED-IND       PIC X.
                   15  MEND-UMB-FARM-EXP-IND          PIC X.
                   15  MEND-UMB-REC-VEH-IND           PIC X.
                   15  MEND-UMB-POWERBOAT-IND         PIC X.
                   15  MEND-UMB-SAILBOAT-IND          PIC X.
                   15  MEND-UMB-BUS-PURS-IND          PIC X.
                   15  MEND-UMB-SWIM-POOL-IND         PIC X.
HO3603             15  MEND-UMB-HIGH-RISK-IND         PIC X.
                   15  MEND-UMB-DRIV-REC-CR-IND       PIC X.
                   15  MEND-UMB-INELIG-RESID-BOAT-RV  PIC X.
                   15  MEND-UMB-RE-QUESTION-THIS-YR   PIC X.
                   15  MEND-UMB-UM-UIM-LIM-IDENT      PIC X.
SRMSRM             15  MFILLER-43                     PIC X(8).         33310006
               10  MEND-UMBRELLA-TERR                 PIC 99     COMP-3.
H03603         10  MEND-UMB-TOTAL-NO-AUTOS            PIC 99     COMP-3.
               10  MEND-UMB-NEXT-REVIEW               PIC 99     COMP-3.
               10  MEND-UMB-NO-YOUTH-DRIVERS          PIC 99     COMP-3.
               10  MEND-UMBRELLA-DATE-FIELDS                     COMP-3.
HO3603             15  MEND-UMB-REN-QUEST-DATE        PIC S9(5).
HO3603             15  MEND-UMB-LAST-REVIEW-DATE      PIC S9(5).
HO3603             15  MEND-UMB-ADD-DATE              PIC S9(5).
HO3603             15  MEND-UMB-ELIM-DATE             PIC S9(5).
HO3603             15  MEND-UMB-NEXT-REVIEW-DATE      PIC S9(5).

               10  MEND-UMBRELLA-DRIVER-TBL.
                 12  MEND-UMBRELLA-DRIVER-INFO        OCCURS 7 TIMES
                                                      INDEXED BY
                                                      MEND-UMB-1.
                   15  MEND-UMB-DRIVER-ACT-TYPE       PIC X.
                   15  MEND-UMB-DRIVER-NUMBER         PIC 9.
                   15  MEND-UMB-DRIVER-FIRST-NAME     PIC X(15).
                   15  MEND-UMB-DRIVER-MI             PIC X.
                   15  MEND-UMB-DRIVER-LAST-NAME      PIC X(20).
                   15  MEND-UMB-DRIVER-DOB            PIC S9(5)  COMP-3.
                   15  MEND-UMB-LICENSE-NUMBER        PIC X(20).
                   15  MEND-UMB-LICENSE-STATE         PIC XX.
                   15  MEND-UMB-DRIVER-AGE            PIC S9(3).
PL4383             15  MEND-UMB-DRIVER-DFL            PIC 9(08).
PL4383             15  MEND-NUMBER-OF-YRS-DRV         PIC 9(03).
PL4383             15  MEND-POLICY-CHANGE-IND         PIC X(01).
PL4383             15  MFILLER-44                     PIC X(05).
SRMSRM*            15  MFILLER-44                     PIC X(17).        33560006

               10  MEND-UMB-PREMIUM-FIELDS                       COMP-3.
HO3603             15  MEND-UMB-BASIC-PREM            PIC S9(5)V99.
HO3603             15  MEND-UMB-ADDL-AUTO-PREM        PIC S9(5)V99.
HO3603             15  MEND-UMB-YOUTHFUL-OP-PREM      PIC S9(5)V99.
HO3603             15  MEND-UMB-MISC-EXP-PREM         PIC S9(5)V99.
HO3603             15  MEND-UMB-INELIG-EXP-PREM       PIC S9(5)V99.
HO3603             15  MEND-UMB-CREDIT-PREM           PIC S9(5)V99.
HO3603             15  MEND-UMB-UM-UIM-PREM           PIC S9(5)V99.
HO3603             15  MEND-UMB-SUB-TOTAL-1M          PIC S9(5)V99.
HO3603             15  MEND-UMB-INCR-LIM-FACTOR       PIC S9V99.
HO3603             15  MEND-UMB-FULL-TERM             PIC S9(5)V99.
HO3603             15  MEND-UMB-AR-ONS                PIC S9(5)V99.
HO3603             15  MEND-UMB-AR-OFF                PIC S9(5)V99.
HO3603             15  MEND-UMB-PREM-THIS-TRX         PIC S9(5)V99.
HO3603             15  MEND-UMB-SUB-TOT-INCR-LIM      PIC S9(5)V99.
HO3603             15  MEND-UMB-HI-RISK-PREM          PIC S9(5)V99.
HO3603         10  MEND-UMB-MIN-PREM-APPLIES          PIC X.
HO3584         10  MEND-UMBRELLA-UM-UIM-LIMITS                   COMP-3.
HO3584             15  MEND-UMB-UM-UIM-PER-PERS       PIC S9(9).
HO3584             15  MEND-UMB-UM-UIM-PER-ACC        PIC S9(9).
HO3584         10  MEND-UMB-YTH-INEX-DR-IND           PIC X.
884930         10  MEND-UMB-UM-UIM-PD-IND             PIC X.
906230         10  MEND-UMB-WTRCRFT-IND               PIC X.
906230         10  MEND-UMB-WTRCRFT-LIAB-LIMIT        PIC S9(7) COMP-3.
906230         10  MEND-UMBRELLA-WTRCRFT-TBL.
906230           12  MEND-UMBRELLA-WTRCRFT-INFO       OCCURS 2 TIMES
906230                                                INDEXED BY
906230                                                MEND-UMB-2.
906230             15  MEND-UMB-WTRCRFT-TYPE          PIC X.
906230             15  MEND-UMB-WTRCRFT-LENGTH        PIC X(2).

PL4383         10  MEND-UMB-INEXP-DRIVER-IND          PIC X.
PL4383         10  MFILLER-45                         PIC X(25).
906230*        10  MFILLER-45                         PIC X(26).
SRMSRM*        10  MFILLER-45                         PIC X(37).        33782006


GEICOX     05  MRESII-GEICO-CREDIT-AREA                          COMP-3.
GEICOX         10  MRESII-NON-SMOKER-CR               PIC SV99.
GEICOX         10  MRESII-NON-SMOKER-FULL             PIC S9(3)V99.
GEICOX         10  MRESII-NON-SMOKER-ONS              PIC S9(3)V99.
GEICOX         10  MRESII-NON-SMOKER-OFF              PIC S9(3)V99.
GEICOX         10  MRESII-PRIME-TIME-CR               PIC SV99.
GEICOX         10  MRESII-PRIME-TIME-FULL             PIC S9(3)V99.
GEICOX         10  MRESII-PRIME-TIME-ONS              PIC S9(3)V99.
GEICOX         10  MRESII-PRIME-TIME-OFF              PIC S9(3)V99.


GEICOX     05  MRESII-NON-SMOKER-IND                  PIC X.
GEICOX     05  MRESII-PRIME-TIME-IND                  PIC X.
HO3588     05  MRESIV-LAST-DATE-EQ-LET-SENT           PIC S9(5)  COMP-3.
HO3612     05  M-NJ-RETCOM-RATING-METHOD              PIC X.
GEICO*     05  MGEICO-YRS-OF-INSURANCE                PIC 9(2).
HO3598     05  MRESIV-SEA-STORM-SHUTTER-TYPE          PIC X.
HO3598         88  M-VALID-SHUTTER-TYPES                  VALUES 'A'
HO3598                                                           'B'.


HO3598     05  MEND-SS-OTH-ADDL-CR                PIC S9(5)V99   COMP-3.
HO3598     05  MEND-SS-BASE-CR                    PIC S9(5)V99   COMP-3.


632080     05  MGEICO-IRAS-FILE                       PIC X.
HO3671     05  MRESIV-EQ-ADL-SW                       PIC X.
640560     05  MRESIV-COASTAL-HA-198-ROLL-ON          PIC X.
640560     05  MRESIV-HA196-TO-HA198-SW               PIC X.
640560     05  MRESIV-HA198-TO-HA196-SW               PIC X.
640560     05  MRESIV-COASTAL-HA198-OPTIONAL          PIC X.
636910     05  MRESIV-CONVTD-TO-WINDPOOL-TERR         PIC X.
640560     05  MRESIV-ZIP-CODE-CHG-IND                PIC X.
640560     05  MRESIV-RESID-PREM-ZIP-CHG-IND          PIC X.
644350     05  MRESIV-COASTAL-58169-ROLL-ON           PIC X. 
644350     05  MRESIV-COASTAL-58169-ELIM-SW           PIC X.
644350     05  MRESIV-COASTAL-58171-ELIM-SW           PIC X.
644350     05  MRESIV-ROLLON-58171-SW                 PIC X.


674850     05  MCLA-CPCS-CLAIM-SERIAL-TBL.
674850         10  MCLA-CPCS-CLAIM-SERIAL-NUM         OCCURS 10 TIMES
674850                                                INDEXED BY
674850                                                MCLA-CPCS-INDEX
674850                                                PIC X(7).


TAPCO0     05  M-ORIGINAL-POLICY-NUMBER-BEU.
TAPCO0         10  M-AETNA-ORIG-POL-NUMBER.
TAPCO0             15  M-AET-ORIG-POL-SYMBOL          PIC XX.
TAPCO0             15  M-AET-ORIG-POL-NUMBER          PIC 9(10).
TAPCO0             15  M-AET-ORIG-POL-SUFFIX          PIC X(3).

TAPCO0         10  M-TIC-ORIG-POLICY-NUMBER           REDEFINES
TAPCO0              M-AETNA-ORIG-POL-NUMBER.
TAPCO0             15  M-TIC-POL-NUMBER               PIC X(9).
TAPCO0             15  M-TIC-POL-FORM                 PIC X(3).
TAPCO0             15  M-TIC-SEQ-NUMBER               PIC X.
SRMSRM             15  MFILLER-46                     PIC XX.           34400006

TAPCO0         10  M-TAP-NEW-BUSINESS                 REDEFINES
TAPCO0              M-AETNA-ORIG-POL-NUMBER.
TAPCO0             15  M-TAP-NB-DEFAULT-POL-SYMBOL    PIC XX.
SRMSRM             15  MFILLER-47                     PIC X(10).        34450006
TAPCO0             15  M-TAP-NB-DEFAULT-POL-SUFFIX    PIC X(3).

TAPCO0         10  M-TAP-POLICY-SOURCE                PIC X.
TAPCO0             88  M-TIC                              VALUE 'T'.
TAPCO0             88  M-AET                              VALUE 'A'.
TAPCO0             88  M-NB                               VALUE 'N'.

TAPCO0     05  M-TAP-SEQUENCE-NUMBER                  PIC X.
TAPCO0     05  M-TAP-POLICY-FORM-NUMBER               PIC X(3).
SRMSRM     05  MFILLER-48                             PIC X(16).        34550006


TAPCO0     05  M-TAP-GENERAL-SEC.
TAPCO0         10  M-DECISION-IND                     PIC X.
TAPCO0             88  M-TAP                              VALUE 'Y'.
TAPCO0             88  M-NON-TAP                          VALUE 'N'
TAPCO0                                                          ' '.

TAPCO0         10  M-MARKET-VALUE                     PIC X(7).
TAPCO0         10  M-MARKET-IND.
TAPCO0             15  M-MKT-IND-ONS                  PIC X.
TAPCO0             15  M-MKT-IND-OFF                  PIC X.

TAPCO0         10  M-DISTRICT-IND.
TAPCO0             15  M-DISTRICT-ID-ONS              PIC X(3).
TAPCO0             15  M-DISTRICT-ID-OFF              PIC X(3).

TAPCO0         10  M-AGENT-AREA.
TAPCO0             15  M-CLIENT-CD                    PIC X(6).

TAPCO0             15  M-OVRR-AGENT                   PIC X(6).
TAPCO0             15  M-OVRR-OFFICE                  PIC X(3).
TAPCO0             15  M-OVRR-COMM-RATE               PIC SV9(4) COMP-3.
TAPCO0             15  M-OVRR-PAF-COMM-RATE           PIC SV9(4) COMP-3.
TAPCO0             15  M-OVRR-PLUS-COMM-RATE          PIC SV9(4) COMP-3.

TAPCO0             15  M-CTRSGN-AGENT                 PIC X(6).
TAPCO0             15  M-CTRSGN-OFFICE                PIC X(3).
TAPCO0             15  M-CTRSGN-COMM-RATE             PIC SV9(4) COMP-3.
TAPCO0             15  M-CTRSGN-PAF-COMM-RATE         PIC SV9(4) COMP-3.
TAPCO0             15  M-CTRSGN-PLUS-COMM-RATE        PIC SV9(4) COMP-3.

TAPCO0             15  M-CONTROL-AGENT-ONS            PIC X(6).
TAPCO0             15  M-CONTROL-AGENT-OFF            PIC X(6).
TAPCO0             15  M-COMM-RATE                    PIC SV9(4) COMP-3.
TAPCO0             15  M-PAF-COMM-RATE                PIC SV9(4) COMP-3.
TAPCO0             15  M-PLUS-COMM-RATE               PIC SV9(4) COMP-3.

TAPCO0             15  M-SUB-AGENT                    PIC X(3).
TAPCO0             15  M-MSTR-OF-SUB-AGT              PIC X(6).
TAPCO0             15  M-PART-AGENT                   PIC X(6).
TAPCO0             15  M-NON-RES-AGENT-CD             PIC XX.

TAPCO0         10  M-SERVICING-OFFICE                 PIC X(3).
TAPCO0         10  M-REPORTING-AREA.
TAPCO0             15  M-RPTG-OFFICE-ONS              PIC X(3).
TAPCO0             15  M-RPTG-OFFICE-OFF              PIC X(3).

SRMSRM         10  MFILLER-49                         PIC X(18).        35040006
TAPCO0         10  M-PAY-CODE-AREA.
TAPCO0             15  M-TYPE-PYMT-CD-ONS             PIC X(2).
TAPCO0             15  M-TYPE-PYMT-CD-OFF             PIC X(2).

TAPCO0         10  M-MASS-MRKT-AREA.
TAPCO0             15  M-MKT-SPEC-PROD-CD-ONS         PIC X(6).
TAPCO0             15  M-MKT-SPEC-PROD-CD-OFF         PIC X(6).

TAPCO0         10  M-EFT-AREA.
TAPCO0             15  M-EFT-CHK-SAV-IND              PIC X.
TAPCO0             15  M-EFT-BANK-ACCT-NO             PIC X(17).
TAPCO0             15  M-EFT-BANK-TRANS-NO            PIC X(9).

TAPCO0         10  M-STATEMENT-BILLING-AREA.
TAPCO0             15  M-DEPT-NO                      PIC X(10).
TAPCO0             15  M-BILL-ENTITY-NO               PIC X(3).
TAPCO0             15  M-BILL-NO                      PIC X(10).

TAPCO0         10  M-ABS-ACCT-NO                      PIC X(9).

TAPCO0         10  M-QUOTE-DATE                       PIC S9(5)  COMP-3.
TAPCO0         10  M-PEND-CANC-DT                     PIC S9(5)  COMP-3.
TAPCO0         10  M-PURGE-DT                         PIC S9(5)  COMP-3.
TAPCO0         10  M-NR-CD                            PIC X.
TAPCO0         10  M-ERP-IND                          PIC X.
TAPCO0         10  M-TVP-IND                          PIC X.
TAPCO0         10  M-SWIMMING-POOL-IND                PIC X.
TAPCO0         10  M-PRICE                            PIC X(5).

TAPCO0         10  M-TIC-INF-COMPANY-CD           PIC X(3).
TAPCO0         10  M-TIC-INF-PRICE                PIC X(5).
TAPCO0         10  M-TIC-INF-WRITTEN-PREM         PIC S9(5)V99   COMP-3.


SRMSRM     05  MFILLER-50A                            PIC X(14).        35390006


HO3648     05  MORS-OUTSIDE-REPT-CLAIM-AREA.
HO3648         10  MORS-OUTSIDE-REPT-CLAIM-INFO       OCCURS 6 TIMES
HO3648                                                INDEXED BY
HO3648                                                MCL-INDEX.
HO3648             15  MORS-CL-ORDER-TYPE             PIC XX.
HO3648                 88  MORS-CLAIM                     VALUE 'CL'.
HO3648             15  MORS-CL-DATE-ORDERED           PIC S9(5) COMP-3.
HO3648             15  MORS-CL-TIME-OF-DAY            PIC S9(4) COMP-3.
HO3648             15  MORS-CL-DATE-RECEIVED          PIC S9(5) COMP-3.
HO3648             15  MORS-CL-DISPOSITION            PIC XX.
HO3648                 88  MORS-CL-CLEAN                  VALUE 'C'.
HO3648                 88  MORS-CL-DIRTY                  VALUE 'D'.
HO3648                 88  MORS-CL-NOHIT                  VALUE 'N'.
HO3648             15  MORS-CL-LAST-NAME              PIC X(20).
HO3648             15  MORS-CL-FIRST-NAME             PIC X(15).
HO3648             15  MORS-CL-MIDDLE-INIT            PIC X.
HO3648             15  MORS-CL-SUFFIX-NAME            PIC X(5).
HO3648             15  MORS-CL-UNDERWRITER-INITS      PIC XXX.
HO3648             15  MORS-CL-SSN                    PIC 9(9).
HO3648             15  MORS-CL-HHM-SEQ-NUMBER         PIC 9(3).
SRMSRM             15  MFILLER-51                     PIC X(42).        35620006


HO3648     05  MORS-OUTSIDE-REPT-CREDIT-AREA.
HO3648         10  MORS-OUTSIDE-REPT-CREDIT-INFO      OCCURS 6 TIMES
HO3648                                                INDEXED BY
HO3648                                                MCR-INDEX.
HO3648             15  MORS-CR-ORDER-TYPE             PIC XX.
HO3648                 88  MORS-CREDIT                    VALUE 'CR'.
HO3648             15  MORS-CR-ORDER-COMPANY          PIC X.
HO3648                 88  MORS-TRW                       VALUE '1'.
HO3648                 88  MORS-CBI                       VALUE '2'.
HO3648                 88  MORS-TRANS-UNION               VALUE '3'.
HO3648             15  MORS-CR-DATE-ORDERED           PIC S9(5)  COMP-3.
HO3648             15  MORS-CR-TIME-OF-DAY            PIC S9(4)  COMP-3.
HO3648             15  MORS-CR-DATE-RECEIVED          PIC S9(5)  COMP-3.
HO3648             15  MORS-CR-DISPOSITION            PIC XX.
HO3648                 88  MORS-CR-CLEAN                  VALUE 'C'.
HO3648                 88  MORS-CR-DIRTY                  VALUE 'D'.
HO3648                 88  MORS-CR-NOHIT                  VALUE 'N'.
HO3648             15  MORS-CR-LAST-NAME              PIC X(20).
HO3648             15  MORS-CR-FIRST-NAME             PIC X(15).
HO3648             15  MORS-CR-MIDDLE-INIT            PIC X.
HO3648             15  MORS-CR-SUFFIX-NAME            PIC X(5).
HO3648             15  MORS-CR-UNDERWRITER-INITS      PIC XXX.
HO3648             15  MORS-CR-SSN                    PIC 9(9).
HO3648             15  MORS-CR-SCORE                  PIC 9(4).
HO3648             15  MORS-CR-SCORE-REASONS-TBL.
HO3648                 20  MORS-CR-SCORE-REASONS      OCCURS 6 TIMES
HO3648                                                INDEXED BY
HO3648                                                CR-SCR-INDEX.
HO3648                     25  MORS-CR-SCR-REASON     PIC XXX.
HO3648             15  MORS-CR-HHM-SEQ-NUMBER         PIC 9(3).
SRMSRM             15  MFILLER-52                     PIC X(36).        35950006

HO3648     05  MORS-CR-CL-CONV-SW                     PIC X.
HO3648     05  MORS-CRIT-ERR-ON-INPUT                 PIC X.


SRMSRM     05  MFILLER-53                             PIC X(18).        36010006


TAPCO0     05  M-EPUB-SECTION.
TAPCO0         10  M-EPUB-GENERAL.
TAPCO0             15  M-EPUB-FIRST-TIME-TAP        PIC X.
TAPCO0                 88  M-FIRST-TIME-TAP             VALUE 'Y'.
TAPCO0                 88  M-NOT-FIRST-TIME-TAP         VALUE 'N'.
TAPCO0             15  M-EPUB-DEC-FLD-IND           PIC X.
TAPCO0                 88  M-FLD-DEC                    VALUE 'Y'.
TAPCO0                 88  M-SUPPRESS-FLD-DEC           VALUE 'N'.
TAPCO0             15  M-EPUB-MORTGAGEE-DEC         PIC X.
TAPCO0                 88  M-MORTGAGEE-DEC              VALUE 'Y'.
TAPCO0                 88  M-SUPPRESS-MORT-DEC          VALUE 'N'.
TAPCO0             15  M-EPUB-AGENT-DEC             PIC X.
TAPCO0                 88  M-AGENT-DEC                  VALUE 'Y'.
TAPCO0                 88  M-SUPPRESS-AGT-DEC           VALUE 'N'.
TAPCO0             15  M-EPUB-FULL-POL-ASSM         PIC X.
TAPCO0             15  M-EPUB-CHG-BASIC-COMM        PIC S9(5)V99 COMP-3.
TAPCO0             15  M-EPUB-CHG-EQ-COMM           PIC S9(5)V99 COMP-3.
TAPCO0             15  M-EPUB-CHG-PLUS-COMM         PIC S9(5)V99 COMP-3.
TAPCO0             15  M-EPUB-CHG-VI-COMM           PIC S9(5)V99 COMP-3.
TAPCO0             15  M-EPUB-EQ-PERCENT            PIC SV9(4)   COMP-3.
TAPCO0             15  M-EPUB-DPRA-OVER-IND         PIC X.
TAPCO0             15  M-EPUB-TOTAL-CREDITS         PIC S9(5)V99 COMP-3.
TAPCO0             15  M-EPUB-TOTAL-SURCHARGE       PIC S9(5)V99 COMP-3.
TAPCO0             15  M-EPUB-TOTAL-OTHER           PIC S9(5)V99 COMP-3.
TAPCO0             15  M-EPUB-CHG-IN-HOME-COMM      PIC S9(5)V99 COMP-3.
TAPCO0             15  M-EPUB-IN-HOME-COMM-RATE     PIC SV9(5)   COMP-3.
NYTPD*             15  M-EPUB-THIRD-RENEWAL         PIC X.
HVHSEL             15  M-EPUB-FIRST-TIME-HVH        PIC X.
TXMOLD             15  M-EPUB-MOLD-CONV-IND         PIC X.
TXMOLD                 88  M-TX-FORM-A              VALUE 'A'.
TXMOLD                 88  M-TX-FORM-B-TO-A         VALUE 'B'.
TXMOLD                 88  M-TX-FORM-C-TO-A         VALUE 'C'.
MDDECS             15  M-RENEWAL-PREM-DIFF          PIC S9(5)V99 COMP-3.
TXMOLD         10  MFILLER-54                       PIC X(69).

TAPCO0         10  M-EPUB-REASON-CD-TBL.
TAPCO0             15  M-EPUB-CHG-REASON-CODES        OCCURS 10 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                MCHG-CD-INDEX.
TAPCO0                 20  M-EPUB-CHG-REASON-CD       PIC X(3).
TAPCO0                 20  M-EPUB-CHG-ENDT-FORM-NO    PIC X(12).
TAPCO0                 20  M-EPUB-CHG-ENDT-ED-DT      PIC X(4).
NBPLET         10  M-EPUB-NBP-LETTER.
NBPLET             15  M-EPUB-NBP-LTR-IND           PIC X(03).
NBPLET             15  M-EPUB-NBP-CR-VENDOR-IND     PIC X(01).
NBPLET             15  M-EPUB-NBP-CL-VENDOR-IND     PIC X(01).
NBPLET             15  M-EPUB-NBP-FILLER            PIC X(05).
NBPLET         10  M-EPUB-NBP-LTR-SENT-TBL.
NBPLET             15  M-EPUB-NBP-LETTER-SENT     OCCURS 7 TIMES
NBPLET                                            INDEXED BY
NBPLET                                            MNBP-LTR-INDEX.
NBPLET                 20  M-EPUB-NBP-LTR-DATE        PIC 9(08).
NBPLET         10  MFILLER-55                         PIC X(328).       36410006
SRMSRM*        10  MFILLER-55                         PIC X(394).       36410006


TAPCO0     05  M-FIRST-MORTGAGEE-ADDRESS.
TAPCO0         10  M-FIRST-MORT-ADDR-1                PIC X(30).


TAPCO0     05  M-SECOND-MORTGAGEE-ADDRESS.
TAPCO0         10  M-SECOND-MORT-ADDR-1               PIC X(30).


TAPCO0     05  M-THIRD-MORTGAGEE-ADDRESS.
TAPCO0         10  M-THIRD-MORT-ADDR-1                PIC X(30).


TAPCO0     05  M-OTHER-PARTY-TBL.
TAPCO0         10  M-OTHER-PARTY-DATA                 OCCURS 2 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                M-PARTY-INDEX.
TAPCO0             15  M-NAME-PERS-OR-ORGN            PIC X(30).
TAPCO0             15  M-O-P-ADDRESS-1                PIC X(30).
TAPCO0             15  M-O-P-ADDRESS-2                PIC X(20).
962190             15  M-O-P-FILLER-1 REDEFINES
962190                              M-O-P-ADDRESS-2.
962190                 20 M-O-P-ADDRESS-2-CITY        PIC X(17).
962190                 20 M-O-P-FILLER-2              PIC X(01).
962190                 20 M-O-P-ADDRESS-2-STATE       PIC X(02).
TAPCO0             15  M-O-P-ZIP-CODE                 PIC S9(5)  COMP-3.
TAPCO0             15  M-INT-IN-PREMISES              PIC X(20).


TAPCO0     05  M-NET-CHG-PREM-CURR                PIC S9(5)V99   COMP-3.
TAPCO0     05  M-NET-CHG-PREM-DEFER               PIC S9(5)V99   COMP-3.
TAPCO0     05  M-NET-CHG-COMM-CURR                PIC S9(5)V99   COMP-3.
TAPCO0     05  M-NET-CHG-COMM-DEFER               PIC S9(5)V99   COMP-3.
TAPCO0     05  M-NET-CHG-TAX-CURR                 PIC S9(5)V99   COMP-3.
TAPCO0     05  M-NET-CHG-TAX-DEFER                PIC S9(5)V99   COMP-3.


TAPCO0     05  M-3RD-MORT-LOAN-NO                     PIC X(20).
TAPCO0     05  M-KBMS-UND-TIER                        PIC X(5).
TAPCO0         88  M-CURR-REG-NSTD-TIER                   VALUE 'REG'
TAPCO0                                                          'NSTD'.
TAPCO0         88  M-CURR-PREF-SPREF-TIER                 VALUE 'PREF'
TAPCO0                                                          'SPREF'.
TAPCO0     05  M-KBMS-PRIOR-TIER                      PIC X(5).
TAPCO0         88  M-PRIOR-REG-NSTD-TIER                  VALUE 'REG'
TAPCO0                                                          'NSTD'.
TAPCO0         88  M-PRIOR-PREF-SPREF-TIER                VALUE 'PREF'
TAPCO0                                                          'SPREF'.
SRMSRM     05  MFILLER-56                             PIC X.            36860006 
TAPCO0     05  M-SWIM-UNFEN-POOL-IND                  PIC X.
TAPCO0     05  M-PREMIUM-LEVEL                        PIC XX.
TAPCO0     05  M-LOCATION-NUM                         PIC X.
TAPCO0     05  M-COMPANY-ALPHA-CODE                   PIC XXX.

TAPCO0     05  M-CAUSE-OF-LOSS-TABLE.
TAPCO0         10  M-CAUSE-OF-LOSS-TBL                OCCURS 10 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                M-CAUSE-INDEX.
TAPCO0             15  M-CAUSE-OF-LOSS                PIC XXX.

TAPCO0     05  M-IS-CTLP-PAY-TABLE.
TAPCO0         10  M-IS-CTLP-ENTRIES                  OCCURS 10 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                M-IS-CTLP-INDEX.
TAPCO0             15  M-IS-CTLP-LINE                 PIC XX.
TAPCO0             15  M-IS-CTLP-COMM-RATE            PIC 9V9(5) COMP-3.
TAPCO0             15  M-IS-CTLP-TOTL-PREM          PIC S9(7)V99 COMP-3.
 
TAPCO0     05  M-IS-CTRP-PAY-TABLE.
TAPCO0         10  M-IS-CTRP-ENTRIES                  OCCURS 10 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                M-IS-CTRP-INDEX.
TAPCO0             15  M-IS-CTRP-LINE                 PIC XX.
TAPCO0             15  M-IS-CTRP-COMM-RATE            PIC 9V9(5) COMP-3.

TAPCO0     05  M-IS-OVRP-PAY-TABLE.
TAPCO0         10  M-IS-OVRP-ENTRIES                  OCCURS 10 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                M-IS-OVRP-INDEX.
TAPCO0             15  M-IS-OVRP-LINE                 PIC XX.
TAPCO0             15  M-IS-OVRP-COMM-RATE            PIC 9V9(5) COMP-3. 

TAPCO0     05  M-FIRST-TRENTON-SW                     PIC X.

TAPCO0     05  M-UMBRELLA-LIABILITY-INFO-AREA                    COMP-3.
TAPCO0         10  M-UMB-AUTO-PER-PERS-LIMIT          PIC S9(7).
TAPCO0         10  M-UMB-AUTO-PER-ACC-LIMIT           PIC S9(7).
TAPCO0         10  M-UMB-AUTO-PROP-DAM-LIMIT          PIC S9(7).
TAPCO0         10  M-UMB-HO-LIABILITY-LIMIT           PIC S9(7).
TAPCO0         10  M-UMB-POWERBOAT-LIAB-LIMIT         PIC S9(7).
TAPCO0         10  M-UMB-REC-VEH-PER-PERS-LIMIT       PIC S9(7).
TAPCO0         10  M-UMB-REC-VEH-PER-ACC-LIMIT        PIC S9(7).
TAPCO0         10  M-UMB-REC-VEH-PROP-DAM-LIMIT       PIC S9(7).
TAPCO0         10  M-UMB-BUS-PURS-LIAB-LIMIT          PIC S9(7).
TAPCO0         10  M-UMB-BUS-PROP-LIAB-LIMIT          PIC S9(7).
TAPCO0         10  M-UMB-MIN-LIMIT-PREM               PIC S9(5)V99.

TAPCO0     05  M-UMBRELLA-INFO-AREA.
TAPCO0         10  M-UMB-LOSS-ASSESS-IND              PIC X.
TAPCO0         10  M-UMB-SAILBOAT-LIAB-LIMIT          PIC S9(7)  COMP-3.
TAPCO0         10  M-FIRST-TAP-EFF-DATE               PIC S9(5)  COMP-3.
TAPCO0         10  M-FIRST-TAP-PROC-DATE              PIC S9(5)  COMP-3.

TAPCO0     05  M-BRANCH-AGENCY-XFER-AREA.
TAPCO0         10  M-XFER-NEW-BRANCH                  PIC X(3).
TAPCO0         10  M-XFER-NEW-AGENT                   PIC X(6).
TAPCO0         10  M-XFER-NEW-SRVC-OFFICE             PIC X(3).
TAPCO0         10  M-XFER-NEW-RPTG-OFFICE             PIC X(3).
TAPCO0         10  M-XFER-OLD-BRANCH                  PIC X(3).
TAPCO0         10  M-XFER-OLD-AGENT                   PIC X(6).
TAPCO0         10  M-XFER-OLD-SRVC-OFFICE             PIC X(3).
TAPCO0         10  M-XFER-OLD-RPTG-OFFICE             PIC X(3).

TAPCO0     05  M-ENDT-TBL-CONV-IND                    PIC X.

TAPCO0     05  M-ATLAS-INFO.
TAPCO0         10  M-ATLAS-CUST-NUM                   PIC X(16).
TAPCO0         10  M-ATLAS-QUOTE-NUM                  PIC X(13).
TAPCO0         10  M-ATLAS-RATE-DATE                  PIC S9(7)  COMP-3.
TAPCO0         10  M-ATLAS-NOTEPAD-IND                PIC X.
TAPCO0         10  M-ATLAS-APPROVED-IND               PIC X.
TAPCO0         10  M-ATLAS-REFER-ISSUE-IND            PIC X.
TAPCO0         10  M-ATLAS-ORIGINATED                 PIC X.
SRMSRM         10  MFILLER-57                         PIC X(10).        37610006

698720     05  M-HURRICANE-ROLLON-SW.
698720         10  MRESIV-ROLLON-58188-SW             PIC X.
698720         10  MRESIV-COASTAL-58188-ELIM-SW       PIC X.
698720         10  MRESIV-58171-ALT-ROLL-ON           PIC X.

TAPCO0     05  M-UMB-PLUS.
TAPCO0         10  M-UMB-INCIDNTL-OCCUPNCY-OCC        PIC X(20).
TAPCO0         10  M-UMB-INCIDNTL-OCCUPNCY-LOC        PIC X(20).
TAPCO0         10  M-UMB-LOSS-ASSESS-LIAB-LIMIT       PIC S9(7)  COMP-3.
TAPCO0         10  M-UMB-BUS-PURS-TABLE.
TAPCO0             15  M-UMB-BUS-PURS-INFO            OCCURS 2 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                M-UMB-BUS-INDEX.
TAPCO0                 20  M-UMB-BUS-PURS-NAME-INSD   PIC X(20).
TAPCO0                 20  M-UMB-BUS-PURS-OCCUPATION  PIC X(15).

TAPCO0     05  MRESII-INFO.
TAPCO0         10  MRESII-HO-EXTRA-CR                PIC SV99    COMP-3.
TAPCO0         10  MRESII-HO-EXTRA-FULL              PIC S999V99 COMP-3.
TAPCO0         10  MRESII-HO-EXTRA-ONS               PIC S999V99 COMP-3.
TAPCO0         10  MRESII-HO-EXTRA-OFF               PIC S999V99 COMP-3.

TAPCO0         10  MRESII-NHSP-IND                   PIC X.
TAPCO0         10  MRESII-NHSP-CR                    PIC SV99    COMP-3.
TAPCO0         10  MRESII-NHSP-FULL                  PIC S999V99 COMP-3.
TAPCO0         10  MRESII-NHSP-ONS                   PIC S999V99 COMP-3.
TAPCO0         10  MRESII-NHSP-OFF                   PIC S999V99 COMP-3.
SRMSRM         10  MFILLER-58                        PIC X(7).          37900006

TAPCO0     05  M-COMMISSION-RATE-AREA.
TAPCO0         10  M-CMSN-POL-CTRL-RT                PIC SV9(5)  COMP-3.
TAPCO0         10  M-CMSN-POL-CSGN-RT                PIC SV9(5)  COMP-3.
TAPCO0         10  M-CMSN-ENDT-RATE-TABLE.
TAPCO0             15  M-CMSN-ENDT-RATES             OCCURS 22 TIMES
TAPCO0                                               INDEXED BY
TAPCO0                                               M-CMSN-ENDT-IDX.
TAPCO0                 20  M-CMSN-ENDT-CTRL-RT       PIC SV9(5)  COMP-3.
TAPCO0                 20  M-CMSN-ENDT-CSGN-RT       PIC SV9(5)  COMP-3.
TAPCO0         10  M-CMSN-TAP-IND                    PIC X.
TAPCO0         10  M-CMSN-OVRR-RT                    PIC SV9(5)  COMP-3.

TAPCO0     05  M-WAIVER-PREMIUM                                  COMP-3.
TAPCO0         10  M-WAIVER-PREM-TOTAL-POLICY        PIC S9(5)V99.
TAPCO0         10  M-WAIVER-PREM-BASIC-COVG          PIC S9(5)V99.
TAPCO0         10  M-WAIVER-PREM-SEC-RES             PIC S9(5)V99.
TAPCO0         10  M-WAIVER-PREM-INCR-UNS-PROP       PIC S9(5)V99.
TAPCO0         10  M-WAIVER-PREM-INCR-ADDL-EXP       PIC S9(5)V99.
TAPCO0         10  M-WAIVER-PREM-INCR-LIAB           PIC S9(5)V99.
TAPCO0         10  M-WAIVER-PREM-RES-EMPLS           PIC S9(5)V99.
TAPCO0         10  M-WAIVER-SURCHARGE-AMT            PIC S9(3)V99.
TAPCO0         10  M-WAIVER-RECOUP-AMT               PIC S9(3)V99.
TAPCO0         10  M-WAIVER-NEW-RECOUP-AMT           PIC S9(3)V99.
TAPCO0         10  M-WAIVER-CAT-SURCH-AMT            PIC S9(3)V99.
728020         10  M-WAIVER-FL-CAT-FUND-AMT          REDEFINES
728020             M-WAIVER-CAT-SURCH-AMT            PIC S9(5).
TAPCO0         10  M-WAIVER-PREM-FAIR-PLAN           PIC S9(5)V99.
TAPCO0         10  M-WAIVER-LOSS-SURCHARGE           PIC S9(5)V99.
TAPCO0         10  M-WAIVER-NEW-HOME                 PIC S9(5)V99.
TAPCO0         10  M-WAIVER-RENEWAL-CREDITS          PIC S9(3)V99.
TAPCO0         10  M-WAIVER-CROSS-CREDIT             PIC S9(5)V99.
TAPCO0         10  M-WAIVER-MAX-CREDIT               PIC S9(3)V99.
TAPCO0         10  M-WAIVER-100-DED-PREM             PIC S9(5)V99.
TAPCO0         10  M-WAIVER-PREM-912                 PIC S9(5)V99.
TAPCO0         10  M-WAIVER-PREM-913                 PIC S9(5)V99.
TAPCO0         10  M-WAIVER-PREM-915                 PIC S9(5)V99.
TAPCO0         10  M-WAIVER-WOOD-STOVE               PIC S9(3)V99.
TAPCO0         10  M-WAIVER-NON-SMOKER               PIC S9(3)V99.
TAPCO0         10  M-WAIVER-PRIME-TIME               PIC S9(3)V99.
TAPCO0         10  M-WAIVER-PROT-DEV                 PIC S9(5)V99.
TAPCO0         10  M-WAIVER-HO-EXTRA-CREDIT          PIC S9(3)V99.
TAPCO0         10  M-WAIVER-NHSP-CREDIT              PIC S9(3)V99.
TAPCO0         10  M-WAIVER-NET-PREM                 PIC S9(5)V99.
TAPCO0         10  M-WAIVER-NET-TAX                  PIC S9(5)V99.
TAPCO0         10  M-WAIVER-NET-COMM                 PIC S9(5)V99.

TAPCO0         10  M-WAIVER-COV-ENDT-AREA       OCCURS  22  TIMES
TAPCO0                                          INDEXED BY
TAPCO0                                          M-WAIVER-ENDT-COV-INDEX.
TAPCO0             15 M-WAIVER-COV-ENDT-PREMIUM PIC S9(5)V99.

TAPCO0         10  M-WAIVER-MVIP-DATA           OCCURS  23  TIMES
TAPCO0                                          INDEXED BY
TAPCO0                                          M-WAIVER-MVIP-INDEX.
TAPCO0             15 M-WAIVER-MVIP-PREM        PIC S9(5)V99.

TAPCO0         10  M-WAIVER-J-0-GP                    PIC S9(5)V99.
TAPCO0         10  M-WAIVER-J-0-NGP                   PIC S9(5)V99.
TAPCO0         10  M-WAIVER-J-1-GP                    PIC S9(5)V99.
TAPCO0         10  M-WAIVER-J-1-NGP                   PIC S9(5)V99.
TAPCO0         10  M-WAIVER-J-2-GP                    PIC S9(5)V99.
TAPCO0         10  M-WAIVER-J-2-NGP                   PIC S9(5)V99.
TAPCO0         10  M-WAIVER-J-3-GP                    PIC S9(5)V99.
TAPCO0         10  M-WAIVER-J-3-NGP                   PIC S9(5)V99.
TAPCO0         10  M-WAIVER-FURS-0                    PIC S9(5)V99.
TAPCO0         10  M-WAIVER-FURS-1                    PIC S9(5)V99.
SRMSRM     05  MFILLER-59                             PIC X(4).         38580006

TAPCO0     05  M-OTHER-INFO.
TAPCO0         10  M-COMPANY-ALPHA-CODE-OFF           PIC X(3).
TAPCO0         10  M-PRICE-OFF                        PIC X(5).
TAPCO0         10  MORS-HHM-SEQ-NUM                   PIC X(3).
TAPCO0         10  M-AGENCY-CDC                       PIC X(4).
SRMSRM         10  MFILLER-60                         PIC X(15).        38650006

TAPCO0     05  MVI-SCHEDULED-ENDT-INFO.
TAPCO0         10  MVI-TEMP-POL-NUM                   PIC 9(10).
SRMSRM         10  MFILLER-61                         PIC X(9).         38690006
TAPCO0         10  MVI-SCHED-FLDS                                COMP-3.
TAPCO0             15  MVI-RATE-TERR-CD               PIC S9(3).
TAPCO0             15  MVI-TERR-CODE                  PIC S9(3).
TAPCO0             15  MVI-TOTAL-AMT-INS              PIC S9(7).
TAPCO0             15  MVI-TOTAL-FULL-TERM-PREM       PIC S9(5)V99.
TAPCO0             15  MVI-TOTAL-ONSET-PREM           PIC S9(5)V99.
TAPCO0             15  MVI-TOTAL-OFFSET-PREM          PIC S9(5)V99.
TAPCO0             15  MVI-NEXT-FILE-REVIEW-DATE      PIC S9(9).
TAPCO0         10  MVI-SCHED-PRINT-IND                PIC X.
TAPCO0             88  MVI-PRINT-SCHED                    VALUE 'Y'.
TAPCO0             88  MVI-DO-NOT-PRINT-SCHED             VALUE 'N'.
TAPCO0         10  MVI-TAP-VIP-IND                    PIC X.
TAPCO0             88  M-TAP-VI-COV                       VALUE 'Y'.
TAPCO0         10  MVI-RE-UPGRADE-AMT                 PIC S9(7)  COMP-3.
TAPCO0         10  MVI-VI-CHG-DATE                    PIC S9(5)  COMP-3.
SRMSRM         10  MFILLER-62                         PIC X(22).        38850006

TAPCO0      07 MVI-ENDORSEMENT-INFO.
TAPCO0         10  MVI-ENDT-DATA                      OCCURS 10 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                MVI-ENDT-INDEX.
TAPCO0             15  MVI-ENDT-ACTION-TYPE           PIC X.
TAPCO0             15  MVI-ENDT-NUM                   PIC X(9).
TAPCO0             15  MVI-ENDT-EDITION-DT            PIC X(4).

TAPCO0      07 MVI-COVERAGE-INFO.
TAPCO0         10  MVI-SCHD-STAMPS-SAFE-CR-IND        PIC X.
TAPCO0             88  MVI-SCHD-STAMPS-YES-SAFE-CR        VALUE 'Y'.
TAPCO0             88  MVI-SCHD-STAMPS-NO-SAFE-CR         VALUE 'N'.
TAPCO0         10  MVI-BLKT-STAMPS-SAFE-CR-IND        PIC X.
TAPCO0             88  MVI-BLKT-STAMPS-YES-SAFE-CR        VALUE 'Y'.
TAPCO0             88  MVI-BLKT-STAMPS-NO-SAFE-CR         VALUE 'N'.
TAPCO0         10  MVI-SCHD-COINS-SAFE-CR-IND         PIC X.
TAPCO0             88  MVI-SCHD-COINS-YES-SAFE-CR         VALUE 'Y'.
TAPCO0             88  MVI-SCHD-COINS-NO-SAFE-CR          VALUE 'N'.
TAPCO0         10  MVI-BLKT-COINS-SAFE-CR-IND         PIC X.
TAPCO0             88  MVI-BLKT-COINS-YES-SAFE-CR         VALUE 'Y'.
TAPCO0             88  MVI-BLKT-COINS-NO-SAFE-CR          VALUE 'N'.
TAPCO0         10  MVI-JEWELRY-UPGRADE-IND            PIC X.
TAPCO0             88  MVI-JEWELRY-UPGRADE-ACCEPT         VALUE 'Y'.
TAPCO0             88  MVI-JEWELRY-UPGRADE-REJECT         VALUE 'N'.

TAPCO0         10  MVI-SCHD-JEWELRY-CLASS-INFO.
TAPCO0             15  MVI-SCHD-JEWELRY-COV-FORM-CD   PIC XX.
TAPCO0             15  MVI-SCHD-JEWELRY-ACTION-TYPE   PIC X.
TAPCO0             15  MVI-SCHD-JEWELRY-RVN           PIC S9(3)  COMP-3.
TAPCO0             15  MVI-SCHD-JEWELRY-CLASS-CODE    PIC S9(5)  COMP-3.
TAPCO0             15  MVI-SCHD-JEWELRY-TOT-AMT-INS   PIC S9(7)  COMP-3.
TAPCO0             15  MVI-SCHD-JEWELRY-TOT-FULL-PREM PIC S9(5)V99
TAPCO0                                                           COMP-3.

TAPCO0             15  MVI-JEWELRY-SRCHG-0-INFO                  COMP-3.
TAPCO0                 20  MVI-J-0-GP-AMT-INS         PIC S9(7).
TAPCO0                 20  MVI-J-0-GP-FULL-TERM-PREM  PIC S9(5)V99.
TAPCO0                 20  MVI-J-0-GP-ONSET-PREM      PIC S9(5)V99.
TAPCO0                 20  MVI-J-0-GP-OFFSET-PREM     PIC S9(5)V99.
TAPCO0                 20  MVI-J-0-NGP-AMT-INS        PIC S9(7).
TAPCO0                 20  MVI-J-0-NGP-FULL-TERM-PREM PIC S9(5)V99.
TAPCO0                 20  MVI-J-0-NGP-ONSET-PREM     PIC S9(5)V99.
TAPCO0                 20  MVI-J-0-NGP-OFFSET-PREM    PIC S9(5)V99.

TAPCO0             15  MVI-JEWELRY-SRCHG-1-INFO                  COMP-3.
TAPCO0                 20  MVI-J-1-GP-AMT-INS         PIC S9(7).
TAPCO0                 20  MVI-J-1-GP-FULL-TERM-PREM  PIC S9(5)V99.
TAPCO0                 20  MVI-J-1-GP-ONSET-PREM      PIC S9(5)V99.
TAPCO0                 20  MVI-J-1-GP-OFFSET-PREM     PIC S9(5)V99.
TAPCO0                 20  MVI-J-1-NGP-AMT-INS        PIC S9(7).
TAPCO0                 20  MVI-J-1-NGP-FULL-TERM-PREM PIC S9(5)V99.
TAPCO0                 20  MVI-J-1-NGP-ONSET-PREM     PIC S9(5)V99.
TAPCO0                 20  MVI-J-1-NGP-OFFSET-PREM    PIC S9(5)V99.

TAPCO0             15  MVI-JEWELRY-SRCHG-2-INFO                  COMP-3.
TAPCO0                 20  MVI-J-2-GP-AMT-INS         PIC S9(7).
TAPCO0                 20  MVI-J-2-GP-FULL-TERM-PREM  PIC S9(5)V99.
TAPCO0                 20  MVI-J-2-GP-ONSET-PREM      PIC S9(5)V99.
TAPCO0                 20  MVI-J-2-GP-OFFSET-PREM     PIC S9(5)V99.
TAPCO0                 20  MVI-J-2-NGP-AMT-INS        PIC S9(7).
TAPCO0                 20  MVI-J-2-NGP-FULL-TERM-PREM PIC S9(5)V99.
TAPCO0                 20  MVI-J-2-NGP-ONSET-PREM     PIC S9(5)V99.
TAPCO0                 20  MVI-J-2-NGP-OFFSET-PREM    PIC S9(5)V99.

TAPCO0             15  MVI-JEWELRY-SRCHG-3-INFO                  COMP-3.
TAPCO0                 20  MVI-J-3-GP-AMT-INS         PIC S9(7).
TAPCO0                 20  MVI-J-3-GP-FULL-TERM-PREM  PIC S9(5)V99.
TAPCO0                 20  MVI-J-3-GP-ONSET-PREM      PIC S9(5)V99.
TAPCO0                 20  MVI-J-3-GP-OFFSET-PREM     PIC S9(5)V99.
TAPCO0                 20  MVI-J-3-NGP-AMT-INS        PIC S9(7).
TAPCO0                 20  MVI-J-3-NGP-FULL-TERM-PREM PIC S9(5)V99.
TAPCO0                 20  MVI-J-3-NGP-ONSET-PREM     PIC S9(5)V99.
TAPCO0                 20  MVI-J-3-NGP-OFFSET-PREM    PIC S9(5)V99.

TAPCO0         10  MVI-SCHD-FURS-CLASS-INFO.
TAPCO0             15  MVI-SCHD-FURS-COV-FORM-CD      PIC XX.
TAPCO0             15  MVI-SCHD-FURS-ACTION-TYPE      PIC X.
TAPCO0             15  MVI-SCHD-FURS-RVN              PIC S9(3)  COMP-3.
TAPCO0             15  MVI-SCHD-FURS-CLASS-CODE       PIC S9(5)  COMP-3.
TAPCO0             15  MVI-SCHD-FURS-TOT-AMT-INS      PIC S9(7)  COMP-3.
TAPCO0             15  MVI-SCHD-FURS-TOT-FULL-PREM    PIC S9(5)V99
TAPCO0                                                           COMP-3.

TAPCO0             15  MVI-FURS-SRCHG-0-INFO                     COMP-3.
TAPCO0                 20  MVI-F-0-AMT-INS            PIC S9(7).
TAPCO0                 20  MVI-F-0-FULL-TERM-PREM     PIC S9(5)V99.
TAPCO0                 20  MVI-F-0-ONSET-PREM         PIC S9(5)V99.
TAPCO0                 20  MVI-F-0-OFFSET-PREM        PIC S9(5)V99.

TAPCO0             15  MVI-FURS-SRCHG-1-INFO                     COMP-3.
TAPCO0                 20  MVI-F-1-AMT-INS            PIC S9(7).
TAPCO0                 20  MVI-F-1-FULL-TERM-PREM     PIC S9(5)V99.
TAPCO0                 20  MVI-F-1-ONSET-PREM         PIC S9(5)V99.
TAPCO0                 20  MVI-F-1-OFFSET-PREM        PIC S9(5)V99.

TAPCO0         10  MVI-OTHER-COV-INFO.
TAPCO0             15  MVI-COV-DATA                   OCCURS 23 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                MVI-COV-INDEX.
TAPCO0                 20  MVI-COV-FORM-SCHD.
TAPCO0                     25  MVI-COV-FORM-CD        PIC XX.
TAPCO0                     25  MVI-COV-BLKT-SCHD-IND  PIC X.
TAPCO0                         88  MVI-COV-BLKT           VALUE 'U'.
TAPCO0                         88  MVI-COV-SCHD           VALUE 'S'.
TAPCO0                 20  MVI-COV-ACTION-TYPE        PIC X.
TAPCO0                 20  MVI-COV-FLDS                          COMP-3.
TAPCO0                     25  MVI-COV-RVN            PIC S9(3).
TAPCO0                     25  MVI-COV-CLASS-CODE     PIC S9(5).
TAPCO0                     25  MVI-COV-AMT-INS        PIC S9(7).
TAPCO0                     25  MVI-COV-FULL-TERM-PREM PIC S9(5)V99.
TAPCO0                     25  MVI-COV-ONSET-PREM     PIC S9(5)V99.
TAPCO0                     25  MVI-COV-OFFSET-PREM    PIC S9(5)V99.

TAPCO0      07 MVI-ITEM-INFO.
TAPCO0         10  MVI-ITEM-DATA                      OCCURS 50 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                MVI-ITEM-INDEX.
TAPCO0             15  MVI-ITEM-ACTION-TYPE           PIC X.
TAPCO0             15  MVI-ITEM-COV-FORM-CD           PIC XX.
TAPCO0             15  MVI-ITEM-NO                    PIC S9(5)  COMP-3.
TAPCO0             15  MVI-ITEM-SRCHG-IND             PIC X.
TAPCO0                 88  MVI-0-ITEM                     VALUE '0'.
TAPCO0                 88  MVI-1-SRCHG-ITEM               VALUE '1'.
TAPCO0                 88  MVI-2-SRCHG-ITEM               VALUE '2'.
TAPCO0                 88  MVI-3-VLT-ITEM                 VALUE '3'.
TAPCO0             15  MVI-ITEM-VALUE                 PIC S9(7)  COMP-3.
TAPCO0             15  MVI-ITEM-MISC-PREM             PIC S9(5)  COMP-3.
TAPCO0             15  MVI-ITEM-APRSL-DT              PIC S9(7)  COMP-3.
TAPCO0             15  MVI-ITEM-GEMPRINT              PIC X.
TAPCO0                 88  MVI-ITEM-IS-GEMPRINT           VALUE 'Y'.
TAPCO0                 88  MVI-ITEM-NOT-GEMPRINT          VALUE 'N'.
TAPCO0             15  MVI-ITEM-VAULT-ENDT            PIC X.
TAPCO0                 88  MVI-ITEM-IN-VAULT              VALUE 'Y'.
TAPCO0                 88  MVI-ITEM-NOT-IN-VAULT          VALUE 'N'.
TAPCO0             15  MVI-ITEM-PAIR-SET-ENDT         PIC X.
TAPCO0                 88  MVI-ITEM-IS-PAIR-SET           VALUE 'Y'.
TAPCO0                 88  MVI-ITEM-NOT-PAIR-SET          VALUE 'N'.
TAPCO0             15  MVI-ITEM-BREAKAGE-ENDT         PIC X.
TAPCO0                 88  MVI-ITEM-HAS-BREAKAGE          VALUE 'Y'.
TAPCO0                 88  MVI-ITEM-NO-BREAKAGE           VALUE 'N'.
SRMSRM      07 MFILLER-63                             PIC X(91).        40270006

TAPCO0     05  M-NON-HISTORIC-COMM-RATES.
TAPCO0         10  M-NON-HISTORIC-COMM-RATE-TBL       OCCURS 20 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                MNON-CMSN-IDX1.
TAPCO0             15  MNON-CMSN-RATE                 PIC SV9(5) COMP-3.

TAPCO0     05  M-CMSN-OFFSET-RATE-AREA.
TAPCO0         10  M-CMSN-POL-CTRL-RT-OFF             PIC SV9(5) COMP-3.
TAPCO0         10  M-CMSN-POL-CSGN-RT-OFF             PIC SV9(5) COMP-3.
TAPCO0         10  M-CMSN-ENDT-RATE-TABLE-OFF.
TAPCO0             15  M-CMSN-ENDT-RATES-OFF          OCCURS 22 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                M-CMSN-ENDT-IDX2.
TAPCO0                 20  M-CMSN-ENDT-CTRL-RT-OFF    PIC SV9(5) COMP-3.
TAPCO0                 20  M-CMSN-ENDT-CSGN-RT-OFF    PIC SV9(5) COMP-3.

TAPCO0     05  MVI-LAST-ITEM-DATA-TABLE.
TAPCO0         10  MVI-LAST-ITEM-DATA                 OCCURS 32 TIMES
TAPCO0                                                INDEXED BY
TAPCO0                                                MVI-LAST-ITEM-IDX.
TAPCO0             15  MVI-HOLD-COV                   PIC XX.
TAPCO0             15  MVI-LAST-ITEM-NUM              PIC S9(5)  COMP-3.

TAPCO0     05  M-CONVB-PA-RENO-IND                    PIC X.
TEXPPC     05  M-RATING-PPC                           PIC X(4).
TEXPPC     05  M-RATING-PROT-CLASS                    PIC S9(3) COMP-3.
TEXPPC     05  M-RATING-FD                            PIC X(15).
728020     05  M-FL-CAT-FUND-SURCH-EXP-SW             PIC X.
728020         88  5-BYTE-CAT-SURCH                   VALUE '1'.
NYTPD*     05  M-THIRD-PARTY-DESIGNEE-SEC.
NYTPD*         10  M-TPD-ACTION-TYPE                  PIC X.
NYTPD*             88  M-TPD-INITIAL-VALUE                VALUE ' '.
NYTPD*             88  M-TPD-ADD-ONLY                     VALUE 'A'.
NYTPD*             88  M-TPD-CHANGE-ONLY                  VALUE 'C'.
NYTPD*             88  M-TPD-ELIMINATE-ONLY               VALUE 'E'.
NYTPD*         10  M-THIRD-PARTY-NAME-1              PIC X(35).
NYTPD*         10  M-THIRD-PARTY-NAME-2              PIC X(35).
NYTPD*         10  M-THIRD-PARTY-ADDRESS.                               D6H10740
NYTPD*             15  M-THIRD-PARTY-ADDR-1          PIC X(30).
NYTPD*             15  M-THIRD-PARTY-ADDR-2          PIC X(30).
962190             15  M-THIRD-FILLER-1 REDEFINES
962190                                  M-THIRD-PARTY-ADDR-2.
962190                 20  M-THIRD-PARTY-ADDR-2-CITY  PIC X(27).
962190                 20  M-THIRD-FILLER-2           PIC X(01).
962190                 20  M-THIRD-PARTY-ADDR-2-STATE PIC X(02).
NYTPD*             15  M-THIRD-PARTY-ZIP-CODE        PIC S9(5)  COMP-3.
NYTPD*         10  M-THIRD-PARTY-ON-POLICY           PIC X.
GACCR      05  M-GEICO-SP-CREDIT-PCT                 PIC SV999  COMP-3.
TEXINT     05  M-PREM-LEVEL-FACTOR                   PIC 9V99.
CITIBK     05  M-CB-AREA.
CITIBK         10  M-CB-CREDIT-CARD-NO               PIC X(16).
CITIBK         10  M-CB-EXPIRATION-DATE              PIC X(4).
792550     05  M-REPLACE-COST-RECOVERY-SW            PIC X.
790230     05  M-TX-ROOF-STATS.
790230         10  M-TX-ROOF-ONS                  PIC X.
790230         10  M-TX-ROOF-OFF                  PIC X.
790230     05  M-UL-CLASS-CODE-STATS.
790230         10  M-UL-CLASS-CODE-ONS            PIC X.
790230         10  M-UL-CLASS-CODE-OFF            PIC X.
DLCDEC     05  M-HVH-SEL-IND                      PIC X.
               88  M-HVH-SEL                       VALUE 'N' 'A'
                                                     'B' 'C' 'D'
                                                     'E' 'F' 'G'.
               88  M-VAL-HVH-SEL                   VALUE 'N' 'A'
                                                     'C' 'D' 'F'
                                                     'G'.
814860     05  M-TX-INLAND-CAT-SW                  PIC X.
823370     05  M-MIN-PREM-INFO                                 COMP-3.
823370         10  M-MIN-PREM-POLICY-ADD         PIC S9(3)V99.
823370         10  M-MIN-PREM-ONSET              PIC S9(3)V99.
823370         10  M-MIN-PREM-OFFSET             PIC S9(3)V99.
830570     05  M-DOWN-PAY-IND                      PIC X.
824800     05  M-BEEN-TO-KBMS                      PIC X.
836310     05  M-NY-THIRD-YEAR-SW                  PIC X.
836310         88  M-NY-THIRD-YEAR-YES             VALUE 'Y'.
836310         88  M-NY-THIRD-YEAR-NO              VALUE 'N'.
836310     05  M-PLUS-TERR-REDEF-SW                PIC X.
836310         88  M-PLUS-TERR-REDEF-APPLIED       VALUE 'Y'.
836810     05  M-PLUS-TERR-REDEF-DT-APPLIED        PIC S9(5)  COMP-3.
857870     05  M-ITV-RECOVERY-SW                   PIC X.
856790     05  M-TX-WH-NEW-CALC-FOR-CREDITS        PIC X.
891360     05  M-GEICO-OUTRPT-SW                   PIC X.
886370     05  M-CALIFORNIA-TAP-TIER-SW            PIC X.
886370         88 M-CA-TAP-TIER-FIRST-TIME         VALUE '1'.
886370         88 M-CA-TAP-TIER                    VALUE 'Y' '1'.


936330     05  M-ROOF-SURCHARGE-FULL                 PIC S999V99.       41080006
936330     05  M-ROOF-SURCHARGE-ONS                  PIC S999V99.       41080006
936330     05  M-ROOF-SURCHARGE-OFF                  PIC S999V99.       41080006
936330     05  M-ROOF-SURCHARGE-PERC                 PIC S99V9(4).      41080006
936330     05  M-ROOF-SURCHARGE-AMT                  PIC S999V99.       41080006
936330     05  M-ROOF-SUR-IND                        PIC X.
936330         88 M-ROOF-SUR-YES                     VALUE 'Y'.
936330*
936330     05  M-TX-UL-CLASS-CODE                    PIC X.
936330     05  M-TX-ROOF-CRED-AMT                    PIC S999V99.
SCHURR     05  M-WENT-THROUGH-SC-HURR-DEDUCT         PIC X.
936330     05  M-TX-ROOF-PERC                        PIC S99V9(4).      41080006

940530     05  MCOSTMTR-TOTAL-LIVING-AREA          PIC S9(5).
940530     05  MCOSTMTR-POINTS                     PIC S9(2).
940530     05  MCOSTMTR-NUMBER-PORCHES             PIC S9(2).
940530     05  MCOSTMTR-NUMBER-BREEZEWAYS          PIC S9(2).
940530     05  MCOSTMTR-NUMBER-DECKS               PIC S9(2).
940530     05  MCOSTMTR-SPECIAL-ROOM-COUNT         PIC S9(2).
940530     05  MCOSTMTR-COV-A                      PIC S9(7).
940530     05  M-ATLAS-COSTIMATOR-IND              PIC X.
946110     05  M-EQ-RETROFIT-IND                   PIC X.
KAHKAH     05  M-RVN-EFFECTIVE-DATE                PIC 9(7).
KAHKAH     05  M-CURR-TIMESTAMP                    PIC X(26).
954040     05  M-TX-CERT-DEADBOLT-PCT              PIC S9V99 COMP-3.
954040     05  M-TX-CERT-DEADBOLT-AMT              PIC S9(5)V99 COMP-3.
954040     05  M-TX-CERT-DEADBOLT-AMT-ONS          PIC S9(5)V99 COMP-3.
954040     05  M-TX-CERT-DEADBOLT-AMT-OFF          PIC S9(5)V99 COMP-3.
954040     05  M-TX-CERT-DEADBOLT-EFF-DATE         PIC S9(5) COMP-3.
954040     05  M-TX-CERT-CENT-BURG-PCT             PIC S9V99 COMP-3.
954040     05  M-TX-CERT-CENT-BURG-AMT             PIC S9(5)V99 COMP-3.
954040     05  M-TX-CERT-CENT-BURG-AMT-ONS         PIC S9(5)V99 COMP-3.
954040     05  M-TX-CERT-CENT-BURG-AMT-OFF         PIC S9(5)V99 COMP-3.
954040     05  M-TX-CERT-CENT-BURG-EFF-DATE        PIC S9(5) COMP-3.
957580     05  MEND-TX-BUILD-ORD-PERC-LIM-ONS      PIC S9(2).
957580     05  MEND-TX-BUILD-ORD-PERC-LIM-OFF      PIC S9(2).
IDFRAD     05  M-ID-FRAUD-ORIGINATION-IND          PIC X.

ELGIBL     05  M-ELIGIBILITY-SEC.
ELGIBL*
ELGIBL         10  MELG-RESIDENCE-INFO.
ELGIBL             15  MELG-PRIMARY-HEAT-SOURCE       PIC X.
ELGIBL                 88  MELG-PRIMARY-HEAT-SOURCE-OIL   VALUE '1'.
ELGIBL                 88  MELG-PRIMARY-HEAT-SOURCE-GAS   VALUE '2'.
ELGIBL                 88  MELG-PRIMARY-HEAT-SOURCE-ELEC  VALUE '3'.
ELGIBL                 88  MELG-PRIMARY-HEAT-SOURCE-OTHER VALUE '4'.
ELGIBL                 88  MELG-PRIMARY-HEAT-SOURCE-NONE  VALUE '9'.
ELGIBL             15  MELG-HEAT-LAST-SRVC            PIC X.
ELGIBL                 88  MELG-HEAT-LAST-SRVC-18-MNTHS   VALUE '0'.
ELGIBL                 88  MELG-HEAT-LAST-SRVC-60-MNTHS   VALUE '1'.
ELGIBL                 88  MELG-HEAT-LAST-SRVC-5-YRS      VALUE '5'.
ELGIBL             15  MELG-ELEC-NUM-OF-AMPS          PIC X.
ELGIBL                 88  MELG-ELEC-NUM-OF-AMPS-LT-100   VALUE 'U'.
ELGIBL                 88  MELG-ELEC-NUM-OF-AMPS-GT-100   VALUE 'O'.
ELGIBL             15  MELG-ELEC-CIRCUIT-BRKRS-IND    PIC X.
ELGIBL                 88  MELG-ELEC-CIRCUIT-BRKRS        VALUE 'Y'.
ELGIBL                 88  MELG-NO-ELEC-CIRCUIT-BRKRS     VALUE 'N'.
ELGIBL             15  MELG-ELEC-KNOB-TUBE-WIRING-IND PIC X.
ELGIBL                 88  MELG-ELEC-KNOB-TUBE-WIRING     VALUE 'Y'.
ELGIBL                 88  MELG-NO-ELEC-KNOB-TUBE-WIRING  VALUE 'N'.
ELGIBL             15  MELG-ELEC-ALUM-WIRING-IND      PIC X.
ELGIBL                 88  MELG-ELEC-ALUM-WIRING          VALUE 'Y'.
ELGIBL                 88  MELG-NO-ELEC-ALUM-WIRING       VALUE 'N'.
ELGIBL             15  MELG-UNDERGROUND-FUEL-TANK-IND PIC X.
ELGIBL                 88  MELG-UNDERGROUND-FUEL-TANK     VALUE 'Y'.
ELGIBL                 88  MELG-NO-UNDERGROUND-FUEL-TANK  VALUE 'N'.
ELGIBL             15  MELG-ROOF-CONDITION-IND        PIC X.
ELGIBL                 88  MELG-ROOF-CONDITION-GOOD       VALUE 'Y'.
ELGIBL                 88  MELG-ROOF-CONDITION-BAD        VALUE 'N'.
ELGIBL             15  MELG-LEAD-PAINT-INSPECT-IND    PIC X.
ELGIBL                 88  MELG-LEAD-PAINT-INSPECT        VALUE 'Y'.
ELGIBL                 88  MELG-NO-LEAD-PAINT-INSPECT     VALUE 'N'.
ELGIBL                 88  MELG-LEAD-PAINT-INSPECT-UNKNWN VALUE 'U'.
ELGIBL             15  MELG-FOUNDATION                PIC X.
ELGIBL                 88  MELG-FOUNDATION-OPEN           VALUE 'O'.
ELGIBL                 88  MELG-FOUNDATION-CLOSED         VALUE 'C'.
ELGIBL                 88  MELG-FOUNDATION-NONE           VALUE 'N'.
ELGIBL             15  MELG-UNDER-CONSTR-IND          PIC X.
ELGIBL                 88  MELG-UNDER-CONSTR              VALUE 'Y'.
ELGIBL                 88  MELG-NOT-UNDER-CONSTR          VALUE 'N'.
ELGIBL             15  MELG-NUM-DAYS-COMPL-CONSTR     PIC 999.
ELGIBL             15  MELG-INSURED-IS-CONTRACTOR-IND PIC X.
ELGIBL                 88  MELG-INSURED-IS-CONTRACTOR     VALUE 'Y'.
ELGIBL                 88  MELG-INSURED-IS-NOT-CONTRACTOR VALUE 'N' ' '.
ELGIBL             15  MELG-PLUMBING-CONDITION        PIC X.
ELGIBL                 88  MELG-PLUMBING-CONDITION-GOOD   VALUE 'Y'.
ELGIBL                 88  MELG-PLUMBING-CONDITION-BAD    VALUE 'N'.
ELGIBL             15  MELG-OCCUPANCY-STATUS          PIC X.
ELGIBL                 88  MELG-OCCUPANCY-STATUS-CHANGE   VALUE 'Y'.
ELGIBL                 88  MELG-OCCUPANCY-STATUS-NO-CHG   VALUE 'N'.
ELGIBL             15  MELG-OCC-STATUS-EXPLAIN        PIC X(50).
ELGIBL             15  MELG-FILLER-1                  PIC X(17).
ELGIBL*
ELGIBL         10  MELG-PROPERTY-INFO.
ELGIBL             15  MELG-SWIM-POOL-TYPE-IND        PIC X.
ELGIBL                 88  MELG-INGROUND-SWIM-POOL        VALUE 'I'.
ELGIBL                 88  MELG-ABOVE-GROUND-SWIM-POOL    VALUE 'A'.
ELGIBL             15  MELG-SWIM-POOL-FENCED-IND      PIC X.
ELGIBL                 88  MELG-SWIM-POOL-FENCED          VALUE 'Y'.
ELGIBL                 88  MELG-SWIM-POOL-NOT-FENCED      VALUE 'N'.
ELGIBL             15  MELG-SWIM-POOL-SLIDE-DIVE-IND  PIC X.
ELGIBL                 88  MELG-SWIM-POOL-SLIDE-DIVE      VALUE 'Y'.
ELGIBL                 88  MELG-NO-SWIM-POOL-SLIDE-DIVE   VALUE 'N'.
ELGIBL             15  MELG-BUSINESS-ON-PREMISES-IND  PIC X.
ELGIBL                 88  MELG-BUSINESS-ON-PREMISES      VALUE 'Y'.
ELGIBL                 88  MELG-NO-BUSINESS-ON-PREMISES   VALUE 'N'.
ELGIBL             15  MELG-BUS-ON-PREM-EXPLAIN       PIC X(50).
ELGIBL             15  MELG-BANKRUPT-LAST-5-YRS-IND   PIC X.
ELGIBL                 88  MELG-BANKRUPT-LAST-5-YRS       VALUE 'Y'.
ELGIBL                 88  MELG-NOT-BANKRUPT-LAST-5-YRS   VALUE 'N'.
ELGIBL             15  MELG-TRAMP-SKTBRD-BIKE-HAZ-IND PIC X.
ELGIBL                 88  MELG-TRAMP-SKTBRD-BIKE-HAZ     VALUE 'Y'.
ELGIBL                 88  MELG-NO-TRAMP-SKTBRD-BIKE-HAZ  VALUE 'N'.
ELGIBL             15  MELG-OWN-VICIOUS-ANIMAL-IND    PIC X.
ELGIBL                 88  MELG-OWN-VICIOUS-ANIMAL        VALUE 'Y'.
ELGIBL                 88  MELG-NOT-OWN-VICIOUS-ANIMAL    VALUE 'N'.
ELGIBL             15  MELG-VICIOUS-ANIMAL-EXPLAIN    PIC X(50).
ELGIBL             15  MELG-OWN-DOG-IND               PIC X.
ELGIBL                 88  MELG-OWN-DOG                   VALUE 'Y'.
ELGIBL                 88  MELG-DOES-NOT-OWN-DOG          VALUE 'N'.
ELGIBL             15  MELG-DOG-BREED                 PIC X.
ELGIBL                 88  MELG-DOG-BREED-AKITA           VALUE 'A'.
ELGIBL                 88  MELG-DOG-BREED-MALAMUTE        VALUE 'B'.
ELGIBL                 88  MELG-DOG-BREED-AM-STSH-TERRIER VALUE 'C'.
ELGIBL                 88  MELG-DOG-BREED-CHOW            VALUE 'D'.
ELGIBL                 88  MELG-DOG-BREED-DOBERMAN        VALUE 'E'.
ELGIBL                 88  MELG-DOG-BREED-GREAT-DANE      VALUE 'F'.
ELGIBL                 88  MELG-DOG-BREED-PIT-BULL        VALUE 'G'.
ELGIBL                 88  MELG-DOG-BREED-HUSKY           VALUE 'H'.
ELGIBL                 88  MELG-DOG-BREED-BULL-TERRIER    VALUE 'I'.
ELGIBL                 88  MELG-DOG-BREED-ROTTWEILER      VALUE 'J'.
ELGIBL                 88  MELG-DOG-BREED-WOLF-HYBRID     VALUE 'K'.
ELGIBL                 88  MELG-DOG-BREED-MIX-OF-ABOVE    VALUE 'L'.
ELGIBL                 88  MELG-DOG-BREED-MIN-PINSCHER    VALUE 'M'.
ELGIBL                 88  MELG-DOG-BREED-OTHER           VALUE 'Z'.
ELGIBL             15  MELG-SUBJECT-FLOOD-WAVES-IND   PIC X.
ELGIBL                 88  MELG-SUBJECT-FLOOD-WAVES       VALUE 'Y'.
ELGIBL                 88  MELG-NOT-SUBJECT-FLOOD-WAVES   VALUE 'N'.
ELGIBL             15  MELG-RENT-WEEKLY-GT-3-MTHS-IND PIC X.
ELGIBL                 88  MELG-RENT-WEEKLY-GT-3-MTHS     VALUE 'Y'.
ELGIBL                 88  MELG-NOT-RENT-WEEKLY-GT-3-MTHS VALUE 'N'.
ELGIBL             15  MELG-SIGN-MINESUB-REJ-FORM-IND PIC X.
ELGIBL                 88  MELG-SIGN-MINESUB-REJ-FORM     VALUE 'Y'.
ELGIBL                 88  MELG-NOT-SIGN-MINESUB-REJ-FORM VALUE 'N'.
ELGIBL             15  MELG-PLUS-IN-SAF-IND           PIC X.
ELGIBL                 88  MELG-PLUS-IN-SAF               VALUE 'Y'.
ELGIBL             15  MELG-CONVERTED-APT-IND         PIC X.
ELGIBL                 88  MELG-CONVERTED-APT             VALUE 'Y'.
ELGIBL                 88  MELG-NOT-CONVERTED-APT         VALUE 'N'.
PL4055             15  MELG-DECL-CAN-NONREN-IND       PIC X.
PL4055                 88  MELG-DECL-CAN-NONREN           VALUE 'Y'.
PL4055                 88  MELG-NOT-DECL-CAN-NONREN       VALUE 'N'.
PL4055             15  MELG-FILLER-2                  PIC X(14).
TXMOLD     05  MEND-MOLD-LIM-ONS                     PIC S9(6).
TXMOLD     05  MEND-MOLD-LIM-OFF                     PIC S9(6).
TXMOLD     05  M-MOLD-APPLIES                        PIC X.
TXMOLD         88  M-MOLD-ON-POL                     VALUE 'Y'.
TXMOLD     05  M-EXPANDED-CREDIT-FIELD-SW            PIC X.
TXMOLD         88  M-EXPANDED-CREDIT-FIELDS          VALUE 'Y'.
TXMOLD     05  M-EXPANDED-FIELDS-FOR-CREDITS.
TXMOLD         10  M-EXP-TX-ROOF-CRED-AMT        PIC S9(5)V99.
TXMOLD         10  MRESV-EXP-LOSS-FREE-CRED-ONS  PIC S9(5)V99 COMP-3.
TXMOLD         10  MRESV-EXP-LOSS-FREE-CRED-FULL PIC S9(5)V99 COMP-3.
TXMOLD         10  MRESV-EXP-LOSS-FREE-CRED-OFF  PIC S9(5)V99 COMP-3.
TXMOLD         10  MRESV-EXP-MAX-CREDIT-ONS      PIC S9(5)V99 COMP-3.
TXMOLD         10  MRESV-EXP-MAX-CREDIT-FULL     PIC S9(5)V99 COMP-3.
TXMOLD         10  MRESV-EXP-MAX-CREDIT-OFF      PIC S9(5)V99 COMP-3.
LOSSUR     05  MRESV-EXPANDED-LOSSUR-FIELD-SW        PIC X.
TXCOND     05  M-TX-ED-DATE-IND                      PIC X.
TXCOND         88 M-TENANT-CONDO-ED-DATE-DONE        VALUE 'Y'.
TXCOND         88 M-HOA-ED-DATE-DONE                 VALUE 'H'.
CWMLD2     05  MEND-MOLD-LIAB-LIM-ONS                PIC S9(6).
CWMLD2     05  MEND-MOLD-LIAB-LIM-OFF                PIC S9(6).
NYMOLD     05  M-NY-ENDT-ED-DATE-DONE-SW             PIC X.
NYMOLD         88 M-NY-ENDT-ED-DATE-DONE             VALUE 'Y'.
NYMOLD     05  M-NY-MOLD-APPLIED-SW                  PIC X.
NYMOLD         88 M-NY-MOLD-APPLIED                  VALUE 'Y'.
NYMOLD     05  M-NY-THIRD-YEAR-NBRC-SW             PIC X.
NYMOLD         88  M-NY-THIRD-YEAR-NBRC-YES        VALUE 'Y'.
NYMOLD         88  M-NY-THIRD-YEAR-NBRC-NO         VALUE 'N'.
NYMOLD*
PL2602     05  MEND-UMBRELLA-DRIVER-OCC-TBL.
PL2602         10  MEND-UMBRELLA-DRIVER-OCC-INFO     OCCURS 7 TIMES
PL2602                                               INDEXED BY
PL2602                                               MEND-UMB-OCC.
PL2602             15  MEND-UMB-DRIVER-OCC           PIC X(02).
PL2602             15  MEND-UMB-DRIVER-EXPLAIN       PIC X(50).
PL2602*
UP2257     05  FILLER                                PIC X(10).
PL2257     05  M-OTHER-SAF-POL-INFO-TBL-GEICO.
PL2257         10  M-OTHER-SAF-POL-INFO-SEC-GEICO    OCCURS 2 TIMES
PL2257                                               INDEXED BY
PL2257                                               MOTH-GEICO.
PL2257             15  MOTH-SAF-POL-NUMB-GEICO.
PL2257                 20  MOTH-POL-SER-NO-GEICO     PIC X(10).
PL2257                 20  MOTH-POL-SUFFIX-GEICO     PIC X(04).
PL2257*
PL3427     05  MEND-UMBRELLA-ELIG-INDS.
PL3427         10  MEND-UMB-RESTRICT-COVG-IND        PIC X.
PL3427         10  MEND-UMB-ACC-VIOL-3YR-IND         PIC X.
PL3427         10  MEND-UMB-MED-IMPAIR-IND           PIC X.
PL3427         10  MEND-UMB-MED-STMT-IND             PIC X.
PL3427         10  MEND-UMB-VOLUNTEER-WRK-IND        PIC X.
PL3427         10  MEND-UMB-CLM-SUIT-5YR-IND         PIC X.
PL3427         10  MEND-UMB-EXPOSURE-NOT-COV-IND     PIC X.
PL3427         10  MEND-UMB-AIRCRAFT-USE-IND         PIC X.
PL3427         10  MEND-UMB-RESIDENT-EMPL-IND        PIC X.
PL4051     05  M-0MR-PREM-LVL-DEFAULT-IND            PIC X.
PL3991     05  M-NY-PLUS-ED-DATE-DONE-SW             PIC X.
PL3991         88 M-NY-PLUS-ED-DATE-DONE             VALUE 'Y'.
PL4089     05  M-AGGREGATED-LOSSES-SW                PIC X.
PL4431     05  M-NY-ENDT-ED1103-DONE-SW              PIC X.
PL4431         88 M-NY-ENDT-ED1103-DONE              VALUE 'Y'.
PL3785     05  MRESV-TX-LLOYDS-DEV-CR-EXPND    PIC S9V9(04) COMP-3.
PL3785     05  MRESV-NB-OR-RE-DEV-USED-EXPND   PIC S9V9(04) COMP-3.
PL3785     05  MRESV-HTFD-STD-DEV-CR-EXPND     PIC S9V9(04) COMP-3.
PL3785     05  MRESV-STD-HTFD-CR-PERC-EXPND    PIC S9V9(04) COMP-3.
PL3785     05  MRESV-TX-DEV-EXPND-SW           PIC X.
PL4667     05  MRESII-PRICE-STABLIZATION-IND   PIC X.
PL4667     05  M-PRICE-STABLIZATION-FIELDS.
PL4667         10 MRESII-PRICE-STABLIZATION-AMT  PIC S9(5)V99 COMP-3.
PL4667         10 MRESII-PRICE-STABLIZATION-ONS  PIC S9(5)V99 COMP-3.
PL4667         10 MRESII-PRICE-STABLIZATION-OFF  PIC S9(5)V99 COMP-3.
PL4667         10 MRESII-PRICE-STABLIZATION-WAIV PIC S9(5)V99 COMP-3.
NCLANN     05  M-NCOIL-1ST-ANNIV-DT            PIC 9(08).
NCLANN     05  M-NCOIL-LST-ANNIV-DT            PIC 9(08).
NCLANN     05  M-KBMS-RECOMMENDED-TIER         PIC X(05).
NCLANN     05  M-NCOIL-BUFFER-TIER             PIC X(01).
DC1203     05  M-NCOIL-IND                     PIC X(01).
UP2257     05  M-GEICO-SOURCE-OF-ADV           PIC X(20).
NCLAN2     05  M-NCOIL-STATE                   PIC X(01).
NCLAN2     05  M-NCOIL-NXT-ANNIV-DT            PIC 9(08).
PL4383     05  M-UMB-CONVERT-INEXPDR-SW        PIC X(01).
PL4383         88 M-UMB-CONVERT-INEXP-DONE     VALUE 'Y'.
PL4489     05  MCLA-CLAIM-AMT-PAID-TABLE.
PL4489         10  MCLA-CLAIM-AMT-PAID-TBL           OCCURS 10 TIMES
PL4489                                               INDEXED BY
PL4489                                               MCLA-PAID-INDEX.
PL4489             15  MCLA-CLAIM-AMT-PAID           PIC S9(9) COMP-3.
PL4507     05  M-RATE-DATE                     PIC 9(8).
PL4507     05  M-RATE-PLAN                     PIC X.
PL4507     05  M-PRO-RATE-FACTOR               PIC S9V999.
PL4507     05  M-EARTHQUAKE-DED-PERCENT        PIC X(2).
PL4507     05  M-PDB-IND                       PIC X.
PL4507     05  M-ILOG-IND                      PIC X.
PL4507     05  M-ROUTER-IND-3                  PIC X.
PL4507     05  M-ROUTER-IND-4                  PIC X.
PL4507     05  M-ROUTER-IND-5                  PIC X.
PL4507     05  M-ROUTER-IND-6                  PIC X.
PL4507     05  M-ROUTER-IND-7                  PIC X.
PL4507     05  M-ROUTER-IND-8                  PIC X.
PL4507     05  M-ROUTER-IND-9                  PIC X.
PL4507     05  M-ROUTER-IND-10                 PIC X.
PL5061***   M-STATE-SURCHARGE FIELDS BROUGHT UP FOR OIGA, BUT USING
PL5061****  GENERIC NAMES, FOR USE BY ANY STATE
PL5061     05  M-STATE-SURCHARGE-TABLE.
PL5061         10  M-SURCHARGE-TBL OCCURS 10 TIMES
PL5061                                   INDEXED BY M-STSURIDX.
PL5061             15  M-SURCHARGE-TAX-ID       PIC  X(02).
PL5061             15  M-SURCHARGE-AMT          PIC S9(7)V99 COMP-3.
PL5061             15  M-SURCHARGE-AMT-ONS      PIC S9(7)V99 COMP-3.
PL5061             15  M-SURCHARGE-AMT-OFF      PIC S9(7)V99 COMP-3.
PL5061             15  M-SURCHARGE-AMT-WAIV     PIC S9(7)V99 COMP-3.
PL4507     05  M-RULE-OF-APPLICATION            PIC X.
PL4511     05  M-PRE-PRERENEWAL-ROUTING-IND     PIC X.
PL4511     05  M-DATE-LAST-UNDERWRITTEN         PIC 9(8).
PL4511     05  M-UW-EXPLANATION-TABLE.
PL4511         10  M-UW-EXPLANATION-TBL         OCCURS 15 TIMES.
PL4511             15  M-UW-EXPLANATION-CODE    PIC X(9).
PL4511             15  M-UW-EXPLANATION-TYPE    PIC X.
PL4827     05  M-APPROVAL-INFO.
PL4827         10  M-APPROVERS-INITIALS             PIC X(3).
PL4827         10  M-APPROVAL-DATE                  PIC 9(8).
PL4827         10  M-UW-APPROVAL-REASON-1           PIC X(80).
PL4827         10  M-UW-APPROVAL-REASON-2           PIC X(80).
PL5119     05  M-MI-CURR-TIMESTAMP              PIC X(26).
PL5061     05  M-TAX-ID-85-RATE              PIC SV9(06) COMP-3.
PL5061     05  MFILLER-64                       PIC X(4151).
PL5119*    05  MFILLER-64                       PIC X(4155).
PL4511*    05  MFILLER-64                       PIC X(4181).
PL4507*    05  MFILLER-64                       PIC X(4511).
PL5061*    05  MFILLER-64                       PIC X(4512).
PL4507*    05  MFILLER-64                            PIC X(4732).
PL4489*    05  MFILLER-64                            PIC X(4757).
PL4383*    05  MFILLER-64                            PIC X(4807).
NCLAN2*    05  MFILLER-64                            PIC X(4808).
UP2257*    05  MFILLER-64                            PIC X(4817).
DC1203*    05  MFILLER-64                            PIC X(4837).
NCLANN*    05  MFILLER-64                            PIC X(4838).
PL4667*    05  MFILLER-64                            PIC X(4860).
PL3785*    05  MFILLER-64                            PIC X(4877).
PL3785*    05  MFILLER-64                            PIC X(4878).
PL4431*    05  MFILLER-64                            PIC X(4890).
PL4089*    05  MFILLER-64                            PIC X(4891).
PL3991*    05  MFILLER-64                            PIC X(4892).
PL4051*    05  MFILLER-64                            PIC X(4893).
PL3427*    05  MFILLER-64                            PIC X(4894).
PL2257*    05  MFILLER-64                            PIC X(4903).
PL2602*    05  MFILLER-64                            PIC X(4941).
NYMOLD*    05  MFILLER-64                            PIC X(5305).
CWMLD2*    05  MFILLER-64                            PIC X(5308).
TXCOND*    05  MFILLER-64                            PIC X(5320).
LOSSUR*    05  MFILLER-64                            PIC X(5321).
TXMOLD*    05  MFILLER-64                            PIC X(5322).
ELGIBL*    05  MFILLER-64                            PIC X(5377).       41080006
IDFRAD*    05  MFILLER-64                            PIC X(5580).       41080006
957580*    05  MFILLER-64                            PIC X(5581).       41080006
954040*    05  MFILLER-64                            PIC X(5585).       41080006
KAHKAH*    05  MFILLER-64                            PIC X(5619).       41080006
946110*    05  MFILLER-64                            PIC X(5652).       41080006
940530*    05  MFILLER-64                            PIC X(5653).       41080006
936330*    05  MFILLER-64                            PIC X(5676).       41080006
SRMSRM*    05  MFILLER-64                            PIC X(5716).       41080006


022140*  02  M-HIST-SPMR-REDEF-INFO                   REDEFINES
022150*      M-HIST-SPMR-DATA-FIELDS.
022160*    05  M-HIST-SPMR-DATA                       OCCURS 26500 TIMES
022170*                                               INDEXED BY
022170*                                               MHIST-INDEX
022170*                                               PIC X.
 

