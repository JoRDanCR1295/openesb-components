*********  UPDATED: 05/07/99    AT: 11.26.38  BY: H.GRAACK           ***LESPFRCP
*********  UPDATED: 04/02/99    AT: 10.23.13  BY: H.GRAACK           ***LESPFRCP
*********  UPDATED: 03/04/99    AT: 16.41.43  BY: H.GRAACK           ***LESPFRCP
*********  UPDATED: 02/16/99    AT: 15.04.35  BY: H.GRAACK           ***LESPFRCP
*********  UPDATED: 02/11/99    AT: 15.53.11  BY: H.GRAACK           ***LESPFRCP
*********  UPDATED: 02/08/99    AT: 13.23.12  BY: H.GRAACK           ***LESPFRCP
*********  UPDATED: 11/20/98    AT: 11.43.03  BY: H.GRAACK           ***LESPFRCP
      *|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      *||  COPYLIB:  LESPFRCP  |||||||||||||||||||||||||||||||||||||||
      *|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      *RECORD FR-COMP-DETAIL GENERATED: 18-NOV-98
      *
      *--------------------------------------------------------------*
      *    STANDARD TEMPLATE FORMAT - CELL(GSSSS) RECORD STRUCTURE   *
      *--------------------------------------------------------------*
      *                   (LEVEL ASSIGNMENTS)                        *
      *                                                              *
      *01  (I-O) RECORD   -->>  01 LEVEL NAME IS PROG/COBOL ASSIGNED *
      *                            AND IS NOT DEFINED IN THE COPYLIB *
      *                                                              *
      *                     COMMON LEVELS FOR CELLS                  *
      *02  GSSSS-RECORD.  -->>  02 LEVEL RECORD  CELL/GSSSS ASSIGNED *
      *03  GSSSS-KEY.     -->>  03 LEVEL KEY  IS CELL/GSSSS ASSIGNED *
      *                            CONTAINING 33-CHAR 04/05/XX AREAS *
      *03  GSSSS-DATA.    -->>  03 LEVEL DATA IS DICTIONARY ASSIGNED *
      *                            CONTAINING VARIOUS 04/05/XX AREAS *
      *03  GSSSS-STATS.   -->>  03 LEVEL STAT IS PROG(LXXX) ASSIGNED *
      *                            CONTAINING SUPPORT 04/05/XX AREAS *
      *                                                              *
      *                 FUNCTIONAL LEVELS FOR CELLS                  *
      *02 -------> GROUP        02 LEVEL RECORD................NAME  *
      *03 -------> GROUP        03 LEVEL MAJOR AREA............NAMES *
      *  04 -----> GROUP/ITEM   04 LEVEL INTERMEDIATE AREA.....NAMES *
      *    05 --->       ITEM   05 LEVEL MINOR AREA/PRIMARY....NAMES *
      *    | XX -> GROUP/ITEM   XX LEVEL MINOR AREA/SECONDARY..NAMES *
      *    |                                                         *
      *   NOTE:                                                      *
      *   ONLY GSSSS-MINOR-PRIMARY-NAME(ITEMS) ARE "RPF" SELECTABLE  *
      *    ALL GSSSS---------------NAMES ARE "COBOL/FILEAID" USABLE  *
      *                                                              *
      *               (COLUMN ALLIGNMENT & SYNTAX)                   *
      *                                                              *
      *  THIS STRUCTURE IS CRITICAL FOR AUTOMATED "RPF" PROCESSING;  *
      *  AND COBOL/FILEAID PROCESSING ALSO DEPENDS ON THIS STANDARD. *
      *                                                              *
      *0.1.........2.........3.........4.........5.........6.........*
      *8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.*
      *||  COPYLIB:  LXXGSSSS  ||||||||||||||||||||||||||||||||||||||*
      *02  GSSSS-RECORD.                                             *
      *03  GSSSS-MAJOR-NAME.                                         *
      *  04  GSSSS-INTERMEDIATE-NAME             ------------        *
      *    05  GSSSS-MINOR-PRIMARY-NAME          ------------        *
      *      XX  GSSSS-MINOR-SECONDARY-NAME      ------------        *
      *          |                               REDEFINES           *
      *          |                               PIC S9(N)V99.       *
      *          |                               PIC 9(NN).          *
      *          |                               PIC X(NN).          *
      *          |                               |                   *
      *          PFX(06)NAME(24) = DATA-NAME(30) USAGE(ALPHA/NUMERIC)*
      *                                                              *
      * COMMENT LINE(COLUMN 07 = "*")                                *
      *--------------------------------------------------------------*
      *
      *01  FR-COMP-DETAIL.
      ****************************************************************
       02  PFRCP-RECORD.
      ****************************************************************
       03  PFRCP-KEY.
         04  PFRCP-CELL.
           05  PFRCP-CELL-GROUP                  PIC X(01).
           05  PFRCP-CELL-SEGMENT                PIC X(04).
         04  PFRCP-CELL-DELIM                    PIC X(01).
         04  PFRCP-DATE.
      * AS OF DATE ...................................... UCCUR-DATE *
           05  PFRCP-TRANS-DATE                  PIC 9(08).
             05  PFRCP-TRANS-D-R                 REDEFINES
                 PFRCP-TRANS-DATE.
             06  PFRCP-TRANS-YR                  PIC 9(04).
             06  PFRCP-TRANS-MNTH                PIC 9(02).
             06  PFRCP-TRANS-DAY                 PIC 9(02).
         04  PFRCP-DATE-DELIM                    PIC X(01).
         04  PFRCP-PATH.
      * AGENT ............................................ UCFR-CODE *
           05  PFRCP-AGENCY-SEC-CODE             PIC X(02).
           05  PFRCP-AGENT-NO                    PIC X(03).
      * POLICY ............................................ UCPOL-NO *
           05  PFRCP-POL-NO                      PIC X(07).
             05  PFRCP-POL-NO-R                  REDEFINES
                 PFRCP-POL-NO.
             06  PFRCP-POL-NO-PFX                PIC X(01).
             06  PFRCP-POL-NO-SFX                PIC X(06).
         04  PFRCP-PATH-R                        REDEFINES
             PFRCP-PATH.
           05  PFRCP-AGENT-CODE                  PIC X(05).
             05  PFRCP-POL-NO-R7                 PIC X(07).
      *  04  PFRCP-PATH-DELIM                    PIC X(01).
         04  PFRCP-SEQN.
      * ................................................ EN-REC-TYPE *
           05  PFRCP-REC-TYPE                    PIC X(02).
      * FOR SAME AGENT-CODE/POL-NO/REC-TYPE .............. (DERIVED) *
           05  PFRCP-POL-TRAN-AGENT              PIC 9(04).
      *  04  PFRCP-SEQN-DELIM                    PIC X(01).
      ****************************************************************
         03  PFRCP-DATA.
      * .................................................... UCFR-ID *
           05  PFRCP-PROD-ID                     PIC X(05).
      * ............................................. UCCOMPANY-CODE *
           05  PFRCP-COMPANY-CODE                PIC X(02).
      * ............................................ UCLINE-BUSINESS *
           05  PFRCP-LINE-BUSINESS               PIC X(01).
      * ................................................. EN-REC-IND *
           05  PFRCP-REC-IND                     PIC X(01).
      * .............................................. UCEMP-PA-CODE *
           05  PFRCP-PA-CODE                     PIC X(02).
      * ............................................ UCINIT-PREM-IND *
           05  PFRCP-INIT-PREM-IND               PIC X(01).
      * .................................................. UCGOM-IND *
           05  PFRCP-GOM-IND                     PIC X(01).
      * ..................................................... UCPREM *
           05  PFRCP-PREMIUM                     PIC S9(8)V99.
      * ............................................... EN-FROM-DATE *
           05  PFRCP-PREM-FROM-DATE              PIC 9(08).
             05  PFRCP-PREM-FROM-D-R             REDEFINES
                 PFRCP-PREM-FROM-DATE.
             06  PFRCP-PREM-FROM-YR              PIC 9(04).
             06  PFRCP-PREM-FROM-MNTH            PIC 9(02).
             06  PFRCP-PREM-FROM-DAY             PIC 9(02).
      * ................................................. EN-TO-DATE *
           05  PFRCP-PREM-TO-DATE                PIC 9(08).
             05  PFRCP-PREM-TO-D-R               REDEFINES
                 PFRCP-PREM-TO-DATE.
             06  PFRCP-PREM-TO-YR                PIC 9(04).
             06  PFRCP-PREM-TO-MNTH              PIC 9(02).
             06  PFRCP-PREM-TO-DAY               PIC 9(02).
      * .............................................. UCFR-EFF-DATE *
           05  PFRCP-EFF-DATE                    PIC 9(08).
             05  PFRCP-EFF-D-R                   REDEFINES
                 PFRCP-EFF-DATE.
             06  PFRCP-EFF-YR                    PIC 9(04).
             06  PFRCP-EFF-MNTH                  PIC 9(02).
             06  PFRCP-EFF-DAY                   PIC 9(02).
      * ........................................... UCFR-TERMIN-DATE *
           05  PFRCP-TERMIN-DATE                 PIC 9(08).
             05  PFRCP-TERMIN-D-R                REDEFINES
                 PFRCP-TERMIN-DATE.
             06  PFRCP-TERMIN-YR                 PIC 9(04).
             06  PFRCP-TERMIN-MNTH               PIC 9(02).
             06  PFRCP-TERMIN-DAY                PIC 9(02).
      * COMPENSATION................................ UCLPC-/FCTR/AMT *
           05  PFRCP-LPC-FCTR                    PIC 9(02).
           05  PFRCP-LPC-COMP                    PIC S9(9).
      * COMPENSATION........................ UCUNADJ-H-/IND/RATE/AMT *
           05  PFRCP-UNADJ-H-IND                 PIC X(01).
           05  PFRCP-UNADJ-H-RATE                PIC S9(1)V999.
           05  PFRCP-UNADJ-H-AMT                 PIC S9(8)V99.
      * COMPENSATION........................................ UCP-G-F *
           05  PFRCP-P-G-F                       PIC X(01).
      * COMPENSATION............................ UCPGF-/IND/RATE/AMT *
           05  PFRCP-PGF-IND                     PIC X(01).
           05  PFRCP-PGF-RATE                    PIC S9(1)V999.
           05  PFRCP-PGF-RATE-GRP                PIC X(03).
           05  PFRCP-PGF-AMT                     PIC S9(8)V99.
      * MTD COMPENSATION.................. UCFRCODE-MTD-/LPC/UNH/PGF *
           05  PFRCP-FR-MTD-LPC                  PIC S9(11).
           05  PFRCP-FR-MTD-UNH                  PIC S9(8)V99.
           05  PFRCP-FR-MTD-PGF                  PIC S9(8)V99.
      * GROUP......... UCGROUP-/TYPE/MODE/PAYTS/ADD-ISSUE/ACCTG-DATE *
           05  PFRCP-GROUP-TYPE                  PIC X(01).
           05  PFRCP-GROUP-MODE                  PIC X(01).
           05  PFRCP-GROUP-PAYTS                 PIC X(01).
           05  PFRCP-GROUP-ADD-ISSUE             PIC X(01).
           05  PFRCP-GROUP-ACCTG-DATE            PIC 9(08).
             05  PFRCP-GROUP-ACCTG-D-R           REDEFINES
                 PFRCP-GROUP-ACCTG-DATE.
             06  PFRCP-GROUP-ACCTG-YR            PIC 9(04).
             06  PFRCP-GROUP-ACCTG-MNTH          PIC 9(02).
             06  PFRCP-GROUP-ACCTG-DAY           PIC 9(02).
      * ............................................. UCSP-VALID-CR *
           05  PFRCP-SP-VALID-CR                 PIC S9(8)V99.
      * .............................................. UCH-PERS-FEE *
           05  PFRCP-HLT-PERS-FEE                PIC S9(8)V99.
      * POLICY INFO.......................... EN-/POL-DATE-BASIC/DUR *
           05  PFRCP-POL-ISS-DATE                PIC 9(08).
             05  PFRCP-POL-ISS-D-R               REDEFINES
                 PFRCP-POL-ISS-DATE.
             06  PFRCP-POL-ISS-YR                PIC 9(04).
             06  PFRCP-POL-ISS-MNTH              PIC 9(02).
             06  PFRCP-POL-ISS-DAY               PIC 9(02).
           05  PFRCP-FAS-POL-DUR                 PIC 9(02).
      * POLICY INFO......................................... UCQRRSS *
           05  PFRCP-QRRSS                       PIC 9(07).
             05  PFRCP-QRRSS-R                   REDEFINES
                 PFRCP-QRRSS.
             06  PFRCP-Q                         PIC 9(01).
             06  PFRCP-RR                        PIC 9(03).
             06  PFRCP-SS                        PIC 9(03).
      * POLICY INFO................................. UCFORM-/NO/SUFF *
           05  PFRCP-FORM-NO                     PIC 9(03).
           05  PFRCP-FORM-SUFF                   PIC 9(01).
      ****************************************************************
       03  PFRCP-STATS.
      * UNUSED ......................................................*
             05  PFRCP-UNUSED                    PIC X(19).
      * ENTITY LRECL ................................. RECORD LENGTH *
             05  PFRCP-LRECL                     PIC 9(03).
      * ENTITY TALLY ................................. RELATIVE REC# *
             05  PFRCP-TALLY                     PIC 9(06).
