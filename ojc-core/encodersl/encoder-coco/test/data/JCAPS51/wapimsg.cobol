      *****************************************************************
      *                                                               *
      *  COPYBOOK NAME       : WAPIMSG                                *
      *                                                               *
      *  DESCRIPTION         : FOUNDATION PRODUCTION                  *
      *                        CICS SERVER OPTION                     *
      *                        APPLICATION VIEW OF THE PARM BLOCK     *
      *                                                               *
      *  CORRESPONDING FND                                            *
      *   C DATA STRUCTURE   :                                        *
      *          FILE        :                                        *
      *                                                               *
      *  LAST UPDATE         : 01 JULY 1993                           *
      *                                                               *
      *  USAGE               : PUBLIC                                 *
      *                                                               *
      *****************************************************************

           02  MSGIO-FUNCTION-CODE                     PIC S9(04) COMP.
               88  MSGIO-CONVERSE                         VALUE +1032.
               88  MSGIO-INIT-PORT                        VALUE +1033.
               88  MSGIO-REGISTER-SERVICE                 VALUE +1034.
               88  MSGIO-RECEIVE                          VALUE +1035.
               88  MSGIO-SEND                             VALUE +1036.
               88  MSGIO-SEND-SELF                        VALUE +1037.
               88  MSGIO-SEND-BROADCAST                   VALUE +1038.
               88  MSGIO-SEND-GUARANTEED-DELVRY           VALUE +1039.
               88  MSGIO-SEND-WITH-ACK                    VALUE +1040.
               88  MSGIO-TERM-PORT                        VALUE +1041.
               88  MSGIO-DEREGISTER-SERVICE               VALUE +1042.
           02  MSGIO-RETURN-CODE                       PIC S9(04) COMP.
               88  MSGIO-RETURN-OK                        VALUE 0.
               88  MSGIO-RETURN-WARNING                   VALUE +4.
               88  MSGIO-RETURN-ERROR                     VALUE +16.
           02  MSGIO-EXPLANATION-CODE                  PIC S9(04) COMP.
               88  MSGIO-SUCCESS                          VALUE +0.
               88  MSGIO-FATAL-ERROR                      VALUE +2.
               88  MSGIO-ERRPARMS                         VALUE +3.
               88  MSGIO-TIMEOUT                          VALUE +4.
               88  MSGIO-NONEXISTENT                      VALUE +6.
               88  MSGIO-SMALLBUFF                        VALUE +7.
               88  MSGIO-UNSUPPORTED-FUNC                 VALUE +9.
               88  MSGIO-LOG-ERROR                        VALUE +10.
               88  MSGIO-INVALID-VERSION                  VALUE +13.
               88  MSGIO-INVALID-PRIORITY                 VALUE +14.
           02  MSGIO-PARM-BLOCK-VERSION                PIC X(02).
               88  MSGIO-API-FIRST-VER                 VALUE '  '.
               88  MSGIO-API-V2                        VALUE '02'.
           02  ACTUAL-LENGTH-SEND                      PIC S9(09) COMP.
           02  MSGIO-ACTUAL-DATA-LENGTH
                   REDEFINES ACTUAL-LENGTH-SEND        PIC S9(09) COMP.
           02  ACTUAL-LENGTH-RECV                      PIC S9(09) COMP.
           02  MSGIO-MAX-DATA-LENGTH                   PIC S9(09) COMP.
           02  MSGIO-THIS-SERVICE-ID.
               03  MSGIO-THIS-ROUTING.
                   04  MSGIO-THIS-APPL-NAME            PIC S9(09) COMP.
                   04  MSGIO-THIS-SERVICE-NAME         PIC S9(04) COMP.
                   04  MSGIO-THIS-SERVICE-VERS         PIC X(02).
               03  MSGIO-THIS-NETWORK-ADDRESS.
                   04  MSGIO-THIS-NODE-ID.
                       05  DOMAIN                      PIC S9(04) COMP.
                       05  STATION                     PIC S9(04) COMP.
                   04  MSGIO-THIS-PORT-ID              PIC X(10).
               03  MSGIO-THIS-PROCESS-INFO.
                   04  MSGIO-THIS-LANGUAGE             PIC X.
                       88  MSGIO-THIS-LANGUAGE-C          VALUE 'A'.
                       88  MSGIO-THIS-LANGUAGE-COBOL      VALUE 'B'.
           02  MSGIO-PEER-SERVICE-ID.
               03  MSGIO-PEER-ROUTING.
                   04  MSGIO-PEER-APPL-NAME            PIC S9(09) COMP.
                   04  MSGIO-PEER-SERVICE-NAME         PIC S9(04) COMP.
                   04  MSGIO-PEER-SERVICE-VERS         PIC X(02).
               03  MSGIO-PEER-NETWORK-ADDRESS.
                   04  MSGIO-PEER-NODE-ID.
                       05  DOMAIN                      PIC S9(04) COMP.
                       05  STATION                     PIC S9(04) COMP.
                   04  MSGIO-PEER-PORT-ID              PIC X(10).
               03  MSGIO-PEER-PROCESS-INFO.
                   04 MSGIO-PEER-LANGUAGE              PIC X.
                       88  MSGIO-PEER-LANGUAGE-C          VALUE 'A'.
                       88  MSGIO-PEER-LANGUAGE-COBOL      VALUE 'B'.
           02  MSGIO-MSG-IDENTIFIER                    PIC X(24).
           02  APPL-MSG-ID                             PIC S9(04) COMP.
           02  SYST-OR-APPL-MSG                        PIC X.
               88  FND-SYST-MSG-FLG                       VALUE 'S'.
               88  FND-APPL-MSG-FLG                       VALUE 'A'.
           02  MSGIO-MSG-TYPE                          PIC X.
               88  MSGIO-TYPE-REQUEST                     VALUE 'R'.
               88  MSGIO-TYPE-REPLY                       VALUE 'P'.
           02  MSGIO-REPLY-REQUESTED                   PIC X.
               88  MSGIO-REPLY-REQUESTED-YES              VALUE 'Y'.
               88  MSGIO-REPLY-REQUESTED-NO               VALUE 'N'.
           02  FILLER                                  PIC X.
           02  MSGIO-ENVIRONMENT                       PIC X(02).
           02  MSGIO-TIMEOUT-INTERVAL                  PIC S9(09) COMP.
               88  MSGIO-USE-SYSTEM-DEFAULT               VALUE ZERO.
               88  MSGIO-RETURN-IMMEDIATELY               VALUE -1.
               88  MSGIO-NEVER-TIMEOUT                    VALUE -2.
           02  MSGIO-PRIORITY                          PIC S9(04) COMP.
               88  MSGIO-PRIORITY-NONE                    VALUE 0.
               88  MSGIO-PRIORITY-LOW                     VALUE 3.
               88  MSGIO-PRIORITY-MEDIUM                  VALUE 7.
               88  MSGIO-PRIORITY-HIGH                    VALUE 11.
           02  MSGIO-APPL-STATUS.
               03  MSGIO-APPL-RETURN-CODE              PIC S9(04) COMP.
                   88  MSGIO-APPL-SUCCESS                 VALUE ZERO.
                   88  MSGIO-APPL-WARNING                 VALUE +4.
                   88  MSGIO-APPL-ERROR                   VALUE +8.
                   88  MSGIO-APPL-FATAL-ERROR             VALUE +16.
               03  MSGIO-APPL-EXPLANATION-CODE         PIC S9(04) COMP.
      *            88  MS-TIMEOUT-OCCURRED                VALUE +27510.
      *            88  MS-NO-SERVICE                      VALUE +27513.
               03  MSGIO-APPL-EXPLANATION-CODE-RD REDEFINES
                         MSGIO-APPL-EXPLANATION-CODE   PIC X(2).
                   88  MS-TIMEOUT-OCCURRED             VALUE X'6B76'.
                   88  MS-NO-SERVICE                   VALUE X'6B79'.
               03  EXPLAN-DATA                         PIC X(04).
           02  SECUR.
               03  MSGIO-USER-ID                       PIC X(08).
               03  USER-PW                             PIC X(08).
           02  TRANSLATION.
               03  MAP-NAME                            PIC X(08).
               03  MAP-VERSION                         PIC X(02).
