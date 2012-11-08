      *****************************************************************
      *                                                               *
      *  COPYBOOK NAME       : WFNDABHI                               *
      *                                                               *
      *  DESCRIPTION         : FOUNDATION PRODUCTION                  *
      *                        CICS SERVER OPTION                     *
      *                        FND-SPECIFIC ABEND HANDLER INTERFACE   *
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

       01  ABHI-ABEND-HANDLER-INTERFACE.
         02  ABHI-ERROR-BLOCK.
           03  ABHI-MONITOR-DATA-BLOCK.
             04  ABHI-PLATFORM-TYPE             PIC X(1).
               88  ABHI-PLATFORM-TYPE-OS2         VALUE 'O'.
               88  ABHI-PLATFORM-WINDOWS          VALUE 'W'.
               88  ABHI-PLATFORM-VMS              VALUE 'V'.
               88  ABHI-PLATFORM-HP-UNIX          VALUE 'H'.
               88  ABHI-PLATFORM-ULTRIX           VALUE 'U'.
               88  ABHI-PLATFORM-MVS-CICS         VALUE 'C'.
             04  FILLER                         PIC X(84).
           03  ABHI-SEVERE-ERROR-CODE           PIC X(4).
           03  ABHI-PGM-ID                      PIC X(8).
           03  ABHI-ERR-APPL-TYPE               PIC X(1).
             88  ABHI-APPL-TYPE-CLIENT            VALUE 'C'.
             88  ABHI-APPL-TYPE-SERVER            VALUE 'S'.
             88  ABHI-APPL-TYPE-ARCH              VALUE 'E'.
           03  ABHI-ERROR-TYPE-CODE             PIC X(1).
             88  ABHI-ERROR-TYPE-APPL             VALUE 'A'.

             88  ABHI-ERROR-TYPE-OS2              VALUE 'O'.

             88  ABHI-ERROR-TYPE-CICS             VALUE 'C'.

             88  ABHI-ERROR-TYPE-DB2              VALUE 'D'.

             88  ABHI-ERROR-TYPE-SYBASE           VALUE 'S'.

             88  ABHI-ERROR-TYPE-INFO             VALUE 'I'.

             88  ABHI-ERROR-TYPE-IMH-DEBUG        VALUE 'T'.

             88  ABHI-ERROR-TYPE-CMN-MOD          VALUE 'Z'.
             88  ABHI-ERROR-TYPE-DBM              VALUE 'B'.
           03  ABHI-ERROR-LOCATION.
             04  ABHI-ERROR-TAG-DATA            PIC X(30).
           03  ABHI-ERROR-MSG-NUM               PIC S9(4) COMP.
           03  ABHI-ERROR-MSG-AREA                PIC X(159).
           03  ABHI-LOAD-IMAGE-NAME               PIC X(8).
         02  ABHI-FCP-ERROR-BLOCK.
           03  ABHI-WIN-NAME                      PIC X(8).
           03  ABHI-PROCESS-ID                    PIC 9(4) COMP.
           03  ABHI-THREAD-ID                     PIC 9(4) COMP.
           03  ABHI-DEPEND-MSG-NUM                PIC S9(4) COMP.
           03  ABHI-DEPEND-MSG-AREA               PIC X(159).
           03  ABHI-RETURN-CODE                   PIC S9(4) COMP.
             88  ABHI-SUCCESS                       VALUE 0.
             88  ABHI-WARNING                       VALUE +4.
             88  ABHI-ERROR                         VALUE +8.
             88  ABHI-FATAL-ERROR                   VALUE +16.
           03  ABHI-EXPLANATION-CODE              PIC X(01).

