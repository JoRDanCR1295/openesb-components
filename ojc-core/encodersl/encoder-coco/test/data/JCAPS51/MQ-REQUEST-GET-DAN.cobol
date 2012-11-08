       01  MQ-REQUEST-GET-DAN.
           03  PHH-MQ-HEADER.
               05  PROGNAME             PIC X(16).
               05  REPLY-STATUS-CD      PIC 9(8).
               05  REPLY-STATUS-MSG     PIC X(64).
               05  MSG-TYPE             PIC X(8).
               05  VERSION              PIC 9(4).
           03  BODY.
               05  DAN-FORMAT-TYP       PIC X(1).
