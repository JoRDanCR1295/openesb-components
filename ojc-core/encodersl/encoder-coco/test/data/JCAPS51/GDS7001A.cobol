      ******************************************************************
      * GDS7001A                                                       *
      * THIS IS THE GLOBAL DIRECTORY SYSTEM INPUT BUFFER.              *
      ******************************************************************
       01  GDS-INPUT-BUFFER.                                            
           03  GDSI-HEADER.                                             
             05  GDSI-BUFFER-ID         PIC X(06).           
             05  GDSI-FROM-SYSTEM       PIC X(06).           
             05  GDSI-FROM-SUBSYSTEM          PIC X(06).           
             05  GDSI-KEY-PASSED              PIC X(08).           
           03  GDSI-DATA-AREA.
             05  GDSI-PU-PRIMARY-KEY.
               10  GDSI-PU-COUNTRY             PIC X(02).
               10  GDSI-PU-STATE               PIC X(02).
               10  GDSI-PU-CITY                PIC X(03).
               10  GDSI-PU-LOC                 PIC 9(02).
             05  GDSI-PU-AREA                 PIC 9(05).
             05  GDSI-PU-GEO-POOL-NUM          PIC 9(03).
             05  GDSI-PU-GEO-POOL-IND           PIC X(02).
             05  GDSI-PU-NTRL-LOC-NBR          PIC 9(05).
             05  GDSI-PU-BOOKING-SYSDATE       PIC 9(06).           
             05  GDSI-PU-PICKUP-SYSDATE        PIC 9(06).            
             05  GDSI-PU-INVOICE-SYSDT         PIC 9(06).
             05  GDSI-PU-TAX-EXMPT-NBR         PIC X(06).           
             05  GDSI-PU-VEH-CD.                             
               10  GDSI-PU-VEH-CLASS           PIC X(01).           
               10  GDSI-PU-VEH-DOORS           PIC 9(04).           
             05  GDSI-PU-UPG-PROMO-VEH-TBL.                           
               10  GDSI-PU-UPG-PROMO-VEH-CD   OCCURS 5 TIMES.
                 15  GDSI-PU-UPG-PROMO-VEH-CLASS PIC X(01).        
                 15  GDSI-PU-UPG-PROMO-VEH-DOORS  PIC 9(04).     
             05  GDSI-AGE-NBR                    PIC 9(02).           
             05  GDSI-RENT-DAYS-NBR              PIC 9(02).           
             05  GDSI-INS-CHG-DAYS-NBR           PIC 9(02).           
             05  GDSI-ARR-DAY-OF-WK-NBR           PIC 9(01).           
             05  GDSI-VARIABLE-REQUEST-AREA.                          
               10  GDSI-REQUEST-TABLE    OCCURS 25 TIMES.             
                 15  GDSI-REQUEST-CD           PIC X(03).
             05  GDSI-DP-PRIMARY-KEY.
               10  GDSI-DP-COUNTRY               PIC X(02).
               10  GDSI-DP-STATE                 PIC X(02).
               10  GDSI-DP-CITY                  PIC X(03).
               10  GDSI-DP-LOC                   PIC 9(02).
             05  GDSI-DP-AREA                     PIC 9(05).
             05  GDSI-DP-GEO-POOL-NUM             PIC 9(03).
             05  GDSI-DP-GEO-POOL-IND             PIC X(02).
             05  GDSI-DP-NTRL-LOC-NBR             PIC 9(05).
             05  GDSI-GREGORIAN-DATE              PIC X(10).
             05  GDSI-ZIP-CODE                    PIC X(10).           
             05  FILLER                           PIC X(90).           
