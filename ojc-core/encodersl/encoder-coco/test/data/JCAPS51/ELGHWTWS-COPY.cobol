      *********************************************************
      *             COPYBOOK NAME:  ELGHWTWS                  *
      *                  TRANSACTION RECORD                   *
      *                        FOR                            *
      *          HEWITT CONNECTIONS 1000 BYTE LAYOUT          *
      *-------------------------------------------------------*
      *       1. RECORD LENGTH = 1000 CHARACTERS              *
      *       2. ORGANIZATION = SEQUENTIAL                    *
      *
      * THE NUMBER IN COLUMNS 73 THRU 80 ARE THE DISPLACEMENT WITHIN
      * THE RECORD WHERE THE FIELD EXISTS.
      *
      * I.E. 5-9 THE DATA IS IN BYTE 5 THRU 9.
      *      130-130 THE DATA IS IN BYTE 130.
      *--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
      ***************************************************************

       01  ELGHWTWS-COPY.
           05 INP-RECORD-TYPE                       PIC X.              1-1
              88 INP-HEWITT-HEADER           VALUE '1'.
              88 INP-HEWITT-MEMBER           VALUE '2'.
              88 INP-HEWITT-DEPENDENT        VALUE '3'.
              88 INP-HEWITT-TRAILER          VALUE '9'.
           05 FILLER                                PIC X(24).          2-25
           05 INP-PRODUCT-TYPE                      PIC X(06).          26-31
           05 INP-EMP-SSN                           PIC X(09).          32-40
           05 INP-DEP-NUMBER                        PIC X(02).          41-42
           05 INP-DEP-SSN                           PIC X(09).          43-51
           05 INP-CHANGE-IND                        PIC X(01).          52-52
           05 INP-ACTION-CODE                       PIC X.              53-53
           05 INP-EMP-PREV-SSN                      PIC X(09).          54-62
           05 INP-PREV-SSN-IND                      PIC X(01).          63-63
           05 INP-EMP-ALT-ID-NO                     PIC X(09).          64-72
           05 INP-SURV-SPOUSE-IND                   PIC X(01).          73-73
           05 INP-RELATIONSHIP                      PIC X(02).          74-75
           05 FILLER                                PIC X(01).          76-76
           05 INP-FIRST-NAME                        PIC X(15).          77-91
           05 FILLER                                PIC X(01).          92-92
           05 INP-MID-INITIAL                       PIC X(01).          93-93
           05 FILLER                                PIC X(01).          94-94
           05 INP-LAST-NAME                         PIC X(30).          95-124
           05 FILLER                                PIC X(01).          125-125
           05 INP-NAME-EXT                          PIC X(06).          126-131
           05 FILLER                                PIC X(10).          132-141
           05 INP-DOB                               PIC 9(08).          142-149
           05 FILLER                                PIC X(01).          150-150
           05 INP-EFF-DATE                          PIC 9(08).          151-158
           05 FILLER                                PIC X(01).          159-159
           05 INP-SEX                               PIC X(01).          160-160
           05 FILLER                                PIC X(51).          161-211
           05 INP-ADDRESS-LINE-1                    PIC X(30).          212-241
           05 FILLER                                PIC X(01).          242-242
           05 INP-ADDRESS-LINE-2                    PIC X(30).          243-272
           05 FILLER                                PIC X(01).          273-273
           05 INP-ADDRESS-LINE-3                    PIC X(30).          274-303
           05 FILLER                                PIC X(01).          304-304
           05 INP-CITY                              PIC X(16).          305-320
           05 FILLER                                PIC X(01).          321-321
           05 INP-STATE                             PIC X(02).          322-323
           05 FILLER                                PIC X(01).          324-324
           05 INP-ZIP-CODE                          PIC X(10).          325-334
           05 FILLER                                PIC X(01).          335-335
           05 INP-COUNTRY                           PIC X(03).          336-338
           05 FILLER                                PIC X(01).          339-339
           05 INP-TELEPHONE                         PIC X(10).          340-349
           05 FILLER                                PIC X(42).          350-391
           05 INP-EMP-TYPE                          PIC X(02).          392-393
           05 FILLER                                PIC X(10).          394-403
           05 INP-RPT-FLD1                          PIC X(15).          404-418
           05 INP-CHANGE-IND-2                      PIC X(01).          419-419
           05 INP-CHANGE-DATE-2                     PIC 9(08).          420-427
           05 FILLER                                PIC X(01).          428-428
           05 INP-RPT-FLD2                          PIC X(15).          429-443
           05 FILLER                                PIC X(01).          444-444
           05 INP-CHANGE-DATE-3                     PIC 9(08).          445-452
           05 FILLER                                PIC X(01).          453-453
           05 INP-RPT-FLD3                          PIC X(15).          454-468
           05 FILLER                                PIC X(01).          469-469
           05 INP-EFF-DATE-2                        PIC 9(08).          470-477
           05 INP-EFF-DATE-IND                      PIC X(01).          478-478
           05 INP-RPT-FLD4                          PIC X(15).          479-493
           05 FILLER                                PIC X(10).          494-503
           05 INP-RPT-FLD5                          PIC X(15).          504-518
           05 FILLER                                PIC X(10).          519-528
           05 INP-RPT-FLD6                          PIC X(15).          529-543
           05 FILLER                                PIC X(10).          544-553
           05 INP-RPT-FLD7                          PIC X(15).          554-568
           05 FILLER                                PIC X(42).          569-610
           05 INP-MEDICARE-FLAG                     PIC X(01).          611-611
           05 FILLER                                PIC X(01).          612-612
           05 INP-DISABLED-FLAG                     PIC X(01).          613-613
           05 FILLER                                PIC X(01).          614-614
           05 INP-STUDENT-FLAG                      PIC X(01).          615-615
           05 FILLER                                PIC X(10).          616-625
           05 INP-BENEFITS-FLAG                     PIC X(01).          626-626
           05 FILLER                                PIC X(44).          627-670
           05 INP-OPTION-CODE                       PIC X(6).           671-676
           05 FILLER                                PIC X(1).           677-677
           05 INP-COVERAGE-FLAG                     PIC X(02).          678-679
           05 FILLER                                PIC X(01).          680-680
           05 INP-COVERAGE-CODE                     PIC X(02).          681-682
           05 FILLER                                PIC X(01).          683-683
           05 INP-COV-EFF-DATE                      PIC 9(08).          684-691
           05 FILLER                                PIC X(01).          692-692
           05 INP-COV-TERM-DATE                     PIC 9(08).          693-700
           05 FILLER                                PIC X(01).          701-701
           05 INP-COV-CODE-EFF-DATE                 PIC 9(08).          702-709
           05 FILLER                                PIC X(02).          710-711
           05 INP-NETWORK-ID                        PIC X(01).          712-712
           05 FILLER                                PIC X(288).         713-1000
