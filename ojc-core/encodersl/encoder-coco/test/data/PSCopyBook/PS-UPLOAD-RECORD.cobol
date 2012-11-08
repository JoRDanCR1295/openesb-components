000100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
000200*     PEOPLESOFT FILE EXTRACT RECORD                                    *
000300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
000400*  10-01-95  PJK  MODS 211, 212, 213, 214, 241, 244                     *
000500*                 CREATE FILE LAYOUT                                    *
000600*  10-09-96  JAN  MOD 583 - ADD POSITION NUMBER                         *
000700*  01-29-97  PJK  MOD 602 - ADD MGR ID AND NAME                         *
000800*  04-18-97  JLB  MOD 666 - ADD LOC ADDR AND INCREASE RECORD
000900*                 LENGTH
001000*  02-09-98  CAK  MOD 829 - ADD S_ORG_LEVE4 DID NOT INCREASE
001100*                 RECORD LENGTH
001200*  02-09-98  CAK  MOD 832 - ADD PREFERRED_NAME DID NOT INCREASE
001300*                 RECORD LENGTH
001400*  04-05-00  JWL  MOD 1261 - ADD EXP HIER LEVELS, ALONG WITH
001500*                 MGR LEVEL DATA AND USERID.  INCREASED
001600*                 LRECL TO 1000
001700*  02-05-01  JWL  ADD HOME PHONE TO END OF FILE
001800*  07-24-02  BSB  ADD FAX TO END OF FILE.  BE SURE TO CHANGE
001900*                 PSPE004F COPYLIB ALSO TO KEEP THEM IN SYNC.
002000*  11-26-02  KPP  ADD LOCATION PHONE AND EMAIL TO END OF FILE
002000*  11-17-03  SVM  ADD OFFICE TYPE AND SUPPORT LOCATION FOR VOT
002100*                 ALSO CHANGE COPYLIB PSPE004F
002200*                                                                       *
002300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  COLS
002400 01  PS-UPLOAD-RECORD.
002500     05  PS-CURRENT-DATE             PIC X(08).                     1-8
002600     05  PS-EMPLOYEE-NUMBER          PIC X(09).                     9-17
002700     05  PS-SSN                      PIC X(09).                    18-26
002800     05  PS-EMPLOYEE-NAME            PIC X(40).                    27-66
002900     05  PS-DEPARTMENT-ID            PIC X(10).                    67-76
003000     05  PS-DEPARTMENT-NAME          PIC X(30).                    77-106
003100     05  PS-DIVISION-NAME            PIC X(10).                   107-116
003200     05  PS-PAYROLL-COMPANY          PIC X(03).                   117-119
003300     05  PS-EXPENSE-COMPANY          PIC X(02).                   120-121
003400     05  PS-HOME-ADDRESS1            PIC X(30).                   122-151
003500     05  PS-HOME-ADDRESS2            PIC X(30).                   152-181
003600     05  PS-HOME-CITY                PIC X(30).                   182-211
003700     05  PS-HOME-STATE               PIC X(02).                   212-213
003800     05  PS-HOME-ZIP                 PIC X(10).                   214-223
003900     05  PS-MAIL-CODE                PIC X(04).                   224-227
004000     05  PS-WORK-PHONE               PIC X(10).                   228-237
004100     05  PS-EMPLOYEE-STATUS          PIC X(01).                   238
004200     05  PS-ACTION-CODE              PIC X(03).                   239-241
004300     05  PS-REGULAR-TEMPORARY        PIC X(01).                   242
004400     05  PS-FULL-PARTTIME            PIC X(01).                   243
004500     05  PS-JOB-CODE                 PIC X(06).                   244-249
004600     05  PS-JOB-TITLE                PIC X(30).                   250-279
004700     05  PS-OFFICER-CODE             PIC X(01).                   280
004800     05  PS-HIRE-DATE                PIC X(08).                   281-288
004900     05  PS-REHIRE-DATE              PIC X(08).                   289-296
005000     05  PS-TERMINATION-DATE         PIC X(08).                   297-304
005100     05  PS-LAST-DATE-WORKED         PIC X(08).                   305-312
005200     05  PS-STOCK-DEDUCTION-CODE     PIC X(06).                   313-318
005300     05  PS-SOURCE-CODE              PIC X(03).                   319-321
005400     05  PS-SOURCE-DESCRIPTION       PIC X(30).                   322-351
005500     05  PS-LOCATION                 PIC X(05).                   352-356
005600     05  PS-LOCATION-DESCRIPTION     PIC X(10).                   357-366
005700     05  PS-LOCATION-CITY            PIC X(30).                   367-396
005800     05  PS-WORK-HOURS               PIC 99V99.                   397-400
005900     05  PS-POSITION-NUMBER          PIC X(8).                    401-408
006000     05  PS-MANAGER-ID               PIC X(9).                    409-417
006100     05  PS-MANAGER-NAME             PIC X(30).                   418-447
006200     05  PS-LOCATION-ADDR1           PIC X(35).                   448-482
006300     05  PS-LOCATION-ADDR2           PIC X(35).                   483-517
006400     05  PS-LOCATION-STATE           PIC X(02).                   518-519
006500     05  PS-LOCATION-ZIP             PIC X(10).                   520-529
006600     05  PS-LOCATION-CNTRY           PIC X(15).                   530-544
006700     05  PS-LOCATION-LONG-DESCR      PIC X(30).                   545-574
006800     05  PS-S-ORG-LEVEL4             PIC X(10).                   575-584
006900     05  PS-PREFERRED-NAME           PIC X(30).                   585-614
007000     05  PS-S-EXP-BUS-UNIT           PIC X(10).                   615-624
007100     05  PS-S-EXP-BUS-NAME           PIC X(30).                   625-654
007200     05  PS-S-EXP-BUS-GROUP          PIC X(10).                   655-664
007300     05  PS-S-EXP-BUS-GRP-NAME       PIC X(30).                   665-694
007400     05  PS-S-EXP-HIER-LEVEL4        PIC X(10).                   695-704
007500     05  PS-S-EXP-HIER-LVL4-NAME     PIC X(30).                   705-734
007600     05  PS-MANAGER-LEVEL            PIC X(02).                   735-736
007700     05  PS-MANAGER-LEVEL-DESCR      PIC X(10).                   737-746
007800     05  PS-TSUSERID                 PIC X(08).                   747-754
007900     05  PS-HOME-PHONE               PIC X(24).                   755-778
008000     05  PS-FAX                      PIC X(24).                   779-802
008100     05  PS-FLAG                     PIC X(01).                   803-803
008200     05  PS-LOCATION-PHONE           PIC X(24).                   804-827
008300     05  PS-EMAIL                    PIC X(70).                   828-897
008400     05  FILLER                      PIC X(6).                    898-903
008500     05  PS-OFFICE-TYPE              PIC X(3).                    904-906
008600     05  PS-SUPPORT-LOCATION         PIC X(10).                   907-916
008700     05  FILLER                      PIC X(84).                   917-1000
