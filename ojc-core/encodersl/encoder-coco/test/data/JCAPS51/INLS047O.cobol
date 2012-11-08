      *****************************************************************
      **                                                             **
      **           FOUNDATION FOR COOPERATIVE PROCESSING             **
      **                                                             **
      **                     COBOL  COPYBOOK                         **
      **                                                             **
      **           COPYRIGHT (C) 1996 ANDERSEN CONSULTING.           **
      **                    ALL RIGHTS RESERVED.                     **
      **                                                             **
      *****************************************************************
      **                                                             **
      **      COPYBOOK: INLS047O                                     **
      **                                                             **
      **  COPYBOOK FOR: Copybook.INLS047O BEREGN NOEGLETAL PPLN      **
      **                                                             **
      **  GENERATED ON: Wed Feb 04 14:14:12 2004                     **
      **                                                             **
      **  SHORT DESCRIPTION:                                         **
      **                                                             **
      **            BY: ENI                                          **
      **                                                             **
      *****************************************************************
       03  OUTPUT-DATA.
         04  STATUSCODES.
           05  RETURNCODE                         PIC S9(4) COMP.
           05  REASONCODE                         PIC S9(4) COMP.
           05  CONCURRENCY-TIME                   PIC X(14).
       04  O-INLS047O.
         05  O-INRG047.
           07  O-KUGSKAT-BRT                     PIC S9(13)V9(2) COMP-3.
           07  O-KUGSKAT-NET                     PIC S9(13)V9(2) COMP-3.
           07  O-1-YDELSE-F-SKAT                 PIC S9(13)V9(2) COMP-3.
           07  O-1-YDELSE-E-SKAT                 PIC S9(13)V9(2) COMP-3.
           07  O-NUVAERDI-YDELSER                PIC S9(13)V9(2) COMP-3.
           07  O-EFK-RT-FOER-SKAT                PIC S9(13)V9(2) COMP-3.
           07  O-EFK-RT-EFT-SKAT                 PIC S9(13)V9(2) COMP-3.
           07  O-OVRFRT-KUF                      PIC S9(13)V9(2) COMP-3.
           07  O-MISTET-KUF                      PIC S9(13)V9(2) COMP-3.
           07  O-DISKONT-FAKTOR-BRF              PIC S9(13)V9(2) COMP-3.
           07  O-SAMLET-LAANE-PRO                PIC S9(13)V9(2) COMP-3.
           07  O-DISKONT-FAKTOR-DEB              PIC S9(13)V9(2) COMP-3.
           07  O-BRF-NPV                         PIC S9(13)V9(2) COMP-3.
           07  O-NUTIDVRD-AF-BDRG                PIC S9(13)V9(2) COMP-3.
           07  O-NETTO-HUSLEJE                   PIC S9(13)V9(2) COMP-3.
           07  O-BRUTTO-HUSLEJE                  PIC S9(13)V9(2) COMP-3.
           07  O-UGAR-TERMBET                    PIC S9(13)V9(2) COMP-3.
           07  O-UGAR-KONTVAERDI                 PIC S9(13)V9(2) COMP-3.
         05  O-VALUTA-KD                         PIC X(3).
           88  O-C8X-DANSKE-KRONER                 VALUE 'DKK'.
           88  O-C8X-EURO                          VALUE 'EUR'.
           88  O-C8X-OBLIGATION                    VALUE 'OBL'.
         05  O-ANTAL-ADVARSLER                   PIC S9(4) COMP-3.
         05  O-NZRG074.
           07  O-ADVARSEL-KODE
                            OCCURS 10 TIMES      PIC 9(5).

