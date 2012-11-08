      * MVS PORT-- CODE RUN THROUGH PROCESSOR
      *            on Sat May 30 01:05:59 1998

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
      **      COPYBOOK: CUCL107M                                     **
      **                                                             **
      **  COPYBOOK FOR: Copybook.CUCL107M MAINTAIN BILL ACCT         **
      **                                                             **
      **  GENERATED ON: Tue Apr 07 10:54:53 1998                     **
      **                                                             **
      **  SHORT DESCRIPTION: CUCL107M MAINTAIN BILL ACCT             **
      **                                                             **
      **            BY: GLECAIN                                      **
      **                                                             **
      *****************************************************************
       01  CUCL107M.
         06  MX-IO-PROC-ERR-MSG                  PIC S9(5) COMP-3
                 VALUE ZERO.
         06  MX-CUST-CONTACT                     PIC S9(5) COMP-3
                 VALUE ZERO.
         06  MX-ERR-UPDT-ADDR                    PIC S9(5) COMP-3
                 VALUE ZERO.
         06  MX-ADD-UPDT-REMARKS-FAILED          PIC S9(5) COMP-3
                 VALUE ZERO.
         06  MX-INVAL-FCTN-CODE                  PIC S9(5) COMP-3
                 VALUE ZERO.
         06  MX-ERROR-UPDT-BDEL                  PIC S9(5) COMP-3
                 VALUE ZERO.
           88  MX-ERR-UPDT-BDEL                    VALUE 10922.

