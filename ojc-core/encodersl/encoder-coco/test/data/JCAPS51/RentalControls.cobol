       01  RentalControls.
           05  CTR  PIC 9(09).
           05  RentalControlData  OCCURS 1 TO 20 TIMES DEPENDING ON CTR.
               10  PostDate             PIC X(17).
               10  LocationID           PIC X(09).
               10  CarClass             PIC X(04).
               10  PickupStartDate      PIC X(08).
               10  PickupEndDate        PIC X(08).
               10  StartDate            PIC X(08).
               10  EndDate              PIC X(08).
               10  LOKStart             PIC X(02).
               10  LOKEnd               PIC X(02).
               10  RatePlan             PIC X(06).
               10  DeleteAdd            PIC X(01).
               10  RentType             PIC X(03).
