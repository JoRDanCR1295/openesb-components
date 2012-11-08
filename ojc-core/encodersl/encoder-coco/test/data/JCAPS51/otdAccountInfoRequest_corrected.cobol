000100******************************************************************00010001
000110* a subordinate item need not have a label                        00011001
000200******************************************************************00020001
011200 01  X30338F-FUNCTION-SEG.                                        01120000
011210     03  X30338F-MSG-LL                 PIC S9(04) COMP.          01121000
011220     03  X30338F-FUNC-TAG.                                        01122000
011230         05  X30338F-FUNC-DESC          PIC X(3).                 01123000
011240         05  X30338F-FUNC-PGMID         PIC X(4).                 01124000
011250         05  X30338F-FUNC-IN-OUT        PIC X.                    01125000
011260     03  X30338F-INPUT-DATA.                                      01126000
011270         05  X30338F-PROFILE-METHOD     PIC X(01).                01127000
011280             88  X30338F-BY-CARD        VALUE '1'.                01128000
011290             88  X30338F-BY-UCN         VALUE '2'.                01129000
011300             88  X30338F-BY-ACT         VALUE '3'.                01130000
011310             88  X30338F-BY-CIF-KEY     VALUE '4'.                01131000
011320             88  X30338F-BY-NXT-ACT-SEQ VALUE '5'.                01132000
011330         05  X30338F-ACT-KEY.                                     01133000
011340             10  X30338F-ACT-COID.                                01134000
011350                 15  X30338F-ACT-COID-N PIC 9(04).                01135000
011360             10  X30338F-ACT-PRDCT      PIC X(03).                01136000
011370             10  X30338F-ACT-NUMBER     PIC X(23).                01137000
011380         05  X30338F-CARD-KEY REDEFINES X30338F-ACT-KEY.          01138000
011390             10  X30338F-CARD-COID.                               01139000
011400                 15  X30338F-CARD-COID-N PIC 9(04).               01140000
011410                 15  FILLER              PIC X(09).               01141000
011420                 15  X30338F-CARD-PLASTIC-NBR        PIC 9(17).   01142000
011430         05  X30338F-UCN-KEY REDEFINES X30338F-ACT-KEY.           01143000
011440             10 X30338F-UCN-COID.                                 01144000
011450                15 X30338F-UCN-COID-N    PIC 9(04).               01145000
011460             10 X30338F-CUSTOMER-NO      PIC 9(15).               01146000
011470             10 FILLER                   PIC 9(11).               01147000
011480         05  X30338F-CIF-KEY REDEFINES X30338F-ACT-KEY.           01148000
011490             10  X30338F-CIF-COID.                                01149000
011500                 15  X30338F-CIF-COID-N  PIC 9(04).               01150000
011510             10  X30338F-CIF-ID          PIC X(09).              01151000
011520             10  X30338F-CIF-TIE-BREAKER.                         01152000
011530                 15 X30338F-CIF-TIE-BREAKER-N PIC 9(04).          01153000
011540             10  FILLER                     PIC 9(13).            01154000
011550         05  X30338F-NEXT-ACT-SEQ           PIC X(30).            01155000
011600                                                                  01160000


