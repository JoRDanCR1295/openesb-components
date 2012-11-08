
       01  HEADER.       
           05 NOMPGM                           PIC X(08).
           05 MSGTYPE                          PIC X(15).
       02  DATA.
           03 PERSONNE.
           05  NOM                             PIC X(20).
           05  PRENOMS                         PIC X(40).
           05  ADRESSE.
           10 LIGNE                            PIC X(80).
           10 LIGNE2                           PIC X(80).
           10 LIGNE3                           PIC X(80).
           10 CODEPOSTAL                       PIC X(05).
           10 VILLE                            PIC X(20).
           05 TEL                              PIC X(10) OCCURS 03.
           05 EMAIL                            PIC X(80) OCCURS 03.