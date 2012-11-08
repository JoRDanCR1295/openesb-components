       01  QMC002-COMMAREA.                                                     
           05  DADES-IN.                                                        
               10  ACCOUNT-NUMBER       PIC  X(07).                             
               10  SLACK-DATA           PIC  X(121).                            
           05  DADES-OUT                PIC  X(1024) VALUE SPACES.              
           05  DADES-OUT-W REDEFINES DADES-OUT.                                 
               10  NUM-LIN              PIC  9(02).                             
      *           * each line is 125 bytes long                                 
               10  LINIA                             OCCURS 7 TIMES.            
                   15  MONEDA           PIC  9(03).                             
                   15  CATEGORIA        PIC  9(03).                             
                   15  SALDO-COMPTABLE  PIC S9(11)V9(02).                       
                   15  SALDO-DISP       PIC S9(11)V9(02).                       
                   15  SALDO-APROXIMAT  PIC S9(11)V9(02).                       
                   15  SALDO-MIG-MESANT PIC S9(11)V9(02).                       
                   15  SALDO-MIG-ANYANT PIC S9(11)V9(02).                       
                   15  LIMIT            PIC S9(11)V9(02).                       
                   15  RETENCIO         PIC S9(11)V9(02).                       
                   15  CODIBLOC         PIC  X(03).                             
                   15  CODI-ESTAT       PIC  X(01).                             
                   15  DATA-COMPT       PIC  X(08).                             
                   15  DATA-DISP        PIC  X(08).                             
                   15  DATA-APROX       PIC  X(08).                             