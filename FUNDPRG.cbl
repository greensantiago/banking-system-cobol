       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUNDPRG.
       AUTHOR. GERAD CARLOS TUPAZ.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'ACCOUNT.DAT'
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS ACCOUNT-NUMBER  
                  ALTERNATE KEY IS USER-ID-ACCT WITH DUPLICATES.
      *****************************************************************
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-REC.
           05 ACCOUNT-NUMBER        PIC 9(10).
           05 ACCOUNT-NAME          PIC X(30).
           05 ACCOUNT-TYPE          PIC X.
           05 CONTACT-ADDRESS       PIC X(50).
           05 CONTACT-NUMBER        PIC X(15).
           05 CONTACT-EMAIL-ADDRESS PIC X(30).
           05 USER-ID-ACCT          PIC X(15).
           05 ACCOUNT-PIN           PIC 9(4).
           05 ACCOUNT-BALANCE       PIC 9(9)V99.
      *****************************************************************
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  NUMBERS PIC 9(10) VALUE 2345698745.
       01  DAYA-USER-ID PIC X(15) VALUE 'geradtupaz'.
       01  ACCOUNT.
           05  SD-ACCOUNT-NUMBER    PIC X(12).
           05  WS-ACCT-NO1          PIC 9(4).
           05  WS-ACCT-NO2          PIC 9(4).
           05  WS-ACCT-NO3          PIC 9(2).
           05  WS-ACCOUNT-NO        PIC 9(10).
           05  WS-ACCOUNT-NAME      PIC X(30) VALUE SPACES.
           05  WS-PIN               PIC 9(4) VALUE ZEROES.
           05  WS-ACCOUNT-BALANCE   PIC ----,---,--9.99.
       01  ACCOUNT-TO-BE-TRANSFER.
           05  WS-AMOUNT-TRANSFER   PIC 9(9)V99 VALUE ZEROES.
           05  WS-ACCOUNT-NO-TRANS  PIC X(12) VALUE SPACES.
           05  WS-ACCOUNT1          PIC 9(04) VALUE ZEROES.
           05  WS-ACCOUNT2          PIC 9(04) VALUE ZEROES.
           05  WS-ACCOUNT3          PIC 9(02) VALUE ZEROES.
           05  SD-ACCOUNT-NO-TRANS  PIC 9(10) VALUE ZEROES.
       01  WS-SUM-AMOUNT            PIC 9(9)V99 VALUE ZEROES.
       01  WS-SUBTRACT-AMOUNT       PIC 9(9)V99 VALUE ZEROES.
       01  SWITCH.
           05 EXIT-SW               PIC X VALUE 'N'.
           05 TRANSFER-SW           PIC X VALUE 'N'.
           05 EOF-SW                PIC X VALUE 'N'.
       01  HOLD-AREA.
           05 HOLD-USER-ID          PIC X(15) VALUE SPACES.
           05 HOLD-FIRST-ACCOUNT    PIC 9(9)V99 VALUE ZEROES.
       01  DUMMY                    PIC X.
       01  TRANSFER-DISPLAY1        PIC X(80) VALUE SPACES.
       01  TRANSFER-DISPLAY2        PIC X(80) VALUE SPACES.
       01  DISPLAY-FOOTER1          PIC X(80) VALUE SPACES.
       01  DISPLAY-FOOTER2          PIC X(80) VALUE SPACES.
       01  NEW-BALANCE-FORMAT1      PIC ZZZ,ZZZ,ZZ9.99.
       01  NEW-BALANCE-FORMAT2      PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-WELCOME               PIC X(50) VALUE SPACES.
      *****************************************************************
      ***************************************************************** 
       LINKAGE SECTION.
       01  LS-NAME    PIC X(70).
       01  LS-USER-ID PIC X(15).
      *****************************************************************
      *****************************************************************     
       SCREEN SECTION.
       COPY "FUNDPRG.SS".          
      *****************************************************************
      *****************************************************************
       PROCEDURE DIVISION USING LS-NAME, LS-USER-ID.
       A100-MAIN-MODULE.
           PERFORM A200-INITIAL-RTN.
           PERFORM A300-PROCESS-RTN UNTIL EXIT-SW = 'Y'.
           PERFORM A600-CLOSE-RTN.
           STOP RUN.
       A200-INITIAL-RTN.
           OPEN I-O ACCOUNT-FILE.
       A300-PROCESS-RTN.
           DISPLAY G-FUNDPRG. 
           DISPLAY ' ' LINE 14 COL 1 ERASE EOS.
           INITIALIZE WS-WELCOME.
           STRING 'WELCOME ' DELIMITED BY SIZE
                  ' '    DELIMITED BY SIZE
           INTO WS-WELCOME
           END-STRING.
           DISPLAY WS-WELCOME LINE 1 COL 1 ERASE EOL.
           DISPLAY LS-USER-ID LINE 23 COL 1 ERASE EOL.
           MOVE DAYA-USER-ID TO HOLD-USER-ID.
           ACCEPT G-ACCT-NO.
           INITIALIZE SD-ACCOUNT-NUMBER.
           STRING WS-ACCT-NO1     DELIMITED BY SIZE
                  WS-ACCT-NO2     DELIMITED BY SIZE
                  WS-ACCT-NO3     DELIMITED BY SIZE
             INTO SD-ACCOUNT-NUMBER
           END-STRING
           MOVE SD-ACCOUNT-NUMBER TO ACCOUNT-NUMBER
           ACCEPT G-PIN.
           IF WS-PIN  NOT = LS-USER-ID
              DISPLAY 'INVALID ACCOUNT NUMBER!' LINE 23 COL 1 ERASE EOL
              DISPLAY 'PRESS ENTER TO CONTINUE...'
                      LINE 24 COL 1 ERASE EOL
              MOVE 'N' TO EXIT-SW
           ELSE
           READ ACCOUNT-FILE
                 INVALID KEY
                   MOVE 'Y' TO EXIT-SW
                 NOT INVALID KEY
                   PERFORM A350-AMOUNT-TRANSFER
           END-READ
           END-IF.
       A350-AMOUNT-TRANSFER.
           IF WS-PIN NOT = ACCOUNT-PIN
              DISPLAY 'INCORRECT PIN!' LINE 23 COL 1 ERASE EOL
              DISPLAY 'EXIT? Y/N:' LINE 24 COL 1 ERASE EOL
              ACCEPT EXIT-SW
           ELSE
              MOVE ACCOUNT-BALANCE TO WS-ACCOUNT-BALANCE
                                      HOLD-FIRST-ACCOUNT 
             DISPLAY G-ACCOUNT-BALANCE
              PERFORM A400-CHECKING-USER-ID
           END-IF.
       A400-CHECKING-USER-ID.
           ACCEPT G-AMOUNT-TRANSFER.
           ACCEPT G-TRANS-ACCOUNT-NO.
           INITIALIZE SD-ACCOUNT-NO-TRANS.
           STRING WS-ACCOUNT1 DELIMITED BY SIZE
                  WS-ACCOUNT2 DELIMITED BY SIZE
                  WS-ACCOUNT3 DELIMITED BY SIZE
              INTO SD-ACCOUNT-NO-TRANS
           MOVE SD-ACCOUNT-NO-TRANS TO ACCOUNT-NUMBER.
           READ ACCOUNT-FILE
                 INVALID KEY
                   DISPLAY 'ACCOUNT NUMBER NOT FOUND' 
                           LINE 24 COL 1 ERASE EOL
                   ACCEPT DUMMY LINE 24
                 NOT INVALID KEY
                 DISPLAY 'ARE YOU SURE YOU WANT TO TRANSFER? '
                         LINE 24 COL 1 ERASE EOL
                 ACCEPT  TRANSFER-SW LINE 24
                 IF TRANSFER-SW = 'Y'
                       IF HOLD-USER-ID = USER-ID-ACCT AND 
                          SD-ACCOUNT-NO-TRANS = ACCOUNT-NUMBER
                          COMPUTE WS-SUM-AMOUNT = ACCOUNT-BALANCE 
                                  +  WS-AMOUNT-TRANSFER
      
                          SUBTRACT WS-AMOUNT-TRANSFER
                            FROM HOLD-FIRST-ACCOUNT 
                            GIVING WS-SUBTRACT-AMOUNT
                          MOVE WS-SUM-AMOUNT TO ACCOUNT-BALANCE 
                                                NEW-BALANCE-FORMAT2
                          PERFORM A500-UPDATE-ACCOUNT-BALANCE
                       END-IF
                 END-IF
           END-READ.
       A500-UPDATE-ACCOUNT-BALANCE.
           REWRITE ACCOUNT-REC
                   INVALID KEY
                      DISPLAY 'ERROR IN UPDATING RECORD.'
                              LINE 23 COL 1 ERASE EOL
                      DISPLAY 'EXIT? Y/N:' LINE 24 COL 1 ERASE EOL
                      ACCEPT EXIT-SW    
           END-REWRITE.
           MOVE NUMBERS TO ACCOUNT-NUMBER.
           READ ACCOUNT-FILE 
                 INVALID KEY
                   DISPLAY 'ACCOUNT NUMBER NOT FOUND'
                 NOT INVALID KEY
                   MOVE WS-SUBTRACT-AMOUNT TO ACCOUNT-BALANCE
                                              NEW-BALANCE-FORMAT1
                   PERFORM A550-REWRITE-RTN
           END-READ.
       A550-REWRITE-RTN.
           REWRITE ACCOUNT-REC
                INVALID KEY
                    DISPLAY 'ERROR IN UPDATING RECORD.'
                           LINE 23 COL 1 ERASE EOL
                    DISPLAY 'EXIT? Y/N:' LINE 24 COL 1 ERASE EOL
                    ACCEPT EXIT-SW    
                NOT INVALID KEY  
                    INITIALIZE TRANSFER-DISPLAY1
                    INITIALIZE TRANSFER-DISPLAY2
                    INITIALIZE DISPLAY-FOOTER1
                    INITIALIZE DISPLAY-FOOTER2
                    STRING 'Transferred funds from Account # ' 
                           DELIMITED BY SIZE
                           WS-ACCOUNT-NO  DELIMITED BY '  '
                           ' to'           DELIMITED BY SIZE
                        INTO TRANSFER-DISPLAY1
                    END-STRING
                    
                    STRING 'Account # '        DELIMITED BY SIZE
                           SD-ACCOUNT-NO-TRANS DELIMITED BY '  '
                           ' was successfully completed.'  
                            DELIMITED BY SIZE
                        INTO TRANSFER-DISPLAY2
                    END-STRING
                    
                    DISPLAY TRANSFER-DISPLAY1 LINE 12 COL 5 ERASE EOL
                    DISPLAY TRANSFER-DISPLAY2 LINE 13 COL 17 ERASE EOL
                    
                    STRING 'Account # '        DELIMITED BY SIZE
                           WS-ACCOUNT-NO       DELIMITED BY ' '
                           ' new balance is '  DELIMITED BY SIZE
                           NEW-BALANCE-FORMAT1 DELIMITED BY SIZE
                        INTO DISPLAY-FOOTER1
                    END-STRING 
       
                    STRING 'Account # '        DELIMITED BY SIZE
                           SD-ACCOUNT-NO-TRANS DELIMITED BY ' '
                           ' new balance is '  DELIMITED BY SIZE
                           NEW-BALANCE-FORMAT2 DELIMITED BY SIZE
                        INTO DISPLAY-FOOTER2
                    END-STRING               
                    
                    DISPLAY DISPLAY-FOOTER1 LINE 22 COL 1 ERASE EOL
                    DISPLAY DISPLAY-FOOTER2 LINE 23 COL 1 ERASE EOL
                    DISPLAY 'EXIT? Y/N:' LINE 24 COL 1 ERASE EOL
                    ACCEPT EXIT-SW
           END-REWRITE.
       A600-CLOSE-RTN.
           CLOSE ACCOUNT-FILE.
           STOP RUN.
      