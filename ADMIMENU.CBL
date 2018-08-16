       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADMIMENU IS INITIAL.
       AUTHOR. GERAD CARLOS TUPAZ.
      *****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  OPTION PIC X VALUE SPACES.
       01  DUMMY  PIC X VALUE SPACES.
       01  WS-WELCOME               PIC X(50) VALUE SPACES.
       01  EXIT-SW PIC X VALUE 'N'.
       01  ALPHA.                      
           05 ALPHALOWER            PIC X(26)
                                    VALUE 'abcdefghijklmnopqrstuvwxyz'.
           05 ALPHAUPPER            PIC X(26)
                                    VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
      *****************************************************************
       LINKAGE SECTION.
       01 LS-NAME PIC X(70).
       01 LS-USER-ID PIC X(15).
      *****************************************************************
       SCREEN SECTION.
       COPY "ADMIMENU.ss".
      *****************************************************************
       PROCEDURE DIVISION USING LS-NAME, LS-USER-ID.
       A100-MAIN-MODULE.
           PERFORM A200-PROCESS-RTN UNTIL EXIT-SW='Y'.
           EXIT PROGRAM.
      *     STOP RUN.
       A200-PROCESS-RTN.
           DISPLAY G-ADMIMENU.
           INITIALIZE WS-WELCOME.
           STRING 'WELCOME ' DELIMITED BY SIZE
                  LS-NAME    DELIMITED BY '    '
                  INTO WS-WELCOME
           END-STRING.
           DISPLAY WS-WELCOME LINE 1 COL 1 ERASE EOL.
           ACCEPT G-OPTION.
           EVALUATE OPTION
             WHEN '1'
               CALL 'ADDACCT'
                    ON EXCEPTION 
                       DISPLAY 'ADDACCT NOT YET AVAILABLE'
                               LINE 23 COLUMN 1
                       DISPLAY 'PRESS ENTER TO CONTINUE. . .' 
                               LINE 24 COLUMN 1 
                       ACCEPT DUMMY LINE 24 
               END-CALL
             WHEN '2'
               CALL 'ADDUSER'
                    ON EXCEPTION 
                       DISPLAY 'ADDUSER NOT YET AVAILABLE' 
                           LINE 23 COLUMN 1 ERASE EOL
                       DISPLAY 'PRESS ENTER TO CONTINUE. . .'
                          LINE 24 COLUMN 1 ERASE EOL
                       ACCEPT DUMMY LINE 24
               END-CALL     
             WHEN '3'
               CALL 'UNLCKPRG'
                    ON EXCEPTION 
                       DISPLAY 'UNLCKPRG NOT YET AVAILABLE' 
                            LINE 23 COLUMN 1 ERASE EOL
                       DISPLAY 'PRESS ENTER TO CONTINUE. . .'
                          LINE 24 COLUMN 1 ERASE EOL
                       ACCEPT DUMMY LINE 24
               END-CALL
             WHEN '4'
               CALL 'ADDBILL'
                    ON EXCEPTION 
                       DISPLAY 'ADDBILL NOT YET AVAILABLE'
                            LINE 23 COLUMN 1 ERASE EOL
                       DISPLAY 'PRESS ENTER TO CONTINUE. . .' 
                          LINE 24 COLUMN 1 
                       ACCEPT DUMMY LINE 24
               END-CALL
             WHEN '5'
               CALL 'LOCKLIST'
                    ON EXCEPTION 
                       DISPLAY 'LOCKLIST NOT YET AVAILABLE'
                            LINE 23 COLUMN 1 ERASE EOL
                       DISPLAY 'PRESS ENTER TO CONTINUE. . .' 
                          LINE 24 COLUMN 1 
                       ACCEPT DUMMY LINE 24
               END-CALL
             WHEN '0'
               DISPLAY 'ARE YOU SURE YOU WANT TO LOG-OUT? Y/N: '
                       LINE 23 COL 2 ERASE EOL
               ACCEPT EXIT-SW LINE 23
               INSPECT EXIT-SW CONVERTING ALPHALOWER TO ALPHAUPPER
             WHEN OTHER
                  DISPLAY 'OPTION OUT OF RANGE'
                       LINE 23 COLUMN 1 ERASE EOL
                  ACCEPT DUMMY LINE 24 
           END-EVALUATE.
      
      
      
      