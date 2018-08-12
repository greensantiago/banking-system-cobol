      *****************************************************************
      * THIS PROGRAM WILL DISPLAY CUSTOMER MENU                       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMENU.
       AUTHOR. AGATHA BACANI IS INITIAL.
       DATE-WRITTEN. 8 10 2018.
      *****************************************************************
       ENVIRONMENT DIVISION.
      *****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SELECTED              PIC X VALUE SPACES.
       01  EXIT-SW                  PIC X VALUE 'N'.
       01  DUMMY                    PIC X VALUE SPACES.
       01  ALPHA.                      
           05 ALPHALOWER            PIC X(26)
                                    VALUE 'abcdefghijklmnopqrstuvwxyz'.
           05 ALPHAUPPER            PIC X(26)
                                    VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
      *****************************************************************
       SCREEN SECTION.
       COPY "CUSTMENU.ss".
      *****************************************************************
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           PERFORM 500-PROCESS-RTN UNTIL EXIT-SW='Y'.
           EXIT PROGRAM.
           STOP RUN.
       500-PROCESS-RTN.
           DISPLAY G-CUSTMENU.
           ACCEPT G-OPTION.
           EVALUATE WS-SELECTED
             WHEN '1'
               CALL 'BALPRG'
                    ON EXCEPTION 
                       DISPLAY 'BALPRG NOT YET AVAILABLE'
                               LINE 23 COLUMN 2
                       DISPLAY 'PRESS ENTER TO CONTINUE. . .' 
                               LINE 24 COLUMN 2 
                       ACCEPT DUMMY LINE 24 
                       
               END-CALL
             WHEN '2'
               CALL 'FUNDPRG'
                    ON EXCEPTION 
                       DISPLAY 'FUNDPRG NOT YET AVAILABLE' 
                           LINE 23 COLUMN 2 ERASE EOL
                       DISPLAY 'PRESS ENTER TO CONTINUE. . .'
                          LINE 24 COLUMN 2 ERASE EOL
                       ACCEPT DUMMY LINE 24
                      
               END-CALL     
             WHEN '3'
               CALL 'PAYMENU'
                    ON EXCEPTION 
                       DISPLAY 'UNLOCK NOT YET AVAILABLE' 
                            LINE 23 COLUMN 2 ERASE EOL
                       DISPLAY 'PRESS ENTER TO CONTINUE. . .'
                          LINE 24 COLUMN 2 ERASE EOL
                       ACCEPT DUMMY LINE 24
                       
               END-CALL
             WHEN '0'
               DISPLAY 'ARE YOU SURE YOU WANT TO LOG-OUT? Y/N:'
                       LINE 24 COL 2 ERASE EOL
               ACCEPT EXIT-SW LINE 24
               INSPECT EXIT-SW CONVERTING ALPHALOWER TO ALPHAUPPER
               DISPLAY ' ' LINE 13 COL 27
             WHEN OTHER 
                  DISPLAY 'OPTION OUT OF RANGE'
                       LINE 23 COLUMN 2 ERASE EOL
                  ACCEPT DUMMY LINE 24 
                  
           END-EVALUATE.
      
      