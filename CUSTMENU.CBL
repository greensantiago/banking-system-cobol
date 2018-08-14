      *****************************************************************
      * THIS PROGRAM WILL DISPLAY CUSTOMER MENU                       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMENU IS INITIAL.
       AUTHOR. AGATHA BACANI.
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
       LINKAGE SECTION.
       01 LS-NAME PIC X(70).
       01 LS-USER-ID PIC X(15).
      *****************************************************************
       SCREEN SECTION.
       COPY "CUSTMENU.ss".
      *****************************************************************
       PROCEDURE DIVISION USING LS-NAME, LS-USER-ID.
       100-MAIN-MODULE.
           PERFORM 500-PROCESS-RTN UNTIL EXIT-SW='Y'.
           EXIT PROGRAM.
       500-PROCESS-RTN.
           
           DISPLAY G-CUSTMENU.
           DISPLAY LS-NAME LINE 1 COL 02 ERASE EOL.
           DISPLAY LS-USER-ID LINE 1 COL 01 ERASE EOL.
           ACCEPT G-OPTION.
           EVALUATE WS-SELECTED
             WHEN '1'
               CALL 'BALPRG'
                    ON EXCEPTION 
                       DISPLAY 'BALPRG NOT YET AVAILABLE'
                               LINE 22 COLUMN 2
                       DISPLAY 'PRESS ENTER TO CONTINUE. . .' 
                               LINE 23 COLUMN 2 
                       ACCEPT DUMMY LINE 23                
               END-CALL
             WHEN '2'
               CALL 'FUNDPRG'
                    ON EXCEPTION 
                       DISPLAY 'FUNDPRG NOT YET AVAILABLE' 
                           LINE 22 COLUMN 2 ERASE EOL
                       DISPLAY 'PRESS ENTER TO CONTINUE. . .'
                          LINE 23 COLUMN 2 ERASE EOL
                       ACCEPT DUMMY LINE 23              
               END-CALL     
             WHEN '3'
               CALL 'PAYMENU'
                    ON EXCEPTION 
                       DISPLAY 'UNLOCK NOT YET AVAILABLE' 
                            LINE 22 COLUMN 2 ERASE EOL
                       DISPLAY 'PRESS ENTER TO CONTINUE. . .'
                          LINE 23 COLUMN 2 ERASE EOL
                       ACCEPT DUMMY LINE 23               
               END-CALL
             WHEN '0'
               DISPLAY 'ARE YOU SURE YOU WANT TO LOG-OUT? Y/N:'
                       LINE 23 COL 2 ERASE EOL
               ACCEPT EXIT-SW LINE 23
               INSPECT EXIT-SW CONVERTING ALPHALOWER TO ALPHAUPPER
             WHEN OTHER 
                  DISPLAY 'OPTION OUT OF RANGE'
                       LINE 23 COLUMN 2 ERASE EOL
                  ACCEPT DUMMY LINE 23         
           END-EVALUATE.
      
      