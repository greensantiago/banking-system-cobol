       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADMIMENU.
       AUTHOR. GERAD CARLOS TUPAZ.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  OPTION PIC X VALUE SPACES.
       01  DUMMY  PIC X VALUE SPACES.
       SCREEN SECTION.
       COPY "ADMIMENU.ss".
       PROCEDURE DIVISION.
       A100-MAIN-MODULE.
           PERFORM A200-PROCESS-RTN UNTIL OPTION = '0'.
           DISPLAY 'THANK YOU' LINE 24 COLUMN 1.
           STOP RUN. 
       A200-PROCESS-RTN.
           DISPLAY G-ADMIMENU.
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
               CALL 'PAYMENU'
                    ON EXCEPTION 
                       DISPLAY 'UNLOCK NOT YET AVAILABLE' 
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
             WHEN OTHER 
                  DISPLAY 'OPTION OUT OF RANGE'
                       LINE 23 COLUMN 1 ERASE EOL
                  ACCEPT DUMMY LINE 24 
           END-EVALUATE.
      
      
      
      