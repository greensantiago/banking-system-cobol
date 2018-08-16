      *****************************************************************
      * LOGIN MODULE                                                  *
      ***************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGINPRG IS INITIAL.
       AUTHOR. AGATHA BACANI.
       DATE-WRITTEN. 8 11 2018.
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT USER-FILE ASSIGN TO 'USER.DAT'
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS USER-ID.
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       COPY USERFILE.
      *****************************************************************
       WORKING-STORAGE SECTION.
       COPY "LOGINPRG.wks".
       COPY ALPHA2.
       01  OTHER-SW                 PIC X VALUE 'Y'.
       01  WS-STRING                PIC X(60) VALUE SPACES. 
       01  WS-STRING-S              PIC X(30) VALUE SPACES.      
       01  WS-CENTER-COL            PIC 9(02) VALUE ZEROES.
       01  ERROR-CTR                PIC 9(02) VALUE ZEROES.
       01  ERROR-MSG                PIC X(70) VALUE SPACES.
       01  DUMMY                    PIC X VALUE SPACES. 
       01  WS-NAME                  PIC X(50) VALUE SPACES.
      *****************************************************************
       01 MONTH-TABLE.
          05 MONTH-TAB.
             10 F PIC X(27) VALUE 'JANUARY  FEBRUARY MARCH'.
             10 F PIC X(27) VALUE 'APRIL    MAY      JUNE '.
             10 F PIC X(27) VALUE 'JULY     AUGUST   SEPTEMBER'.
             10 F PIC X(27) VALUE 'OCTOBER  NOVEMBER DECEMBER'.
          05 MONTH-NAME REDEFINES MONTH-TAB OCCURS 12 TIMES PIC X(9).  
       01 DAY-TABLE.
          05 DAY-TAB.
             10 F PIC X(30) VALUE 'MONDAY    TUESDAY   WEDNESDAY '.
             10 F PIC X(30) VALUE 'THURSDAY  FRIDAY    SATURDAY'.
             10 F PIC X(10) VALUE 'SUNDAY'.
          05 DAY-NAME REDEFINES DAY-TAB OCCURS 7 TIMES PIC X(10).
       01  SYSTEM-DATE VALUE ALL ZEROES.
           05 SYS-YY PIC 99.
           05 SYS-MM PIC 99.
           05 SYS-DD PIC 99.
       01  SYS-DAY-OF-WEEK PIC 9 VALUE ZEROES.
       01  WS-DATE-TODAY   PIC X(40) VALUE SPACES.
      *****************************************************************
       SCREEN SECTION.
       COPY "LOGINPRG.ss".
      *****************************************************************
       PROCEDURE DIVISION.
       A100-MAIN-MODULE.
           PERFORM A300-INITIAL-RTN.
           PERFORM A500-PROCESS-RTN UNTIL OTHER-SW = 'N'.
           PERFORM A900-CLOSE-RTN.
      *****************************************************************
       A300-INITIAL-RTN.
            OPEN I-O USER-FILE.
      *****************************************************************
       A500-PROCESS-RTN.
           DISPLAY G-LOGINPRG.
           DISPLAY G-USER-ID.
           ACCEPT G-USER-ID.
           DISPLAY G-USER-PASS.
           ACCEPT G-USER-PASS.
           INSPECT WS-USER-ID
                   CONVERTING LOWERCASE TO UPPERCASE.
           INSPECT WS-USER-PASS
                    CONVERTING LOWERCASE TO UPPERCASE.
           INSPECT WS-USER-PASS
                   CONVERTING UPPERCASE TO ENCRYPT-ALPHA.
           MOVE WS-USER-ID TO USER-ID.
           READ USER-FILE
                 INVALID KEY
                    DISPLAY 'USER DOES NOT EXIST'
                            LINE 22 COL 2 ERASE EOL
                    PERFORM A850-TRY-OTHER-USER
                 NOT INVALID KEY
                    PERFORM A600-CHECK-RTN
           END-READ.
      *****************************************************************
       A600-CHECK-RTN.
           IF USER-PASSWORD NOT = WS-USER-PASS
              MOVE 3 TO ERROR-CTR
              IF USER-PASSWORD-ATTEMPT < 2
                 ADD 1 TO USER-PASSWORD-ATTEMPT
                 SUBTRACT USER-PASSWORD-ATTEMPT FROM ERROR-CTR
                          GIVING ERROR-CTR
                 INITIALIZE ERROR-MSG
                 STRING 'INCORRECT PASSWORD ' DELIMITED BY SIZE
                         ERROR-CTR            DELIMITED BY SIZE
                         'TRY REMAINING'      DELIMITED BY SIZE
                         INTO ERROR-MSG
                 END-STRING
                 DISPLAY ERROR-MSG LINE 22 COL 02 ERASE EOL
                 ACCEPT DUMMY LINE 22
                 REWRITE USER-REC
                    INVALID KEY
                       DISPLAY 'ERROR IN WRITING RECORDS'
                               LINE 22 COL 02 ERASE EOL
                       PERFORM A850-TRY-OTHER-USER
                 END-REWRITE
              ELSE
                 DISPLAY 
                 'ACCOUNT IS LOCKED DUE TO MULTIPLE FAILED LOGIN'
                 LINE 22 COL 02 ERASE EOL
                 ADD 1 TO USER-PASSWORD-ATTEMPT
                 MOVE '1' TO RECORD-LOCK
                 REWRITE USER-REC
                    INVALID KEY
                       DISPLAY 'ERROR IN WRITING RECORDS'
                               LINE 22 COL 02 ERASE EOL
                       PERFORM A850-TRY-OTHER-USER
                 END-REWRITE
                 PERFORM A850-TRY-OTHER-USER
              END-IF
            ELSE
              IF RECORD-LOCK = 1
                 DISPLAY 'ACCOUNT LOCKED. CONTACT YOUR ADMINISTRATOR.'
                          LINE 22 COL 02 ERASE EOL
                 PERFORM A850-TRY-OTHER-USER
              ELSE
                 MOVE 0 TO USER-PASSWORD-ATTEMPT
                 REWRITE USER-REC
                 END-REWRITE
                 PERFORM 100-SHOW-FOOTERS-RTN
                 MOVE 'N' TO OTHER-SW
                 PERFORM A900-CLOSE-RTN
                 PERFORM 200-CALL-MENU-RTN
              END-IF
           END-IF.
      *****************************************************************
       A850-TRY-OTHER-USER.
           DISPLAY 'DO YOU WANT TO TRY OTHER USER? Y/N: '
                   LINE 23 COL 2 ERASE EOL.
           ACCEPT OTHER-SW LINE 23.
           INSPECT OTHER-SW CONVERTING LOWERCASE TO UPPERCASE.
      *****************************************************************
       200-CALL-MENU-RTN.
           IF USER-ID='ADMIN'
               CALL 'ADMIMENU' USING WS-NAME,
                                     WS-USER-ID 
                    ON EXCEPTION
                       DISPLAY 'ADMINMENU IS NOT YET AVAILABLE'
                                LINE 22 COL 2 ERASE EOL
                       DISPLAY 'PRESS ENTER TO CONTINUE..'
                                LINE 23 COL 2 ERASE EOL
                       ACCEPT DUMMY        
               END-CALL
           ELSE
              CALL 'CUSTMENU' USING WS-NAME,
                                    WS-USER-ID
                   ON EXCEPTION
                       DISPLAY 'CUSTMENU IS NOT YET AVAILABLE'
                                LINE 22 COL 2 ERASE EOL
                       DISPLAY 'PRESS ENTER TO CONTINUE..'
                                LINE 23 COL 2 ERASE EOL
                       ACCEPT DUMMY 
              END-CALL
           END-IF.
           PERFORM A300-INITIAL-RTN.
           DISPLAY G-LOGINPRG.
           DISPLAY G-USER-ID.
           DISPLAY G-USER-PASS.
           DISPLAY 'DO YOU WANT TO CLOSE THE PROGRAM? Y/N: ' 
                    LINE 23 COL 2 ERASE EOL.
           ACCEPT OTHER-SW LINE 23.
           DISPLAY ' ' LINE 23 COL 1 ERASE EOL.
           INSPECT OTHER-SW CONVERTING LOWERCASE TO UPPERCASE.
           IF OTHER-SW = 'Y'
              CLOSE USER-FILE
              STOP RUN
      *     ELSE
      *        DISPLAY G-USER-ID
      *        DISPLAY G-USER-PASS
      *        MOVE 'Y' TO OTHER-SW
      *        ACCEPT G-USER-ID
           END-IF.
      *****************************************************************
       100-SHOW-FOOTERS-RTN.
           INITIALIZE WS-NAME ERROR-MSG.
           STRING USER-FNAME      DELIMITED BY '  '
                  ' '             DELIMITED BY SIZE
                  USER-MNAME(1:1) DELIMITED BY SIZE
                  '. '            DELIMITED BY SIZE
                  USER-LNAME      DELIMITED BY '  '
              INTO WS-NAME
           END-STRING.
           STRING 'WELCOME ' DELIMITED BY SIZE
                  WS-NAME DELIMITED BY '    '
              INTO ERROR-MSG
           END-STRING.
           MOVE ERROR-MSG TO WS-STRING.
           CALL 'CENTRPRG' USING WS-STRING,
                               WS-CENTER-COL
           END-CALL.
           DISPLAY WS-STRING
              LINE 18 COL WS-CENTER-COL ERASE EOL.
           ACCEPT SYSTEM-DATE FROM DATE.
           ACCEPT SYS-DAY-OF-WEEK FROM DAY-OF-WEEK.
           MOVE SPACES TO ERROR-MSG.
           STRING 'TODAY IS ' DELIMITED BY SIZE
                      DAY-NAME   (SYS-DAY-OF-WEEK) DELIMITED BY SPACE
                      ', '        DELIMITED BY SIZE
                      MONTH-NAME (SYS-MM) DELIMITED BY SPACE
                      ' '        DELIMITED BY SIZE
                      SYS-DD     DELIMITED BY SIZE
                      ', 20'     DELIMITED BY SIZE
                      SYS-YY     DELIMITED BY SIZE
              INTO ERROR-MSG
           END-STRING.
           MOVE ERROR-MSG TO WS-STRING.
           CALL 'CENTRPRG' USING WS-STRING,
                               WS-CENTER-COL
           END-CALL.
           DISPLAY WS-STRING
              LINE 19 COL WS-CENTER-COL ERASE EOL.
           ACCEPT DUMMY LINE 24.
      *****************************************************************
       A900-CLOSE-RTN.
           CLOSE USER-FILE.
      *     STOP RUN.
      