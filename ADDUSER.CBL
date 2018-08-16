       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDUSER IS INITIAL.
       AUTHOR. AGATHA BACANI.
       DATE-WRITTEN. 08 08 2018.
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT USER-FILE ASSIGN TO 'USER.DAT'
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS USER-ID.
      ****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE
           RECORD CONTAINS 123 CHARACTERS
           LABEL RECORD IS STANDARD
           DATA RECORD IS USER-REC.
       COPY USERFILE.
      *****************************************************************
       WORKING-STORAGE SECTION.
       01 WS-USERID                 PIC X(15).
       01 WS-PASSWORD               PIC X(15).
       01 WS-USER-NAME.
          05 WS-LNAME               PIC X(15).
          05 WS-FNAME               PIC X(15).
          05 WS-MNAME               PIC X(15).
       01 EXIT-SW                   PIC X VALUE 'N'.
       01 DUMMY                     PIC X VALUE SPACES.
       01 SAVE-SW                   PIC X VALUE 'Y'.
       COPY ALPHA2.
      *****************************************************************
       SCREEN SECTION.
       COPY "ADDUSER.ss".
       01 WALLPAPER BACKGROUND-COLOR 4 FOREGROUND-COLOR 7.
          05 BLANK SCREEN.
      *****************************************************************
       PROCEDURE DIVISION.
       A100-MAIN-MODULE.
           PERFORM A200-INITIAL-RTN.
           PERFORM A500-PROCESS-RTN UNTIL EXIT-SW = 'Y'.
           PERFORM A900-CLOSE-RTN.
      *     STOP RUN.
      *****************************************************************
       A200-INITIAL-RTN.
           OPEN I-O USER-FILE.
           DISPLAY G-ADDUSER.
      *****************************************************************
       A500-PROCESS-RTN.
           ACCEPT G-USER-ID.
           INSPECT WS-USERID CONVERTING LOWERCASE TO UPPERCASE.
           MOVE WS-USERID TO USER-ID.
           READ USER-FILE
              INVALID KEY
                PERFORM 100-ACCEPT-OTHER-FIELDS
              NOT INVALID KEY
                DISPLAY 'USER ALREADY EXISTS'
                        LINE 22 COL 2 ERASE EOL
                DISPLAY 'DO YOU WANT TO EXIT? Y/N: '
                        LINE 23 COL 2 ERASE EOL
                ACCEPT EXIT-SW LINE 23
                INSPECT EXIT-SW CONVERTING LOWERCASE TO UPPERCASE
                IF EXIT-SW = 'N'
                   PERFORM 900-CLEAR-FIELDS-RTN
                END-IF
           END-READ.
      *****************************************************************
       100-ACCEPT-OTHER-FIELDS.
           ACCEPT G-USER-PASSWORD.
           INSPECT WS-PASSWORD
                   CONVERTING LOWERCASE TO UPPERCASE.
           INSPECT WS-PASSWORD
                   CONVERTING UPPERCASE TO ENCRYPT-ALPHA.
           ACCEPT G-USER-LNAME.
           INSPECT WS-LNAME
                   CONVERTING LOWERCASE TO UPPERCASE.
           ACCEPT G-USER-FNAME.
           INSPECT WS-FNAME
                   CONVERTING LOWERCASE TO UPPERCASE.
           ACCEPT G-USER-MNAME.
           INSPECT WS-MNAME
                   CONVERTING LOWERCASE TO UPPERCASE.
           MOVE WS-PASSWORD TO USER-PASSWORD.
           MOVE WS-LNAME TO USER-LNAME. 
           MOVE WS-FNAME TO USER-FNAME.
           MOVE WS-MNAME TO USER-MNAME.
           MOVE 1 TO RECORD-LOCK.
           MOVE 0 TO USER-PASSWORD-ATTEMPT.
           DISPLAY 'SAVE NEW USER CREDENTIALS? Y/N: '
                    LINE 22 COL 2 ERASE EOL.
           ACCEPT SAVE-SW LINE 22.
           INSPECT SAVE-SW
                   CONVERTING LOWERCASE TO UPPERCASE.
           IF SAVE-SW='Y'
               WRITE USER-REC
                 INVALID KEY
                    DISPLAY 'ERROR IN WRITING RECORD'
                    LINE 22 COL 2 ERASE EOL
                 NOT INVALID KEY
                    DISPLAY 'NEW USER HAS BEEN ADDED'
                    LINE 22 COL 2 ERASE EOL
              END-WRITE 
           END-IF.
           DISPLAY 'DO YOU WANT TO EXIT? Y/N: '
                    LINE 23 COL 2 ERASE EOL.
           ACCEPT EXIT-SW LINE 23.
           INSPECT EXIT-SW CONVERTING LOWERCASE TO UPPERCASE.
           IF EXIT-SW = 'N'
              PERFORM 900-CLEAR-FIELDS-RTN
           END-IF.
      *****************************************************************
       900-CLEAR-FIELDS-RTN.
           DISPLAY ' ' LINE 21 COL 1 ERASE EOL.
           DISPLAY ' ' LINE 22 COL 1 ERASE EOL.
           DISPLAY ' ' LINE 23 COL 1 ERASE EOL.
           DISPLAY ' ' LINE 24 COL 1 ERASE EOL.
           DISPLAY G-USER-ID.
           DISPLAY G-USER-PASSWORD.
           DISPLAY G-USER-LNAME.
           DISPLAY G-USER-FNAME.
           DISPLAY G-USER-MNAME.
       A900-CLOSE-RTN.
            CLOSE USER-FILE.
            EXIT PROGRAM. 
       