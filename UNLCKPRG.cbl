      ****************************************************************
      * THIS PROGRAM WILL UNLOCK USER                                *
      **************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNLCKPRG IS INITIAL.
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
       01  WS-USER-ID                PIC X(15).
       01  EXIT-SW                   PIC X VALUE 'N'.
       01  DUMMY                     PIC X VALUE SPACES.
       01  UNLOCK-SW                 PIC X VALUE 'Y'.
       COPY APLHA.
      *****************************************************************
       SCREEN SECTION.
       COPY "UNLCKPRG.ss".
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
           DISPLAY G-UNLCKPRG.
      *****************************************************************
       A500-PROCESS-RTN.
           ACCEPT G-USER-ID.
           INSPECT WS-USER-ID CONVERTING ALPHALOWER TO ALPHAUPPER.
           MOVE WS-USER-ID TO USER-ID.
           READ USER-FILE
              INVALID KEY
                DISPLAY 'USER ID DOES NOT EXISTS'
                        LINE 22 COL 2 ERASE EOL
                DISPLAY 'DO YOU WANT TO EXIT? Y/N: '
                        LINE 23 COL 2 ERASE EOL
                ACCEPT EXIT-SW LINE 23
                INSPECT EXIT-SW CONVERTING ALPHALOWER TO ALPHAUPPER
                IF EXIT-SW = 'N'
                   PERFORM 900-CLEAR-FIELDS-RTN
                END-IF
              NOT INVALID KEY
                PERFORM 100-UNLOCK-USER
           END-READ.
      *****************************************************************
       100-UNLOCK-USER.
           IF RECORD-LOCK NOT = 1
              DISPLAY 'ERROR: USER IS NOT LOCKED'
                       LINE 22 COL 2 ERASE EOL
              DISPLAY 'DO YOU WANT TO EXIT? Y/N: '
                       LINE 23 COL 2 ERASE EOL
              ACCEPT EXIT-SW LINE 23
              INSPECT EXIT-SW CONVERTING ALPHALOWER TO ALPHAUPPER
              IF EXIT-SW = 'N'
                 PERFORM 900-CLEAR-FIELDS-RTN
              END-IF
           ELSE
           IF RECORD-LOCK = 1
              DISPLAY USER-LNAME 
                      LINE 13 COL 22 ERASE EOL
              DISPLAY '|'
                      LINE 13 COL 76 ERASE EOL
              DISPLAY USER-FNAME
                      LINE 14 COL 22 ERASE EOL
              DISPLAY '|'
                      LINE 14 COL 76 ERASE EOL
              DISPLAY USER-MNAME
                      LINE 15 COL 22 ERASE EOL
              DISPLAY '|' LINE 15 COL 76 ERASE EOL
              DISPLAY 'PROCEED IN UNLOCKING USER? Y/N: '
                      LINE 22 COL 2 ERASE EOL
              ACCEPT UNLOCK-SW LINE 22
              PERFORM A814-UNLOCK-USER-RTN
           END-IF.
      *****************************************************************
       A814-UNLOCK-USER-RTN.
           INSPECT UNLOCK-SW CONVERTING ALPHALOWER TO ALPHAUPPER.
           MOVE 0 TO RECORD-LOCK.
           MOVE 0 TO USER-PASSWORD-ATTEMPT.
           IF UNLOCK-SW = 'Y'
              REWRITE USER-REC
                  INVALID KEY
                      DISPLAY 'ERROR IN UPDATING RECORD.'
                               LINE 22 COL 2 ERASE EOL
                    NOT INVALID KEY
                      DISPLAY 'RECORD HAS BEEN UPDATED.'
                              LINE 22 COL 2 ERASE EOL
              END-REWRITE
           END-IF.
           DISPLAY 'DO YOU WANT TO EXIT? Y/N: '
                    LINE 23 COL 2 ERASE EOL.
           ACCEPT EXIT-SW LINE 23.
           INSPECT EXIT-SW CONVERTING ALPHALOWER TO ALPHAUPPER.
           IF EXIT-SW = 'N'
              PERFORM 900-CLEAR-FIELDS-RTN
           END-IF.
      *****************************************************************
       900-CLEAR-FIELDS-RTN.
           DISPLAY ' ' LINE 22 COL 1 ERASE EOL.
           DISPLAY ' ' LINE 23 COL 1 ERASE EOL.
           DISPLAY ' ' LINE 24 COL 1 ERASE EOL.
           DISPLAY ' ' LINE 13 COL 22 ERASE EOL.
           DISPLAY ' ' LINE 14 COL 22 ERASE EOL.
           DISPLAY ' ' LINE 15 COL 22 ERASE EOL.
      *****************************************************************
       A900-CLOSE-RTN.
            CLOSE USER-FILE.
            EXIT PROGRAM. 
            