      ****************************************************************
      * THIS PROGRAM WILL ADDUSER                                    *
      **************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNLCKPRG.
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
            SELECT TEMP-USER-FILE ASSIGN TO 'TEMPUSER.DAT'
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
       FD  TEMP-USER-FILE.
       01  TEMP-USER-REC.
      *****************************************************************
       WORKING-STORAGE SECTION.
       01 WS-USER-ID                PIC X(15).
       01 EXIT-SW                   PIC X VALUE 'N'.
       01 DUMMY                     PIC X VALUE SPACES.
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
           STOP RUN.
       A200-INITIAL-RTN.
           OPEN I-O USER-FILE.
           DISPLAY G-UNLCKPRG.
       A500-PROCESS-RTN.
           ACCEPT G-USER-ID.
           MOVE WS-USER-ID TO USER-ID.
           READ USER-FILE
              INVALID KEY
                PERFORM 100-UNLOCK-USER
              NOT INVALID KEY
                DISPLAY 'USER ALREADY EXISTS'
                        LINE 22 COL 2 ERASE EOL
                DISPLAY 'DO YOU WANT TO EXIT? Y/N: '
                        LINE 23 COL 2 ERASE EOL
                ACCEPT EXIT-SW LINE 23
                INSPECT EXIT-SW CONVERTING ALPHALOWER TO ALPHAUPPER
                IF EXIT-SW = 'N'
                   PERFORM 900-CLEAR-FIELDS-RTN
                END-IF
           END-READ.
      