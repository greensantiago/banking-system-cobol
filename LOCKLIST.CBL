      ****************************************************************
      * THIS PROGRAM WILL LIST LOCKED USERS AND WILL ENABLE          *
      * ADMIN TO UNLOCK THEM                                         *
      **************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOCKLIST IS INITIAL.
       AUTHOR. AGATHA BACANI.
       DATE-WRITTEN. 08 11 2018.
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
                   RECORD KEY IS T-USER-ID.
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
           05  T-NO                  PIC 9(04).
           05  T-USER-ID             PIC X(15).
           05  T-USER-NAME           PIC X(50).
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  WS-USER-ID                PIC X(15).
       01  EXIT-SW                   PIC X VALUE 'N'.
       01  DUMMY                     PIC X VALUE SPACES.
       01  UNLOCK-SW                 PIC X VALUE 'Y'.
       01  WS-NAME                   PIC X(50) VALUE SPACES.
       01  EOF-SW-READ               PIC 9 VALUE ZEROES.
       01  EOF-SW-TEMP               PIC 9 VALUE ZEROES.
      *****************************************************************
       SCREEN SECTION.
       COPY "LOCKLIST.ss".
      *****************************************************************
       PROCEDURE DIVISION.
       A100-MAIN-MODULE.
           PERFORM A300-INITIAL-RTN.
           PERFORM A400-READ-RTN UNTIL EOF-SW-READ = 1.
           PERFORM A500-PROCESS-RTN.
           PERFORM A900-CLOSE-RTN.
      *****************************************************************
       A300-INITIAL-RTN.
           OPEN I-O USER-FILE
                    TEMP-USER-FILE.
           READ USER-FILE NEXT RECORD
                AT END
                   MOVE 1 TO EOF-SW-READ
           END-READ.
           MOVE 0 TO T-NO.
      *****************************************************************
       A400-READ-RTN.
           IF RECORD-LOCK = 1
              ADD 1 TO T-NO
              MOVE USER-ID TO T-USER-ID
              INITIALIZE WS-NAME
              STRING USER-LNAME   DELIMITED BY '  '
                     ', '         DELIMITED BY SIZE
                     USER-FNAME   DELIMITED BY '  '
                     ' '          DELIMITED BY SIZE
                     USER-MNAME(46:1) DELIMITED BY SIZE
                     '.'          DELIMITED BY SIZE
                     INTO WS-NAME
              END-STRING
              MOVE WS-NAME TO T-USER-NAME
              WRITE TEMP-USER-REC
                    INVALID KEY
                       DISPLAY 'ERROR IN WRITING RECORDS'
                               LINE 22 COL 2 ERASE EOL
                       ACCEPT DUMMY
                    NOT INVALID KEY
                       DISPLAY T-NO LINE 22 COL 2 ERASE EOL
           END-IF.
           READ USER-FILE NEXT RECORD
                AT END
                   MOVE 1 TO EOF-SW-READ
           END-READ.
      *****************************************************************
       A500-PROCESS-RTN.
           DISPLAY G-LOCKLIST.
      *****************************************************************
       A900-CLOSE-RTN.
           STOP RUN.
      *****************************************************************