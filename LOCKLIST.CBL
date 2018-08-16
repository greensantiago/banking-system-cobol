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
       01  LINECTR                   PIC 99 VALUE ZEROES.
       01  SEQ                       PIC 9(04) VALUE ZEROES.
      *****************************************************************
       SCREEN SECTION.
       COPY "LOCKLIST.ss".
      *****************************************************************
       PROCEDURE DIVISION.
       A100-MAIN-MODULE.
           PERFORM A300-INITIAL-RTN.
           PERFORM A400-READ-RTN UNTIL EOF-SW-READ = 1.
           PERFORM A500-START-TEMP-FILE.
           PERFORM A600-DISPLAY-RTN UNTIL EOF-SW-TEMP = 1.
           PERFORM A900-CLOSE-RTN.
      *****************************************************************
       A300-INITIAL-RTN.
           DISPLAY G-LOCKLIST.
           OPEN I-O USER-FILE
                    TEMP-USER-FILE.
           MOVE 0 TO T-NO.
           PERFORM 100-READ-USER-FILE-RTN.
      *****************************************************************
       A400-READ-RTN.
           IF RECORD-LOCK = 1
              ADD 1 TO T-NO
              MOVE USER-ID TO T-USER-ID
              INITIALIZE WS-NAME
              STRING USER-LNAME        DELIMITED BY '  '
                     ', '              DELIMITED BY SIZE
                     USER-FNAME        DELIMITED BY '  '
                     ' '               DELIMITED BY SIZE
                     USER-MNAME (1:1) DELIMITED BY SIZE
                     '.'               DELIMITED BY SIZE
                     INTO WS-NAME
              END-STRING
              MOVE WS-NAME TO T-USER-NAME
              WRITE TEMP-USER-REC
                    INVALID KEY
                       DISPLAY 'ERROR IN WRITING RECORDS'
                               LINE 22 COL 2 ERASE EOL
                       ACCEPT DUMMY
      *              NOT INVALID KEY
      *                 DISPLAY T-NO LINE 23 COL 2 ERASE EOL
      *                 ACCEPT DUMMY
           END-IF.
           PERFORM 100-READ-USER-FILE-RTN.
      *****************************************************************
       100-READ-USER-FILE-RTN.
           READ USER-FILE NEXT RECORD
                AT END
                   MOVE 1 TO EOF-SW-READ
            END-READ.
      *****************************************************************
       200-READ-TEMP-USER-RTN.
           READ TEMP-USER-FILE NEXT RECORD
                AT END
                  MOVE 1 TO EOF-SW-TEMP
           END-READ.
      *****************************************************************
       A500-START-TEMP-FILE.
           IF EOF-SW-TEMP NOT = 1
              DISPLAY 'PRESS ENTER TO CONTINUE..'
                      LINE 23 COL 2
              ACCEPT DUMMY      
           END-IF.
           MOVE 1 TO SEQ.
           MOVE SEQ TO T-NO.
           START TEMP-USER-FILE
               INVALID KEY
                 DISPLAY T-NO LINE 24 COL 2 ERASE EOL
                 DISPLAY 'NOT FOUND' LINE 24
               NOT INVALID KEY
                 PERFORM 200-READ-TEMP-USER-RTN
           END-START.
      *****************************************************************
       A600-DISPLAY-RTN.
           PERFORM VARYING LINECTR FROM 9 BY 1 UNTIL LINECTR > 20
                   OR EOF-SW-TEMP = 1
              DISPLAY T-NO        LINE LINECTR COL 3 ERASE EOL
              DISPLAY T-USER-ID   LINE LINECTR COL 8 ERASE EOL
              DISPLAY T-USER-NAME LINE LINECTR COL 24 ERASE EOL
              PERFORM 200-READ-TEMP-USER-RTN
           END-PERFORM.
      *****************************************************************
       A900-CLOSE-RTN.     
           CLOSE USER-FILE TEMP-USER-FILE.
           STOP RUN.
      *****************************************************************