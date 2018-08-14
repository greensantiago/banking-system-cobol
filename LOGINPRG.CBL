      *****************************************************************
      * LOGIN MODULE                                                  *
      ***************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGINPRG.
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
       01  OTHER-SW                  PIC X VALUE 'Y'.
       01  LOCKED-CTR                PIC 9 VALUE ZEROES.
       01  WS-STRING                PIC X(60) VALUE SPACES. 
       01  WS-STRING-S              PIC X(30) VALUE SPACES.      
       01  WS-CENTER-COL            PIC 9(02) VALUE ZEROES.
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
            DISPLAY G-LOGINPRG.
            DISPLAY G-USER-ID.
            ACCEPT G-USER-ID.
      *****************************************************************
       A500-PROCESS-RTN.
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
                            LINE 22 COLUMN 2
                    DISPLAY 'TRY OTHER USER? Y/N: '
                            LINE 23 COLUMN 2
                    ACCEPT OTHER-SW LINE 2
                 NOT INVALID KEY
                    PERFORM A600-CHECK-RTN
            END-READ.
      *****************************************************************
       A600-CHECK-RTN.
           IF USER-PASSWORD NOT = WS-USER-PASS
              IF RECORD-LOCK = 1
                 DISPLAY 'ACCOUNT IS LOCKED DUE TO MULTIPLE ATTEMPTS'
                         LINE 23 COL 2 ERASE EOL
                 DISPLAY 'TRY OTHER USER? Y/N: '
                          LINE 23 COLUMN 2
                 ACCEPT OTHER-SW LINE 2
              ELSE IF USER-PASSWORD-ATTEMPT > 3
                 ADD 1 TO USER-PASSWORD-ATTEMPT
                 DISPLAY 'INCORRECT PASSWORD'
                         LINE 22 COL 2
                 DISPLAY 'TRY OTHER USER? Y/N: '
                            LINE 23 COLUMN 2
                    ACCEPT OTHER-SW LINE 2
              END-IF
           ELSE IF WS-USER-ID = 'ADMIN'
              CALL 'ADMIMENU'
                    ON EXCEPTION
                       DISPLAY 'ADMIMENU IS NOT YET AVAILABLE'
              END-CALL
           END-IF.
       A900-CLOSE-RTN.
           CLOSE USER-FILE.
           STOP RUN.
      