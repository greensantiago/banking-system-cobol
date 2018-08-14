       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDBILL IS INITIAL.
       AUTHOR. GERAD CARLOS TUPAZ.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BILLS-FILE ASSIGN TO 'BILLS.DAT'
                  ORGANIZATION IS INDEXED
           		ACCESS MODE IS DYNAMIC
           		RECORD KEY BILLS-CODE.   		
       DATA DIVISION.
       FILE SECTION.
       FD  BILLS-FILE.
       01  BILLS-REC.
           05 BILLS-CODE           PIC X(10).
           05 BILLS-NAME           PIC X(50).
           05 BILLS-ADDRESS        PIC X(50).
           05 BILLS-CONTACT-PERSON PIC X(30).
           05 BILLS-CONTACT-NUMBER PIC X(20).
       WORKING-STORAGE SECTION.
       01  WS-BILLS-CODE           PIC X(10) VALUE SPACES.
       01  WS-BILLS-NAME           PIC X(50) VALUE SPACES.
       01  WS-BILLS-ADDRESS        PIC X(50) VALUE SPACES.
       01  WS-BILLS-CONTACT-PERSON PIC X(30) VALUE SPACES.
       01  WS-BILLS-CONTACT-NUMBER PIC X(20) VALUE SPACES.
       01  SAVE-SW          PIC X VALUE 'N'.
       01  EXIT-SW          PIC X VALUE 'N'.
       01  DUMMY            PIC X.
       SCREEN SECTION.
       COPY "ADDBILL.ss". 
       PROCEDURE DIVISION.
       A100-MAIN-MODULE.
           DISPLAY G-ADDBILL.
           PERFORM A100-INITIAL-RTN.
           PERFORM A200-PERFORM-RTN UNTIL EXIT-SW = 'Y'.
           PERFORM A500-CLOSE-RTN.
           STOP RUN.
       A100-INITIAL-RTN.
           OPEN I-O BILLS-FILE.
           MOVE 'N' TO EXIT-SW.
       A200-PERFORM-RTN.
           DISPLAY G-ADDBILL.
           ACCEPT G-BILLS-CODE.
           MOVE WS-BILLS-CODE TO BILLS-CODE.
           READ BILLS-FILE
                INVALID KEY
                   PERFORM A300-ADD-NEW-BILL
      
                NOT INVALID KEY
                   DISPLAY 'THE BILL CODE IS ALREADY EXISTED!' 
                           LINE 22 COL 1 ERASE EOL
                   DISPLAY 'EXIT? Y/N:' 
                           LINE 23 COL 1 ERASE EOL
                   ACCEPT EXIT-SW LINE 23 
           END-READ.
           
       A300-ADD-NEW-BILL.
           ACCEPT G-BILLS-NAME.
           ACCEPT G-BILLS-ADDRESS.
           ACCEPT G-BILLS-CONTACT-PERSON.
           ACCEPT G-BILLS-CONTACT-NUMBER.
           MOVE WS-BILLS-CODE TO BILLS-CODE.
           MOVE WS-BILLS-NAME TO BILLS-NAME.
           MOVE WS-BILLS-ADDRESS TO BILLS-ADDRESS.
           MOVE WS-BILLS-CONTACT-PERSON TO BILLS-CONTACT-PERSON.
           MOVE WS-BILLS-CONTACT-NUMBER TO BILLS-CONTACT-NUMBER.
           DISPLAY 'DO YOU WANT TO SAVE THIS RECORD? Y/N: '
                   LINE 23 COL 1
           ACCEPT SAVE-SW.
           IF SAVE-SW = 'Y'
           WRITE BILLS-REC
                 INVALID KEY
                    DISPLAY 'ERROR IN WRITING RECORD' LINE 23 COL 1
                 NOT INVALID KEY
                    DISPLAY 'NEW BILL CODE IS SUCCESSFULLY ADDED' 
                             LINE 22 COL 1 ERASE EOL    
           END-WRITE 
           END-IF.
           DISPLAY 'EXIT? Y/N:' LINE 23 COL 1 ERASE EOL.
           ACCEPT EXIT-SW LINE 23.
           IF EXIT-SW = 'N'
              PERFORM A350-CLEAR-FIELDS
           END-IF.
       A350-CLEAR-FIELDS.
           DISPLAY G-BILLS-CODE.
           DISPLAY G-BILLS-NAME.
           DISPLAY G-BILLS-ADDRESS.
           DISPLAY G-BILLS-CONTACT-PERSON.
           DISPLAY G-BILLS-CONTACT-NUMBER.
       A500-CLOSE-RTN.
           CLOSE BILLS-FILE.
           EXIT PROGRAM.              