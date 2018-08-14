       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING-SYSTEM.
       AUTHOR. EJ AGATHA GERARD.
       INSTALLATION. BPI.
       DATE-WRITTEN. 08/08/2018.
       DATE-COMPILED.
       SECURITY. TELLER.
      *THIS PROGRAM WILL ADD NEW ACCOUNT 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       OBJECT-COMPUTER. DELL.
       SOURCE-COMPUTER. DELL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
             SELECT ACCOUNT-FILE ASSIGN TO 'ACCOUNT.DAT'
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS ACCOUNT-NUMBER
                    ALTERNATE RECORD KEY IS USER-ID-ACCT
                    WITH DUPLICATES.
             SELECT USER-FILE ASSIGN TO 'USER.DAT'
                    ORGANIZATION IS INDEXED 
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS USER-ID.    
       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNT-FILE.
       COPY ACCOUNT.
       FD USER-FILE.
       COPY USERFILE.
       WORKING-STORAGE SECTION.
       01 EXIT-SW PIC X VALUE SPACES.
       01 SAVE-SW PIC X.
       01 ACCOUNT-NO PIC 9(10).
       01 ACC-NO1 PIC 9(4).
       01 ACC-NO2 PIC 9(4).
       01 ACC-NO3  PIC 9(2).
       01 ACC-NAME-S PIC X(30).
       01 ACC-TYPE-S PIC X.
       01 CONTACT-ADD PIC X(50).
       01 CONTACT-NUM PIC 9(11).
       01 CONTACT-EMAIL-ADD PIC X(30).
       01 ADMIN-USER-ID-S PIC X(15).
       01 ACC-PIN-S PIC 9(4).
       01 ACC-BALANCE-S PIC S9(9)V99.
       01 DUMMY PIC X.
       SCREEN SECTION.
       COPY "ADDACC.ss".
       PROCEDURE DIVISION.
           A1000-MAIN-MODULE.
       	 PERFORM A2000-INITIAL-RTN.
      	 PERFORM A5000-PROCESS-RTN UNTIL EXIT-SW ='Y'.
      	 CLOSE ACCOUNT-FILE
      	       USER-FILE.
       	 EXIT PROGRAM.
        A2000-INITIAL-RTN.
      *      DISPLAY G-ADDACC.
      *     ACCEPT G-ADDACC.
            OPEN I-O ACCOUNT-FILE
                INPUT USER-FILE.
           
        A5000-PROCESS-RTN.
           INITIALIZE ACC-NO1,
                      ACC-NO2,
                      ACC-NO3,
                      ACC-NAME-S,
                      ACC-TYPE-S,
                      CONTACT-ADD,
                      CONTACT-NUM,
                      CONTACT-EMAIL-ADD,
                      ADMIN-USER-ID-S,
                      ACC-PIN-S,
                      ACC-BALANCE-S.     
           DISPLAY G-ADDACC.
               
           ACCEPT ACC-NO1  WITH PROMPT AUTO REQUIRED LINE 6 COL 21.
           ACCEPT ACC-NO2  WITH PROMPT AUTO REQUIRED LINE 6 COL 26.
           ACCEPT ACC-NO3  WITH PROMPT REQUIRED LINE 6 COL 31.
      
           STRING  ACC-NO1 DELIMITED BY SIZE
                          ACC-NO2 DELIMITED BY SIZE
                          ACC-NO3 DELIMITED BY SIZE
                          INTO ACCOUNT-NO
           END-STRING.
           
           MOVE ACCOUNT-NO TO ACCOUNT-NUMBER.
           READ ACCOUNT-FILE
                      INVALID KEY
                         PERFORM A6000-CONDITION-RTN    
                      NOT INVALID KEY
                         DISPLAY 'ACCOUNT NO ALREADY EXIST.' 
                         LINE 24 COL 1   
                               
           END-READ.
      
           MOVE 'Y' TO EXIT-SW.
           
       A6000-CONDITION-RTN.
           ACCEPT ACC-NAME-S WITH PROMPT REQUIRED LINE 7 COL 21.
           ACCEPT ACC-TYPE-S WITH PROMPT REQUIRED LINE 8 COL 24. 
           ACCEPT CONTACT-ADD WITH PROMPT REQUIRED LINE 9 COL 21.
           ACCEPT CONTACT-NUM WITH PROMPT REQUIRED LINE 10 COL 21.
           ACCEPT CONTACT-EMAIL-ADD WITH PROMPT REQUIRED LINE 11 COL 21.
           ACCEPT ADMIN-USER-ID-S WITH PROMPT REQUIRED LINE 13 COL 13.
           ACCEPT ACC-PIN-S WITH PROMPT REQUIRED LINE 14 COL 13.
           ACCEPT ACC-BALANCE-S WITH PROMPT REQUIRED LINE 15 COL 23.
           INSPECT ACC-TYPE-S CONVERTING 'c,s' TO 'C,S'. 
           MOVE  ADMIN-USER-ID-S TO USER-ID.
           READ USER-FILE
                      INVALID KEY
                         DISPLAY 'USER ID DOES NOT EXIST.'
                         LINE 24 COL 1
                      NOT INVALID KEY
                           
                     IF ACC-TYPE-S NOT = 'S'
                         AND ACC-TYPE-S NOT = 'C'
                             DISPLAY 'INVALID ACCOUNT TYPE'
                             LINE 24 COL 1
                         ELSE 
                            MOVE ACCOUNT-NO TO ACCOUNT-NUMBER
                            MOVE ACC-NAME-S TO ACCOUNT-NAME
                            MOVE ACC-TYPE-S TO ACCOUNT-TYPE
                            MOVE CONTACT-ADD TO CONTACT-ADDRESS
                            MOVE CONTACT-NUM TO CONTACT-NUMBER
                            MOVE CONTACT-EMAIL-ADD 
                            TO CONTACT-EMAIL-ADDRESS
                            MOVE ADMIN-USER-ID-S TO USER-ID-ACCT
                            MOVE ACC-PIN-S TO ACCOUNT-PIN
                            MOVE ACC-BALANCE-S TO ACCOUNT-BALANCE
                            DISPLAY 'SAVE?'  
                            LINE 23 COL 1
                            ACCEPT SAVE-SW LINE 23
                        IF SAVE-SW = 'Y'
                            WRITE ACCOUNT-REC
                                 INVALID KEY
                                     DISPLAY 'ERROR IN WRITING RECORD.'
                                     
                                 NOT INVALID KEY
                                     DISPLAY 'RECORD HAS BEEN ADDED.'
                                     LINE 23 COL 1
                            END-WRITE
                        END-IF 
                    END-IF
           END-READ.
           ACCEPT DUMMY.
           
          
           
           
          
           
        
       
       
          
       
              
      