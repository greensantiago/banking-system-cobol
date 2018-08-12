       IDENTIFICATION DIVISION.
       PROGRAM-ID. CENTER IS INITIAL.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. TRAINING-PC.
       OBJECT-COMPUTER. TRAINING-PC.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STRING1                PIC X(60) VALUE SPACES.
       01 WS-STRING2                PIC X(60) VALUE SPACES.
       01 WS-CTR                    PIC 9(02) VALUE ZEROES.
       
       LINKAGE SECTION.
       01 LS-NAME                   PIC X(60).
       01 LS-CENTER                 PIC 9(02).
       
       PROCEDURE DIVISION USING LS-NAME, LS-CENTER.
       100-MAIN-MODULE.
           UNSTRING LS-NAME DELIMITED BY '     '
             INTO WS-STRING1 COUNT IN WS-CTR
                  WS-STRING2 
           END-UNSTRING.       
           COMPUTE LS-CENTER = (80 - WS-CTR) / 2.
           EXIT PROGRAM.
           