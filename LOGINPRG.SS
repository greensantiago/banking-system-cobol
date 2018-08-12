       01 G-LOGINPRG.
         02 BACKGROUND-COLOR 4 FOREGROUND-COLOR 7.
          03 BLANK SCREEN.
          03 LINE 3 COL 4 HIGHLIGHT VALUE "+----------------------------
      -"-------------------------------------------+".
          03 LINE 4 COL 4 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 5 COL 4 HIGHLIGHT VALUE "|".
          03 COL 26 HIGHLIGHT VALUE "BANK".
          03 COL 31 HIGHLIGHT VALUE "OF".
          03 COL 34 HIGHLIGHT VALUE "THE".
          03 COL 38 HIGHLIGHT VALUE "PHILIPPINE".
          03 COL 49 HIGHLIGHT VALUE "ISLANDS".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 6 COL 4 HIGHLIGHT VALUE "|".
          03 COL 26 HIGHLIGHT VALUE "<<".
          03 COL 29 HIGHLIGHT VALUE "***".
          03 COL 39 HIGHLIGHT VALUE "LOGIN".
          03 COL 50 HIGHLIGHT VALUE "***".
          03 COL 54 HIGHLIGHT VALUE ">>".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 7 COL 4 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 8 COL 4 HIGHLIGHT VALUE "+----------------------------
      -"-------------------------------------------+".
          03 LINE 9 COL 4 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 10 COL 4 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 11 COL 4 HIGHLIGHT VALUE "|".
          03 COL 25 HIGHLIGHT VALUE "USER".
          03 COL 30 HIGHLIGHT VALUE "ID".
          03 COL 34 HIGHLIGHT VALUE ":".
          03 COL 36 HIGHLIGHT VALUE "[".
          03 COL 52 HIGHLIGHT VALUE "]".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 12 COL 4 HIGHLIGHT VALUE "|".
          03 COL 25 HIGHLIGHT VALUE "PASSWORD".
          03 COL 34 HIGHLIGHT VALUE ":".
          03 COL 36 HIGHLIGHT VALUE "[".
          03 COL 52 HIGHLIGHT VALUE "]".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 13 COL 4 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 14 COL 4 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 15 COL 4 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 16 COL 4 HIGHLIGHT VALUE "+===========================
      -"============================================+".
          03 LINE 17 COL 4 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 18 COL 4 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 19 COL 4 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 20 COL 4 HIGHLIGHT VALUE "+---------------------------
      -"--------------------------------------------+".
          03 LINE 11 COL 37 HIGHLIGHT PIC X(15) TO WS-USER-ID PROMPT
       REQUIRED SIZE 15.
          03 LINE 12 COL 37 HIGHLIGHT PIC X(15) TO WS-USER-PASS PROMPT
       REQUIRED NO-ECHO SIZE 15.
       01 G-USER-ID.
          03 LINE 11 COL 37 HIGHLIGHT PIC X(15) TO WS-USER-ID PROMPT
       REQUIRED SIZE 15.
       01 G-USER-PASS.
          03 LINE 12 COL 37 HIGHLIGHT PIC X(15) TO WS-USER-PASS PROMPT
       REQUIRED NO-ECHO SIZE 15.