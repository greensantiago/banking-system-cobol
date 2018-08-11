       01 G-UNLCKPRG.
         02 BACKGROUND-COLOR 4 FOREGROUND-COLOR 7.
          03 BLANK SCREEN.
          03 LINE 2 COL 26 HIGHLIGHT VALUE "BANK".
          03 COL 31 HIGHLIGHT VALUE "OF".
          03 COL 34 HIGHLIGHT VALUE "THE".
          03 COL 38 HIGHLIGHT VALUE "PHILIPPINE".
          03 COL 49 HIGHLIGHT VALUE "ISLANDS".
          03 LINE 3 COL 30 HIGHLIGHT VALUE "ADMINISTRATOR'S".
          03 COL 46 HIGHLIGHT VALUE "MODULE".
          03 LINE 5 COL 5 HIGHLIGHT VALUE "+----------------------------
      -"------------------------------------------+".
          03 LINE 6 COL 5 HIGHLIGHT VALUE "|".
          03 COL 33 HIGHLIGHT VALUE "UNLOCK".
          03 COL 40 HIGHLIGHT VALUE "USER".
          03 COL 45 HIGHLIGHT VALUE "ID".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 7 COL 5 HIGHLIGHT VALUE "+----------------------------
      -"------------------------------------------+".
          03 LINE 8 COL 5 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 9 COL 5 HIGHLIGHT VALUE "|".
          03 COL 9 HIGHLIGHT VALUE "ENTER".
          03 COL 15 HIGHLIGHT VALUE "USER".
          03 COL 20 HIGHLIGHT VALUE "ID:".
          03 COL 24 HIGHLIGHT VALUE "[".
          03 COL 40 HIGHLIGHT VALUE "]".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 10 COL 5 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 11 COL 5 HIGHLIGHT VALUE "+===========================
      -"===========================================+".
          03 LINE 12 COL 5 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 13 COL 5 HIGHLIGHT VALUE "|".
          03 COL 9 HIGHLIGHT VALUE "LAST".
          03 COL 16 HIGHLIGHT VALUE "NAME:".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 14 COL 5 HIGHLIGHT VALUE "|".
          03 COL 9 HIGHLIGHT VALUE "FIRST".
          03 COL 16 HIGHLIGHT VALUE "NAME:".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 15 COL 5 HIGHLIGHT VALUE "|".
          03 COL 9 HIGHLIGHT VALUE "MIDDLE".
          03 COL 16 HIGHLIGHT VALUE "NAME:".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 16 COL 5 HIGHLIGHT VALUE "|".
          03 COL 76 HIGHLIGHT VALUE "|".
          03 LINE 17 COL 5 HIGHLIGHT VALUE "+---------------------------
      -"-------------------------------------------+".
          03 LINE 9 COL 25 HIGHLIGHT PIC X(15) TO WS-USER-ID PROMPT
       REQUIRED SIZE 15.
       01 G-USER-ID.
          03 LINE 9 COL 25 HIGHLIGHT PIC X(15) TO WS-USER-ID PROMPT
       REQUIRED SIZE 15.