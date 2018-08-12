       01  ALPHA.                      
           05 LOWERCASE             PIC X(26)
                                    VALUE 'abcdefghijklmnopqrstuvwxyz'.
           05 UPPERCASE             PIC X(26)
                                    VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           05 ENCRYPT-ALPHA         PIC X(26)
                                    VALUE 'OPQRSTUVWXYZABCDEFGHIJKLMN'.
       01  NUMBERS.
           05 DNUMBERS              PIC X(10)
                                    VALUE '1234567890'.
           05 ENCRYPT-NUM           PIC X(10)
                                    VALUE '!@#$%^&*()'.