       01  USER-REC.
           02 USER-ID PIC X(15).
           02 USER-NAME.
              03 USER-LNAME  PIC X(15).
              03 USER-FNAME  PIC X(15).
              03 USER-MNAME  PIC X(15).
           02 USER-PASSWORD  PIC X(15).
           02 USER-PASSWORD1 PIC X(15).
           02 USER-PASSWORD2 PIC X(15).
           02 USER-PASSWORD3 PIC X(15).
           02 USER-PASSWORD-ATTEMPT PIC 99.
           02 RECORD-LOCK    PIC X.