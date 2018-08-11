       01  USER-REC.
           05 USER-ID               PIC X(15).      
           05 USER-NAME.
              10 USER-LNAME         PIC X(15).
              10 USER-FNAME         PIC X(15).
              10 USER-MNAME         PIC X(15).
           05 USER-PASSWORD         PIC X(15).
           05 USER-PASSWORD1        PIC X(15).
           05 USER-PASSWORD2        PIC X(15).
           05 USER-PASSWORD3        PIC X(15).
           05 USER-PASSWORD-ATTEMPT PIC 99.
           05 RECORD-LOCK           PIC X.
           