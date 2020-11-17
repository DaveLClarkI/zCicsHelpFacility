       01  SYHELPD-RECORD.
         03  HELPD-KEY.
           05  HELPD-TRAN              PIC  X(4).
           05  HELPD-NAME              PIC  X(8).
           05  HELPD-SEQU              PIC S9(5)    PACKED-DECIMAL.
         03  HELPD-TABLE.
           05  HELPD-ENTRY             PIC  X(08)   OCCURS 12.
         03  HELPD-DATA      REDEFINES HELPD-TABLE
                                       PIC  X(64).
