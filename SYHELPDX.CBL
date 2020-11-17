      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    SYHELPDX.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  DEC 2009.
       DATE-COMPILED.
       INSTALLATION.  WINWHOLESALE GROUP SERVICES.
       SECURITY.      NONE.
      *REMARKS.       PROMOTE SYHELPD RECORDS TO PRODUCTION.

      * CHANGE HISTORY ------------------------------------------------
      * 12/18/2009 DLC ORIGINAL PROGRAM.
      * 11/22/2019 DLC COMPLETE REDESIGN FROM ORIGINAL PROGRAM BECAUSE
      *                IT IS A NEW FILE NAME NOW AND THE
      *                RECORD LAYOUT HAS CHANGED DRASTICALLY.
      * END OF HISTORY ------------------------------------------------

      /*****************************************************************
      *                                                                *
      *    ENVIRONMENT DIVISION                                        *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.

      ******************************************************************
      *    CONFIGURATION SECTION                                       *
      ******************************************************************
       CONFIGURATION SECTION.

       SOURCE-COMPUTER. IBM-2086-A04-140.
       OBJECT-COMPUTER. IBM-2086-A04-140.

       SPECIAL-NAMES.
           UPSI-0 ON  STATUS IS PROMOTE-HELPD,
                  OFF STATUS IS EXTRACT-HELPD.

      ******************************************************************
      *    INPUT-OUTPUT SECTION                                        *
      ******************************************************************
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT CARDPCH
               ASSIGN TO SYS009-UR-IJSYSPH.

           SELECT CARDRDR
               ASSIGN TO SYS010-UR-IJSYSIN.

           SELECT SYHELPD
               ASSIGN TO SYHELPD
               ACCESS IS DYNAMIC INDEXED
               RECORD KEY IS HELPD-KEY
               FILE STATUS IS FILE1-STAT
                              FILE1-FDBK.

      /*****************************************************************
      *                                                                *
      *    DATA DIVISION                                               *
      *                                                                *
      ******************************************************************
       DATA DIVISION.

      ******************************************************************
      *    FILE SECTION                                                *
      ******************************************************************
       FILE SECTION.

       FD  CARDPCH.
       01  PUNCH-CARD                  PIC  X(80).

       FD  CARDRDR.
       01  INPUT-CARD.
         03  INPUT-HELPTRAN            PIC  X(04).
         03  INPUT-HELPNAME            PIC  X(08).
         03                            PIC  X(68).

       FD  SYHELPD
           RECORD IS VARYING IN SIZE
             FROM 79 TO 111 CHARACTERS
             DEPENDING ON HELPD-RECL.
       COPY SYHELPD.

      ******************************************************************
      *    WORKING-STORAGE SECTION                                     *
      ******************************************************************
       WORKING-STORAGE SECTION.

       01  WS-FIELDS.
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.
         03  LOWVALUE                  PIC  X(01)   VALUE LOW-VALUE.
         03  THIS-PGM                  PIC  X(08)   VALUE 'SYHELPDX'.

         03  HELPD                     PIC S9(04)   BINARY VALUE 1.

         03  CARDPCH-SWITCH            PIC  X(01)   VALUE 'C'.
           88  CARDPCH-CLOSED                       VALUE 'C'.
           88  CARDPCH-OPEN                         VALUE 'O'.
         03  CARDRDR-SWITCH            PIC  X(01)   VALUE 'C'.
           88  CARDRDR-CLOSED                       VALUE 'C'.
           88  CARDRDR-AT-END                       VALUE 'E'.
           88  CARDRDR-OPEN                         VALUE 'O'.
         03  RECORD-SWITCH             PIC  X(01)   VALUE SPACES.
           88  RECORD-NOT-SELECTED                  VALUE ' '.
           88  RECORD-IS-SELECTED                   VALUE 'Y'.

         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.

         03  HELPD-RECL                PIC  9(05)   VALUE ZEROES.
         03  REC-CNT                   PIC  9(07)   VALUE ZEROES.

         03  LAST-KEY                  PIC  X(12)   VALUE LOW-VALUES.
         03  LAST-SEQU                 PIC S9(5)    PACKED-DECIMAL.

       01  JCL-AREA.
         03  JCL-PRE                   PIC S9(04)   BINARY VALUE ZEROES.
         03  JCL-MAX                   PIC S9(04)   BINARY VALUE ZEROES.
       01  JCL-DATA.
         03  PIC X(40) VALUE '* $$ LST CLASS=Z                        '.
         03  PIC X(40) VALUE '// JOB    SYHELPDX   PROMOTE HELP PAGES '.
         03  PIC X(40) VALUE '// UPSI   1                             '.
         03  PIC X(40) VALUE '// ASSGN  SYS010,SYSIPT                 '.
         03  PIC X(40) VALUE '// EXEC   PGM=SYHELPDX,SIZE=*           '.
         03  PIC X(40) VALUE '###STOP###                              '.
         03  PIC X(40) VALUE '/* EOD                                  '.
         03  PIC X(40) VALUE '/& EOJ                                  '.
       01  JCL-TABLE                   REDEFINES    JCL-DATA.
         03  JCL-CARD                  PIC  X(40)   OCCURS 8.

       COPY RTCMAN.

       COPY VSMSTATW.

       COPY WILDCOMP.

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    MAINLINE ROUTINE                                            *
      ******************************************************************
       A00-MAINLINE-ROUTINE.

           PERFORM B10-INITIALIZATION THRU B15-EXIT.

           IF  EXTRACT-HELPD
               PERFORM C00-EXTRACT-HELPD THRU C90-EXIT
                 UNTIL CARDRDR-AT-END
                    OR RTC-CODE NOT = ZERO
           ELSE
               PERFORM D00-PROMOTE-HELPD THRU D90-EXIT
                 UNTIL CARDRDR-AT-END
                    OR RTC-CODE NOT = ZERO
           END-IF.

           PERFORM B20-TERMINATION THRU B25-EXIT.

           GOBACK.

      /*****************************************************************
      *    PROGRAM INITIALIZATION ROUTINE                              *
      ******************************************************************
       B10-INITIALIZATION.

           COPY BATCHINI.

           OPEN INPUT CARDRDR.
           SET  CARDRDR-OPEN           TO TRUE.

           IF  RTC-CODE = ZERO

             MOVE 'SYHELPD'            TO VSAM-FILE(HELPD)
             MOVE LENGTH OF HELPD-KEY  TO VSAM-KEYL(HELPD)

             PERFORM WITH TEST BEFORE
               VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
                 SET FUNC-OPEN(VSUB)   TO TRUE
             END-PERFORM

             IF  PROMOTE-HELPD
                 OPEN I-O    SYHELPD
             ELSE
                 OPEN INPUT  SYHELPD
             END-IF

             PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL

             IF  RTC-CODE = ZERO
                 PERFORM WITH TEST BEFORE
                   VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
                     SET FUNC-READ(VSUB) TO TRUE
                 END-PERFORM
             END-IF

             IF  RTC-CODE = ZERO
                 MOVE HELPD            TO VSUB
                 COMPUTE JCL-MAX       =  LENGTH OF JCL-DATA
                                       /  LENGTH OF JCL-CARD
             END-IF

           END-IF.

       B15-EXIT.
           EXIT.

      /*****************************************************************
      *    PROGRAM TERMINATION ROUTINE                                 *
      ******************************************************************
       B20-TERMINATION.

           IF  CARDPCH-OPEN
               COMPUTE IDX = JCL-PRE + 1
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM IDX BY 1 UNTIL IDX > JCL-MAX
                   MOVE JCL-CARD(IDX)  TO PUNCH-CARD
                   PERFORM U50-PUT-PUNCH THRU U55-EXIT
               END-PERFORM
               CLOSE CARDPCH
               SET  CARDPCH-CLOSED     TO TRUE
           END-IF.

           PERFORM WITH TEST BEFORE
             UNTIL CARDRDR-AT-END
               PERFORM U10-GET-INPUT THRU U15-EXIT
           END-PERFORM.
           CLOSE CARDRDR.
           SET  CARDRDR-CLOSED         TO TRUE.

           PERFORM WITH TEST BEFORE
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
               SET  FUNC-CLOSE(VSUB)   TO TRUE
           END-PERFORM.

           CLOSE SYHELPD.

           PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL.

           IF  RTC-CODE = ZERO
             IF  PROMOTE-HELPD
               DISPLAY THIS-PGM ':  PROMOTED RECORDS = ' REC-CNT
                                     UPON CONSOLE
             ELSE
               DISPLAY THIS-PGM ': EXTRACTED RECORDS = ' REC-CNT
                                     UPON CONSOLE
             END-IF
           END-IF.

           COPY BATCHRTN.

       B25-EXIT.
           EXIT.

      /*****************************************************************
      *    CHECK A VSAM OR WORK FILE'S STATUS                          *
      ******************************************************************
       COPY VSMSTATP.

      /*****************************************************************
      *    EXTRACT HELP PAGES                                          *
      ******************************************************************

       C00-EXTRACT-HELPD.

           PERFORM U10-GET-INPUT THRU U15-EXIT.
           IF  CARDRDR-AT-END
           OR  INPUT-HELPTRAN NOT > SPACES
               GO TO C90-EXIT
           END-IF.

           MOVE INPUT-HELPTRAN         TO HELPD-KEY.
           INSPECT HELPD-TRAN   REPLACING ALL '*' BY ' '.
           MOVE ZEROES                 TO TALLY.
           INSPECT HELPD-TRAN    TALLYING TALLY
                    FOR CHARACTERS BEFORE INITIAL SPACE.

           IF  INPUT-HELPTRAN = '*'
           OR  INPUT-HELPNAME NOT > SPACES
               MOVE '*'                TO INPUT-HELPNAME
           ELSE
               MOVE INPUT-HELPNAME     TO HELPD-NAME
               INSPECT HELPD-NAME REPLACING ALL '*' BY ' '
           END-IF.

           MOVE ZEROES                 TO HELPD-SEQU.

           SET  FUNC-START(VSUB)       TO TRUE.
           START SYHELPD KEY >= HELPD-KEY END-START.

           PERFORM WITH TEST BEFORE
             UNTIL NOT STAT-NORMAL(VSUB)

               SET FUNC-READNEXT(VSUB) TO TRUE
               READ SYHELPD NEXT RECORD END-READ

               IF  NOT STAT-NORMAL(VSUB)
               OR  INPUT-HELPTRAN NOT = '*'
               AND HELPD-TRAN(1:TALLY) > INPUT-HELPTRAN(1:TALLY)
                 IF  STAT-NORMAL(VSUB)
                   SET STAT-EOFILE(VSUB) TO TRUE
                 END-IF
               ELSE
                 PERFORM P10-CHECK-FOR-MATCH THRU P15-EXIT
                 IF  RECORD-IS-SELECTED
                   ADD  1              TO REC-CNT
                   PERFORM P20-PUNCH-SEGMENT THRU P25-EXIT
                 END-IF
               END-IF
           END-PERFORM.

           IF  NOT STAT-NORMAL(VSUB)
           AND NOT STAT-EOFILE(VSUB)
               MOVE HELPD-KEY          TO VSAM-KEYD(VSUB)
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               GO TO C90-EXIT
           END-IF.

       C90-EXIT.
           EXIT.

      /*****************************************************************
      *    PROMOTE HELP PAGES                                          *
      ******************************************************************

       D00-PROMOTE-HELPD.

           PERFORM U10-GET-INPUT THRU U15-EXIT.
           IF  CARDRDR-AT-END
               GO TO D90-EXIT
           END-IF.

           IF  INPUT-CARD(1:12) NOT = LAST-KEY
               MOVE INPUT-CARD(1:12)   TO HELPD-KEY
               MOVE ZEROES             TO HELPD-SEQU
               SET  FUNC-START(VSUB)   TO TRUE
               START SYHELPD KEY >= HELPD-KEY END-START
               IF  NOT STAT-NORMAL(VSUB)
                   MOVE HELPD-KEY      TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                   GO TO D90-EXIT
               END-IF
               SET FUNC-READNEXT(VSUB) TO TRUE
               READ SYHELPD NEXT RECORD END-READ
               IF  NOT STAT-NORMAL(VSUB)
               OR  HELPD-KEY(1:12) NOT = INPUT-CARD(1:12)
                   IF  STAT-EOFILE(VSUB)
                   OR  HELPD-KEY(1:12) NOT = INPUT-CARD(1:12)
                       CONTINUE
                   ELSE
                       MOVE HELPD-KEY  TO VSAM-KEYD(VSUB)
                       PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                       GO TO D90-EXIT
                   END-IF
               ELSE
                   PERFORM WITH TEST BEFORE
                     UNTIL HELPD-KEY(1:12) NOT = INPUT-CARD(1:12)
                        OR NOT STAT-NORMAL(VSUB)
                       SET FUNC-DELETE(VSUB) TO TRUE
                       DELETE SYHELPD RECORD END-DELETE
                       IF  STAT-NORMAL(VSUB)
                         SET FUNC-READNEXT(VSUB) TO TRUE
                         READ SYHELPD NEXT RECORD END-READ
                       END-IF
                   END-PERFORM
                   IF  NOT STAT-NORMAL(VSUB)
                   AND NOT STAT-EOFILE(VSUB)
                       MOVE HELPD-KEY  TO VSAM-KEYD(VSUB)
                       PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                       GO TO D90-EXIT
                   END-IF
               END-IF
               MOVE INPUT-CARD(1:12)   TO LAST-KEY
               MOVE ZEROES             TO LAST-SEQU
           END-IF.

           MOVE LAST-KEY               TO HELPD-KEY.
           MOVE LAST-SEQU              TO HELPD-SEQU.

           EVALUATE INPUT-CARD(13:1)
           WHEN '|'
               COMPUTE HELPD-RECL = LENGTH OF HELPD-KEY
                                  + LENGTH OF HELPD-TABLE
               MOVE INPUT-CARD(14:)    TO HELPD-TABLE
               SET  FUNC-WRITE(VSUB)   TO TRUE
               WRITE SYHELPD-RECORD END-WRITE
           WHEN '+'
               SET  FUNC-READ(VSUB)    TO TRUE
               READ SYHELPD RECORD END-READ
               IF  NOT STAT-NORMAL(VSUB)
                   MOVE HELPD-KEY      TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                   GO TO D90-EXIT
               END-IF
               COMPUTE HELPD-RECL = LENGTH OF HELPD-KEY
                                  + LENGTH OF HELPD-TABLE
               MOVE INPUT-CARD(14:)    TO HELPD-TABLE(65:)
               SET  FUNC-REWRITE(VSUB) TO TRUE
               REWRITE SYHELPD-RECORD END-REWRITE
           WHEN '@'
               COMPUTE HELPD-RECL = LENGTH OF HELPD-KEY
                                  + LENGTH OF HELPD-DATA
               ADD  1                  TO LAST-SEQU
                                          HELPD-SEQU
               MOVE INPUT-CARD(14:)    TO HELPD-DATA
               SET  FUNC-WRITE(VSUB)   TO TRUE
               WRITE SYHELPD-RECORD END-WRITE
           WHEN OTHER
               DISPLAY THIS-PGM ': UNEXPECTED CARD CODE'
                                     UPON CONSOLE
               MOVE +16                TO RTC-CODE
               PERFORM B80-CALL-RTCMAN THRU B85-EXIT
               GO TO D90-EXIT
           END-EVALUATE.

           ADD  1                      TO REC-CNT.

           IF  NOT STAT-NORMAL(VSUB)
               MOVE HELPD-KEY          TO VSAM-KEYD(VSUB)
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               GO TO D90-EXIT
           END-IF.

       D90-EXIT.
           EXIT.

      /*****************************************************************
      *    PERFORMED ROUTINES                                          *
      ******************************************************************

       P10-CHECK-FOR-MATCH.
           SET  WILDCOMP-PGM           TO TRUE.
           SET  RECORD-NOT-SELECTED    TO TRUE.
           IF  INPUT-HELPTRAN      NOT = '*'
               MOVE LENGTH OF INPUT-HELPTRAN
                                       TO WILDCOMP-LEN
               MOVE INPUT-HELPTRAN     TO WILDCOMP-STR1
               MOVE HELPD-TRAN         TO WILDCOMP-STR2
               CALL WILDCOMP-CTL    USING WILDCOMP-PARMS
               IF  NOT WILDCOMP-STR1-EQ-STR2
                   GO TO P15-EXIT
               END-IF
           END-IF.
           IF  INPUT-HELPNAME      NOT = '*'
               MOVE LENGTH OF INPUT-HELPNAME
                                       TO WILDCOMP-LEN
               MOVE INPUT-HELPNAME     TO WILDCOMP-STR1
               MOVE HELPD-NAME         TO WILDCOMP-STR2
               CALL WILDCOMP-CTL    USING WILDCOMP-PARMS
               IF  NOT WILDCOMP-STR1-EQ-STR2
                   GO TO P15-EXIT
               END-IF
           END-IF.
           SET  RECORD-IS-SELECTED     TO TRUE.
       P15-EXIT.
           EXIT.

       P20-PUNCH-SEGMENT.
           IF  NOT CARDPCH-OPEN
               OPEN OUTPUT CARDPCH
               SET  CARDPCH-OPEN       TO TRUE
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > JCL-MAX
                      OR JCL-CARD(IDX) = '###STOP###'
                   MOVE JCL-CARD(IDX)  TO PUNCH-CARD
                   PERFORM U50-PUT-PUNCH THRU U55-EXIT
               END-PERFORM
               IF  IDX > JCL-MAX
                   MOVE JCL-MAX        TO JCL-PRE
               ELSE
                   MOVE IDX            TO JCL-PRE
               END-IF
           END-IF.

           IF  HELPD-SEQU = ZERO
               STRING HELPD-TRAN HELPD-NAME '|' HELPD-TABLE(1:64)
                   DELIMITED BY SIZE INTO PUNCH-CARD
               PERFORM U50-PUT-PUNCH THRU U55-EXIT
               STRING HELPD-TRAN HELPD-NAME '+' HELPD-TABLE(65:)
                   DELIMITED BY SIZE INTO PUNCH-CARD
           ELSE
               STRING HELPD-TRAN HELPD-NAME '@' HELPD-DATA
                   DELIMITED BY SIZE INTO PUNCH-CARD
           END-IF.

           PERFORM U50-PUT-PUNCH THRU U55-EXIT.
       P25-EXIT.
           EXIT.

      /*****************************************************************
      *    SHARED UTILITY ROUTINES                                     *
      ******************************************************************

       U10-GET-INPUT.
           READ CARDRDR RECORD AT END
               SET  CARDRDR-AT-END     TO TRUE
           END-READ.
       U15-EXIT.
           EXIT.

       U50-PUT-PUNCH.
           WRITE PUNCH-CARD END-WRITE.
           MOVE SPACES                 TO PUNCH-CARD.
       U55-EXIT.
           EXIT.

