 CBL XOPTS(SP)
      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    HELPPGM.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  NOV 2009.
       DATE-COMPILED.
       INSTALLATION.  WINWHOLESALE GROUP SERVICES.
       SECURITY.      EDIT FUNCTION RESTRICTED BY THE BSM.
      *REMARKS.       WIN CICS HELP PROGRAM.

      * CHANGE HISTORY ------------------------------------------------
      * 09/29/2006 DLC ORIGINAL PROGRAM.
      * 10/08/2011 DLC CHANGE HELP FUNCTION RETURN TO CALLING PROGRAM
      *                TO RETURN TO CALLING TRANSACTION, INSTEAD.
      * 11/22/2019 DLC COMPLETE REDESIGN OF ORIGINAL PROGRAM TO CHANGE
      *                FROM CAPTURING SCREEN DATA INTO A LINE EDITOR.
/DLC0/* 01/20/2020 DLC ADD A 'P'RINT FUNCTION TO DOCUMENT HELP SCREENS.
/DLC1/* 04/24/2020 DLC FIX HELP SELECTION SCREEN CREATION.
/DLC2/* 05/08/2020 DLC HELP PREVENT ACCIDENTAL LOSS OF EDIT CHANGES.
/DLC3/* 06/25/2020 DLC HELP-ON-HELP DURING HELP EDIT FUNCTION CAN CAUSE
/DLC3/*                CORRUPTION OF THE HELP SCREEN BEING EDITED.
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
           SYSLST IS PRINTER.

      /*****************************************************************
      *                                                                *
      *    DATA DIVISION                                               *
      *                                                                *
      ******************************************************************
       DATA DIVISION.

      ******************************************************************
      *    WORKING-STORAGE SECTION                                     *
      ******************************************************************
       WORKING-STORAGE SECTION.

       01  CONTROL-FIELDS.
      *
      * FIELDS TO MANAGE PROGRAM AND MAP RESOURCE NAMES
         COPY COMMWORK       REPLACING =='TRAN'==    BY =='HELP'==
                                       =='PROGRAM'== BY =='HELPPGM'==.
         03  HELPMS                    PIC  X(08)   VALUE 'HELPMS  '.
         03  LISTMAP                   PIC  X(08)   VALUE 'LISTMAP '.
         03  HELPMAP                   PIC  X(08)   VALUE 'HELPMAP '.
         03  HELPXXX                   PIC  X(08)   VALUE 'HELPXXX '.
         03  SELTWIN                   PIC  X(08)   VALUE 'SELTWIN '.
         03  SELTERR                   PIC  X(08)   VALUE 'SELTERR '.
/DLC0/   03  WAITWIN                   PIC  X(08)   VALUE 'WAITWIN '.
      *
      * FIELDS TO MANAGE REFERENCED FILE NAMES
         03  SYHELPD                   PIC  X(08)   VALUE 'SYHELPD '.
      *
      * FIELDS TO MANAGE REFERENCED EXTERNAL PROGRAM NAMES
         03  MULTMESG                  PIC  X(08)   VALUE 'MULTMESG'.
      *
      * FIELDS TO MANAGE ERROR HANDLING
         03  ERROR-SWITCH              PIC S9(01)   VALUE ZEROES.
           88  NO-ERRORS-FOUND                      VALUE ZEROES.
           88  ERROR-AT-CURSOR                      VALUE -1.
           88  ERRORS-FOUND                         VALUES -9 THRU -1.
         03  OPTION-SWITCH             PIC  X(01)   VALUE 'N'.
           88  OPTION-OMITTED                       VALUE 'N'.
           88  OPTION-SELECTED                      VALUE 'Y'.
         03  EOD-SWITCH                PIC  X(01)   VALUE 'N'.
           88  EOD-NOT-MARKED                       VALUE 'N'.
           88  EOD-MARKED                           VALUE 'Y'.
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LEN                       PIC S9(04)   BINARY VALUE ZEROES.
         03  L2                        PIC S9(04)   BINARY VALUE ZEROES.
         03  OFS                       PIC S9(04)   BINARY VALUE ZEROES.
         03  POS                       PIC S9(04)   BINARY VALUE ZEROES.
         03  SCR-ADDR   REDEFINES POS  PIC  XX.
         03  PTR                       PIC S9(04)   BINARY VALUE ZEROES.
         03  P2                        PIC S9(04)   BINARY VALUE ZEROES.
         03  R2                        PIC S9(04)   BINARY VALUE ZEROES.
         03  C2                        PIC S9(04)   BINARY VALUE ZEROES.
         03  BEGP                      PIC S9(04)   BINARY VALUE ZEROES.
         03  ENDP                      PIC S9(04)   BINARY VALUE ZEROES.
      *
      * FIELDS TO CALCULATE CURSOR POSITION AND MANAGE MAPS W/ OCCURS
         03  ROW                       PIC S9(04)   BINARY VALUE ZEROES.
         03  COL                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LISTHEAD                  PIC S9(04)   BINARY VALUE +04.
         03  LISTROWS                  PIC S9(04)   BINARY VALUE +20.
         03  LISTCOLS                  PIC S9(04)   BINARY VALUE +05.
         03  LISTCWDT                  PIC S9(04)   BINARY VALUE +27.
         03  HELPHEAD                  PIC S9(04)   BINARY VALUE +03.
         03  HELPPAGE                  PIC S9(04)   BINARY VALUE +43.
         03  HELPROWS                  PIC S9(04)   BINARY VALUE +22.
         03  HELPCOLS                  PIC S9(04)   BINARY VALUE +02.
         03  HELPCWDT                  PIC S9(04)   BINARY VALUE +67.
         03  HELP-MAX                  PIC S9(04)   BINARY VALUE +8600.
         03  SELTHEAD                  PIC S9(04)   BINARY VALUE +04.
         03  SELTROWS                  PIC S9(04)   BINARY VALUE +06.
         03  SELTCOLS                  PIC S9(04)   BINARY VALUE +02.
/DLC0/   03  LINE-CNT                  PIC S9(04)   BINARY VALUE +99.
/DLC0/   03  PAGE-CNT                  PIC S9(04)   BINARY VALUE ZEROES.
      *
         03  HELPD-RECL                PIC S9(04)   BINARY VALUE +111.
         03  HELPD-MAXL                PIC S9(04)   BINARY VALUE +111.
/DLC0/   03  PCL-ESC                   PIC  X(1)    VALUE x'EF'.
         03  EDIT-ROW                  PIC  9(04).
         03  EDIT-COL                  PIC  9(04).
         03  EDIT-ROWS                 PIC  9(04).
         03  EDIT-COLS                 PIC  9(04).
/DLC0/   03  PCL-BOLD                  PIC  X(4)    VALUE '(s5B'.
/DLC0/   03  PCL-NORM                  PIC  X(4)    VALUE '(s0B'.
/DLC0/   03  POPUP-TEXT                PIC  X(15)   VALUE SPACES.
         03  LOWER-CASE  PIC  X(26)  VALUE 'abcdefghijklmnopqrstuvwxyz'.
         03  UPPER-CASE  PIC  X(26)  VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
         03  MORE-HELP                 PIC  X(63)   VALUE '
      -      '                                      (...more...)'.
         03  END-OF-HELP               PIC  X(63)   VALUE
             '                        --- THE END ---'.
         03  VAR-TEXTL                 PIC S9(4)    BINARY.
         03  VAR-TEXT                  PIC  X(80)   VALUE SPACES.
         03  MULTI-EDIT-MSG            PIC  X(78)   VALUE SPACES.
         03  VAR-DATA                  PIC  X(98)   VALUE SPACES.
/DLC0/   03  VAR-LINE                  PIC  X(132)  VALUE SPACES.
      *
      * TO CONVERT NUMERIC TO BINARY-CODED CHARACTER AND VICE VERSA
         03  DOUBLE-WORD               PIC S9(18)   BINARY.
         03  EIGHT-BYTES               REDEFINES    DOUBLE-WORD.
           05                          PIC  X(2).
           05  SIX-BYTES.
             07                        PIC  X(2).
             07  FULL-WORD             PIC S9(9)    BINARY.
             07  FOUR-BYTES            REDEFINES    FULL-WORD.
               09  HI-MSB              PIC  X.
               09  THREE-BYTES.
                 11  HI-LSB            PIC  X.
                 11  HALF-WORD         PIC S9(4)    BINARY.
                 11  TWO-BYTES         REDEFINES    HALF-WORD.
                   13  LO-MSB          PIC  X.
                   13  LO-LSB          PIC  X.
      *
      * TO CONVERT DATES AND TIMES
         03  NUM-DATE                  PIC  9(8).
         03  WRK-DATE                  REDEFINES    NUM-DATE.
           05  DTE-GRMO                PIC  99.
           05  DTE-GRDA                PIC  99.
           05  DTE-GRYR                PIC  9999.
         03  WS-ABSTIME                PIC S9(15)   PACKED-DECIMAL.
         03  WS-DSPDATE                PIC  99/99/9999.
         03  WS-WRKDATE                REDEFINES    WS-DSPDATE.
           05  WS-MODATE               PIC  9(02).
           05                          PIC  X.
           05  WS-DADATE               PIC  9(02).
           05                          PIC  X.
           05  WS-CYDATE               PIC  9(04).
         03  WS-WRKTIME.
           05  WS-HRSTIME              PIC  9(02).
           05                          PIC  X(03).
           05  WS-XXXTIME              PIC  X(03).
      *
         03  DIGIT                     PIC  9.
         03  VAR-FIND                  PIC  X(40)   VALUE SPACES.
         03  VAR-FINDL                 PIC S9(4)    BINARY.
         03  VAR-REPLL                 PIC S9(4)    BINARY.
         03  VAR-REPL.
           05  VAR-BYTE                PIC  X       OCCURS 1 TO 40
                                       DEPENDING ON VAR-REPLL.

       01  HELP-HEAD.
           05                          PIC  X(01)   VALUE '1'.
           05                          PIC  X(07)   VALUE SPACES.
           05                          PIC  X(07)   VALUE '{HELP:}'.
           05  HEAD-HELP               PIC  X(04).
           05                          PIC  X(09)   VALUE '  {NAME:}'.
           05  HEAD-NAME               PIC  X(08).
           05                          PIC  X(26)   VALUE SPACES.
           05                          PIC  X(07)   VALUE '{PAGE:}'.
           05  HEAD-PAGE               PIC  Z(03).

      * APPLICATION SECURITY CONTROL AREAS
       01  VSECSERV-CONTROL.
         03  VSECSERV                  PIC  X(8)    VALUE 'VSECSERV'.
         03  VSECSERV-RESOURCE         PIC  X(39)   VALUE
                              'WINAPPS.SY.HELP.SYHELPD'.
       01  VSECSERV-COMMAREA.
         COPY VSECSERV.

      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS

       01  CICSINFO-PARMS.
         COPY CICSINFO.

       COPY HEXMAN.

       COPY LOGGING.

/DLC0/ COPY PRTMAN.

       COPY TXTMAN.

       COPY UNEXERRW.
           05  CONFIRM-MSG   REDEFINES UNEX-MSG     PIC  X(79).

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

      * HELP CICS COMMUNICATION AREA
       01  DFHCOMMAREA.
         COPY COMMAREA.
      *
           05  HELP-SAVEAREA.
             07  HELP-SAVESEQU         PIC  9(4).
             07  HELP-HOW-STARTED      PIC  X(1).
               88  HELP-STARTED-FOR-LIST            VALUE 'L'.
               88  HELP-STARTED-FOR-HELP            VALUE 'H'.
             07  HELP-ACCESS           PIC  X(1).
               88  HELP-READONLY                    VALUE '2'.
               88  HELP-UPDATE                      VALUE '3'.
               88  HELP-ADMIN                       VALUE '4'.
             07  SECTION-SWITCH        PIC  X(1)    VALUE 'N'.
               88  SECTION-HEADERS-ABSENT           VALUE 'N'.
               88  SECTION-HEADERS-PRESENT          VALUE 'Y'.
             07  HELP-ADJUSTED         PIC S9(4)    BINARY.
             07  HELP-SECTION          PIC  x(40).
      *
           05  TS-QUEUE.
/DLC3/*      07  TS-TRAN               PIC  X(4).
/DLC3/*      07  TS-TERM               PIC  X(4).
/DLC3/       07  TS-BASENAME           PIC  X(8).
/DLC3/       07  TS-QUALIFIER          PIC  X(8).
           05  TS-TOTL                 PIC S9(4)    BINARY.
           05  TS-ITEM                 PIC S9(4)    BINARY.
           05  SS-ITEM                 PIC S9(4)    BINARY.
           05  TS-RECD                 PIC  X(64).
      *
           05  T2-QUEUE.
/DLC3/*      07  T2-TERM               PIC  X(4).
/DLC3/*      07  T2-TRAN               PIC  X(4).
/DLC3/       07  T2-BASENAME           PIC  X(8).
/DLC3/       07  T2-QUALIFIER          PIC  X(8).
           05  T2-TOTL                 PIC S9(4)    BINARY.
           05  T2-ITEM                 PIC S9(4)    BINARY.
           05  T2-RECD                 PIC  X(64).
      *
           05  HELP-SAVE-TIOA          PIC  X(3830).
           05  HELP-BUFLEN             PIC S9(4)    BINARY.
           05  HELP-BUFFER             PIC  X(4096).

      * HELP TEMP COMMUNICATION AREA
       COPY COMMTEMP.

      * 3270 DATA STREAM CONTROL CHARACTERS
       COPY IBM3270.

      * SYHELPD RECORD I/O AREA
       COPY SYHELPD.

      * BMS TERMINAL I/O AREAS
       COPY HELPMS.

       01  LISTMAPS                    REDEFINES LISTMAPI.
         02                            PIC  X(147).
         02  LISTMAP-ROW-DETAIL        OCCURS    20 TIMES.
           03  LISTMAP-COL-DETAIL      OCCURS     5 TIMES.
             04  LIST-SELECT-L         PIC S9(4)    BINARY.
             04  LIST-SELECT-X.
               05  LIST-SELECT-F.
                 06  LIST-SELECT-A     PIC  X(1).
               05  LIST-SELECT-C       PIC  X(1).
               05  LIST-SELECT-H       PIC  X(1).
             04  LIST-SELECT-I         PIC  X(001).
             04  LIST-SELECT-O
                           REDEFINES   LIST-SELECT-I
                                       PIC  X(001).
             04  LIST-HELP-TRAN-L      PIC S9(4)    BINARY.
             04  LIST-HELP-TRAN-X.
               05  LIST-HELP-TRAN-F.
                 06  LIST-HELP-TRAN-A  PIC  X(1).
               05  LIST-HELP-TRAN-C    PIC  X(1).
               05  LIST-HELP-TRAN-H    PIC  X(1).
             04  LIST-HELP-TRAN-I      PIC  X(004).
             04  LIST-HELP-TRAN-O
                           REDEFINES   LIST-HELP-TRAN-I
                                       PIC  X(004).
             04  LIST-HELP-NAME-L      PIC S9(4)    BINARY.
             04  LIST-HELP-NAME-X.
               05  LIST-HELP-NAME-F.
                 06  LIST-HELP-NAME-A  PIC  X(1).
               05  LIST-HELP-NAME-C    PIC  X(1).
               05  LIST-HELP-NAME-H    PIC  X(1).
             04  LIST-HELP-NAME-I      PIC  X(008).
             04  LIST-HELP-NAME-O
                           REDEFINES   LIST-HELP-NAME-I
                                       PIC  X(008).
             04  LIST-HELP-PTOT-L      PIC S9(4)    BINARY.
             04  LIST-HELP-PTOT-X.
               05  LIST-HELP-PTOT-F.
                 06  LIST-HELP-PTOT-A  PIC  X(1).
               05  LIST-HELP-PTOT-C    PIC  X(1).
               05  LIST-HELP-PTOT-H    PIC  X(1).
             04  LIST-HELP-PTOT-I      PIC  X(003).
             04  LIST-HELP-PTOT-O
                           REDEFINES   LIST-HELP-PTOT-I
                                       PIC  ZZ9.
         02                            PIC  X(83).

       01  HELPMAPS.
         02                            PIC  X(147).
         02  HELPMAP-ROW-DETAIL        OCCURS    22 TIMES.
           03  HELPMAP-COL-DETAIL      OCCURS     2 TIMES.
             04  HELP-TEXT-L           PIC S9(4)    COMP.
             04  HELP-TEXT-X.
               05  HELP-TEXT-F.
                 06  HELP-TEXT-A       PIC  X(1).
               05  HELP-TEXT-C         PIC  X(1).
               05  HELP-TEXT-H         PIC  X(1).
             04  HELP-TEXT-I           PIC  X(063).
             04  HELP-TEXT-O
                           REDEFINES   HELP-TEXT-I
                                       PIC  X(063).
         02                            PIC  X(259).

       01  SELTWINS.
         02                            PIC  X(12).
         02  SELTWIN-ROW-DETAIL        OCCURS     6 TIMES.
           03  SELTWIN-COL-DETAIL      OCCURS     2 TIMES.
             04  SELT-SELECTION-L      PIC S9(4)    COMP.
             04  SELT-SELECTION-X.
               05  SELT-SELECTION-F.
                 06  SELT-SELECTION-A
                                       PIC  X(1).
               05  SELT-SELECTION-C    PIC  X(1).
               05  SELT-SELECTION-H    PIC  X(1).
             04  SELT-SELECTION-I      PIC  X(008).
             04  SELT-SELECTION-O
                           REDEFINES   SELT-SELECTION-I
                                       PIC  X(008).

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    PROGRAM INITIALIZATION                                      *
      ******************************************************************
       A00-HELP-INITIALIZATION.

           COPY COMMENTR.

           EXEC CICS LINK
                     PROGRAM  ('CICSINFO')
                     COMMAREA (CICSINFO-PARMS)
           END-EXEC.
           MOVE CICS-FULLDATE          TO WS-WRKDATE.
           MOVE CICS-FULLTIME          TO WS-WRKTIME.
           PERFORM Q02-REFORMAT-TIME THRU Q05-EXIT.

           COPY COMMMAPT.

           COPY COMM3270.

           EXEC CICS HANDLE CONDITION
                     DISABLED (Z70-HELP-CLOSED)
                     NOTOPEN  (Z70-HELP-CLOSED)
           END-EXEC.

           IF  COMM-CURRPGM = THIS-PGM
               GO TO B50-HELP-ROUTINE-SELECTION
           END-IF.

       B00-HELP-FIRST-TIME-ONLY.

           COPY COMMINIT.

           IF  COMM-CURRRTN = 'RETN'
           AND COMM-HELPAREA(1:4) > SPACES
               GO TO B50-HELP-ROUTINE-SELECTION
           END-IF.

           IF  COMM-CURRRTN NOT = 'RETN'
                              AND 'EXIT'
               MOVE SPACES             TO COMM-HELPTRAN
                                          COMM-HELPNAME
           END-IF.

           MOVE ZEROES                 TO HELP-SAVESEQU
                                          HELP-ADJUSTED.
           MOVE SPACES                 TO HELP-SECTION.

           IF  COMM-HELPTRAN <= SPACES
               SET HELP-STARTED-FOR-LIST TO TRUE
           ELSE
               SET HELP-STARTED-FOR-HELP TO TRUE
           END-IF.

           MOVE SPACES                 TO VSECSERV-HEADER.
           SET  VSECSERV-TRAN          TO TRUE.
           SET  VSECSERV-CHK-SECURITY  TO TRUE.
           MOVE VSECSERV-RESOURCE      TO VSECSERV-CGI-REQU.
           EXEC CICS LINK
                     PROGRAM  (VSECSERV)
                     COMMAREA (VSECSERV-HEADER)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO X00-UNEX-ERR
           END-IF.

           EVALUATE TRUE
           WHEN VSECSERV-SEC-ALTERABLE MOVE '4' TO HELP-ACCESS
           WHEN VSECSERV-SEC-UPDATABLE MOVE '3' TO HELP-ACCESS
           WHEN OTHER                  MOVE '2' TO HELP-ACCESS
           END-EVALUATE.

           STRING '<' COMM-HEADER
                  ' ' COMM-HELPAREA
                  ' ' HELP-SAVEAREA(1:6)
                  ' ' CICS-USERID
               DELIMITED BY SIZE     INTO LOGF-MESG.
           PERFORM Q100-LOGIT        THRU Q199-EXIT.

       B50-HELP-ROUTINE-SELECTION.

           IF  COMM-CURRRTN = 'RETN'
           AND COMM-HELPAREA(1:4) > SPACES
               MOVE COMM-HELPAREA(1:4) TO COMM-CURRRTN
               MOVE INIT-FLAG          TO COMM-HELPAREA(1:4)
           END-IF.

           EVALUATE COMM-CURRRTN
           WHEN 'SELT' GO TO F00-SELT-ROUTINE
           WHEN 'EDIT' GO TO E00-EDIT-ROUTINE
           WHEN 'HELP' GO TO D00-HELP-ROUTINE
           WHEN 'LIST' GO TO C00-LIST-ROUTINE
           END-EVALUATE.

           EVALUATE TRUE
           WHEN HELP-STARTED-FOR-HELP GO TO D00-HELP-ROUTINE
           WHEN HELP-READONLY         GO TO Z90-HELP-TERMINATION
           END-EVALUATE.

      /*****************************************************************
      *    LIST ROUTINE                                                *
      ******************************************************************
       C00-LIST-ROUTINE.

           EXEC CICS GETMAIN
                     SET      (ADDRESS OF LISTMAPI)
                     LENGTH   (LENGTH OF LISTMAPI)
                     INITIMG  (LOVALUE)
           END-EXEC.

           IF  COMM-CURRRTN NOT = 'LIST'
           OR  COMM-HELPAREA(1:4) = INIT-FLAG
               IF COMM-HELPAREA(1:4) = INIT-FLAG
                   MOVE SPACES         TO COMM-HELPAREA(1:4)
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN > SPACES
               AND COMM-CURRRTN NOT = 'EXIT'
                   MOVE HELP-SAVE-TIOA TO LISTMAPO
               END-IF
               PERFORM C80-LIST-OUTPUT
               EXEC CICS SEND
                         MAP      (LISTMAP)
                         FROM     (LISTMAPO)
                         MAPSET   (HELPMS)
                         TERMINAL
                         ERASE
                         NOHANDLE
               END-EXEC
               GO TO C50-LIST-DISPLAY
           END-IF.

       C10-LIST-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (LISTMAP)
                     INTO     (LISTMAPI)
                     MAPSET   (HELPMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO C70-LIST-ERRORS
           END-IF.

       C20-LIST-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
           WHEN AIDPFK03
               GO TO Z90-HELP-TERMINATION

           WHEN AIDPFK01
               IF  COMM-HELPAREA(1:4) <= SPACES
                   MOVE COMM-CURRRTN   TO COMM-HELPAREA(1:4)
                   PERFORM T40-SAVE-COMMAREA THRU T45-EXIT
                   MOVE 'HELP'         TO COMM-CURRPGM
                   GO TO T51-HELP-SAVE-DONE
               END-IF
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'Already viewing Help-on-Help.'
                                       TO LIST-MESSAGE-O
               GO TO C80-LIST-OUTPUT

           WHEN AIDENTER
           WHEN AIDPFK06
           WHEN AIDPFK07
           WHEN AIDPFK08
           WHEN AIDPFK09
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'That attention key has no current action assigned.'
                                       TO LIST-MESSAGE-O
               GO TO C80-LIST-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'That function key has no current action assigned.'
                                       TO LIST-MESSAGE-O
               GO TO C80-LIST-OUTPUT
           END-EVALUATE.

       C30-LIST-PROCESS.

      * scroll to top or bottom of the list
           IF  EIBAID = AIDPFK06 OR AIDPFK09
               PERFORM Q20-INITKEY-SYHELPD
               IF  EIBAID = AIDPFK06
                   PERFORM Q21-STARTBR-SYHELPD
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(NOTFND)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q23-READNEXT-SYHELPD THRU Q23-CHECK-IT
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(INVREQ)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  HELPD-TRAN = LIST-HELP-TRAN-I(1 1)
                   AND HELPD-NAME = LIST-HELP-NAME-I(1 1)
                       MOVE 'This is the first page.' TO LIST-MESSAGE-O
                   END-IF
               ELSE
                   MOVE HIGH-VALUES    TO HELPD-KEY
                   PERFORM Q22-STARTEQ-SYHELPD
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(NOTFND)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q24-READPREV-SYHELPD THRU Q24-CHECK-IT
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(INVREQ)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  HELPD-KEY = HIGH-VALUES
                       MOVE LOW-VALUES TO HELPD-KEY
                   END-IF
                   IF  HELPD-TRAN = LIST-HELP-TRAN-I(1 1)
                   AND HELPD-NAME = LIST-HELP-NAME-I(1 1)
                       MOVE 'This is the last page.' TO LIST-MESSAGE-O
                   END-IF
               END-IF
               MOVE HELPD-TRAN          TO LIST-HELP-TRAN-O(1 1)
               MOVE HELPD-NAME          TO LIST-HELP-NAME-O(1 1)
               PERFORM Q25-ENDBR-SYHELPD
               GO TO C50-LIST-DISPLAY
           END-IF.

      * convert cursor position to row and column coordinates
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.
      * adjust row and column coordinates for header/detail screens
           SUBTRACT LISTHEAD         FROM ROW.
           COMPUTE COL = (COL + LISTCWDT) / LISTCWDT.

      * scroll to previous or next page
           IF  EIBAID = AIDPFK07 OR AIDPFK08
               PERFORM Q20-INITKEY-SYHELPD
               MOVE LIST-HELP-TRAN-I(1 1) TO HELPD-TRAN
               MOVE LIST-HELP-NAME-I(1 1) TO HELPD-NAME
               PERFORM Q21-STARTBR-SYHELPD
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                             AND DFHRESP(NOTFND)
                   GO TO C70-LIST-ERRORS
               END-IF
               IF  EIBAID = AIDPFK07
                   IF  ROW <= 1 OR >= LISTROWS
                   OR  LIST-HELP-TRAN-I(ROW COL) <= SPACES
                       COMPUTE IDX = LISTROWS
                   ELSE
                       COMPUTE IDX = LISTROWS - ROW
                   END-IF
                   PERFORM Q23-READNEXT-SYHELPD
                   PERFORM Q24-READPREV-SYHELPD THRU Q24-CHECK-IT
                     VARYING IDX FROM IDX BY -1
                       UNTIL IDX < ZERO
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                                 AND DFHRESP(INVREQ)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  EIBRESP = DFHRESP(ENDFILE)
                              OR DFHRESP(INVREQ)
                       MOVE 'This is the first entry.' TO LIST-MESSAGE-O
                       PERFORM Q23-READNEXT-SYHELPD THRU Q23-CHECK-IT
                       IF  EIBRESP NOT = DFHRESP(NORMAL)
                                     AND DFHRESP(INVREQ)
                           GO TO C70-LIST-ERRORS
                       END-IF
                   END-IF
               ELSE
                   IF  ROW <= 1 OR >= LISTROWS
                   OR  LIST-HELP-TRAN-I(ROW COL) <= SPACES
                       COMPUTE IDX = LISTROWS
                   ELSE
                       COMPUTE IDX = ROW - 1
                   END-IF
                   PERFORM Q23-READNEXT-SYHELPD THRU Q23-CHECK-IT
                     VARYING IDX FROM IDX BY -1
                       UNTIL IDX < ZERO
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                                 AND DFHRESP(NOTFND)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  EIBRESP = DFHRESP(ENDFILE)
                              OR DFHRESP(NOTFND)
                       MOVE 'This is the last entry.' TO LIST-MESSAGE-O
                       PERFORM Q24-READPREV-SYHELPD THRU Q24-CHECK-IT
                       IF  EIBRESP NOT = DFHRESP(NORMAL)
                                     AND DFHRESP(INVREQ)
                           GO TO C70-LIST-ERRORS
                       END-IF
                   END-IF
               END-IF
               MOVE HELPD-TRAN           TO LIST-HELP-TRAN-O(1 1)
               MOVE HELPD-NAME           TO LIST-HELP-NAME-O(1 1)
               PERFORM Q25-ENDBR-SYHELPD
               GO TO C50-LIST-DISPLAY
           END-IF.

      * check if default selection needs to be made
           IF  ROW >= 1 AND <= LISTROWS
/DLC0/*        PERFORM C44-DUMMY WITH TEST AFTER
/DLC0/*          VARYING C2 FROM 1 BY 1
/DLC0/*            UNTIL C2 > LISTCOLS
/DLC0/*               OR LIST-SELECT-I(R2 C2) > SPACE
/DLC0/*          AFTER   R2 FROM 1 BY 1
/DLC0/*            UNTIL R2 > LISTROWS
/DLC0/*               OR LIST-SELECT-I(R2 C2) > SPACE
/DLC0/         PERFORM R10-SEARCH-FOR-OPTION THRU R15-EXIT
/DLC0/*        IF  R2 > LISTROWS AND C2 > LISTCOLS
/DLC0/         IF  OPTION-OMITTED
                   MOVE LENGTH OF LIST-SELECT-O
                                       TO LIST-SELECT-L(ROW COL)
                   MOVE 'S'            TO LIST-SELECT-O(ROW COL)
               END-IF
           END-IF.

      * PERFORM DETAIL FIELD EDITING HERE

           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           PERFORM C40-LIST-UPDATES THRU C45-EXIT
             VARYING C2 FROM LISTCOLS BY -1 UNTIL C2 < 1
             AFTER   R2 FROM LISTROWS BY -1 UNTIL R2 < 1.

           IF  ERRORS-FOUND
               GO TO C80-LIST-OUTPUT
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO C70-LIST-ERRORS
           END-IF.

           GO TO C50-LIST-DISPLAY.

       C40-LIST-UPDATES.

      * PROCESS ANY ALLOWED UPDATES HERE

           INSPECT LIST-SELECT-I(R2 C2) CONVERTING LOWER-CASE
                                                TO UPPER-CASE.
           EVALUATE LIST-SELECT-I(R2 C2)
           WHEN '*'
/DLC0/     WHEN 'P'
               CONTINUE

           WHEN 'A'
           WHEN 'D'
           WHEN 'S'
/DLC1/         IF  LIST-HELP-NAME-I(R2 C2) NOT > SPACES
/DLC1/             MOVE SPACES         TO LIST-HELP-NAME-I(R2 C2)
/DLC1/         END-IF
               PERFORM Q20-INITKEY-SYHELPD
               MOVE LIST-HELP-TRAN-I(R2 C2) TO HELPD-TRAN
               MOVE LIST-HELP-NAME-I(R2 C2) TO HELPD-NAME
               PERFORM Q26-READGE-SYHELPD
               IF  EIBRESP = DFHRESP(NOTFND) OR DFHRESP(ENDFILE)
               OR  HELPD-TRAN NOT = LIST-HELP-TRAN-I(R2 C2)
               OR  HELPD-NAME NOT = LIST-HELP-NAME-I(R2 C2)
                 IF  LIST-SELECT-I(R2 C2) NOT = 'A'
                   MOVE -1             TO ERROR-SWITCH
                                          LIST-SELECT-L(R2 C2)
                   MOVE EXHREVRS       TO LIST-MESSAGE-H
                   MOVE 'Selected entry does not exist.'
                                       TO LIST-MESSAGE-O
                   GO TO C45-EXIT
                 ELSE
                   IF  LIST-HELP-TRAN-I(R2 C2) NOT > SPACES
                     MOVE -1           TO ERROR-SWITCH
                                          LIST-HELP-TRAN-L(R2 C2)
                     MOVE EXHREVRS     TO LIST-MESSAGE-H
                     MOVE 'Transaction name is required.'
                                       TO LIST-MESSAGE-O
                     GO TO C45-EXIT
                   END-IF
                 END-IF
               ELSE
                 IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C45-EXIT
                 END-IF
               END-IF

               EVALUATE LIST-SELECT-I(R2 C2)
               WHEN 'D'
                   IF  NOT HELP-ADMIN
                     MOVE -1           TO ERROR-SWITCH
                                          LIST-HELP-TRAN-L(R2 C2)
                     MOVE EXHREVRS     TO LIST-MESSAGE-H
                     MOVE 'Not authorized for that option.'
                                       TO LIST-MESSAGE-O
                     GO TO C45-EXIT
                   END-IF
                   COMPUTE EIBCPOSN = (LISTHEAD + (R2 - 1)) * SCRNWDTH
                                    + (LISTCWDT * (C2 - 1)) + 1
                   EXEC CICS SEND CONTROL CURSOR(EIBCPOSN) END-EXEC
                   MOVE SPACES         TO MULTI-EDIT-MSG
                   MOVE 1              TO LEN
                   STRING 'Delete this help set altogether?  '
                          'Press F2 to confirm.' DELIMITED BY SIZE
                     INTO MULTI-EDIT-MSG WITH POINTER LEN
                   SUBTRACT 1        FROM LEN
                   PERFORM V10-GET-CONFIRMATION THRU V15-EXIT
                   IF  EIBAID  NOT = AIDPFK02
                   OR  EIBRESP NOT = DFHRESP(NORMAL)
                     MOVE LOW-VALUES   TO LIST-SELECT-I(R2 C2)
                     GO TO C45-EXIT
                   END-IF
                   PERFORM Q28-DELETEALL-SYHELPD
                   IF  EIBRESP = DFHRESP(NORMAL)
                     MOVE '*'          TO LIST-SELECT-I(R2 C2)
                   END-IF
               WHEN 'A'
                   IF  EIBRESP = DFHRESP(NOTFND) OR DFHRESP(ENDFILE)
                     MOVE LIST-HELP-TRAN-I(R2 C2)
                                       TO HELPD-TRAN
                     IF  LIST-HELP-NAME-I(R2 C2) NOT > SPACES
                       MOVE SPACES     TO HELPD-NAME
                     ELSE
                       MOVE LIST-HELP-NAME-I(R2 C2)
                                       TO HELPD-NAME
                     END-IF
                     IF  HELPD-NAME NOT > SPACES
                       MOVE ZEROES     TO HELPD-SEQU
/DLC1/                 MOVE SPACES     TO HELPD-TABLE
                     ELSE
                       MOVE 1          TO HELPD-SEQU
                     END-IF
                     PERFORM Q27-WRITE-SYHELPD
                   END-IF
               END-EVALUATE

           WHEN OTHER
               IF  LIST-SELECT-O(R2 C2) > SPACE
                   MOVE -1             TO ERROR-SWITCH
                                          LIST-SELECT-L(R2 C2)
                   MOVE EXHREVRS       TO LIST-MESSAGE-H
                   MOVE 'Unknown option selection.'
                                       TO LIST-MESSAGE-O
                   GO TO C45-EXIT
               END-IF
           END-EVALUATE.

/DLC0/*C44-DUMMY.
       C45-EXIT.
           EXIT.

       C50-LIST-DISPLAY.

           MOVE 'LIST'                 TO COMM-CURRRTN.

      * skip if heading selections changed or no detail lines
           IF  LIST-HELP-TRAN-I(1 1) NOT > SPACES
               GO TO C60-LIST-DETAIL
           END-IF.

      * CHECK FOR OPTION SELECTIONS

/DLC0/*    PERFORM C44-DUMMY WITH TEST AFTER
/DLC0/*      VARYING COL FROM 1 BY 1
/DLC0/*        UNTIL COL > LISTCOLS
/DLC0/*          OR  LIST-SELECT-I(ROW COL) = 'A' OR 'S'
/DLC0/*      AFTER   ROW FROM 1 BY 1
/DLC0/*        UNTIL ROW > LISTROWS
/DLC0/*          OR  LIST-SELECT-I(ROW COL) = 'A' OR 'S'.
/DLC0/     PERFORM R10-SEARCH-FOR-OPTION THRU R15-EXIT.

/DLC0/*    IF  ROW <= LISTROWS
/DLC0/*    AND COL <= LISTCOLS
/DLC0/     IF  OPTION-SELECTED
           AND NO-ERRORS-FOUND
/DLC0/         MOVE R2                 TO ROW
/DLC0/         MOVE C2                 TO COL
/DLC0/         EVALUATE LIST-SELECT-I(ROW COL)
/DLC0/         WHEN 'S'
/DLC0/         WHEN 'A'
                 MOVE '*'              TO LIST-SELECT-O(ROW COL)
                 MOVE LIST-HELP-TRAN-I(ROW COL)
                                       TO COMM-HELPTRAN
                 MOVE LIST-HELP-NAME-I(ROW COL)
                                       TO COMM-HELPNAME
                 MOVE 1                TO HELP-SAVESEQU
                 MOVE LISTMAPI         TO HELP-SAVE-TIOA
                 GO TO D00-HELP-ROUTINE
/DLC0/         WHEN 'P'
/DLC0/           IF  LIST-HELP-TRAN-I(ROW COL) NOT > SPACES
/DLC0/               MOVE LOW-VALUES   TO LIST-SELECT-O(ROW COL)
/DLC0/               GO TO C50-LIST-DISPLAY
/DLC0/           END-IF
/DLC0/           MOVE '*'              TO LIST-SELECT-O(ROW COL)
/DLC0/           MOVE LIST-HELP-TRAN-I(ROW COL)
/DLC0/                                 TO COMM-HELPTRAN
/DLC0/           MOVE LIST-HELP-NAME-I(ROW COL)
/DLC0/                                 TO COMM-HELPNAME
/DLC0/           MOVE 1                TO HELP-SAVESEQU
/DLC0/           MOVE LISTMAPI         TO HELP-SAVE-TIOA
/DLC0/           GO TO H00-PRINT-ROUTINE
/DLC0/         WHEN OTHER
/DLC0/           MOVE     -1           TO ERROR-SWITCH
/DLC0/                                    LIST-SELECT-L(ROW COL)
/DLC0/           MOVE  EXHREVRS        TO LIST-MESSAGE-H
/DLC0/           MOVE 'Unknown option selection.'
/DLC0/                                 TO LIST-MESSAGE-O
/DLC0/         END-EVALUATE
               GO TO C80-LIST-OUTPUT
           END-IF.

       C60-LIST-DETAIL.

           MOVE 1                      TO ROW COL.

           PERFORM Q20-INITKEY-SYHELPD.
           MOVE LIST-HELP-TRAN-I(1 1)  TO HELPD-TRAN.
           MOVE LIST-HELP-NAME-I(1 1)  TO HELPD-NAME.
           PERFORM Q21-STARTBR-SYHELPD.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO C65-LIST-BLANKS
           END-IF.

           PERFORM C62-LIST-BUILD WITH TEST AFTER
             VARYING COL FROM 1 BY 1
               UNTIL COL > LISTCOLS
                  OR EIBRESP NOT = DFHRESP(NORMAL)
             AFTER   ROW FROM 1 BY 1
               UNTIL ROW > LISTROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL).

           GO TO C65-LIST-BLANKS.

       C62-LIST-BUILD.

           IF  ROW <= LISTROWS
           AND COL <= LISTCOLS
             PERFORM Q23-READNEXT-SYHELPD THRU Q23-CHECK-IT
             IF  EIBRESP = DFHRESP(NORMAL)
               MOVE HELPD-TRAN          TO LIST-HELP-TRAN-O(ROW COL)
               MOVE HELPD-NAME          TO LIST-HELP-NAME-O(ROW COL)
               MOVE HELPD-SEQU          TO LIST-HELP-PTOT-O(ROW COL)
               IF  HELPD-NAME <= SPACES
                 MOVE ZEROES            TO CNT
                 PERFORM WITH TEST BEFORE
                   VARYING IDX FROM 1 BY 1
                     UNTIL IDX > 12
                   IF  HELPD-ENTRY(IDX) > SPACES
                     ADD  1             TO CNT
                   END-IF
                 END-PERFORM
                 MOVE CNT               TO LIST-HELP-PTOT-O(ROW COL)
               ELSE
                 PERFORM P60-GET-LAST-PAGE THRU P65-EXIT
               END-IF
             END-IF
           END-IF.

       C65-LIST-BLANKS.

           IF  ROW > LISTROWS
               MOVE 1                  TO IDX
               ADD  1                  TO COL
           ELSE
               MOVE ROW                TO IDX
           END-IF.

           PERFORM C66-LIST-FILLIT WITH TEST BEFORE
             VARYING COL FROM COL BY 1 UNTIL COL > LISTCOLS
             AFTER   ROW FROM IDX BY 1 UNTIL ROW > LISTROWS.

           GO TO C68-LIST-CONTINUE.

       C66-LIST-FILLIT.

           IF  ROW <= LISTROWS
           AND COL <= LISTCOLS
               MOVE LOW-VALUES         TO LISTMAP-COL-DETAIL(ROW COL)
               MOVE FLDUNPRO           TO LIST-HELP-TRAN-A(ROW COL)
                                          LIST-HELP-NAME-A(ROW COL)
               MOVE EXHULINE           TO LIST-HELP-TRAN-H(ROW COL)
                                          LIST-HELP-NAME-H(ROW COL)
           END-IF.
           MOVE 1                      TO IDX.

       C68-LIST-CONTINUE.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(NOTFND)
                         AND DFHRESP(ENDFILE)
               GO TO C70-LIST-ERRORS
           END-IF.

           PERFORM Q25-ENDBR-SYHELPD.

       C70-LIST-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO LIST-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       C80-LIST-OUTPUT.

           MOVE MAPTITLE               TO LIST-COMPANY-O.
           MOVE WS-WRKDATE             TO LIST-SYSDATE-O.
           MOVE WS-WRKTIME             TO LIST-SYSTIME-O.
           MOVE CICS-NETNAME           TO LIST-NETNAME-O.
           IF  CICS-NETNAME NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO LIST-NETNAME-O
           END-IF.

       C85-LIST-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (LISTMAP)
                             DATAONLY
                             FROM     (LISTMAPO)
                             MAPSET   (HELPMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (LISTMAP)
                             DATAONLY
                             FROM     (LISTMAPO)
                             MAPSET   (HELPMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               MOVE -1                 TO LIST-SELECT-L(1 1)
               EXEC CICS SEND
                         MAP      (LISTMAP)
                         FROM     (LISTMAPO)
                         MAPSET   (HELPMS)
                         TERMINAL
                         FREEKB
                         ERASE
                         CURSOR
               END-EXEC
           END-IF.

       C90-LIST-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (LISTMAPI)
           END-EXEC.

           GO TO Z80-HELP-RETURN.

      /*****************************************************************
      *    HELP ROUTINE                                                *
      ******************************************************************
       D00-HELP-ROUTINE.

           IF  ADDRESS OF HELPMAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF HELPMAPI)
                         LENGTH   (LENGTH OF HELPMAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
               SET ADDRESS OF HELPMAPS TO ADDRESS OF HELPMAPI
           END-IF.

           IF  COMM-CURRRTN NOT = 'HELP'
           OR  COMM-HELPAREA(1:4)    = INIT-FLAG
               IF COMM-HELPAREA(1:4) = INIT-FLAG
                   MOVE SPACES         TO COMM-HELPAREA(1:4)
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               PERFORM D80-HELP-OUTPUT
               EXEC CICS SEND
                         MAP      (HELPMAP)
                         FROM     (HELPMAPO)
                         MAPSET   (HELPMS)
                         TERMINAL
                         ERASE
                         NOHANDLE
               END-EXEC
               GO TO D50-HELP-DISPLAY
           END-IF.

       D10-HELP-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (HELPMAP)
                     INTO     (HELPMAPI)
                     MAPSET   (HELPMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO D70-HELP-ERRORS
           END-IF.

       D20-HELP-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               PERFORM Q35-DELETEQ-TS
/DLC3/         MOVE SPACES             TO TS-QUEUE
               EVALUATE TRUE
               WHEN COMM-LINKPGM = 'HELP'
                   GO TO Z90-HELP-TERMINATION
               WHEN HELP-STARTED-FOR-LIST
                   MOVE 'RETN'         TO COMM-CURRRTN
                   GO TO C00-LIST-ROUTINE
               WHEN OTHER
                   IF  COMM-HELPTRAN > SPACES
                   AND COMM-LINKTRN  > SPACES
                       MOVE COMM-LINKTRN TO COMM-LINKPGM
                   END-IF
                   GO TO Z90-HELP-TERMINATION
               END-EVALUATE

           WHEN AIDPFK13
               IF  COMM-HELPAREA(1:4) <= SPACES
                   MOVE COMM-CURRRTN   TO COMM-HELPAREA(1:4)
                   PERFORM T40-SAVE-COMMAREA THRU T45-EXIT
                   MOVE 'HELP'         TO COMM-CURRPGM
                   GO TO T51-HELP-SAVE-DONE
               END-IF
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'Already viewing Help-on-Help.'
                                       TO LIST-MESSAGE-O
               GO TO D80-HELP-OUTPUT

           WHEN AIDPFK01
           WHEN AIDPFK02
           WHEN AIDPFK04
           WHEN AIDPFK05
           WHEN AIDPFK12
               EVALUATE TRUE
               WHEN COMM-HELPNAME <= SPACES
                   GO TO D30-HELP-PROCESS
               WHEN EIBAID = AIDPFK01
                   IF  COMM-HELPAREA(1:4) <= SPACES
                     MOVE COMM-CURRRTN TO COMM-HELPAREA(1:4)
                     PERFORM T40-SAVE-COMMAREA THRU T45-EXIT
                     MOVE 'HELP'       TO COMM-CURRPGM
                     GO TO T51-HELP-SAVE-DONE
                   END-IF
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO HELP-MESSAGE-H
                   MOVE 'Already viewing Help-on-Help.'
                                       TO HELP-MESSAGE-O
                   GO TO D80-HELP-OUTPUT
               WHEN OTHER
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO HELP-MESSAGE-H
                   MOVE 'That function key has no current action assigne
      -                 'd.'           TO HELP-MESSAGE-O
                   GO TO D80-HELP-OUTPUT
               END-EVALUATE

           WHEN AIDENTER
           WHEN AIDPFK03
           WHEN AIDPFK06
           WHEN AIDPFK07
           WHEN AIDPFK08
           WHEN AIDPFK09
           WHEN AIDPFK10
           WHEN AIDPFK11
           WHEN AIDPFK22
           WHEN AIDPFK24
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO HELP-MESSAGE-H
               MOVE 'That attention key has no current action assigned.'
                                       TO HELP-MESSAGE-O
               GO TO D80-HELP-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO HELP-MESSAGE-H
               MOVE 'That function key has no current action assigned.'
                                       TO HELP-MESSAGE-O
               GO TO D80-HELP-OUTPUT
           END-EVALUATE.

       D30-HELP-PROCESS.

/DLC0/     IF  EIBAID = AIDPFK22
/DLC0/         MOVE 1                  TO HELP-SAVESEQU
/DLC0/         GO TO H00-PRINT-ROUTINE
/DLC0/     END-IF.

           IF  EIBAID = AIDPFK24
               IF  COMM-HELPAREA(1:4) > SPACES
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO HELP-MESSAGE-H
                   MOVE 'Cannot edit in Help-on-Help mode.'
                                       TO HELP-MESSAGE-O
                   GO TO D80-HELP-OUTPUT
               END-IF
               IF  NOT HELP-READONLY
                   GO TO E00-EDIT-ROUTINE
               END-IF
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO HELP-MESSAGE-H
               MOVE 'Not authorized for that function.'
                                       TO HELP-MESSAGE-O
               GO TO D80-HELP-OUTPUT
           END-IF.

      * convert cursor position to row and column coordinates
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.
      * adjust row and column coordinates for header/detail screens
           SUBTRACT HELPHEAD         FROM ROW.
           COMPUTE COL = (COL + HELPCWDT) / HELPCWDT.

           IF  COMM-HELPNAME > SPACES

               EVALUATE EIBAID
               WHEN AIDPFK03
                   MOVE SPACES         TO COMM-HELPNAME
                   PERFORM Q35-DELETEQ-TS
                   PERFORM Q20-INITKEY-SYHELPD
                   MOVE COMM-HELPTRAN  TO HELPD-TRAN
                   MOVE COMM-HELPNAME  TO HELPD-NAME
                   MOVE 1              TO HELPD-SEQU
                   PERFORM Q26-READEQ-SYHELPD
                   IF  EIBRESP = DFHRESP(NOTFND)
                       MOVE DFHRESP(NORMAL) TO EIBRESP
                       IF  HELP-STARTED-FOR-LIST
                           MOVE 'RETN' TO COMM-CURRRTN
                           GO TO C00-LIST-ROUTINE
                       END-IF
                       IF  COMM-HELPTRAN > SPACES
                       AND COMM-LINKTRN  > SPACES
                           MOVE COMM-LINKTRN TO COMM-LINKPGM
                       END-IF
                       GO TO Z90-HELP-TERMINATION
                   END-IF
               WHEN AIDPFK06
                   MOVE ZEROES         TO HELP-ADJUSTED
                   MOVE SPACES         TO HELP-SECTION
                   IF  TS-ITEM > 1
                       MOVE 1          TO TS-ITEM
                   ELSE
                       MOVE 'This is the first page.'
                                       TO HELP-MESSAGE-O
                   END-IF
               WHEN AIDPFK07
                   MOVE ZEROES         TO HELP-ADJUSTED
                   MOVE SPACES         TO HELP-SECTION
                   IF  TS-ITEM > 1
                       IF  ROW < 1 OR HELPROWS <= ROW
                         IF  SECTION-HEADERS-ABSENT
                           SUBTRACT HELPPAGE FROM TS-ITEM
                         ELSE
                           PERFORM P80-FIND-PREV-SECTION THRU P85-EXIT
                           IF  EIBRESP NOT = DFHRESP(NORMAL)
                             GO TO D70-HELP-ERRORS
                           END-IF
                         END-IF
                       ELSE
                         IF  COL > 1
                           ADD  1      TO TS-ITEM
                         END-IF
                         COMPUTE TS-ITEM = TS-ITEM
                                         - (HELPROWS - ROW)
                       END-IF
                       IF  TS-ITEM < 1
                         MOVE 1        TO TS-ITEM
                       END-IF
                   ELSE
                       MOVE 'This is the first page.'
                                       TO HELP-MESSAGE-O
                   END-IF
               WHEN AIDPFK08
                   IF  TS-ITEM < (TS-TOTL - (HELPROWS - 1))
                       IF  ROW <= 1 OR HELPROWS < ROW
                         ADD  HELPPAGE TO TS-ITEM
                       ELSE
                         COMPUTE TS-ITEM = TS-ITEM + (ROW - 1)
                       END-IF
                       IF  HELP-ADJUSTED > ZEROES
                         SUBTRACT HELP-ADJUSTED FROM TS-ITEM
                         MOVE ZEROES   TO HELP-ADJUSTED
                       END-IF
                       IF  TS-ITEM > TS-TOTL
                         MOVE ZEROES   TO HELP-ADJUSTED
                         MOVE SPACES   TO HELP-SECTION
                         COMPUTE TS-ITEM = TS-TOTL - (HELPROWS - 1)
                         IF  TS-ITEM < 1
                           MOVE 1      TO TS-ITEM
                         END-IF
                       END-IF
                   ELSE
                       MOVE 'This is the last page.'
                                       TO HELP-MESSAGE-O
                   END-IF
               WHEN AIDPFK09
                   MOVE ZEROES         TO HELP-ADJUSTED
                   MOVE SPACES         TO HELP-SECTION
                   IF  TS-ITEM < (TS-TOTL - (HELPROWS - 1))
                       COMPUTE TS-ITEM = TS-TOTL - (HELPROWS - 1)
                       IF  TS-ITEM < 1
                         MOVE 1        TO TS-ITEM
                       END-IF
                   ELSE
                       MOVE 'This is the last page.'
                                       TO HELP-MESSAGE-O
                   END-IF
               WHEN AIDPFK10
               WHEN AIDPFK11
                   IF  SECTION-HEADERS-ABSENT
                     MOVE -2           TO ERROR-SWITCH
                     MOVE EXHREVRS     TO HELP-MESSAGE-H
                     MOVE 'No section headers present in help page.'
                                       TO HELP-MESSAGE-O
                     GO TO D80-HELP-OUTPUT
                   END-IF
                   MOVE ZEROES         TO HELP-ADJUSTED
                   MOVE SPACES         TO HELP-SECTION
                   IF  EIBAID = AIDPFK10
                     PERFORM P80-FIND-PREV-SECTION THRU P85-EXIT
                   ELSE
                     PERFORM P90-FIND-NEXT-SECTION THRU P95-EXIT
                   END-IF
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                     GO TO D70-HELP-ERRORS
                   END-IF
               END-EVALUATE

           ELSE

               PERFORM Q20-INITKEY-SYHELPD
               MOVE COMM-HELPTRAN      TO HELPD-TRAN
               MOVE SPACES             TO HELPD-NAME
               MOVE ZEROES             TO HELPD-SEQU
               PERFORM Q26-READEQ-SYHELPD
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NOTFND)
                       GO TO D70-HELP-ERRORS
                   END-IF
                   MOVE DFHRESP(NORMAL) TO EIBRESP
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO HELP-MESSAGE-H
                   MOVE 'No help pages assigned for selection.'
                                       TO HELP-MESSAGE-O
                   GO TO D80-HELP-OUTPUT
               END-IF
               EVALUATE EIBAID
               WHEN AIDPFK12 MOVE 12   TO IDX
               WHEN AIDPFK11 MOVE 11   TO IDX
               WHEN AIDPFK10 MOVE 10   TO IDX
               WHEN OTHER    MOVE EIBAID TO DIGIT
                             MOVE DIGIT TO IDX
               END-EVALUATE
               IF  HELPD-ENTRY(IDX) > SPACES
                   MOVE HELPD-ENTRY(IDX) TO COMM-HELPNAME
                   PERFORM Q35-DELETEQ-TS
               ELSE
                   MOVE DFHRESP(NORMAL) TO EIBRESP
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO HELP-MESSAGE-H
                   MOVE 'Help page not assigned for selection.'
                                       TO HELP-MESSAGE-O
                   GO TO D80-HELP-OUTPUT
               END-IF

           END-IF.

       D50-HELP-DISPLAY.

           MOVE 'HELP'                 TO COMM-CURRRTN.

       D60-HELP-DETAIL.

           IF  TS-TOTL <= ZERO
               PERFORM P10-EXTRACT-HELP-TEXT THRU P15-EXIT
               IF  ERRORS-FOUND
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO HELP-MESSAGE-H
                   IF  HELP-MESSAGE-I <= SPACES
                       PERFORM X00-UNEX-ERR
                       MOVE UNEX-MSG   TO HELP-MESSAGE-O
                   END-IF
                   GO TO D80-HELP-OUTPUT
               END-IF
           END-IF.

           MOVE 1                      TO IDX
                                          HELP-BUFLEN.
           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR COMM-HELPNAME > SPACES
                 AND IDX > HELPPAGE
                  OR COMM-HELPNAME <= SPACES
                 AND IDX > (HELPPAGE + 1)
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS

             IF  EIBRESP = DFHRESP(NORMAL)
               IF  TS-RECD <= SPACES
               AND IDX = 1
                 IF  SS-ITEM = TS-TOTL
                   MOVE SPACES         TO VAR-TEXT
                   PERFORM P33-CHECK-END-OF-HELP THRU P35-EXIT
                 ELSE
                   SUBTRACT 1        FROM IDX
                   ADD  1              TO TS-ITEM
                 END-IF
               ELSE
                 IF  TS-RECD(1:3) = '==='
                   PERFORM P70-SAVE-SECTION-HEADER THRU P75-EXIT
                 END-IF
                 IF  IDX = 1
                 AND TS-RECD(1:3) NOT = '==='
                 AND HELP-SECTION > SPACES
                   MOVE HELP-SECTION   TO VAR-TEXT
                   PERFORM Q90-SET-LENGTH
                   MOVE SPACES         TO VAR-TEXT
                   STRING '==='
                          HELP-SECTION(1:VAR-TEXTL)
                          '===(continued)'
                     DELIMITED BY SIZE INTO VAR-TEXT
                   PERFORM P30-BUILD-HELP-BUFFER
                   ADD  1              TO IDX
                                          HELP-ADJUSTED
                   MOVE SPACES         TO VAR-TEXT
                   PERFORM P30-BUILD-HELP-BUFFER
                   ADD  1              TO IDX
                                          HELP-ADJUSTED
                 END-IF
                 MOVE TS-RECD          TO VAR-TEXT
                 PERFORM P30-BUILD-HELP-BUFFER THRU P35-EXIT
               END-IF
             END-IF
             ADD  1                    TO IDX
           END-PERFORM.

           SUBTRACT 1                FROM HELP-BUFLEN.

       D70-HELP-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO HELP-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO HELP-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       D80-HELP-OUTPUT.

           MOVE MAPTITLE               TO HELP-COMPANY-O.
           MOVE WS-WRKDATE             TO HELP-SYSDATE-O.
           MOVE WS-WRKTIME             TO HELP-SYSTIME-O.
           MOVE CICS-NETNAME           TO HELP-NETNAME-O.
           IF  CICS-NETNAME NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO HELP-NETNAME-O
           END-IF.

           IF  COMM-HELPNAME <= SPACES
               MOVE FLDASDRK           TO HELP-END-LABEL-A
                                          HELP-END-ACTION-A
                                          HELP-PREV-LABEL-A
                                          HELP-PREV-ACTION-A
                                          HELP-NEXT-LABEL-A
                                          HELP-NEXT-ACTION-A
                                          HELP-SECTION-LABEL-A
                                          HELP-SECTION-ACTION-A
           END-IF.
           IF  SECTION-HEADERS-ABSENT
               MOVE FLDASDRK           TO HELP-SECTION-LABEL-A
                                          HELP-SECTION-ACTION-A
           END-IF.
           IF  HELP-READONLY
           OR  COMM-HELPAREA(1:4) > SPACES
               MOVE 'F22'              TO HELP-EDIT-LABEL-O
               MOVE '= Print.'         TO HELP-EDIT-ACTION-O
           END-IF.

           MOVE COMM-HELPTRAN          TO HELP-TRAN-CODE-O.
           MOVE COMM-HELPNAME          TO HELP-NAME-CODE-O.

       D85-HELP-SENDMAP.

           IF  ERRORS-FOUND
               EXEC CICS SEND
                         MAP      (HELPMAP)
                         DATAONLY
                         FROM     (HELPMAPO)
                         MAPSET   (HELPMS)
                         TERMINAL
                         FREEKB
                         ALARM
                         CURSOR   (EIBCPOSN)
               END-EXEC
           ELSE
               MOVE -1                 TO HELP-ESC-LABEL-L
               EXEC CICS SEND
                         MAP      (HELPMAP)
                         FROM     (HELPMAPO)
                         MAPSET   (HELPMS)
                         TERMINAL
                         ERASE
                         CURSOR
               END-EXEC
               IF  HELP-BUFLEN > ZERO
                   EXEC CICS SEND
                             FROM     (HELP-BUFFER)
                             LENGTH   (HELP-BUFLEN)
                             CTLCHAR  (WRTFKFRS)
                   END-EXEC
               ELSE
                   EXEC CICS SEND CONTROL
                             FREEKB
                             FRSET
                   END-EXEC
               END-IF
           END-IF.

       D90-HELP-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (HELPMAPI)
           END-EXEC.

           GO TO Z80-HELP-RETURN.

      /*****************************************************************
      *    EDIT ROUTINE                                                *
      ******************************************************************
       E00-EDIT-ROUTINE.

           IF  ADDRESS OF HELPMAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF HELPMAPI)
                         LENGTH   (LENGTH OF HELPMAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
               SET ADDRESS OF HELPMAPS TO ADDRESS OF HELPMAPI
           END-IF.

           IF  COMM-CURRRTN NOT = 'EDIT'
           OR  COMM-HELPAREA(1:4)    = INIT-FLAG
               IF COMM-HELPAREA(1:4) = INIT-FLAG
                   MOVE SPACES         TO COMM-HELPAREA(1:4)
               END-IF
               EXEC CICS SET TERMINAL(EIBTRMID)
                         TRANIDONLY
                         NOHANDLE
               END-EXEC
               SET  NO-ERRORS-FOUND    TO TRUE
               PERFORM E80-EDIT-OUTPUT
               EXEC CICS SEND
                         MAP      (HELPMAP)
                         FROM     (HELPMAPO)
                         MAPSET   (HELPMS)
                         TERMINAL
                         ERASE
                         NOHANDLE
               END-EXEC
               MOVE 1                  TO ROW COL
               GO TO E50-EDIT-DISPLAY
           END-IF.

       E10-EDIT-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (HELPMAP)
                     INTO     (HELPMAPI)
                     MAPSET   (HELPMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO E70-EDIT-ERRORS
           END-IF.

       E20-EDIT-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
           WHEN AIDPFK03
/DLC2/         MOVE SPACES             TO MULTI-EDIT-MSG
/DLC2/         MOVE 1                  TO LEN
/DLC2/         STRING 'This action will discard any changes.  '
/DLC2/                'Press F2 to confirm.' DELIMITED BY SIZE
/DLC2/           INTO MULTI-EDIT-MSG WITH POINTER LEN
/DLC2/         SUBTRACT 1            FROM LEN
/DLC2/         PERFORM V10-GET-CONFIRMATION THRU V15-EXIT
/DLC2/         IF  EIBAID  NOT = AIDPFK02
/DLC2/         OR  EIBRESP NOT = DFHRESP(NORMAL)
/DLC2/             MOVE AIDENTER       TO EIBAID
/DLC2/         ELSE
                   PERFORM Q35-DELETEQ-TS
/DLC3/             MOVE SPACES         TO TS-QUEUE
                   EXEC CICS SET TERMINAL(EIBTRMID)
                             UCTRAN
                             NOHANDLE
                   END-EXEC
                   GO TO D00-HELP-ROUTINE
/DLC2/         END-IF

           WHEN AIDPFK01
               IF  COMM-HELPAREA(1:4) <= SPACES
                   EXEC CICS SET TERMINAL(EIBTRMID)
                             UCTRAN
                             NOHANDLE
                   END-EXEC
                   MOVE COMM-CURRRTN   TO COMM-HELPAREA(1:4)
                   PERFORM T40-SAVE-COMMAREA THRU T45-EXIT
                   MOVE 'HELP'         TO COMM-CURRPGM
                   GO TO T51-HELP-SAVE-DONE
               END-IF
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'Already viewing Help-on-Help.'
                                       TO LIST-MESSAGE-O
               GO TO E80-EDIT-OUTPUT

           WHEN AIDENTER
           WHEN AIDPFK02
           WHEN AIDPFK04
           WHEN AIDPFK05
           WHEN AIDPFK06
           WHEN AIDPFK07
           WHEN AIDPFK08
           WHEN AIDPFK09
           WHEN AIDPFK10
           WHEN AIDPFK11
           WHEN AIDPFK12
           WHEN AIDPFK13
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO HELP-MESSAGE-H
               MOVE 'That attention key has no current action assigned.'
                                       TO HELP-MESSAGE-O
               GO TO E80-EDIT-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO HELP-MESSAGE-H
               MOVE 'That function key has no current action assigned.'
                                       TO HELP-MESSAGE-O
               GO TO E80-EDIT-OUTPUT
           END-EVALUATE.

       E30-EDIT-PROCESS.

      * PERFORM DETAIL FIELD EDITING HERE

           MOVE DFHRESP(NORMAL)        TO EIBRESP.

      * loop on body text lines to save them
           MOVE ZEROES                 TO IDX.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL IDX > HELPPAGE
                  OR EIBRESP NOT = DFHRESP(NORMAL)
             ADD  1                    TO IDX
             PERFORM P20-CONVERT-IDX-TO-POS THRU P25-EXIT

             MOVE SPACES               TO TS-RECD
             PERFORM Q32-READQ-TS

             IF  HELP-TEXT-L(ROW COL)   > ZERO
             OR  HELP-TEXT-F(ROW COL)   = FLDMODFY OR FLDCURSM
               IF  HELP-TEXT-I(ROW COL) > SPACES
                 MOVE HELP-TEXT-I(ROW COL)
                                       TO TS-RECD
                 SET  TXT-REQUEST-FIND TO TRUE
                 MOVE ZEROES           TO TXT-PNTR
                 MOVE 3                TO TXT-PARM-STRLEN
                 MOVE '==='            TO TXT-PARM-STRING
                 MOVE LENGTH OF TS-RECD
                                       TO TXT-MSTR-BUFLEN
                 MOVE TS-RECD          TO TXT-MSTR-BUFFER
                 CALL 'TXTMAN'      USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
                 IF  TXT-STRING-WAS-FOUND
                   IF  TXT-PNTR > 1
                     MOVE EXHREVRS     TO HELP-MESSAGE-H
                     MOVE 'Section headers must start in the first posit
      -                   'ion.'       TO HELP-MESSAGE-O
                   ELSE
                     SET  SECTION-HEADERS-PRESENT
                                       TO TRUE
                   END-IF
                 END-IF
               ELSE
                 MOVE SPACES           TO TS-RECD
               END-IF
             END-IF

             IF  EIBRESP = DFHRESP(ITEMERR)
               IF  TS-RECD <= SPACES
                 MOVE DFHRESP(NORMAL)  TO EIBRESP
               ELSE
                 PERFORM Q33-WRITEQ-TS
                 ADD  1                TO TS-TOTL
               END-IF
             ELSE
               PERFORM Q34-REWRITEQ-TS
             END-IF

             IF  SS-ITEM >= HELP-MAX
               MOVE DFHRESP(ENDFILE)   TO EIBRESP
             END-IF
           END-PERFORM.

           IF  ERRORS-FOUND
               GO TO E80-EDIT-OUTPUT
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               GO TO E70-EDIT-ERRORS
           END-IF.

      * END OF DETAIL FIELD EDITING

           IF  EIBAID = AIDPFK05
               GO TO E40-EDIT-UPDATE
           END-IF.

           IF (EIBAID = AIDPFK06 OR AIDPFK07 OR AIDPFK08
                     OR AIDPFK09 OR AIDPFK10 OR AIDPFK11)
           AND COMM-HELPNAME <= SPACES
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO HELP-MESSAGE-H
               MOVE 'Help selection pages are not scrollable.'
                                       TO HELP-MESSAGE-O
               GO TO E80-EDIT-OUTPUT
           END-IF.

      * convert cursor position to row and column coordinates
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.
      * adjust row and column coordinates for header/detail screens
           SUBTRACT HELPHEAD         FROM ROW.
           COMPUTE COL = (COL + HELPCWDT) / HELPCWDT.

           EVALUATE EIBAID
           WHEN AIDPFK02
      * (re)flow the current paragraph
               COMPUTE SS-ITEM = TS-ITEM
                               + (HELPROWS * (COL - 1))
                               + (ROW - 1)
               IF  SS-ITEM <= TS-TOTL
                 PERFORM Q50-FLOW-PARAGRAPH THRU Q55-EXIT
                 IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO E70-EDIT-ERRORS
                 END-IF
               ELSE
                 MOVE 'End of data.'   TO HELP-MESSAGE-O
               END-IF

           WHEN AIDPFK04
      * delete text line processing
               COMPUTE SS-ITEM = TS-ITEM
                               + (HELPROWS * (COL - 1))
                               + (ROW - 1)
               IF  SS-ITEM <= TS-TOTL
                 MOVE SPACES           TO MULTI-EDIT-MSG
                 MOVE 1                TO LEN
                 STRING 'Delete this help text line?  '
                        'Press F2 to confirm.' DELIMITED BY SIZE
                     INTO MULTI-EDIT-MSG WITH POINTER LEN
                 SUBTRACT 1          FROM LEN
                 PERFORM V10-GET-CONFIRMATION THRU V15-EXIT
                 IF  EIBAID  NOT = AIDPFK02
                 OR  EIBRESP NOT = DFHRESP(NORMAL)
                     GO TO E50-EDIT-DISPLAY
                 END-IF
                 PERFORM Q60-DELETE-LINE THRU Q65-EXIT
                 IF  EIBRESP NOT = DFHRESP(NORMAL)
                     GO TO E70-EDIT-ERRORS
                 END-IF
               ELSE
                 MOVE 'End of data.'   TO HELP-MESSAGE-O
               END-IF

           WHEN AIDPFK06
      * scroll to first help page
               IF  TS-ITEM > 1
                   MOVE 1              TO TS-ITEM
               ELSE
                   MOVE 'This is the first page.'
                                       TO HELP-MESSAGE-O
               END-IF
               GO TO E50-EDIT-DISPLAY

           WHEN AIDPFK07
      * scroll to previous help page
               IF  TS-ITEM > 1
                   IF  ROW < 1 OR HELPROWS <= ROW
                       SUBTRACT HELPPAGE FROM TS-ITEM
                   ELSE
                       IF  COL > 1
                           ADD  1      TO TS-ITEM
                       END-IF
                       COMPUTE TS-ITEM = TS-ITEM
                                       - (HELPROWS - ROW)
                   END-IF
                   IF  TS-ITEM < 1
                       MOVE 1          TO TS-ITEM
                   END-IF
               ELSE
                   MOVE 'This is the first page.'
                                       TO HELP-MESSAGE-O
               END-IF
               GO TO E50-EDIT-DISPLAY

           WHEN AIDPFK08
      * scroll to next help page
               IF  TS-ITEM < (TS-TOTL - (HELPROWS - 1))
                   IF  ROW <= 1 OR HELPROWS < ROW
                       ADD  HELPPAGE   TO TS-ITEM
                   ELSE
                       COMPUTE TS-ITEM = TS-ITEM + (ROW - 1)
                   END-IF
                   IF  TS-ITEM > TS-TOTL
                       COMPUTE TS-ITEM = TS-TOTL - (HELPROWS - 1)
                       IF  TS-ITEM < 1
                           MOVE 1      TO TS-ITEM
                       END-IF
                   END-IF
               ELSE
                   MOVE 'This is the last page.'
                                       TO HELP-MESSAGE-O
               END-IF
               GO TO E50-EDIT-DISPLAY

           WHEN AIDPFK09
      * scroll to last help page
               IF  TS-ITEM < (TS-TOTL - (HELPROWS - 1))
                   COMPUTE TS-ITEM = TS-TOTL - (HELPROWS - 1)
                   IF  TS-ITEM < 1
                     MOVE 1            TO TS-ITEM
                   END-IF
               ELSE
                   MOVE 'This is the last page.'
                                       TO HELP-MESSAGE-O
               END-IF
               GO TO E50-EDIT-DISPLAY

           WHEN AIDPFK10
           WHEN AIDPFK11
      * scroll to prev/next help section header
               IF  SECTION-HEADERS-ABSENT
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO HELP-MESSAGE-H
                   MOVE 'No section headers present in help page.'
                                       TO HELP-MESSAGE-O
                   GO TO E80-EDIT-OUTPUT
               END-IF
               IF  EIBAID = AIDPFK10
                   PERFORM P80-FIND-PREV-SECTION THRU P85-EXIT
               ELSE
                   PERFORM P90-FIND-NEXT-SECTION THRU P95-EXIT
               END-IF
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO E70-EDIT-ERRORS
               END-IF
               GO TO E50-EDIT-DISPLAY

           WHEN AIDPFK12
      * exit to the selections screen for a main help page
               IF  COMM-HELPNAME <= SPACES
                   GO TO F00-SELT-ROUTINE
               END-IF
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO HELP-MESSAGE-H
               MOVE 'Selections are only for a main help page.'
                                       TO HELP-MESSAGE-O
               GO TO E80-EDIT-OUTPUT

           WHEN AIDPFK13
      * split/join processing
               COMPUTE SS-ITEM = TS-ITEM
                               + (HELPROWS * (COL - 1))
                               + (ROW - 1)
               IF  SS-ITEM <= TS-TOTL
                 PERFORM Q80-SPLTJOIN-LINE THRU Q85-EXIT
                 IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO E70-EDIT-ERRORS
                 END-IF
               ELSE
                 MOVE 'End of data.'   TO HELP-MESSAGE-O
               END-IF

           END-EVALUATE.

           GO TO E50-EDIT-DISPLAY.

       E40-EDIT-UPDATE.

           PERFORM Q20-INITKEY-SYHELPD.

      * reload all ts records into dataset
           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM 1 BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS

             MOVE COMM-HELPTRAN        TO HELPD-TRAN
             MOVE COMM-HELPNAME        TO HELPD-NAME
             MOVE SS-ITEM              TO HELPD-SEQU
             PERFORM Q26-READUP-SYHELPD
             IF  EIBRESP = DFHRESP(NORMAL)
                        OR DFHRESP(NOTFND)
               INSPECT TS-RECD REPLACING ALL FLDMRK BY SPACE
               MOVE TS-RECD            TO HELPD-DATA
               IF  EIBRESP = DFHRESP(NOTFND)
                   PERFORM Q27-WRITE-SYHELPD
               ELSE
                   PERFORM Q27-REWRITE-SYHELPD
               END-IF
             END-IF
           END-PERFORM.

      * remove orphaned records
           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM SS-ITEM BY 1
               UNTIL EIBRESP NOT = DFHRESP(NORMAL)

             MOVE COMM-HELPTRAN        TO HELPD-TRAN
             MOVE COMM-HELPNAME        TO HELPD-NAME
             MOVE SS-ITEM              TO HELPD-SEQU
             PERFORM Q26-READUP-SYHELPD
             IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q28-DELETE-SYHELPD
             END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(NOTFND)
               GO TO E70-EDIT-ERRORS
           END-IF.

           PERFORM Q35-DELETEQ-TS.
/DLC3/     MOVE SPACES                 TO TS-QUEUE.

           EXEC CICS SET TERMINAL(EIBTRMID)
                     UCTRAN
                     NOHANDLE
           END-EXEC.

           MOVE COMM-CURRRTN           TO COMM-SAVE-FUNC.
           MOVE 'RETN'                 TO COMM-CURRRTN.
           GO TO D00-HELP-ROUTINE.

       E50-EDIT-DISPLAY.

           MOVE 'EDIT'                 TO COMM-CURRRTN.

       E60-EDIT-DETAIL.
           MOVE ZEROES                 TO HELP-ADJUSTED.
           MOVE SPACES                 TO HELP-SECTION.

           IF  TS-TOTL <= ZERO
/DLC3/     OR  TS-QUALIFIER(1:4) NOT = COMM-CURRRTN
/DLC3/         IF  TS-QUEUE <= SPACES
/DLC3/         OR  TS-QUALIFIER(1:4) NOT = COMM-CURRRTN
/DLC3/             PERFORM Q35-DELETEQ-TS
/DLC3/             MOVE 'EDITING1'     TO TS-QUALIFIER
/DLC3/         END-IF
               PERFORM P10-EXTRACT-HELP-TEXT THRU P15-EXIT
               IF  ERRORS-FOUND
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO HELP-MESSAGE-H
                   IF  HELP-MESSAGE-I <= SPACES
                       PERFORM X00-UNEX-ERR
                       MOVE UNEX-MSG   TO HELP-MESSAGE-O
                   END-IF
                   GO TO E80-EDIT-OUTPUT
               END-IF
           END-IF.

           MOVE 1                      TO IDX.
           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR COMM-HELPNAME > SPACES
                 AND IDX > HELPPAGE
                  OR COMM-HELPNAME <= SPACES
                 AND IDX > (HELPPAGE + 1)
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS
             IF  EIBRESP = DFHRESP(NORMAL)
                 PERFORM P20-CONVERT-IDX-TO-POS THRU P25-EXIT

      * BUILD DETAIL LINE OUTPUT HERE
                 MOVE LOW-VALUES       TO HELPMAP-COL-DETAIL(ROW COL)
                 MOVE FLDUNPRO         TO HELP-TEXT-A(ROW COL)
                 MOVE TS-RECD          TO HELP-TEXT-O(ROW COL)
             ELSE
               SUBTRACT 1            FROM IDX
             END-IF
             ADD  1                    TO IDX
           END-PERFORM.

           IF  TS-TOTL >= HELP-MAX
               MOVE DFHRESP(ENDFILE)   TO EIBRESP
           END-IF.

           IF  COMM-HELPNAME > SPACES
               IF  SS-ITEM > TS-TOTL
                   MOVE FLDASBRT       TO HELP-TEXT-A(22 2)
                   MOVE END-OF-HELP    TO HELP-TEXT-O(22 2)
               ELSE
                   MOVE FLDASBRT       TO HELP-TEXT-A(22 2)
                   MOVE MORE-HELP      TO HELP-TEXT-O(22 2)
               END-IF
           END-IF.

       E65-EDIT-BLANKS.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM IDX BY 1
               UNTIL IDX > HELPPAGE
             PERFORM P20-CONVERT-IDX-TO-POS THRU P25-EXIT
             MOVE LOW-VALUES           TO HELPMAP-COL-DETAIL(ROW COL)
             MOVE FLDUNPRO             TO HELP-TEXT-A(ROW COL)
           END-PERFORM.

       E70-EDIT-ERRORS.

           IF  EIBRESP = DFHRESP(ENDFILE)
               MOVE 'Maximum size reached.'
                                       TO HELP-MESSAGE-O
               GO TO E80-EDIT-OUTPUT
           END-IF.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO HELP-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO HELP-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       E80-EDIT-OUTPUT.

           MOVE MAPTITLE               TO HELP-COMPANY-O.
           MOVE '             Help Page Edit' TO HELP-TITLE-O.
           MOVE WS-WRKDATE             TO HELP-SYSDATE-O.
           MOVE WS-WRKTIME             TO HELP-SYSTIME-O.
           MOVE CICS-NETNAME           TO HELP-NETNAME-O.
           IF  CICS-NETNAME NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO HELP-NETNAME-O
           END-IF.

           MOVE 'F2'                   TO HELP-END-LABEL-O.
           MOVE '= Flow,'              TO HELP-END-ACTION-O.
           IF  COMM-HELPNAME > SPACES
               IF  SECTION-HEADERS-ABSENT
                   MOVE FLDASDRK       TO HELP-SECTION-LABEL-A
                                          HELP-SECTION-ACTION-A
               END-IF
               MOVE ' F5'              TO HELP-EDIT-LABEL-O
               MOVE '= Update.'        TO HELP-EDIT-ACTION-O
           ELSE
               MOVE FLDASDRK           TO HELP-PREV-LABEL-A
                                          HELP-PREV-ACTION-A
                                          HELP-NEXT-LABEL-A
                                          HELP-NEXT-ACTION-A
                                          HELP-SECTION-LABEL-A
                                          HELP-SECTION-ACTION-A
               MOVE 'F12'              TO HELP-EDIT-LABEL-O
               MOVE '= Selects.'       TO HELP-EDIT-ACTION-O
           END-IF.

           MOVE COMM-HELPTRAN          TO HELP-TRAN-CODE-O.
           MOVE COMM-HELPNAME          TO HELP-NAME-CODE-O.

       E85-EDIT-SENDMAP.

           IF  ERRORS-FOUND
               EXEC CICS SEND
                         MAP      (HELPMAP)
                         DATAONLY
                         FROM     (HELPMAPO)
                         MAPSET   (HELPMS)
                         TERMINAL
                         FREEKB
                         ALARM
                         CURSOR   (EIBCPOSN)
               END-EXEC
           ELSE
               COMPUTE PTR = SCRNWDTH * HELPHEAD
               IF  EIBCPOSN < PTR
               OR  EIBCPOSN > (SCRNWDTH * (HELPHEAD + HELPROWS) - 1)
                   ADD  1  TO PTR  GIVING EIBCPOSN
               END-IF
               EXEC CICS SEND
                         MAP      (HELPMAP)
                         FROM     (HELPMAPO)
                         MAPSET   (HELPMS)
                         TERMINAL
                         FREEKB
                         ERASE
                         CURSOR   (EIBCPOSN)
               END-EXEC
           END-IF.

       E90-EDIT-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (HELPMAPI)
           END-EXEC.

           GO TO Z80-HELP-RETURN.

      /*****************************************************************
      *    SELT ROUTINE                                                *
      ******************************************************************
       F00-SELT-ROUTINE.

           IF  ADDRESS OF SELTWINI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF SELTWINI)
                         LENGTH   (LENGTH OF SELTWINI)
                         INITIMG  (LOVALUE)
               END-EXEC
               SET ADDRESS OF SELTWINS TO ADDRESS OF SELTWINI
           END-IF.

           IF  COMM-CURRRTN NOT = 'SELT'
           OR  COMM-HELPAREA(1:4)    = INIT-FLAG
               IF COMM-HELPAREA(1:4) = INIT-FLAG
                   MOVE SPACES         TO COMM-HELPAREA(1:4)
               END-IF
               EXEC CICS SET TERMINAL(EIBTRMID)
                         UCTRAN
                         NOHANDLE
               END-EXEC
               SET  NO-ERRORS-FOUND    TO TRUE
               EXEC CICS SEND
                         MAP      (HELPXXX)
                         MAPONLY
                         MAPSET   (HELPMS)
                         TERMINAL
                         NOHANDLE
               END-EXEC
               EXEC CICS SEND
                         MAP      (SELTWIN)
                         MAPONLY
                         MAPSET   (HELPMS)
                         TERMINAL
                         NOHANDLE
               END-EXEC
               GO TO F50-SELT-DISPLAY
           END-IF.

       F10-SELT-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (SELTWIN)
                     INTO     (SELTWINI)
                     MAPSET   (HELPMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO F70-SELT-OUTPUT
           END-IF.

       F20-SELT-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
           WHEN AIDPFK03
               EXEC CICS SET TERMINAL(EIBTRMID)
                         TRANIDONLY
                         NOHANDLE
               END-EXEC
               GO TO E00-EDIT-ROUTINE

           WHEN AIDPFK01
               IF  COMM-HELPAREA(1:4) <= SPACES
                   MOVE COMM-CURRRTN   TO COMM-HELPAREA(1:4)
                   PERFORM T40-SAVE-COMMAREA THRU T45-EXIT
                   MOVE 'HELP'         TO COMM-CURRPGM
                   GO TO T51-HELP-SAVE-DONE
               END-IF
               MOVE -2                 TO ERROR-SWITCH
               GO TO F70-SELT-OUTPUT

           WHEN AIDENTER
               CONTINUE

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               GO TO F70-SELT-OUTPUT
           END-EVALUATE.

       F30-SELT-PROCESS.

           PERFORM Q20-INITKEY-SYHELPD.
           MOVE COMM-HELPTRAN          TO HELPD-TRAN.
           MOVE SPACES                 TO HELPD-NAME.
           MOVE ZEROES                 TO HELPD-SEQU.
           PERFORM Q26-READUP-SYHELPD.

           PERFORM F40-SELT-UPDATES THRU F45-EXIT
             VARYING COL FROM 1 BY 1 UNTIL COL > SELTCOLS
                                     OR EIBRESP NOT = DFHRESP(NORMAL)
             AFTER   ROW FROM 1 BY 1 UNTIL ROW > SELTROWS
                                     OR EIBRESP NOT = DFHRESP(NORMAL).

           EVALUATE EIBRESP
           WHEN DFHRESP(NOTFND) PERFORM Q27-WRITE-SYHELPD
           WHEN DFHRESP(NORMAL) PERFORM Q27-REWRITE-SYHELPD
           END-EVALUATE.

           GO TO E00-EDIT-ROUTINE.

       F40-SELT-UPDATES.

           IF  ROW <= SELTROWS
           AND COL <= SELTCOLS
               COMPUTE IDX              = (COL - 1) * SELTROWS
                                        + ROW
               MOVE SELT-SELECTION-I(ROW COL)
                                       TO HELPD-ENTRY(IDX)
           END-IF.

       F45-EXIT.
           EXIT.

       F50-SELT-DISPLAY.

           MOVE 'SELT'                 TO COMM-CURRRTN.

       F60-SELT-DETAIL.

           PERFORM F66-SELT-FILLIT WITH TEST BEFORE
             VARYING COL FROM 1 BY 1 UNTIL COL > SELTCOLS
             AFTER   ROW FROM 1 BY 1 UNTIL ROW > SELTROWS.

           PERFORM Q20-INITKEY-SYHELPD.
           MOVE COMM-HELPTRAN          TO HELPD-TRAN.
           MOVE SPACES                 TO HELPD-NAME.
           MOVE ZEROES                 TO HELPD-SEQU.
           PERFORM Q26-READEQ-SYHELPD.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1
               UNTIL IDX > 12
                  OR EIBRESP NOT = DFHRESP(NORMAL)
               SUBTRACT 1 FROM IDX GIVING OFS
               DIVIDE OFS BY SELTROWS GIVING COL REMAINDER ROW
               ADD  1                  TO ROW COL
               MOVE HELPD-ENTRY(IDX)   TO SELT-SELECTION-O(ROW COL)
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               MOVE -2                 TO ERROR-SWITCH
           END-IF.

           PERFORM Q25-ENDBR-SYHELPD.

           GO TO F70-SELT-OUTPUT.

       F66-SELT-FILLIT.
           MOVE SPACES                 TO SELT-SELECTION-O(ROW COL).

       F70-SELT-OUTPUT.

           IF  ERRORS-FOUND
           OR  EIBRESP NOT = DFHRESP(NORMAL)
               EXEC CICS SEND
                         MAP      (SELTERR)
                         MAPONLY
                         MAPSET   (HELPMS)
                         TERMINAL
                         WAIT
                         ALARM
                         CURSOR   (EIBCPOSN)
                         NOHANDLE
               END-EXEC
               EXEC CICS DELAY
                         INTERVAL(1)
                         NOHANDLE
               END-EXEC
           END-IF.

           MOVE -1                     TO SELT-SELECTION-L(1 1).
           EXEC CICS SEND
                     MAP      (SELTWIN)
                     FROM     (SELTWINO)
                     MAPSET   (HELPMS)
                     TERMINAL
                     FREEKB
                     CURSOR
           END-EXEC.

       F90-SELT-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (SELTWINI)
           END-EXEC.

           GO TO Z80-HELP-RETURN.

/DLC0//*****************************************************************
  |   *    PRINT ROUTINE                                               *
  |   ******************************************************************
  v    H00-PRINT-ROUTINE.

           MOVE 'Generating...  '      TO POPUP-TEXT.
           PERFORM Q10-DISPLAY-POPUP THRU Q15-EXIT.

       H10-PRINT-SETUP.

           PERFORM Q20-INITKEY-SYHELPD.
           MOVE COMM-HELPTRAN          TO HELPD-TRAN
                                          HEAD-HELP.
           MOVE COMM-HELPNAME          TO HELPD-NAME
                                          HEAD-NAME.
           MOVE 1                      TO HELPD-SEQU.

           PERFORM Q21-STARTBR-SYHELPD.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               PERFORM Q200-UNEX-LOG
               IF  COMM-CURRRTN = 'HELP'
                   MOVE 'No help data found.  See log.'
                                       TO HELP-MESSAGE-O
                   GO TO D80-HELP-OUTPUT
               END-IF
               MOVE 'No help data found.  See log.'
                                       TO LIST-MESSAGE-O
               GO TO C80-LIST-OUTPUT
           END-IF.

           MOVE SPACES                 TO PRT-LNK-REQU
                                          PRT-STR-DATA.

           SET  PRT-PWR-LSTQ           TO TRUE.
           MOVE 'P610'                 TO PRT-PWR-FNO.
           IF  HEAD-NAME > SPACES
               STRING HEAD-HELP HEAD-NAME
                   DELIMITED BY SIZE INTO PRT-PWR-JOBN
           ELSE
               STRING HEAD-HELP '@SEL'
                   DELIMITED BY SIZE INTO PRT-PWR-JOBN
           END-IF.
           MOVE CICS-USERID            TO PRT-PWR-USER.
           EXEC CICS ASSIGN NETNAME(PRT-PWR-UINF) END-EXEC.
           MOVE 10                     TO PTR.
           STRING CICS-OPID               DELIMITED BY SIZE
               INTO PRT-PWR-UINF     WITH POINTER PTR.

       H20-PRINT-HELP-DATA.

           PERFORM WITH TEST BEFORE
             UNTIL HELPD-TRAN NOT = COMM-HELPTRAN
                OR HELPD-NAME NOT = COMM-HELPNAME
                OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q23-READNEXT-SYHELPD
             IF  EIBRESP = DFHRESP(NORMAL)
             AND HELPD-TRAN = COMM-HELPTRAN
             AND HELPD-NAME = COMM-HELPNAME
               IF  LINE-CNT >= 60
                 PERFORM H40-PRINT-HEADER
               END-IF
               IF  HELPD-DATA(1:3) = '==='
                 IF  LINE-CNT >= 58
                   PERFORM H40-PRINT-HEADER
                 END-IF
                 PERFORM H50-PRINT-SECTION
               ELSE
                 MOVE SPACE            TO PRT-LNK-REQU
                 IF  HELPD-DATA(1:1) = '{'
                   MOVE HELPD-DATA     TO VAR-LINE(8:)
                 ELSE
                   MOVE HELPD-DATA     TO VAR-LINE(9:)
                 END-IF
                 PERFORM H60-PRINT-TEXT
               END-IF
             END-IF
           END-PERFORM.

           IF  PRINT-COMPLETED
           AND (EIBRESP = DFHRESP(NORMAL) OR DFHRESP(ENDFILE))
             PERFORM Q25-ENDBR-SYHELPD
             SET  PRINT-ENDOFMSG       TO TRUE
             PERFORM H80-CALL-PRTMAN THRU H85-EXIT
           END-IF.

           IF  NOT PRINT-COMPLETED
           OR  EIBRESP NOT = DFHRESP(NORMAL)
               IF  COMM-CURRRTN = 'HELP'
                   MOVE 'Print submit failed.  See log.'
                                       TO HELP-MESSAGE-O
                   GO TO D80-HELP-OUTPUT
               END-IF
               MOVE 'Print submit failed.  See log.'
                                       TO LIST-MESSAGE-O
               GO TO C80-LIST-OUTPUT
           END-IF.

           GO TO H90-PRINT-RETURN.

       H40-PRINT-HEADER.
           ADD  1                      TO PAGE-CNT.
           MOVE PAGE-CNT               TO HEAD-PAGE.

           MOVE HELP-HEAD(1:1)         TO PRT-LNK-REQU.
           MOVE HELP-HEAD(2:)          TO VAR-LINE.
           PERFORM H60-PRINT-TEXT.

           MOVE '0'                    TO PRT-LNK-REQU.
           MOVE SPACES                 TO PRT-STR-DATA.
           PERFORM H70-PRINT-LINE.

       H50-PRINT-SECTION.
           MOVE 1                      TO LEN.
           STRING '        ' PCL-ESC PCL-BOLD HELPD-DATA(4:)
               DELIMITED BY SIZE     INTO VAR-LINE
                                     WITH POINTER LEN.
           SUBTRACT 1                FROM LEN.

           MOVE LEN                    TO TXT-MSTR-BUFLEN.
           MOVE VAR-LINE               TO TXT-MSTR-BUFFER.

           MOVE +3                     TO VAR-FINDL.
           MOVE '==='                  TO VAR-FIND.
           COMPUTE VAR-REPLL = LENGTH OF PCL-NORM + 2.
           STRING PCL-ESC PCL-NORM ' '
               DELIMITED BY SIZE     INTO VAR-REPL.
           MOVE ZEROES                 TO TXT-PNTR.
           PERFORM P50-REPLACE-ONCE.

           MOVE SPACE                  TO PRT-LNK-REQU.
           MOVE TXT-MSTR-BUFFER        TO PRT-STR-DATA.
           PERFORM H70-PRINT-LINE.

           MOVE SPACES                 TO VAR-LINE.

       H60-PRINT-TEXT.
           MOVE LENGTH OF VAR-LINE     TO TXT-MSTR-BUFLEN.
           MOVE VAR-LINE               TO TXT-MSTR-BUFFER.

           MOVE +1                     TO VAR-FINDL.
           MOVE '{'                    TO VAR-FIND.
           COMPUTE VAR-REPLL = LENGTH OF PCL-BOLD + 2.
           STRING ' ' PCL-ESC PCL-BOLD
               DELIMITED BY SIZE     INTO VAR-REPL.
           MOVE ZEROES                 TO TXT-PNTR.
           PERFORM P50-REPLACE-ONCE THRU P52-REPLACE-ALL.

           MOVE +1                     TO VAR-FINDL.
           MOVE '}'                    TO VAR-FIND.
           COMPUTE VAR-REPLL = LENGTH OF PCL-NORM + 2.
           STRING PCL-ESC PCL-NORM ' '
               DELIMITED BY SIZE     INTO VAR-REPL.
           MOVE ZEROES                 TO TXT-PNTR.
           PERFORM P50-REPLACE-ONCE THRU P52-REPLACE-ALL.

           MOVE TXT-MSTR-BUFFER        TO PRT-STR-DATA.
           PERFORM H70-PRINT-LINE.

           MOVE SPACES                 TO VAR-LINE.

       H70-PRINT-LINE.
           EVALUATE PRT-LNK-REQU
             WHEN '1'        MOVE 1    TO LINE-CNT
             WHEN '+'        CONTINUE
             WHEN '-'        ADD  3    TO LINE-CNT
             WHEN '0'        ADD  2    TO LINE-CNT
             WHEN OTHER      ADD  1    TO LINE-CNT
           END-EVALUATE.
           PERFORM H80-CALL-PRTMAN   THRU H85-EXIT.

       H80-CALL-PRTMAN.
           EXEC CICS LINK
                     PROGRAM  (PRT-PGM-NAME)
                     COMMAREA (PRT-PGM-COMM)
                     LENGTH   (PRT-PGM-COML)
           END-EXEC.
       H82-CHK-COMPLETE.
           IF  PRINT-COMPLETED
               MOVE SPACES             TO PRT-STR-DATA
           ELSE
               MOVE PRT-LNK-RESP       TO UNEX-RESP
               MOVE PRT-LNK-RSP2       TO UNEX-RESP2
               STRING PRT-PGM-NAME ' FAILURE: RC=' PRT-LNK-RETN
                      ', RS=' UNEX-RESP ', R2=' UNEX-RESP2
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               IF  NOT PRINT-ENDOFMSG
                   SET PRINT-ENDOFMSG  TO TRUE
                   PERFORM H80-CALL-PRTMAN
               END-IF
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       H85-EXIT.
           EXIT.

       H90-PRINT-RETURN.

           IF  COMM-CURRRTN = 'HELP'
               STRING 'Job ' PRT-PWR-JOBN ' written to LST queue.'
                   DELIMITED BY SIZE INTO HELP-MESSAGE-O
               GO TO D80-HELP-OUTPUT
           END-IF.
  ^
  |        STRING 'Job ' PRT-PWR-JOBN ' written to LST queue.'
  |            DELIMITED BY SIZE     INTO LIST-MESSAGE-O.
/DLC0/     GO TO C80-LIST-OUTPUT.

      /*****************************************************************
      *    PERFORMED ROUTINES                                          *
      ******************************************************************

       P10-EXTRACT-HELP-TEXT.
           PERFORM Q35-DELETEQ-TS.
           SET  SECTION-HEADERS-ABSENT TO TRUE.

           PERFORM Q20-INITKEY-SYHELPD.
           MOVE COMM-HELPTRAN          TO HELPD-TRAN.
           MOVE COMM-HELPNAME          TO HELPD-NAME.
           MOVE 1                      TO HELPD-SEQU.
           PERFORM Q21-STARTBR-SYHELPD.

           MOVE ZEROES                 TO SS-ITEM.
           PERFORM WITH TEST BEFORE
             UNTIL HELPD-TRAN NOT = COMM-HELPTRAN
                OR HELPD-NAME NOT = COMM-HELPNAME
                OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q23-READNEXT-SYHELPD
             IF  EIBRESP = DFHRESP(NORMAL)
             AND HELPD-TRAN = COMM-HELPTRAN
             AND HELPD-NAME = COMM-HELPNAME
                 IF  HELPD-NAME > SPACES
                 AND HELPD-DATA(1:3) = '==='
                     SET  SECTION-HEADERS-PRESENT
                                       TO TRUE
                 END-IF
                 MOVE HELPD-DATA       TO TS-RECD
                 PERFORM Q33-WRITEQ-TS
                 IF  SS-ITEM < HELP-MAX
                     ADD  1            TO SS-ITEM
                 ELSE
                     MOVE DFHRESP(ENDFILE)
                                       TO EIBRESP
                 END-IF
             END-IF
           END-PERFORM.

           IF  EIBRESP = DFHRESP(NORMAL)
                      OR DFHRESP(NOTFND)
                      OR DFHRESP(ENDFILE)
               PERFORM Q25-ENDBR-SYHELPD
               PERFORM Q31-NUMITEMS-TS
               IF  TS-TOTL <= ZERO
                   MOVE SPACES         TO TS-RECD
                   PERFORM Q33-WRITEQ-TS
                   PERFORM Q31-NUMITEMS-TS
               END-IF
           END-IF.
       P15-EXIT.
           EXIT.

       P20-CONVERT-IDX-TO-POS.
           SUBTRACT 1 FROM IDX     GIVING OFS.
           DIVIDE OFS BY HELPROWS  GIVING COL REMAINDER ROW.
           COMPUTE POS = (ROW + HELPHEAD) * SCRNWDTH + (COL * HELPCWDT).
           ADD  1                      TO ROW COL.
       P25-EXIT.
           EXIT.

       P30-BUILD-HELP-BUFFER.
           PERFORM P20-CONVERT-IDX-TO-POS THRU P25-EXIT.

      * prepare help text
           PERFORM Q90-SET-LENGTH.
           PERFORM P40-FIND-SPECIAL-CHARS THRU P45-EXIT.

      * build output buffer
           STRING SCRSETBA SCR-ADDR
               DELIMITED BY SIZE     INTO HELP-BUFFER
                                     WITH POINTER HELP-BUFLEN.
           IF  VAR-TEXT(1:1) NOT = SCRSTFLD
               STRING SCRSTFLD FLDASKIP
                   DELIMITED BY SIZE INTO HELP-BUFFER
                                     WITH POINTER HELP-BUFLEN
           END-IF.
           STRING VAR-TEXT(1:VAR-TEXTL)
               DELIMITED BY SIZE     INTO HELP-BUFFER
                                     WITH POINTER HELP-BUFLEN.
       P33-CHECK-END-OF-HELP.
           IF  COMM-HELPNAME > SPACES
             EVALUATE TRUE
      * end of help set?
             WHEN SS-ITEM >= TS-TOTL
               SET  EOD-MARKED         TO TRUE
               ADD 1                   TO IDX
               PERFORM P20-CONVERT-IDX-TO-POS THRU P25-EXIT
               STRING SCRSETBA SCR-ADDR SCRSTFLD FLDASBRT END-OF-HELP
                   DELIMITED BY SIZE INTO HELP-BUFFER
                                     WITH POINTER HELP-BUFLEN
      * end of help page?
             WHEN IDX = HELPPAGE
               ADD 1                   TO IDX
               PERFORM P20-CONVERT-IDX-TO-POS THRU P25-EXIT
               STRING SCRSETBA SCR-ADDR SCRSTFLD FLDASBRT MORE-HELP
                   DELIMITED BY SIZE INTO HELP-BUFFER
                                     WITH POINTER HELP-BUFLEN
             END-EVALUATE
           END-IF.
       P35-EXIT.
           EXIT.

       P40-FIND-SPECIAL-CHARS.
           MOVE VAR-TEXTL              TO TXT-MSTR-BUFLEN.
           MOVE VAR-TEXT(1:VAR-TEXTL)  TO TXT-MSTR-BUFFER.

           IF  TXT-MSTR-BUFFER(1:3) = '==='
             MOVE +3                   TO VAR-FINDL
             MOVE '==='                TO VAR-FIND
             MOVE +2                   TO VAR-REPLL
             STRING SCRSTFLD FLDASBRT     DELIMITED BY SIZE
                                     INTO VAR-REPL
             MOVE ZEROES               TO TXT-PNTR
             PERFORM P50-REPLACE-ONCE

             MOVE +3                   TO VAR-FINDL
             MOVE '==='                TO VAR-FIND
             MOVE +2                   TO VAR-REPLL
             STRING SCRSTFLD FLDASKIP     DELIMITED BY SIZE
                                     INTO VAR-REPL
             MOVE ZEROES               TO TXT-PNTR
             PERFORM P50-REPLACE-ONCE
           END-IF.

           MOVE +1                     TO VAR-FINDL
           MOVE '{'                    TO VAR-FIND.
           MOVE +2                     TO VAR-REPLL.
           STRING SCRSTFLD FLDASBRT       DELIMITED BY SIZE
                                     INTO VAR-REPL.
           MOVE ZEROES                 TO TXT-PNTR.
           PERFORM P50-REPLACE-ONCE THRU P52-REPLACE-ALL.

           MOVE +1                     TO VAR-FINDL
           MOVE '}'                    TO VAR-FIND.
           MOVE +2                     TO VAR-REPLL.
           STRING SCRSTFLD FLDASKIP       DELIMITED BY SIZE
                                     INTO VAR-REPL.
           MOVE ZEROES                 TO TXT-PNTR.
           PERFORM P50-REPLACE-ONCE THRU P52-REPLACE-ALL.

           MOVE TXT-MSTR-BUFLEN        TO VAR-TEXTL.
           MOVE TXT-MSTR-BUFFER(1:VAR-TEXTL)
                                       TO VAR-TEXT.
       P45-EXIT.
           EXIT.

       P50-REPLACE-ONCE.
           PERFORM P54-REPLACE-STRING THRU P55-EXIT.
       P52-REPLACE-ALL.
           IF  TXT-PNTR < TXT-MSTR-BUFLEN
               GO TO P50-REPLACE-ONCE
           END-IF.
       P54-REPLACE-STRING.
           SET  TXT-REQUEST-FIND       TO TRUE.
           MOVE VAR-FINDL              TO TXT-PARM-STRLEN.
           MOVE VAR-FIND               TO TXT-PARM-STRING.
           CALL 'TXTMAN'            USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING.
           IF  TXT-STRING-NOT-FOUND
               GO TO P55-EXIT
           END-IF.
           IF  NOT TXT-STRING-WAS-FOUND
               MOVE -2                 TO ERROR-SWITCH
               GO TO P55-EXIT
           END-IF.

           IF  TXT-MSTR-BUFFER(TXT-PNTR:5) = '{...}'
           OR  TXT-MSTR-BUFFER(TXT-PNTR - 4:5) = '{...}'
               GO TO P55-EXIT
           END-IF.

           IF  VAR-FINDL = VAR-REPLL
               SET TXT-REQUEST-OVERLAY TO TRUE
               MOVE VAR-REPL           TO TXT-PARM-STRING
               CALL 'TXTMAN'        USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
           ELSE
               SET  TXT-REQUEST-DELETE TO TRUE
               MOVE SPACES             TO TXT-PARM-STRING
               CALL 'TXTMAN'        USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
               IF  TXT-REQUEST-COMPLETED
                   SET  TXT-REQUEST-INSERT TO TRUE
                   MOVE VAR-REPLL      TO TXT-PARM-STRLEN
                   MOVE VAR-REPL       TO TXT-PARM-STRING
                   IF  VAR-REPLL > VAR-FINDL
                     COMPUTE TXT-MSTR-BUFLEN = TXT-MSTR-BUFLEN
                                             + (VAR-REPLL - VAR-FINDL)
                   END-IF
                   CALL 'TXTMAN'    USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
                   IF  VAR-REPLL < VAR-FINDL
                     COMPUTE TXT-MSTR-BUFLEN = TXT-MSTR-BUFLEN
                                             - (VAR-FINDL - VAR-REPLL)
                   END-IF
               END-IF
           END-IF.

           IF  NOT TXT-REQUEST-COMPLETED
               MOVE    -2              TO ERROR-SWITCH
               GO TO P55-EXIT
           END-IF.
       P55-EXIT.
           EXIT.

       P60-GET-LAST-PAGE.
           PERFORM WITH TEST AFTER
             UNTIL HELPD-TRAN NOT = LIST-HELP-TRAN-O(ROW COL)
                OR HELPD-NAME NOT = LIST-HELP-NAME-O(ROW COL)
                OR EIBRESP NOT = DFHRESP(NORMAL)
               PERFORM Q23-READNEXT-SYHELPD
               IF  EIBRESP = DFHRESP(NORMAL)
               AND HELPD-TRAN = LIST-HELP-TRAN-O(ROW COL)
               AND HELPD-NAME = LIST-HELP-NAME-O(ROW COL)
                   DIVIDE HELPD-SEQU BY HELPPAGE
                                    GIVING P2
                                 REMAINDER R2
                   IF  R2 > ZERO
                       ADD  1           TO P2
                   END-IF
                   MOVE P2              TO LIST-HELP-PTOT-O(ROW COL)
               END-IF
           END-PERFORM.
           EVALUATE EIBRESP
           WHEN DFHRESP(NORMAL)
               PERFORM Q24-READPREV-SYHELPD
           WHEN DFHRESP(ENDFILE)
               PERFORM Q24-READPREV-SYHELPD
               PERFORM Q23-READNEXT-SYHELPD
           END-EVALUATE.
       P65-EXIT.
           EXIT.

       P70-SAVE-SECTION-HEADER.
           MOVE TS-RECD(4:)            TO VAR-TEXT.
           PERFORM Q90-SET-LENGTH.

           MOVE ZEROES                 TO TALLY.
           INSPECT VAR-TEXT(1:VAR-TEXTL)
                                 TALLYING TALLY
                                      FOR CHARACTERS
                                   BEFORE INITIAL '==='.
           MOVE VAR-TEXT(1:TALLY)      TO HELP-SECTION.

      * adjust section header positioning, if necessary
           PERFORM P20-CONVERT-IDX-TO-POS THRU P25-EXIT.
           IF  COL = 1 AND ROW > (HELPROWS - 2)
           OR  COL = 2 AND ROW > (HELPROWS - 3)
               PERFORM WITH TEST BEFORE
                 VARYING CNT FROM ROW BY 1
                   UNTIL COL = 1 AND CNT > HELPROWS
                      OR COL = 2 AND CNT > (HELPROWS - 2)
                 MOVE SPACES           TO VAR-TEXT
                 PERFORM P30-BUILD-HELP-BUFFER
                 ADD  1                TO IDX
                                          HELP-ADJUSTED
               END-PERFORM
               IF  COL = 2
                 MOVE SPACES           TO TS-RECD
                 ADD  1                TO HELP-ADJUSTED
               END-IF
           END-IF.
       P75-EXIT.
           EXIT.

       P80-FIND-PREV-SECTION.
           IF  TS-ITEM > 1
              SUBTRACT 1             FROM TS-ITEM
                                   GIVING SS-ITEM
           ELSE
              MOVE 1                   TO SS-ITEM
           END-IF.
           PERFORM WITH TEST AFTER
             VARYING SS-ITEM FROM SS-ITEM BY -1
               UNTIL SS-ITEM <= 1
                  OR TS-RECD(1:3) = '==='
                  OR EIBRESP NOT = DFHRESP(NORMAL)
               PERFORM Q32-READQ-TS
           END-PERFORM.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO P85-EXIT
           END-IF.
           IF  TS-RECD(1:3) = '==='
           OR  TS-ITEM > 1
               MOVE SS-ITEM            TO TS-ITEM
           ELSE
               MOVE 'This is the first section.'
                                       TO HELP-MESSAGE-O
           END-IF.
       P85-EXIT.
           EXIT.

       P90-FIND-NEXT-SECTION.
           IF  TS-ITEM < TS-TOTL
               ADD  1                  TO TS-ITEM
                                   GIVING SS-ITEM
           ELSE
               MOVE TS-TOTL            TO SS-ITEM
           END-IF.
           PERFORM WITH TEST AFTER
             VARYING SS-ITEM FROM SS-ITEM BY 1
               UNTIL SS-ITEM >= TS-TOTL
                  OR TS-RECD(1:3) = '==='
                  OR EIBRESP NOT = DFHRESP(NORMAL)
               PERFORM Q32-READQ-TS
           END-PERFORM.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO P95-EXIT
           END-IF.
           IF  TS-RECD(1:3) = '==='
               MOVE SS-ITEM            TO TS-ITEM
           ELSE
               MOVE 'This is the last section.'
                                       TO HELP-MESSAGE-O
           END-IF.
       P95-EXIT.
           EXIT.

      /*****************************************************************
      *    MORE PERFORMED ROUTINES                                     *
      ******************************************************************

       Q00-FORMAT-DATE-AND-TIME.
           EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     FULLDATE (WS-WRKDATE)          DATESEP
                     TIME     (WS-WRKTIME)          TIMESEP
           END-EXEC.
       Q02-REFORMAT-TIME.
           IF  WS-HRSTIME < 12
               MOVE ' AM'              TO WS-XXXTIME
               IF  WS-HRSTIME = 00
                   MOVE 12             TO WS-HRSTIME
               END-IF
           ELSE
               MOVE ' PM'              TO WS-XXXTIME
               IF  WS-HRSTIME > 12
                   SUBTRACT 12       FROM WS-HRSTIME
               END-IF
           END-IF.
       Q05-EXIT.
           EXIT.

/DLC0/ Q10-DISPLAY-POPUP.
  |
  |        EXEC CICS GETMAIN
  v                  SET      (ADDRESS OF WAITWINI)
                     LENGTH   (LENGTH OF WAITWINI)
                     INITIMG  (LOVALUE)
           END-EXEC.

           MOVE POPUP-TEXT             TO WAIT-MESSAGE-O.
           EXEC CICS SEND
                     MAP      (WAITWIN)
                     FROM     (WAITWINO)
                     MAPSET   (HELPMS)
                     TERMINAL
                     CURSOR(+926)
                     WAIT
                     NOHANDLE
           END-EXEC.

           EXEC CICS FREEMAIN
                     DATA     (WAITWINI)
  ^        END-EXEC.
  |
  |    Q15-EXIT.
/DLC0/     EXIT.

       Q20-INITKEY-SYHELPD.
           IF  ADDRESS OF SYHELPD-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF SYHELPD-RECORD)
                         LENGTH   (HELPD-MAXL)
               END-EXEC
           END-IF.
           MOVE LOW-VALUES             TO HELPD-KEY.
           MOVE ZEROES                 TO HELPD-SEQU.
       Q21-STARTBR-SYHELPD.
           EXEC CICS STARTBR
                     FILE     (SYHELPD)
                     RIDFLD   (HELPD-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q22-STARTEQ-SYHELPD.
           EXEC CICS STARTBR
                     FILE     (SYHELPD)
                     RIDFLD   (HELPD-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q23-READNEXT-SYHELPD.
           MOVE HELPD-MAXL             TO HELPD-RECL.
           EXEC CICS READNEXT
                     FILE     (SYHELPD)
                     INTO     (SYHELPD-RECORD)
                     LENGTH   (HELPD-RECL)
                     RIDFLD   (HELPD-KEY)
                     NOHANDLE
           END-EXEC.
       Q23-CHECK-IT.
           IF  EIBRESP = DFHRESP(NORMAL)
               IF  HELPD-NAME <= SPACES AND HELPD-SEQU > ZERO
               OR  HELPD-NAME  > SPACES AND HELPD-SEQU > 1
                   GO TO Q23-READNEXT-SYHELPD
               END-IF
           END-IF.
       Q24-READPREV-SYHELPD.
           MOVE HELPD-MAXL             TO HELPD-RECL.
           EXEC CICS READPREV
                     FILE     (SYHELPD)
                     INTO     (SYHELPD-RECORD)
                     LENGTH   (HELPD-RECL)
                     RIDFLD   (HELPD-KEY)
                     NOHANDLE
           END-EXEC.
       Q24-CHECK-IT.
           IF  EIBRESP = DFHRESP(NORMAL)
               IF  HELPD-NAME <= SPACES AND HELPD-SEQU > ZERO
               OR  HELPD-NAME  > SPACES AND HELPD-SEQU > 1
                   GO TO Q24-READPREV-SYHELPD
               END-IF
           END-IF.
       Q25-ENDBR-SYHELPD.
           EXEC CICS ENDBR
                     FILE     (SYHELPD)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(INVREQ)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       Q26-READEQ-SYHELPD.
           MOVE HELPD-MAXL             TO HELPD-RECL.
           EXEC CICS READ
                     FILE     (SYHELPD)
                     INTO     (SYHELPD-RECORD)
                     LENGTH   (HELPD-RECL)
                     RIDFLD   (HELPD-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q26-READGE-SYHELPD.
           MOVE HELPD-MAXL             TO HELPD-RECL.
           EXEC CICS READ
                     FILE     (SYHELPD)
                     INTO     (SYHELPD-RECORD)
                     LENGTH   (HELPD-RECL)
                     RIDFLD   (HELPD-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q26-READUP-SYHELPD.
           MOVE HELPD-MAXL             TO HELPD-RECL.
           EXEC CICS READ
                     FILE     (SYHELPD)
                     INTO     (SYHELPD-RECORD)
                     LENGTH   (HELPD-RECL)
                     RIDFLD   (HELPD-KEY)
                     EQUAL
                     UPDATE
                     NOHANDLE
           END-EXEC.
       Q27-WRITE-SYHELPD.
           IF  HELPD-NAME <= SPACES
           AND HELPD-SEQU = ZERO
               COMPUTE HELPD-RECL = LENGTH OF HELPD-KEY
                                  + LENGTH OF HELPD-TABLE
           ELSE
               COMPUTE HELPD-RECL = LENGTH OF HELPD-KEY
                                  + LENGTH OF HELPD-DATA
           END-IF.
           EXEC CICS WRITE
                     FILE     (SYHELPD)
                     FROM     (SYHELPD-RECORD)
                     LENGTH   (HELPD-RECL)
                     RIDFLD   (HELPD-KEY)
                     NOHANDLE
           END-EXEC.
       Q27-REWRITE-SYHELPD.
           IF  HELPD-NAME <= SPACES
           AND HELPD-SEQU = ZERO
               COMPUTE HELPD-RECL = LENGTH OF HELPD-KEY
                                  + LENGTH OF HELPD-TABLE
           ELSE
               COMPUTE HELPD-RECL = LENGTH OF HELPD-KEY
                                  + LENGTH OF HELPD-DATA
           END-IF.
           EXEC CICS REWRITE
                     FILE     (SYHELPD)
                     FROM     (SYHELPD-RECORD)
                     LENGTH   (HELPD-RECL)
                     NOHANDLE
           END-EXEC.
       Q28-DELETE-SYHELPD.
           EXEC CICS DELETE
                     FILE     (SYHELPD)
                     NOHANDLE
           END-EXEC.
       Q28-DELETEALL-SYHELPD.
           EXEC CICS DELETE
                     FILE     (SYHELPD)
                     RIDFLD   (HELPD-KEY)
                     KEYLENGTH(12)
                     GENERIC
                     NOHANDLE
           END-EXEC.
       Q28-UNLOCK-SYHELPD.
           EXEC CICS UNLOCK
                     FILE     (SYHELPD)
                     NOHANDLE
           END-EXEC.
       Q29-EXIT.
           EXIT.

       Q31-NUMITEMS-TS.
           MOVE 1                      TO TS-ITEM.
/DLC3/*              QUEUE   (TS-QUEUE)
           EXEC CICS READQ TS
/DLC3/               QNAME   (TS-QUEUE)
                     ITEM    (TS-ITEM)
                     INTO    (TS-RECD)
                     NUMITEMS(TS-TOTL)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(QIDERR)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
               MOVE ZEROES             TO TS-TOTL
           END-IF.
       Q32-READQ-TS.
/DLC3/*              QUEUE   (TS-QUEUE)
           EXEC CICS READQ TS
/DLC3/               QNAME   (TS-QUEUE)
                     INTO    (TS-RECD)
                     ITEM    (SS-ITEM)
                     NOHANDLE
           END-EXEC.
       Q33-WRITEQ-TS.
/DLC3/*              QUEUE   (TS-QUEUE)
           EXEC CICS WRITEQ TS
/DLC3/               QNAME   (TS-QUEUE)
                     FROM    (TS-RECD)
                     MAIN
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(NORMAL)
               MOVE SPACES             TO TS-RECD
           END-IF.
       Q34-REWRITEQ-TS.
/DLC3/*              QUEUE   (TS-QUEUE)
           EXEC CICS WRITEQ TS
/DLC3/               QNAME   (TS-QUEUE)
                     FROM    (TS-RECD)
                     ITEM    (SS-ITEM)
                     REWRITE
                     MAIN
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(NORMAL)
               MOVE SPACES             TO TS-RECD
           END-IF.
       Q35-DELETEQ-TS.
           IF  TS-QUEUE <= SPACES
/DLC3/*        MOVE THIS-TRN           TO TS-TRAN
/DLC3/*        MOVE EIBTRMID           TO TS-TERM
/DLC3/         MOVE THIS-TRN           TO TS-BASENAME(1:4)
/DLC3/         MOVE EIBTRMID           TO TS-BASENAME(5:4)
/DLC3/         MOVE 'DISPLAY'          TO TS-QUALIFIER
           END-IF.
           MOVE ZEROES                 TO HELP-ADJUSTED
                                          TS-TOTL.
           MOVE 1                      TO TS-ITEM.
           MOVE SPACES                 TO HELP-SECTION
                                          TS-RECD.
/DLC3/*              QUEUE   (TS-QUEUE)
           EXEC CICS DELETEQ TS
/DLC3/               QNAME   (TS-QUEUE)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(QIDERR)
               MOVE DFHRESP(NORMAL) TO EIBRESP
           END-IF.
       Q39-EXIT.
           EXIT.

       Q43-WRITEQ-2-TS.
/DLC3/*              QUEUE   (T2-QUEUE)
           EXEC CICS WRITEQ TS
/DLC3/               QNAME   (T2-QUEUE)
                     FROM    (T2-RECD)
                     ITEM    (T2-ITEM)
                     MAIN
                     NOHANDLE
           END-EXEC.
       Q45-DELETEQ-2-TS.
      * swap between two queue names
/DLC3/*    MOVE TS-TRAN                TO T2-TRAN.
/DLC3/*    MOVE TS-TERM                TO T2-TERM.
/DLC3/     MOVE TS-QUEUE               TO T2-QUEUE.
/DLC3/     IF  T2-QUALIFIER = 'EDITING1'
/DLC3/        MOVE 'EDITING2'          TO T2-QUALIFIER
/DLC3/     ELSE
/DLC3/        MOVE 'EDITING1'          TO T2-QUALIFIER
/DLC3/     END-IF.
      *
           MOVE ZEROES                 TO T2-TOTL.
           MOVE 1                      TO T2-ITEM.
           MOVE SPACES                 TO T2-RECD.
/DLC3/*              QUEUE   (T2-QUEUE)
           EXEC CICS DELETEQ TS
/DLC3/               QNAME   (T2-QUEUE)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(QIDERR)
               MOVE DFHRESP(NORMAL) TO EIBRESP
           END-IF.
       Q49-EXIT.
           EXIT.

       Q50-FLOW-PARAGRAPH.
      * find the start of a paragraph as the line after a blank line,
      *       or the line after a field mark, or the first help line
           MOVE SS-ITEM                TO CNT
           PERFORM WITH TEST AFTER
             VARYING SS-ITEM FROM SS-ITEM BY -1
               UNTIL SS-ITEM <= 1
                  OR TS-RECD <= SPACES
                  OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q32-READQ-TS
             IF  EIBRESP = DFHRESP(NORMAL)
               IF  TS-RECD > SPACES
                 MOVE ZEROES           TO TALLY
                 INSPECT TS-RECD TALLYING TALLY FOR ALL FLDMRK
                 IF  TALLY > ZERO
                   MOVE DFHRESP(ENDFILE) TO EIBRESP
                 END-IF
               END-IF
             END-IF
           END-PERFORM.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               GO TO Q55-EXIT
           END-IF.
           IF  EIBRESP = DFHRESP(ENDFILE)
           OR  TS-RECD <= SPACES
               ADD  1                  TO SS-ITEM
           END-IF.

      * extract words from map until a blank line or field-mark found
           MOVE +3000                  TO TXT-MSTR-BUFLEN.
           MOVE SPACES                 TO TXT-MSTR-BUFFER.
           MOVE 1                      TO PTR.
           MOVE SS-ITEM                TO BEGP.

           PERFORM WITH TEST AFTER
             VARYING SS-ITEM FROM BEGP BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR TS-RECD <= SPACES
                  OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q32-READQ-TS
             IF  EIBRESP = DFHRESP(NORMAL)
             AND TS-RECD > SPACES
               MOVE SS-ITEM            TO ENDP
      * ... parse out the words in the current line
               PERFORM Q95-LEFT-JUSTIFY
               MOVE 1                  TO P2
               PERFORM WITH TEST BEFORE
                 UNTIL P2 > LENGTH OF TS-RECD
                   UNSTRING TS-RECD
                     DELIMITED BY ALL SPACES OR ALL LOW-VALUES
                     INTO VAR-FIND COUNT IN VAR-FINDL
                     WITH POINTER P2
                   END-UNSTRING
                   STRING VAR-FIND(1:VAR-FINDL) ' ' DELIMITED BY SIZE
                     INTO TXT-MSTR-BUFFER WITH POINTER PTR
                   END-STRING
                   IF  VAR-FIND(VAR-FINDL:1) = '.'
                     STRING ' ' DELIMITED BY SIZE
                       INTO TXT-MSTR-BUFFER WITH POINTER PTR
                     END-STRING
                   END-IF
               END-PERFORM
      * ... look for field-mark character as end of data
               MOVE ZEROES             TO TALLY
               INSPECT TS-RECD TALLYING TALLY FOR ALL FLDMRK
               IF  TALLY > ZERO
                 MOVE DFHRESP(ENDFILE) TO EIBRESP
               END-IF
             END-IF
      * end of extract
           END-PERFORM.

           SUBTRACT 2   FROM PTR   GIVING TXT-MSTR-BUFLEN

      * reformat paragraph back into the same lines
           MOVE 1                      TO PTR.
           MOVE LENGTH OF HELP-TEXT-I(1 1)
                                       TO LEN.
           PERFORM WITH TEST AFTER
             VARYING SS-ITEM FROM BEGP BY 1
               UNTIL SS-ITEM >= ENDP
                  OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q32-READQ-TS
             MOVE SPACES               TO TS-RECD
             IF  PTR <= TXT-MSTR-BUFLEN
                 MOVE LEN              TO L2
                 COMPUTE P2 = PTR + L2 - 1
                 EVALUATE TRUE
                 WHEN TXT-MSTR-BUFFER(P2 + 0:1) = SPACE
                     CONTINUE
                 WHEN TXT-MSTR-BUFFER(P2 + 1:1) = SPACE
                     ADD  1            TO P2 L2
                 WHEN OTHER
                     PERFORM WITH TEST BEFORE
                       VARYING P2 FROM P2 BY -1
                         UNTIL TXT-MSTR-BUFFER(P2:1) = SPACE
                     END-PERFORM
                     SUBTRACT PTR FROM P2 GIVING L2
                 END-EVALUATE
                 MOVE TXT-MSTR-BUFFER(PTR:L2)
                                       TO TS-RECD(1:)
                 COMPUTE PTR = PTR + L2
                 IF  TXT-MSTR-BUFFER(PTR:1) = SPACE
                     ADD  1            TO PTR
                 END-IF
             END-IF
             PERFORM Q34-REWRITEQ-TS
           END-PERFORM.

           MOVE CNT                    TO SS-ITEM.
       Q55-EXIT.
           EXIT.

       Q60-DELETE-LINE.
           IF  SS-ITEM > TS-TOTL
               GO TO Q65-EXIT
           END-IF.

           MOVE SS-ITEM                TO PTR.
           PERFORM Q45-DELETEQ-2-TS.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM 1 BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS

             IF  EIBRESP = DFHRESP(NORMAL)
               IF  SS-ITEM NOT = PTR
                 EXEC CICS SUSPEND END-EXEC
                 MOVE TS-RECD          TO T2-RECD
                 PERFORM Q43-WRITEQ-2-TS
               END-IF
               IF  SS-ITEM = 32767
                 MOVE DFHRESP(ENDFILE) TO EIBRESP
               END-IF
             END-IF
           END-PERFORM.

/DLC3/*              QUEUE   (TS-QUEUE)
           EXEC CICS DELETEQ TS
/DLC3/               QNAME   (TS-QUEUE)
                     NOHANDLE
           END-EXEC.

           MOVE T2-QUEUE               TO TS-QUEUE.
           MOVE T2-ITEM                TO TS-TOTL.
           SUBTRACT 1  FROM PTR    GIVING SS-ITEM.

           IF  TS-TOTL >= HELP-MAX
           OR  EIBRESP = DFHRESP(ITEMERR)
               MOVE DFHRESP(ENDFILE)   TO EIBRESP
           END-IF.
       Q65-EXIT.
           EXIT.

       Q70-INSERT-LINE.
           IF  SS-ITEM > TS-TOTL
               GO TO Q65-EXIT
           END-IF.

           MOVE SS-ITEM                TO PTR.
           PERFORM Q45-DELETEQ-2-TS.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM 1 BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS

             IF  EIBRESP = DFHRESP(NORMAL)
               EXEC CICS SUSPEND END-EXEC
               MOVE TS-RECD            TO T2-RECD
               PERFORM Q43-WRITEQ-2-TS
               IF  SS-ITEM = PTR
                 EXEC CICS SUSPEND END-EXEC
                 MOVE SPACES           TO T2-RECD
                 PERFORM Q43-WRITEQ-2-TS
               END-IF
               IF  SS-ITEM = 32767
                 MOVE DFHRESP(ENDFILE) TO EIBRESP
               END-IF
             END-IF
           END-PERFORM.

/DLC3/*              QUEUE   (TS-QUEUE)
           EXEC CICS DELETEQ TS
/DLC3/               QNAME   (TS-QUEUE)
                     NOHANDLE
           END-EXEC.

           MOVE T2-QUEUE               TO TS-QUEUE.
           MOVE T2-ITEM                TO TS-TOTL.
           MOVE PTR                    TO SS-ITEM.

           IF  TS-TOTL >= HELP-MAX
           OR  EIBRESP = DFHRESP(ITEMERR)
               MOVE DFHRESP(ENDFILE)   TO EIBRESP
           END-IF.
       Q75-EXIT.
           EXIT.

       Q80-SPLTJOIN-LINE.
           PERFORM Q32-READQ-TS.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               IF  EIBRESP = DFHRESP(ITEMERR)
                 MOVE DFHRESP(NORMAL) TO EIBRESP
               END-IF
               GO TO Q85-EXIT
           END-IF.

           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.
           SUBTRACT HELPHEAD         FROM ROW.
           SUBTRACT 1                FROM COL.
           IF  COL > LENGTH OF HELP-TEXT-I(1 1)
               COMPUTE COL = COL - 4
                           - LENGTH OF HELP-TEXT-I(1 1)
           END-IF.

           EVALUATE TRUE
           WHEN TS-RECD <= SPACES
      * if completely blank, insert a blank line after this one
               MOVE SPACES             TO VAR-TEXT
               PERFORM Q70-INSERT-LINE THRU Q75-EXIT

           WHEN TS-RECD(COL:) > SPACES
      * if text following, split this line to new line
               MOVE TS-RECD(COL:)      TO VAR-TEXT
               PERFORM Q70-INSERT-LINE THRU Q75-EXIT
               IF  EIBRESP = DFHRESP(NORMAL)
                          OR DFHRESP(ENDFILE)
                   ADD  1              TO SS-ITEM
                   PERFORM Q32-READQ-TS
                   IF  EIBRESP = DFHRESP(ITEMERR)
                       MOVE VAR-TEXT   TO TS-RECD
                       PERFORM Q33-WRITEQ-TS
                   ELSE
                       MOVE VAR-TEXT   TO TS-RECD
                       PERFORM Q34-REWRITEQ-TS
                   END-IF
                   SUBTRACT 1        FROM SS-ITEM
                   PERFORM Q32-READQ-TS
                   MOVE SPACES         TO TS-RECD(COL:)
                   PERFORM Q34-REWRITEQ-TS
               END-IF

           WHEN OTHER
      * no text following, join next line to this line
               COMPUTE L2 = LENGTH OF HELP-TEXT-I(1 1)
                          - (COL - 1)
               ADD  1                  TO SS-ITEM
               PERFORM Q32-READQ-TS
               IF  EIBRESP = DFHRESP(ITEMERR)
                 MOVE DFHRESP(NORMAL) TO EIBRESP
                 GO TO Q85-EXIT
               END-IF
               PERFORM Q95-LEFT-JUSTIFY
               PERFORM WITH TEST BEFORE
                 VARYING P2 FROM L2 BY -1 UNTIL P2 < 1
                      OR TS-RECD(P2:1) <= SPACE
               END-PERFORM
               MOVE LEN                TO VAR-TEXTL
               MOVE TS-RECD            TO VAR-TEXT
               IF  P2 > ZERO
                 MOVE VAR-TEXT(P2 + 1:) TO TS-RECD
                 PERFORM Q34-REWRITEQ-TS
                 MOVE SPACES           TO VAR-TEXT(P2 + 1:)
                 SUBTRACT 1          FROM SS-ITEM
                 PERFORM Q32-READQ-TS
                 MOVE VAR-TEXT         TO TS-RECD(COL:)
                 PERFORM Q34-REWRITEQ-TS
                 ADD  1                TO SS-ITEM
                 PERFORM Q32-READQ-TS
               END-IF
      * if remaining line is blank, delete it
               IF  TS-RECD <= SPACES
                 PERFORM Q60-DELETE-LINE THRU Q65-EXIT
               END-IF
           END-EVALUATE.
       Q85-EXIT.
           EXIT.

       Q90-SET-LENGTH.
           IF  VAR-TEXT > SPACES
               PERFORM WITH TEST BEFORE
                 VARYING VAR-TEXTL FROM LENGTH OF VAR-TEXT BY -1
                   UNTIL VAR-TEXTL <= 5
                      OR VAR-TEXT(VAR-TEXTL:1) > SPACE
               END-PERFORM
           ELSE
               MOVE 5                  TO VAR-TEXTL
               MOVE SPACES             TO VAR-TEXT
           END-IF.

       Q95-LEFT-JUSTIFY.
           IF  TS-RECD > SPACES
             MOVE LENGTH OF TS-RECD    TO LEN
             PERFORM WITH TEST BEFORE
               UNTIL TS-RECD(1:1) > SPACE
                 SUBTRACT 1          FROM LEN
                 MOVE TS-RECD(2:LEN)   TO TS-RECD(1:)
             END-PERFORM
           END-IF.

/DLC0//*****************************************************************
  |   *    EVEN MORE PERFORMED ROUTINES                                *
  |   ******************************************************************
  v
       R10-SEARCH-FOR-OPTION.
           SET  OPTION-OMITTED         TO TRUE.
           MOVE 1                      TO R2 C2.
           PERFORM WITH TEST AFTER
             UNTIL OPTION-SELECTED
                OR R2 > LISTROWS AND C2 > LISTCOLS
             IF  R2 <= LISTROWS
             AND C2 <= LISTCOLS
             AND LIST-SELECT-I(R2 C2) > SPACE AND NOT = '*'
               SET  OPTION-SELECTED    TO TRUE
             ELSE
               ADD  1                  TO C2
               IF  C2 > LISTCOLS
                 ADD  1                TO R2
                 IF  R2 <= LISTROWS
                   MOVE 1              TO C2
                 END-IF
               END-IF
  ^          END-IF
  |        END-PERFORM.
  |    R15-EXIT.
/DLC0/     EXIT.

      /*****************************************************************
      *    UNEXPECTED ERRORS AND DEBUG LOGGING                         *
      ******************************************************************
       COPY LOGGINGP.

      /*****************************************************************
      *    PROGRAM EXIT TO 2ND-LEVEL PGM OR RETURN TO 1ST-LEVEL PGM    *
      ******************************************************************
       COPY COMMLINK.
      * the above copybook ends with a return to cics

      ******************************************************************
      *    TRANSFER TO THE CICS HELP SYSTEM                            *
      ******************************************************************
       COPY COMMHELP.
      * the above copybook ends with a return to cics

      /*****************************************************************
      *    VERIFICATION ROUTINE                                        *
      ******************************************************************
       V10-GET-CONFIRMATION.
           MOVE AIDENTER               TO EIBAID.
           MOVE 1                      TO POS.
           IF  COMM-CURRRTN = 'LIST'
               MOVE 25                 TO PTR
           ELSE
               MOVE 26                 TO PTR
           END-IF.
      *                         row  adjust            col  adjust
           COMPUTE HALF-WORD = (PTR - 1) * SCRNWDTH + (001 - 1).
           STRING SCRSETBA TWO-BYTES SCRSTFEX X'02'
                  TYPFIELD FLDASBRT  TYPHILIT EXHBLINK
               DELIMITED BY SIZE INTO VAR-DATA WITH POINTER POS.
           STRING MULTI-EDIT-MSG(1:LEN)
               DELIMITED BY SIZE INTO VAR-DATA WITH POINTER POS.
           EVALUATE TRUE
           WHEN LEN < LENGTH OF MULTI-EDIT-MSG - 2
      *                             row  adjust            col  adjust
               COMPUTE HALF-WORD = (PTR - 1) * SCRNWDTH + (080 - 1)
               STRING SCREPEAT TWO-BYTES X'40'
                      SCRSTFLD FLDASKIP DELIMITED BY SIZE
                   INTO VAR-DATA WITH POINTER POS
           WHEN LEN < LENGTH OF MULTI-EDIT-MSG
               STRING SCRSTFLD FLDASKIP DELIMITED BY SIZE
                   INTO VAR-DATA WITH POINTER POS
           END-EVALUATE.
           SUBTRACT 1                FROM POS.

           EXEC CICS CONVERSE
                     FROM     (VAR-DATA)
                     FROMLENGTH(POS)
                     CTLCHAR  (WRTFKFSA)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(EOC)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       V15-EXIT.
           EXIT.

      /*****************************************************************
      *    PROGRAM ERRORS (UNEXPECTED)                                 *
      ******************************************************************
       COPY UNEXERRP.
      * the above copybook ends with a return to cics

      /*****************************************************************
      *    PROGRAM EXIT ROUTINES (NORMAL)                              *
      ******************************************************************

       Z70-HELP-CLOSED.

           COPY COMMXCTL.
      * the above transfers control to standard NOT OPEN screen

       Z80-HELP-RETURN.

           COPY COMMRETN.
      * the above copybook is a psuedo-conversational return to cics

       Z90-HELP-TERMINATION.

           MOVE LENGTH OF COMM-HEADER  TO LINK-LEN.
           IF  COMM-HELPAREA(1:4) > SPACES
               ADD  4                  TO LINK-LEN
           END-IF.

           COPY COMMEXIT REPLACING DFHCOMMAREA BY COMM-HEADER.
      * the above copybook ends with a return to cics

