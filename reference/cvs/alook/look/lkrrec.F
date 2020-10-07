      SUBROUTINE LKRREC(COMAND)
C ---------------------------------------------------------
C ---------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkevt.h"
#include "lkfil.h"
#include "lkunpk.h"
      CHARACTER COMAND*(*),CH
      LOGICAL LKOPTI, FSEQ
      EXTERNAL LKOPTI
      DATA NAFILM /0/
C ---------------------------------------------------------
      IF (NAFILM.EQ.0) NAFILM = NAMIND('FILM')
      IF (.NOT.SEQFIL) THEN
          WRITE(LOUT,*)' [Sequential file not opened]'
          RETURN
      ENDIF
C
      NUMB = 0
      CALL LKGTINT(COMAND,1,' ',NUMB,CH)
      IF (CH.NE.' '.AND.CH.NE.'I') THEN
          WRITE(LOUT,*) '[Invalid event or run number parameter]'
          RETURN
      ENDIF
      IF (LKOPTI(COMAND,'RUN'))THEN
         FSEQ = .TRUE.
         IF (LKOPTI(COMAND,'NSEQ')) FSEQ = .FALSE.
C     read/run numb = read run no. numb
         IRUN = NUMB
         IVOULU = 0
C     reset class word to 0 and reading mode to INTE
         CALL ABSTRCL(0)
         CALL ABMODE ('INTE')
         CALL LKCHKRU (FSEQ,IRUN,IRUNTR,IRET)
         IF (IRET.NE.0) GOTO 999
      ELSEIF (LKOPTI(COMAND,'CLASS')) THEN
C     set read class word
         IF (IABS(NUMB).GT.30) THEN
            WRITE(LOUT,*) '[Invalid class bit; should be 0-30]'
         ELSE
            CALL ABSTRCL (NUMB)
            IF (NUMB.EQ.25) THEN      ! slow control records
               CALL ABMODE ('BATC')
            ELSE
               CALL ABMODE ('INTE')
            ENDIF
         ENDIF
         RETURN
      ELSEIF (NUMB.EQ.0) THEN
C     read next record selected by the CLASS word
         IVOULU = 0
         IRUN = 0
      ELSE
C     read numb = read event no. numb of current run
         IVOULU = NUMB
         IRUN = IRUNTR
      ENDIF
C
      CALL BDROP (IW,'SRUNSEVT')
C     read event no. IVOULU in run no. IRUN
C     if IVOULU<current event# rewind input file
         IF (IRUN.EQ.0 .AND. IVOULU.EQ.0) GOTO 1
         IF (IVOULU.LT.IEVTR) THEN
            IF (LINSEL.GT.0) THEN
               CALL LKRWND('/EDIR')
            ELSE
               CALL LKRWND('/DATA')
            ENDIF
         ENDIF
 1       CALL LKGTEVT(FSEQ,'E',ULIST,IRUN,IVOULU,IER)
         IF(IER.EQ.4 ) THEN
C         EOF on EDIR : try to read a new FILM card if any
              CALL LKOPRD1 ('    ', '    ',LER)
              IF (LER.NE.0) THEN
                SEQFIL = .FALSE.
                WRITE(LOUT,*) ' [no more input file]'
                GOTO 999
              ELSE
                CALL ABRUEV (KRUN,KEVT)
                IF(IRUN.EQ.0.AND.IVOULU.EQ.0) GOTO 999
                IF(IVOULU.EQ.0) THEN
                  IF(IRUN.NE.KRUN) GOTO 1
                ELSE
                  IF(IVOULU.NE.KEVT) GOTO 1
                ENDIF
                GOTO 999
              ENDIF
         ELSE IF(IER.EQ.2) THEN
           WRITE(LOUT,*) ' [End of Run]'
         ELSE IF(IER.EQ.7) THEN
           WRITE(LOUT,*) ' [End of Selected data] '
         ELSE IF(IER.EQ.8) THEN
           WRITE(LOUT,*) ' [Event not found]'
         ENDIF
C
 100  CALL LKSHOW (IER)
C     reset ULIST for next record to the default list
 999  CALL LKRSUNP
      RETURN
      END
#endif
